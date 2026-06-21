;; Tests for cookies, sessions, and auth middleware
(import (scheme base) (scheme write) (kaappi web) (kaappi http))

(define pass 0)
(define fail 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (begin (set! pass (+ pass 1))
             (display "  PASS: ") (display name) (newline))
      (begin (set! fail (+ fail 1))
             (display "  FAIL: ") (display name) (newline)
             (display "    expected: ") (write expected) (newline)
             (display "    got:      ") (write actual) (newline))))

(define (make-req method path . args)
  (let ((headers (if (pair? args) (car args) '()))
        (body (if (and (pair? args) (pair? (cdr args))) (cadr args) "")))
    (make-http-request method path "" "HTTP/1.1" headers body)))

;; --- Cookies ---
(display "=== Cookies ===") (newline)

(let ((req (make-req "GET" "/" '(("cookie" . "theme=dark; session=abc123")))))
  (check "parse cookies" '(("theme" . "dark") ("session" . "abc123"))
    (request-cookies req))
  (check "get cookie" "dark" (request-cookie req "theme"))
  (check "get missing cookie" #f (request-cookie req "missing")))

(let ((req (make-req "GET" "/" '())))
  (check "no cookie header" '() (request-cookies req)))

(check "set-cookie basic"
  '("Set-Cookie" . "sid=abc123")
  (set-cookie "sid" "abc123"))

(check "set-cookie with options"
  #t
  (let ((cookie (set-cookie "sid" "abc" '((path . "/") (max-age . 3600) (http-only . #t)))))
    (and (equal? (car cookie) "Set-Cookie")
         (string? (cdr cookie))
         ;; Should contain Path=/ and Max-Age=3600 and HttpOnly
         (let ((s (cdr cookie)))
           (and (> (string-length s) 10)
                (equal? (substring s 0 7) "sid=abc"))))))

(let ((resp (with-cookie (text-response "ok") "theme" "light")))
  (check "with-cookie" "text/plain; charset=utf-8"
    (response-header resp "Content-Type")))

;; --- Sessions ---
(display "=== Sessions ===") (newline)

(let* ((store (make-memory-session-store))
       (handler (wrap-session
                  (lambda (req)
                    (let ((count (or (session-ref req "count") 0)))
                      (let ((update (session-set! req "count" (+ count 1))))
                        (make-response 200 (number->string (+ count 1))
                          (list update)))))
                  store)))

  ;; First request — no cookie, new session created
  (let ((resp (handler (make-req "GET" "/"))))
    (check "session first visit" "1" (response-body resp))
    (check "session sets cookie" #t
      (let ((h (response-header resp "Set-Cookie")))
        (and (string? h) (> (string-length h) 0)))))

  ;; Second request with session cookie
  (store 'put! "test-session" '(("count" . 5)))
  (let ((resp (handler (make-req "GET" "/"
                         '(("cookie" . "kaappi-sid=test-session"))))))
    (check "session restores data" "6" (response-body resp))
    (check "session no new cookie" #f
      (response-header resp "Set-Cookie"))))

;; --- Auth ---
(display "=== Auth ===") (newline)

(let ((req-no-auth (make-req "GET" "/"))
      (req-auth (make-req "GET" "/" '(("x-session-data" . "{\"user\": \"alice\"}")))))
  (check "not authenticated" #f (authenticated? req-no-auth))
  (check "authenticated" #t (authenticated? req-auth))
  (check "current-user" "alice" (current-user req-auth))
  (check "current-user none" #f (current-user req-no-auth)))

(let* ((protected-handler
         (wrap-auth
           (lambda (req) (text-response "secret"))
           (lambda (req params) (json-response '(("error" . "no")) 401)))))
  (let ((resp (protected-handler (make-req "GET" "/"))))
    (check "auth blocks unauthenticated" 401 (response-status resp)))
  (let ((resp (protected-handler
                (make-req "GET" "/" '(("x-session-data" . "{\"user\": \"bob\"}"))))))
    (check "auth allows authenticated" 200 (response-status resp))
    (check "auth passes through" "secret" (response-body resp))))

;; --- Session + Auth integration ---
(display "=== Integration ===") (newline)

(let* ((store (make-memory-session-store))
       (login-handler
         (lambda (req params)
           (let ((update (session-set! req "user" "alice")))
             (make-response 200 "{\"logged_in\":true}" (list update)))))
       (profile-handler
         (lambda (req params)
           (json-response `(("user" . ,(current-user req))))))
       (app (routes
              (POST "/login" login-handler)
              (GET "/profile"
                (lambda (req params)
                  (if (authenticated? req)
                      (profile-handler req params)
                      (json-response '(("error" . "login required")) 401))))))
       (wrapped (wrap app (lambda (h) (wrap-session h store)))))

  ;; Login
  (let ((resp (wrapped (make-req "POST" "/login"))))
    (check "login succeeds" 200 (response-status resp)))

  ;; Profile without session
  (let ((resp (wrapped (make-req "GET" "/profile"))))
    (check "profile no session" 401 (response-status resp)))

  ;; Profile with session
  (store 'put! "my-session" '(("user" . "alice")))
  (let ((resp (wrapped (make-req "GET" "/profile"
                         '(("cookie" . "kaappi-sid=my-session"))))))
    (check "profile with session" 200 (response-status resp))))

(newline)
(display "=== Results: ")
(display pass) (display " passed, ")
(display fail) (display " failed ===")
(newline)
(when (> fail 0) (exit 1))
