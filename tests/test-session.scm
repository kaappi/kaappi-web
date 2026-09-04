;; Tests for cookies, sessions, and auth middleware
(import (scheme base) (scheme write) (kaappi http) (kaappi web))

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

;; --- Empty session data ({}) ---
(display "=== Empty object session data ===") (newline)

;; kaappi-json reads {} as the distinct json-empty-object value; the
;; session helpers must accept it wherever an alist is expected.
(let ((req (make-req "GET" "/" '(("x-session-data" . "{}")))))
  (check "session-ref on {} data" #f (session-ref req "user"))
  (check "authenticated with {} data" #f (authenticated? req))
  (check "request-json empty object" '()
    (request-json (make-req "POST" "/" '(("x-parsed-json" . "{}")) "")))
  (check "request-json empty object body" '()
    (request-json (make-req "POST" "/" '() "{}"))))

(check "session-set! on {} data" "{\"user\":\"alice\"}"
  (cdr (session-set! (make-req "GET" "/" '(("x-session-data" . "{}"))) "user" "alice")))

;; deleting the last key must put {} on the wire, not []
(check "session-delete! last key" "{}"
  (cdr (session-delete! (make-req "GET" "/"
                        '(("x-session-data" . "{\"user\":\"alice\"}")))
                        "user")))

;; the internal x-session-data header carries the same shape: a fresh
;; or emptied session is {}, not []
(check "fresh session data header is {}" "{}"
  (let* ((seen #f)
         (handler (wrap-session
                    (lambda (req)
                      (set! seen (cdr (assoc "x-session-data"
                                             (request-headers req))))
                      (make-response 200 "ok" '()))
                    (make-memory-session-store))))
    (handler (make-req "GET" "/"))
    seen))

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

;; --- Session ID uniqueness ---
;; Regression: generate-session-id used to derive every character from the
;; low 4 bits of an LCG whose nibble stream is a fixed period-16 cycle, so
;; only 16 distinct ids could ever exist and two requests collided ~1/16
;; of the time (the flaky "profile no session" nightly failure).
(display "=== Session IDs ===") (newline)

(define (response-sid resp)
  ;; "kaappi-sid=<32 hex chars>; Path=/; HttpOnly"
  (let ((h (response-header resp "Set-Cookie")))
    (and h (substring h 11 43))))

(define (hex-string-32? s)
  (and (string? s)
       (= (string-length s) 32)
       (let loop ((i 0))
         (or (= i 32)
             (and (memv (string-ref s i)
                        '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                          #\a #\b #\c #\d #\e #\f))
                  (loop (+ i 1)))))))

(let* ((store (make-memory-session-store))
       (handler (wrap-session (lambda (req) (make-response 200 "ok" '()))
                              store)))
  (let loop ((i 0) (sids '()) (dups 0) (malformed 0))
    (if (= i 300)
        (begin
          (check "300 fresh sessions get well-formed ids" 0 malformed)
          (check "300 fresh sessions get 300 distinct ids" 0 dups))
        (let ((sid (response-sid (handler (make-req "GET" "/")))))
          (loop (+ i 1)
                (cons sid sids)
                (if (member sid sids) (+ dups 1) dups)
                (if (hex-string-32? sid) malformed (+ malformed 1)))))))

(let loop ((i 0) (leaked 0))
  (if (= i 100)
      (check "cookie-less request never inherits a fresh login session (100x)"
             0 leaked)
      (let* ((store (make-memory-session-store))
             (app (routes
                    (POST "/login"
                      (lambda (req params)
                        (make-response 200 "ok"
                          (list (session-set! req "user" "alice")))))
                    (GET "/profile"
                      (lambda (req params)
                        (if (authenticated? req)
                            (json-response '(("user" . "alice")))
                            (json-response '(("error" . "login required")) 401))))))
             (wrapped (wrap app (lambda (h) (wrap-session h store)))))
        (wrapped (make-req "POST" "/login"))
        (loop (+ i 1)
              (if (= (response-status (wrapped (make-req "GET" "/profile"))) 200)
                  (+ leaked 1)
                  leaked)))))

(newline)
(display "=== Results: ")
(display pass) (display " passed, ")
(display fail) (display " failed ===")
(newline)
(when (> fail 0) (exit 1))
