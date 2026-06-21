;; Offline routing tests (no server needed)
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

;; --- Path matching ---
(display "=== Path Matching ===") (newline)

;; Use match-path indirectly via route matching

;; --- Route matching ---
(display "=== Route Matching ===") (newline)

(define (make-test-request method path)
  (make-http-request method path "" "HTTP/1.1" '() ""))

(define test-app
  (routes
    (GET "/" (lambda (req params)
              (text-response "home")))
    (GET "/users" (lambda (req params)
                    (json-response '(("users" . ())))))
    (GET "/users/:id" (lambda (req params)
                        (json-response
                          `(("id" . ,(param params "id"))))))
    (POST "/users" (lambda (req params)
                     (json-response '(("created" . #t)) 201)))
    (DELETE "/users/:id" (lambda (req params)
                           (json-response '(("deleted" . #t)))))
    (GET "/users/:id/posts/:pid" (lambda (req params)
                                   (json-response
                                     `(("user" . ,(param params "id"))
                                       ("post" . ,(param params "pid"))))))))

;; GET /
(let ((resp (test-app (make-test-request "GET" "/"))))
  (check "GET / status" 200 (response-status resp))
  (check "GET / body" "home" (response-body resp)))

;; GET /users
(let ((resp (test-app (make-test-request "GET" "/users"))))
  (check "GET /users status" 200 (response-status resp))
  (check "GET /users json" "{\"users\":[]}" (response-body resp)))

;; GET /users/42
(let ((resp (test-app (make-test-request "GET" "/users/42"))))
  (check "GET /users/:id status" 200 (response-status resp))
  (check "GET /users/:id body" "{\"id\":\"42\"}" (response-body resp)))

;; POST /users
(let ((resp (test-app (make-test-request "POST" "/users"))))
  (check "POST /users status" 201 (response-status resp))
  (check "POST /users body" "{\"created\":true}" (response-body resp)))

;; DELETE /users/7
(let ((resp (test-app (make-test-request "DELETE" "/users/7"))))
  (check "DELETE /users/:id status" 200 (response-status resp)))

;; GET /users/5/posts/99 (multi-param)
(let ((resp (test-app (make-test-request "GET" "/users/5/posts/99"))))
  (check "multi-param status" 200 (response-status resp))
  (check "multi-param body" "{\"user\":\"5\",\"post\":\"99\"}" (response-body resp)))

;; 404 — no matching route
(let ((resp (test-app (make-test-request "GET" "/missing"))))
  (check "404 status" 404 (response-status resp)))

;; Method mismatch
(let ((resp (test-app (make-test-request "PUT" "/users"))))
  (check "method mismatch 404" 404 (response-status resp)))

;; --- Response helpers ---
(display "=== Response Helpers ===") (newline)

(let ((resp (json-response '(("a" . 1)))))
  (check "json-response status" 200 (response-status resp))
  (check "json-response body" "{\"a\":1}" (response-body resp))
  (check "json-response ct" "application/json"
    (response-header resp "Content-Type")))

(let ((resp (json-response '(("err" . "bad")) 400)))
  (check "json-response custom status" 400 (response-status resp)))

(let ((resp (text-response "hello")))
  (check "text-response body" "hello" (response-body resp)))

(let ((resp (html-response "<h1>Hi</h1>")))
  (check "html-response body" "<h1>Hi</h1>" (response-body resp)))

(let ((resp (redirect "/other")))
  (check "redirect status" 302 (response-status resp))
  (check "redirect location" "/other" (response-header resp "Location")))

(let ((resp (no-content)))
  (check "no-content status" 204 (response-status resp)))

;; --- Param helpers ---
(display "=== Param Helpers ===") (newline)

(let ((params '(("id" . "42") ("name" . "alice"))))
  (check "param found" "42" (param params "id"))
  (check "param missing" #f (param params "nope"))
  (check "param/number" 42 (param/number params "id"))
  (check "param/number missing" #f (param/number params "nope")))

;; --- Middleware ---
(display "=== Middleware ===") (newline)

;; wrap-errors catches exceptions
(let* ((bad-handler (lambda (req) (error "boom")))
       (safe (wrap-errors bad-handler))
       (resp (safe (make-test-request "GET" "/"))))
  (check "wrap-errors catches" 500 (response-status resp)))

;; wrap composes left-to-right
(let* ((log-output '())
       (mw1 (lambda (handler) (lambda (req) (set! log-output (cons 1 log-output)) (handler req))))
       (mw2 (lambda (handler) (lambda (req) (set! log-output (cons 2 log-output)) (handler req))))
       (app (wrap (lambda (req) (text-response "ok")) mw1 mw2))
       (resp (app (make-test-request "GET" "/"))))
  (check "wrap order" '(1 2) log-output)
  (check "wrap result" "ok" (response-body resp)))

(newline)
(display "=== Results: ")
(display pass) (display " passed, ")
(display fail) (display " failed ===")
(newline)
(when (> fail 0) (exit 1))
