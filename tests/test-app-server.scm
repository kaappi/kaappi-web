;; Test server for integration tests — used by test-app.sh
(import (scheme base) (kaappi web))

(define app
  (routes
    (GET "/" (lambda (req params) (text-response "Hello, World!")))

    (GET "/json" (lambda (req params)
                   (json-response '(("ok" . #t)))))

    (GET "/users/:id" (lambda (req params)
                        (json-response
                          `(("id" . ,(param/number params "id"))
                            ("name" . "Alice")))))

    (GET "/users/:uid/posts/:pid"
      (lambda (req params)
        (json-response
          `(("user" . ,(param/number params "uid"))
            ("post" . ,(param/number params "pid"))))))

    (POST "/echo" (lambda (req params)
                    (let ((body (request-json req)))
                      (json-response body))))

    (DELETE "/items/:id" (lambda (req params)
                           (json-response '(("deleted" . #t)))))))

(serve
  (wrap app
    wrap-json-body
    wrap-logging
    wrap-errors)
  19877)
