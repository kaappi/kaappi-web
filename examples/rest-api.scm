;;; REST API — User management with PostgreSQL + Redis cache
;;; Rewritten with (kaappi web) framework.
;;;
;;; Compare with kaappi-examples/rest-api/app.scm (166 lines)
;;; to see how the framework eliminates boilerplate.

(import (scheme base) (scheme write)
        (kaappi web) (kaappi pg) (kaappi redis) (kaappi json))

;; --- Setup ---

(define db (pg-connect "dbname=kaappi_demo"))
(define cache (redis-connect "127.0.0.1" 6379))

(pg-exec db "CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT now()
)")

;; --- Helpers ---

(define (row->user row)
  `(("id"         . ,(vector-ref row 0))
    ("name"       . ,(vector-ref row 1))
    ("email"      . ,(vector-ref row 2))
    ("created_at" . ,(vector-ref row 3))))

(define (cache-key id) (string-append "user:" (number->string id)))

(define (get-user-cached id)
  (let ((cached (redis-get cache (cache-key id))))
    (if cached
        (json-read-string cached)
        (let ((rows (pg-query db
                      "SELECT id, name, email, created_at::text FROM users WHERE id = $1"
                      id)))
          (if (null? rows) #f
              (let ((user (row->user (car rows))))
                (redis-set cache (cache-key id) (json-write-string user))
                (redis-expire cache (cache-key id) 60)
                user))))))

;; --- Routes ---

(define app
  (routes
    (GET "/health"
      (lambda (req params)
        (json-response '(("status" . "ok")))))

    (GET "/users"
      (lambda (req params)
        (let ((rows (pg-query db
                      "SELECT id, name, email, created_at::text FROM users ORDER BY id")))
          (json-response (map row->user rows)))))

    (GET "/users/:id"
      (lambda (req params)
        (let ((user (get-user-cached (param/number params "id"))))
          (if user
              (json-response user)
              (json-response '(("error" . "User not found")) 404)))))

    (POST "/users"
      (lambda (req params)
        (let* ((body (request-json req))
               (name  (cdr (or (assoc "name" body) '("" . ""))))
               (email (cdr (or (assoc "email" body) '("" . "")))))
          (if (or (equal? name "") (equal? email ""))
              (json-response '(("error" . "name and email required")) 400)
              (let ((rows (pg-query db
                            "INSERT INTO users (name, email) VALUES ($1, $2)
                             RETURNING id, name, email, created_at::text"
                            name email)))
                (json-response (row->user (car rows)) 201))))))

    (DELETE "/users/:id"
      (lambda (req params)
        (let ((id (param/number params "id")))
          (let ((n (pg-exec db "DELETE FROM users WHERE id = $1" id)))
            (redis-del cache (cache-key id))
            (if (> n 0)
                (json-response '(("deleted" . #t)))
                (json-response '(("error" . "User not found")) 404))))))))

;; --- Start ---

(serve
  (wrap app
    wrap-json-body
    wrap-logging
    (wrap-cors "*")
    wrap-errors)
  8080)
