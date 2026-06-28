;;; (kaappi web) — Web framework for Kaappi Scheme
;;;
;;; Ring/Compojure-inspired: handlers are functions, middleware are
;;; higher-order functions, routes are data.

(define-library (kaappi web)
  (import (scheme base) (scheme write) (scheme char) (scheme cxr) (scheme time)
          (kaappi http) (kaappi json))
  (export ;; Routing
          routes GET POST PUT DELETE PATCH HEAD
          ;; Response helpers
          json-response text-response html-response
          redirect no-content
          ;; Request utilities
          param param/number request-json
          ;; Cookies
          request-cookies request-cookie
          set-cookie with-cookie
          ;; Sessions
          make-memory-session-store
          wrap-session session-ref session-set! session-delete!
          session-destroy! session-id
          ;; Auth
          wrap-auth authenticated? current-user
          ;; Middleware
          wrap wrap-json-body wrap-logging wrap-cors wrap-errors
          ;; Server
          serve serve-prefork)
  (begin

    ;; =================================================================
    ;; Path matching
    ;; =================================================================

    (define (split-path path)
      (let ((len (string-length path)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((= i len)
             (reverse (if (> i start)
                          (cons (substring path start i) acc)
                          acc)))
            ((char=? (string-ref path i) #\/)
             (if (> i start)
                 (loop (+ i 1) (+ i 1) (cons (substring path start i) acc))
                 (loop (+ i 1) (+ i 1) acc)))
            (else (loop (+ i 1) start acc))))))

    (define (param-segment? seg)
      (and (> (string-length seg) 1)
           (char=? (string-ref seg 0) #\:)))

    (define (param-name seg)
      (substring seg 1 (string-length seg)))

    (define (match-path pattern path)
      (let ((pat-segs (split-path pattern))
            (path-segs (split-path path)))
        (if (not (= (length pat-segs) (length path-segs)))
            #f
            (let loop ((ps pat-segs) (rs path-segs) (params '()))
              (cond
                ((null? ps) (reverse params))
                ((param-segment? (car ps))
                 (loop (cdr ps) (cdr rs)
                       (cons (cons (param-name (car ps)) (car rs)) params)))
                ((equal? (car ps) (car rs))
                 (loop (cdr ps) (cdr rs) params))
                (else #f))))))

    ;; =================================================================
    ;; Route descriptors
    ;; =================================================================

    (define (make-route method pattern handler)
      (list method pattern handler))

    (define (route-method r) (car r))
    (define (route-pattern r) (cadr r))
    (define (route-handler r) (caddr r))

    (define (try-route route request)
      (let ((method (request-method request))
            (path   (request-path request)))
        (if (equal? method (route-method route))
            (let ((params (match-path (route-pattern route) path)))
              (if params
                  ((route-handler route) request params)
                  #f))
            #f)))

    (define (routes . route-list)
      (lambda (request)
        (let loop ((rs route-list))
          (if (null? rs)
              (json-response '(("error" . "Not found")) 404)
              (let ((result (try-route (car rs) request)))
                (if result result (loop (cdr rs))))))))

    ;; Route constructor shortcuts
    (define (GET pattern handler)    (make-route "GET" pattern handler))
    (define (POST pattern handler)   (make-route "POST" pattern handler))
    (define (PUT pattern handler)    (make-route "PUT" pattern handler))
    (define (DELETE pattern handler) (make-route "DELETE" pattern handler))
    (define (PATCH pattern handler)  (make-route "PATCH" pattern handler))
    (define (HEAD pattern handler)   (make-route "HEAD" pattern handler))

    ;; =================================================================
    ;; Response helpers
    ;; =================================================================

    (define (json-response data . args)
      (let ((status (if (pair? args) (car args) 200)))
        (make-response status (json-write-string data)
          '(("Content-Type" . "application/json")))))

    (define (text-response text . args)
      (let ((status (if (pair? args) (car args) 200)))
        (make-response status text
          '(("Content-Type" . "text/plain; charset=utf-8")))))

    (define (html-response html . args)
      (let ((status (if (pair? args) (car args) 200)))
        (make-response status html
          '(("Content-Type" . "text/html; charset=utf-8")))))

    (define (redirect location . args)
      (let ((status (if (pair? args) (car args) 302)))
        (make-response status ""
          (list (cons "Location" location)))))

    (define (no-content)
      (make-response 204 ""))

    ;; =================================================================
    ;; Request utilities
    ;; =================================================================

    (define (param params name)
      (let ((pair (assoc name params)))
        (if pair (cdr pair) #f)))

    (define (param/number params name)
      (let ((v (param params name)))
        (if v (string->number v) #f)))

    (define (request-json req)
      (let ((h (request-header req "x-parsed-json")))
        (if h
            (json-read-string h)
            (let ((body (request-body req)))
              (if (equal? body "")
                  '()
                  (json-read-string body))))))

    ;; =================================================================
    ;; Middleware
    ;; =================================================================

    (define (wrap handler . middlewares)
      (let loop ((h handler) (mws middlewares))
        (if (null? mws)
            h
            (loop ((car mws) h) (cdr mws)))))

    ;; --- wrap-json-body: parse JSON request bodies ---

    (define (json-content-type? req)
      (let ((ct (or (request-header req "content-type") "")))
        (string-contains ct "application/json")))

    (define (string-contains haystack needle)
      (let ((hlen (string-length haystack))
            (nlen (string-length needle)))
        (if (> nlen hlen) #f
            (let loop ((i 0))
              (cond ((> (+ i nlen) hlen) #f)
                    ((equal? (substring haystack i (+ i nlen)) needle) #t)
                    (else (loop (+ i 1))))))))

    (define (wrap-json-body handler)
      (lambda (request)
        (if (and (json-content-type? request)
                 (not (equal? (request-body request) "")))
            (let* ((parsed (json-write-string (json-read-string (request-body request))))
                   (new-headers (cons (cons "x-parsed-json" (request-body request))
                                      (request-headers request)))
                   (new-req (make-http-request
                              (request-method request)
                              (request-path request)
                              (request-query request)
                              (request-version request)
                              new-headers
                              (request-body request))))
              (handler new-req))
            (handler request))))

    ;; --- wrap-logging ---

    (define (wrap-logging handler)
      (lambda (request)
        (display (request-method request))
        (display " ")
        (display (request-path request))
        (let ((q (request-query request)))
          (when (and q (not (equal? q "")))
            (display "?") (display q)))
        (newline)
        (handler request)))

    ;; --- wrap-cors ---

    (define (wrap-cors origin)
      (lambda (handler)
        (lambda (request)
          (if (equal? (request-method request) "OPTIONS")
              (make-response 204 ""
                (list (cons "Access-Control-Allow-Origin" origin)
                      (cons "Access-Control-Allow-Methods"
                            "GET, POST, PUT, DELETE, PATCH, OPTIONS")
                      (cons "Access-Control-Allow-Headers"
                            "Content-Type, Authorization")
                      (cons "Access-Control-Max-Age" "86400")))
              (let ((resp (handler request)))
                (make-response
                  (response-status resp)
                  (response-body resp)
                  (cons (cons "Access-Control-Allow-Origin" origin)
                        (response-headers resp))))))))

    ;; --- wrap-errors ---

    (define (wrap-errors handler)
      (lambda (request)
        (guard (exn
                (#t (json-response
                      '(("error" . "Internal server error")) 500)))
          (handler request))))

    ;; =================================================================
    ;; Cookies
    ;; =================================================================

    (define (request-cookies req)
      (let ((header (request-header req "cookie")))
        (if (not header) '()
            (parse-cookie-header header))))

    (define (parse-cookie-header s)
      (let ((len (string-length s)))
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((= i len)
             (reverse (let ((pair (parse-one-cookie (substring s start i))))
                        (if pair (cons pair acc) acc))))
            ((char=? (string-ref s i) #\;)
             (let ((pair (parse-one-cookie (substring s start i))))
               (loop (+ i 1) (+ i 1)
                     (if pair (cons pair acc) acc))))
            (else (loop (+ i 1) start acc))))))

    (define (cookie-char-pos s ch)
      (let ((len (string-length s)))
        (let loop ((i 0))
          (cond ((= i len) #f)
                ((char=? (string-ref s i) ch) i)
                (else (loop (+ i 1)))))))

    (define (parse-one-cookie s)
      (let ((trimmed (string-trim-both s)))
        (let ((eq (cookie-char-pos trimmed #\=)))
          (if eq
              (cons (substring trimmed 0 eq)
                    (substring trimmed (+ eq 1) (string-length trimmed)))
              #f))))

    (define (string-trim-both s)
      (let ((len (string-length s)))
        (let ((start (let loop ((i 0))
                       (if (and (< i len) (char=? (string-ref s i) #\space))
                           (loop (+ i 1)) i)))
              (end (let loop ((i len))
                     (if (and (> i 0) (char=? (string-ref s (- i 1)) #\space))
                         (loop (- i 1)) i))))
          (if (>= start end) "" (substring s start end)))))

    (define (request-cookie req name)
      (let ((pair (assoc name (request-cookies req))))
        (if pair (cdr pair) #f)))

    (define (set-cookie name value . args)
      (let ((opts (if (pair? args) (car args) '())))
        (let ((out (open-output-string)))
          (display name out) (display "=" out) (display value out)
          (for-each
            (lambda (opt)
              (let ((key (car opt)) (val (cdr opt)))
                (cond
                  ((eq? key 'path) (display "; Path=" out) (display val out))
                  ((eq? key 'max-age) (display "; Max-Age=" out) (display val out))
                  ((eq? key 'domain) (display "; Domain=" out) (display val out))
                  ((eq? key 'same-site) (display "; SameSite=" out) (display val out))
                  ((and (eq? key 'http-only) val) (display "; HttpOnly" out))
                  ((and (eq? key 'secure) val) (display "; Secure" out)))))
            opts)
          (cons "Set-Cookie" (get-output-string out)))))

    (define (with-cookie resp name value . args)
      (let ((cookie (apply set-cookie name value args)))
        (make-response (response-status resp) (response-body resp)
          (cons cookie (response-headers resp)))))

    ;; =================================================================
    ;; Sessions
    ;; =================================================================

    (define *session-cookie-name* "kaappi-sid")

    (define (generate-session-id)
      (let ((out (open-output-string))
            (chars "0123456789abcdef"))
        (let loop ((i 0) (seed (modulo (exact (round (* (current-second) 1000000))) 2147483647)))
          (when (< i 32)
            (let ((next (modulo (+ (* seed 1103515245) 12345) 2147483648)))
              (write-char (string-ref chars (modulo next 16)) out)
              (loop (+ i 1) next))))
        (get-output-string out)))

    ;; In-memory session store
    (define (make-memory-session-store)
      (let ((sessions '()))
        (define (get sid)
          (let ((pair (assoc sid sessions)))
            (if pair (cdr pair) #f)))
        (define (put! sid data)
          (set! sessions
            (cons (cons sid data)
                  (let remove ((ss sessions))
                    (cond ((null? ss) '())
                          ((equal? (caar ss) sid) (remove (cdr ss)))
                          (else (cons (car ss) (remove (cdr ss)))))))))
        (define (del! sid)
          (set! sessions
            (let remove ((ss sessions))
              (cond ((null? ss) '())
                    ((equal? (caar ss) sid) (remove (cdr ss)))
                    (else (cons (car ss) (remove (cdr ss))))))))
        (lambda (op . args)
          (cond
            ((eq? op 'get) (get (car args)))
            ((eq? op 'put!) (put! (car args) (cadr args)))
            ((eq? op 'del!) (del! (car args)))))))

    (define (wrap-session handler . args)
      (let ((store (if (pair? args) (car args) (make-memory-session-store))))
        (lambda (request)
          (let* ((sid (or (request-cookie request *session-cookie-name*)
                          (generate-session-id)))
                 (is-new (not (request-cookie request *session-cookie-name*)))
                 (data (or (store 'get sid) '()))
                 (session-header (string-append "sid=" sid))
                 (data-header (json-write-string data))
                 (new-headers (cons (cons "x-session-id" session-header)
                                    (cons (cons "x-session-data" data-header)
                                          (request-headers request))))
                 (new-req (make-http-request
                            (request-method request) (request-path request)
                            (request-query request) (request-version request)
                            new-headers (request-body request)))
                 (resp (handler new-req))
                 (updated-data (let ((h (response-header resp "x-session-update")))
                                 (if h (json-read-string h) data))))
            (store 'put! sid updated-data)
            (let ((clean-headers
                    (let remove ((hs (response-headers resp)))
                      (cond ((null? hs) '())
                            ((equal? (caar hs) "x-session-update")
                             (remove (cdr hs)))
                            (else (cons (car hs) (remove (cdr hs))))))))
              (if is-new
                  (make-response (response-status resp) (response-body resp)
                    (cons (set-cookie *session-cookie-name* sid
                                 '((path . "/") (http-only . #t)))
                          clean-headers))
                  (make-response (response-status resp) (response-body resp)
                    clean-headers)))))))

    (define (session-ref req key)
      (let ((data-str (request-header req "x-session-data")))
        (if data-str
            (let ((data (json-read-string data-str)))
              (let ((pair (assoc key data)))
                (if pair (cdr pair) #f)))
            #f)))

    (define (session-id req)
      (let ((h (request-header req "x-session-id")))
        (if h (substring h 4 (string-length h)) #f)))

    (define (session-set! req key value)
      (let* ((data-str (or (request-header req "x-session-data") "{}"))
             (data (json-read-string data-str))
             (updated (cons (cons key value)
                            (let remove ((pairs data))
                              (cond ((null? pairs) '())
                                    ((equal? (caar pairs) key) (remove (cdr pairs)))
                                    (else (cons (car pairs) (remove (cdr pairs)))))))))
        (cons "x-session-update" (json-write-string updated))))

    (define (session-delete! req key)
      (let* ((data-str (or (request-header req "x-session-data") "{}"))
             (data (json-read-string data-str))
             (updated (let remove ((pairs data))
                        (cond ((null? pairs) '())
                              ((equal? (caar pairs) key) (remove (cdr pairs)))
                              (else (cons (car pairs) (remove (cdr pairs))))))))
        (cons "x-session-update" (json-write-string updated))))

    (define (session-destroy! req)
      (cons "x-session-update" "{}"))

    ;; =================================================================
    ;; Auth
    ;; =================================================================

    (define (authenticated? req)
      (not (eq? (session-ref req "user") #f)))

    (define (current-user req)
      (session-ref req "user"))

    (define (wrap-auth handler . args)
      (let ((on-unauth (if (pair? args) (car args)
                           (lambda (req params)
                             (json-response '(("error" . "Unauthorized")) 401)))))
        (lambda (request)
          (if (authenticated? request)
              (handler request)
              (if (procedure? on-unauth)
                  (on-unauth request '())
                  (redirect on-unauth))))))

    ;; =================================================================
    ;; Server
    ;; =================================================================

    (define (serve handler port . args)
      (let ((host (if (pair? args) (car args) "0.0.0.0")))
        (http-listen
          (lambda (request) (handler request))
          port host)))

    (define (serve-prefork handler port workers . args)
      (let ((host (if (pair? args) (car args) "0.0.0.0")))
        (http-listen-prefork
          (lambda (request) (handler request))
          port workers host)))))
