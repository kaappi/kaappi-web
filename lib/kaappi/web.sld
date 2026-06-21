;;; (kaappi web) — Web framework for Kaappi Scheme
;;;
;;; Ring/Compojure-inspired: handlers are functions, middleware are
;;; higher-order functions, routes are data.

(define-library (kaappi web)
  (import (scheme base) (scheme write) (scheme char) (scheme cxr)
          (kaappi http) (kaappi json))
  (export ;; Routing
          routes GET POST PUT DELETE PATCH HEAD
          ;; Response helpers
          json-response text-response html-response
          redirect no-content
          ;; Request utilities
          param param/number request-json
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
