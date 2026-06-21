# kaappi-web

Web framework for [Kaappi Scheme](https://github.com/kaappi/kaappi).
Ring/Compojure-inspired: handlers are functions, middleware are
higher-order functions, routes are data.

Pure Scheme — no build step. Depends on
[kaappi-http](https://github.com/kaappi/kaappi-http) and
[kaappi-json](https://github.com/kaappi/kaappi-json).

## Quick Start

```scheme
(import (kaappi web))

(define app
  (routes
    (GET "/" (lambda (req params)
              (text-response "Hello, World!")))

    (GET "/users/:id" (lambda (req params)
                        (json-response
                          `(("id" . ,(param/number params "id"))))))

    (POST "/users" (lambda (req params)
                     (let ((body (request-json req)))
                       (json-response body 201))))))

(serve
  (wrap app wrap-json-body wrap-logging wrap-errors)
  8080)
```

```bash
kaappi --lib-path /path/to/kaappi-http/lib \
       --lib-path /path/to/kaappi-json/lib \
       --lib-path /path/to/kaappi-web/lib \
       app.scm
```

## Routing

```scheme
(routes
  (GET    "/path"       handler)
  (POST   "/path"       handler)
  (PUT    "/path"       handler)
  (DELETE "/path"       handler)
  (PATCH  "/path"       handler)
  (HEAD   "/path"       handler))
```

Path parameters with `:name`:

```scheme
(GET "/users/:id/posts/:pid"
  (lambda (req params)
    ;; params = (("id" . "42") ("pid" . "7"))
    (param params "id")           ; => "42"
    (param/number params "id")    ; => 42
    ...))
```

Automatic 404 when no route matches.

## Response Helpers

| Procedure | Description |
|---|---|
| `(json-response data [status])` | JSON with `application/json` |
| `(text-response text [status])` | Plain text |
| `(html-response html [status])` | HTML |
| `(redirect url [status])` | 302 redirect (or custom) |
| `(no-content)` | 204 No Content |

## Middleware

Middleware are `(handler → handler)` functions, composed with `wrap`:

```scheme
(wrap app
  wrap-json-body          ; parse JSON request bodies
  wrap-logging            ; log requests to stdout
  (wrap-cors "*")         ; add CORS headers
  wrap-errors)            ; catch exceptions → 500
```

### Built-in

| Middleware | Description |
|---|---|
| `wrap-json-body` | Parses `application/json` bodies; access via `(request-json req)` |
| `wrap-logging` | Prints `METHOD /path` to stdout |
| `(wrap-cors origin)` | CORS headers + OPTIONS preflight handling |
| `wrap-errors` | Catches exceptions, returns `{"error":"Internal server error"}` |

### Custom middleware

```scheme
(define (wrap-auth handler)
  (lambda (request)
    (if (request-header request "authorization")
        (handler request)
        (json-response '(("error" . "Unauthorized")) 401))))
```

## Tests

```bash
# Offline routing tests
kaappi --lib-path ... tests/test-routing.scm

# Integration tests (starts server, curls it)
bash tests/test-app.sh
```

## License

MIT
