[English](http.md)

# HTTP Server Reference

## Types

| Type | Description |
|------|-------------|
| `HttpRequest` | Opaque handle for an incoming HTTP request |
| `HttpResponse` | Opaque handle for an outgoing HTTP response |

Both types are opaque pointers. `HttpRequest` is provided by the server framework; `HttpResponse` is created via `http_response()`.

## Functions (from `std.http`)

These functions require explicit import:

```python
from std.http import http_listen, http_method, http_path, http_header, http_body, http_response
```

### Server

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit` | Starts an HTTP server on the given address. Blocks in an accept loop, calling `handler` for each request. |

### Request Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_method` | `(req: HttpRequest) -> str` | Returns the HTTP method (e.g., `"GET"`, `"POST"`). |
| `http_path` | `(req: HttpRequest) -> str` | Returns the request path (e.g., `"/hello"`). |
| `http_header` | `(req: HttpRequest, key: str) -> Option<str>` | Returns the value of a request header (case-insensitive lookup). Returns `None` if not found. |
| `http_body` | `(req: HttpRequest) -> str` | Returns the request body as a string. |

### Response Builder

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_response` | `(status: int, headers: Map<str, str>, body: str) -> HttpResponse` | Creates an HTTP response with the given status code, headers, and body. |

## Usage Example

### Basic HTTP Server

```python
from std.http import http_listen, http_method, http_path, http_header, http_body, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    let method = http_method(req)
    let path = http_path(req)
    if path == "/hello":
        return http_response(200, {"Content-Type": "text/plain"}, "Hello, World!")
    if path == "/echo":
        let body = http_body(req)
        return http_response(200, {"Content-Type": "text/plain"}, body)
    return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### Non-blocking Server with `spawn`

```python
from std.http import http_listen, http_path, http_response

fn start_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        let path = http_path(req)
        if path == "/api/health":
            return http_response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

let t: Task<str> = spawn start_server(8080)
# Server runs in background thread
```

### Reading Headers

```python
from std.http import http_listen, http_header, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    match http_header(req, "Authorization"):
        case Some(token):
            return http_response(200, {"Content-Type": "text/plain"}, "Authenticated: " + token)
        case None:
            return http_response(401, {"Content-Type": "text/plain"}, "Unauthorized")
)
```

## Behavior

- `http_listen()` binds to the address, starts listening, and enters an infinite accept loop.
- Each accepted connection reads one HTTP/1.1 request, calls the handler, sends the response, and closes the connection.
- `Content-Length` is automatically added to the response if not provided in the headers map.
- The server supports HTTP/1.1 with `Content-Length`-based body reading.
- Header lookup via `http_header()` is case-insensitive.

## Supported Status Codes

The following status codes have standard reason phrases:

| Code | Reason |
|------|--------|
| 200 | OK |
| 201 | Created |
| 204 | No Content |
| 301 | Moved Permanently |
| 302 | Found |
| 304 | Not Modified |
| 400 | Bad Request |
| 401 | Unauthorized |
| 403 | Forbidden |
| 404 | Not Found |
| 405 | Method Not Allowed |
| 500 | Internal Server Error |

Other status codes use `"Unknown"` as the reason phrase.

## Error Handling

- `http_listen()` raises a runtime error if `bind()` fails (e.g., port already in use).
- Malformed requests (incomplete request line) are silently skipped and the connection is closed.
- The handler function must always return an `HttpResponse` — there is no default response.
