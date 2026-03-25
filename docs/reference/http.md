[English](http.md)

# HTTP Reference

## Types

| Type | Description |
|------|-------------|
| `HttpRequest` | Opaque handle for an incoming HTTP request |
| `HttpResponse` | Opaque handle for an outgoing HTTP response |
| `HttpClientResponse` | Opaque handle for an HTTP client response |

`HttpRequest` is provided by the server framework. `HttpResponse` is created via `http_response()`. `HttpClientResponse` is returned by client functions (`http_get`, `http_post`, `http_request`).

## Functions (from `std.http`)

These functions require explicit import:

```python
from std.http import http_listen, http_method, http_path, http_header, http_body, http_query, http_query_all, http_cookie, http_cookies, http_response
```

### Server

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit` | Starts an HTTP server on the given address. Blocks in an accept loop, calling `handler` for each request. |
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int) -> Unit` | Starts an HTTP server that stops after processing `max_requests` requests. Enables `spawn` + `join` lifecycle management. |

### Request Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_method` | `(req: HttpRequest) -> str` | Returns the HTTP method (e.g., `"GET"`, `"POST"`). |
| `http_path` | `(req: HttpRequest) -> str` | Returns the request path without the query string (e.g., `"/search"` for `"/search?q=hello"`). |
| `http_header` | `(req: HttpRequest, key: str) -> Option<str>` | Returns the value of a request header (case-insensitive lookup). Returns `None` if not found. |
| `http_body` | `(req: HttpRequest) -> str` | Returns the request body as a string. |
| `http_query` | `(req: HttpRequest, key: str) -> Option<str>` | Returns the value of a query parameter. Returns `None` if not found. Values are automatically URL-decoded. |
| `http_query_all` | `(req: HttpRequest) -> Map<str, str>` | Returns all query parameters as a map. Keys and values are automatically URL-decoded. |
| `http_cookie` | `(req: HttpRequest, name: str) -> Option<str>` | Returns the value of a cookie by name. Returns `None` if not found. |
| `http_cookies` | `(req: HttpRequest) -> Map<str, str>` | Returns all cookies as a map. |

### Response Builder

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_response` | `(status: int, headers: Map<str, str>, body: str) -> HttpResponse` | Creates an HTTP response with the given status code, headers, and body. |

## Usage Example

### Basic HTTP Server

```python
from std.http import http_listen, http_method, http_path, http_header, http_body, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    method = http_method(req)
    path = http_path(req)
    if path == "/hello":
        return http_response(200, {"Content-Type": "text/plain"}, "Hello, World!")
    if path == "/echo":
        body = http_body(req)
        return http_response(200, {"Content-Type": "text/plain"}, body)
    return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### Non-blocking Server with `spawn`

```python
from std.http import http_listen, http_path, http_response

fn start_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        if path == "/api/health":
            return http_response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

t: Task<str> = spawn start_server(8080)
# Server runs in background thread
```

### Server with Request Limit (`max_requests`)

```python
from std.http import http_listen, http_path, http_response, http_get, http_client_status, http_client_body

fn start_server(ready: Channel<int>) -> str:
    send(ready, 1)
    http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
        return http_response(200, {"Content-Type": "text/plain"}, "Hello!")
    , 1)  # Stop after 1 request
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn start_server(ready)
recv(ready)
sleep(100)

match http_get("http://127.0.0.1:8080/"):
    case Ok(resp):
        print(http_client_body(resp))  # "Hello!"
    case Err(e):
        print("error")

result = join(t)  # Server exits after 1 request; join completes
```

### Reading Query Parameters

```python
from std.http import http_listen, http_path, http_query, http_query_all, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    path = http_path(req)
    if path == "/search":
        match http_query(req, "q"):
            case Some(query):
                return http_response(200, {"Content-Type": "text/plain"}, "Search: " + query)
            case None:
                return http_response(400, {"Content-Type": "text/plain"}, "Missing query parameter: q")
    return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
)
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

### Reading Cookies

```python
from std.http import http_listen, http_cookie, http_cookies, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    match http_cookie(req, "session_id"):
        case Some(sid):
            return http_response(200, {"Content-Type": "text/plain"}, "Session: " + sid)
        case None:
            return http_response(401, {"Content-Type": "text/plain"}, "No session")
)
```

## Behavior

- `http_listen()` binds to the address, starts listening, and enters an accept loop.
- When called with 3 arguments, the accept loop runs indefinitely.
- When called with 4 arguments (`max_requests`), the server stops after processing the specified number of requests. `max_requests` must be a positive integer. This enables `spawn` + `join` lifecycle management. Malformed requests (silently skipped) do not count toward the limit.
- Each accepted connection reads one HTTP/1.1 request, calls the handler, sends the response, and closes the connection.
- `Content-Length` is automatically added to the response if not provided in the headers map.
- The server supports HTTP/1.1 with `Content-Length`-based body reading and `Transfer-Encoding: chunked` decoding.
- When `Transfer-Encoding: chunked` is present in a request, the body is automatically decoded and concatenated — Ry code receives the full body transparently.
- If both `Transfer-Encoding: chunked` and `Content-Length` are present, the request is rejected as malformed (per RFC 9112).
- To send a chunked response, include `"Transfer-Encoding": "chunked"` in the headers map passed to `http_response()`. The body will be encoded in chunked format automatically.
- Header lookup via `http_header()` is case-insensitive.
- `http_path()` returns the path without the query string. Query parameters are accessed separately via `http_query()` or `http_query_all()`.
- Query parameter values are automatically URL-decoded (`%20` → space, `+` → space).
- For duplicate query parameter keys, the first value is returned.
- `http_cookie()` and `http_cookies()` parse the `Cookie` header by splitting on `;`, then splitting each pair on the first `=`. Leading and trailing whitespace is trimmed from names and values.
- For duplicate cookie names, the first value is returned.
- Cookie values may contain `=` characters (only the first `=` separates the name from the value).

## Supported Status Codes

The following status codes have standard reason phrases (RFC 9110):

| Code | Reason |
|------|--------|
| 100 | Continue |
| 101 | Switching Protocols |
| 200 | OK |
| 201 | Created |
| 202 | Accepted |
| 203 | Non-Authoritative Information |
| 204 | No Content |
| 205 | Reset Content |
| 206 | Partial Content |
| 300 | Multiple Choices |
| 301 | Moved Permanently |
| 302 | Found |
| 303 | See Other |
| 304 | Not Modified |
| 307 | Temporary Redirect |
| 308 | Permanent Redirect |
| 400 | Bad Request |
| 401 | Unauthorized |
| 403 | Forbidden |
| 404 | Not Found |
| 405 | Method Not Allowed |
| 406 | Not Acceptable |
| 408 | Request Timeout |
| 409 | Conflict |
| 410 | Gone |
| 411 | Length Required |
| 413 | Content Too Large |
| 414 | URI Too Long |
| 415 | Unsupported Media Type |
| 416 | Range Not Satisfiable |
| 417 | Expectation Failed |
| 422 | Unprocessable Content |
| 426 | Upgrade Required |
| 429 | Too Many Requests |
| 500 | Internal Server Error |
| 501 | Not Implemented |
| 502 | Bad Gateway |
| 503 | Service Unavailable |
| 504 | Gateway Timeout |
| 505 | HTTP Version Not Supported |

Other status codes use `"Unknown"` as the reason phrase.

## HTTP Client

### Client Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_get` | `(url: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP GET request to the given URL. |
| `http_post` | `(url: str, body: str, headers: Map<str, str>) -> Result<HttpClientResponse, Error>` | Sends an HTTP POST request with body and headers. |
| `http_request` | `(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP request with a custom method. |

### Response Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_client_status` | `(resp: HttpClientResponse) -> int` | Returns the HTTP status code. |
| `http_client_body` | `(resp: HttpClientResponse) -> str` | Returns the response body as a string. |
| `http_client_header` | `(resp: HttpClientResponse, key: str) -> Option<str>` | Returns the value of a response header (case-insensitive). Returns `None` if not found. |
| `http_client_response_free` | `(resp: HttpClientResponse) -> Unit` | Frees the response and its associated memory. Call when done with the response. |

### Client Usage Example

```python
from std.http import http_get, http_post, http_client_status, http_client_body, http_client_header

# Simple GET request
match http_get("http://example.com/api/data"):
    case Ok(resp):
        status = http_client_status(resp)
        body = http_client_body(resp)
        print(to_str(status) + ": " + body)
    case Err(e):
        print("Request failed")

# POST request with body and headers
headers: Map<str, str> = {"Content-Type": "application/json"}
match http_post("http://example.com/api/data", "{\"key\": \"value\"}", headers):
    case Ok(resp):
        print(http_client_body(resp))
    case Err(e):
        print("Request failed")
```

### Client Behavior

- Only `http://` URLs are supported. `https://` returns `Err` (TLS support is planned).
- The `Host` header is automatically added based on the URL.
- `Connection: close` is always sent; each request uses a separate TCP connection.
- `Content-Length` is always added automatically (including `0` for empty bodies). User-provided `Content-Length` headers are overridden with the correct value.
- Response body reading supports `Content-Length`, `Transfer-Encoding: chunked`, or read-until-close.
- Response header lookup is case-insensitive.
- Connection timeout is 5 seconds; receive timeout is 30 seconds.
- Returns `Err` on connection failure, invalid URL, or malformed response.
- `HttpClientResponse` owns allocated memory (headers, body). Call `http_client_response_free()` when done to avoid memory leaks.

### Redirect Behavior

HTTP client functions automatically follow redirect responses (3xx with `Location` header).

- **Supported status codes**: 301, 302, 303, 307, 308
- **Maximum redirects**: 10 (returns `Err` if exceeded)
- **Method conversion** (per RFC 9110):
  - 301, 302: `POST` is changed to `GET` (body is dropped); other methods are preserved
  - 303: Method is always changed to `GET` (body is dropped)
  - 307, 308: Method and body are preserved
- **URL resolution**: Absolute URLs, protocol-relative (`//...`), absolute paths (`/...`), and relative paths in `Location` headers are supported
- User-provided headers are re-sent on each redirect hop, except sensitive headers (`Authorization`, `Proxy-Authorization`, `Cookie`) which are stripped on cross-origin redirects (different host or port)
- If the `Location` header is missing or empty, the redirect response is returned as-is
- The caller receives only the final response after all redirects are followed

## Error Handling

- `http_listen()` raises a runtime error if `bind()` fails (e.g., port already in use).
- Malformed requests (incomplete request line) are silently skipped and the connection is closed.
- The handler function must always return an `HttpResponse` — there is no default response.
- Client functions return `Result<HttpClientResponse, Error>` — use `match` to handle success and failure.
