# HTTP Reference

## Types

| Type | Description |
|------|-------------|
| `HttpRequest` | Opaque handle for an incoming HTTP request |
| `HttpResponse` | Opaque handle for an outgoing HTTP response |
| `HttpClientResponse` | Opaque handle for an HTTP client response |

`HttpRequest` is provided by the server framework. `HttpResponse` is created via `response()`. `HttpClientResponse` is returned by client functions (`http_get`, `http_post`, `http_request`).

## Functions (from `http`)

These functions require explicit import:

```ry
from http import listen, method, path, header, body, body_bytes, query, query_all, cookie, cookies, form_field, form_file, form_fields, response
```

### Server

| Function | Signature | Description |
|----------|-----------|-------------|
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>) -> Result<Unit, Error>` | Starts an HTTP server on the given address. Blocks in an accept loop, calling `handler` for each request. Returns `Err` if bind fails. |
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, max_requests: int) -> Result<Unit, Error>` | Starts an HTTP server that stops after processing `max_requests` requests. Enables `async fn` + `block_on()` lifecycle management. |
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, max_requests: int, port_callback: fn(int) -> Unit) -> Result<Unit, Error>` | Same as above, but calls `port_callback` with the actual bound port after `bind` + `listen` succeeds. Use with port `0` for OS-assigned ephemeral ports. |

### Request Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `method` | `(req: HttpRequest) -> str` | Returns the HTTP method (e.g., `"GET"`, `"POST"`). |
| `path` | `(req: HttpRequest) -> str` | Returns the request path without the query string (e.g., `"/search"` for `"/search?q=hello"`). |
| `header` | `(req: HttpRequest, key: str) -> Result<Option<str>, Error>` | Returns the value of a request header (case-insensitive lookup). Returns `Ok(None)` if not found. Returns `Err` if `key` contains an embedded NUL byte. |
| `body` | `(req: HttpRequest) -> str` | Returns the request body as a string. Truncated at the first NUL byte; use `body_bytes` for binary data. |
| `body_bytes` | `(req: HttpRequest) -> List<u8>` | Returns the request body as a byte list. Binary-safe — preserves all bytes including NUL. |
| `query` | `(req: HttpRequest, key: str) -> Result<Option<str>, Error>` | Returns the value of a query parameter. Returns `Ok(None)` if not found. Values are automatically URL-decoded. Returns `Err` if `key` contains an embedded NUL byte. |
| `query_all` | `(req: HttpRequest) -> Map<str, str>` | Returns all query parameters as a map. Keys and values are automatically URL-decoded. |
| `cookie` | `(req: HttpRequest, name: str) -> Result<Option<str>, Error>` | Returns the value of a cookie by name. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `cookies` | `(req: HttpRequest) -> Map<str, str>` | Returns all cookies as a map. |
| `form_field` | `(req: HttpRequest, name: str) -> Result<Option<str>, Error>` | Returns the value of a multipart form text field. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `form_file` | `(req: HttpRequest, name: str) -> Result<Option<Map<str, str>>, Error>` | Returns file upload info as an `Option`. `Some(map)` contains keys `"filename"`, `"content_type"`, `"data"`. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `form_fields` | `(req: HttpRequest) -> Map<str, str>` | Returns all multipart form text fields as a map. |

### Response Builder

| Function | Signature | Description |
|----------|-----------|-------------|
| `response` | `(status: int, headers: Map<str, str>, body: str) -> Result<HttpResponse, Error>` | Creates an HTTP response with the given status code, headers, and body. Returns `Err` if any header key or value contains an embedded NUL byte. Headers whose key or value contains CR (`\r`) or LF (`\n`) are silently dropped. The `body` is binary-safe and may contain NUL bytes. |

## Usage Example

### Basic HTTP Server

```ry
from http import listen, method, path, header, body, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    m = method(req)
    p = path(req)
    if p == "/hello":
        return response(200, {"Content-Type": "text/plain"}, "Hello, World!")
    if p == "/echo":
        b = body(req)
        return response(200, {"Content-Type": "text/plain"}, b)
    return response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### Non-blocking Server with `async fn`

```ry
from http import listen, path, response

async fn start_server(port: int) -> str:
    listen("127.0.0.1", port, (req: HttpRequest) -> Result<HttpResponse, Error>:
        p = path(req)
        if p == "/api/health":
            return response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

t = start_server(8080)
# Server runs in background task
```

### Server with Request Limit (`max_requests`)

```ry
from http import listen, path, response, http_get, status, body

port_holder = [0]
fn on_port(p: int) -> Unit:
    port_holder[0] = p

async fn start_server() -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "Hello!")
    , 1, on_port)  # Stop after 1 request; call on_port with bound port
    return "done"

t = start_server()
sleep(100)  # Wait for server to start
port = port_holder[0]

case http_get("http://127.0.0.1:" + to_str(port) + "/"):
    Ok(resp):
        print(body(resp))  # "Hello!"
    Err(e):
        print("error")

result = block_on(t)  # Server exits after 1 request; block_on completes
```

### Reading Query Parameters

```ry
from http import listen, path, query, query_all, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    p = path(req)
    if p == "/search":
        case query(req, "q")?:
            Some(q):
                return response(200, {"Content-Type": "text/plain"}, "Search: " + q)
            None:
                return response(400, {"Content-Type": "text/plain"}, "Missing query parameter: q")
    return response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### Reading Headers

```ry
from http import listen, header, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    case header(req, "Authorization")?:
        Some(token):
            return response(200, {"Content-Type": "text/plain"}, "Authenticated: " + token)
        None:
            return response(401, {"Content-Type": "text/plain"}, "Unauthorized")
)
```

### Handling Form Submissions

```ry
from http import listen, form_field, form_file, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    case form_field(req, "username")?:
        Some(name):
            case form_file(req, "avatar")?:
                Some(file_info):
                    filename = file_info["filename"]
                    return response(200, {"Content-Type": "text/plain"}, "Hello " + name + ", file: " + filename)
                None:
                    return response(400, {"Content-Type": "text/plain"}, "No file uploaded")
        None:
            return response(400, {"Content-Type": "text/plain"}, "Missing username")
)
```

### Reading Cookies

```ry
from http import listen, cookie, cookies, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    case cookie(req, "session_id")?:
        Some(sid):
            return response(200, {"Content-Type": "text/plain"}, "Session: " + sid)
        None:
            return response(401, {"Content-Type": "text/plain"}, "No session")
)
```

## Behavior

- `listen()` binds to the address, starts listening, and enters an accept loop.
- When called with 3 arguments, the accept loop runs indefinitely.
- When called with 4 arguments (`max_requests`), the server stops after processing the specified number of requests. `max_requests` must be a positive integer. This enables `async fn` + `block_on()` lifecycle management. Malformed requests (silently skipped) do not count toward the limit.
- When called with 5 arguments (`max_requests`, `port_callback`), `port_callback` is called synchronously with the actual bound port after `bind` + `listen` succeeds. This allows safe use of port `0` (OS-assigned ephemeral port) to avoid port conflicts in parallel tests.
- The server supports HTTP/1.1 keep-alive by default. Multiple requests can be processed on a single connection. The server checks the `Connection` header on each request: if `Connection: close` is sent, the connection is closed after the response; otherwise the connection stays open for subsequent requests. Idle connections are closed after a 5-second timeout.
- `Content-Length` is automatically added to the response if not provided in the headers map.
- The server supports HTTP/1.1 with `Content-Length`-based body reading and `Transfer-Encoding: chunked` decoding.
- When `Transfer-Encoding: chunked` is present in a request, the body is automatically decoded and concatenated — Ry code receives the full body transparently.
- If both `Transfer-Encoding: chunked` and `Content-Length` are present, the request is rejected as malformed (per RFC 9112).
- To send a chunked response, include `"Transfer-Encoding": "chunked"` in the headers map passed to `response()`. The body will be encoded in chunked format automatically.
- Header lookup via `header()` is case-insensitive.
- `path()` returns the path without the query string. Query parameters are accessed separately via `query()` or `query_all()`.
- Query parameter values are automatically URL-decoded (`%20` → space, `+` → space).
- For duplicate query parameter keys, the first value is returned.
- `cookie()` and `cookies()` parse the `Cookie` header by splitting on `;`, then splitting each pair on the first `=`. Leading and trailing whitespace is trimmed from names and values.
- For duplicate cookie names, the first value is returned.
- Cookie values may contain `=` characters (only the first `=` separates the name from the value).
- `form_field()`, `form_file()`, and `form_fields()` parse `multipart/form-data` request bodies. Parsing is lazy — the body is parsed on the first call and cached.
- The `boundary` parameter is extracted from the `Content-Type` header and supports both quoted and unquoted values.
- Parts with a `filename` in `Content-Disposition` are treated as file uploads; parts without are treated as text fields.
- For duplicate field/file names, the first value is returned.
- `form_file()` returns `Ok(Some(map))` with keys `"filename"`, `"content_type"`, and `"data"`, or `Ok(None)` if the field is not found. If no `Content-Type` is specified for the part, it defaults to `"application/octet-stream"`.
- For non-multipart requests, form functions return `Ok(None)` (for `form_field`, `form_file`) or an empty map (for `form_fields`).

## NUL Safety

Functions that accept `str` arguments ultimately pass them to C APIs (HTTP headers, query string lookup) that cannot handle embedded NUL bytes. These functions return `Err` when a NUL byte is detected:

- `header(req, key)` — `key` must not contain NUL
- `query(req, key)` — `key` must not contain NUL
- `cookie(req, name)` — `name` must not contain NUL
- `form_field(req, name)` — `name` must not contain NUL
- `form_file(req, name)` — `name` must not contain NUL
- `response(status, headers, body)` — header keys and values must not contain NUL; `body` is binary-safe and **may** contain NUL
- `http_get(url)` / `http_post(url, ...)` / `http_request(method, url, ...)` — `url` and `method` must not contain NUL; request body is binary-safe

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
| 402 | Payment Required |
| 403 | Forbidden |
| 404 | Not Found |
| 405 | Method Not Allowed |
| 406 | Not Acceptable |
| 407 | Proxy Authentication Required |
| 408 | Request Timeout |
| 409 | Conflict |
| 410 | Gone |
| 411 | Length Required |
| 412 | Precondition Failed |
| 413 | Content Too Large |
| 414 | URI Too Long |
| 415 | Unsupported Media Type |
| 416 | Range Not Satisfiable |
| 417 | Expectation Failed |
| 418 | I'm a teapot |
| 421 | Misdirected Request |
| 422 | Unprocessable Content |
| 425 | Too Early |
| 426 | Upgrade Required |
| 428 | Precondition Required |
| 429 | Too Many Requests |
| 431 | Request Header Fields Too Large |
| 451 | Unavailable For Legal Reasons |
| 500 | Internal Server Error |
| 501 | Not Implemented |
| 502 | Bad Gateway |
| 503 | Service Unavailable |
| 504 | Gateway Timeout |
| 505 | HTTP Version Not Supported |
| 507 | Insufficient Storage |
| 508 | Loop Detected |
| 511 | Network Authentication Required |

Other status codes use `"Unknown"` as the reason phrase.

## HTTP Client

### Client Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `http_get` | `(url: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP GET request to the given URL. Returns `Err` if `url` contains an embedded NUL byte, or on connection/protocol failure. |
| `http_post` | `(url: str, body: str, headers: Map<str, str>) -> Result<HttpClientResponse, Error>` | Sends an HTTP POST request with body and headers. Returns `Err` if `url` or any header key/value contains an embedded NUL byte, or on failure. The `body` is binary-safe. |
| `http_request` | `(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP request with a custom method. Returns `Err` if `method` or `url` contains an embedded NUL byte, or on failure. |

### Response Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `status` | `(resp: HttpClientResponse) -> int` | Returns the HTTP status code. |
| `body` | `(resp: HttpClientResponse) -> str` | Returns the response body as a string. Truncated at the first NUL byte; use `body_bytes` for binary data. |
| `body_bytes` | `(resp: HttpClientResponse) -> List<u8>` | Returns the response body as a byte list. Binary-safe — preserves all bytes including NUL. |
| `header` | `(resp: HttpClientResponse, key: str) -> Result<Option<str>, Error>` | Returns the value of a response header (case-insensitive). Returns `Ok(None)` if not found. Returns `Err` if `key` contains an embedded NUL byte. |
| `http_client_response_free` | `(resp: HttpClientResponse) -> Unit` | Frees the response and its associated memory. Call when done with the response. |

### Client Usage Example

```ry
from http import http_get, http_post, status, body, header

# Simple GET request
case http_get("http://example.com/api/data"):
    Ok(resp):
        s = status(resp)
        b = body(resp)
        print(to_str(s) + ": " + b)
    Err(e):
        print("Request failed")

# POST request with body and headers
headers: Map<str, str> = {"Content-Type": "application/json"}
case http_post("http://example.com/api/data", "{\"key\": \"value\"}", headers):
    Ok(resp):
        print(body(resp))
    Err(e):
        print("Request failed")
```

### Client Behavior

- Both `http://` and `https://` URLs are supported. HTTPS uses TLS with system CA bundle certificate validation.
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

- `listen()` returns `Err` if `bind()` fails (e.g., port already in use). Previously this panicked; callers should now handle the `Result`.
- Malformed requests or idle timeouts on a keep-alive connection cause the connection to be closed. The server then resumes accepting new connections.
- The handler function must always return a `Result<HttpResponse, Error>` — when the handler returns `Err`, the server synthesizes a 500 Internal Server Error response.
- Client functions return `Result<HttpClientResponse, Error>` — use `case` to handle success and failure.
