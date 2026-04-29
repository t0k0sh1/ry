# HTTP Reference

## Types

| Type | Description |
|------|-------------|
| `HttpRequest` | Opaque handle for an incoming HTTP request |
| `HttpResponse` | Opaque handle for an outgoing HTTP response |
| `HttpClientResponse` | Opaque handle for an HTTP client response |

`HttpRequest` is provided by the server framework. `HttpResponse` is created via `response()`. `HttpClientResponse` is returned by client functions (`httpGet`, `httpPost`, `httpRequest`).

## Functions (from `http`)

These functions require explicit import:

```ry
from http import listen, method, path, header, body, bodyBytes, query, queryAll, cookie, cookies, formField, formFile, formFields, response
```

### Server

| Function | Signature | Description |
|----------|-----------|-------------|
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>) -> Result<Unit, Error>` | Starts an HTTP server on the given address. Blocks in an accept loop, calling `handler` for each request. Returns `Err` if bind fails. |
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, maxRequests: int) -> Result<Unit, Error>` | Starts an HTTP server that stops after processing `maxRequests` requests. Enables `async fn` + `blockOn()` lifecycle management. |
| `listen` | `(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, maxRequests: int, portCallback: fn(int) -> Unit) -> Result<Unit, Error>` | Same as above, but calls `portCallback` with the actual bound port after `bind` + `listen` succeeds. Use with port `0` for OS-assigned ephemeral ports. |

### Request Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `method` | `(req: HttpRequest) -> str` | Returns the HTTP method (e.g., `"GET"`, `"POST"`). |
| `path` | `(req: HttpRequest) -> str` | Returns the request path without the query string (e.g., `"/search"` for `"/search?q=hello"`). |
| `header` | `(req: HttpRequest, key: str) -> Result<Option<str>, Error>` | Returns the value of a request header (case-insensitive lookup). Returns `Ok(None)` if not found. Returns `Err` if `key` contains an embedded NUL byte. |
| `body` | `(req: HttpRequest) -> str` | Returns the request body as a string. Truncated at the first NUL byte; use `bodyBytes` for binary data. |
| `bodyBytes` | `(req: HttpRequest) -> List<u8>` | Returns the request body as a byte list. Binary-safe — preserves all bytes including NUL. |
| `query` | `(req: HttpRequest, key: str) -> Result<Option<str>, Error>` | Returns the value of a query parameter. Returns `Ok(None)` if not found. Values are automatically URL-decoded. Returns `Err` if `key` contains an embedded NUL byte. |
| `queryAll` | `(req: HttpRequest) -> Map<str, str>` | Returns all query parameters as a map. Keys and values are automatically URL-decoded. |
| `cookie` | `(req: HttpRequest, name: str) -> Result<Option<str>, Error>` | Returns the value of a cookie by name. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `cookies` | `(req: HttpRequest) -> Map<str, str>` | Returns all cookies as a map. |
| `formField` | `(req: HttpRequest, name: str) -> Result<Option<str>, Error>` | Returns the value of a multipart form text field. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `formFile` | `(req: HttpRequest, name: str) -> Result<Option<Map<str, str>>, Error>` | Returns file upload info as an `Option`. `Some(map)` contains keys `"filename"`, `"content_type"`, `"data"`. Returns `Ok(None)` if not found. Returns `Err` if `name` contains an embedded NUL byte. |
| `formFields` | `(req: HttpRequest) -> Map<str, str>` | Returns all multipart form text fields as a map. |

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

async fn startServer(port: int) -> str:
    listen("127.0.0.1", port, (req: HttpRequest) -> Result<HttpResponse, Error>:
        p = path(req)
        if p == "/api/health":
            return response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

t = startServer(8080)
# Server runs in background task
```

### Server with Request Limit (`maxRequests`)

```ry
from http import listen, path, response, httpGet, status, body

port_holder = [0]
fn onPort(p: int) -> Unit:
    port_holder[0] = p

async fn startServer() -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "Hello!")
    , 1, onPort)  # Stop after 1 request; call onPort with bound port
    return "done"

t = startServer()
sleep(100)  # Wait for server to start
port = port_holder[0]

case httpGet("http://127.0.0.1:" + toStr(port) + "/"):
    Ok(resp):
        print(body(resp))  # "Hello!"
    Err(e):
        print("error")

result = blockOn(t)  # Server exits after 1 request; blockOn completes
```

### Reading Query Parameters

```ry
from http import listen, path, query, queryAll, response

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
from http import listen, formField, formFile, response

listen("127.0.0.1", 8080, (req: HttpRequest) -> Result<HttpResponse, Error>:
    case formField(req, "username")?:
        Some(name):
            case formFile(req, "avatar")?:
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
- When called with 4 arguments (`maxRequests`), the server stops after processing the specified number of requests. `maxRequests` must be a positive integer. This enables `async fn` + `blockOn()` lifecycle management. Malformed requests (silently skipped) do not count toward the limit.
- When called with 5 arguments (`maxRequests`, `portCallback`), `portCallback` is called synchronously with the actual bound port after `bind` + `listen` succeeds. This allows safe use of port `0` (OS-assigned ephemeral port) to avoid port conflicts in parallel tests.
- The server supports HTTP/1.1 keep-alive by default. Multiple requests can be processed on a single connection. The server checks the `Connection` header on each request: if `Connection: close` is sent, the connection is closed after the response; otherwise the connection stays open for subsequent requests. Idle connections are closed after a 5-second timeout.
- `Content-Length` is automatically added to the response if not provided in the headers map.
- The server supports HTTP/1.1 with `Content-Length`-based body reading and `Transfer-Encoding: chunked` decoding.
- When `Transfer-Encoding: chunked` is present in a request, the body is automatically decoded and concatenated — Ry code receives the full body transparently.
- If both `Transfer-Encoding: chunked` and `Content-Length` are present, the request is rejected as malformed (per RFC 9112).
- To send a chunked response, include `"Transfer-Encoding": "chunked"` in the headers map passed to `response()`. The body will be encoded in chunked format automatically.
- Header lookup via `header()` is case-insensitive.
- `path()` returns the path without the query string. Query parameters are accessed separately via `query()` or `queryAll()`.
- Query parameter values are automatically URL-decoded (`%20` → space, `+` → space).
- For duplicate query parameter keys, the first value is returned.
- `cookie()` and `cookies()` parse the `Cookie` header by splitting on `;`, then splitting each pair on the first `=`. Leading and trailing whitespace is trimmed from names and values.
- For duplicate cookie names, the first value is returned.
- Cookie values may contain `=` characters (only the first `=` separates the name from the value).
- `formField()`, `formFile()`, and `formFields()` parse `multipart/form-data` request bodies. Parsing is lazy — the body is parsed on the first call and cached.
- The `boundary` parameter is extracted from the `Content-Type` header and supports both quoted and unquoted values.
- Parts with a `filename` in `Content-Disposition` are treated as file uploads; parts without are treated as text fields.
- For duplicate field/file names, the first value is returned.
- `formFile()` returns `Ok(Some(map))` with keys `"filename"`, `"content_type"`, and `"data"`, or `Ok(None)` if the field is not found. If no `Content-Type` is specified for the part, it defaults to `"application/octet-stream"`.
- For non-multipart requests, form functions return `Ok(None)` (for `formField`, `formFile`) or an empty map (for `formFields`).

## NUL Safety

Functions that accept `str` arguments ultimately pass them to C APIs (HTTP headers, query string lookup) that cannot handle embedded NUL bytes. These functions return `Err` when a NUL byte is detected:

- `header(req, key)` — `key` must not contain NUL
- `query(req, key)` — `key` must not contain NUL
- `cookie(req, name)` — `name` must not contain NUL
- `formField(req, name)` — `name` must not contain NUL
- `formFile(req, name)` — `name` must not contain NUL
- `response(status, headers, body)` — header keys and values must not contain NUL; `body` is binary-safe and **may** contain NUL
- `httpGet(url)` / `httpPost(url, ...)` / `httpRequest(method, url, ...)` — `url` and `method` must not contain NUL; request body is binary-safe

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
| `httpGet` | `(url: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP GET request to the given URL. Returns `Err` if `url` contains an embedded NUL byte, or on connection/protocol failure. |
| `httpPost` | `(url: str, body: str, headers: Map<str, str>) -> Result<HttpClientResponse, Error>` | Sends an HTTP POST request with body and headers. Returns `Err` if `url` or any header key/value contains an embedded NUL byte, or on failure. The `body` is binary-safe. |
| `httpRequest` | `(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>` | Sends an HTTP request with a custom method. Returns `Err` if `method` or `url` contains an embedded NUL byte, or on failure. |

### Response Accessors

| Function | Signature | Description |
|----------|-----------|-------------|
| `status` | `(resp: HttpClientResponse) -> int` | Returns the HTTP status code. |
| `body` | `(resp: HttpClientResponse) -> str` | Returns the response body as a string. Truncated at the first NUL byte; use `bodyBytes` for binary data. |
| `bodyBytes` | `(resp: HttpClientResponse) -> List<u8>` | Returns the response body as a byte list. Binary-safe — preserves all bytes including NUL. |
| `header` | `(resp: HttpClientResponse, key: str) -> Result<Option<str>, Error>` | Returns the value of a response header (case-insensitive). Returns `Ok(None)` if not found. Returns `Err` if `key` contains an embedded NUL byte. |
| `httpClientResponseFree` | `(resp: HttpClientResponse) -> Unit` | Frees the response and its associated memory. Call when done with the response. |

### Client Usage Example

```ry
from http import httpGet, httpPost, status, body, header

# Simple GET request
case httpGet("http://example.com/api/data"):
    Ok(resp):
        s = status(resp)
        b = body(resp)
        print(toStr(s) + ": " + b)
    Err(e):
        print("Request failed")

# POST request with body and headers
headers: Map<str, str> = {"Content-Type": "application/json"}
case httpPost("http://example.com/api/data", "{\"key\": \"value\"}", headers):
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
- `HttpClientResponse` owns allocated memory (headers, body). Call `httpClientResponseFree()` when done to avoid memory leaks.

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
