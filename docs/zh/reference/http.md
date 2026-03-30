[English](../../reference/http.md)

# HTTP 参考手册

## 类型

| 类型 | 说明 |
|------|------|
| `HttpRequest` | 传入 HTTP 请求的不透明句柄 |
| `HttpResponse` | 传出 HTTP 响应的不透明句柄 |
| `HttpClientResponse` | HTTP 客户端响应的不透明句柄 |

`HttpRequest` 由服务器框架提供。`HttpResponse` 通过 `http_response()` 创建。`HttpClientResponse` 由客户端函数（`http_get`、`http_post`、`http_request`）返回。

## 函数（来自 `http`）

这些函数需要显式导入：

```python
from http import http_listen, http_method, http_path, http_header, http_body, http_query, http_query_all, http_cookie, http_cookies, http_form_field, http_form_file, http_form_fields, http_response
```

### 服务器

| 函数 | 签名 | 说明 |
|------|------|------|
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit` | 在指定地址启动 HTTP 服务器。在接受循环中阻塞，对每个请求调用 `handler`。 |
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int) -> Unit` | 启动一个在处理 `max_requests` 个请求后停止的 HTTP 服务器。支持 `async fn` + `block_on()` 的生命周期管理。 |
| `http_listen` | `(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int, port_callback: fn(int) -> Unit) -> Unit` | 与上述相同，但在 `bind` + `listen` 成功后调用 `port_callback` 并传入实际绑定的端口。可与端口 `0` 配合使用以获取操作系统分配的临时端口。 |

### 请求访问器

| 函数 | 签名 | 说明 |
|------|------|------|
| `http_method` | `(req: HttpRequest) -> str` | 返回 HTTP 方法（例如 `"GET"`、`"POST"`）。 |
| `http_path` | `(req: HttpRequest) -> str` | 返回不含查询字符串的请求路径（例如对于 `"/search?q=hello"` 返回 `"/search"`）。 |
| `http_header` | `(req: HttpRequest, key: str) -> Option<str>` | 返回请求头的值（不区分大小写查找）。未找到时返回 `None`。 |
| `http_body` | `(req: HttpRequest) -> str` | 以字符串形式返回请求体。 |
| `http_query` | `(req: HttpRequest, key: str) -> Option<str>` | 返回查询参数的值。未找到时返回 `None`。值会自动进行 URL 解码。 |
| `http_query_all` | `(req: HttpRequest) -> Map<str, str>` | 以映射形式返回所有查询参数。键和值会自动进行 URL 解码。 |
| `http_cookie` | `(req: HttpRequest, name: str) -> Option<str>` | 按名称返回 cookie 的值。未找到时返回 `None`。 |
| `http_cookies` | `(req: HttpRequest) -> Map<str, str>` | 以映射形式返回所有 cookie。 |
| `http_form_field` | `(req: HttpRequest, name: str) -> Option<str>` | 返回 multipart 表单文本字段的值。未找到时返回 `None`。 |
| `http_form_file` | `(req: HttpRequest, name: str) -> Option<Map<str, str>>` | 以 `Option` 形式返回文件上传信息。`Some(map)` 包含 `"filename"`、`"content_type"`、`"data"` 键。未找到时返回 `None`。 |
| `http_form_fields` | `(req: HttpRequest) -> Map<str, str>` | 以映射形式返回所有 multipart 表单文本字段。 |

### 响应构建器

| 函数 | 签名 | 说明 |
|------|------|------|
| `http_response` | `(status: int, headers: Map<str, str>, body: str) -> HttpResponse` | 使用给定的状态码、响应头和响应体创建 HTTP 响应。 |

## 使用示例

### 基本 HTTP 服务器

```python
from http import http_listen, http_method, http_path, http_header, http_body, http_response

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

### 使用 `async fn` 的非阻塞服务器

```python
from http import http_listen, http_path, http_response

async fn start_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        if path == "/api/health":
            return http_response(200, {"Content-Type": "application/json"}, "{\"status\": \"ok\"}")
        return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

t = start_server(8080)
# 服务器在后台任务中运行
```

### 带请求限制的服务器（`max_requests`）

```python
from http import http_listen, http_path, http_response, http_get, http_client_status, http_client_body

port_holder = [0]
fn on_port(p: int) -> Unit:
    port_holder[0] = p

async fn start_server() -> str:
    http_listen("127.0.0.1", 0, fn(req: HttpRequest) -> HttpResponse:
        return http_response(200, {"Content-Type": "text/plain"}, "Hello!")
    , 1, on_port)  # 处理 1 个请求后停止；调用 on_port 传入绑定的端口
    return "done"

t = start_server()
sleep(100)  # 等待服务器启动
port = port_holder[0]

when http_get("http://127.0.0.1:" + to_str(port) + "/"):
    case Ok(resp):
        print(http_client_body(resp))  # "Hello!"
    case Err(e):
        print("error")

result = block_on(t)  # 服务器在处理 1 个请求后退出；block_on 完成
```

### 读取查询参数

```python
from http import http_listen, http_path, http_query, http_query_all, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    path = http_path(req)
    if path == "/search":
        when http_query(req, "q"):
            case Some(query):
                return http_response(200, {"Content-Type": "text/plain"}, "Search: " + query)
            case None:
                return http_response(400, {"Content-Type": "text/plain"}, "Missing query parameter: q")
    return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
)
```

### 读取请求头

```python
from http import http_listen, http_header, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    when http_header(req, "Authorization"):
        case Some(token):
            return http_response(200, {"Content-Type": "text/plain"}, "Authenticated: " + token)
        case None:
            return http_response(401, {"Content-Type": "text/plain"}, "Unauthorized")
)
```

### 处理表单提交

```python
from http import http_listen, http_form_field, http_form_file, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    when http_form_field(req, "username"):
        case Some(name):
            when http_form_file(req, "avatar"):
                case Some(file_info):
                    filename = file_info["filename"]
                    return http_response(200, {"Content-Type": "text/plain"}, "Hello " + name + ", file: " + filename)
                case None:
                    return http_response(400, {"Content-Type": "text/plain"}, "No file uploaded")
        case None:
            return http_response(400, {"Content-Type": "text/plain"}, "Missing username")
)
```

### 读取 Cookie

```python
from http import http_listen, http_cookie, http_cookies, http_response

http_listen("127.0.0.1", 8080, fn(req: HttpRequest) -> HttpResponse:
    when http_cookie(req, "session_id"):
        case Some(sid):
            return http_response(200, {"Content-Type": "text/plain"}, "Session: " + sid)
        case None:
            return http_response(401, {"Content-Type": "text/plain"}, "No session")
)
```

## 行为

- `http_listen()` 绑定到地址，开始监听，并进入接受循环。
- 使用 3 个参数调用时，接受循环无限运行。
- 使用 4 个参数调用时（`max_requests`），服务器在处理指定数量的请求后停止。`max_requests` 必须是正整数。这支持 `async fn` + `block_on()` 的生命周期管理。格式错误的请求（静默跳过）不计入限制。
- 使用 5 个参数调用时（`max_requests`、`port_callback`），在 `bind` + `listen` 成功后同步调用 `port_callback` 并传入实际绑定的端口。这允许安全使用端口 `0`（操作系统分配的临时端口）以避免并行测试中的端口冲突。
- 服务器默认支持 HTTP/1.1 keep-alive。单个连接上可以处理多个请求。服务器在每个请求上检查 `Connection` 头：如果发送了 `Connection: close`，则在响应后关闭连接；否则连接保持打开以处理后续请求。空闲连接在 5 秒超时后关闭。
- 如果响应头映射中未提供 `Content-Length`，则会自动添加。
- 服务器支持基于 `Content-Length` 的请求体读取和 `Transfer-Encoding: chunked` 解码的 HTTP/1.1。
- 当请求中存在 `Transfer-Encoding: chunked` 时，请求体会自动解码并拼接 - Ry 代码透明地接收完整的请求体。
- 如果同时存在 `Transfer-Encoding: chunked` 和 `Content-Length`，请求将被视为格式错误而拒绝（依据 RFC 9112）。
- 要发送分块响应，请在传给 `http_response()` 的响应头映射中包含 `"Transfer-Encoding": "chunked"`。响应体将自动以分块格式编码。
- 通过 `http_header()` 的头部查找不区分大小写。
- `http_path()` 返回不含查询字符串的路径。查询参数通过 `http_query()` 或 `http_query_all()` 单独访问。
- 查询参数值会自动进行 URL 解码（`%20` -> 空格，`+` -> 空格）。
- 对于重复的查询参数键，返回第一个值。
- `http_cookie()` 和 `http_cookies()` 通过按 `;` 分割来解析 `Cookie` 头，然后按第一个 `=` 分割每个键值对。名称和值的前后空白会被去除。
- 对于重复的 cookie 名称，返回第一个值。
- Cookie 值可以包含 `=` 字符（只有第一个 `=` 用于分隔名称和值）。
- `http_form_field()`、`http_form_file()` 和 `http_form_fields()` 解析 `multipart/form-data` 请求体。解析是惰性的 - 在第一次调用时解析请求体并缓存。
- `boundary` 参数从 `Content-Type` 头中提取，支持带引号和不带引号的值。
- `Content-Disposition` 中带有 `filename` 的部分被视为文件上传；没有的则被视为文本字段。
- 对于重复的字段/文件名称，返回第一个值。
- `http_form_file()` 返回 `Some(map)`，包含 `"filename"`、`"content_type"` 和 `"data"` 键；如果未找到字段则返回 `None`。如果部分未指定 `Content-Type`，默认为 `"application/octet-stream"`。
- 对于非 multipart 请求，表单函数返回 `None`（对于 `http_form_field`、`http_form_file`）或空映射（对于 `http_form_fields`）。

## 支持的状态码

以下状态码具有标准原因短语（RFC 9110）：

| 代码 | 原因 |
|------|------|
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

其他状态码使用 `"Unknown"` 作为原因短语。

## HTTP 客户端

### 客户端函数

| 函数 | 签名 | 说明 |
|------|------|------|
| `http_get` | `(url: str) -> Result<HttpClientResponse, Error>` | 向给定 URL 发送 HTTP GET 请求。 |
| `http_post` | `(url: str, body: str, headers: Map<str, str>) -> Result<HttpClientResponse, Error>` | 发送带有请求体和请求头的 HTTP POST 请求。 |
| `http_request` | `(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>` | 使用自定义方法发送 HTTP 请求。 |

### 响应访问器

| 函数 | 签名 | 说明 |
|------|------|------|
| `http_client_status` | `(resp: HttpClientResponse) -> int` | 返回 HTTP 状态码。 |
| `http_client_body` | `(resp: HttpClientResponse) -> str` | 以字符串形式返回响应体。 |
| `http_client_header` | `(resp: HttpClientResponse, key: str) -> Option<str>` | 返回响应头的值（不区分大小写）。未找到时返回 `None`。 |
| `http_client_response_free` | `(resp: HttpClientResponse) -> Unit` | 释放响应及其关联的内存。使用完响应后调用。 |

### 客户端使用示例

```python
from http import http_get, http_post, http_client_status, http_client_body, http_client_header

# 简单的 GET 请求
when http_get("http://example.com/api/data"):
    case Ok(resp):
        status = http_client_status(resp)
        body = http_client_body(resp)
        print(to_str(status) + ": " + body)
    case Err(e):
        print("Request failed")

# 带请求体和请求头的 POST 请求
headers: Map<str, str> = {"Content-Type": "application/json"}
when http_post("http://example.com/api/data", "{\"key\": \"value\"}", headers):
    case Ok(resp):
        print(http_client_body(resp))
    case Err(e):
        print("Request failed")
```

### 客户端行为

- 支持 `http://` 和 `https://` URL。HTTPS 使用 TLS 和系统 CA 证书包进行证书验证。
- `Host` 头会根据 URL 自动添加。
- 始终发送 `Connection: close`；每个请求使用单独的 TCP 连接。
- `Content-Length` 始终自动添加（包括空请求体的 `0`）。用户提供的 `Content-Length` 头会被正确的值覆盖。
- 响应体读取支持 `Content-Length`、`Transfer-Encoding: chunked` 或读取到连接关闭。
- 响应头查找不区分大小写。
- 连接超时为 5 秒；接收超时为 30 秒。
- 连接失败、无效 URL 或格式错误的响应时返回 `Err`。
- `HttpClientResponse` 拥有已分配的内存（头部、响应体）。使用完毕后调用 `http_client_response_free()` 以避免内存泄漏。

### 重定向行为

HTTP 客户端函数会自动跟随重定向响应（带有 `Location` 头的 3xx 响应）。

- **支持的状态码**：301、302、303、307、308
- **最大重定向次数**：10（超过时返回 `Err`）
- **方法转换**（依据 RFC 9110）：
  - 301、302：`POST` 会被更改为 `GET`（请求体被丢弃）；其他方法保持不变
  - 303：方法始终更改为 `GET`（请求体被丢弃）
  - 307、308：方法和请求体保持不变
- **URL 解析**：支持 `Location` 头中的绝对 URL、协议相对 URL（`//...`）、绝对路径（`/...`）和相对路径
- 用户提供的头部在每次重定向跳转时重新发送，但敏感头部（`Authorization`、`Proxy-Authorization`、`Cookie`）在跨源重定向（不同主机或端口）时会被移除
- 如果 `Location` 头缺失或为空，则按原样返回重定向响应
- 调用者仅接收所有重定向完成后的最终响应

## 错误处理

- 如果 `bind()` 失败（例如端口已被占用），`http_listen()` 会引发运行时错误。
- 格式错误的请求或 keep-alive 连接上的空闲超时会导致连接关闭。服务器随后继续接受新连接。
- 处理函数必须始终返回 `HttpResponse` - 没有默认响应。
- 客户端函数返回 `Result<HttpClientResponse, Error>` - 使用 `when` 处理成功和失败情况。
