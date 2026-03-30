[English](../../reference/net.md)

# 网络（TCP）参考手册

## 类型

| 类型 | 说明 |
|------|------|
| `TcpListener` | TCP 服务器套接字的不透明句柄 |
| `TcpStream` | TCP 连接的不透明句柄 |
| `TlsStream` | TLS 加密 TCP 连接的不透明句柄 |

这两种类型都是不透明指针。不能直接构造；使用 `bind()` 或 `connect()` 获取 `TcpListener`/`TcpStream`，使用 `tls_connect()` 获取 `TlsStream`。

## 函数（来自 `net`）

这些函数需要显式导入：

```python
from net import bind, listen, accept, connect, listener_port, shutdown, set_timeout, set_recv_timeout, set_send_timeout, tls_connect
```

| 函数 | 签名 | 说明 |
|------|------|------|
| `bind` | `(host: str, port: int) -> Result<TcpListener, Error>` | 创建绑定到指定地址的 TCP 服务器套接字。失败时返回 `Err`。使用端口 `0` 进行动态分配。 |
| `listen` | `(listener: TcpListener, backlog: int) -> Result<Unit, Error>` | 开始监听传入连接。失败时返回 `Err`。 |
| `accept` | `(listener: TcpListener) -> Result<TcpStream, Error>` | 接受新连接。等待客户端连接最多 1 秒。超时或失败时返回 `Err`。 |
| `connect` | `(host: str, port: int) -> Result<TcpStream, Error>` | 连接到远程 TCP 服务器。5 秒后超时。超时或失败时返回 `Err`。 |
| `listener_port` | `(listener: TcpListener) -> int` | 返回监听器绑定的实际端口号。在绑定到端口 `0`（操作系统分配的端口）时很有用。 |
| `shutdown` | `(listener: TcpListener) -> Unit` | 通知监听器停止接受连接。使任何挂起的 `accept()` 在最多 1 秒内返回。 |
| `tls_connect` | `(host: str, port: int) -> Result<TlsStream, Error>` | 使用 TLS 加密连接到远程服务器。根据系统 CA 证书包验证服务器证书。连接或握手失败时返回 `Err`。 |
| `set_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 设置接收和发送超时（毫秒）。 |
| `set_recv_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 设置接收超时（毫秒）。如果在此时间内没有数据到达，`recv()` 返回 `Err`。 |
| `set_send_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | 设置发送超时（毫秒）。 |

## 内建重载函数

这些函数是内建的，可与 TCP 套接字类型配合使用。无需导入。

| 函数 | 签名 | 说明 |
|------|------|------|
| `send` | `(stream: TcpStream\|TlsStream, data: List<u8>) -> Result<int, Error>` | 通过 TCP 或 TLS 连接发送字节。成功时返回 `Ok` 和已发送的字节数，失败时返回 `Err`。 |
| `recv` | `(stream: TcpStream\|TlsStream, max: int) -> Result<List<u8>, Error>` | 接收最多 `max` 个字节。连接关闭时返回 `Ok` 和空列表，出错时返回 `Err`。 |
| `close` | `(handle: TcpStream\|TlsStream) -> Unit` | 关闭 TCP 或 TLS 流。 |
| `close` | `(handle: TcpListener) -> Unit` | 关闭 TCP 监听器。 |

## 使用示例

### 回显服务器

```python
from net import bind, listen, accept, connect
from io import str_to_bytes, bytes_to_str

# 服务器
when bind("127.0.0.1", 8080):
    case Ok(server):
        when listen(server, 128):
            case Ok(_):
                when accept(server):
                    case Ok(conn):
                        when recv(conn, 4096):
                            case Ok(data):
                                when send(conn, data):
                                    case Ok(_):
                                        ...
                                    case Err(e):
                                        print(e.message)
                            case Err(e):
                                print(e.message)
                        close(conn)
                    case Err(e):
                        ...
            case Err(e):
                print("listen failed")
        close(server)
    case Err(e):
        print("bind failed")
```

### 客户端

```python
when connect("127.0.0.1", 8080):
    case Ok(conn):
        when send(conn, str_to_bytes("hello")):
            case Ok(_):
                ...
            case Err(e):
                print(e.message)
        when recv(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
                    case Ok(s):
                        print(s)
                    case Err(e):
                        print(e.message)
            case Err(e):
                print(e.message)
        close(conn)
    case Err(e):
        print("connect failed")
```

### 使用 `async fn` 的并发回显服务器

```python
from net import bind, listen, accept, connect, listener_port
from io import str_to_bytes, bytes_to_str

async fn echo_server(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            when recv(conn, 4096):
                case Ok(data):
                    when send(conn, data):
                        case Ok(_):
                            ...
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(conn)
        case Err(e):
            ...
    close(server)
    return "done"

when bind("127.0.0.1", 0):
    case Ok(server):
        when listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = echo_server(server)
                # ... 使用 port 的客户端代码 ...
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        ...
```

## 超时配置

默认情况下，如果未设置自定义超时，`recv()` 使用 30 秒超时。使用 `set_timeout()`、`set_recv_timeout()` 或 `set_send_timeout()` 来覆盖默认值：

```python
from net import connect, set_recv_timeout

when connect("127.0.0.1", 8080):
    case Ok(conn):
        set_recv_timeout(conn, 5000)  # 5 秒超时
        when recv(conn, 4096):
            case Ok(data):
                ...
            case Err(e):
                print("timeout or error")
        close(conn)
    case Err(e):
        print("connect failed")
```

传入 `0` 可禁用超时（无限等待）。

## 错误处理

- 除 `close()` 外，所有 TCP 函数都返回 `Result<T, Error>` - 使用 `when` 配合 `Ok`/`Err` 来处理失败情况。
- 当对端关闭连接时，`recv()` 返回 `Ok` 和空的 `List<u8>`；实际错误（超时、套接字错误）时返回 `Err`。
- `close()` 关闭套接字并释放句柄。在关闭后使用句柄是未定义行为。

## 字节转换

TCP 操作使用 `List<u8>`。使用 `io` 中的 `str_to_bytes()` 和 `bytes_to_str()` 在字符串和字节列表之间进行转换。
