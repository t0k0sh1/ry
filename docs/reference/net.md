[English](net.md)

# Network (TCP) Reference

## Types

| Type | Description |
|------|-------------|
| `TcpListener` | Opaque handle for a TCP server socket |
| `TcpStream` | Opaque handle for a TCP connection |
| `TlsStream` | Opaque handle for a TLS-encrypted TCP connection |

Both types are opaque pointers. They cannot be constructed directly; use `bind()` or `connect()` to obtain `TcpListener`/`TcpStream`, and `tls_connect()` for `TlsStream`.

## Functions (from `net`)

These functions require explicit import:

```python
from net import bind, listen, accept, connect, listener_port, shutdown, set_timeout, set_recv_timeout, set_send_timeout, tls_connect
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `bind` | `(host: str, port: int) -> Result<TcpListener, Error>` | Creates a TCP server socket bound to the given address. Returns `Err` on failure. Use port `0` for dynamic allocation. |
| `listen` | `(listener: TcpListener, backlog: int) -> Result<Unit, Error>` | Starts listening for incoming connections. Returns `Err` on failure. |
| `accept` | `(listener: TcpListener) -> Result<TcpStream, Error>` | Accepts a new connection. Waits up to 1 second for a client to connect. Returns `Err` on timeout or failure. |
| `connect` | `(host: str, port: int) -> Result<TcpStream, Error>` | Connects to a remote TCP server. Times out after 5 seconds. Returns `Err` on timeout or failure. |
| `listener_port` | `(listener: TcpListener) -> int` | Returns the actual port number the listener is bound to. Useful when binding to port `0` (OS-assigned port). |
| `shutdown` | `(listener: TcpListener) -> Unit` | Signals the listener to stop accepting connections. Causes any pending `accept()` to return within at most 1 second. |
| `tls_connect` | `(host: str, port: int) -> Result<TlsStream, Error>` | Connects to a remote server with TLS encryption. Validates the server certificate against the system CA bundle. Returns `Err` on connection or handshake failure. |
| `set_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets both receive and send timeout in milliseconds. |
| `set_recv_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets the receive timeout in milliseconds. `recv()` returns `Err` if no data arrives within this time. |
| `set_send_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets the send timeout in milliseconds. |

## Built-in Overloaded Functions

These functions are built-in and work with both `Channel<T>` and TCP socket types. No import needed.

| Function | Signature | Description |
|----------|-----------|-------------|
| `send` | `(stream: TcpStream\|TlsStream, data: List<byte>) -> Result<int, Error>` | Sends bytes through a TCP or TLS connection. Returns `Ok` with the number of bytes sent, or `Err` on failure. |
| `recv` | `(stream: TcpStream\|TlsStream, max: int) -> Result<List<byte>, Error>` | Receives up to `max` bytes. Returns `Ok` with an empty list on connection close, or `Err` on error. |
| `close` | `(handle: TcpStream\|TlsStream) -> Unit` | Closes a TCP or TLS stream. |
| `close` | `(handle: TcpListener) -> Unit` | Closes a TCP listener. |

## Usage Example

### Echo Server

```python
from net import bind, listen, accept, connect
from io import str_to_bytes, bytes_to_str

# Server
match bind("127.0.0.1", 8080):
    case Ok(server):
        match listen(server, 128):
            case Ok(_):
                match accept(server):
                    case Ok(conn):
                        match recv(conn, 4096):
                            case Ok(data):
                                match send(conn, data):
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

### Client

```python
match connect("127.0.0.1", 8080):
    case Ok(conn):
        match send(conn, str_to_bytes("hello")):
            case Ok(_):
                ...
            case Err(e):
                print(e.message)
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
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

### Concurrent Echo Server with `spawn`

```python
fn echo_server(port: int) -> str:
    match bind("127.0.0.1", port):
        case Ok(server):
            match listen(server, 1):
                case Ok(_):
                    match accept(server):
                        case Ok(conn):
                            match recv(conn, 4096):
                                case Ok(data):
                                    match send(conn, data):
                                        case Ok(_):
                                            ...
                                        case Err(e):
                                            ...
                                case Err(e):
                                    ...
                            close(conn)
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(server)
        case Err(e):
            ...
    return "done"

t: Task<str> = spawn echo_server(8080)
# ... client code ...
join(t)
```

## Timeout Configuration

By default, `recv()` uses a 30-second timeout if no custom timeout is set. Use `set_timeout()`, `set_recv_timeout()`, or `set_send_timeout()` to override the default:

```python
from net import connect, set_recv_timeout

match connect("127.0.0.1", 8080):
    case Ok(conn):
        set_recv_timeout(conn, 5000)  # 5-second timeout
        match recv(conn, 4096):
            case Ok(data):
                ...
            case Err(e):
                print("timeout or error")
        close(conn)
    case Err(e):
        print("connect failed")
```

Pass `0` to disable the timeout (wait indefinitely).

## Error Handling

- All TCP functions except `close()` return `Result<T, Error>` — use `match` with `Ok`/`Err` to handle failure.
- `recv()` returns `Ok` with an empty `List<byte>` when the connection is closed by the peer, and `Err` on actual errors (timeout, socket error).
- `close()` closes the socket and frees the handle. Using a handle after close is undefined behavior.

## Byte Conversion

TCP operations work with `List<byte>`. Use `str_to_bytes()` and `bytes_to_str()` from `io` to convert between strings and byte lists.
