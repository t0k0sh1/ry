[English](net.md)

# Network (TCP) Reference

## Types

| Type | Description |
|------|-------------|
| `TcpListener` | Opaque handle for a TCP server socket |
| `TcpStream` | Opaque handle for a TCP connection |
| `TlsStream` | Opaque handle for a TLS-encrypted TCP connection |

All types are opaque pointers managed by ARC (Automatic Reference Counting). They cannot be constructed directly; use `bind()` or `connect()` to obtain `TcpListener`/`TcpStream`, and `tls_connect()` for `TlsStream`. Resources are automatically closed when no longer referenced — explicit `close()` calls are optional but supported for immediate cleanup.

## Functions (from `net`)

These functions require explicit import:

```ry
from net import bind, listen, accept, connect, listener_port, shutdown, set_timeout, set_receive_timeout, set_send_timeout, tls_connect
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `bind` | `(host: str, port: int) -> Result<TcpListener, Error>` | Creates a TCP server socket bound to the given address. Returns `Err` on failure (including if `host` contains an embedded NUL byte, or `port` is outside [0, 65535]). Use port `0` for dynamic allocation. |
| `listen` | `(listener: TcpListener, backlog: int) -> Result<Unit, Error>` | Starts listening for incoming connections. Returns `Err` on failure. |
| `accept` | `(listener: TcpListener) -> Result<TcpStream, Error>` | Accepts a new connection. Waits up to 1 second for a client to connect. Returns `Err` on timeout or failure. |
| `connect` | `(host: str, port: int) -> Result<TcpStream, Error>` | Connects to a remote TCP server. Times out after 5 seconds. Returns `Err` on timeout or failure (including if `host` contains an embedded NUL byte). |
| `listener_port` | `(listener: TcpListener) -> int` | Returns the actual port number the listener is bound to. Useful when binding to port `0` (OS-assigned port). Returns `-1` if the underlying `getsockname` call fails. |
| `shutdown` | `(listener: TcpListener) -> Unit` | Signals the listener to stop accepting connections. Causes any pending `accept()` to return within at most 1 second. |
| `tls_connect` | `(host: str, port: int) -> Result<TlsStream, Error>` | Connects to a remote server with TLS encryption. Validates the server certificate against the system CA bundle. Returns `Err` on connection or handshake failure (including if `host` contains an embedded NUL byte). |
| `set_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets both receive and send timeout in milliseconds. |
| `set_receive_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets the receive timeout in milliseconds. `receive()` returns `Err` if no data arrives within this time. |
| `set_send_timeout` | `(stream: TcpStream\|TlsStream, ms: int) -> Unit` | Sets the send timeout in milliseconds. |

## Built-in Overloaded Functions

These functions are built-in and work with TCP socket types. No import needed.

| Function | Signature | Description |
|----------|-----------|-------------|
| `send` | `(stream: TcpStream\|TlsStream, data: List<u8>) -> Result<int, Error>` | Sends bytes through a TCP or TLS connection. Returns `Ok` with the number of bytes sent, or `Err` on failure. |
| `receive` | `(stream: TcpStream\|TlsStream, max: int) -> Result<List<u8>, Error>` | Receives up to `max` bytes. Returns `Ok` with an empty list on connection close, or `Err` on error. |
| `close` | `(handle: TcpStream\|TlsStream) -> Unit` | Closes a TCP or TLS stream. |
| `close` | `(handle: TcpListener) -> Unit` | Closes a TCP listener. |

## Usage Example

### Echo Server

```ry
from net import bind, listen, accept, connect
from io import to_bytes, bytes_to_str

# Server
case bind("127.0.0.1", 8080):
    Ok(server):
        case listen(server, 128):
            Ok(_):
                case accept(server):
                    Ok(conn):
                        case receive(conn, 4096):
                            Ok(data):
                                case send(conn, data):
                                    Ok(_):
                                        ...
                                    Err(e):
                                        print(e.message)
                            Err(e):
                                print(e.message)
                        close(conn)
                    Err(e):
                        ...
            Err(e):
                print("listen failed")
        close(server)
    Err(e):
        print("bind failed")
```

### Client

```ry
case connect("127.0.0.1", 8080):
    Ok(conn):
        case send(conn, to_bytes("hello")):
            Ok(_):
                ...
            Err(e):
                print(e.message)
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(s):
                        print(s)
                    Err(e):
                        print(e.message)
            Err(e):
                print(e.message)
        close(conn)
    Err(e):
        print("connect failed")
```

### Concurrent Echo Server with `async function`

```ry
from net import bind, listen, accept, connect, listener_port
from io import to_bytes, bytes_to_str

async function echo_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    case send(conn, data):
                        Ok(_):
                            ...
                        Err(e):
                            ...
                Err(e):
                    ...
            close(conn)
        Err(e):
            ...
    close(server)
    return "done"

case bind("127.0.0.1", 0):
    Ok(server):
        case listen(server, 1):
            Ok(_):
                port = listener_port(server)
                t = echo_server(server)
                # ... client code using port ...
                block_on(t)
            Err(e):
                ...
    Err(e):
        ...
```

## Timeout Configuration

By default, `receive()` uses a 30-second timeout if no custom timeout is set. Use `set_timeout()`, `set_receive_timeout()`, or `set_send_timeout()` to override the default:

```ry
from net import connect, set_receive_timeout

case connect("127.0.0.1", 8080):
    Ok(conn):
        set_receive_timeout(conn, 5000)  # 5-second timeout
        case receive(conn, 4096):
            Ok(data):
                ...
            Err(e):
                print("timeout or error")
        close(conn)
    Err(e):
        print("connect failed")
```

Pass `0` to disable the timeout (wait indefinitely).

## Error Handling

- All TCP functions except `close()` return `Result<T, Error>` — use `case` with `Ok`/`Err` to handle failure.
- `receive()` returns `Ok` with an empty `List<u8>` when the connection is closed by the peer, and `Err` on actual errors (timeout, socket error).
- `close()` closes the socket and frees the handle. Using a handle after close is undefined behavior.

## Byte Conversion

TCP operations work with `List<u8>`. Use `to_bytes()` and `bytes_to_str()` from `io` to convert between strings and byte lists.
