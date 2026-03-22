[English](net.md)

# Network (TCP) Reference

## Types

| Type | Description |
|------|-------------|
| `TcpListener` | Opaque handle for a TCP server socket |
| `TcpStream` | Opaque handle for a TCP connection |

Both types are opaque pointers. They cannot be constructed directly; use `bind()` or `connect()` to obtain them.

## Functions (from `std.net`)

These functions require explicit import:

```python
from std.net import bind, listen, accept, connect
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `bind` | `(host: str, port: int) -> Option<TcpListener>` | Creates a TCP server socket bound to the given address. Returns `None` on failure. |
| `listen` | `(listener: TcpListener, backlog: int) -> Unit` | Starts listening for incoming connections. Runtime error on failure. |
| `accept` | `(listener: TcpListener) -> Option<TcpStream>` | Accepts a new connection. Blocks until a client connects. Returns `None` on failure. |
| `connect` | `(host: str, port: int) -> Option<TcpStream>` | Connects to a remote TCP server. Returns `None` on failure. |

## Built-in Overloaded Functions

These functions are built-in and work with both `Channel<T>` and TCP socket types. No import needed.

| Function | Signature | Description |
|----------|-----------|-------------|
| `send` | `(stream: TcpStream, data: List<byte>) -> int` | Sends bytes through a TCP connection. Returns the number of bytes sent. Runtime error on failure. |
| `recv` | `(stream: TcpStream, max: int) -> List<byte>` | Receives up to `max` bytes from a TCP connection. Returns an empty list on connection close. |
| `close` | `(handle: TcpStream) -> Unit` | Closes a TCP stream. |
| `close` | `(handle: TcpListener) -> Unit` | Closes a TCP listener. |

## Usage Example

### Echo Server

```python
from std.net import bind, listen, accept, connect
from std.io import str_to_bytes, bytes_to_str

# Server
match bind("127.0.0.1", 8080):
    case Some(server):
        listen(server, 128)
        match accept(server):
            case Some(conn):
                @const
                data: List<byte> = recv(conn, 4096)
                send(conn, data)
                close(conn)
            case None:
                ...
        close(server)
    case None:
        print("bind failed")
```

### Client

```python
match connect("127.0.0.1", 8080):
    case Some(conn):
        send(conn, str_to_bytes("hello"))
        @const
        resp: List<byte> = recv(conn, 4096)
        print(bytes_to_str(resp))
        close(conn)
    case None:
        print("connect failed")
```

### Concurrent Echo Server with `spawn`

```python
fn echo_server(port: int) -> str:
    match bind("127.0.0.1", port):
        case Some(server):
            listen(server, 1)
            match accept(server):
                case Some(conn):
                    @const
                    data: List<byte> = recv(conn, 4096)
                    send(conn, data)
                    close(conn)
                case None:
                    ...
            close(server)
        case None:
            ...
    return "done"

@const
t: Task<str> = spawn echo_server(8080)
# ... client code ...
join(t)
```

## Error Handling

- `bind()` and `connect()` return `Option<T>` — use `match` to handle failure.
- `listen()` raises a runtime error on failure.
- `send()` raises a runtime error on failure.
- `recv()` returns an empty `List<byte>` when the connection is closed or when a receive error occurs; unlike `listen()`/`send()`, it does not raise on failure.
- `close()` closes the socket and frees the handle. Using a handle after close is undefined behavior.

## Byte Conversion

TCP operations work with `List<byte>`. Use `str_to_bytes()` and `bytes_to_str()` from `std.io` to convert between strings and byte lists.
