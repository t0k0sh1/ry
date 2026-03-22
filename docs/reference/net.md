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
| `bind` | `(host: str, port: int) -> Result<TcpListener, Error>` | Creates a TCP server socket bound to the given address. Returns `Err` on failure. |
| `listen` | `(listener: TcpListener, backlog: int) -> Unit` | Starts listening for incoming connections. Runtime error on failure. |
| `accept` | `(listener: TcpListener) -> Result<TcpStream, Error>` | Accepts a new connection. Waits up to 1 second for a client to connect. Returns `Err` on timeout or failure. |
| `connect` | `(host: str, port: int) -> Result<TcpStream, Error>` | Connects to a remote TCP server. Times out after 5 seconds. Returns `Err` on timeout or failure. |

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
    case Ok(server):
        listen(server, 128)
        match accept(server):
            case Ok(conn):
                data: List<byte> = recv(conn, 4096)
                send(conn, data)
                close(conn)
            case Err(e):
                ...
        close(server)
    case Err(e):
        print("bind failed")
```

### Client

```python
match connect("127.0.0.1", 8080):
    case Ok(conn):
        send(conn, str_to_bytes("hello"))
        resp: List<byte> = recv(conn, 4096)
        match bytes_to_str(resp):
            case Ok(s):
                print(s)
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
            listen(server, 1)
            match accept(server):
                case Ok(conn):
                    data: List<byte> = recv(conn, 4096)
                    send(conn, data)
                    close(conn)
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

## Error Handling

- `bind()` and `connect()` return `Result<T, Error>` — use `match` with `Ok`/`Err` to handle failure.
- `listen()` raises a runtime error on failure.
- `send()` raises a runtime error on failure.
- `recv()` returns an empty `List<byte>` when the connection is closed or when a receive error occurs; unlike `listen()`/`send()`, it does not raise on failure.
- `close()` closes the socket and frees the handle. Using a handle after close is undefined behavior.

## Byte Conversion

TCP operations work with `List<byte>`. Use `str_to_bytes()` and `bytes_to_str()` from `std.io` to convert between strings and byte lists.
