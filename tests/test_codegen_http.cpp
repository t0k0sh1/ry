#include "test_codegen_common.hpp"

static const std::string HTTP_DECLS = R"(
@native
fn bind(host: str, port: int) -> Result<TcpListener, Error>
@native
fn listen(listener: TcpListener, backlog: int) -> Unit
@native
fn accept(listener: TcpListener) -> Result<TcpStream, Error>
@native
fn connect(host: str, port: int) -> Result<TcpStream, Error>
@native
fn listener_port(listener: TcpListener) -> int
@native
fn str_to_bytes(s: str) -> List<byte>
@native
fn bytes_to_str(bs: List<byte>) -> Result<str, Error>
)";

// ============================================================
// Manual server: tests HTTP response parsing via raw TCP.
// Uses dynamic port allocation and Channel synchronization.
// ============================================================

TEST_F(CodeGenTest, HttpListenCallback) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn manual_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            listen(server, 1)
            port = listener_port(server)
            send(ready, port)
            match accept(server):
                case Ok(conn):
                    data: List<byte> = recv(conn, 4096)
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                    send(conn, str_to_bytes(response_str))
                    close(conn)
                case Err(e):
                    ...
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn manual_server(ready)
port = recv(ready)
match connect("127.0.0.1", port):
    case Ok(conn):
        send(conn, str_to_bytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n"))
        resp: List<byte> = recv(conn, 4096)
        match bytes_to_str(resp):
            case Ok(msg):
                print(contains(msg, "200 OK"))
                print(contains(msg, "Hello!"))
            case Err(e):
                print("false")
                print("false")
        close(conn)
    case Err(e):
        print("connect failed")
join(t)
)"), "true\ntrue\n");
}

// ============================================================
// Manual server echoing method:path via raw TCP.
// Uses dynamic port allocation.
// ============================================================

TEST_F(CodeGenTest, HttpListenWithSpawn) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            listen(server, 1)
            port = listener_port(server)
            send(ready, port)
            match accept(server):
                case Ok(conn):
                    data: List<byte> = recv(conn, 4096)
                    body = "POST:/api/data"
                    content_length = len(body)
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " + to_str(content_length) + "\r\n\r\n" + body
                    send(conn, str_to_bytes(response_str))
                    close(conn)
                case Err(e):
                    ...
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn run_server(ready)
port = recv(ready)

match connect("127.0.0.1", port):
    case Ok(conn):
        send(conn, str_to_bytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n"))
        resp: List<byte> = recv(conn, 4096)
        match bytes_to_str(resp):
            case Ok(response_msg):
                print(contains(response_msg, "POST:/api/data"))
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")
join(t)
)"), "true\n");
}

// ============================================================
// 404 response
// ============================================================

TEST_F(CodeGenTest, HttpResponse404) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn manual_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            listen(server, 1)
            port = listener_port(server)
            send(ready, port)
            match accept(server):
                case Ok(conn):
                    data: List<byte> = recv(conn, 4096)
                    response_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
                    send(conn, str_to_bytes(response_str))
                    close(conn)
                case Err(e):
                    ...
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn manual_server(ready)
port = recv(ready)
match connect("127.0.0.1", port):
    case Ok(conn):
        send(conn, str_to_bytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n"))
        resp: List<byte> = recv(conn, 4096)
        match bytes_to_str(resp):
            case Ok(response_msg):
                print(contains(response_msg, "404 Not Found"))
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")
join(t)
)"), "true\n");
}
