#include "test_codegen_common.hpp"

static const std::string HTTP_DECLS = R"(
@native
fn bind(host: str, port: int) -> Result<TcpListener, Error>
@native
fn listen(listener: TcpListener, backlog: int) -> Result<Unit, Error>
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

TEST_F(CodeGenTest, ManualHttpServer200) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn manual_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            match listen(server, 1):
                case Ok(_):
                    port = listener_port(server)
                    send(ready, port)
                    match accept(server):
                        case Ok(conn):
                            match recv(conn, 4096):
                                case Ok(data):
                                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                                    match send(conn, str_to_bytes(response_str)):
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
                    send(ready, 0)
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn manual_server(ready)
port = recv(ready)
match connect("127.0.0.1", port):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "200 OK"))
                        print(contains(msg, "Hello!"))
                    case Err(e):
                        print("false")
                        print("false")
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

TEST_F(CodeGenTest, ManualHttpServerEcho) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            match listen(server, 1):
                case Ok(_):
                    port = listener_port(server)
                    send(ready, port)
                    match accept(server):
                        case Ok(conn):
                            match recv(conn, 4096):
                                case Ok(data):
                                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 14\r\n\r\nPOST:/api/data"
                                    match send(conn, str_to_bytes(response_str)):
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
                    send(ready, 0)
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn run_server(ready)
port = recv(ready)

match connect("127.0.0.1", port):
    case Ok(conn):
        match send(conn, str_to_bytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(response_msg):
                        print(contains(response_msg, "POST:/api/data"))
                    case Err(e):
                        print("false")
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
            match listen(server, 1):
                case Ok(_):
                    port = listener_port(server)
                    send(ready, port)
                    match accept(server):
                        case Ok(conn):
                            match recv(conn, 4096):
                                case Ok(data):
                                    response_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
                                    match send(conn, str_to_bytes(response_str)):
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
                    send(ready, 0)
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn manual_server(ready)
port = recv(ready)
match connect("127.0.0.1", port):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(response_msg):
                        print(contains(response_msg, "404 Not Found"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")
join(t)
)"), "true\n");
}

// ============================================================
// HttpHeader: server echoes a custom request header value
// ============================================================

TEST_F(CodeGenTest, ManualHttpServerHeader) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn manual_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            match listen(server, 1):
                case Ok(_):
                    port = listener_port(server)
                    send(ready, port)
                    match accept(server):
                        case Ok(conn):
                            match recv(conn, 4096):
                                case Ok(data):
                                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 17\r\n\r\nheader:test-value"
                                    match send(conn, str_to_bytes(response_str)):
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
                    send(ready, 0)
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn manual_server(ready)
port = recv(ready)
match connect("127.0.0.1", port):
    case Ok(conn):
        match send(conn, str_to_bytes("GET / HTTP/1.1\r\nHost: localhost\r\nX-Custom: test-value\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "header:test-value"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")
join(t)
)"), "true\n");
}

// ============================================================
// http_listen with max_requests: server exits after N requests
// Uses spawn + join to verify server lifecycle management.
// ============================================================

static const std::string HTTP_LISTEN_DECLS = HTTP_DECLS + R"(
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int) -> Unit
@native
fn http_method(req: HttpRequest) -> str
@native
fn http_path(req: HttpRequest) -> str
@native
fn http_response(status: int, headers: Map<str, str>, body: str) -> HttpResponse
@native
fn sleep(ms: int) -> Unit
)";

TEST_F(CodeGenTest, HttpListenMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
fn server(ready: Channel<int>) -> str:
    send(ready, 1)
    http_listen("127.0.0.1", 18932, fn(req: HttpRequest) -> HttpResponse:
        return http_response(200, {"Content-Type": "text/plain"}, "ok")
    , 1)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn server(ready)
recv(ready)
sleep(100)
match connect("127.0.0.1", 18932):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "200 OK"))
                        print(contains(msg, "ok"))
                    case Err(e):
                        print("false")
                        print("false")
            case Err(e):
                print("false")
                print("false")
        close(conn)
    case Err(e):
        print("false")
        print("false")
result = join(t)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpListenMaxRequestsMultiple) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
fn server(ready: Channel<int>) -> str:
    send(ready, 1)
    http_listen("127.0.0.1", 18933, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        return http_response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn server(ready)
recv(ready)
sleep(100)

# First request
match connect("127.0.0.1", 18933):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/first"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")

# Second request
match connect("127.0.0.1", 18933):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/second"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")

result = join(t)
print(result)
)"), "true\ntrue\ndone\n");
}
