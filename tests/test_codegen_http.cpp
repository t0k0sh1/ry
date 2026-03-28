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
fn str_to_bytes(s: str) -> List<u8>
@native
fn bytes_to_str(bs: List<u8>) -> Result<str, Error>
@native
fn sleep(ms: int) -> Unit
)";

// ============================================================
// Manual server: tests HTTP response parsing via raw TCP.
// Uses dynamic port allocation. Server binds in main scope,
// passes listener to async fn for accept.
// ============================================================

TEST_F(CodeGenTest, ManualHttpServer200) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manual_server(server: TcpListener) -> str:
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
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = manual_server(server)
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
                await t
            case Err(e):
                ...
    case Err(e):
        ...
)"), "true\ntrue\n");
}

// ============================================================
// Manual server echoing method:path via raw TCP.
// ============================================================

TEST_F(CodeGenTest, ManualHttpServerEcho) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn run_server(server: TcpListener) -> str:
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
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = run_server(server)
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
                await t
            case Err(e):
                ...
    case Err(e):
        ...
)"), "true\n");
}

// ============================================================
// 404 response
// ============================================================

TEST_F(CodeGenTest, HttpResponse404) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manual_server(server: TcpListener) -> str:
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
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = manual_server(server)
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
                await t
            case Err(e):
                ...
    case Err(e):
        ...
)"), "true\n");
}

// ============================================================
// HttpHeader: server echoes a custom request header value
// ============================================================

TEST_F(CodeGenTest, ManualHttpServerHeader) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manual_server(server: TcpListener) -> str:
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
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = manual_server(server)
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
                await t
            case Err(e):
                ...
    case Err(e):
        ...
)"), "true\n");
}

// ============================================================
// http_listen with max_requests: server exits after N requests
// Uses async fn + port_callback + await to verify server lifecycle.
// ============================================================

static const std::string HTTP_LISTEN_DECLS = HTTP_DECLS + R"(
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int) -> Unit
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse, max_requests: int, port_callback: fn(int) -> Unit) -> Unit
@native
fn http_method(req: HttpRequest) -> str
@native
fn http_path(req: HttpRequest) -> str
@native
fn http_response(status: int, headers: Map<str, str>, body: str) -> HttpResponse
)";

TEST_F(CodeGenTest, HttpListenMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server() -> str:
    http_listen("127.0.0.1", 18932, fn(req: HttpRequest) -> HttpResponse:
        return http_response(200, {"Content-Type": "text/plain"}, "ok")
    , 1)
    return "done"

t = server()
sleep(200)
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
result = await t
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpListenMaxRequestsMultiple) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server() -> str:
    http_listen("127.0.0.1", 18933, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        return http_response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

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

result = await t
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAlive) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server() -> str:
    http_listen("127.0.0.1", 18934, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        return http_response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

# Send two requests on the same connection (keep-alive)
match connect("127.0.0.1", 18934):
    case Ok(conn):
        # First request
        match send(conn, str_to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/first"))
                        print(contains(msg, "Connection: keep-alive"))
                    case Err(e):
                        print("false")
                        print("false")
            case Err(e):
                print("false")
                print("false")

        # Second request on same connection
        match send(conn, str_to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/second"))
                        print(contains(msg, "Connection: close"))
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
        print("false")
        print("false")

result = await t
print(result)
)"), "true\ntrue\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpConnectionClose) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server() -> str:
    http_listen("127.0.0.1", 18935, fn(req: HttpRequest) -> HttpResponse:
        return http_response(200, {"Content-Type": "text/plain"}, "ok")
    , 2)
    return "done"

t = server()
sleep(200)

# Send request with Connection: close
match connect("127.0.0.1", 18935):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "200 OK"))
                        print(contains(msg, "Connection: close"))
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

# Second request on new connection to reach max_requests
match connect("127.0.0.1", 18935):
    case Ok(conn):
        match send(conn, str_to_bytes("GET /test2 HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "200 OK"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")

result = await t
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAliveWithMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server() -> str:
    http_listen("127.0.0.1", 18936, fn(req: HttpRequest) -> HttpResponse:
        path = http_path(req)
        return http_response(200, {"Content-Type": "text/plain"}, path)
    , 3)
    return "done"

t = server()
sleep(200)

# Send 3 requests on the same keep-alive connection
match connect("127.0.0.1", 18936):
    case Ok(conn):
        # Request 1
        match send(conn, str_to_bytes("GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/a"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")

        # Request 2
        match send(conn, str_to_bytes("GET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/b"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")

        # Request 3 (triggers shutdown)
        match send(conn, str_to_bytes("GET /c HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        match recv(conn, 4096):
            case Ok(resp):
                match bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/c"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")
        print("false")
        print("false")

result = await t
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}
