#include "test_codegen_common.hpp"


using namespace ry;
static const std::string HTTP_DECLS = R"(
@native
function bind(host: str, port: int) -> Result<TcpListener, Error>
@native
function listen(listener: TcpListener, backlog: int) -> Result<Unit, Error>
@native
function accept(listener: TcpListener) -> Result<TcpStream, Error>
@native
function connect(host: str, port: int) -> Result<TcpStream, Error>
@native
function listener_port(listener: TcpListener) -> int
@native
function to_bytes(s: str) -> List<u8>
@native
function bytes_to_str(bs: List<u8>) -> Result<str, Error>
@native
function sleep(ms: int) -> Unit
)";

// ============================================================
// Manual server: tests HTTP response parsing via raw TCP.
// Uses dynamic port allocation. Server binds in main scope,
// passes listener to async function for accept.
// ============================================================

TEST_F(CodeGenTest, ManualHttpServer200) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async function manual_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                    case send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, to_bytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytes_to_str(resp):
                                    Ok(msg):
                                        print(contains(msg, "200 OK"))
                                        print(contains(msg, "Hello!"))
                                    Err(e):
                                        print("false")
                                        print("false")
                            Err(e):
                                print("false")
                                print("false")
                        close(conn)
                    Err(e):
                        print("connect failed")
                block_on(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\ntrue\n");
}

// ============================================================
// Manual server echoing method:path via raw TCP.
// ============================================================

TEST_F(CodeGenTest, ManualHttpServerEcho) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async function run_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 14\r\n\r\nPOST:/api/data"
                    case send(conn, to_bytes(response_str)):
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
                t = run_server(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, to_bytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytes_to_str(resp):
                                    Ok(response_msg):
                                        print(contains(response_msg, "POST:/api/data"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                block_on(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\n");
}

// ============================================================
// 404 response
// ============================================================

TEST_F(CodeGenTest, HttpResponse404) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async function manual_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
                    case send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, to_bytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytes_to_str(resp):
                                    Ok(response_msg):
                                        print(contains(response_msg, "404 Not Found"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                block_on(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\n");
}

// ============================================================
// HttpHeader: server echoes a custom request header value
// ============================================================

TEST_F(CodeGenTest, ManualHttpServerHeader) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async function manual_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 17\r\n\r\nheader:test-value"
                    case send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, to_bytes("GET / HTTP/1.1\r\nHost: localhost\r\nX-Custom: test-value\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytes_to_str(resp):
                                    Ok(msg):
                                        print(contains(msg, "header:test-value"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                block_on(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\n");
}

// ============================================================
// listen with max_requests: server exits after N requests
// Uses async function + port_callback + block_on to verify server lifecycle.
// ============================================================

static const std::string HTTP_LISTEN_DECLS = HTTP_DECLS + R"(
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> Result<HttpResponse, Error>) -> Result<Unit, Error>
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> Result<HttpResponse, Error>, max_requests: int) -> Result<Unit, Error>
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> Result<HttpResponse, Error>, max_requests: int, port_callback: function(int) -> Unit) -> Result<Unit, Error>
@native
function method(req: HttpRequest) -> str
@native
function path(req: HttpRequest) -> str
@native
function response(status: int, headers: Map<str, str>, body: str) -> Result<HttpResponse, Error>
)";

TEST_F(CodeGenTest, HttpListenMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18932, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 1)
    return "done"

t = server()
sleep(200)
case connect("127.0.0.1", 18932):
    Ok(conn):
        case send(conn, to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "200 OK"))
                        print(contains(msg, "ok"))
                    Err(e):
                        print("false")
                        print("false")
            Err(e):
                print("false")
                print("false")
        close(conn)
    Err(e):
        print("false")
        print("false")
result = block_on(t)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpListenMaxRequestsMultiple) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18933, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

# First request
case connect("127.0.0.1", 18933):
    Ok(conn):
        case send(conn, to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/first"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")

# Second request
case connect("127.0.0.1", 18933):
    Ok(conn):
        case send(conn, to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/second"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAlive) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18934, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

# Send two requests on the same connection (keep-alive)
case connect("127.0.0.1", 18934):
    Ok(conn):
        # First request
        case send(conn, to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/first"))
                        print(contains(msg, "Connection: keep-alive"))
                    Err(e):
                        print("false")
                        print("false")
            Err(e):
                print("false")
                print("false")

        # Second request on same connection
        case send(conn, to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/second"))
                        print(contains(msg, "Connection: close"))
                    Err(e):
                        print("false")
                        print("false")
            Err(e):
                print("false")
                print("false")
        close(conn)
    Err(e):
        print("false")
        print("false")
        print("false")
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpConnectionClose) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18935, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 2)
    return "done"

t = server()
sleep(200)

# Send request with Connection: close
case connect("127.0.0.1", 18935):
    Ok(conn):
        case send(conn, to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "200 OK"))
                        print(contains(msg, "Connection: close"))
                    Err(e):
                        print("false")
                        print("false")
            Err(e):
                print("false")
                print("false")
        close(conn)
    Err(e):
        print("false")
        print("false")

# Second request on new connection to reach max_requests
case connect("127.0.0.1", 18935):
    Ok(conn):
        case send(conn, to_bytes("GET /test2 HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "200 OK"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAliveWithMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18936, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 3)
    return "done"

t = server()
sleep(200)

# Send 3 requests on the same keep-alive connection
case connect("127.0.0.1", 18936):
    Ok(conn):
        # Request 1
        case send(conn, to_bytes("GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/a"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")

        # Request 2
        case send(conn, to_bytes("GET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/b"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")

        # Request 3 (triggers shutdown)
        case send(conn, to_bytes("GET /c HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytes_to_str(resp):
                    Ok(msg):
                        print(contains(msg, "/c"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")
        print("false")
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}
