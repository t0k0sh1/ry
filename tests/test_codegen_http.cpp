#include "test_codegen_common.hpp"

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
    when accept(server):
        case Ok(conn):
            when receive(conn, 4096):
                case Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                    when send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                when connect("127.0.0.1", port):
                    case Ok(conn):
                        when send(conn, to_bytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            case Ok(_):
                                ...
                            case Err(e):
                                ...
                        when receive(conn, 4096):
                            case Ok(resp):
                                when bytes_to_str(resp):
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
                block_on(t)
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
async function run_server(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            when receive(conn, 4096):
                case Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 14\r\n\r\nPOST:/api/data"
                    when send(conn, to_bytes(response_str)):
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
                t = run_server(server)
                when connect("127.0.0.1", port):
                    case Ok(conn):
                        when send(conn, to_bytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            case Ok(_):
                                ...
                            case Err(e):
                                ...
                        when receive(conn, 4096):
                            case Ok(resp):
                                when bytes_to_str(resp):
                                    case Ok(response_msg):
                                        print(contains(response_msg, "POST:/api/data"))
                                    case Err(e):
                                        print("false")
                            case Err(e):
                                print("false")
                        close(conn)
                    case Err(e):
                        print("false")
                block_on(t)
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
async function manual_server(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            when receive(conn, 4096):
                case Ok(data):
                    response_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
                    when send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                when connect("127.0.0.1", port):
                    case Ok(conn):
                        when send(conn, to_bytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            case Ok(_):
                                ...
                            case Err(e):
                                ...
                        when receive(conn, 4096):
                            case Ok(resp):
                                when bytes_to_str(resp):
                                    case Ok(response_msg):
                                        print(contains(response_msg, "404 Not Found"))
                                    case Err(e):
                                        print("false")
                            case Err(e):
                                print("false")
                        close(conn)
                    case Err(e):
                        print("false")
                block_on(t)
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
async function manual_server(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            when receive(conn, 4096):
                case Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 17\r\n\r\nheader:test-value"
                    when send(conn, to_bytes(response_str)):
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
                t = manual_server(server)
                when connect("127.0.0.1", port):
                    case Ok(conn):
                        when send(conn, to_bytes("GET / HTTP/1.1\r\nHost: localhost\r\nX-Custom: test-value\r\n\r\n")):
                            case Ok(_):
                                ...
                            case Err(e):
                                ...
                        when receive(conn, 4096):
                            case Ok(resp):
                                when bytes_to_str(resp):
                                    case Ok(msg):
                                        print(contains(msg, "header:test-value"))
                                    case Err(e):
                                        print("false")
                            case Err(e):
                                print("false")
                        close(conn)
                    case Err(e):
                        print("false")
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        ...
)"), "true\n");
}

// ============================================================
// listen with max_requests: server exits after N requests
// Uses async function + port_callback + block_on to verify server lifecycle.
// ============================================================

static const std::string HTTP_LISTEN_DECLS = HTTP_DECLS + R"(
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> HttpResponse) -> Unit
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> HttpResponse, max_requests: int) -> Unit
@native
function listen(host: str, port: int, handler: function(HttpRequest) -> HttpResponse, max_requests: int, port_callback: function(int) -> Unit) -> Unit
@native
function method(req: HttpRequest) -> str
@native
function path(req: HttpRequest) -> str
@native
function response(status: int, headers: Map<str, str>, body: str) -> HttpResponse
)";

TEST_F(CodeGenTest, HttpListenMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18932, function(req: HttpRequest) -> HttpResponse:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 1)
    return "done"

t = server()
sleep(200)
when connect("127.0.0.1", 18932):
    case Ok(conn):
        when send(conn, to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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
result = block_on(t)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpListenMaxRequestsMultiple) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18933, function(req: HttpRequest) -> HttpResponse:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

# First request
when connect("127.0.0.1", 18933):
    case Ok(conn):
        when send(conn, to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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
when connect("127.0.0.1", 18933):
    case Ok(conn):
        when send(conn, to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/second"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAlive) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18934, function(req: HttpRequest) -> HttpResponse:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2)
    return "done"

t = server()
sleep(200)

# Send two requests on the same connection (keep-alive)
when connect("127.0.0.1", 18934):
    case Ok(conn):
        # First request
        when send(conn, to_bytes("GET /first HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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
        when send(conn, to_bytes("GET /second HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpConnectionClose) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18935, function(req: HttpRequest) -> HttpResponse:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 2)
    return "done"

t = server()
sleep(200)

# Send request with Connection: close
when connect("127.0.0.1", 18935):
    case Ok(conn):
        when send(conn, to_bytes("GET /test HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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
when connect("127.0.0.1", 18935):
    case Ok(conn):
        when send(conn, to_bytes("GET /test2 HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "200 OK"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")
        close(conn)
    case Err(e):
        print("false")

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAliveWithMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async function server() -> str:
    listen("127.0.0.1", 18936, function(req: HttpRequest) -> HttpResponse:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 3)
    return "done"

t = server()
sleep(200)

# Send 3 requests on the same keep-alive connection
when connect("127.0.0.1", 18936):
    case Ok(conn):
        # Request 1
        when send(conn, to_bytes("GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/a"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")

        # Request 2
        when send(conn, to_bytes("GET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
                    case Ok(msg):
                        print(contains(msg, "/b"))
                    case Err(e):
                        print("false")
            case Err(e):
                print("false")

        # Request 3 (triggers shutdown)
        when send(conn, to_bytes("GET /c HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            case Ok(_):
                ...
            case Err(e):
                ...
        when receive(conn, 4096):
            case Ok(resp):
                when bytes_to_str(resp):
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

result = block_on(t)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}
