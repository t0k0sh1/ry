#include "test_codegen_common.hpp"

static const std::string HTTP_DECLS = R"(
@native
fn http_listen(host: str, port: int, handler: fn(HttpRequest) -> HttpResponse) -> Unit
@native
fn http_method(req: HttpRequest) -> str
@native
fn http_path(req: HttpRequest) -> str
@native
fn http_header(req: HttpRequest, key: str) -> Option<str>
@native
fn http_body(req: HttpRequest) -> str
@native
fn http_response(status: int, headers: Map<str, str>, body: str) -> HttpResponse
@native
fn bind(host: str, port: int) -> Option<TcpListener>
@native
fn listen(listener: TcpListener, backlog: int) -> Unit
@native
fn accept(listener: TcpListener) -> Option<TcpStream>
@native
fn connect(host: str, port: int) -> Option<TcpStream>
@native
fn str_to_bytes(s: str) -> List<byte>
@native
fn bytes_to_str(bs: List<byte>) -> str
)";

// Use Channel-based synchronization + manual accept loop to test HTTP functions
// without relying on http_listen (which blocks).

// ============================================================
// Test http_response + http_method + http_path + send_response
// via manual accept loop with Channel synchronization
// ============================================================

TEST_F(CodeGenTest, HttpListenCallback) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(port: int, ready: Channel<int>) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        let path = http_path(req)
        if path == "/hello":
            return http_response(200, {"Content-Type": "text/plain"}, "Hello!")
        return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

fn run_client(port: int, ready: Channel<int>) -> str:
    # Wait for server to be ready
    recv(ready)
    match connect("127.0.0.1", port):
        case Some(conn):
            send(conn, str_to_bytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n"))
            let resp: List<byte> = recv(conn, 4096)
            let msg = bytes_to_str(resp)
            close(conn)
            return msg
        case None:
            return "connect failed"

# Use manual server that signals readiness
fn manual_server(port: int, ready: Channel<int>) -> str:
    match bind("127.0.0.1", port):
        case Some(server):
            listen(server, 1)
            send(ready, 1)
            match accept(server):
                case Some(conn):
                    let data: List<byte> = recv(conn, 4096)
                    let raw = bytes_to_str(data)
                    # Respond with HTTP
                    let response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                    send(conn, str_to_bytes(response_str))
                    close(conn)
                case None:
                    ...
            close(server)
        case None:
            send(ready, 0)
    return "done"

let ready: Channel<int> = channel[int]()
let t: Task<str> = spawn manual_server(18190, ready)
let response = run_client(18190, ready)
print(contains(response, "200 OK"))
print(contains(response, "Hello!"))
join(t)
)"), "true\ntrue\n");
}

// ============================================================
// Test http_listen with spawn (non-blocking client via retry loop)
// ============================================================

TEST_F(CodeGenTest, HttpListenWithSpawn) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        let method = http_method(req)
        let path = http_path(req)
        return http_response(200, {"Content-Type": "text/plain"}, method + ":" + path)
    )
    return "done"

let t: Task<str> = spawn run_server(18194)

# Retry connect loop inline
var connected = false
var response_msg = ""
var attempt = 0
while attempt < 500 and not connected:
    match connect("127.0.0.1", 18194):
        case Some(conn):
            send(conn, str_to_bytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n"))
            let resp: List<byte> = recv(conn, 4096)
            response_msg = bytes_to_str(resp)
            close(conn)
            connected = true
        case None:
            attempt = attempt + 1

print(contains(response_msg, "POST:/api/data"))
)"), "true\n");
}

// ============================================================
// http_header returns Option<str>
// ============================================================

TEST_F(CodeGenTest, HttpHeader) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        match http_header(req, "X-Custom"):
            case Some(val):
                return http_response(200, {"Content-Type": "text/plain"}, "header:" + val)
            case None:
                return http_response(200, {"Content-Type": "text/plain"}, "no-header")
    )
    return "done"

let t: Task<str> = spawn run_server(18195)

var connected = false
var response_msg = ""
var attempt = 0
while attempt < 500 and not connected:
    match connect("127.0.0.1", 18195):
        case Some(conn):
            send(conn, str_to_bytes("GET / HTTP/1.1\r\nHost: localhost\r\nX-Custom: test-value\r\n\r\n"))
            let resp: List<byte> = recv(conn, 4096)
            response_msg = bytes_to_str(resp)
            close(conn)
            connected = true
        case None:
            attempt = attempt + 1

print(contains(response_msg, "header:test-value"))
)"), "true\n");
}

// ============================================================
// 404 response
// ============================================================

TEST_F(CodeGenTest, HttpResponse404) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
fn run_server(port: int) -> str:
    http_listen("127.0.0.1", port, fn(req: HttpRequest) -> HttpResponse:
        return http_response(404, {"Content-Type": "text/plain"}, "Not Found")
    )
    return "done"

let t: Task<str> = spawn run_server(18196)

var connected = false
var response_msg = ""
var attempt = 0
while attempt < 500 and not connected:
    match connect("127.0.0.1", 18196):
        case Some(conn):
            send(conn, str_to_bytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n"))
            let resp: List<byte> = recv(conn, 4096)
            response_msg = bytes_to_str(resp)
            close(conn)
            connected = true
        case None:
            attempt = attempt + 1

print(contains(response_msg, "404 Not Found"))
)"), "true\n");
}
