#include "test_codegen_common.hpp"
#include <cstdlib>
#include <string>
#include <thread>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>


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

static const std::string HTTP_CLIENT_DECLS = R"(
@native("http")
function http_get(url: str) -> Result<HttpClientResponse, Error>
@native("http")
function http_request(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>
@native("http")
function status(response: HttpClientResponse) -> int
@native("http")
function body(response: HttpClientResponse) -> str
@native("http")
function http_client_response_free(response: HttpClientResponse) -> Unit
)";

class AllowPrivateHTTPGuard {
public:
    AllowPrivateHTTPGuard() {
        const char *prev = std::getenv("RY_ALLOW_PRIVATE_HTTP");
        had_prev_ = (prev != nullptr);
        if (had_prev_) prev_value_ = prev;
        ::setenv("RY_ALLOW_PRIVATE_HTTP", "1", 1);
    }

    ~AllowPrivateHTTPGuard() {
        if (had_prev_)
            ::setenv("RY_ALLOW_PRIVATE_HTTP", prev_value_.c_str(), 1);
        else
            ::unsetenv("RY_ALLOW_PRIVATE_HTTP");
    }

private:
    bool had_prev_ = false;
    std::string prev_value_;
};

static void mock_http_server(int fd, const std::string &response) {
    char buf[4096];
    (void)::recv(fd, buf, sizeof(buf), 0);
    size_t sent = 0;
    while (sent < response.size()) {
        ssize_t w = ::write(fd, response.c_str() + sent, response.size() - sent);
        if (w <= 0) break;
        sent += static_cast<size_t>(w);
    }
    ::close(fd);
}

static std::string mock_http_server_capture(int fd, const std::string &response) {
    char buf[8192];
    std::string request;
    struct timeval tv = {2, 0};
    ::setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    while (true) {
        ssize_t n = ::recv(fd, buf, sizeof(buf), 0);
        if (n <= 0) break;
        request.append(buf, static_cast<size_t>(n));
        if (request.find("\r\n\r\n") != std::string::npos) break;
    }
    size_t sent = 0;
    while (sent < response.size()) {
        ssize_t w = ::write(fd, response.c_str() + sent, response.size() - sent);
        if (w <= 0) break;
        sent += static_cast<size_t>(w);
    }
    ::close(fd);
    return request;
}

static int start_mock_server() {
    int srv = ::socket(AF_INET, SOCK_STREAM, 0);
    if (srv < 0) return -1;
    int opt = 1;
    ::setsockopt(srv, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    struct sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = 0;
    if (::bind(srv, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)) != 0) {
        ::close(srv);
        return -1;
    }
    if (::listen(srv, 1) != 0) {
        ::close(srv);
        return -1;
    }
    return srv;
}

static int get_server_port(int srv) {
    struct sockaddr_in addr{};
    socklen_t len = sizeof(addr);
    ::getsockname(srv, reinterpret_cast<struct sockaddr *>(&addr), &len);
    return ntohs(addr.sin_port);
}

struct JoinGuard {
    std::thread &t;
    explicit JoinGuard(std::thread &thread) : t(thread) {}
    ~JoinGuard() {
        if (t.joinable()) t.join();
    }
};

class CodeGenHttpClientTest : public CodeGenTest {
protected:
    AllowPrivateHTTPGuard allow_private_;
};

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

TEST_F(CodeGenHttpClientTest, HttpGetCaseBindingPreservesHttpClientResponseMetadata) {
    std::string response =
        "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, reinterpret_cast<struct sockaddr *>(&client_addr), &client_len);
        mock_http_server(conn, response);
    });
    JoinGuard join_guard(server_thread);

    std::string src = HTTP_CLIENT_DECLS +
        "case http_get(\"http://127.0.0.1:" + std::to_string(port) + "/case\"):\n"
        "    Ok(resp):\n"
        "        print(status(resp))\n"
        "        print(body(resp))\n"
        "        http_client_response_free(resp)\n"
        "    Err(e):\n"
        "        print(\"err:\" + e.message)\n";
    EXPECT_EQ(runSource(src), "200\nhello\n");
    ::close(srv);
}

TEST_F(CodeGenHttpClientTest, HttpRequestCaseBindingPreservesHttpClientResponseMetadata) {
    std::string response =
        "HTTP/1.1 201 Created\r\nContent-Length: 7\r\nConnection: close\r\n\r\ncreated";
    int srv = start_mock_server();
    if (srv < 0) GTEST_SKIP() << "could not create mock server (network unavailable)";
    int port = get_server_port(srv);

    std::string captured_request;
    std::thread server_thread([&]() {
        struct sockaddr_in client_addr{};
        socklen_t client_len = sizeof(client_addr);
        int conn = ::accept(srv, reinterpret_cast<struct sockaddr *>(&client_addr), &client_len);
        captured_request = mock_http_server_capture(conn, response);
    });
    JoinGuard join_guard(server_thread);

    std::string src = HTTP_CLIENT_DECLS +
        "headers: Map<str, str> = {\"X-Test\": \"1\"}\n"
        "case http_request(\"POST\", \"http://127.0.0.1:" + std::to_string(port) + "/echo\", headers, \"payload\"):\n"
        "    Ok(resp):\n"
        "        print(status(resp))\n"
        "        print(body(resp))\n"
        "        http_client_response_free(resp)\n"
        "    Err(e):\n"
        "        print(\"err:\" + e.message)\n";
    EXPECT_EQ(runSource(src), "201\ncreated\n");

    ::close(srv);
    server_thread.join();
    EXPECT_NE(captured_request.find("POST /echo HTTP/1.1"), std::string::npos);
    EXPECT_NE(captured_request.find("X-Test: 1"), std::string::npos);
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
