// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"
#include <cerrno>
#include <cstdlib>
#include <string>
#include <thread>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <poll.h>
#include <unistd.h>


using namespace ry;
// Post-#2339: net helpers used by http server tests carry the explicit
// `@native("net")` tag for descriptor-driven dispatch (mirrors the io
// test rewrite in #2338). `sleep` stays bare because thread remains
// table-driven through Installment 3.
static const std::string HTTP_DECLS = R"(
@native("net")
fn bind(host: str, port: int) -> Result<TcpListener, Error>
@native("net")
fn listen(listener: TcpListener, backlog: int) -> Result<Unit, Error>
@native("net")
fn accept(listener: TcpListener) -> Result<TcpStream, Error>
@native("net")
fn connect(host: str, port: int) -> Result<TcpStream, Error>
@native("net")
fn listenerPort(listener: TcpListener) -> int
@native("io")
fn toBytes(s: str) -> List<u8>
@native("io")
fn bytesToStr(bs: List<u8>) -> Result<str, Error>
@native
fn sleep(ms: int) -> Unit
)";

static const std::string HTTP_CLIENT_DECLS = R"(
@native("http")
fn httpGet(url: str) -> Result<HttpClientResponse, Error>
@native("http")
fn httpRequest(method: str, url: str, headers: Map<str, str>, body: str) -> Result<HttpClientResponse, Error>
@native("http")
fn status(response: HttpClientResponse) -> int
@native("http")
fn body(response: HttpClientResponse) -> str
@native("http")
fn httpClientResponseFree(response: HttpClientResponse) -> Unit
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

// Wait up to timeout_sec for a connection on `srv`, then accept.
// Returns -1 on timeout/error so the caller's ADD_FAILURE path runs and
// the server thread exits, allowing JoinGuard to join without hanging.
static int accept_with_timeout(int srv, int timeout_sec) {
    struct pollfd pfd{};
    pfd.fd = srv;
    pfd.events = POLLIN;
    int ready;
    do {
        ready = ::poll(&pfd, 1, timeout_sec * 1000);
    } while (ready < 0 && errno == EINTR);
    if (ready <= 0) return -1;
    if (pfd.revents & (POLLERR | POLLNVAL | POLLHUP)) return -1;
    struct sockaddr_in client_addr{};
    socklen_t client_len = sizeof(client_addr);
    return ::accept(srv, reinterpret_cast<struct sockaddr *>(&client_addr), &client_len);
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

// ===== [contract] Manual server: tests HTTP response parsing via raw TCP =====
// Uses dynamic port allocation. Server binds in main scope, passes listener to
// async fn for accept.

TEST_F(CodeGenTest, ManualHttpServer200) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manualServer(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 6\r\n\r\nHello!"
                    case send(conn, toBytes(response_str)):
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
                port = listenerPort(server)
                t = manualServer(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, toBytes("GET /hello HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytesToStr(resp):
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
                blockOn(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\ntrue\n");
}

// ===== [contract] Manual server echoing method:path via raw TCP =====

TEST_F(CodeGenTest, ManualHttpServerEcho) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn runServer(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 14\r\n\r\nPOST:/api/data"
                    case send(conn, toBytes(response_str)):
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
                port = listenerPort(server)
                t = runServer(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, toBytes("POST /api/data HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytesToStr(resp):
                                    Ok(response_msg):
                                        print(contains(response_msg, "POST:/api/data"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                blockOn(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\n");
}

// ===== [contract] 404 response =====

TEST_F(CodeGenTest, HttpResponse404) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manualServer(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 9\r\n\r\nNot Found"
                    case send(conn, toBytes(response_str)):
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
                port = listenerPort(server)
                t = manualServer(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, toBytes("GET /missing HTTP/1.1\r\nHost: localhost\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytesToStr(resp):
                                    Ok(response_msg):
                                        print(contains(response_msg, "404 Not Found"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                blockOn(t)
            Err(e):
                ...
    Err(e):
        ...
)"), "true\n");
}

// ===== [contract] HttpHeader: server echoes a custom request header value =====

TEST_F(CodeGenTest, ManualHttpServerHeader) {
    EXPECT_EQ(runSource(HTTP_DECLS + R"(
async fn manualServer(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    response_str = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 17\r\n\r\nheader:test-value"
                    case send(conn, toBytes(response_str)):
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
                port = listenerPort(server)
                t = manualServer(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        case send(conn, toBytes("GET / HTTP/1.1\r\nHost: localhost\r\nX-Custom: test-value\r\n\r\n")):
                            Ok(_):
                                ...
                            Err(e):
                                ...
                        case receive(conn, 4096):
                            Ok(resp):
                                case bytesToStr(resp):
                                    Ok(msg):
                                        print(contains(msg, "header:test-value"))
                                    Err(e):
                                        print("false")
                            Err(e):
                                print("false")
                        close(conn)
                    Err(e):
                        print("false")
                blockOn(t)
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
        int conn = accept_with_timeout(srv, 10);
        if (conn < 0) {
            ADD_FAILURE() << "accept failed";
            return;
        }
        mock_http_server(conn, response);
    });
    JoinGuard join_guard(server_thread);

    std::string src = HTTP_CLIENT_DECLS +
        "case httpGet(\"http://127.0.0.1:" + std::to_string(port) + "/case\"):\n"
        "    Ok(resp):\n"
        "        print(status(resp))\n"
        "        print(body(resp))\n"
        "        httpClientResponseFree(resp)\n"
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
        int conn = accept_with_timeout(srv, 10);
        if (conn < 0) {
            ADD_FAILURE() << "accept failed";
            return;
        }
        captured_request = mock_http_server_capture(conn, response);
    });
    JoinGuard join_guard(server_thread);

    std::string src = HTTP_CLIENT_DECLS +
        "headers: Map<str, str> = {\"X-Test\": \"1\"}\n"
        "case httpRequest(\"POST\", \"http://127.0.0.1:" + std::to_string(port) + "/echo\", headers, \"payload\"):\n"
        "    Ok(resp):\n"
        "        print(status(resp))\n"
        "        print(body(resp))\n"
        "        httpClientResponseFree(resp)\n"
        "    Err(e):\n"
        "        print(\"err:\" + e.message)\n";
    EXPECT_EQ(runSource(src), "201\ncreated\n");

    ::close(srv);
    server_thread.join();
    EXPECT_NE(captured_request.find("POST /echo HTTP/1.1"), std::string::npos);
    EXPECT_NE(captured_request.find("X-Test: 1"), std::string::npos);
}

// ===== [contract] listen with max_requests: server exits after N requests =====
// Uses async fn + port_callback + blockOn to verify server lifecycle.

static const std::string HTTP_LISTEN_DECLS = HTTP_DECLS + R"(
@native("http")
fn listen(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>) -> Result<Unit, Error>
@native("http")
fn listen(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, maxRequests: int) -> Result<Unit, Error>
@native("http")
fn listen(host: str, port: int, handler: fn(HttpRequest) -> Result<HttpResponse, Error>, maxRequests: int, portCallback: fn(int) -> Unit) -> Result<Unit, Error>
@native("http")
fn method(req: HttpRequest) -> str
@native("http")
fn path(req: HttpRequest) -> str
@native("http")
fn response(status: int, headers: Map<str, str>, body: str) -> Result<HttpResponse, Error>
@native("thread")
fn atomicIntNew(value: int) -> AtomicInt
@native("thread")
fn atomicIntLoad(atomic: AtomicInt) -> int
@native("thread")
fn atomicIntStore(atomic: AtomicInt, value: int) -> Unit
@native("thread")
fn atomicIntFree(atomic: AtomicInt) -> Unit
)";

TEST_F(CodeGenTest, HttpListenMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server(portHolder: AtomicInt) -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 1, (p: int) -> Unit:
        atomicIntStore(portHolder, p)
    )
    return "done"

portHolder = atomicIntNew(0)
t = server(portHolder)
port = 0
while port == 0:
    port = atomicIntLoad(portHolder)
case connect("127.0.0.1", port):
    Ok(conn):
        case send(conn, toBytes("GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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
result = blockOn(t)
atomicIntFree(portHolder)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpListenMaxRequestsMultiple) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server(portHolder: AtomicInt) -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2, (p: int) -> Unit:
        atomicIntStore(portHolder, p)
    )
    return "done"

portHolder = atomicIntNew(0)
t = server(portHolder)
port = 0
while port == 0:
    port = atomicIntLoad(portHolder)

# First request
case connect("127.0.0.1", port):
    Ok(conn):
        case send(conn, toBytes("GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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
case connect("127.0.0.1", port):
    Ok(conn):
        case send(conn, toBytes("GET /second HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
                    Ok(msg):
                        print(contains(msg, "/second"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")

result = blockOn(t)
atomicIntFree(portHolder)
print(result)
)"), "true\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAlive) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server(portHolder: AtomicInt) -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 2, (p: int) -> Unit:
        atomicIntStore(portHolder, p)
    )
    return "done"

portHolder = atomicIntNew(0)
t = server(portHolder)
port = 0
while port == 0:
    port = atomicIntLoad(portHolder)

# Send two requests on the same connection (keep-alive)
case connect("127.0.0.1", port):
    Ok(conn):
        # First request
        case send(conn, toBytes("GET /first HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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
        case send(conn, toBytes("GET /second HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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

result = blockOn(t)
atomicIntFree(portHolder)
print(result)
)"), "true\ntrue\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpConnectionClose) {
    // The portCallback is a single-expression `-> Unit` lambda
    // (`=> atomicIntStore(...)`) bound to a local and passed to `listen` to pin
    // the #2421 fix end-to-end: prior to #2421 this lambda form crashed codegen
    // (single-expr Unit lambda path) AND the 5-arg `http.listen` mis-invoked
    // any capturing callback (raw void(i64) call vs. closure struct). Hits both
    // fixes in one execution path.
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server(portHolder: AtomicInt) -> str:
    pcb = (p: int) -> Unit => atomicIntStore(portHolder, p)
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        return response(200, {"Content-Type": "text/plain"}, "ok")
    , 2, pcb)
    return "done"

portHolder = atomicIntNew(0)
t = server(portHolder)
port = 0
while port == 0:
    port = atomicIntLoad(portHolder)

# Send request with Connection: close
case connect("127.0.0.1", port):
    Ok(conn):
        case send(conn, toBytes("GET /test HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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

# Second request on new connection to reach maxRequests
case connect("127.0.0.1", port):
    Ok(conn):
        case send(conn, toBytes("GET /test2 HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
                    Ok(msg):
                        print(contains(msg, "200 OK"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")
        close(conn)
    Err(e):
        print("false")

result = blockOn(t)
atomicIntFree(portHolder)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}

TEST_F(CodeGenTest, HttpKeepAliveWithMaxRequests) {
    EXPECT_EQ(runSource(HTTP_LISTEN_DECLS + R"(
async fn server(portHolder: AtomicInt) -> str:
    listen("127.0.0.1", 0, (req: HttpRequest) -> Result<HttpResponse, Error>:
        path = path(req)
        return response(200, {"Content-Type": "text/plain"}, path)
    , 3, (p: int) -> Unit:
        atomicIntStore(portHolder, p)
    )
    return "done"

portHolder = atomicIntNew(0)
t = server(portHolder)
port = 0
while port == 0:
    port = atomicIntLoad(portHolder)

# Send 3 requests on the same keep-alive connection
case connect("127.0.0.1", port):
    Ok(conn):
        # Request 1
        case send(conn, toBytes("GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
                    Ok(msg):
                        print(contains(msg, "/a"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")

        # Request 2
        case send(conn, toBytes("GET /b HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
                    Ok(msg):
                        print(contains(msg, "/b"))
                    Err(e):
                        print("false")
            Err(e):
                print("false")

        # Request 3 (triggers shutdown)
        case send(conn, toBytes("GET /c HTTP/1.1\r\nHost: localhost\r\n\r\n")):
            Ok(_):
                ...
            Err(e):
                ...
        case receive(conn, 4096):
            Ok(resp):
                case bytesToStr(resp):
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

result = blockOn(t)
atomicIntFree(portHolder)
print(result)
)"), "true\ntrue\ntrue\ndone\n");
}
