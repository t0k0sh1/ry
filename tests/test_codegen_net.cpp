#include "test_codegen_common.hpp"
#include "ry/runtime_net.hpp"
#include "ry/runtime_io.hpp"
#include <sys/socket.h>
#include <unistd.h>
#include <cstring>
#include <vector>
#include <thread>

static const std::string NET_DECLS = R"(
@native
fn bind(host: str, port: int) -> Result<TcpListener, Error>
@native
fn listen(listener: TcpListener, backlog: int) -> Result<Unit, Error>
@native
fn accept(listener: TcpListener) -> Result<TcpStream, Error>
@native
fn connect(host: str, port: int) -> Result<TcpStream, Error>
@native
fn tls_connect(host: str, port: int) -> Result<TlsStream, Error>
@native
fn to_bytes(s: str) -> List<u8>
@native
fn bytes_to_str(bs: List<u8>) -> Result<str, Error>
@native
fn set_timeout(stream: TcpStream, ms: int) -> Unit
@native
fn set_receive_timeout(stream: TcpStream, ms: int) -> Unit
@native
fn set_send_timeout(stream: TcpStream, ms: int) -> Unit
@native
fn sleep(ms: int) -> Unit
)";

// ============================================================
// bind returns Result<TcpListener, Error>
// ============================================================

TEST_F(CodeGenTest, NetBindReturnsResult) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when bind("127.0.0.1", 0):
    case Ok(server):
        print("ok")
        close(server)
    case Err(e):
        print("fail")
)"), "ok\n");
}

// ============================================================
// connect to invalid address returns Err
// ============================================================

TEST_F(CodeGenTest, NetConnectInvalidReturnsErr) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when connect("127.0.0.1", 19999):
    case Ok(conn):
        print("connected")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

// ============================================================
// listen returns Result<Unit, Error>
// ============================================================

TEST_F(CodeGenTest, NetListenReturnsResult) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when bind("127.0.0.1", 0):
    case Ok(server):
        when listen(server, 1):
            case Ok(_):
                print("ok")
            case Err(e):
                print("listen err")
        close(server)
    case Err(e):
        print("bind err")
)"), "ok\n");
}

// ============================================================
// Echo round-trip: server and client via async fn
// Binds in main scope, passes listener to async fn for accept.
// ============================================================

TEST_F(CodeGenTest, NetEchoRoundTrip) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
@native
fn listener_port(listener: TcpListener) -> int

async fn run_server(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            when receive(conn, 4096):
                case Ok(data):
                    when bytes_to_str(data):
                        case Ok(msg):
                            when send(conn, to_bytes("echo:" + msg)):
                                case Ok(_):
                                    ...
                                case Err(e):
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

fn run_client(port: int) -> str:
    when connect("127.0.0.1", port):
        case Ok(conn):
            when send(conn, to_bytes("hello")):
                case Ok(_):
                    ...
                case Err(e):
                    ...
            when receive(conn, 4096):
                case Ok(resp):
                    when bytes_to_str(resp):
                        case Ok(msg):
                            close(conn)
                            return msg
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(conn)
            return "fail"
        case Err(e):
            return "fail"

when bind("127.0.0.1", 0):
    case Ok(server):
        when listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = run_server(server)
                resp_msg = run_client(port)
                print(resp_msg)
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        ...
)"), "echo:hello\n");
}

// ============================================================
// __ry_tcp_send: closed fd returns -1 (no exit)
// ============================================================

TEST(TcpSend, ClosedFdReturnsNegative) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);
    ::close(fds[0]);
    ::close(fds[1]);

    // Build a minimal IOListHeader
    IOListHeader header;
    int8_t data[] = {'x'};
    header.data = data;
    header.len = 1;
    header.cap = 1;

    int64_t result = __ry_tcp_send(&fds[0], &header);
    EXPECT_EQ(result, -1);
}

// ============================================================
// __ry_send_all: basic full-send via socketpair
// ============================================================

TEST(SendAll, SmallPayload) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    const char *msg = "hello";
    ssize_t sent = __ry_send_all(fds[0], msg, 5);
    EXPECT_EQ(sent, 5);

    char buf[16] = {};
    ssize_t n = ::recv(fds[1], buf, sizeof(buf), 0);
    EXPECT_EQ(n, 5);
    EXPECT_EQ(std::string(buf, 5), "hello");

    ::close(fds[0]);
    ::close(fds[1]);
}

TEST(SendAll, LargePayloadWithPartialWrites) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Shrink socket buffers to force partial writes
    int bufsize = 4096;
    ::setsockopt(fds[0], SOL_SOCKET, SO_SNDBUF, &bufsize, sizeof(bufsize));
    ::setsockopt(fds[1], SOL_SOCKET, SO_RCVBUF, &bufsize, sizeof(bufsize));

    const size_t payload_size = 128 * 1024;
    std::vector<char> payload(payload_size, 'A');

    // Send in a separate thread since small buffers will block
    std::thread sender([&]() {
        ssize_t sent = __ry_send_all(fds[0], payload.data(), payload_size);
        EXPECT_EQ(sent, (ssize_t)payload_size);
        ::close(fds[0]);
    });

    // Receive all data
    std::vector<char> received;
    received.reserve(payload_size);
    char buf[4096];
    while (true) {
        ssize_t n = ::recv(fds[1], buf, sizeof(buf), 0);
        if (n <= 0) break;
        received.insert(received.end(), buf, buf + n);
    }

    sender.join();
    ::close(fds[1]);

    EXPECT_EQ(received.size(), payload_size);
    EXPECT_EQ(received, payload);
}

TEST(SendAll, ClosedFdReturnsError) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);
    ::close(fds[0]);
    ::close(fds[1]);

    ssize_t sent = __ry_send_all(fds[0], "x", 1);
    EXPECT_EQ(sent, -1);
}

TEST(SendAll, ZeroLengthSucceeds) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    ssize_t sent = __ry_send_all(fds[0], "", 0);
    EXPECT_EQ(sent, 0);

    ::close(fds[0]);
    ::close(fds[1]);
}

// ============================================================
// __ry_tcp_receive: peer close returns empty IOListHeader
// ============================================================

TEST(TcpRecv, PeerCloseReturnsEmptyList) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Close the peer end so receive() returns 0
    ::close(fds[1]);

    // TcpStreamHandle has fd as its first field
    auto *result = (IOListHeader *)__ry_tcp_receive(&fds[0], 4096);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(result->len, 0);
    EXPECT_EQ(result->cap, 0);
    EXPECT_EQ(result->data, nullptr);

    free(result);
    ::close(fds[0]);
}

TEST(TcpRecv, ErrorReturnsNull) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);
    // Close both ends so receive() gets an error (not just EOF)
    ::close(fds[1]);
    ::close(fds[0]);

    // receive on closed fd returns nullptr (error)
    auto *result = (IOListHeader *)__ry_tcp_receive(&fds[0], 4096);
    EXPECT_EQ(result, nullptr);
}

// ============================================================
// set_timeout / set_receive_timeout / set_send_timeout
// ============================================================

TEST_F(CodeGenTest, NetSetTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when connect("127.0.0.1", 19997):
    case Ok(conn):
        set_timeout(conn, 500)
        print("ok")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetSetRecvTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when connect("127.0.0.1", 19996):
    case Ok(conn):
        set_receive_timeout(conn, 500)
        print("ok")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetSetSendTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when connect("127.0.0.1", 19995):
    case Ok(conn):
        set_send_timeout(conn, 500)
        print("ok")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetRecvTimesOutWithShortTimeout) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
@native
fn listener_port(listener: TcpListener) -> int

async fn server_task(server: TcpListener) -> str:
    when accept(server):
        case Ok(conn):
            # Don't send anything - let client timeout
            sleep(500)
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
                t = server_task(server)
                when connect("127.0.0.1", port):
                    case Ok(conn):
                        set_receive_timeout(conn, 100)
                        when receive(conn, 4096):
                            case Ok(data):
                                print("got data")
                            case Err(e):
                                print("timeout")
                        close(conn)
                    case Err(e):
                        print("connect failed")
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        ...
)"), "timeout\n");
}

// ============================================================
// __ry_tcp_set_timeout: runtime-level test
// ============================================================

TEST(TcpTimeout, SetTimeoutSetsSocketOptions) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // TcpStreamHandle layout: { int fd; }
    // socketpair fd can be used directly since TcpStreamHandle.fd is first field
    __ry_tcp_set_timeout(&fds[0], 250);

    struct timeval tv;
    socklen_t len = sizeof(tv);
    ::getsockopt(fds[0], SOL_SOCKET, SO_RCVTIMEO, &tv, &len);
    EXPECT_EQ(tv.tv_sec, 0);
    EXPECT_EQ(tv.tv_usec, 250000);

    ::getsockopt(fds[0], SOL_SOCKET, SO_SNDTIMEO, &tv, &len);
    EXPECT_EQ(tv.tv_sec, 0);
    EXPECT_EQ(tv.tv_usec, 250000);

    ::close(fds[0]);
    ::close(fds[1]);
}

// ============================================================
// tls_connect to invalid host returns Err
// ============================================================

TEST_F(CodeGenTest, TlsConnectInvalidReturnsErr) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when tls_connect("127.0.0.1", 19993):
    case Ok(conn):
        print("connected")
        close(conn)
    case Err(e):
        print("tls err")
)"), "tls err\n");
}

// ============================================================
// TLS send/receive/close overloads compile for TlsStream
// ============================================================

TEST_F(CodeGenTest, TlsStreamOverloadsCompile) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
when tls_connect("127.0.0.1", 19992):
    case Ok(conn):
        when send(conn, to_bytes("hello")):
            case Ok(n):
                print("sent")
            case Err(e):
                print("send err")
        when receive(conn, 4096):
            case Ok(data):
                print("recv ok")
            case Err(e):
                print("recv err")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

TEST(TcpTimeout, SetRecvTimeoutOnly) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    __ry_tcp_set_recv_timeout(&fds[0], 1500);

    struct timeval tv;
    socklen_t len = sizeof(tv);
    ::getsockopt(fds[0], SOL_SOCKET, SO_RCVTIMEO, &tv, &len);
    EXPECT_EQ(tv.tv_sec, 1);
    EXPECT_EQ(tv.tv_usec, 500000);

    ::close(fds[0]);
    ::close(fds[1]);
}

TEST(TcpRecv, ZeroMaxBytesReturnsEmptyList) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    auto *result = (IOListHeader *)__ry_tcp_receive(&fds[0], 0);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(result->len, 0);
    EXPECT_EQ(result->cap, 0);
    EXPECT_EQ(result->data, nullptr);

    free(result);
    ::close(fds[0]);
    ::close(fds[1]);
}
