#include "test_codegen_common.hpp"
#include "ry/runtime_net.hpp"
#include "ry/runtime_io.hpp"
#include <sys/socket.h>
#include <unistd.h>
#include <cstring>
#include <vector>
#include <thread>


using namespace ry;
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
fn tlsConnect(host: str, port: int) -> Result<TlsStream, Error>
@native
fn toBytes(s: str) -> List<u8>
@native
fn bytesToStr(bs: List<u8>) -> Result<str, Error>
@native
fn setTimeout(stream: TcpStream, ms: int) -> Unit
@native
fn setReceiveTimeout(stream: TcpStream, ms: int) -> Unit
@native
fn setSendTimeout(stream: TcpStream, ms: int) -> Unit
@native
fn sleep(ms: int) -> Unit
)";

// ============================================================
// bind returns Result<TcpListener, Error>
// ============================================================

TEST_F(CodeGenTest, NetBindReturnsResult) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case bind("127.0.0.1", 0):
    Ok(server):
        print("ok")
        close(server)
    Err(e):
        print("fail")
)"), "ok\n");
}

// ============================================================
// connect to invalid address returns Err
// ============================================================

TEST_F(CodeGenTest, NetConnectInvalidReturnsErr) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case connect("127.0.0.1", 19999):
    Ok(conn):
        print("connected")
        close(conn)
    Err(e):
        print("err")
)"), "err\n");
}

// ============================================================
// listen returns Result<Unit, Error>
// ============================================================

TEST_F(CodeGenTest, NetListenReturnsResult) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case bind("127.0.0.1", 0):
    Ok(server):
        case listen(server, 1):
            Ok(_):
                print("ok")
            Err(e):
                print("listen err")
        close(server)
    Err(e):
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
fn listenerPort(listener: TcpListener) -> int

async fn run_server(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            case receive(conn, 4096):
                Ok(data):
                    case bytesToStr(data):
                        Ok(msg):
                            case send(conn, toBytes("echo:" + msg)):
                                Ok(_):
                                    ...
                                Err(e):
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

fn run_client(port: int) -> str:
    case connect("127.0.0.1", port):
        Ok(conn):
            case send(conn, toBytes("hello")):
                Ok(_):
                    ...
                Err(e):
                    ...
            case receive(conn, 4096):
                Ok(resp):
                    case bytesToStr(resp):
                        Ok(msg):
                            close(conn)
                            return msg
                        Err(e):
                            ...
                Err(e):
                    ...
            close(conn)
            return "fail"
        Err(e):
            return "fail"

case bind("127.0.0.1", 0):
    Ok(server):
        case listen(server, 1):
            Ok(_):
                port = listenerPort(server)
                t = run_server(server)
                resp_msg = run_client(port)
                print(resp_msg)
                blockOn(t)
            Err(e):
                ...
    Err(e):
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

    arc_free(result);
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
// setTimeout / setReceiveTimeout / setSendTimeout
// ============================================================

TEST_F(CodeGenTest, NetSetTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case connect("127.0.0.1", 19997):
    Ok(conn):
        setTimeout(conn, 500)
        print("ok")
        close(conn)
    Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetSetRecvTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case connect("127.0.0.1", 19996):
    Ok(conn):
        setReceiveTimeout(conn, 500)
        print("ok")
        close(conn)
    Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetSetSendTimeoutCompiles) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case connect("127.0.0.1", 19995):
    Ok(conn):
        setSendTimeout(conn, 500)
        print("ok")
        close(conn)
    Err(e):
        print("err")
)"), "err\n");
}

TEST_F(CodeGenTest, NetRecvTimesOutWithShortTimeout) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
@native
fn listenerPort(listener: TcpListener) -> int

async fn server_task(server: TcpListener) -> str:
    case accept(server):
        Ok(conn):
            # Don't send anything - let client timeout
            sleep(500)
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
                t = server_task(server)
                case connect("127.0.0.1", port):
                    Ok(conn):
                        setReceiveTimeout(conn, 100)
                        case receive(conn, 4096):
                            Ok(data):
                                print("got data")
                            Err(e):
                                print("timeout")
                        close(conn)
                    Err(e):
                        print("connect failed")
                blockOn(t)
            Err(e):
                ...
    Err(e):
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
// tlsConnect to invalid host returns Err
// ============================================================

TEST_F(CodeGenTest, TlsConnectInvalidReturnsErr) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case tlsConnect("127.0.0.1", 19993):
    Ok(conn):
        print("connected")
        close(conn)
    Err(e):
        print("tls err")
)"), "tls err\n");
}

// ============================================================
// TLS send/receive/close overloads compile for TlsStream
// ============================================================

TEST_F(CodeGenTest, TlsStreamOverloadsCompile) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
case tlsConnect("127.0.0.1", 19992):
    Ok(conn):
        case send(conn, toBytes("hello")):
            Ok(n):
                print("sent")
            Err(e):
                print("send err")
        case receive(conn, 4096):
            Ok(data):
                print("recv ok")
            Err(e):
                print("recv err")
        close(conn)
    Err(e):
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

    arc_free(result);
    ::close(fds[0]);
    ::close(fds[1]);
}
