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
fn listen(listener: TcpListener, backlog: int) -> Unit
@native
fn accept(listener: TcpListener) -> Result<TcpStream, Error>
@native
fn connect(host: str, port: int) -> Result<TcpStream, Error>
@native
fn str_to_bytes(s: str) -> List<byte>
@native
fn bytes_to_str(bs: List<byte>) -> Result<str, Error>
)";

// ============================================================
// bind returns Result<TcpListener, Error>
// ============================================================

TEST_F(CodeGenTest, NetBindReturnsResult) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
match bind("127.0.0.1", 18080):
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
match connect("127.0.0.1", 19999):
    case Ok(conn):
        print("connected")
        close(conn)
    case Err(e):
        print("err")
)"), "err\n");
}

// ============================================================
// Echo round-trip: server and client via spawn
// Uses dynamic port allocation (port 0 + listener_port) to avoid conflicts.
// ============================================================

TEST_F(CodeGenTest, NetEchoRoundTrip) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
@native
fn listener_port(listener: TcpListener) -> int

fn run_server(ready: Channel<int>) -> str:
    match bind("127.0.0.1", 0):
        case Ok(server):
            listen(server, 1)
            port = listener_port(server)
            send(ready, port)
            match accept(server):
                case Ok(conn):
                    data: List<byte> = recv(conn, 4096)
                    match bytes_to_str(data):
                        case Ok(msg):
                            send(conn, str_to_bytes("echo:" + msg))
                        case Err(e):
                            ...
                    close(conn)
                case Err(e):
                    ...
            close(server)
        case Err(e):
            send(ready, 0)
    return "done"

fn run_client(port: int) -> str:
    match connect("127.0.0.1", port):
        case Ok(conn):
            send(conn, str_to_bytes("hello"))
            resp: List<byte> = recv(conn, 4096)
            match bytes_to_str(resp):
                case Ok(msg):
                    close(conn)
                    return msg
                case Err(e):
                    close(conn)
                    return "fail"
        case Err(e):
            return "fail"

ready: Channel<int> = channel[int]()
t: Task<str> = spawn run_server(ready)
port = recv(ready)
resp_msg = run_client(port)
print(resp_msg)
join(t)
)"), "echo:hello\n");
}

// ============================================================
// send/recv/close overload coexistence with Channel
// ============================================================

TEST_F(CodeGenTest, NetSendRecvCloseOverloadWithChannel) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
fn producer(ch: Channel<int>) -> int:
    send(ch, 42)
    return 0

ch: Channel<int> = channel[int]()
t: Task<int> = spawn producer(ch)
val = recv(ch)
print(val)
close(ch)
join(t)
)"), "42\n");
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
// __ry_tcp_recv: peer close returns empty IOListHeader
// ============================================================

TEST(TcpRecv, PeerCloseReturnsEmptyList) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    // Close the peer end so recv() returns 0
    ::close(fds[1]);

    // TcpStreamHandle has fd as its first field
    auto *result = (IOListHeader *)__ry_tcp_recv(&fds[0], 4096);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(result->len, 0);
    EXPECT_EQ(result->cap, 0);
    EXPECT_EQ(result->data, nullptr);

    free(result);
    ::close(fds[0]);
}

TEST(TcpRecv, ZeroMaxBytesReturnsEmptyList) {
    int fds[2];
    ASSERT_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);

    auto *result = (IOListHeader *)__ry_tcp_recv(&fds[0], 0);
    ASSERT_NE(result, nullptr);
    EXPECT_EQ(result->len, 0);
    EXPECT_EQ(result->cap, 0);
    EXPECT_EQ(result->data, nullptr);

    free(result);
    ::close(fds[0]);
    ::close(fds[1]);
}
