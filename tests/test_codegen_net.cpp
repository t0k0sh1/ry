#include "test_codegen_common.hpp"

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
// TODO: re-enable once network test hang on CI (macos-14) is resolved
// See: https://github.com/t0k0sh1/ry/issues/96
// ============================================================

// TEST_F(CodeGenTest, NetEchoRoundTrip) {
//     EXPECT_EQ(runSource(NET_DECLS + R"(
// fn run_server(port: int, ready: Channel<int>) -> str:
//     match bind("127.0.0.1", port):
//         case Ok(server):
//             listen(server, 1)
//             send(ready, 1)
//             match accept(server):
//                 case Ok(conn):
//                     data: List<byte> = recv(conn, 4096)
//                     msg = bytes_to_str(data)
//                     send(conn, str_to_bytes("echo:" + msg))
//                     close(conn)
//                 case Err(e):
//                     ...
//             close(server)
//         case Err(e):
//             send(ready, 0)
//     return "done"
//
// fn run_client(port: int) -> str:
//     match connect("127.0.0.1", port):
//         case Ok(conn):
//             send(conn, str_to_bytes("hello"))
//             resp: List<byte> = recv(conn, 4096)
//             msg = bytes_to_str(resp)
//             close(conn)
//             return msg
//         case Err(e):
//             return "fail"
//
// ready: Channel<int> = channel[int]()
// t: Task<str> = spawn run_server(18081, ready)
// recv(ready)
// resp_msg = run_client(18081)
// print(resp_msg)
// join(t)
// )"), "echo:hello\n");
// }

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
