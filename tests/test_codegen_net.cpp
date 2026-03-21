#include "test_codegen_common.hpp"

static const std::string NET_DECLS = R"(
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

// ============================================================
// bind returns Option<TcpListener>
// ============================================================

TEST_F(CodeGenTest, NetBindReturnsOption) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
match bind("127.0.0.1", 18080):
    case Some(server):
        print("ok")
        close(server)
    case None:
        print("fail")
)"), "ok\n");
}

// ============================================================
// connect to invalid address returns None
// ============================================================

TEST_F(CodeGenTest, NetConnectInvalidReturnsNone) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
match connect("127.0.0.1", 19999):
    case Some(conn):
        print("connected")
        close(conn)
    case None:
        print("none")
)"), "none\n");
}

// ============================================================
// Echo round-trip: server and client via spawn
// ============================================================

TEST_F(CodeGenTest, NetEchoRoundTrip) {
    EXPECT_EQ(runSource(NET_DECLS + R"(
fn run_server(port: int) -> str:
    match bind("127.0.0.1", port):
        case Some(server):
            listen(server, 1)
            match accept(server):
                case Some(conn):
                    let data: List<byte> = recv(conn, 4096)
                    let msg = bytes_to_str(data)
                    send(conn, str_to_bytes("echo:" + msg))
                    close(conn)
                case None:
                    ...
            close(server)
        case None:
            ...
    return "done"

fn run_client(port: int) -> str:
    match connect("127.0.0.1", port):
        case Some(conn):
            send(conn, str_to_bytes("hello"))
            let resp: List<byte> = recv(conn, 4096)
            let msg = bytes_to_str(resp)
            close(conn)
            return msg
        case None:
            return "fail"

let t: Task<str> = spawn run_server(18081)
let resp_msg = run_client(18081)
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

let ch: Channel<int> = channel[int]()
let t: Task<int> = spawn producer(ch)
let val = recv(ch)
print(val)
close(ch)
join(t)
)"), "42\n");
}
