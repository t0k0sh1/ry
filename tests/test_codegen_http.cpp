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

// All HTTP tests disabled: they hang on CI (macos-14) due to blocking
// accept/recv in http_listen's infinite loop and spawn-based server patterns.
// TODO: re-enable once network test hang on CI (macos-14) is resolved

// TEST_F(CodeGenTest, HttpListenCallback) { ... }
// TEST_F(CodeGenTest, HttpListenWithSpawn) { ... }
// TEST_F(CodeGenTest, HttpHeader) { ... }
// TEST_F(CodeGenTest, HttpResponse404) { ... }
