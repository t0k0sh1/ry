#include "test_codegen_common.hpp"
#include "ry/native_call_descriptor.hpp"
#include "ry/stdlib_registry.hpp"

using namespace ry;

// =============================================================================
// Pure-function inference rule tests
//
// inferLibraryName is a pure function over (directiveTag, declaringModule),
// so rule (a) / rule (b) / both-empty cases can be exercised directly without
// driving a full compile. This avoids the runSource harness's inability to
// forge `SourceLocation::file_id` to look like `share/std/<M>/<M>.ry` (which
// is what `deriveNativePackage` reads to produce `sig.package`).
// =============================================================================

TEST(NativeCallDescriptor, InferLibrary_ExplicitTagWins) {
    // rule (a): non-empty tag returns as-is regardless of declaring module.
    EXPECT_EQ(inferLibraryName("base64", ""), std::optional<std::string>("base64"));
}

TEST(NativeCallDescriptor, InferLibrary_ExplicitTagWinsOverModule) {
    // rule (a) precedence: explicit tag overrides rule (b) inference.
    EXPECT_EQ(inferLibraryName("io", "math"), std::optional<std::string>("io"));
}

TEST(NativeCallDescriptor, InferLibrary_ModuleKeyedFallback_KnownLib) {
    // rule (b): bare @native in share/std/io/io.ry → library = "io"
    EXPECT_EQ(inferLibraryName("", "io"), std::optional<std::string>("io"));
}

TEST(NativeCallDescriptor, InferLibrary_ModuleKeyedFallback_UnknownLib) {
    // rule (b) miss: math has no libry_math.dylib build artifact.
    EXPECT_EQ(inferLibraryName("", "math"), std::nullopt);
}

TEST(NativeCallDescriptor, InferLibrary_UserCodeBareNative) {
    // rule (b) requires a known module name; user code carries empty
    // declaringModule, so inference produces nullopt.
    EXPECT_EQ(inferLibraryName("", ""), std::nullopt);
}

TEST(NativeCallDescriptor, KnownNativeLibsLocalLiteral) {
    // Local-only consistency guard: catches accidentally editing the C++
    // literal at src/native_call_descriptor.cpp without updating this
    // expected set (or vice versa). Does NOT catch drift between
    // CMakeLists.txt:386 `RY_NATIVE_LIBS` and the C++ list — that
    // cross-file invariant remains hand-maintained until a future PR
    // either injects the list from CMake or adds a configure-time check.
    const std::unordered_set<std::string> expected = {
        "base64", "path", "convert", "filesystem", "gc", "testing",
        "io", "json", "json5", "net", "thread", "http",
    };
    EXPECT_EQ(knownNativeLibs(), expected);
}

// =============================================================================
// End-to-end descriptor storage test (rule (a) only)
//
// runSource bypasses ModuleLoader, so embedded source's `s->loc` cannot be
// forged to look like `share/std/io/io.ry`; rule (b) end-to-end is therefore
// out of reach until a consumer (e.g. dispatchIO descriptor migration) drives
// a real share/std/ source through the build pipeline. Rule (a) works via
// the @native("<lib>") directive tag without any file-path dependency.
// =============================================================================

TEST(NativeCallDescriptor, DescriptorStorage_PopulatedForLibTaggedNative) {
    std::string src =
        "@native(\"base64\")\n"
        "fn encode(data: str) -> str\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("base64::encode");
    ASSERT_NE(it, descs.end())
        << "expected descriptor under key 'base64::encode'";
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    ASSERT_TRUE(desc.library_name.has_value());
    EXPECT_EQ(*desc.library_name, "base64");

    // Pilot fields (#2337): encode(str) -> str → Direct wrapping, str arg
    // gates nothing, base64-keyed error channel populated.
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::Direct);
    EXPECT_TRUE(desc.out_param_type_name.empty());
    EXPECT_EQ(desc.error_channel, "__ry_base64_get_last_error");
    EXPECT_EQ(desc.require_list_u8_arg, -1);
}

// =============================================================================
// Pilot field population tests (#2337) — exercise each populate path so a
// regression in codegen_fn.cpp's descriptor-build site (or in
// inferReturnWrapping) is caught directly, not only via the IR-diff gate.
// =============================================================================

TEST(NativeCallDescriptor, DescriptorStorage_RequireListU8ArgPopulated) {
    // encodeBytes(input: List<u8>) -> str: descriptor captures arg index 0
    // as the List<u8> gate. The runtime check at dispatch time uses this
    // pre-computed index instead of re-scanning params.
    std::string src =
        "@native(\"base64\")\n"
        "fn encodeBytes(input: List<u8>) -> str\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("base64::encodeBytes");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::Direct);
    EXPECT_EQ(desc.require_list_u8_arg, 0)
        << "encodeBytes' first param is List<u8> — descriptor must record idx 0";
    EXPECT_EQ(desc.error_channel, "__ry_base64_get_last_error");
}

TEST(NativeCallDescriptor, DescriptorStorage_ResultPtrAndErrorChannel) {
    // decode(input: str) -> Result<str, Error>: ResultPtr wrapping + the
    // base64-keyed error channel. The descriptor pre-computes both so
    // emitGenericNativeCall's wrapPtrAsResult call uses the same error fn
    // as the legacy table-driven path.
    std::string src =
        "@native(\"base64\")\n"
        "fn decode(input: str) -> Result<str, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("base64::decode");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::ResultPtr);
    EXPECT_TRUE(desc.out_param_type_name.empty());
    EXPECT_EQ(desc.error_channel, "__ry_base64_get_last_error");
    EXPECT_EQ(desc.require_list_u8_arg, -1);
}

TEST(NativeCallDescriptor, DescriptorStorage_BoolFromI64ForPathIsAbsolute) {
    // isAbsolute(p: str) -> bool: BoolFromI64 wrapping, no out param.
    // path's error channel is also populated even though isAbsolute
    // doesn't return a Result (the channel is module-keyed, not per-fn).
    std::string src =
        "@native(\"path\")\n"
        "fn isAbsolute(p: str) -> bool\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("path::isAbsolute");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::BoolFromI64);
    EXPECT_TRUE(desc.out_param_type_name.empty());
    EXPECT_EQ(desc.error_channel, "__ry_path_get_last_error");
}

TEST(NativeCallDescriptor, DescriptorStorage_ResultOutParamWithType) {
    // Result<int, Error> → ResultOutParam with out_param_type_name == "int".
    // Exercises the second element of inferReturnWrapping's return pair,
    // which the descriptor now stores for filesystem's fileSize / isFile
    // / isDir / isSymlink consumers of emitGenericNativeCall.
    std::string src =
        "@native(\"filesystem\")\n"
        "fn fileSize(path: str) -> Result<int, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("filesystem::fileSize");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::ResultOutParam);
    EXPECT_EQ(desc.out_param_type_name, "int");
    EXPECT_EQ(desc.error_channel, "__ry_filesystem_get_last_error");
}

// =============================================================================
// Installment 2-a (#2338): resource_kind population
//
// inferResourceKind looks up the inner type of a Result<T, Error> against
// ResourceKindRegistry. For io::open's `Result<File, Error>` return type
// the registered "File" kind (rk_file, registered at static init in
// src/codegen_call_io.cpp) is captured on the descriptor. Non-resource
// returners (str, int, List<u8>, Unit) get NONE.
// =============================================================================

TEST(NativeCallDescriptor, DescriptorStorage_ResourceKindForOpenReturnsFile) {
    // io::open(path: str, mode: str) -> Result<File, Error>: the descriptor
    // carries rk_file so the descriptor-driven path (and any future migration
    // of io::open out of dispatchIO's custom emitter into emitGenericNativeCall)
    // can tag the wrapped result without a hand-written addResourceKind call.
    std::string src =
        "@native(\"io\")\n"
        "fn open(path: str, mode: str) -> Result<File, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("io::open");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_NE(desc.resource_kind, ResourceKindRegistry::NONE);
    // The kind id is the integer returned by registerKind("File", ...) in
    // codegen_call_io.cpp's static init. Cross-check by name via the
    // registry's getInfo to keep the assertion stable against init order.
    const auto *info = ResourceKindRegistry::instance().getInfo(desc.resource_kind);
    ASSERT_NE(info, nullptr);
    EXPECT_EQ(info->typeName, "File");
}

TEST(NativeCallDescriptor, DescriptorStorage_ResourceKindNoneForReadText) {
    // io::readText returns Result<str, Error> — no resource. The descriptor's
    // resource_kind stays NONE so emitGenericNativeCall's addResourceKind
    // call is skipped on this path.
    std::string src =
        "@native(\"io\")\n"
        "fn readText(path: str) -> Result<str, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("io::readText");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    EXPECT_EQ(it->second[0].resource_kind, ResourceKindRegistry::NONE);
}

TEST(NativeCallDescriptor, DescriptorStorage_ResourceKindNoneForFilesystemFileSize) {
    // filesystem::fileSize returns Result<int, Error> — no resource. Guards
    // the regression where a future change to inferResourceKind might
    // accidentally match `int` against a resource (e.g. if the registry
    // ever gained a primitive-typed entry).
    std::string src =
        "@native(\"filesystem\")\n"
        "fn fileSize(path: str) -> Result<int, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("filesystem::fileSize");
    ASSERT_NE(it, descs.end());
    EXPECT_EQ(it->second[0].resource_kind, ResourceKindRegistry::NONE);
}

// =============================================================================
// Installment 2-b (#2339): per-entry error_channel override via
// ResourceKindRegistry::Info::errorChannelLibrary
//
// TlsStream is the only resource today whose error channel module differs
// from its owning library: it lives in the http library (linkage) but
// reports errors through __ry_tls_get_last_error. The descriptor populator
// reads `errorChannelLibrary` and overrides `error_channel` accordingly,
// independent of the @native package tag.
// =============================================================================

TEST(NativeCallDescriptor, ErrorChannelOverride_TlsStreamGoesToTlsChannel) {
    // @native("net") tlsConnect(...) -> Result<TlsStream, Error>:
    // the package-derived default would be __ry_net_get_last_error, but
    // rk_tls_stream's errorChannelLibrary="tls" overrides it to
    // __ry_tls_get_last_error. The override resolves the descriptor's
    // per-entry mismatch without any @native syntax extension.
    std::string src =
        "@native(\"net\")\n"
        "fn tlsConnect(host: str, port: int) -> Result<TlsStream, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("net::tlsConnect");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.return_wrapping, CodeGenReturnWrapping::ResultPtr);
    EXPECT_EQ(desc.error_channel, "__ry_tls_get_last_error")
        << "TlsStream must override the package-derived __ry_net_get_last_error";
    EXPECT_NE(desc.resource_kind, ResourceKindRegistry::NONE);
    const auto *info = ResourceKindRegistry::instance().getInfo(desc.resource_kind);
    ASSERT_NE(info, nullptr);
    EXPECT_EQ(info->typeName, "TlsStream");
    EXPECT_STREQ(info->library, "http");
    EXPECT_STREQ(info->errorChannelLibrary, "tls");
}

TEST(NativeCallDescriptor, ErrorChannelOverride_TcpStreamUsesPackageDefault) {
    // @native("net") connect(...) -> Result<TcpStream, Error>:
    // rk_tcp_stream's errorChannelLibrary defaults to "net" (same as library)
    // so no override fires; the descriptor keeps the package-derived
    // __ry_net_get_last_error.
    std::string src =
        "@native(\"net\")\n"
        "fn connect(host: str, port: int) -> Result<TcpStream, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("net::connect");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.error_channel, "__ry_net_get_last_error");
    const auto *info = ResourceKindRegistry::instance().getInfo(desc.resource_kind);
    ASSERT_NE(info, nullptr);
    EXPECT_STREQ(info->library, "net");
    EXPECT_STREQ(info->errorChannelLibrary, "net");
}

// =============================================================================
// Installment 2-c (#2381): per-overload override table tests
//
// kOverrides in src/codegen_native_call_descriptor.cpp encodes
// exported_symbol / nul_checks / iterator_elem / wrapping_override for
// the handle-coupled / NUL-checked / file-coupled entries that pre-2-c
// kept custom emitters. The lookups below exercise the populate-side
// path so a regression in either the table literal or the
// lookupNativeOverloadOverride matcher is caught at unit-test granularity.
// =============================================================================

TEST(NativeCallDescriptor, Override_LookupReturnsNulloptForUnknownEntry) {
    EXPECT_FALSE(lookupNativeOverloadOverride("http", "nonExistent", {"str"})
                 .has_value());
    // Matched (package, callee) but mismatched param types still returns
    // nullopt — same-name overloads under a different signature shape
    // (e.g. a hypothetical method(HttpClientResponse) that isn't declared)
    // do not pick up sibling overrides.
    EXPECT_FALSE(lookupNativeOverloadOverride("http", "method", {"HttpClientResponse"})
                 .has_value());
}

TEST(NativeCallDescriptor, Override_IoOpenExportedSymbol) {
    auto ov = lookupNativeOverloadOverride("io", "open", {"str", "str"});
    ASSERT_TRUE(ov.has_value());
    EXPECT_EQ(ov->exported_symbol, "__ry_io_file_open");
    EXPECT_TRUE(ov->nul_checks.empty());
    EXPECT_FALSE(ov->wrapping_overridden);
}

TEST(NativeCallDescriptor, Override_IoLinesIteratorWrappingAndSymbol) {
    auto ov = lookupNativeOverloadOverride("io", "lines", {"File"});
    ASSERT_TRUE(ov.has_value());
    EXPECT_EQ(ov->exported_symbol, "__ry_io_file_read_line");
    EXPECT_EQ(ov->iterator_elem_type_name, "str");
    ASSERT_TRUE(ov->wrapping_overridden);
    EXPECT_EQ(ov->wrapping_override, CodeGenReturnWrapping::IteratorFromHandle);
}

TEST(NativeCallDescriptor, Override_IoReadLineFileResultOutParamOption) {
    auto ov = lookupNativeOverloadOverride("io", "readLine", {"File"});
    ASSERT_TRUE(ov.has_value());
    EXPECT_EQ(ov->exported_symbol, "__ry_io_file_read_line");
    ASSERT_TRUE(ov->wrapping_overridden);
    EXPECT_EQ(ov->wrapping_override,
              CodeGenReturnWrapping::ResultOutParamOption);
}

TEST(NativeCallDescriptor, Override_HttpHeaderNulCheckMaskAndMessage) {
    auto ov = lookupNativeOverloadOverride("http", "header",
                                            {"HttpRequest", "str"});
    ASSERT_TRUE(ov.has_value());
    EXPECT_EQ(ov->exported_symbol, "__ry_http_header");
    ASSERT_EQ(ov->nul_checks.size(), 1u);
    EXPECT_EQ(ov->nul_checks[0].param_index, 1);
    EXPECT_EQ(ov->nul_checks[0].hint, "hdr_key");
    EXPECT_EQ(ov->nul_checks[0].err_global_prefix, "http_hdr_nul");
    EXPECT_EQ(ov->nul_checks[0].err_message,
              "header: key contains embedded NUL");
    ASSERT_TRUE(ov->wrapping_overridden);
    EXPECT_EQ(ov->wrapping_override,
              CodeGenReturnWrapping::OptionFromNullablePtr);
}

TEST(NativeCallDescriptor, Override_HttpRequestTwoNestedNulChecks) {
    // httpRequest checks method (idx 0) then url (idx 1). The vector
    // order drives the nesting in emitGenericNativeCall's NUL-check
    // chain (outer = method, inner = url) so the customEmitter's
    // pre-migration nested emitResultBranch shape is preserved.
    auto ov = lookupNativeOverloadOverride("http", "httpRequest",
        {"str", "str", "Map<str, str>", "str"});
    ASSERT_TRUE(ov.has_value());
    ASSERT_EQ(ov->nul_checks.size(), 2u);
    EXPECT_EQ(ov->nul_checks[0].param_index, 0);
    EXPECT_EQ(ov->nul_checks[0].err_global_prefix, "http_req_method_nul");
    EXPECT_EQ(ov->nul_checks[1].param_index, 1);
    EXPECT_EQ(ov->nul_checks[1].err_global_prefix, "http_req_url_nul");
}

TEST(NativeCallDescriptor, Override_NetSetTimeoutDisambiguatesByHandleType) {
    auto tcp = lookupNativeOverloadOverride("net", "setTimeout",
                                             {"TcpStream", "int"});
    ASSERT_TRUE(tcp.has_value());
    EXPECT_EQ(tcp->exported_symbol, "__ry_tcp_set_timeout");

    auto tls = lookupNativeOverloadOverride("net", "setTimeout",
                                             {"TlsStream", "int"});
    ASSERT_TRUE(tls.has_value());
    EXPECT_EQ(tls->exported_symbol, "__ry_tls_set_timeout");
}

TEST(NativeCallDescriptor, InferHandleParamIndex_FirstResourceParam) {
    // listen(TcpListener, int) -> handle idx 0
    EXPECT_EQ(inferHandleParamIndex({"TcpListener", "int"}), 0);
    // header(HttpRequest, str) -> handle idx 0
    EXPECT_EQ(inferHandleParamIndex({"HttpRequest", "str"}), 0);
    // open(str, str) -> no handle
    EXPECT_EQ(inferHandleParamIndex({"str", "str"}), -1);
    // Hypothetical 2-resource shape: returns the FIRST one (matches
    // current dispatcher convention where the handle is always the lead).
    EXPECT_EQ(inferHandleParamIndex({"File", "TcpStream"}), 0);
}

TEST(NativeCallDescriptor, DescriptorStorage_HandleParamIndexPopulatedForBody) {
    // body(HttpRequest) populates handle_param_index = 0 and
    // handle_resource_kind = rk_http_request via inferHandleParamIndex.
    std::string src =
        "@native(\"http\")\n"
        "fn body(req: HttpRequest) -> str\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("http::body");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.handle_param_index, 0);
    EXPECT_NE(desc.handle_resource_kind, ResourceKindRegistry::NONE);
    const auto *info = ResourceKindRegistry::instance().getInfo(
        desc.handle_resource_kind);
    ASSERT_NE(info, nullptr);
    EXPECT_EQ(info->typeName, "HttpRequest");
    // exported_symbol comes from the kOverrides table — body(HttpRequest)
    // → __ry_http_body; the convention-derived "__ry_http_body" happens
    // to match here, but the explicit override makes the contract durable
    // against future changes to the derivation.
    EXPECT_EQ(desc.exported_symbol, "__ry_http_body");
}

TEST(NativeCallDescriptor, DescriptorStorage_NulChecksPopulatedForHttpHeader) {
    std::string src =
        "@native(\"http\")\n"
        "fn header(req: HttpRequest, key: str) -> Result<Option<str>, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("http::header");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    ASSERT_EQ(desc.nul_checks.size(), 1u);
    EXPECT_EQ(desc.nul_checks[0].param_index, 1);
    EXPECT_EQ(desc.nul_checks[0].err_global_prefix, "http_hdr_nul");
    EXPECT_EQ(desc.return_wrapping,
              CodeGenReturnWrapping::OptionFromNullablePtr);
}

TEST(NativeCallDescriptor, ErrorChannelOverride_HttpResponseUsesPackageDefault) {
    // @native("http") response(...) -> Result<HttpResponse, Error>:
    // rk_http_response's errorChannelLibrary defaults to "http" so the
    // descriptor keeps the package-derived __ry_http_get_last_error.
    std::string src =
        "@native(\"http\")\n"
        "fn response(status: int, headers: Map<str, str>, body: str)"
        " -> Result<HttpResponse, Error>\n"
        "print(\"setup only\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    const auto &descs = cg.getNativeCallDescriptors();
    auto it = descs.find("http::response");
    ASSERT_NE(it, descs.end());
    ASSERT_EQ(it->second.size(), 1u);

    const auto &desc = it->second[0];
    EXPECT_EQ(desc.error_channel, "__ry_http_get_last_error");
    const auto *info = ResourceKindRegistry::instance().getInfo(desc.resource_kind);
    ASSERT_NE(info, nullptr);
    EXPECT_EQ(info->typeName, "HttpResponse");
    EXPECT_STREQ(info->errorChannelLibrary, "http");
}
