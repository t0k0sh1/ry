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
