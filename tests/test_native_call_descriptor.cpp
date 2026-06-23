#include "test_codegen_common.hpp"
#include "ry/native_call_descriptor.hpp"

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
}
