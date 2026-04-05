#include "test_codegen_common.hpp"

class DirectiveTest : public CodeGenTest {};

// --- deriveRuntimeFnName tests ---

TEST(NativeFnNaming, PackageFunction) {
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("base64", "encode"), "__ry_base64_encode");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("base64", "decode"), "__ry_base64_decode");
}

TEST(NativeFnNaming, MathFunctions) {
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("math", "sin"), "__ry_math_sin");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("math", "cos"), "__ry_math_cos");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("math", "floor"), "__ry_math_floor");
}

TEST(NativeFnNaming, EmptyPackage) {
    // Empty package still produces a name, but callers should not use it
    // for builtins since they use varied naming (libc, inline IR, etc.)
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("", "print"), "__ry_print");
}

TEST(NativeFnNaming, MultiWordFunctionName) {
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("filesystem", "list_dir"), "__ry_filesystem_list_dir");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("path", "get_extension"), "__ry_path_get_extension");
}

TEST(NativeFnNaming, ErrorGetter) {
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("base64", "get_last_error"), "__ry_base64_get_last_error");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("io", "get_last_error"), "__ry_io_get_last_error");
}

// --- NativeFnSignature registry tests ---

TEST(NativeFnSigs, RegistryPopulatedAndKeyed) {
    // Compile source with @native declarations and verify the registry
    std::string src =
        "@native\n"
        "function print(value: str) -> Unit\n"
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();

    // Builtins (no package) are keyed by bare name
    ASSERT_TRUE(sigs.count("print"));
    ASSERT_EQ(sigs.at("print").size(), 1u);
    EXPECT_EQ(sigs.at("print")[0].name, "print");
    EXPECT_EQ(sigs.at("print")[0].package, "");
    EXPECT_EQ(sigs.at("print")[0].return_type_name, "Unit");
    ASSERT_EQ(sigs.at("print")[0].params.size(), 1u);
    EXPECT_EQ(sigs.at("print")[0].params[0].name, "value");
    EXPECT_EQ(sigs.at("print")[0].params[0].type_name, "str");

    ASSERT_TRUE(sigs.count("contains"));
    ASSERT_EQ(sigs.at("contains").size(), 1u);
    EXPECT_EQ(sigs.at("contains")[0].name, "contains");
    EXPECT_EQ(sigs.at("contains")[0].return_type_name, "bool");
    ASSERT_EQ(sigs.at("contains")[0].params.size(), 2u);
}

TEST(NativeFnSigs, OverloadsGrouped) {
    std::string src =
        "@native\n"
        "function range(n: int) -> List<int>\n"
        "@native\n"
        "function range(start: int, end_val: int) -> List<int>\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();
    ASSERT_TRUE(sigs.count("range"));
    ASSERT_EQ(sigs.at("range").size(), 2u);
    EXPECT_EQ(sigs.at("range")[0].params.size(), 1u);
    EXPECT_EQ(sigs.at("range")[1].params.size(), 2u);
}

TEST(NativeFnSigs, DirectivesRecorded) {
    std::string src =
        "@native\n"
        "@deprecated\n"
        "function old_fn(x: int) -> int\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();
    ASSERT_TRUE(sigs.count("old_fn"));
    auto &directives = sigs.at("old_fn")[0].directive_names;
    EXPECT_NE(std::find(directives.begin(), directives.end(), "native"),
              directives.end());
    EXPECT_NE(std::find(directives.begin(), directives.end(), "deprecated"),
              directives.end());
}

TEST(NativeFnSigs, LibraryFieldParsed) {
    // @native("base64") parses correctly at AST level
    std::string src =
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    // Verify the directive parameter was parsed
    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_FALSE((*fn)->directives.empty());
    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "native");
    ASSERT_EQ(d.params.size(), 1u);
    EXPECT_EQ(d.params[0].key, "");
    EXPECT_EQ(d.params[0].value, "base64");
    EXPECT_TRUE(d.params[0].is_string);
}

TEST(NativeFnSigs, LibraryFieldStoredAtCodegen) {
    // @native("base64") stores library name in registry.
    // The registry key uses the library name as package when the source
    // file is not under std/<pkg>/ (i.e., deriveNativePackage returns "").
    std::string src =
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();
    // Key is "base64::encode" (library used as effective package)
    ASSERT_TRUE(sigs.count("base64::encode"));
    EXPECT_EQ(sigs.at("base64::encode")[0].library, "base64");
}

TEST(NativeFnSigs, LibraryFieldEmptyForBareNative) {
    std::string src =
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();
    ASSERT_TRUE(sigs.count("contains"));
    EXPECT_EQ(sigs.at("contains")[0].library, "");
}

TEST(NativeFnSigs, GetRequiredLibrariesOnlyIncludesCalledFunctions) {
    // Declares functions from "base64" and "path" libraries, but only calls
    // the base64 function. getRequiredLibraries() should return only "base64".
    std::string src =
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n"
        "@native(\"path\")\n"
        "function basename(p: str) -> str\n"
        "print(encode(\"Hello\"))\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &libs = cg.getRequiredLibraries();
    ASSERT_EQ(libs.size(), 1u);
    EXPECT_TRUE(libs.count("base64"));
}

TEST(NativeFnSigs, GetRequiredLibrariesEmptyWhenNothingCalled) {
    // Declares @native("libname") functions but doesn't call any.
    // getRequiredLibraries() should return empty (demand-driven loading).
    std::string src =
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n"
        "print(\"no native calls\")\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &libs = cg.getRequiredLibraries();
    EXPECT_TRUE(libs.empty());
}

TEST(NativeFnSigs, GetRequiredLibrariesEmptyForBareNative) {
    std::string src =
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n";

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &libs = cg.getRequiredLibraries();
    EXPECT_TRUE(libs.empty());
}

// 1. @deprecated function called -> warning, execution normal
TEST_F(DirectiveTest, DeprecatedFunctionWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "function old_func() -> int:\n"
        "    return 42\n"
        "print(old_func())\n"
    );
    EXPECT_EQ(output, "42\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_func' is deprecated");
}

// 2. @deprecated type constructed -> warning
TEST_F(DirectiveTest, DeprecatedTypeWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "record OldPoint:\n"
        "    x: int\n"
        "    y: int\n"
        "p = OldPoint(1, 2)\n"
        "print(p.x)\n"
    );
    EXPECT_EQ(output, "1\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'OldPoint' is deprecated");
}

// 3. @deprecated let referenced -> warning
TEST_F(DirectiveTest, DeprecatedVariableWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "old_val = 99\n"
        "print(old_val)\n"
    );
    EXPECT_EQ(output, "99\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_val' is deprecated");
}

// 4. @deprecated field accessed -> warning
TEST_F(DirectiveTest, DeprecatedFieldWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "record MyType:\n"
        "    @deprecated\n"
        "    old_field: int\n"
        "    new_field: int\n"
        "m = MyType(1, 2)\n"
        "print(m.old_field)\n"
        "print(m.new_field)\n"
    );
    EXPECT_EQ(output, "1\n2\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'MyType.old_field' is deprecated");
}

// 5. Definition alone does not produce warnings
TEST_F(DirectiveTest, DeprecatedNoWarningOnDefinition) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "function unused_func() -> int:\n"
        "    return 1\n"
        "@deprecated\n"
        "unused_val = 42\n"
        "print(0)\n"
    );
    EXPECT_EQ(output, "0\n");
    EXPECT_TRUE(warnings.empty());
}

// 6. Non-deprecated entities produce no warnings
TEST_F(DirectiveTest, NonDeprecatedNoWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "function good_func() -> int:\n"
        "    return 10\n"
        "good_val = 20\n"
        "print(good_func())\n"
        "print(good_val)\n"
    );
    EXPECT_EQ(output, "10\n20\n");
    EXPECT_TRUE(warnings.empty());
}

// 7. Deprecated function still works correctly
TEST_F(DirectiveTest, DeprecatedFunctionStillWorks) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(add(3, 4))\n"
    );
    EXPECT_EQ(output, "7\n");
    ASSERT_EQ(warnings.size(), 1);
}

// 8. Multiple directives stacked (parse check)
TEST_F(DirectiveTest, MultipleDirectives) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated\n"
        "@deprecated\n"
        "function multi() -> int:\n"
        "    return 1\n"
        "print(multi())\n"
    );
    EXPECT_EQ(output, "1\n");
    EXPECT_FALSE(warnings.empty());
}

// 9. Directive with params parses correctly
TEST_F(DirectiveTest, DirectiveWithParams) {
    auto [output, warnings] = runSourceWithWarnings(
        "@deprecated(reason=\"use new_func instead\")\n"
        "function old_api() -> int:\n"
        "    return 0\n"
        "print(old_api())\n"
    );
    EXPECT_EQ(output, "0\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_api' is deprecated");
}

// 10. Unknown directive causes parse error
TEST_F(DirectiveTest, UnknownDirectiveError) {
    EXPECT_THROW({
        runSource("@unknown\nfn foo() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 10b. Positional string argument on non-@native directive causes parse error
TEST_F(DirectiveTest, PositionalArgOnNonNativeError) {
    EXPECT_THROW({
        runSource("@inline(\"always\")\nfunction add(a: int, b: int) -> int:\n    return a + b\n");
    }, std::runtime_error);
    EXPECT_THROW({
        runSource("@deprecated(\"old\")\nfunction foo() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 10c. @native("") with empty string causes parse error
TEST_F(DirectiveTest, NativeEmptyLibraryNameError) {
    EXPECT_THROW({
        runSource("@native(\"\")\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10d. @native("a", "b") or @native("a", key=val) — extra arguments rejected
TEST_F(DirectiveTest, NativeLibraryExtraArgsError) {
    EXPECT_THROW({
        runSource("@native(\"a\", \"b\")\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
    EXPECT_THROW({
        runSource("@native(\"a\", key=val)\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10e. @native(key=value) — key-value params not allowed for @native
TEST_F(DirectiveTest, NativeKeyValueParamsError) {
    EXPECT_THROW({
        runSource("@native(lib=base64)\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 11. Directive on invalid target causes parse error
TEST_F(DirectiveTest, DirectiveOnInvalidTarget) {
    EXPECT_THROW({
        runSource("@deprecated\nif true\n    print(1)\n");
    }, std::runtime_error);
}

// ===== @native function tests =====

// 12. @native function declaration - builtin function still works
TEST_F(DirectiveTest, NativeFnDeclaration) {
    std::string output = runSource(
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello world\", \"world\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

// 13. @native function operator declaration - builtin operator still works
TEST_F(DirectiveTest, NativeFnOperatorDeclaration) {
    std::string output = runSource(
        "@native\n"
        "function operator+(a: str, b: str) -> str\n"
        "print(\"hello\" + \" world\")\n"
    );
    EXPECT_EQ(output, "hello world\n");
}

// 14. @native function with body causes error
TEST_F(DirectiveTest, NativeFnWithBodyError) {
    EXPECT_THROW({
        runSource("@native\nfn bad() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 15. @native function with UFCS-style builtin
TEST_F(DirectiveTest, NativeFnUfcsBuiltin) {
    std::string output = runSource(
        "@native\n"
        "function to_upper(s: str) -> str\n"
        "print(to_upper(\"hello\"))\n"
    );
    EXPECT_EQ(output, "HELLO\n");
}

// 16. Multiple @native function declarations coexist
TEST_F(DirectiveTest, MultipleNativeFnDeclarations) {
    std::string output = runSource(
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "@native\n"
        "function to_upper(s: str) -> str\n"
        "print(contains(\"hello\", \"ell\"))\n"
        "print(to_upper(\"world\"))\n"
    );
    EXPECT_EQ(output, "true\nWORLD\n");
}

// ===== @native function type signature validation =====

TEST_F(DirectiveTest, NativeFnTypeCheckPass) {
    std::string output = runSource(
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\", \"ell\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

TEST_F(DirectiveTest, NativeFnTypeCheckFailArgCount) {
    EXPECT_THROW(runSource(
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\"))\n"
    ), std::runtime_error);
}

TEST_F(DirectiveTest, NativeFnOverloadResolution) {
    std::string output = runSource(
        "@native\n"
        "function range(n: int) -> List<int>\n"
        "@native\n"
        "function range(start: int, end_val: int) -> List<int>\n"
        "@native\n"
        "function range(start: int, end_val: int, step: int) -> List<int>\n"
        "print(length(range(5)))\n"
        "print(length(range(1, 4)))\n"
        "print(length(range(0, 10, 2)))\n"
    );
    EXPECT_EQ(output, "5\n3\n5\n");
}

TEST_F(DirectiveTest, NativeFnWithoutSignatureStillWorks) {
    // Builtin functions work even without @native declaration
    std::string output = runSource(
        "print(contains(\"hello\", \"ell\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

TEST_F(DirectiveTest, CoreStrDeclarationsWork) {
    std::string output = runSource(
        "@native\n"
        "function to_upper(s: str) -> str\n"
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "@native\n"
        "function starts_with(s: str, prefix: str) -> bool\n"
        "print(to_upper(\"hello\"))\n"
        "print(contains(\"hello world\", \"world\"))\n"
        "print(starts_with(\"hello\", \"hel\"))\n"
    );
    EXPECT_EQ(output, "HELLO\ntrue\ntrue\n");
}

// ===== @native("libname") directive syntax tests =====

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchForStdlibName) {
    // A user-declared @native("base64") fn encode(str) → str (not imported
    // from stdlib) is handled by the table-driven base64 dispatcher because
    // the sig key "base64::encode" matches. Verifies the function is callable.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n"
        "print(encode(\"test\"))\n"
    );
    EXPECT_EQ(output, "dGVzdA==\n");
}

TEST_F(DirectiveTest, NativeFnLibraryDeclarationOnly) {
    // @native("libname") declaration is accepted and registered for dispatch.
    // Without calling the function, the declaration alone is valid.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n"
        "print(\"ok\")\n"
    );
    EXPECT_EQ(output, "ok\n");
}

TEST_F(DirectiveTest, NativeFnLibraryDoesNotShadowBuiltin) {
    // @native("libname") registers for dispatch but does not shadow
    // built-in functions with the same name.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "function contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\", \"ell\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchFallsThrough) {
    // The generic native dispatch (for non-stdlib @native("libname")) falls
    // through on type mismatch, letting user-defined overloads handle the call.
    // This uses "mylib" (not a stdlib name) so it goes through the generic path.
    std::string output = runSource(
        "@native(\"mylib\")\n"
        "function greet(name: str) -> str\n"
        "function greet(x: int) -> str:\n"
        "    return \"int:\" + to_str(x)\n"
        "print(greet(42))\n"
    );
    EXPECT_EQ(output, "int:42\n");
}

// ===== @native("libname") generic dispatch tests =====

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchDirect) {
    // @native("base64") fn encode(data: str) -> str dispatches through the
    // generic path (not the hardcoded base64 dispatch table) because the
    // source is not under std/base64/. The runtime symbol __ry_base64_encode
    // is resolved from the process (statically linked).
    std::string output = runSource(
        "@native(\"base64\")\n"
        "function encode(data: str) -> str\n"
        "print(encode(\"Hello\"))\n"
    );
    EXPECT_EQ(output, "SGVsbG8=\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchResultPtr) {
    // @native("base64") fn decode(data: str) -> Result<str, Error>
    // Tests ResultPtr wrapping in the generic dispatch path.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "function decode(data: str) -> Result<str, Error>\n"
        "match decode(\"SGVsbG8=\"):\n"
        "    case Ok(s):\n"
        "        print(s)\n"
        "    case Err(e):\n"
        "        print(e.message)\n"
    );
    EXPECT_EQ(output, "Hello\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchBool) {
    // Tests BoolFromI64 wrapping in the generic dispatch path.
    std::string output = runSource(
        "@native(\"path\")\n"
        "function is_absolute(p: str) -> bool\n"
        "print(is_absolute(\"/usr/bin\"))\n"
        "print(is_absolute(\"relative\"))\n"
    );
    EXPECT_EQ(output, "true\nfalse\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchResultBool) {
    // Result<bool, Error> must use ResultOutParam with i64 out-param,
    // then truncate to i1. Verifies the codegen doesn't crash or miscompile.
    // We compile-only since no matching C function exists in the test harness.
    EXPECT_NO_THROW(compileSource(
        "@native(\"mylib\")\n"
        "function check(s: str) -> Result<bool, Error>\n"
        "match check(\"test\"):\n"
        "    case Ok(b):\n"
        "        print(b)\n"
        "    case Err(e):\n"
        "        print(e.message)\n"
    ));
}

// ===== @inline tests =====

TEST_F(DirectiveTest, InlineDefault) {
    std::string output = runSource(
        "@inline\n"
        "function add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(add(3, 4))\n"
    );
    EXPECT_EQ(output, "7\n");
}

TEST_F(DirectiveTest, InlineModeAlways) {
    std::string output = runSource(
        "@inline(mode=\"always\")\n"
        "function mul(a: int, b: int) -> int:\n"
        "    return a * b\n"
        "print(mul(5, 6))\n"
    );
    EXPECT_EQ(output, "30\n");
}

TEST_F(DirectiveTest, InlineModeHint) {
    std::string output = runSource(
        "@inline(mode=\"hint\")\n"
        "function sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "print(sub(10, 3))\n"
    );
    EXPECT_EQ(output, "7\n");
}

TEST_F(DirectiveTest, InlineModeNever) {
    std::string output = runSource(
        "@inline(mode=\"never\")\n"
        "function negate(a: int) -> int:\n"
        "    return -a\n"
        "print(negate(-5))\n"
    );
    EXPECT_EQ(output, "5\n");
}

TEST_F(DirectiveTest, InlineInvalidMode) {
    EXPECT_THROW(runSource(
        "@inline(mode=\"aggressive\")\n"
        "function bad() -> int:\n"
        "    return 1\n"
        "print(bad())\n"
    ), std::runtime_error);
}

TEST_F(DirectiveTest, InlineWithNativeError) {
    EXPECT_THROW(runSource(
        "@inline\n"
        "@native\n"
        "function contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\", \"ell\"))\n"
    ), std::runtime_error);
}

TEST_F(DirectiveTest, InlineWithDeprecated) {
    auto [output, warnings] = runSourceWithWarnings(
        "@inline\n"
        "@deprecated\n"
        "function old_add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(old_add(1, 2))\n"
    );
    EXPECT_EQ(output, "3\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_add' is deprecated");
}

TEST_F(DirectiveTest, InlineRecursive) {
    std::string output = runSource(
        "@inline\n"
        "function fact(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * fact(n - 1)\n"
        "print(fact(5))\n"
    );
    EXPECT_EQ(output, "120\n");
}
