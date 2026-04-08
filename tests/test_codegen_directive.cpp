#include "test_codegen_common.hpp"
#include <cstring>


using namespace ry;
// Test-only native symbol for exercising the generic dispatch path.
// This is NOT covered by any hardcoded stdlib dispatcher, so calls
// to @native("testlib") fn greet(str) MUST go through emitGenericNativeCall.
extern "C" const char *__ry_testlib_greet(const char *name) {
    static thread_local char buf[256];
    snprintf(buf, sizeof(buf), "hello %s", name);
    return strdup(buf);
}

// Result<bool, Error> test symbol: returns status 0 (ok) with bool out-param.
extern "C" int64_t __ry_testlib_check(const char *s, int64_t *out) {
    *out = (strcmp(s, "yes") == 0) ? 1 : 0;
    return 0;  // 0 = success
}

static thread_local char testlib_err_buf[64] = {0};
extern "C" const char *__ry_testlib_get_last_error() {
    return strdup(testlib_err_buf);
}

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
    EXPECT_EQ(sigs.at("print")[0].returnTypeName, "Unit");
    ASSERT_EQ(sigs.at("print")[0].params.size(), 1u);
    EXPECT_EQ(sigs.at("print")[0].params[0].name, "value");
    EXPECT_EQ(sigs.at("print")[0].params[0].typeName, "str");

    ASSERT_TRUE(sigs.count("contains"));
    ASSERT_EQ(sigs.at("contains").size(), 1u);
    EXPECT_EQ(sigs.at("contains")[0].name, "contains");
    EXPECT_EQ(sigs.at("contains")[0].returnTypeName, "bool");
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
    auto &directives = sigs.at("old_fn")[0].directiveNames;
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

    // Verify the directive argument was parsed
    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_FALSE((*fn)->directives.empty());
    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "native");
    ASSERT_EQ(d.args.size(), 1u);
    EXPECT_FALSE(d.args[0].name.has_value());  // positional argument
    ASSERT_NE(d.args[0].value, nullptr);
    auto *strExpr = std::get_if<StringExpr>(&d.args[0].value->data);
    ASSERT_NE(strExpr, nullptr);
    EXPECT_EQ(strExpr->value, "base64");
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

// 10e-1. @native(123) — non-string positional argument causes validation error
TEST_F(DirectiveTest, NativeNonStringArgError) {
    EXPECT_THROW({
        runSource("@native(123)\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10e. @native(key=value) — key-value params not allowed for @native
TEST_F(DirectiveTest, NativeKeyValueParamsError) {
    EXPECT_THROW({
        runSource("@native(lib=base64)\nfunction foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10f. Unknown directive on RecordStmt causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnRecordError) {
    EXPECT_THROW({
        runSource("@unknown\nrecord Foo:\n    x: int\n");
    }, std::runtime_error);
}

// 10g. Unknown directive on a record field causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnFieldError) {
    EXPECT_THROW({
        runSource("record Foo:\n    @unknown\n    x: int\n");
    }, std::runtime_error);
}

// 10h. Unknown directive on AssignStmt causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnAssignError) {
    EXPECT_THROW({
        runSource("@unknown\nx = 42\n");
    }, std::runtime_error);
}

// 10i. Unknown directive on ForStmt causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnForError) {
    EXPECT_THROW({
        runSource("@unknown\nfor i in range(10):\n    print(i)\n");
    }, std::runtime_error);
}

// 10j. @deprecated with positional arg on record causes codegen error
TEST_F(DirectiveTest, DeprecatedPositionalArgOnRecordError) {
    EXPECT_THROW({
        runSource("@deprecated(\"old\")\nrecord Foo:\n    x: int\n");
    }, std::runtime_error);
}

// 10k. Unknown directive on TupleDestructStmt causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnTupleDestructError) {
    EXPECT_THROW({
        runSource("@unknown\na, b = (1, 2)\n");
    }, std::runtime_error);
}

// 10l. Unknown directive on CallStmt causes codegen error
TEST_F(DirectiveTest, UnknownDirectiveOnCallStmtError) {
    EXPECT_THROW({
        runSource("@unknown\nprint(1)\n");
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

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchEndToEnd) {
    // This test exercises the FULL generic dispatch path with a symbol
    // (__ry_testlib_greet) that is NOT covered by any hardcoded stdlib
    // dispatcher. The call MUST go through emitGenericNativeCall.
    std::string output = runSource(
        "@native(\"testlib\")\n"
        "function greet(name: str) -> str\n"
        "print(greet(\"world\"))\n"
    );
    EXPECT_EQ(output, "hello world\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchDirect) {
    // @native("base64") fn encode(data: str) -> str — the sig key
    // "base64::encode" is found by the table-driven base64 dispatcher.
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
    // Result<bool, Error> via the generic dispatch path with a real
    // runtime symbol (__ry_testlib_check). Exercises the full ResultOutParam
    // ABI: i64 status, i64 out-param, and the final i64→i1 truncation.
    std::string output = runSource(
        "@native(\"testlib\")\n"
        "function check(s: str) -> Result<bool, Error>\n"
        "match check(\"yes\"):\n"
        "    case Ok(b):\n"
        "        print(b)\n"
        "    case Err(e):\n"
        "        print(e.message)\n"
    );
    EXPECT_EQ(output, "true\n");
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

// ===== Generalized directive invocation syntax (#663) =====

// @it("description") is parseable on a function declaration (AST check)
TEST(DirectiveSyntax, ItDirectiveParsedOnFunction) {
    std::string src =
        "@it(\"should add integers\")\n"
        "function test_add():\n"
        "    expect(1 + 2).to_eq(3)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "it");
    ASSERT_EQ(d.args.size(), 1u);
    EXPECT_FALSE(d.args[0].name.has_value());
    auto *strExpr = std::get_if<StringExpr>(&d.args[0].value->data);
    ASSERT_NE(strExpr, nullptr);
    EXPECT_EQ(strExpr->value, "should add integers");
}

// @describe("group") is parseable on a function declaration (AST check)
TEST(DirectiveSyntax, DescribeDirectiveParsedOnFunction) {
    std::string src =
        "@describe(\"calculator\")\n"
        "function calculator_tests():\n"
        "    expect(1 + 1).to_eq(2)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "describe");
    ASSERT_EQ(d.args.size(), 1u);
    EXPECT_FALSE(d.args[0].name.has_value());
    auto *strExpr = std::get_if<StringExpr>(&d.args[0].value->data);
    ASSERT_NE(strExpr, nullptr);
    EXPECT_EQ(strExpr->value, "calculator");
}

// Mixed positional + named arguments parse correctly
TEST(DirectiveSyntax, MixedPositionalAndNamedArgs) {
    // @property(count=50) — named arg with numeric value
    std::string src =
        "@property(count=50)\n"
        "@it(\"should commute\")\n"
        "function test_commute(a: int, b: int):\n"
        "    expect(a + b).to_eq(b + a)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 2u);

    // @property directive
    auto &prop = (*fn)->directives[0];
    EXPECT_EQ(prop.name, "property");
    ASSERT_EQ(prop.args.size(), 1u);
    ASSERT_TRUE(prop.args[0].name.has_value());
    EXPECT_EQ(*prop.args[0].name, "count");
    auto *numExpr = std::get_if<NumberExpr>(&prop.args[0].value->data);
    ASSERT_NE(numExpr, nullptr);
    EXPECT_EQ(numExpr->value, 50);

    // @it directive
    auto &it = (*fn)->directives[1];
    EXPECT_EQ(it.name, "it");
    ASSERT_EQ(it.args.size(), 1u);
    EXPECT_FALSE(it.args[0].name.has_value());
    auto *strExpr = std::get_if<StringExpr>(&it.args[0].value->data);
    ASSERT_NE(strExpr, nullptr);
    EXPECT_EQ(strExpr->value, "should commute");
}

// One directive with both positional and named args in the same invocation.
// This verifies the parser correctly captures both arg kinds in a single directive.
TEST(DirectiveSyntax, MixedPositionalAndNamedArgsInOneDirective) {
    // Use a hypothetical directive name — parser accepts any name; validation is deferred.
    std::string src =
        "@custom_directive(\"label\", count=50)\n"
        "function foo():\n"
        "    expect(1).to_eq(1)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);

    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "custom_directive");
    ASSERT_EQ(d.args.size(), 2u);

    // First arg: positional string "label"
    EXPECT_FALSE(d.args[0].name.has_value());
    auto *strExpr = std::get_if<StringExpr>(&d.args[0].value->data);
    ASSERT_NE(strExpr, nullptr);
    EXPECT_EQ(strExpr->value, "label");

    // Second arg: named "count" = 50
    ASSERT_TRUE(d.args[1].name.has_value());
    EXPECT_EQ(*d.args[1].name, "count");
    auto *numExpr = std::get_if<NumberExpr>(&d.args[1].value->data);
    ASSERT_NE(numExpr, nullptr);
    EXPECT_EQ(numExpr->value, 50);
}

// ===== @it / @describe directive codegen tests (#634) =====

// Basic @it on a named function compiles and runs as a test case
TEST_F(DirectiveTest, ItDirectiveBasicCodegen) {
    EXPECT_EQ(runTestSource(
        "@it(\"adds 1 + 2 = 3\")\n"
        "function test_add():\n"
        "    expect(1 + 2).to_eq(3)\n"
    ), "\033[32m+ adds 1 + 2 = 3\033[0m\n\n1 passed, 0 failed\n");
}

// @it requires test mode — should error outside test mode
TEST_F(DirectiveTest, ItDirectiveRequiresTestMode) {
    EXPECT_THROW(
        compileSource(
            "@it(\"should fail\")\n"
            "function test_x():\n"
            "    expect(1).to_eq(1)\n"
        ),
        std::runtime_error
    );
}

// @it on a function with params (without @each/@property) should error
TEST_F(DirectiveTest, ItDirectiveRejectsParamsWithoutEachOrProperty) {
    EXPECT_THROW(
        []() {
            Lexer lex(
                "@it(\"bad\")\n"
                "function test_bad(x: int):\n"
                "    expect(x).to_eq(1)\n"
            );
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @describe on a named function wraps nested @it functions in a describe group
TEST_F(DirectiveTest, DescribeDirectiveBasicCodegen) {
    EXPECT_EQ(runTestSource(
        "@describe(\"math\")\n"
        "function math_tests():\n"
        "    @it(\"subtraction\")\n"
        "    function test_sub():\n"
        "        expect(10 - 3).to_eq(7)\n"
        "\n"
        "    @it(\"multiplication\")\n"
        "    function test_mul():\n"
        "        expect(4 * 5).to_eq(20)\n"
    ), "math\n  \033[32m+ subtraction\033[0m\n  \033[32m+ multiplication\033[0m\n\n2 passed, 0 failed\n");
}

// @each + @it on a named function: parameterized tests
TEST_F(DirectiveTest, ItDirectiveWithEach) {
    EXPECT_EQ(runTestSource(
        "@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])\n"
        "@it(\"adds {0} + {1} = {2}\")\n"
        "function test_add(a: int, b: int, expected: int):\n"
        "    expect(a + b).to_eq(expected)\n"
    ), "\033[32m+ adds 1 + 2 = 3\033[0m\n"
       "\033[32m+ adds 0 + 0 = 0\033[0m\n"
       "\033[32m+ adds -1 + 1 = 0\033[0m\n"
       "\n3 passed, 0 failed\n");
}

// @property + @it on a named function: property-based tests
TEST_F(DirectiveTest, ItDirectiveWithProperty) {
    std::string out = runTestSource(
        "@property(count=10)\n"
        "@it(\"addition is commutative\")\n"
        "function test_commutative(a: int, b: int):\n"
        "    expect(a + b).to_eq(b + a)\n"
    );
    EXPECT_NE(out.find("+ addition is commutative"), std::string::npos);
    EXPECT_NE(out.find("1 passed, 0 failed"), std::string::npos);
}

// @it on an async function should error
TEST_F(DirectiveTest, ItDirectiveRejectsAsyncFunction) {
    EXPECT_THROW(
        []() {
            Lexer lex(
                "@it(\"bad\")\n"
                "async function test_async():\n"
                "    expect(1).to_eq(1)\n"
            );
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @describe on a function with parameters should error
TEST_F(DirectiveTest, DescribeDirectiveRejectsParams) {
    EXPECT_THROW(
        []() {
            Lexer lex(
                "@describe(\"group\")\n"
                "function grp(x: int):\n"
                "    @it(\"sub\")\n"
                "    function t():\n"
                "        expect(x).to_eq(1)\n"
            );
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @describe nested inside @describe: inner @it functions run under the outer group header
TEST_F(DirectiveTest, DescribeDirectiveNestedDescribe) {
    EXPECT_EQ(runTestSource(
        "@describe(\"outer\")\n"
        "function outer_tests():\n"
        "    @it(\"direct child\")\n"
        "    function test_direct():\n"
        "        expect(1 + 1).to_eq(2)\n"
        "\n"
        "    @describe(\"inner\")\n"
        "    function inner_tests():\n"
        "        @it(\"nested child\")\n"
        "        function test_nested():\n"
        "            expect(2 * 3).to_eq(6)\n"
    ), "outer\n"
       "  \033[32m+ direct child\033[0m\n"
       "  inner\n"
       "    \033[32m+ nested child\033[0m\n"
       "\n2 passed, 0 failed\n");
}

// Positional argument that is a function call expression.
// Verifies @custom(make_inputs()) is parsed as a CallExpr, not a bare VariableExpr.
TEST(DirectiveSyntax, CompoundExprCallAsPositionalArg) {
    std::string src =
        "@custom(make_inputs())\n"
        "function foo():\n"
        "    expect(1).to_eq(1)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    EXPECT_EQ(d.name, "custom");
    ASSERT_EQ(d.args.size(), 1u);
    EXPECT_FALSE(d.args[0].name.has_value());
    auto *call = std::get_if<std::unique_ptr<CallExpr>>(&d.args[0].value->data);
    ASSERT_NE(call, nullptr);
}

// Positional argument that is a binary expression.
// Verifies @custom(x + 1) is parsed as a BinaryExpr, not a bare VariableExpr.
TEST(DirectiveSyntax, CompoundExprBinaryAsPositionalArg) {
    std::string src =
        "@custom(x + 1)\n"
        "function foo():\n"
        "    expect(1).to_eq(1)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    ASSERT_EQ(d.args.size(), 1u);
    EXPECT_FALSE(d.args[0].name.has_value());
    ASSERT_NE(std::get_if<std::unique_ptr<BinaryExpr>>(&d.args[0].value->data), nullptr);
}

// Named argument whose value is a binary expression.
// Verifies @custom(count=2 + 3) is parsed as named arg "count" with a BinaryExpr value.
TEST(DirectiveSyntax, NamedArgWithCompoundValue) {
    std::string src =
        "@custom(count=2 + 3)\n"
        "function foo():\n"
        "    expect(1).to_eq(1)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    ASSERT_EQ(d.args.size(), 1u);
    ASSERT_TRUE(d.args[0].name.has_value());
    EXPECT_EQ(*d.args[0].name, "count");
    ASSERT_NE(std::get_if<std::unique_ptr<BinaryExpr>>(&d.args[0].value->data), nullptr);
}

// Named argument whose value is a function call expression.
// Verifies @custom(data=make_data()) is parsed as named arg "data" with a CallExpr value.
TEST(DirectiveSyntax, NamedArgWithCallValue) {
    std::string src =
        "@custom(data=make_data())\n"
        "function foo():\n"
        "    expect(1).to_eq(1)\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    auto &d = (*fn)->directives[0];
    ASSERT_EQ(d.args.size(), 1u);
    ASSERT_TRUE(d.args[0].name.has_value());
    EXPECT_EQ(*d.args[0].name, "data");
    auto *call = std::get_if<std::unique_ptr<CallExpr>>(&d.args[0].value->data);
    ASSERT_NE(call, nullptr);
}

// @describe with shared setup: variables declared in the describe body are captured by inner @it
TEST_F(DirectiveTest, DescribeDirectiveSharedSetup) {
    EXPECT_EQ(runTestSource(
        "@describe(\"shared setup\")\n"
        "function shared_tests():\n"
        "    x = 10\n"
        "    y = 20\n"
        "    @it(\"uses x\")\n"
        "    function test_x():\n"
        "        expect(x).to_eq(10)\n"
        "    @it(\"uses x and y\")\n"
        "    function test_xy():\n"
        "        expect(x + y).to_eq(30)\n"
    ), "shared setup\n"
       "  \033[32m+ uses x\033[0m\n"
       "  \033[32m+ uses x and y\033[0m\n"
       "\n2 passed, 0 failed\n");
}

// @describe three levels deep: indentation tracks nesting depth
TEST_F(DirectiveTest, DescribeDirectiveThreeLevelNesting) {
    EXPECT_EQ(runTestSource(
        "@describe(\"level 1\")\n"
        "function l1():\n"
        "    @describe(\"level 2\")\n"
        "    function l2():\n"
        "        @describe(\"level 3\")\n"
        "        function l3():\n"
        "            @it(\"deep test\")\n"
        "            function test_deep():\n"
        "                expect(true).to_be_true()\n"
    ), "level 1\n"
       "  level 2\n"
       "    level 3\n"
       "      \033[32m+ deep test\033[0m\n"
       "\n1 passed, 0 failed\n");
}

// @each + @it inside @describe: parameterized tests inside a group
TEST_F(DirectiveTest, DescribeDirectiveWithEach) {
    EXPECT_EQ(runTestSource(
        "@describe(\"parameterized\")\n"
        "function param_tests():\n"
        "    @each([(1, 2, 3), (4, 5, 9)])\n"
        "    @it(\"{0} + {1} = {2}\")\n"
        "    function test_add(a: int, b: int, expected: int):\n"
        "        expect(a + b).to_eq(expected)\n"
    ), "parameterized\n"
       "  \033[32m+ 1 + 2 = 3\033[0m\n"
       "  \033[32m+ 4 + 5 = 9\033[0m\n"
       "\n2 passed, 0 failed\n");
}

// @property + @it inside @describe: property tests inside a group
TEST_F(DirectiveTest, DescribeDirectiveWithProperty) {
    std::string out = runTestSource(
        "@describe(\"property group\")\n"
        "function prop_tests():\n"
        "    @property(count=5)\n"
        "    @it(\"int identity\")\n"
        "    function test_id(a: int):\n"
        "        expect(a).to_eq(a)\n"
    );
    EXPECT_NE(out.find("property group"), std::string::npos);
    EXPECT_NE(out.find("+ int identity"), std::string::npos);
    EXPECT_NE(out.find("1 passed, 0 failed"), std::string::npos);
}

// Old describe() lambda syntax emits a deprecation warning
TEST_F(DirectiveTest, DescribeCallEmitsDeprecationWarning) {
    auto [output, warnings] = runTestSourceWithWarnings(
        "describe(\"old style\", ():\n"
        "    it(\"test\", ():\n"
        "        expect(1).to_eq(1)\n"
        "    )\n"
        ")\n"
    );
    bool found = false;
    for (const auto &w : warnings)
        if (w.find("describe") != std::string::npos && w.find("deprecated") != std::string::npos)
            found = true;
    EXPECT_TRUE(found) << "Expected deprecation warning for describe() call syntax";
}

// Old it() lambda syntax emits a deprecation warning
TEST_F(DirectiveTest, ItCallEmitsDeprecationWarning) {
    auto [output, warnings] = runTestSourceWithWarnings(
        "describe(\"g\", ():\n"
        "    it(\"old it\", ():\n"
        "        expect(1).to_eq(1)\n"
        "    )\n"
        ")\n"
    );
    bool found = false;
    for (const auto &w : warnings)
        if (w.find("it") != std::string::npos && w.find("deprecated") != std::string::npos)
            found = true;
    EXPECT_TRUE(found) << "Expected deprecation warning for it() call syntax";
}

// Unknown directive name: parser now accepts any name (validation deferred to codegen).
// This test verifies the AST is correctly populated; codegen will reject at emit time.
TEST(DirectiveSyntax, UnknownDirectiveParseSucceeds) {
    std::string src =
        "@unknown_directive\n"
        "function foo() -> int:\n"
        "    return 1\n";
    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    // Parse succeeds (validation deferred to codegen)
    ASSERT_FALSE(prog.empty());
    auto *fn = std::get_if<std::unique_ptr<FnStmt>>(&prog[0]);
    ASSERT_NE(fn, nullptr);
    ASSERT_EQ((*fn)->directives.size(), 1u);
    ASSERT_EQ((*fn)->directives[0].name, "unknown_directive");
}
