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

// Backward-compatible aliases for the shared helper in test_codegen_common.hpp.
// The shared helper covers @it / @describe (formerly testing-only) plus the
// six directives migrated from the built-in registry in #1390.
static std::string withItDescribeDecls(const char *src) {
    return withStdlibDirectiveDecls(src);
}

static std::string withCoreAndTestingDirectiveDecls(const char *src) {
    return withStdlibDirectiveDecls(src);
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
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("filesystem", "listDir"), "__ry_filesystem_listDir");
    EXPECT_EQ(CodeGen::deriveRuntimeFnName("path", "isAbsolute"), "__ry_path_isAbsolute");
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
        "fn print(value: str) -> Unit\n"
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n";

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
        "fn range(n: int) -> List<int>\n"
        "@native\n"
        "fn range(start: int, endVal: int) -> List<int>\n";

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
    std::string src = withCoreAndTestingDirectiveDecls(
        "@native\n"
        "@deprecated\n"
        "fn oldFn(x: int) -> int\n");

    Lexer lex(src);
    Parser parser(lex);
    Program prog = parser.parseProgram();

    CodeGen cg;
    cg.compile(prog);

    auto &sigs = cg.getNativeFnSigs();
    ASSERT_TRUE(sigs.count("oldFn"));
    auto &directives = sigs.at("oldFn")[0].directiveNames;
    EXPECT_NE(std::find(directives.begin(), directives.end(), "native"),
              directives.end());
    EXPECT_NE(std::find(directives.begin(), directives.end(), "deprecated"),
              directives.end());
}

TEST(NativeFnSigs, LibraryFieldParsed) {
    // @native("base64") parses correctly at AST level
    std::string src =
        "@native(\"base64\")\n"
        "fn encode(data: str) -> str\n";

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
        "fn encode(data: str) -> str\n";

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
        "fn contains(s: str, sub: str) -> bool\n";

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
        "fn encode(data: str) -> str\n"
        "@native(\"path\")\n"
        "fn basename(p: str) -> str\n"
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
        "fn encode(data: str) -> str\n"
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
        "fn contains(s: str, sub: str) -> bool\n";

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
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "fn oldFunc() -> int:\n"
        "    return 42\n"
        "print(oldFunc())\n"
    ));
    EXPECT_EQ(output, "42\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'oldFunc' is deprecated");
}

// 2. @deprecated type constructed -> warning
TEST_F(DirectiveTest, DeprecatedTypeWarning) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "record OldPoint:\n"
        "    x: int\n"
        "    y: int\n"
        "p = OldPoint(1, 2)\n"
        "print(p.x)\n"
    ));
    EXPECT_EQ(output, "1\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'OldPoint' is deprecated");
}

// 3. @deprecated let referenced -> warning
TEST_F(DirectiveTest, DeprecatedVariableWarning) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "old_val = 99\n"
        "print(old_val)\n"
    ));
    EXPECT_EQ(output, "99\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'old_val' is deprecated");
}

// 4. @deprecated field accessed -> warning
TEST_F(DirectiveTest, DeprecatedFieldWarning) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "record MyType:\n"
        "    @deprecated\n"
        "    oldField: int\n"
        "    newField: int\n"
        "m = MyType(1, 2)\n"
        "print(m.oldField)\n"
        "print(m.newField)\n"
    ));
    EXPECT_EQ(output, "1\n2\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'MyType.oldField' is deprecated");
}

// 5. Definition alone does not produce warnings
TEST_F(DirectiveTest, DeprecatedNoWarningOnDefinition) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "fn unusedFunc() -> int:\n"
        "    return 1\n"
        "@deprecated\n"
        "unused_val = 42\n"
        "print(0)\n"
    ));
    EXPECT_EQ(output, "0\n");
    EXPECT_TRUE(warnings.empty());
}

// 6. Non-deprecated entities produce no warnings
TEST_F(DirectiveTest, NonDeprecatedNoWarning) {
    auto [output, warnings] = runSourceWithWarnings(
        "fn goodFunc() -> int:\n"
        "    return 10\n"
        "good_val = 20\n"
        "print(goodFunc())\n"
        "print(good_val)\n"
    );
    EXPECT_EQ(output, "10\n20\n");
    EXPECT_TRUE(warnings.empty());
}

// 7. Deprecated function still works correctly
TEST_F(DirectiveTest, DeprecatedFunctionStillWorks) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(add(3, 4))\n"
    ));
    EXPECT_EQ(output, "7\n");
    ASSERT_EQ(warnings.size(), 1);
}

// 8. Multiple directives stacked (parse check)
TEST_F(DirectiveTest, MultipleDirectives) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated\n"
        "@deprecated\n"
        "fn multi() -> int:\n"
        "    return 1\n"
        "print(multi())\n"
    ));
    EXPECT_EQ(output, "1\n");
    EXPECT_FALSE(warnings.empty());
}

// 9. Directive with params parses correctly
TEST_F(DirectiveTest, DirectiveWithParams) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@deprecated(reason=\"use new_func instead\")\n"
        "fn oldApi() -> int:\n"
        "    return 0\n"
        "print(oldApi())\n"
    ));
    EXPECT_EQ(output, "0\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'oldApi' is deprecated");
}

// 10. Unknown directive causes parse error
TEST_F(DirectiveTest, UnknownDirectiveError) {
    EXPECT_THROW({
        runSource("@unknown\nfn foo() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 10b. Defaulted parameters of stdlib-declared user directives accept
// positional arguments (#1402).
TEST_F(DirectiveTest, StdlibDirectiveDefaultedParamAcceptsPositionalArg) {
    EXPECT_NO_THROW({
        runSource(withCoreAndTestingDirectiveDecls(
            "@inline(\"always\")\nfn add(a: int, b: int) -> int:\n    return a + b\n"));
    });
    EXPECT_NO_THROW({
        runSource(withCoreAndTestingDirectiveDecls(
            "@deprecated(\"old\")\nfn foo() -> int:\n    return 1\n"));
    });
}

// 10c. @native("") with empty string causes parse error
TEST_F(DirectiveTest, NativeEmptyLibraryNameError) {
    EXPECT_THROW({
        runSource("@native(\"\")\nfn foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10d. @native("a", "b") or @native("a", key=val) — extra arguments rejected
TEST_F(DirectiveTest, NativeLibraryExtraArgsError) {
    EXPECT_THROW({
        runSource("@native(\"a\", \"b\")\nfn foo(x: int) -> int\n");
    }, std::runtime_error);
    EXPECT_THROW({
        runSource("@native(\"a\", key=val)\nfn foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10e-1. @native(123) — non-string positional argument causes validation error
TEST_F(DirectiveTest, NativeNonStringArgError) {
    EXPECT_THROW({
        runSource("@native(123)\nfn foo(x: int) -> int\n");
    }, std::runtime_error);
}

// 10e. @native(key=value) — key-value params not allowed for @native
TEST_F(DirectiveTest, NativeKeyValueParamsError) {
    EXPECT_THROW({
        runSource("@native(lib=base64)\nfn foo(x: int) -> int\n");
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

// 10j. @deprecated("old") on a record is now accepted (positional form for
// defaulted parameter — see #1402).
TEST_F(DirectiveTest, DeprecatedPositionalArgOnRecordAccepted) {
    EXPECT_NO_THROW({
        runSource(withCoreAndTestingDirectiveDecls(
            "@deprecated(\"old\")\nrecord Foo:\n    x: int\n"));
    });
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
        runSource(withCoreAndTestingDirectiveDecls(
            "@deprecated\nif true\n    print(1)\n"));
    }, std::runtime_error);
}

// ===== @native fn tests =====

// 12. @native fn declaration - builtin function still works
TEST_F(DirectiveTest, NativeFnDeclaration) {
    std::string output = runSource(
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello world\", \"world\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

// 13. @native fn operator declaration - builtin operator still works
TEST_F(DirectiveTest, NativeFnOperatorDeclaration) {
    std::string output = runSource(
        "@native\n"
        "fn operator+(a: str, b: str) -> str\n"
        "print(\"hello\" + \" world\")\n"
    );
    EXPECT_EQ(output, "hello world\n");
}

// 14. @native fn with body causes error
TEST_F(DirectiveTest, NativeFnWithBodyError) {
    EXPECT_THROW({
        runSource("@native\nfn bad() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// 15. @native fn with UFCS-style builtin
TEST_F(DirectiveTest, NativeFnUfcsBuiltin) {
    std::string output = runSource(
        "@native\n"
        "fn toUpper(s: str) -> str\n"
        "print(toUpper(\"hello\"))\n"
    );
    EXPECT_EQ(output, "HELLO\n");
}

// 16. Multiple @native fn declarations coexist
TEST_F(DirectiveTest, MultipleNativeFnDeclarations) {
    std::string output = runSource(
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "@native\n"
        "fn toUpper(s: str) -> str\n"
        "print(contains(\"hello\", \"ell\"))\n"
        "print(toUpper(\"world\"))\n"
    );
    EXPECT_EQ(output, "true\nWORLD\n");
}

// ===== @native fn type signature validation =====

TEST_F(DirectiveTest, NativeFnTypeCheckPass) {
    std::string output = runSource(
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\", \"ell\"))\n"
    );
    EXPECT_EQ(output, "true\n");
}

TEST_F(DirectiveTest, NativeFnTypeCheckFailArgCount) {
    EXPECT_THROW(runSource(
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\"))\n"
    ), std::runtime_error);
}

TEST_F(DirectiveTest, NativeFnOverloadResolution) {
    std::string output = runSource(
        "@native\n"
        "fn range(n: int) -> List<int>\n"
        "@native\n"
        "fn range(start: int, endVal: int) -> List<int>\n"
        "@native\n"
        "fn range(start: int, endVal: int, step: int) -> List<int>\n"
        "print(len(range(5)))\n"
        "print(len(range(1, 4)))\n"
        "print(len(range(0, 10, 2)))\n"
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
        "fn toUpper(s: str) -> str\n"
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "@native\n"
        "fn startsWith(s: str, prefix: str) -> bool\n"
        "print(toUpper(\"hello\"))\n"
        "print(contains(\"hello world\", \"world\"))\n"
        "print(startsWith(\"hello\", \"hel\"))\n"
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
        "fn encode(data: str) -> str\n"
        "print(encode(\"test\"))\n"
    );
    EXPECT_EQ(output, "dGVzdA==\n");
}

TEST_F(DirectiveTest, NativeFnLibraryDeclarationOnly) {
    // @native("libname") declaration is accepted and registered for dispatch.
    // Without calling the function, the declaration alone is valid.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "fn encode(data: str) -> str\n"
        "print(\"ok\")\n"
    );
    EXPECT_EQ(output, "ok\n");
}

TEST_F(DirectiveTest, NativeFnLibraryDoesNotShadowBuiltin) {
    // @native("libname") registers for dispatch but does not shadow
    // built-in functions with the same name.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "fn contains(s: str, sub: str) -> bool\n"
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
        "fn greet(name: str) -> str\n"
        "fn greet(x: int) -> str:\n"
        "    return \"int:\" + toStr(x)\n"
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
        "fn greet(name: str) -> str\n"
        "print(greet(\"world\"))\n"
    );
    EXPECT_EQ(output, "hello world\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchDirect) {
    // @native("base64") fn encode(data: str) -> str — the sig key
    // "base64::encode" is found by the table-driven base64 dispatcher.
    std::string output = runSource(
        "@native(\"base64\")\n"
        "fn encode(data: str) -> str\n"
        "print(encode(\"Hello\"))\n"
    );
    EXPECT_EQ(output, "SGVsbG8=\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchResultPtr) {
    // @native("base64") fn decode(data: str) -> Result<str, Error>
    // Tests ResultPtr wrapping in the generic dispatch path.
    std::string output = runSource(
        "@native(\"base64\")\nfn decode(data: str) -> Result<str, Error>\ncase decode(\"SGVsbG8=\"):\n    Ok(s):\n        print(s)\n    Err(e):\n        print(e.message)\n"
    );
    EXPECT_EQ(output, "Hello\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchBool) {
    // Tests BoolFromI64 wrapping in the generic dispatch path.
    std::string output = runSource(
        "@native(\"path\")\n"
        "fn isAbsolute(p: str) -> bool\n"
        "print(isAbsolute(\"/usr/bin\"))\n"
        "print(isAbsolute(\"relative\"))\n"
    );
    EXPECT_EQ(output, "true\nfalse\n");
}

TEST_F(DirectiveTest, NativeFnLibraryGenericDispatchResultBool) {
    // Result<bool, Error> via the generic dispatch path with a real
    // runtime symbol (__ry_testlib_check). Exercises the full ResultOutParam
    // ABI: i64 status, i64 out-param, and the final i64→i1 truncation.
    std::string output = runSource(
        "@native(\"testlib\")\nfn check(s: str) -> Result<bool, Error>\ncase check(\"yes\"):\n    Ok(b):\n        print(b)\n    Err(e):\n        print(e.message)\n"
    );
    EXPECT_EQ(output, "true\n");
}

// ===== @inline tests =====

TEST_F(DirectiveTest, InlineDefault) {
    std::string output = runSource(withCoreAndTestingDirectiveDecls(
        "@inline\n"
        "fn add(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(add(3, 4))\n"
    ));
    EXPECT_EQ(output, "7\n");
}

TEST_F(DirectiveTest, InlineModeAlways) {
    std::string output = runSource(withCoreAndTestingDirectiveDecls(
        "@inline(mode=\"always\")\n"
        "fn mul(a: int, b: int) -> int:\n"
        "    return a * b\n"
        "print(mul(5, 6))\n"
    ));
    EXPECT_EQ(output, "30\n");
}

TEST_F(DirectiveTest, InlineModeHint) {
    std::string output = runSource(withCoreAndTestingDirectiveDecls(
        "@inline(mode=\"hint\")\n"
        "fn sub(a: int, b: int) -> int:\n"
        "    return a - b\n"
        "print(sub(10, 3))\n"
    ));
    EXPECT_EQ(output, "7\n");
}

TEST_F(DirectiveTest, InlineModeNever) {
    std::string output = runSource(withCoreAndTestingDirectiveDecls(
        "@inline(mode=\"never\")\n"
        "fn negate(a: int) -> int:\n"
        "    return -a\n"
        "print(negate(-5))\n"
    ));
    EXPECT_EQ(output, "5\n");
}

TEST_F(DirectiveTest, InlineInvalidMode) {
    EXPECT_THROW(runSource(withCoreAndTestingDirectiveDecls(
        "@inline(mode=\"aggressive\")\n"
        "fn bad() -> int:\n"
        "    return 1\n"
        "print(bad())\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, InlineWithNativeError) {
    EXPECT_THROW(runSource(withCoreAndTestingDirectiveDecls(
        "@inline\n"
        "@native\n"
        "fn contains(s: str, sub: str) -> bool\n"
        "print(contains(\"hello\", \"ell\"))\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, InlineWithDeprecated) {
    auto [output, warnings] = runSourceWithWarnings(withCoreAndTestingDirectiveDecls(
        "@inline\n"
        "@deprecated\n"
        "fn oldAdd(a: int, b: int) -> int:\n"
        "    return a + b\n"
        "print(oldAdd(1, 2))\n"
    ));
    EXPECT_EQ(output, "3\n");
    ASSERT_EQ(warnings.size(), 1);
    EXPECT_EQ(warnings[0], "warning: 'oldAdd' is deprecated");
}

TEST_F(DirectiveTest, InlineRecursive) {
    std::string output = runSource(withCoreAndTestingDirectiveDecls(
        "@inline\n"
        "fn fact(n: int) -> int:\n"
        "    if n <= 1:\n"
        "        return 1\n"
        "    return n * fact(n - 1)\n"
        "print(fact(5))\n"
    ));
    EXPECT_EQ(output, "120\n");
}

// ===== Generalized directive invocation syntax (#663) =====

// @it("description") is parseable on a function declaration (AST check)
TEST(DirectiveSyntax, ItDirectiveParsedOnFunction) {
    std::string src =
        "@it(\"should add integers\")\n"
        "fn testAdd():\n"
        "    expect(1 + 2).toEq(3)\n";
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
        "fn calculatorTests():\n"
        "    expect(1 + 1).toEq(2)\n";
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
        "fn testCommute(a: int, b: int):\n"
        "    expect(a + b).toEq(b + a)\n";
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
        "fn foo():\n"
        "    expect(1).toEq(1)\n";
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
    EXPECT_EQ(runTestSource(withItDescribeDecls(
        "@it(\"should add 1 + 2 = 3\")\n"
        "fn testAdd():\n"
        "    expect(1 + 2).toEq(3)\n"
    )), "\033[32m+ should add 1 + 2 = 3\033[0m\n\n1 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @it requires test mode — should error outside test mode
TEST_F(DirectiveTest, ItDirectiveRequiresTestMode) {
    EXPECT_THROW(
        compileSource(withItDescribeDecls(
            "@it(\"should fail\")\n"
            "fn testX():\n"
            "    expect(1).toEq(1)\n"
        )),
        std::runtime_error
    );
}

// @it on a function with params (without @each/@property) should error
TEST_F(DirectiveTest, ItDirectiveRejectsParamsWithoutEachOrProperty) {
    EXPECT_THROW(
        []() {
            Lexer lex(withItDescribeDecls(
                "@it(\"bad\")\n"
                "fn testBad(x: int):\n"
                "    expect(x).toEq(1)\n"
            ));
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
    EXPECT_EQ(runTestSource(withItDescribeDecls(
        "@describe(\"math\")\n"
        "fn mathTests():\n"
        "    @it(\"should subtract\")\n"
        "    fn testSub():\n"
        "        expect(10 - 3).toEq(7)\n"
        "\n"
        "    @it(\"should multiply\")\n"
        "    fn testMul():\n"
        "        expect(4 * 5).toEq(20)\n"
    )), "math\n  \033[32m+ should subtract\033[0m\n  \033[32m+ should multiply\033[0m\n\n2 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @each + @it on a named function: parameterized tests
TEST_F(DirectiveTest, ItDirectiveWithEach) {
    EXPECT_EQ(runTestSource(withCoreAndTestingDirectiveDecls(
        "@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])\n"
        "@it(\"should add {0} + {1} = {2}\")\n"
        "fn testAdd(a: int, b: int, expected: int):\n"
        "    expect(a + b).toEq(expected)\n"
    )), "\033[32m+ should add 1 + 2 = 3\033[0m\n"
       "\033[32m+ should add 0 + 0 = 0\033[0m\n"
       "\033[32m+ should add -1 + 1 = 0\033[0m\n"
       "\n3 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @property + @it on a named function: property-based tests
TEST_F(DirectiveTest, ItDirectiveWithProperty) {
    std::string out = runTestSource(withCoreAndTestingDirectiveDecls(
        "@property(count=10)\n"
        "@it(\"should verify addition is commutative\")\n"
        "fn testCommutative(a: int, b: int):\n"
        "    expect(a + b).toEq(b + a)\n"
    ));
    EXPECT_NE(out.find("+ should verify addition is commutative"), std::string::npos);
    EXPECT_NE(out.find("1 passed, 0 failed"), std::string::npos);
}

// @it on an async fn should error
TEST_F(DirectiveTest, ItDirectiveRejectsAsyncFunction) {
    EXPECT_THROW(
        []() {
            Lexer lex(withItDescribeDecls(
                "@it(\"bad\")\n"
                "async fn testAsync():\n"
                "    expect(1).toEq(1)\n"
            ));
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
            Lexer lex(withItDescribeDecls(
                "@describe(\"group\")\n"
                "fn grp(x: int):\n"
                "    @it(\"sub\")\n"
                "    fn t():\n"
                "        expect(x).toEq(1)\n"
            ));
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @it on a function with a return type annotation should error.
// Uses -> Unit so the body naturally returns Unit — the only throw is from @it enforcement,
// not from the secondary "does not return a value on all code paths" check.
TEST_F(DirectiveTest, ItDirectiveRejectsReturnTypeAnnotation) {
    EXPECT_THROW(
        []() {
            Lexer lex(withItDescribeDecls(
                "@it(\"bad\")\n"
                "fn testWithRet() -> Unit:\n"
                "    expect(1).toEq(1)\n"
            ));
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @each @it on a function with a return type annotation should error.
TEST_F(DirectiveTest, ItDirectiveRejectsReturnTypeOnEach) {
    EXPECT_THROW(
        []() {
            Lexer lex(withCoreAndTestingDirectiveDecls(
                "@each([(1, 2)])\n"
                "@it(\"bad each {0} {1}\")\n"
                "fn testEach(a: int, b: int) -> Unit:\n"
                "    expect(a).toEq(b)\n"
            ));
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @property @it on a function with a return type annotation should error.
TEST_F(DirectiveTest, ItDirectiveRejectsReturnTypeOnProperty) {
    EXPECT_THROW(
        []() {
            Lexer lex(withCoreAndTestingDirectiveDecls(
                "@property(count=10)\n"
                "@it(\"bad property\")\n"
                "fn testProp(a: int) -> Unit:\n"
                "    expect(a).toEq(a)\n"
            ));
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// @describe on a function with a return type annotation should error.
TEST_F(DirectiveTest, DescribeDirectiveRejectsReturnTypeAnnotation) {
    EXPECT_THROW(
        []() {
            Lexer lex(withItDescribeDecls(
                "@describe(\"group\")\n"
                "fn grp() -> Unit:\n"
                "    @it(\"sub\")\n"
                "    fn t():\n"
                "        expect(1).toEq(1)\n"
            ));
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
    EXPECT_EQ(runTestSource(withItDescribeDecls(
        "@describe(\"outer\")\n"
        "fn outerTests():\n"
        "    @it(\"should pass as direct child\")\n"
        "    fn testDirect():\n"
        "        expect(1 + 1).toEq(2)\n"
        "\n"
        "    @describe(\"inner\")\n"
        "    fn innerTests():\n"
        "        @it(\"should pass as nested child\")\n"
        "        fn testNested():\n"
        "            expect(2 * 3).toEq(6)\n"
    )), "outer\n"
       "  \033[32m+ should pass as direct child\033[0m\n"
       "  inner\n"
       "    \033[32m+ should pass as nested child\033[0m\n"
       "\n2 passed, 0 failed, 0 skipped, 0 todo\n");
}

// Positional argument that is a function call expression.
// Verifies @custom(make_inputs()) is parsed as a CallExpr, not a bare VariableExpr.
TEST(DirectiveSyntax, CompoundExprCallAsPositionalArg) {
    std::string src =
        "@custom(make_inputs())\n"
        "fn foo():\n"
        "    expect(1).toEq(1)\n";
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
        "fn foo():\n"
        "    expect(1).toEq(1)\n";
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
        "fn foo():\n"
        "    expect(1).toEq(1)\n";
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
        "fn foo():\n"
        "    expect(1).toEq(1)\n";
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
    EXPECT_EQ(runTestSource(withItDescribeDecls(
        "@describe(\"shared setup\")\n"
        "fn sharedTests():\n"
        "    x = 10\n"
        "    y = 20\n"
        "    @it(\"should use x\")\n"
        "    fn testX():\n"
        "        expect(x).toEq(10)\n"
        "    @it(\"should use x and y\")\n"
        "    fn testXy():\n"
        "        expect(x + y).toEq(30)\n"
    )), "shared setup\n"
       "  \033[32m+ should use x\033[0m\n"
       "  \033[32m+ should use x and y\033[0m\n"
       "\n2 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @describe three levels deep: indentation tracks nesting depth
TEST_F(DirectiveTest, DescribeDirectiveThreeLevelNesting) {
    EXPECT_EQ(runTestSource(withItDescribeDecls(
        "@describe(\"level 1\")\n"
        "fn l1():\n"
        "    @describe(\"level 2\")\n"
        "    fn l2():\n"
        "        @describe(\"level 3\")\n"
        "        fn l3():\n"
        "            @it(\"should pass at deep nesting\")\n"
        "            fn testDeep():\n"
        "                expect(true).toBeTrue()\n"
    )), "level 1\n"
       "  level 2\n"
       "    level 3\n"
       "      \033[32m+ should pass at deep nesting\033[0m\n"
       "\n1 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @each + @it inside @describe: parameterized tests inside a group
TEST_F(DirectiveTest, DescribeDirectiveWithEach) {
    EXPECT_EQ(runTestSource(withCoreAndTestingDirectiveDecls(
        "@describe(\"parameterized\")\n"
        "fn paramTests():\n"
        "    @each([(1, 2, 3), (4, 5, 9)])\n"
        "    @it(\"should add {0} + {1} = {2}\")\n"
        "    fn testAdd(a: int, b: int, expected: int):\n"
        "        expect(a + b).toEq(expected)\n"
    )), "parameterized\n"
       "  \033[32m+ should add 1 + 2 = 3\033[0m\n"
       "  \033[32m+ should add 4 + 5 = 9\033[0m\n"
       "\n2 passed, 0 failed, 0 skipped, 0 todo\n");
}

// @property + @it inside @describe: property tests inside a group
TEST_F(DirectiveTest, DescribeDirectiveWithProperty) {
    std::string out = runTestSource(withCoreAndTestingDirectiveDecls(
        "@describe(\"property group\")\n"
        "fn propTests():\n"
        "    @property(count=5)\n"
        "    @it(\"should hold int identity\")\n"
        "    fn testId(a: int):\n"
        "        expect(a).toEq(a)\n"
    ));
    EXPECT_NE(out.find("property group"), std::string::npos);
    EXPECT_NE(out.find("+ should hold int identity"), std::string::npos);
    EXPECT_NE(out.find("1 passed, 0 failed"), std::string::npos);
}

// Unknown directive name: parser now accepts any name (validation deferred to codegen).
// This test verifies the AST is correctly populated; codegen will reject at emit time.
TEST(DirectiveSyntax, UnknownDirectiveParseSucceeds) {
    std::string src =
        "@unknown_directive\n"
        "fn foo() -> int:\n"
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

// ===== @directive definition syntax (#708) — codegen smoke tests =====

// Codegen accepts a bare @directive definition (no IR is emitted; the stmt is a no-op).
TEST_F(DirectiveTest, DirectiveDefCodegenSmokeBareDefinition) {
    EXPECT_NO_THROW({
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn d(description: str)\n"
        );
    });
}

// Codegen + JIT execution: @directive definition followed by ordinary top-level
// statements still produces a runnable __ry_main__ that prints the expected output.
TEST_F(DirectiveTest, DirectiveDefCodegenSmokeFollowedByMain) {
    std::string output = runSource(
        "@directive(target=[\"function\"])\n"
        "fn d(description: str)\n"
        "print(\"ok\")\n"
    );
    EXPECT_EQ(output, "ok\n");
}

// Multiple @directive definitions with distinct names are accepted.
// Collisions with built-ins and duplicate registrations are covered by
// the dedicated tests below.
TEST_F(DirectiveTest, DirectiveDefCodegenSmokeMultiple) {
    std::string output = runSource(
        "@directive(target=[\"function\"])\n"
        "fn myIt(description: str)\n"
        "@directive(target=[\"function\", \"record\"])\n"
        "fn cacheable()\n"
        "print(\"ok\")\n"
    );
    EXPECT_EQ(output, "ok\n");
}

// ===== @directive registry & validation tests =====

// A `@directive` definition registers itself in the per-program user registry
// and a subsequent application of `@<name>(...)` passes validation.
TEST_F(DirectiveTest, UserDirectiveRegistersOnDefStmt) {
    EXPECT_NO_THROW(runSource(
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str)\n"
        "@logged(\"hello\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
        "print(targetFn())\n"
    ));
}

// Defining a `@directive` whose name collides with a built-in (e.g. @native)
// is rejected, regardless of signature.
TEST_F(DirectiveTest, UserDirectiveCollisionWithBuiltinRejected) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn native()\n"
        ),
        std::runtime_error
    );
}

// Declaring the same `@directive` name twice in one program is rejected.
TEST_F(DirectiveTest, UserDirectiveDuplicateRegistrationRejected) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn dup(label: str)\n"
            "@directive(target=[\"function\"])\n"
            "fn dup(other: str)\n"
        ),
        std::runtime_error
    );
}

// Applying an unregistered directive name still yields the original
// "unknown directive" error after the 2-step lookup.
TEST_F(DirectiveTest, UserDirectiveUnknownStillRejected) {
    EXPECT_THROW(
        compileSource(
            "@undefined_directive(\"x\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Passing an unknown named argument to a user-defined directive is rejected.
TEST_F(DirectiveTest, UserDirectiveRejectsUnknownNamedArg) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(label: str = \"x\")\n"
            "@mydir(unknown_named=\"y\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// A required parameter (no default) may also be passed by name (#1397).
TEST_F(DirectiveTest, UserDirectiveRequiredParamAcceptsNamedArg) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn mydir(description: str)\n"
        "@mydir(description=\"hi\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// Required-param directives still reject truly unknown named args (#1397).
TEST_F(DirectiveTest, UserDirectiveRejectsUnknownNamedArgWithRequiredParam) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(description: str)\n"
            "@mydir(unknown=\"x\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Providing the same parameter both positionally and by name is rejected.
TEST_F(DirectiveTest, UserDirectiveRejectsRequiredParamProvidedTwice) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(description: str)\n"
            "@mydir(\"hi\", description=\"again\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Omitting a required parameter (with no positional fallback either) is rejected.
TEST_F(DirectiveTest, UserDirectiveRejectsMissingRequiredParam) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(description: str)\n"
            "@mydir()\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Passing the same named argument twice is rejected.
TEST_F(DirectiveTest, UserDirectiveRejectsDuplicateNamedArg) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(label: str = \"x\")\n"
            "@mydir(label=\"a\", label=\"b\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Required positional + optional named — common usage must remain accepted.
TEST_F(DirectiveTest, UserDirectiveAcceptsMixedPositionalAndNamed) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn mydir(description: str, level: str = \"info\")\n"
        "@mydir(\"hi\", level=\"warn\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// A defaulted parameter can be passed positionally (#1402, central spec).
TEST_F(DirectiveTest, UserDirectiveDefaultedParamAcceptsPositionalArg) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str = \"info\")\n"
        "@logged(\"warn\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// A positional argument overrides the declared default (#1402).
TEST_F(DirectiveTest, UserDirectiveDefaultedParamPositionalOverridesDefault) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str = \"info\")\n"
        "@logged(\"error\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// Omitting a defaulted parameter is still legal — sibling positive test for
// the "missing required" rejection path (Rule 2).
TEST_F(DirectiveTest, UserDirectiveOmittedDefaultedParamStillUsesDefault) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str = \"info\")\n"
        "@logged()\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// Named form for a defaulted parameter remains legal — sibling positive test
// for `UserDirectiveRejectsDuplicateNamedArg` (Rule 2).
TEST_F(DirectiveTest, UserDirectiveDefaultedParamAcceptsNamedArg) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn logged(label: str = \"info\")\n"
        "@logged(label=\"warn\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// All-positional including a defaulted trailing parameter (#1402).
TEST_F(DirectiveTest, UserDirectiveAcceptsAllPositionalIncludingDefaulted) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn mydir(description: str, level: str = \"info\")\n"
        "@mydir(\"hi\", \"warn\")\n"
        "fn targetFn() -> int:\n"
        "    return 1\n"
    ));
}

// Providing a defaulted parameter both positionally and by name is rejected.
TEST_F(DirectiveTest, UserDirectiveRejectsDefaultedParamProvidedTwice) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(description: str, level: str = \"info\")\n"
            "@mydir(\"hi\", \"warn\", level=\"error\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// Exceeding the new max_positional (now params.size() including defaulted)
// is still rejected (#1402, direct trigger for the expanded max bound).
TEST_F(DirectiveTest, UserDirectiveRejectsTooManyPositionalArgsWithDefaulted) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn logged(label: str = \"info\")\n"
            "@logged(\"a\", \"b\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// In a mixed signature, a missing required parameter still throws even when
// other parameters are defaulted — direct trigger for the per-parameter
// `defaulted_params.count(pname)` guard (Rule 1).
TEST_F(DirectiveTest, UserDirectiveRejectsMissingRequiredWhenDefaultedAlsoDeclared) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn mydir(description: str, level: str = \"info\")\n"
            "@mydir(level=\"warn\")\n"
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// After @it is migrated out of the built-in registry, source that uses @it
// without `from std/testing import it` must be rejected with an
// unknown-directive error.
TEST_F(DirectiveTest, BareItRejectedWithoutTestingImport) {
    EXPECT_THROW(
        []() {
            Lexer lex(
                "@it(\"smoke test\")\n"
                "fn myTest():\n"
                "    expect(1).toEq(1)\n"
            );
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);  // test_mode = true; @it would otherwise also fail on test-mode check
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// Symmetric to BareItRejectedWithoutTestingImport: source that uses @describe
// without `from std/testing import describe` must also be rejected with an
// unknown-directive error after the import-enforcement unification (#721).
TEST_F(DirectiveTest, BareDescribeRejectedWithoutTestingImport) {
    EXPECT_THROW(
        []() {
            Lexer lex(
                "@describe(\"my group\")\n"
                "fn myGroup():\n"
                "    pass\n"
            );
            Parser parser(lex);
            Program prog = parser.parseProgram();
            CodeGen cg(true);  // test_mode = true; @describe would otherwise also fail on test-mode check
            cg.compile(prog);
        }(),
        std::runtime_error
    );
}

// ===== #1425: silent no-op when user-defined directive applied outside target=[...] =====
//
// Issue #1425 resolves user-defined directive target-mismatch behavior to
// silent no-op: the directive's effect (future metadata/hooks) is suppressed,
// argument validation is skipped, and no diagnostic is produced. The use site
// continues compiling and executing normally.

// `@audit(target=["function"])` applied to a record is silently ignored — the
// record still compiles. This is the central example from issue #1425.
TEST_F(DirectiveTest, FunctionOnlyAppliedToRecordCompiles) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit(\"hello\")\n"
        "record User:\n"
        "    id: int\n"
    ));
}

// `target=["function"]` applied to a record field is silently ignored.
TEST_F(DirectiveTest, FunctionOnlyAppliedToFieldCompiles) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "record User:\n"
        "    @audit(\"hi\")\n"
        "    id: int\n"
    ));
}

// `target=["function"]` applied to an assignment statement is silently ignored.
TEST_F(DirectiveTest, FunctionOnlyAppliedToStatementCompiles) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit(\"hi\")\n"
        "total = 1\n"
    ));
}

// `target=["function"]` applied to a tuple-destructure statement is silently
// ignored. Covers the `TupleDestructStmt` validateDirectives call site.
TEST_F(DirectiveTest, FunctionOnlyAppliedToTupleDestructCompiles) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit(\"hi\")\n"
        "a, b = (1, 2)\n"
    ));
}

// `target=["function"]` applied to a function-call statement is silently
// ignored. Covers the `CallStmt` validateDirectives call site, made
// reachable by #1427 (parser gate now allows user-defined directives on
// function-call statements; built-ins like `@native` are still rejected).
TEST_F(DirectiveTest, CallStmtAcceptsUserDirectiveAsSilentNoOp) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit(\"hi\")\n"
        "print(1)\n"
    ));
}

// `target=["function"]` applied to a `for` statement is silently ignored.
// Covers the `ForStmt` validateDirectives call site, made reachable by
// #1427 (parser gate now allows user-defined directives on `for`
// statements; `@native` and multiple `@parallel` are still rejected).
TEST_F(DirectiveTest, ForLoopAcceptsUserDirectiveAsSilentNoOp) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit(\"hi\")\n"
        "for i in range(3):\n"
        "    print(i)\n"
    ));
}

// `target=["for"]` matches a `for` statement use site — directive passes
// through codegen validateDirectives() without being silenced.
TEST_F(DirectiveTest, ForLoopAcceptsUserDirectiveTargetMatch) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"for\"])\n"
        "fn audit(label: str = \"\")\n"
        "@audit\n"
        "for i in range(3):\n"
        "    print(i)\n"
    ));
}

// `target=["statement"]` matches a function-call statement (CallStmt is one
// of the kinds folded into Statement) — directive passes through codegen
// validateDirectives() without being silenced.
TEST_F(DirectiveTest, CallStmtAcceptsUserDirectiveTargetMatch) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"statement\"])\n"
        "fn audit(label: str = \"\")\n"
        "@audit\n"
        "print(1)\n"
    ));
}

// Multiple user-defined directives on a single `for` statement compile —
// the new gate uses registry-membership rather than `directives.size() <= 1`,
// so any number of user-defined directives is accepted. `@parallel` is not
// used here because `compileSource` does not load stdlib; the
// `parallel_count > 1` rejection is exercised separately by
// `ForLoopRejectsMultipleParallel`.
TEST_F(DirectiveTest, ForLoopAcceptsMultipleUserDirectives) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"for\"])\n"
        "fn tag1(label: str = \"\")\n"
        "@directive(target=[\"for\"])\n"
        "fn tag2(label: str = \"\")\n"
        "@tag1\n"
        "@tag2\n"
        "for i in range(3):\n"
        "    print(i)\n"
    ));
}

// Regression guard: the compiler built-in directive `@native` is still
// rejected on `for` statements at parse time after #1427 (only
// user-defined directives were relaxed; built-ins remain restricted).
TEST_F(DirectiveTest, ForLoopRejectsBuiltinNativeDirective) {
    EXPECT_THROW(
        compileSource(
            "@native\n"
            "for i in range(3):\n"
            "    print(i)\n"
        ),
        std::runtime_error
    );
}

// Regression guard: the compiler built-in directive `@native` is still
// rejected on function-call statements at parse time after #1427.
TEST_F(DirectiveTest, CallStmtRejectsBuiltinNativeDirective) {
    EXPECT_THROW(
        compileSource(
            "@native\n"
            "print(1)\n"
        ),
        std::runtime_error
    );
}

// Regression guard: applying `@parallel` more than once on a single `for`
// statement is still rejected at parse time after #1427.
TEST_F(DirectiveTest, ForLoopRejectsMultipleParallel) {
    EXPECT_THROW(
        compileSource(
            "@parallel\n"
            "@parallel\n"
            "for i in range(3):\n"
            "    print(i)\n"
        ),
        std::runtime_error
    );
}

// On a target mismatch, argument validation is also skipped — even a missing
// required argument compiles silently. The arg-validation skip is the
// observable mechanism that proves the silent-no-op guard fires before
// `validateDirectiveSignature`.
TEST_F(DirectiveTest, MismatchSilencesArgValidation) {
    EXPECT_NO_THROW(compileSource(
        "@directive(target=[\"function\"])\n"
        "fn audit(label: str)\n"
        "@audit\n"  // missing required arg, but target=record so silent
        "record User:\n"
        "    id: int\n"
    ));
}

// Regression guard: when the target matches, argument validation still fires
// — a missing required argument throws as before. Pairs with
// `MismatchSilencesArgValidation` to lock both halves of the spec.
TEST_F(DirectiveTest, TargetMatchStillValidatesArgs) {
    EXPECT_THROW(
        compileSource(
            "@directive(target=[\"function\"])\n"
            "fn audit(label: str)\n"
            "@audit\n"  // missing required arg, target matches → throws
            "fn targetFn() -> int:\n"
            "    return 1\n"
        ),
        std::runtime_error
    );
}

// ===== @public directive tests (#1545) =====
// Parser/registry-level acceptance only — visibility semantics are deferred
// to #1544. The annotation is inert: codegen passes through with no effect.

TEST_F(DirectiveTest, PublicDirectiveRegistered) {
    const auto &registry = builtinDirectiveRegistry();
    auto it = registry.find("public");
    ASSERT_NE(it, registry.end()) << "@public must be registered as a built-in directive";
    const auto &sig = it->second;
    EXPECT_EQ(sig.name, "public");
    using T = DirectiveTarget;
    EXPECT_EQ(sig.allowed_targets,
              static_cast<uint8_t>(T::Function | T::Record | T::Statement));
    EXPECT_EQ(sig.min_positional, 0);
    EXPECT_EQ(sig.max_positional, 0);
    EXPECT_TRUE(sig.positional_param_names.empty());
    EXPECT_TRUE(sig.defaulted_params.empty());
    EXPECT_EQ(sig.custom_validator, nullptr);
}

TEST_F(DirectiveTest, PublicDirectiveOnFnCodegenPass) {
    EXPECT_NO_THROW({
        runSource("@public\nfn answer() -> int:\n    return 42\n"
                  "print(answer())\n");
    });
}

TEST_F(DirectiveTest, PublicDirectiveOnLetCodegenPass) {
    EXPECT_NO_THROW({
        runSource("@public\nx = 42\nprint(x)\n");
    });
}

TEST_F(DirectiveTest, PublicDirectiveOnRecordCodegenPass) {
    EXPECT_NO_THROW({
        runSource("@public\nrecord Foo:\n    x: int\n"
                  "f = Foo(7)\nprint(f.x)\n");
    });
}

// Direct regression for the max_positional=0 rejection branch
// (per .claude/rules/tests-rejection-tdd.md).
TEST_F(DirectiveTest, PublicDirectiveRejectsArgs) {
    EXPECT_THROW({
        runSource("@public(\"foo\")\nfn bar() -> int:\n    return 1\n");
    }, std::runtime_error);
}

// ===== #1687: @skip / @only / @todo test-selection directives =====

// Positive cases that must continue to parse and compile cleanly.
TEST_F(DirectiveTest, SkipOnItCompiles) {
    EXPECT_NO_THROW(runTestSource(withStdlibDirectiveDecls(
        "@skip\n"
        "@it(\"placeholder\")\n"
        "fn t():\n"
        "    expect(1).toEq(1)\n"
    )));
}

TEST_F(DirectiveTest, OnlyOnItCompiles) {
    EXPECT_NO_THROW(runTestSource(withStdlibDirectiveDecls(
        "@only\n"
        "@it(\"focused\")\n"
        "fn t():\n"
        "    expect(1).toEq(1)\n"
    )));
}

TEST_F(DirectiveTest, TodoOnItCompiles) {
    // Body intentionally uses an undefined identifier — @todo suppresses body
    // codegen, so this must still compile (no type / lookup check).
    EXPECT_NO_THROW(runTestSource(withStdlibDirectiveDecls(
        "@todo\n"
        "@it(\"future implementation\")\n"
        "fn t():\n"
        "    expect(undefinedReference).toEq(0)\n"
    )));
}

// Mutual exclusivity: any two of @skip / @only / @todo together must reject.
TEST_F(DirectiveTest, SkipAndOnlyMutuallyExclusive) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@skip\n"
        "@only\n"
        "@it(\"conflict\")\n"
        "fn t():\n"
        "    expect(1).toEq(1)\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, SkipAndTodoMutuallyExclusive) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@skip\n"
        "@todo\n"
        "@it(\"conflict\")\n"
        "fn t():\n"
        "    expect(1).toEq(1)\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, OnlyAndTodoMutuallyExclusive) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@only\n"
        "@todo\n"
        "@it(\"conflict\")\n"
        "fn t():\n"
        "    expect(1).toEq(1)\n"
    )), std::runtime_error);
}

// @describe must reject @skip / @only / @todo. MVP supports @it only.
TEST_F(DirectiveTest, SkipOnDescribeRejected) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@skip\n"
        "@describe(\"group\")\n"
        "fn g():\n"
        "    @it(\"inner\")\n"
        "    fn inner():\n"
        "        expect(1).toEq(1)\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, OnlyOnDescribeRejected) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@only\n"
        "@describe(\"group\")\n"
        "fn g():\n"
        "    @it(\"inner\")\n"
        "    fn inner():\n"
        "        expect(1).toEq(1)\n"
    )), std::runtime_error);
}

TEST_F(DirectiveTest, TodoOnDescribeRejected) {
    EXPECT_THROW(runTestSource(withStdlibDirectiveDecls(
        "@todo\n"
        "@describe(\"group\")\n"
        "fn g():\n"
        "    @it(\"inner\")\n"
        "    fn inner():\n"
        "        expect(1).toEq(1)\n"
    )), std::runtime_error);
}
