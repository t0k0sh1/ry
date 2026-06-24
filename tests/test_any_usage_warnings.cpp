#include <gtest/gtest.h>
#include <array>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

struct RunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
};

static RunResult runRy(std::vector<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0)
        return {};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        close(pipeErr[0]);
        close(pipeErr[1]);
        return {};
    }

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);
        setenv("RY_ENV", "internal", 1);

        // Full RY_BINARY_PATH as argv[0] — `.claude/rules/tests-cpp-conventions.md`
        // notes bare "ry" breaks exe-adjacent stdlib resolution on Linux.
        std::vector<const char *> argv{RY_BINARY_PATH};
        argv.insert(argv.end(), args.begin(), args.end());
        argv.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
        _exit(127);
    }

    close(pipeOut[1]);
    close(pipeErr[1]);

    auto readAll = [](int fd) -> std::string {
        std::string result;
        std::array<char, 4096> buf{};
        ssize_t n;
        while ((n = read(fd, buf.data(), buf.size())) > 0)
            result.append(buf.data(), static_cast<size_t>(n));
        close(fd);
        return result;
    };

    RunResult r;
    std::thread outReader([&]() { r.out = readAll(pipeOut[0]); });
    std::thread errReader([&]() { r.err = readAll(pipeErr[0]); });
    outReader.join();
    errReader.join();
    int status = 0;
    waitpid(pid, &status, 0);
    r.exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return r;
}

class AnyUsageWarningsTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Build-adjacent so the package.toml ancestor chain resolves stdlib.
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_any_usage_warnings_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

// ---------------------------------------------------------------------------
// Pattern 1: x: any = <concrete RHS>
// ---------------------------------------------------------------------------

TEST_F(AnyUsageWarningsTest, Pattern1ConcreteLiteralFiresWarning) {
    auto p = writeTmp("p1_fire.ry",
        "x: any = 42\n"
        "print(x)\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: variable 'x' is annotated 'any' but its initializer has a concrete type"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("remove the annotation"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern1AnyRhsSuppressed) {
    auto p = writeTmp("p1_any_rhs.ry",
        "fn getAny() -> any:\n"
        "    return 42\n"
        "x: any = getAny()\n"
        "print(x)\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("annotated 'any' but its initializer has a concrete type"),
              std::string::npos)
        << "stderr was: " << r.err;
}

// ---------------------------------------------------------------------------
// Pattern 2: @public function exposes any in its signature
// ---------------------------------------------------------------------------

TEST_F(AnyUsageWarningsTest, Pattern2PublicFnAnyParamFiresWarning) {
    auto p = writeTmp("p2_param.ry",
        "@public\n"
        "fn process(data: any) -> int:\n"
        "    return 1\n"
        "print(process(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: public function 'process' parameter 'data' is typed 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern2PublicFnAnyReturnFiresWarning) {
    auto p = writeTmp("p2_return.ry",
        "@public\n"
        "fn getVal() -> any:\n"
        "    return 42\n"
        "print(getVal())\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: public function 'getVal' returns 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern2NonPublicFnSuppressed) {
    auto p = writeTmp("p2_private.ry",
        "fn process(data: any) -> int:\n"
        "    return 1\n"
        "print(process(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("public function"), std::string::npos)
        << "stderr was: " << r.err;
}

// ---------------------------------------------------------------------------
// Pattern 3: unannotated fn params (implicit any)
// ---------------------------------------------------------------------------

TEST_F(AnyUsageWarningsTest, Pattern3UnannotatedParamFiresWarning) {
    auto p = writeTmp("p3_fire.ry",
        "fn addOne(x) -> int:\n"
        "    return 1\n"
        "print(addOne(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: parameter 'x' of function 'addOne' has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("defaults to 'any'"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3ExplicitAnyParamSuppressed) {
    // Explicit `x: any` annotation marks an intentional type-erasure use case;
    // Pattern 3 (which targets implicit any) should not fire.
    auto p = writeTmp("p3_explicit_any.ry",
        "fn process(x: any) -> int:\n"
        "    return 1\n"
        "print(process(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("has no type annotation"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3ParenLambdaUnannotatedFires) {
    // #2323: lambda params without an annotation default to `any` and now fire
    // the same Pattern 3 deprecation warning as named-fn params.
    auto p = writeTmp("p3_paren_lambda.ry",
        "f = (x) -> bool => true\n"
        "print(f(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: parameter 'x' of lambda has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("defaults to 'any'"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3ParenLambdaMultiParamFires) {
    // Each unannotated param produces its own warning.
    auto p = writeTmp("p3_paren_lambda_multi.ry",
        "f = (x, y) -> bool => true\n"
        "print(f(1, 2))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: parameter 'x' of lambda has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("warning: parameter 'y' of lambda has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3BareLambdaUnannotatedFires) {
    // Bare-form lambdas (`x => expr` without parens) also fire — the syntax has
    // no slot for a type annotation, so users must switch to the paren form.
    // The body returns the parameter unchanged so the Pattern 3 lint fires
    // without colliding with the `any-arithmetic` rule (#2322).
    auto p = writeTmp("p3_bare_lambda.ry",
        "id = x => x\n"
        "print(id(5))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("warning: parameter 'x' of lambda has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3LambdaExplicitTypeSuppressed) {
    auto p = writeTmp("p3_lambda_typed.ry",
        "f = (x: int) -> int => x + 1\n"
        "print(f(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("has no type annotation"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3LambdaExplicitAnySuppressed) {
    // Mirrors Pattern3ExplicitAnyParamSuppressed for lambdas: an explicit
    // `: any` annotation marks an intentional type-erasure use case.
    auto p = writeTmp("p3_lambda_explicit_any.ry",
        "f = (x: any) -> int => 1\n"
        "print(f(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("has no type annotation"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, Pattern3AnnotatedParamSuppressed) {
    auto p = writeTmp("p3_annotated.ry",
        "fn addOne(x: int) -> int:\n"
        "    return x + 1\n"
        "print(addOne(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("has no type annotation"), std::string::npos)
        << "stderr was: " << r.err;
}

// Pattern 4 (implicit any → concrete unwrap across all four Path 9 sub-cases)
// was promoted from a warning to a hard error in #2322; coverage now lives
// entirely in `tests/test_strict_any.cpp` (AnyImplicitUnwrap*RejectedByDefault).

// ---------------------------------------------------------------------------
// Cross-pattern interactions
// ---------------------------------------------------------------------------

TEST_F(AnyUsageWarningsTest, Pattern2And3NoDoubleFireOnUnannotatedPublicFn) {
    // `@public fn f(x)` triggers Pattern 3 (unannotated) but NOT Pattern 2
    // (which is gated on has_explicit_type to avoid duplicate noise on the
    // same param).
    auto p = writeTmp("p2_3_dedup.ry",
        "@public\n"
        "fn f(x) -> int:\n"
        "    return 1\n"
        "print(f(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("parameter 'x' of function 'f' has no type annotation"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_EQ(r.err.find("public function 'f' parameter 'x' is typed 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
}

// ---------------------------------------------------------------------------
// Scope gate: warnings must fire on the user's whole package (multi-file
// projects included) but NOT on stdlib / cross-package definitions the
// user cannot edit.
// ---------------------------------------------------------------------------

TEST_F(AnyUsageWarningsTest, IntraPackageImportWarnsOnSecondaryFile) {
    // The user's own `util.ry` lives in the same package as `main.ry`.
    // Even though it is reached via `from util import ...`, its definitions
    // must warn — strict-any in #2322 will eventually apply to the user's
    // full package, so the warning phase must point them out now.
    writeTmp("util_intra.ry",
        "fn helper(x) -> int:\n"          // Pattern 3 in user's own lib
        "    return 1\n"
        "\n"
        "@public\n"
        "fn pub(v: any) -> any:\n"        // Pattern 2 in user's own lib
        "    return v\n");
    auto p = writeTmp("main_intra.ry",
        "from util_intra import helper\n"
        "print(helper(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("parameter 'x' of function 'helper'"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("public function 'pub' parameter 'v' is typed 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("public function 'pub' returns 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, StdlibImportsDoNotWarn) {
    // Stdlib modules use `any` in many @public signatures
    // (json.stringify, map.getPath, ...). Importing them must not produce
    // warnings the user cannot act on. `from ry.math import sqrt` exercises
    // the from-stdlib gate (ModuleLoader marks stdlib file_ids as external).
    auto p = writeTmp("uses_stdlib.ry",
        "from ry.math import sqrt\n"
        "print(sqrt(16.0))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.err.find("warning: "), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, IntraPackageImportWarnsOnBodyLevelPatterns) {
    // Pin the file_id gate for AssignStmt sites inside an imported body
    // (Pattern 1). The imported file's stmts share file_id with the
    // enclosing fn, so suppression at the FnStmt level extends to nested
    // var decls — but only because current_loc_ updates via emitStmt
    // (AssignStmt&). Lock in that intra-package code also warns body-level.
    writeTmp("util_body.ry",
        "fn run(x: int) -> int:\n"
        "    erased: any = x\n"           // Pattern 1 in user's own lib
        "    return x\n");
    auto p = writeTmp("main_body.ry",
        "from util_body import run\n"
        "print(run(7))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("variable 'erased' is annotated 'any'"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(AnyUsageWarningsTest, MainFileWarningsStillFireWithImports) {
    // Even when imports are present, the user's OWN code in main must still
    // surface warnings — the gate filters by file_id, not by program shape.
    writeTmp("libMain.ry",
        "fn helper(x: int) -> int:\n"
        "    return x + 1\n");
    auto p = writeTmp("main_mixed.ry",
        "from libMain import helper\n"
        "fn local(y) -> int:\n"
        "    return helper(1)\n"
        "print(local(42))\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.err.find("parameter 'y' of function 'local'"),
              std::string::npos)
        << "stderr was: " << r.err;
}
