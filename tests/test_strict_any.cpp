#include <gtest/gtest.h>
#include <array>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>
#include <utility>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

namespace {

struct RunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
};

using EnvList = std::vector<std::pair<std::string, std::string>>;

// Mirrors tests/test_deprecated_warnings.cpp::runRy but takes an extra
// per-call env list applied in the CHILD (after fork, before execv) so
// RY_STRICT_ANY never leaks between cases.
RunResult runRy(std::vector<const char *> args, const EnvList &extraEnv) {
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
        // Clear inherited RY_STRICT_ANY so compat-mode cases (which pass
        // an empty extraEnv) cannot be flipped strict by the parent
        // process's env. extraEnv may re-set it for strict cases.
        unsetenv("RY_STRICT_ANY");
        for (const auto &kv : extraEnv)
            setenv(kv.first.c_str(), kv.second.c_str(), 1);

        // argv[0] must be the full RY_BINARY_PATH (not bare "ry") for
        // Linux glibc exe-adjacent stdlib resolution; see
        // .claude/rules/tests-cpp-conventions.md.
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

class StrictAnyTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Place fixtures under the build tree so the source file's
        // ancestor chain reaches the repo's package.toml.
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_strict_any_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

constexpr const char *kStrictAnyEnv = "RY_STRICT_ANY";
constexpr const char *kArithRuleTag = "[strict-any/any-arithmetic]";
constexpr const char *kImplicitUnwrapTag = "[strict-any/any-implicit-unwrap]";

} // namespace

TEST_F(StrictAnyTest, AnyArithRejectedWithEnv) {
    auto p = writeTmp("any_arith.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a + b)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kArithRuleTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyArithAllowedWithoutFlag) {
    auto p = writeTmp("any_arith_compat.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a + b)\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "3\n");
}

TEST_F(StrictAnyTest, StrictAnyCliFlagWorks) {
    auto p = writeTmp("any_arith_cli.ry",
        "a: any = 1\n"
        "b: any = 2\n"
        "print(a + b)\n");
    auto r = runRy({"--strict-any", "run", p.string().c_str()}, {});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kArithRuleTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyComparisonStillWorksInStrict) {
    auto p = writeTmp("any_cmp.ry",
        "a: any = 1\n"
        "print(a == 1)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_EQ(r.exit_code, 0)
        << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_EQ(r.out, "true\n");
}

TEST_F(StrictAnyTest, UnaryAnyArithRejectedInStrict) {
    auto p = writeTmp("any_unary.ry",
        "a: any = 5\n"
        "print(-a)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kArithRuleTag), std::string::npos)
        << "stderr was: " << r.err;
}

// Compound assignment routes through applyCompoundOp → emitBinaryOp
// (codegen_stmt_misc.cpp), so the guard fires for `a += 1` the same way
// it does for `a = a + 1`. Locks in that the rule covers the compound
// path — a regression here would silently break the headline contract.
TEST_F(StrictAnyTest, CompoundAnyArithRejectedInStrict) {
    auto p = writeTmp("any_compound.ry",
        "a: any = 1\n"
        "a += 2\n"
        "print(a)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kArithRuleTag), std::string::npos)
        << "stderr was: " << r.err;
}

// `ry --strict-any test <file>` (pre-subcommand position) covers the test
// runner integration mentioned in #2319. The CLI flag is consumed by
// parseGlobalFlags (src/cli/cli.cpp) which setenv-s RY_STRICT_ANY before
// the subcommand dispatcher reaches `ry test`; the test runner then
// fork-execs per-file subprocesses that inherit the env var.
TEST_F(StrictAnyTest, StrictAnyForTestSubcommandViaPreSubcommandFlag) {
    auto p = writeTmp("any_arith_test.test.ry",
        "from ry.testing import it\n"
        "@it(\"rejects any arithmetic\")\n"
        "fn rejectsAnyArith():\n"
        "    a: any = 1\n"
        "    b: any = 2\n"
        "    print(a + b)\n");
    auto r = runRy({"--strict-any", "test", p.string().c_str()}, {});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kArithRuleTag), std::string::npos)
        << "stderr was: " << r.err;
}

// ---------------------------------------------------------------------------
// #2321: any-implicit-unwrap rule. Covers the four Path 9 sub-cases from
// docs/architecture/implicit-any-paths.md:
//   9a — variable declaration `n: int = v` where `v: any`
//   9b — named function-call argument `f(v)` (and the default-value path)
//   9c — lambda-call argument
//   9d — `Ok(v)` / `Err(v)` / `Some(v)` into a typed Result/Option slot
// Each sub-case has a strict-rejects / compat-allows pair mirroring the
// arithmetic test structure above.
// ---------------------------------------------------------------------------

TEST_F(StrictAnyTest, AnyImplicitUnwrapVarDeclRejectedInStrict) {
    auto p = writeTmp("any_unwrap_var.ry",
        "v: any = 1\n"
        "n: int = v\n"
        "print(n)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapVarDeclAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_var_compat.ry",
        "v: any = 1\n"
        "n: int = v\n"
        "print(n)\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "1\n");
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapNamedCallArgRejectedInStrict) {
    auto p = writeTmp("any_unwrap_call.ry",
        "fn f(n: int) -> int:\n"
        "    return n + 1\n"
        "v: any = 1\n"
        "print(f(v))\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapNamedCallArgAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_call_compat.ry",
        "fn f(n: int) -> int:\n"
        "    return n + 1\n"
        "v: any = 1\n"
        "print(f(v))\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "2\n");
}

// Default-value branch (codegen_call_user.cpp:220-222) emits a distinct
// "passing 'any' default value" message. Pins that the branch is wired —
// the explicit-arg test above hits the :192-194 branch and would not
// surface a regression in the default-value codegen.
TEST_F(StrictAnyTest, AnyImplicitUnwrapDefaultValueRejectedInStrict) {
    auto p = writeTmp("any_unwrap_default.ry",
        "fn getAny() -> any:\n"
        "    return 42\n"
        "fn f(x: int = getAny()) -> int:\n"
        "    return x + 1\n"
        "print(f())\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
    EXPECT_NE(r.err.find("default value"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapDefaultValueAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_default_compat.ry",
        "fn getAny() -> any:\n"
        "    return 42\n"
        "fn f(x: int = getAny()) -> int:\n"
        "    return x + 1\n"
        "print(f())\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "43\n");
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapLambdaCallArgRejectedInStrict) {
    auto p = writeTmp("any_unwrap_lambda.ry",
        "g = (n: int) -> int => n + 1\n"
        "v: any = 1\n"
        "print(g(v))\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapLambdaCallArgAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_lambda_compat.ry",
        "g = (n: int) -> int => n + 1\n"
        "v: any = 1\n"
        "print(g(v))\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "2\n");
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapOkSlotRejectedInStrict) {
    auto p = writeTmp("any_unwrap_ok.ry",
        "fn f(v: any) -> Result<int, str>:\n"
        "    return Ok(v)\n"
        "case f(1):\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapOkSlotAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_ok_compat.ry",
        "fn f(v: any) -> Result<int, str>:\n"
        "    return Ok(v)\n"
        "case f(1):\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "1\n");
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapErrSlotRejectedInStrict) {
    auto p = writeTmp("any_unwrap_err.ry",
        "fn f(v: any) -> Result<int, str>:\n"
        "    return Err(v)\n"
        "case f(\"oops\"):\n"
        "    Ok(n): print(n)\n"
        "    Err(e): print(e)\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapSomeSlotRejectedInStrict) {
    auto p = writeTmp("any_unwrap_some.ry",
        "fn f(v: any) -> Option<int>:\n"
        "    return Some(v)\n"
        "case f(1):\n"
        "    Some(n): print(n)\n"
        "    None: print(\"none\")\n");
    auto r = runRy({"run", p.string().c_str()}, {{kStrictAnyEnv, "1"}});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find(kImplicitUnwrapTag), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(StrictAnyTest, AnyImplicitUnwrapSomeSlotAllowedWithoutFlag) {
    auto p = writeTmp("any_unwrap_some_compat.ry",
        "fn f(v: any) -> Option<int>:\n"
        "    return Some(v)\n"
        "case f(1):\n"
        "    Some(n): print(n)\n"
        "    None: print(\"none\")\n");
    auto r = runRy({"run", p.string().c_str()}, {});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.out, "1\n");
}
