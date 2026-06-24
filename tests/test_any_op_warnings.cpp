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

namespace {

struct RunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
};

RunResult runRy(std::vector<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0) return {};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]); close(pipeOut[1]);
        close(pipeErr[0]); close(pipeErr[1]);
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

        // argv[0] must be the full RY_BINARY_PATH so Linux glibc's
        // fs::canonical(parent_path(argv[0])) resolves the exe-adjacent
        // stdlib search path (.claude/rules/tests-cpp-conventions.md).
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

size_t countOccurrences(const std::string &haystack, const std::string &needle) {
    if (needle.empty()) return 0;
    size_t count = 0;
    size_t pos = 0;
    while ((pos = haystack.find(needle, pos)) != std::string::npos) {
        ++count;
        pos += needle.size();
    }
    return count;
}

class AnyOpWarningsTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "any_op_warnings_test";
        fs::remove_all(tmpDir_);
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeScript(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

} // namespace

// Arithmetic on `any` emits a deprecation warning and mentions the
// `asType` migration path. The program still runs and produces the
// expected result — this is warn-only during the deprecation window.
TEST_F(AnyOpWarningsTest, ArithmeticPlusEmitsWarning) {
    auto p = writeScript("any_plus.ry",
        "x: any = 10\n"
        "y: any = 20\n"
        "z: any = x + y\n"
        "print(z)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("asType"), std::string::npos)
        << "stderr should suggest 'asType' for migration: " << r.err;
    EXPECT_NE(r.err.find("'+'"), std::string::npos)
        << "stderr should quote the offending operator: " << r.err;
    EXPECT_NE(r.out.find("30"), std::string::npos)
        << "program must still produce the result: " << r.out;
}

TEST_F(AnyOpWarningsTest, OrderingLtEmitsWarning) {
    auto p = writeScript("any_lt.ry",
        "x: any = 3\n"
        "y: any = 4\n"
        "print(x < y)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("'<'"), std::string::npos)
        << "stderr should quote the offending operator: " << r.err;
    EXPECT_NE(r.out.find("true"), std::string::npos)
        << "program must still produce the result: " << r.out;
}

TEST_F(AnyOpWarningsTest, UnaryNegEmitsWarning) {
    auto p = writeScript("any_neg.ry",
        "x: any = 5\n"
        "print(-x)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "stderr should contain 'deprecated': " << r.err;
    EXPECT_NE(r.err.find("unary"), std::string::npos)
        << "stderr should label the operator as unary: " << r.err;
    EXPECT_NE(r.out.find("-5"), std::string::npos)
        << "program must still negate: " << r.out;
}

// Equality is explicitly retained per #2316 acceptance criteria —
// `__ry_any_eq` returns 0 (false) on type mismatch rather than trapping,
// so it's safe dynamic. No warning expected.
TEST_F(AnyOpWarningsTest, EqualityNoWarning) {
    auto p = writeScript("any_eq.ry",
        "x: any = 3\n"
        "y: any = 3\n"
        "print(x == y)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "equality on any must NOT warn (retained-and-documented): " << r.err;
    EXPECT_NE(r.out.find("true"), std::string::npos) << r.out;
}

TEST_F(AnyOpWarningsTest, InequalityNoWarning) {
    auto p = writeScript("any_ne.ry",
        "x: any = 3\n"
        "y: any = 4\n"
        "print(x != y)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "inequality on any must NOT warn (retained-and-documented): " << r.err;
    EXPECT_NE(r.out.find("true"), std::string::npos) << r.out;
}

// String conversion is retained too — `print(any)` and f-string
// interpolation must not warn.
TEST_F(AnyOpWarningsTest, PrintAnyNoWarning) {
    auto p = writeScript("any_print.ry",
        "x: any = 42\n"
        "print(x)\n"
        "print(f\"value={x}\")\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "print(any) and f-string must NOT warn (retained-and-documented): "
        << r.err;
}

// Compound assignment routes through emitBinaryOp -> emitAnyBinaryOp,
// so the warning at the binary-op gate covers it for free.
TEST_F(AnyOpWarningsTest, CompoundAssignEmitsWarning) {
    auto p = writeScript("any_compound.ry",
        "x: any = 10\n"
        "x += 5\n"
        "print(x)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "compound assignment must warn (routes through emitAnyBinaryOp): "
        << r.err;
    EXPECT_NE(r.out.find("15"), std::string::npos) << r.out;
}

// Per-operator dedup: five `+` uses on `any` in one program produce
// exactly one warning.
TEST_F(AnyOpWarningsTest, WarningDedupedPerOperator) {
    auto p = writeScript("any_dedup.ry",
        "x: any = 1\n"
        "a: any = x + x\n"
        "b: any = x + x\n"
        "c: any = x + x\n"
        "d: any = x + x\n"
        "e: any = x + x\n"
        "print(e)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_EQ(countOccurrences(r.err, "deprecated"), 1u)
        << "same operator must warn at most once, stderr was: " << r.err;
}

// One operand `any` + one concrete: the concrete is auto-wrapped at the
// binary-op gate (codegen_expr.cpp:1716-1717), so the warning still
// fires inside emitAnyBinaryOp.
TEST_F(AnyOpWarningsTest, MixedAnyAndConcreteEmitsWarning) {
    auto p = writeScript("any_mixed.ry",
        "x: any = 10\n"
        "print(x + 20)\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stderr: " << r.err;
    EXPECT_NE(r.err.find("deprecated"), std::string::npos)
        << "mixed any+concrete must warn (concrete is auto-wrapped): "
        << r.err;
    EXPECT_NE(r.out.find("30"), std::string::npos) << r.out;
}
