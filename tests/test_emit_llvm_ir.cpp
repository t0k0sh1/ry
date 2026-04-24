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

// Run ry binary with the given arguments.
// Returns {stdout, stderr, exit_code}.  stdin is closed in the child.
// RY_ENV=internal is set so tests run without the installed global stdlib;
// all test sources in this file use only built-in types (no stdlib import).
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

        std::vector<const char *> argv{"ry"};
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

class EmitLlvmIrTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::temp_directory_path() / "ry_emit_llvm_ir_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

// ------------------------------------------------------------------
// Happy paths
// ------------------------------------------------------------------

TEST_F(EmitLlvmIrTest, ValidFileExitZero) {
    auto p = writeTmp("add.ry",
        "fn add(a: int, b: int) -> int:\n"
        "  return a + b\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
}

TEST_F(EmitLlvmIrTest, OutputContainsModuleId) {
    auto p = writeTmp("add.ry",
        "fn add(a: int, b: int) -> int:\n"
        "  return a + b\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_NE(r.out.find("ModuleID"), std::string::npos);
}

TEST_F(EmitLlvmIrTest, OutputContainsFunctionDefine) {
    auto p = writeTmp("add.ry",
        "fn add(a: int, b: int) -> int:\n"
        "  return a + b\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_NE(r.out.find("define"), std::string::npos);
}

TEST_F(EmitLlvmIrTest, UnoptimizedIrRetainsAlloca) {
    // Unoptimized IR keeps alloca/store/load for function arguments.
    // Post-opt IR would mem2reg these away; --emit-llvm-ir must not run opt.
    auto p = writeTmp("add.ry",
        "fn add(a: int, b: int) -> int:\n"
        "  return a + b\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_NE(r.out.find("alloca"), std::string::npos);
}

TEST_F(EmitLlvmIrTest, DoesNotExecuteProgram) {
    // If executed, main() overflows INT64_MAX + 1 and the overflow handler
    // aborts with a non-zero exit and stderr output.  With --emit-llvm-ir,
    // codegen must succeed and execution must be skipped entirely.
    auto p = writeTmp("noop.ry",
        "fn main():\n"
        "  x: int = 9223372036854775807\n"
        "  x = x + 1\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_TRUE(r.err.empty());
}

// ------------------------------------------------------------------
// Error paths
// ------------------------------------------------------------------

TEST_F(EmitLlvmIrTest, NonexistentFileExitNonzero) {
    auto r = runRy({"--emit-llvm-ir", "/nonexistent/does_not_exist.ry"});
    EXPECT_NE(r.exit_code, 0);
}

TEST_F(EmitLlvmIrTest, SyntaxErrorExitNonzero) {
    auto p = writeTmp("bad.ry", "fn bad( -> int:\n  return 0\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_FALSE(r.err.empty());
}

TEST_F(EmitLlvmIrTest, SyntaxErrorStdoutEmpty) {
    // On error, no partial IR should appear on stdout.
    auto p = writeTmp("bad.ry", "fn bad( -> int:\n  return 0\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_TRUE(r.out.empty());
}
