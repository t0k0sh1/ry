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

// withStdlibDirectiveDecls() is intentionally omitted: this test exercises the
// real CLI -> ModuleLoader -> directive.ry path, not the in-process harness.
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

static size_t countOccurrences(const std::string &haystack, const std::string &needle) {
    if (needle.empty())
        return 0;
    size_t count = 0;
    size_t pos = 0;
    while ((pos = haystack.find(needle, pos)) != std::string::npos) {
        ++count;
        pos += needle.size();
    }
    return count;
}

class DeprecatedWarningsTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::temp_directory_path() / "ry_deprecated_warnings_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

TEST_F(DeprecatedWarningsTest, DeprecatedFunctionEmitsWarningToStderr) {
    auto p = writeTmp("depr_once.ry",
        "@deprecated\n"
        "fn oldApi() -> int:\n"
        "    return 1\n"
        "print(oldApi())\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "1\n");
    EXPECT_NE(r.err.find("warning: 'oldApi' is deprecated"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(DeprecatedWarningsTest, DeprecatedFunctionWarningDeduplicated) {
    auto p = writeTmp("depr_many.ry",
        "@deprecated\n"
        "fn oldApi() -> int:\n"
        "    return 1\n"
        "print(oldApi())\n"
        "print(oldApi())\n"
        "print(oldApi())\n"
        "print(oldApi())\n"
        "print(oldApi())\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "1\n1\n1\n1\n1\n");
    EXPECT_EQ(countOccurrences(r.err, "warning: 'oldApi' is deprecated"), 1u)
        << "stderr was: " << r.err;
}

TEST_F(DeprecatedWarningsTest, NonDeprecatedFunctionNoWarning) {
    auto p = writeTmp("no_depr.ry",
        "fn freshApi() -> int:\n"
        "    return 1\n"
        "print(freshApi())\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "1\n");
    EXPECT_EQ(r.err.find("deprecated"), std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(DeprecatedWarningsTest, EmitLlvmIrPathEmitsDeprecationWarning) {
    auto p = writeTmp("depr_ir.ry",
        "@deprecated\n"
        "fn oldApi() -> int:\n"
        "    return 1\n"
        "print(oldApi())\n");
    auto r = runRy({"--emit-llvm-ir", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("ModuleID"), std::string::npos);
    EXPECT_NE(r.err.find("warning: 'oldApi' is deprecated"), std::string::npos)
        << "stderr was: " << r.err;
}
