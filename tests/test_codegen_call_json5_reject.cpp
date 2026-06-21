#include <gtest/gtest.h>
#include <array>
#include <cerrno>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

// Subprocess-based test for emitJson5Load type rejection. Mirrors
// tests/test_codegen_call_json_reject.cpp — the non-File branch must reject
// any pointer-typed argument that is not a str. Per #1869 argv[0] must be
// RY_BINARY_PATH (Linux glibc fs::canonical("") trap).

struct RunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
};

static RunResult runRy(const std::vector<const char *> &args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0)
        return {};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {};
    }

    std::vector<const char *> argv{RY_BINARY_PATH};
    argv.insert(argv.end(), args.begin(), args.end());
    argv.push_back(nullptr);

    // setenv must run in the PARENT before fork — it is not async-signal-safe
    // and would deadlock if called in the child between fork and execv. Save
    // the previous value so the parent's env isn't permanently mutated and
    // RY_ENV=internal doesn't leak into subsequent tests in the same process.
    const char *prev_env = getenv("RY_ENV");
    bool had_prev_env = prev_env != nullptr;
    std::string saved_env = had_prev_env ? prev_env : "";
    setenv("RY_ENV", "internal", 1);

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
    pid_t wp;
    do {
        wp = waitpid(pid, &status, 0);
    } while (wp == -1 && errno == EINTR);
    r.exit_code = (wp != -1 && WIFEXITED(status)) ? WEXITSTATUS(status) : -1;
    // Restore the parent's RY_ENV so this fixture does not leak into later tests.
    if (had_prev_env) setenv("RY_ENV", saved_env.c_str(), 1);
    else unsetenv("RY_ENV");
    return r;
}

class Json5LoadTypeRejectTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_json5_load_reject_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

TEST_F(Json5LoadTypeRejectTest, LoadRejectsListPointer) {
    auto p = writeTmp("load_reject_list.ry",
        "from json5 import load\n"
        "xs: List<int> = [1, 2, 3]\n"
        "r = load[Map<str, int>](xs)\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find("load[T]() requires a str or File argument"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(Json5LoadTypeRejectTest, LoadRejectsMapPointer) {
    auto p = writeTmp("load_reject_map.ry",
        "from json5 import load\n"
        "m: Map<str, int> = {\"a\": 1}\n"
        "r = load[List<int>](m)\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find("load[T]() requires a str or File argument"),
              std::string::npos)
        << "stderr was: " << r.err;
}

// Regression guard: str argument must still compile and execute.
TEST_F(Json5LoadTypeRejectTest, LoadAcceptsStrArgument) {
    auto p = writeTmp("load_accept_str.ry",
        "from json5 import load\n"
        "case load[Map<str, int>](\"{a: 1}\"):\n"
        "  Ok(_):\n"
        "    print(\"ok\")\n"
        "  Err(_):\n"
        "    print(\"err\")\n");
    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_EQ(r.out, "ok\n") << "stderr: " << r.err;
}
