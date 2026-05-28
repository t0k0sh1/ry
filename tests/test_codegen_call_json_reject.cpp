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

// Subprocess-based test for emitJsonLoad type rejection (#1941).
//
// emitJsonLoad's non-File branch must reject any pointer-typed argument that
// is not actually a str — List / Map / Set / closure / resource handles all
// share ptrTy_ (opaque pointer) with str, so the prior `getType() != ptrTy_`
// check let them through and the value reached __ry_json_parse_to_any which
// reads it as text (invalid read under ASan/UBSan). The fix gates on
// isStringValue, which combines the ptrTy_ check with isNonStrPointer.
//
// The in-process codegen harness (CodeGenTest::runSource) cannot compile
// `from json import load` because it bypasses ModuleLoader. This test forks
// ./build/ry so the full Parser -> ModuleLoader -> CodeGen pipeline runs.
//
// Per .claude/rules/tests-cpp-conventions.md (#1869), argv[0] passes the
// full RY_BINARY_PATH; the bare "ry" form breaks fs::canonical("") on Linux
// glibc and silently masks the test by failing stdlib resolution before
// codegen runs.

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

    // Build argv and set RY_ENV in the PARENT before fork. After fork the
    // child must only invoke async-signal-safe calls (close / dup2 / execv);
    // setenv and std::vector heap operations can deadlock on libc / allocator
    // locks if any other thread in the test runner is holding them.
    std::vector<const char *> argv{RY_BINARY_PATH};
    argv.insert(argv.end(), args.begin(), args.end());
    argv.push_back(nullptr);
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
    return r;
}

class JsonLoadTypeRejectTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Place fixtures inside the build tree so the source file's ancestor
        // chain reaches the repo's package.toml and the loader can resolve
        // _dev_stdlib (where json lives). /tmp has no such ancestor.
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_json_load_reject_test";
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTmp(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

TEST_F(JsonLoadTypeRejectTest, LoadRejectsListPointer) {
    auto p = writeTmp("load_reject_list.ry",
        "from json import load\n"
        "xs: List<int> = [1, 2, 3]\n"
        "r = load[Map<str, int>](xs)\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find("load[T]() requires a str or File argument"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(JsonLoadTypeRejectTest, LoadRejectsMapPointer) {
    auto p = writeTmp("load_reject_map.ry",
        "from json import load\n"
        "m: Map<str, int> = {\"a\": 1}\n"
        "r = load[List<int>](m)\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find("load[T]() requires a str or File argument"),
              std::string::npos)
        << "stderr was: " << r.err;
}

TEST_F(JsonLoadTypeRejectTest, LoadRejectsSetPointer) {
    auto p = writeTmp("load_reject_set.ry",
        "from json import load\n"
        "s: Set<int> = {1, 2, 3}\n"
        "r = load[Map<str, int>](s)\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_NE(r.err.find("load[T]() requires a str or File argument"),
              std::string::npos)
        << "stderr was: " << r.err;
}

// Regression guard: str argument must still compile and execute. This locks
// in the positive sibling so the fix does not over-reject.
TEST_F(JsonLoadTypeRejectTest, LoadAcceptsStrArgument) {
    auto p = writeTmp("load_accept_str.ry",
        "from json import load\n"
        "case load[Map<str, int>](\"{\\\"a\\\": 1}\"):\n"
        "  Ok(_):\n"
        "    print(\"ok\")\n"
        "  Err(_):\n"
        "    print(\"err\")\n");
    auto r = runRy({p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout: " << r.out << "\nstderr: " << r.err;
    EXPECT_EQ(r.out, "ok\n") << "stderr: " << r.err;
}
