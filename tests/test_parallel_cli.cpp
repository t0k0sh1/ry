#include <gtest/gtest.h>

#include <array>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include <sys/wait.h>
#include <unistd.h>

namespace fs = std::filesystem;


// Local copy of the runRy helper used in tests/test_help.cpp.
// Each subprocess test file keeps its own copy per the existing pattern.
// RY_BINARY_PATH is passed as BOTH execv's path and argv[0] per
// .claude/rules/tests-cpp-conventions.md (Linux glibc's fs::canonical("")
// fails when argv[0] is a bare "ry", breaking stdlib discovery).
struct RunResult {
    std::string out;
    std::string err;
    int exit_code;
};

static RunResult runRy(std::initializer_list<const char *> args) {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0 || pipe(pipeErr) != 0) return {"", "", -1};

    pid_t pid = fork();
    if (pid < 0) return {"", "", -1};

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);
        setenv("RY_ENV", "internal", 1);

        std::vector<const char *> argv;
        argv.push_back(RY_BINARY_PATH);
        for (auto a : args) argv.push_back(a);
        argv.push_back(nullptr);

        execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
        _exit(127);
    }

    close(pipeOut[1]);
    close(pipeErr[1]);

    auto readAll = [](int fd) -> std::string {
        std::string result;
        std::array<char, 4096> buf;
        ssize_t n;
        while ((n = read(fd, buf.data(), buf.size())) > 0)
            result.append(buf.data(), static_cast<size_t>(n));
        close(fd);
        return result;
    };

    std::string out = readAll(pipeOut[0]);
    std::string err = readAll(pipeErr[0]);

    int status;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;

    return {out, err, exit_code};
}

// --- Help text mentions the optional N ---

TEST(ParallelCLI, HelpMentionsOptionalN) {
    auto r = runRy({"test", "--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("--parallel"), std::string::npos);
    EXPECT_NE(r.out.find("[N]"), std::string::npos);
}

// --- Rejection: zero / empty / non-digit / negative / malformed ---

TEST(ParallelCLI, RejectsZeroSpaced) {
    auto r = runRy({"test", "-p", "0"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsZeroEqualsForm) {
    auto r = runRy({"test", "--parallel=0"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsEmptyEqualsForm) {
    auto r = runRy({"test", "--parallel="});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("requires a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsNonDigitEqualsForm) {
    auto r = runRy({"test", "--parallel=abc"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsNegativeSpaced) {
    auto r = runRy({"test", "-p", "-5"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsMalformedSpaced) {
    auto r = runRy({"test", "-p", "1.5"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

TEST(ParallelCLI, RejectsTrailingJunkSpaced) {
    auto r = runRy({"test", "-p", "1abc"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("must be a positive integer"), std::string::npos);
}

// --- Disambiguation: non-digit / non-count next arg is treated as target,
// NOT as an invalid count. The parser must NOT report a parallel-count
// error for these. The "no such file" or "package.toml not found"
// diagnostic from the path-handling code is acceptable; the parallel-
// count error is not.

TEST(ParallelCLI, NonDigitNonCountArgTreatedAsTarget) {
    auto r = runRy({"test", "-p", "no_such_path_2177_xyz.ry"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_EQ(r.err.find("worker count must be a positive integer"),
              std::string::npos);
}

// --- #2234: subprocess fan-out 一本化の検知テスト ---
//
// 新仕様: `ry test <dir>` は -p の有無に関係なく subprocess fan-out 経路に入る。
// multi-file 時の --coverage / --trace / --outline は警告 + disable (cross-subprocess
// 集計は scope 外; #2236 で outline が部分対応予定)。
//
// fixture は package.toml なしの tmpdir + 2 個の minimal .test.ry。
// `runRy({"test", tmpdir})` で path_is_directory == true 分岐に入る。

class TestRunnerFanOut : public ::testing::Test {
protected:
    fs::path tmpDir_;
    std::string dir_;
    void SetUp() override {
        // tmpdir は ry バイナリ隣に置く (test_codegen_call_json_reject.cpp と同じ流儀)。
        // SetUp 側は例外形式で既存テストの規約に揃える (TearDown は noexcept なので ec 版)。
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() /
                  "ry_test_runner_fanout_test";
        fs::remove_all(tmpDir_);
        fs::create_directories(tmpDir_);
        // testing intrinsics は `from testing import` が必要 (tests/spec/*.test.ry 流儀)。
        // package.toml なし tmpdir でも ModuleLoader は global stdlib (~/.ry/share or
        // exe-adjacent share) を解決できる。RY_ENV=internal を runRy 内で setenv するため
        // 開発時は exe-adjacent share を使う。
        std::ofstream(tmpDir_ / "a.test.ry")
            << "from testing import it, expect\n"
            << "\n"
            << "@it(\"a passes\")\n"
            << "fn aPasses():\n"
            << "  expect(1).toEq(1)\n";
        std::ofstream(tmpDir_ / "b.test.ry")
            << "from testing import it, expect\n"
            << "\n"
            << "@it(\"b passes\")\n"
            << "fn bPasses():\n"
            << "  expect(2).toEq(2)\n";
        dir_ = tmpDir_.string();
    }
    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmpDir_, ec);
    }
};

// `-p` なしでも subprocess fan-out 経路 (旧仕様では sequential 経路で stderr に
// "worker" 文字列は出なかった)
TEST_F(TestRunnerFanOut, DefaultModeUsesSubprocessDispatch) {
    auto r = runRy({"test", dir_.c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout=" << r.out << "\nstderr=" << r.err;
    EXPECT_NE(r.err.find("worker"), std::string::npos)
        << "Default mode (no -p) should now go through subprocess fan-out, "
           "which prints '...with N worker(s)...' to stderr.\nstderr=" << r.err;
}

// `-p 1` で 1 worker subprocess 経路 (単複対応で "1 worker", not "1 workers")
TEST_F(TestRunnerFanOut, SingleWorkerUsesSubprocessDispatch) {
    auto r = runRy({"test", "-p", "1", dir_.c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout=" << r.out << "\nstderr=" << r.err;
    EXPECT_NE(r.err.find("1 worker"), std::string::npos)
        << "-p 1 should use subprocess fan-out with singular 'worker', "
           "not plural 'workers'.\nstderr=" << r.err;
    EXPECT_EQ(r.err.find("1 workers"), std::string::npos)
        << "'1 workers' is a grammar wart; expected singular form.\nstderr="
        << r.err;
}

// --coverage multi-file で警告 + disable
TEST_F(TestRunnerFanOut, CoverageMultiFileEmitsWarningAndDisables) {
    auto r = runRy({"test", "--coverage", dir_.c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout=" << r.out << "\nstderr=" << r.err;
    EXPECT_NE(r.err.find("Warning: --coverage is not supported with multi-file"),
              std::string::npos)
        << "Multi-file --coverage should warn and disable, since per-file "
           "coverage is not aggregated across subprocesses.\nstderr=" << r.err;
}

// --trace multi-file で警告 + disable
TEST_F(TestRunnerFanOut, TraceMultiFileEmitsWarningAndDisables) {
    auto r = runRy({"test", "--trace", dir_.c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout=" << r.out << "\nstderr=" << r.err;
    EXPECT_NE(r.err.find("Warning: --trace is not supported with multi-file"),
              std::string::npos)
        << "Multi-file --trace should warn and disable, since multiple "
           "subprocesses would clobber the same trace-out file.\nstderr="
        << r.err;
}

// --outline multi-file で警告 + disable (silent regression 防止)
TEST_F(TestRunnerFanOut, OutlineMultiFileEmitsWarningAndDisables) {
    auto r = runRy({"test", "--outline", dir_.c_str()});
    EXPECT_EQ(r.exit_code, 0) << "stdout=" << r.out << "\nstderr=" << r.err;
    EXPECT_NE(r.err.find("Warning: --outline is not supported with multi-file"),
              std::string::npos)
        << "Multi-file --outline should warn and disable until #2236 threads "
           "--outline through to subprocesses.\nstderr=" << r.err;
}
