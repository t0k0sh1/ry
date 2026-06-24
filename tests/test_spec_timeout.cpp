#include <gtest/gtest.h>
#include <array>
#include <chrono>
#include <cstdlib>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>
#include <vector>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

struct TimeoutRunResult {
    std::string out;
    std::string err;
    int exit_code = -1;
    bool killed_by_backstop = false;
};

// Backstop wait: tests use @timeout(200) so the child should exit well under a
// second under normal builds, and within a few seconds under ASan on slow CI
// runners.  A 30-second WNOHANG poll + SIGKILL fallback prevents an
// unfinished runtime implementation from hanging ry_tests indefinitely while
// being generous enough that JIT warmup on instrumented Linux containers does
// not cause spurious failures (the actual @timeout(200) deadline is what gets
// exercised; the backstop only fires on broken implementations).
static constexpr int kBackstopSeconds = 30;

static TimeoutRunResult runRyTest(const fs::path &scriptPath) {
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

        // chdir to the repo root so the dev stdlib path in package.toml
        // resolves; otherwise `from testing import timeout` is unresolved.
        if (chdir(RY_SOURCE_ROOT) != 0)
            _exit(126);

        std::vector<const char *> argv{"ry", "test",
                                        scriptPath.c_str(), nullptr};
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

    TimeoutRunResult r;
    std::thread outReader([&]() { r.out = readAll(pipeOut[0]); });
    std::thread errReader([&]() { r.err = readAll(pipeErr[0]); });

    auto deadline = std::chrono::steady_clock::now() +
                    std::chrono::seconds(kBackstopSeconds);
    int status = 0;
    bool reaped = false;
    while (std::chrono::steady_clock::now() < deadline) {
        pid_t w = waitpid(pid, &status, WNOHANG);
        if (w == pid) {
            reaped = true;
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
    if (!reaped) {
        r.killed_by_backstop = true;
        kill(pid, SIGKILL);
        waitpid(pid, &status, 0);
    }
    outReader.join();
    errReader.join();
    r.exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return r;
}

class SpecTimeoutTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Test files must live under a directory whose upward search reaches
        // the repo's package.toml — otherwise dev stdlib resolution fails
        // and `from testing import timeout` cannot be satisfied.  Use the
        // per-preset CMake binary dir (e.g. build/, build-asan/) so the
        // directory is host-backed under docker per-preset bind mounts —
        // a hard-coded `RY_SOURCE_ROOT/build` would route asan / fuzz
        // writes to the docker overlay and ENOSPC there.  Per-pid subdir
        // avoids parallel test-run collisions.
        tmpDir_ = fs::path(RY_BINARY_DIR) /
                  ("test_spec_timeout_" +
                   std::to_string(static_cast<long>(getpid())));
        fs::create_directories(tmpDir_);
    }

    void TearDown() override { fs::remove_all(tmpDir_); }

    fs::path writeTest(const std::string &name, const std::string &content) {
        auto p = tmpDir_ / name;
        std::ofstream(p) << content;
        return p;
    }
};

// Infinite-loop test under @timeout(200) must be marked as failed and
// subsequent tests must still run.  This is the core regression guard
// for #1688 — without per-test timeout, the alarm() handler would
// _exit(124) and the trailing test would never execute.
TEST_F(SpecTimeoutTest, InfiniteLoopTestTimesOutAndContinuesToNext) {
    auto path = writeTest("infinite_loop.test.ry",
        "from ry.testing import it, expect, timeout\n"
        "\n"
        "@timeout(200)\n"
        "@it(\"infinite loop times out\")\n"
        "fn infiniteLoopTimesOut():\n"
        "  i = 0\n"
        "  while i < 1:\n"
        "    i = 0\n"
        "\n"
        "@it(\"runs after timeout\")\n"
        "fn runsAfterTimeout():\n"
        "  expect(1 + 1).toEq(2)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because one test timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("timeout"), std::string::npos)
        << "stdout should mention 'timeout'.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("runs after timeout"), std::string::npos)
        << "Subsequent test must still run after a timeout.\n"
        << "stdout:\n" << r.out;
}

// Mixed sequence [normal, timeout, timeout, normal]: state cleanup between
// consecutive timeouts must be symmetric, and trailing normal tests must
// still run after multiple timeouts. Guards against a regression where
// signal-handler / timeout-end paths drift in their state-reset behaviour.
TEST_F(SpecTimeoutTest, MixedTimeoutsContinueExecution) {
    auto path = writeTest("mixed_timeouts.test.ry",
        "from ry.testing import it, expect, timeout\n"
        "\n"
        "@it(\"normal one\")\n"
        "fn normalOne():\n"
        "  expect(1).toEq(1)\n"
        "\n"
        "@timeout(200)\n"
        "@it(\"timeout one\")\n"
        "fn timeoutOne():\n"
        "  i = 0\n"
        "  while i < 1:\n"
        "    i = 0\n"
        "\n"
        "@timeout(200)\n"
        "@it(\"timeout two\")\n"
        "fn timeoutTwo():\n"
        "  i = 0\n"
        "  while i < 1:\n"
        "    i = 0\n"
        "\n"
        "@it(\"normal two\")\n"
        "fn normalTwo():\n"
        "  expect(2).toEq(2)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because two tests timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("normal one"), std::string::npos)
        << "First normal test must run.\n"
        << "stdout:\n" << r.out;
    EXPECT_NE(r.out.find("timeout one"), std::string::npos)
        << "First timeout test must be reported.\n"
        << "stdout:\n" << r.out;
    EXPECT_NE(r.out.find("timeout two"), std::string::npos)
        << "Second timeout test must be reported (state cleanup after "
           "previous timeout).\n"
        << "stdout:\n" << r.out;
    EXPECT_NE(r.out.find("normal two"), std::string::npos)
        << "Trailing normal test must still run after two timeouts.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("2 passed"), std::string::npos)
        << "Both normal tests must be counted as passed.\n"
        << "stdout:\n" << r.out;
    EXPECT_NE(r.out.find("2 failed"), std::string::npos)
        << "Both timeout tests must be counted as failed.\n"
        << "stdout:\n" << r.out;
}

// #1781: @afterEach must run even when @timeout fires mid-test. Without the
// 2-phase sigsetjmp restructure, the SIGALRM siglongjmp tears past the
// inlined @afterEach and external resources (mock state, counters, file
// handles set up by @beforeEach) are never released. A subsequent test that
// observes the cleanup side-effect must see it.
TEST_F(SpecTimeoutTest, AfterEachRunsAfterTimedOutTest) {
    auto path = writeTest("after_each_after_timeout.test.ry",
        "from ry.testing import it, expect, describe, timeout, afterEach\n"
        "\n"
        "@describe(\"ae after timeout\")\n"
        "fn group():\n"
        "    counter = 0\n"
        "\n"
        "    @afterEach\n"
        "    fn cleanup():\n"
        "        counter = counter + 100\n"
        "\n"
        "    @timeout(200)\n"
        "    @it(\"body hangs\")\n"
        "    fn bodyHangs():\n"
        "        i = 0\n"
        "        while i < 1:\n"
        "            i = 0\n"
        "\n"
        "    @it(\"sees afterEach side effect\")\n"
        "    fn seesAfterEachSideEffect():\n"
        "        expect(counter).toEq(100)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because one test timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("timeout after 200ms"), std::string::npos)
        << "stdout should report the body timeout.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("sees afterEach side effect"), std::string::npos)
        << "Subsequent test must still run.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("1 passed"), std::string::npos)
        << "The trailing test must pass, proving @afterEach ran after the "
           "timed-out test and updated the observed counter.\n"
        << "stdout:\n" << r.out;
    EXPECT_NE(r.out.find("1 failed"), std::string::npos)
        << "The timeout test must be counted as failed.\n"
        << "stdout:\n" << r.out;
}

// #1781: A hanging @afterEach must NOT block subsequent tests. The AE phase
// gets its own sigsetjmp landing pad with the same @timeout(N) budget; if
// AE itself blows the budget, a secondary failure line is reported and the
// test loop continues to the next @it.
TEST_F(SpecTimeoutTest, AfterEachHangSurfacedAsSecondaryFailure) {
    auto path = writeTest("after_each_hang.test.ry",
        "from ry.testing import it, expect, describe, timeout, afterEach\n"
        "\n"
        "# group1 has a hanging @afterEach; only its @timeout-guarded @it\n"
        "# safely exercises it. group2 has no @afterEach so the trailing\n"
        "# assertion is reached.\n"
        "@describe(\"ae hang surfaces as secondary failure\")\n"
        "fn group1():\n"
        "    @afterEach\n"
        "    fn hangAE():\n"
        "        i = 0\n"
        "        while i < 1:\n"
        "            i = 0\n"
        "\n"
        "    @timeout(150)\n"
        "    @it(\"body hangs\")\n"
        "    fn bodyHangs():\n"
        "        i = 0\n"
        "        while i < 1:\n"
        "            i = 0\n"
        "\n"
        "@describe(\"trailing\")\n"
        "fn group2():\n"
        "    @it(\"runs after both hangs\")\n"
        "    fn runsAfterBothHangs():\n"
        "        expect(1).toEq(1)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s. A hanging @afterEach must NOT block the "
           "subsequent test.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because the body timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("@afterEach (timeout after 150ms)"),
              std::string::npos)
        << "stdout should surface the hung @afterEach as a secondary "
           "failure line.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("runs after both hangs"), std::string::npos)
        << "Subsequent test must still run after both body and "
           "@afterEach timed out.\n"
        << "stdout:\n" << r.out;
}

// #1781 Open question 2 coverage: when the timer fires inside @beforeEach
// (not the test body), @afterEach must still run — the body sigsetjmp covers
// the full file BE → describe BE → user fn cascade, so any timeout within
// that chain falls through to the @afterEach phase. Without this guarantee
// users would have to mirror cleanup outside @afterEach.
TEST_F(SpecTimeoutTest, AfterEachRunsAfterBeforeEachTimedOut) {
    auto path = writeTest("after_each_after_before_each_timeout.test.ry",
        "from ry.testing import it, expect, describe, timeout, "
        "beforeEach, afterEach\n"
        "\n"
        "# Module-level so the trailing observer describe can read it.\n"
        "counter = 0\n"
        "\n"
        "@describe(\"ae after beforeEach timeout\")\n"
        "fn group():\n"
        "    @beforeEach\n"
        "    fn hangBE():\n"
        "        i = 0\n"
        "        while i < 1:\n"
        "            i = 0\n"
        "\n"
        "    @afterEach\n"
        "    fn cleanup():\n"
        "        counter = counter + 100\n"
        "\n"
        "    @timeout(200)\n"
        "    @it(\"never reaches body — beforeEach hangs\")\n"
        "    fn neverReachesBody():\n"
        "        expect(true).toEq(true)\n"
        "\n"
        "@describe(\"trailing observer\")\n"
        "fn observer():\n"
        "    @it(\"sees afterEach side effect from prior describe\")\n"
        "    fn sees():\n"
        "        expect(counter).toEq(100)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because the @beforeEach phase timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("timeout after 200ms"), std::string::npos)
        << "stdout should report the timeout (body phase covers "
           "@beforeEach + test body).\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("sees afterEach side effect from prior describe"),
              std::string::npos)
        << "The trailing observer test must run.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("1 passed"), std::string::npos)
        << "The trailing test must pass — @afterEach ran after the "
           "timed-out @beforeEach and updated the counter, proving the "
           "tolerate-partial-setup rule.\n"
        << "stdout:\n" << r.out;
}

// #1781 + #1780 interaction: file-top-level @afterEach must also run after a
// timed-out test. The 2-phase sigsetjmp emits file-level and describe-level
// @afterEach symmetrically inside aeNormalBB, so this is the canary for the
// file-level layer.
TEST_F(SpecTimeoutTest, FileLevelAfterEachAlsoRunsAfterTimeout) {
    auto path = writeTest("file_after_each_after_timeout.test.ry",
        "from ry.testing import it, expect, timeout, afterEach\n"
        "\n"
        "counter = 0\n"
        "\n"
        "@afterEach\n"
        "fn fileCleanup():\n"
        "    counter = counter + 1\n"
        "\n"
        "@timeout(200)\n"
        "@it(\"body hangs\")\n"
        "fn bodyHangs():\n"
        "    i = 0\n"
        "    while i < 1:\n"
        "        i = 0\n"
        "\n"
        "@it(\"sees file-level afterEach side effect\")\n"
        "fn seesFileAE():\n"
        "    expect(counter).toEq(1)\n");

    auto r = runRyTest(path);

    ASSERT_FALSE(r.killed_by_backstop)
        << "Backstop fired — ry test did not return within "
        << kBackstopSeconds << "s.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.exit_code, 0)
        << "Expected non-zero exit because the body timed out.\n"
        << "stdout:\n" << r.out << "\nstderr:\n" << r.err;

    EXPECT_NE(r.out.find("timeout after 200ms"), std::string::npos)
        << "stdout should report the body timeout.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("sees file-level afterEach side effect"),
              std::string::npos)
        << "Subsequent test must still run.\n"
        << "stdout:\n" << r.out;

    EXPECT_NE(r.out.find("1 passed"), std::string::npos)
        << "The trailing test must pass, proving file-top-level @afterEach "
           "ran after the timed-out test.\n"
        << "stdout:\n" << r.out;
}
