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

class RyNamespaceWarningTest : public ::testing::Test {
protected:
    fs::path tmpDir_;

    void SetUp() override {
        // Place fixtures inside the build tree so the script's ancestor chain
        // reaches the repo's package.toml and find_share_dir resolves the
        // _dev_stdlib override (matching the test_deprecated_warnings.cpp
        // pattern).
        tmpDir_ = fs::path(RY_BINARY_PATH).parent_path() / "ry_namespace_warning_test";
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

TEST_F(RyNamespaceWarningTest, UserDefinedRyDirectoryEmitsShadowWarning) {
    // The user has a local `ry/` directory in the same directory as their
    // script. Even though the loader resolves `ry.math` against the stdlib
    // search paths (the intercept never consults the referrer_dir for `ry/*`),
    // the presence of a local `ry/` is a future reservation conflict and
    // must be surfaced as a warning on stderr.
    fs::create_directories(tmpDir_ / "ry");

    auto p = writeScript("uses_ry_math.ry",
        "from ry.math import sqrt\n"
        "print(sqrt(9.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "3.0\n");
    EXPECT_NE(r.err.find("warning"), std::string::npos)
        << "stderr should contain a warning: " << r.err;
    EXPECT_NE(r.err.find("ry"), std::string::npos)
        << "warning should mention 'ry': " << r.err;
    EXPECT_NE(r.err.find("reserved"), std::string::npos)
        << "warning should mention 'reserved': " << r.err;
}

TEST_F(RyNamespaceWarningTest, UserDefinedRyFileEmitsShadowWarning) {
    // A `ry.ry` file (single-file module form) also conflicts and must
    // warn. Same emission, different probe target.
    std::ofstream(tmpDir_ / "ry.ry") << "# placeholder for user-defined ry module\n";

    auto p = writeScript("uses_ry_lang.ry",
        "from ry.lang import map\n"
        "print(map([1, 2, 3], (x: int) => x * 10))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "[10, 20, 30]\n");
    EXPECT_NE(r.err.find("warning"), std::string::npos)
        << "stderr should contain a warning: " << r.err;
    EXPECT_NE(r.err.find("reserved"), std::string::npos)
        << "warning should mention 'reserved': " << r.err;
}

TEST_F(RyNamespaceWarningTest, NoShadowEmitsNoWarning) {
    // Negative case: a script that uses ry.* but has no local `ry/` shadow
    // must not emit any namespace warning.
    auto p = writeScript("no_shadow.ry",
        "from ry.math import sqrt\n"
        "print(sqrt(16.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "4.0\n");
    EXPECT_EQ(r.err.find("reserved"), std::string::npos)
        << "stderr should not contain a reserved-namespace warning: " << r.err;
}

TEST_F(RyNamespaceWarningTest, ShadowWarningDeduplicatedAcrossImports) {
    // Multiple ry.* imports in the same file must produce at most one
    // shadowing warning, matching the existing @deprecated dedup behavior.
    fs::create_directories(tmpDir_ / "ry");

    auto p = writeScript("multi_ry.ry",
        "from ry.math import sqrt, PI\n"
        "from ry.lang import map\n"
        "print(sqrt(25.0))\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_EQ(r.out, "5.0\n");
    size_t count = 0;
    size_t pos = 0;
    while ((pos = r.err.find("reserved", pos)) != std::string::npos) {
        ++count;
        pos += sizeof("reserved") - 1;
    }
    EXPECT_EQ(count, 1u) << "expected exactly one warning, stderr was: " << r.err;
}
