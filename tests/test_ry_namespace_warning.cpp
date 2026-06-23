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

// #2297 — bare `import ry` / `from ry import X` も予約 namespace の対象。
// shadow 警告は出すが、bare 形式は ry.<module> を要求する rejection で失敗させる。
// 警告と rejection の双方が "reserved" を含むため、テストは distinct fragment で
// 区別する: 警告 → "is ignored" / rejection → "use 'ry.<module>'".

TEST_F(RyNamespaceWarningTest, BareImportRyEmitsWarningAndRejects) {
    // local `ry/` shadow + bare `import ry` → shadow 警告 + bare reject を両方出す。
    fs::create_directories(tmpDir_ / "ry");

    auto p = writeScript("bare_import_ry.ry", "import ry\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "bare 'import ry' must reject, stderr was: " << r.err;
    EXPECT_NE(r.err.find("is ignored"), std::string::npos)
        << "stderr should contain shadow warning ('is ignored'): " << r.err;
    EXPECT_NE(r.err.find("use 'ry.<module>'"), std::string::npos)
        << "stderr should contain rejection hint ('use 'ry.<module>''): " << r.err;
}

TEST_F(RyNamespaceWarningTest, BareFromRyImportEmitsWarningAndRejects) {
    // local `ry/helper.ry` shadow + `from ry import helper` → bare 形式の reject。
    fs::create_directories(tmpDir_ / "ry");
    std::ofstream(tmpDir_ / "ry" / "helper.ry")
        << "@public\nfn helper() -> int:\n    return 42\n";

    auto p = writeScript("bare_from_ry.ry", "from ry import helper\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "bare 'from ry import' must reject, stderr was: " << r.err;
    EXPECT_NE(r.err.find("is ignored"), std::string::npos)
        << "stderr should contain shadow warning ('is ignored'): " << r.err;
    EXPECT_NE(r.err.find("use 'ry.<module>'"), std::string::npos)
        << "stderr should contain rejection hint ('use 'ry.<module>''): " << r.err;
}

TEST_F(RyNamespaceWarningTest, BareImportRyWithoutShadowRejects) {
    // shadow なしでも bare `import ry` は reject。警告は出ない。
    auto p = writeScript("bare_no_shadow.ry", "import ry\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "bare 'import ry' must reject, stderr was: " << r.err;
    EXPECT_EQ(r.err.find("is ignored"), std::string::npos)
        << "no shadow → no 'is ignored' warning, stderr was: " << r.err;
    EXPECT_NE(r.err.find("use 'ry.<module>'"), std::string::npos)
        << "stderr should contain rejection hint ('use 'ry.<module>''): " << r.err;
}

TEST_F(RyNamespaceWarningTest, InternalStdlibModuleRejectedWithHint) {
    // `from ry.builtins import print` のような internal stdlib module は許可リスト外
    // として reject される。bare `from builtins import print` は影響を受けない (今回の
    // 対象外なのでテストもしない)。
    auto p = writeScript("internal_module.ry",
        "from ry.builtins import print\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "internal 'ry.builtins' must reject, stderr was: " << r.err;
    EXPECT_NE(r.err.find("'ry.builtins' is not a public stdlib module"),
              std::string::npos)
        << "error should name the module + 'not a public stdlib module': " << r.err;
    EXPECT_NE(r.err.find("ry.lang"), std::string::npos)
        << "error should list 'ry.lang' in available modules: " << r.err;
    EXPECT_NE(r.err.find("ry.math"), std::string::npos)
        << "error should list 'ry.math' in available modules: " << r.err;
    // #2309: net / json5 を allowlist に追加した後、エラーリストの 13 件表示
    // (`ry.base64, ry.net, ry.json5` を含む) を回帰でロックする。
    EXPECT_NE(r.err.find("ry.net"), std::string::npos)
        << "error should list 'ry.net' in available modules (#2309): " << r.err;
    EXPECT_NE(r.err.find("ry.json5"), std::string::npos)
        << "error should list 'ry.json5' in available modules (#2309): " << r.err;
}

TEST_F(RyNamespaceWarningTest, BogusRyModuleErrorUsesDotSpelling) {
    // ユーザが書いた dot spelling (`ry.bogus`) で reject されるべき。loader 内部の
    // slash 表現 (`ry/bogus`) が user-facing error にリークしてはいけない。
    auto p = writeScript("bogus_ry.ry",
        "from ry.bogus import x\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "'ry.bogus' must reject, stderr was: " << r.err;
    EXPECT_NE(r.err.find("'ry.bogus'"), std::string::npos)
        << "error should mention 'ry.bogus' (dot spelling): " << r.err;
    EXPECT_EQ(r.err.find("ry/bogus"), std::string::npos)
        << "error must NOT use slash spelling 'ry/bogus': " << r.err;
}

TEST_F(RyNamespaceWarningTest, NestedRyPathRejectedAsNotPublic) {
    // public surface は full enumeration なので、`ry.<public>.<deeper>` のような
    // ネスト import も "not a public stdlib module" として早期 reject される
    // (たとえ `share/std/math/internal.ry` が将来追加されても resolve loop に
    // 落とさず、policy gate でブロックする)。
    auto p = writeScript("nested_ry.ry",
        "from ry.math.internal import x\n");

    auto r = runRy({"run", p.string().c_str()});
    EXPECT_NE(r.exit_code, 0) << "nested ry.* path must reject, stderr was: " << r.err;
    EXPECT_NE(r.err.find("'ry.math.internal' is not a public stdlib module"),
              std::string::npos)
        << "error should explicitly state the nested path is not public: " << r.err;
    EXPECT_EQ(r.err.find("ry/math/internal"), std::string::npos)
        << "error must NOT use slash spelling: " << r.err;
}
