#include <gtest/gtest.h>
#include <array>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <string>
#include <utility>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

namespace {

struct RunResult {
    std::string out;
    std::string err;
    int exit_code;
};

RunResult runRyDocs(std::initializer_list<const char *> args) {
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
        argv.push_back("docs");
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

} // namespace

TEST(CmdDocs, HelpShowsUsage) {
    auto r = runRyDocs({"--help"});
    EXPECT_EQ(r.exit_code, 0);
    EXPECT_NE(r.out.find("Usage: ry docs"), std::string::npos);
}

TEST(CmdDocs, UnsupportedFormatRejected) {
    auto r = runRyDocs({"--format", "xml"});
    EXPECT_NE(r.exit_code, 0);
    EXPECT_NE(r.err.find("unsupported format"), std::string::npos);
}

class CmdDocsTest : public ::testing::Test {
protected:
    fs::path tmp_dir_;

    void SetUp() override {
        const auto *info = ::testing::UnitTest::GetInstance()->current_test_info();
        std::string suffix = std::to_string(getpid());
        suffix += "_";
        suffix += info->name();
        tmp_dir_ = fs::temp_directory_path() / ("ry_cmd_docs_test_" + suffix);
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
        fs::create_directories(tmp_dir_);
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
    }

    void writeFile(const std::string &rel_path, const std::string &content) {
        const fs::path abs = tmp_dir_ / rel_path;
        fs::create_directories(abs.parent_path());
        std::ofstream ofs(abs);
        ofs << content;
    }

    std::string readFile(const std::string &rel_path) {
        std::ifstream f(tmp_dir_ / rel_path);
        if (!f) return {};
        std::string content((std::istreambuf_iterator<char>(f)),
                            std::istreambuf_iterator<char>{});
        return content;
    }

    bool pathExists(const std::string &rel_path) {
        return fs::exists(tmp_dir_ / rel_path);
    }

    void writePackageToml(const std::string &extra = "") {
        writeFile("package.toml",
                  std::string("[project]\nname = \"docs_test\"\nversion = \"0.0.1\"\n"
                              "entry = \"src/main.ry\"\n\n"
                              "[paths]\nsrc = \"src\"\n") + extra);
    }

    std::pair<std::string, int> runRyDocsInDir(
            const std::vector<const char *> &extra_args = {}) {
        int pipeOut[2];
        if (pipe(pipeOut) != 0) return {"", -1};

        pid_t pid = fork();
        if (pid < 0) {
            close(pipeOut[0]);
            close(pipeOut[1]);
            return {"", -1};
        }

        if (pid == 0) {
            close(pipeOut[0]);
            dup2(pipeOut[1], STDOUT_FILENO);
            dup2(pipeOut[1], STDERR_FILENO);
            close(pipeOut[1]);
            close(STDIN_FILENO);
            if (chdir(tmp_dir_.c_str()) != 0) _exit(126);
            setenv("RY_ENV", "internal", 1);

            std::vector<const char *> argv;
            argv.push_back(RY_BINARY_PATH);
            argv.push_back("docs");
            for (auto a : extra_args) argv.push_back(a);
            argv.push_back(nullptr);

            execv(RY_BINARY_PATH, const_cast<char *const *>(argv.data()));
            _exit(127);
        }

        close(pipeOut[1]);

        std::string output;
        std::array<char, 4096> buf;
        ssize_t n;
        while ((n = read(pipeOut[0], buf.data(), buf.size())) > 0) {
            output.append(buf.data(), static_cast<size_t>(n));
        }
        close(pipeOut[0]);

        int status;
        waitpid(pid, &status, 0);
        int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
        return {output, exit_code};
    }
};

TEST_F(CmdDocsTest, GeneratesIndexAndModulePages) {
    writePackageToml();
    writeFile("src/hello.ry",
              "@public @doc(\"Greets the world.\")\n"
              "fn hello() -> str:\n"
              "  return \"hi\"\n");

    auto [out, rc] = runRyDocsInDir();
    EXPECT_EQ(rc, 0) << out;

    EXPECT_TRUE(pathExists("docs/api/index.html"));
    EXPECT_TRUE(pathExists("docs/api/modules/hello.html"));

    auto index = readFile("docs/api/index.html");
    EXPECT_NE(index.find("hello"), std::string::npos);
    EXPECT_NE(index.find("modules/hello.html"), std::string::npos);

    auto mod = readFile("docs/api/modules/hello.html");
    EXPECT_NE(mod.find("fn hello"), std::string::npos);
    EXPECT_NE(mod.find("Greets the world"), std::string::npos);
    EXPECT_NE(mod.find("data-kind=\"fn\""), std::string::npos);
}

TEST_F(CmdDocsTest, EmitsDocsJsonByDefault) {
    writePackageToml();
    writeFile("src/hello.ry",
              "@public @doc(\"Greets the world.\")\n"
              "fn hello() -> str:\n"
              "  return \"hi\"\n");

    auto [out, rc] = runRyDocsInDir();
    EXPECT_EQ(rc, 0) << out;
    EXPECT_TRUE(pathExists("docs/api/docs.json"));

    auto json = readFile("docs/api/docs.json");
    EXPECT_NE(json.find("\"ry_version\""), std::string::npos);
    EXPECT_NE(json.find("\"modules\""), std::string::npos);
    EXPECT_NE(json.find("\"name\": \"hello\""), std::string::npos);
    EXPECT_NE(json.find("\"kind\": \"fn\""), std::string::npos);
    EXPECT_NE(json.find("\"signature\""), std::string::npos);
    EXPECT_NE(json.find("Greets the world"), std::string::npos);
    EXPECT_NE(json.find("\"is_public\": true"), std::string::npos);
    EXPECT_NE(json.find("\"path\": \"src/hello.ry\""), std::string::npos);
}

TEST_F(CmdDocsTest, DeterministicAcrossRuns) {
    writePackageToml();
    writeFile("src/aaa.ry",
              "@public @doc(\"alpha\")\nfn alpha() -> int:\n  return 1\n\n"
              "@public @doc(\"beta\")\nfn beta() -> int:\n  return 2\n");
    writeFile("src/zzz.ry",
              "@public @doc(\"zeta\")\nfn zeta() -> int:\n  return 3\n");

    auto [out1, rc1] = runRyDocsInDir({"--out", "out1"});
    EXPECT_EQ(rc1, 0) << out1;
    auto [out2, rc2] = runRyDocsInDir({"--out", "out2"});
    EXPECT_EQ(rc2, 0) << out2;

    EXPECT_EQ(readFile("out1/index.html"), readFile("out2/index.html"));
    EXPECT_EQ(readFile("out1/modules/aaa.html"), readFile("out2/modules/aaa.html"));
    EXPECT_EQ(readFile("out1/modules/zzz.html"), readFile("out2/modules/zzz.html"));
    EXPECT_EQ(readFile("out1/docs.json"), readFile("out2/docs.json"));

    auto json = readFile("out1/docs.json");
    EXPECT_LT(json.find("\"name\": \"aaa\""), json.find("\"name\": \"zzz\""));
    EXPECT_LT(json.find("\"name\": \"alpha\""), json.find("\"name\": \"beta\""));
}

TEST_F(CmdDocsTest, CleanRequiresManifest) {
    writePackageToml();
    writeFile("src/hello.ry", "@public fn hello() -> str:\n  return \"hi\"\n");

    auto [out, rc] = runRyDocsInDir({"--clean"});
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find(".ry-docs-manifest"), std::string::npos);
}

TEST_F(CmdDocsTest, CleanRemovesTrackedFilesAndKeepsOthers) {
    writePackageToml();
    writeFile("src/hello.ry", "@public fn hello() -> str:\n  return \"hi\"\n");

    auto [gen_out, gen_rc] = runRyDocsInDir();
    ASSERT_EQ(gen_rc, 0) << gen_out;
    ASSERT_TRUE(pathExists("docs/api/index.html"));

    writeFile("docs/api/hand_made.html", "<html>handcrafted</html>");

    auto [clean_out, clean_rc] = runRyDocsInDir({"--clean"});
    EXPECT_EQ(clean_rc, 0) << clean_out;

    EXPECT_FALSE(pathExists("docs/api/index.html"));
    EXPECT_FALSE(pathExists("docs/api/modules/hello.html"));
    EXPECT_FALSE(pathExists("docs/api/docs.json"));
    EXPECT_FALSE(pathExists("docs/api/.ry-docs-manifest"));
    EXPECT_FALSE(pathExists("docs/api/modules"));
    EXPECT_TRUE(pathExists("docs/api/hand_made.html"));
    EXPECT_EQ(readFile("docs/api/hand_made.html"), "<html>handcrafted</html>");
}

TEST_F(CmdDocsTest, RefusesOverwriteUntrackedFile) {
    writePackageToml();
    writeFile("src/hello.ry", "@public fn hello() -> str:\n  return \"hi\"\n");
    writeFile("docs/api/index.html", "<html>handwritten</html>");

    auto [out, rc] = runRyDocsInDir();
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("refusing to overwrite"), std::string::npos);

    EXPECT_EQ(readFile("docs/api/index.html"), "<html>handwritten</html>");
}

TEST_F(CmdDocsTest, SecondRunReusesManifestAndOverwrites) {
    writePackageToml();
    writeFile("src/hello.ry", "@public fn hello() -> str:\n  return \"hi\"\n");

    auto [first_out, first_rc] = runRyDocsInDir();
    ASSERT_EQ(first_rc, 0) << first_out;
    ASSERT_TRUE(pathExists("docs/api/.ry-docs-manifest"));

    auto [second_out, second_rc] = runRyDocsInDir();
    EXPECT_EQ(second_rc, 0) << second_out;
    EXPECT_TRUE(pathExists("docs/api/index.html"));
    EXPECT_TRUE(pathExists("docs/api/.ry-docs-manifest"));
}

TEST_F(CmdDocsTest, ExcludesPrivateByDefault) {
    writePackageToml();
    writeFile("src/mixed.ry",
              "@public @doc(\"visible api\")\n"
              "fn pubFn() -> int:\n  return 1\n\n"
              "@doc(\"hidden internal\")\n"
              "fn privFn() -> int:\n  return 2\n");

    auto [out, rc] = runRyDocsInDir();
    EXPECT_EQ(rc, 0) << out;

    auto mod = readFile("docs/api/modules/mixed.html");
    EXPECT_NE(mod.find("pubFn"), std::string::npos);
    EXPECT_EQ(mod.find("privFn"), std::string::npos);
    EXPECT_NE(mod.find("visible api"), std::string::npos);
    EXPECT_EQ(mod.find("hidden internal"), std::string::npos);
}

TEST_F(CmdDocsTest, MarksVisibilityInHtml) {
    writePackageToml();
    writeFile("src/mixed.ry",
              "@public fn pubFn() -> int:\n  return 1\n\n"
              "fn privFn() -> int:\n  return 2\n");

    auto [out, rc] = runRyDocsInDir({"--include-private"});
    EXPECT_EQ(rc, 0) << out;

    auto mod = readFile("docs/api/modules/mixed.html");
    auto pub_idx = mod.find("data-name=\"pubFn\"");
    auto priv_idx = mod.find("data-name=\"privFn\"");
    ASSERT_NE(pub_idx, std::string::npos);
    ASSERT_NE(priv_idx, std::string::npos);
    EXPECT_NE(mod.find("data-visibility=\"public\"", pub_idx), std::string::npos);
    EXPECT_NE(mod.find("data-visibility=\"private\"", priv_idx), std::string::npos);
}

TEST_F(CmdDocsTest, RegenerationPurgesOrphanedPages) {
    writePackageToml();
    writeFile("src/hello.ry", "@public fn hello() -> str:\n  return \"hi\"\n");

    auto [first_out, first_rc] = runRyDocsInDir();
    ASSERT_EQ(first_rc, 0) << first_out;
    ASSERT_TRUE(pathExists("docs/api/modules/hello.html"));

    std::error_code ec;
    fs::remove(tmp_dir_ / "src/hello.ry", ec);
    writeFile("src/farewell.ry", "@public fn farewell() -> str:\n  return \"bye\"\n");

    auto [second_out, second_rc] = runRyDocsInDir();
    EXPECT_EQ(second_rc, 0) << second_out;
    EXPECT_FALSE(pathExists("docs/api/modules/hello.html"));
    EXPECT_TRUE(pathExists("docs/api/modules/farewell.html"));
}

TEST_F(CmdDocsTest, IncludesPrivateWithFlag) {
    writePackageToml();
    writeFile("src/mixed.ry",
              "@public @doc(\"visible api\")\n"
              "fn pubFn() -> int:\n  return 1\n\n"
              "@doc(\"hidden internal\")\n"
              "fn privFn() -> int:\n  return 2\n");

    auto [out, rc] = runRyDocsInDir({"--include-private"});
    EXPECT_EQ(rc, 0) << out;

    auto mod = readFile("docs/api/modules/mixed.html");
    EXPECT_NE(mod.find("pubFn"), std::string::npos);
    EXPECT_NE(mod.find("privFn"), std::string::npos);
    EXPECT_NE(mod.find("visible api"), std::string::npos);
    EXPECT_NE(mod.find("hidden internal"), std::string::npos);
}

TEST_F(CmdDocsTest, EmitsRecordFields) {
    writePackageToml();
    writeFile("src/shapes.ry",
              "@public @doc(\"A 2D point.\")\n"
              "record Point:\n"
              "  @doc(\"Horizontal coordinate.\")\n"
              "  x: int\n"
              "  @doc(\"Vertical coordinate.\")\n"
              "  y: int\n");

    auto [out, rc] = runRyDocsInDir();
    EXPECT_EQ(rc, 0) << out;

    auto mod = readFile("docs/api/modules/shapes.html");
    EXPECT_NE(mod.find("data-kind=\"record\""), std::string::npos);
    EXPECT_NE(mod.find("data-kind=\"field\""), std::string::npos);
    EXPECT_NE(mod.find("data-name=\"Point.x\""), std::string::npos);
    EXPECT_NE(mod.find("data-name=\"Point.y\""), std::string::npos);
    EXPECT_NE(mod.find("Horizontal coordinate"), std::string::npos);
    EXPECT_NE(mod.find("Vertical coordinate"), std::string::npos);

    auto json = readFile("docs/api/docs.json");
    EXPECT_NE(json.find("\"kind\": \"field\""), std::string::npos);
    EXPECT_NE(json.find("\"name\": \"Point.x\""), std::string::npos);
}

TEST_F(CmdDocsTest, EmitJsonFlagIsNoOp) {
    writePackageToml();
    writeFile("src/hello.ry",
              "@public fn hello() -> str:\n"
              "  return \"hi\"\n");

    auto [out1, rc1] = runRyDocsInDir();
    EXPECT_EQ(rc1, 0) << out1;
    auto json_default = readFile("docs/api/docs.json");
    auto html_default = readFile("docs/api/index.html");

    auto [out2, rc2] = runRyDocsInDir({"--emit-json"});
    EXPECT_EQ(rc2, 0) << out2;
    auto json_with_flag = readFile("docs/api/docs.json");
    auto html_with_flag = readFile("docs/api/index.html");

    EXPECT_EQ(json_default, json_with_flag);
    EXPECT_EQ(html_default, html_with_flag);
}
