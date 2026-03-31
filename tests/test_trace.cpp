#include <gtest/gtest.h>
#include <array>
#include <filesystem>
#include <fstream>
#include <poll.h>
#include <string>
#include <vector>
#include <unistd.h>
#include <sys/wait.h>

namespace fs = std::filesystem;

struct TraceRunResult {
    std::string out;
    std::string err;
    int exit_code;
};

static TraceRunResult runRy(const std::vector<std::string> &args,
                            const std::string &cwd = "") {
    int pipeOut[2];
    int pipeErr[2];
    if (pipe(pipeOut) != 0) return {"", "", -1};
    if (pipe(pipeErr) != 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        return {"", "", -1};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeOut[0]);
        close(pipeOut[1]);
        close(pipeErr[0]);
        close(pipeErr[1]);
        return {"", "", -1};
    }

    if (pid == 0) {
        close(pipeOut[0]);
        close(pipeErr[0]);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeErr[1], STDERR_FILENO);
        close(pipeOut[1]);
        close(pipeErr[1]);
        close(STDIN_FILENO);

        if (!cwd.empty() && chdir(cwd.c_str()) != 0) _exit(126);
        setenv("RY_ENV", "internal", 1);

        std::vector<char *> argv;
        argv.push_back(const_cast<char *>("ry"));
        for (const auto &arg : args)
            argv.push_back(const_cast<char *>(arg.c_str()));
        argv.push_back(nullptr);

        execv(RY_BINARY_PATH, argv.data());
        _exit(127);
    }

    close(pipeOut[1]);
    close(pipeErr[1]);

    // Drain both pipes concurrently via poll to avoid deadlock when
    // stderr (trace output) fills the pipe buffer while we block on stdout.
    std::string out, err;
    struct pollfd fds[2] = {
        {pipeOut[0], POLLIN, 0},
        {pipeErr[0], POLLIN, 0},
    };
    int open_fds = 2;
    std::array<char, 4096> buf;
    while (open_fds > 0) {
        int ret = poll(fds, 2, -1);
        if (ret <= 0) break;
        for (int i = 0; i < 2; ++i) {
            if (fds[i].fd < 0) continue;
            if (fds[i].revents & (POLLIN | POLLHUP)) {
                ssize_t n = read(fds[i].fd, buf.data(), buf.size());
                if (n > 0) {
                    (i == 0 ? out : err).append(buf.data(), n);
                } else {
                    close(fds[i].fd);
                    fds[i].fd = -1;
                    --open_fds;
                }
            }
        }
    }

    int status = 0;
    waitpid(pid, &status, 0);
    int exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
    return {out, err, exit_code};
}

class TraceModeTest : public ::testing::Test {
protected:
    fs::path tmp_dir_;

    void SetUp() override {
        tmp_dir_ = fs::temp_directory_path() /
                   ("ry_trace_test_" + std::to_string(getpid()));
        fs::create_directories(tmp_dir_);
    }

    void TearDown() override {
        std::error_code ec;
        fs::remove_all(tmp_dir_, ec);
    }

    fs::path writeFile(const std::string &name, const std::string &content) {
        fs::path path = tmp_dir_ / name;
        std::ofstream out(path);
        out << content;
        return path;
    }
};

TEST_F(TraceModeTest, HelpMentionsTraceFlags) {
    auto result = runRy({"--help"});
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_NE(result.out.find("--trace"), std::string::npos);
    EXPECT_NE(result.out.find("--trace-out=PATH"), std::string::npos);
}

TEST_F(TraceModeTest, TraceGoesToStderrAndProgramOutputStaysOnStdout) {
    auto script = writeFile(
        "trace_sample.ry",
        "function pick(x: int) -> int:\n"
        "    if x > 0:\n"
        "        return x\n"
        "    return 0\n"
        "\n"
        "print(pick(1))\n");

    auto result = runRy({"--trace", script.string()});
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_EQ(result.out, "1\n");
    EXPECT_NE(result.err.find("\"event\":\"session.start\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"parse.start\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"codegen.done\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"jit.ready\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"exec.start\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"call.enter\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"branch.if\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"return\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"call.exit\""), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"exec.end\""), std::string::npos);
}

TEST_F(TraceModeTest, TraceCanBeRedirectedToFile) {
    auto script = writeFile("trace_file.ry", "print(\"ok\")\n");
    fs::path tracePath = tmp_dir_ / "trace.jsonl";

    auto result = runRy({"--trace", "--trace-out=" + tracePath.string(), script.string()});
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_EQ(result.out, "ok\n");
    EXPECT_TRUE(result.err.empty());

    std::ifstream in(tracePath);
    std::string trace((std::istreambuf_iterator<char>(in)),
                      std::istreambuf_iterator<char>());
    EXPECT_NE(trace.find("\"event\":\"session.start\""), std::string::npos);
    EXPECT_NE(trace.find("\"event\":\"exec.end\""), std::string::npos);
}

TEST_F(TraceModeTest, TraceWithParallelTestsFallsBackToSequential) {
    fs::create_directories(tmp_dir_ / "tests");
    std::ofstream pkg(tmp_dir_ / "package.toml");
    pkg << "[project]\nname = \"trace\"\nversion = \"0.1.0\"\nentry = \"main.ry\"\n";
    std::ofstream mainFile(tmp_dir_ / "main.ry");
    mainFile << "print(\"main\")\n";
    std::ofstream testFile(tmp_dir_ / "tests/sample.test.ry");
    testFile << "describe(\"x\", function():\n"
                "  it(\"y\", function():\n"
                "    expect(1).to_eq(1)\n"
                "  )\n"
                ")\n";

    auto result = runRy({"test", "--parallel", "--trace", "tests"}, tmp_dir_.string());
    EXPECT_EQ(result.exit_code, 0);
    EXPECT_NE(result.err.find("falling back to sequential"), std::string::npos);
    EXPECT_NE(result.err.find("\"event\":\"exec.start\""), std::string::npos);
}
