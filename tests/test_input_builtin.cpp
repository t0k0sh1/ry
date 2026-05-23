#include <gtest/gtest.h>
#include <array>
#include <cerrno>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <string>
#include <utility>
#include <unistd.h>
#include <sys/wait.h>


// Run ry binary with a source file argument, piping stdin_data to the child
// process's stdin. Returns {stdout+stderr, exit_code}.
//
// Unlike runRyStdin() in test_stdin.cpp (which pipes source code to `ry -c`),
// this helper writes the Ry source to a temp file and runs `ry <file>`, so the
// child's stdin is free to carry the test's input data. This is what exercises
// the input() builtin — the Ry program reads from stdin while executing.
static std::pair<std::string, int> runRyWithStdin(const std::string &ry_source,
                                                  const std::string &stdin_data) {
    namespace fs = std::filesystem;
    auto tmp = fs::temp_directory_path() /
               ("ry_input_test_" + std::to_string(getpid()) + "_" +
                std::to_string(reinterpret_cast<uintptr_t>(&ry_source)) + ".ry");
    {
        std::ofstream ofs(tmp);
        ofs << ry_source;
    }

    int pipeIn[2];   // parent writes stdin_data → child stdin
    int pipeOut[2];  // child stdout/stderr → parent reads
    if (pipe(pipeIn) != 0) {
        fs::remove(tmp);
        return {"", -1};
    }
    if (pipe(pipeOut) != 0) {
        close(pipeIn[0]);
        close(pipeIn[1]);
        fs::remove(tmp);
        return {"", -1};
    }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipeIn[0]);
        close(pipeIn[1]);
        close(pipeOut[0]);
        close(pipeOut[1]);
        fs::remove(tmp);
        return {"", -1};
    }

    if (pid == 0) {
        close(pipeIn[1]);
        close(pipeOut[0]);
        dup2(pipeIn[0], STDIN_FILENO);
        dup2(pipeOut[1], STDOUT_FILENO);
        dup2(pipeOut[1], STDERR_FILENO);
        close(pipeIn[0]);
        close(pipeOut[1]);
        setenv("RY_ENV", "internal", 1);
        execl(RY_BINARY_PATH, RY_BINARY_PATH, tmp.c_str(), nullptr);
        _exit(127);
    }

    close(pipeIn[0]);
    close(pipeOut[1]);

    if (!stdin_data.empty()) {
        size_t off = 0;
        while (off < stdin_data.size()) {
            ssize_t w = write(pipeIn[1], stdin_data.data() + off,
                              stdin_data.size() - off);
            if (w < 0 && errno == EINTR)
                continue;
            if (w <= 0)
                break;
            off += static_cast<size_t>(w);
        }
    }
    close(pipeIn[1]);

    std::string output;
    std::array<char, 4096> buf;
    ssize_t n;
    while ((n = read(pipeOut[0], buf.data(), buf.size())) > 0) {
        output.append(buf.data(), static_cast<size_t>(n));
    }
    close(pipeOut[0]);

    int status = 0;
    int exit_code = -1;
    for (;;) {
        pid_t wp = waitpid(pid, &status, 0);
        if (wp >= 0) {
            exit_code = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
            break;
        }
        if (errno != EINTR)
            break;
    }

    fs::remove(tmp);
    return {output, exit_code};
}

// Common driver: prints "line:<s>" / "EOF" / "ERR:<msg>" for one input() call.
static const char *kInputDriver_PrintLine = R"(case input():
    Ok(opt):
        case opt:
            Some(s): print("line:" + s)
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)";

TEST(InputBuiltin, NoArg_BasicLine) {
    auto [out, rc] = runRyWithStdin(kInputDriver_PrintLine, "hello\n");
    EXPECT_EQ(out, "line:hello\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, NoArg_EOFReturnsOkNone) {
    auto [out, rc] = runRyWithStdin(kInputDriver_PrintLine, "");
    EXPECT_EQ(out, "EOF\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, NoArg_EmptyLineReturnsOkSomeEmpty) {
    // Core breaking-change guarantee: an empty input line is Ok(Some("")),
    // distinct from EOF which is Ok(None).
    auto [out, rc] = runRyWithStdin(R"(case input():
    Ok(opt):
        case opt:
            Some(s): print(len(s))
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "\n");
    EXPECT_EQ(out, "0\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, NoArg_TrailingNewlineStripped) {
    auto [out, rc] = runRyWithStdin(R"(case input():
    Ok(opt):
        case opt:
            Some(s): print(len(s))
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "foo\n");
    EXPECT_EQ(out, "3\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, NoArg_NoTrailingNewline) {
    auto [out, rc] = runRyWithStdin(R"(case input():
    Ok(opt):
        case opt:
            Some(s):
                print(s)
                print(len(s))
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "bar");
    EXPECT_EQ(out, "bar\n3\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, Prompt_WritesToStdoutBeforeRead) {
    auto [out, rc] = runRyWithStdin(R"(case input("Q: "):
    Ok(opt):
        case opt:
            Some(s): print(s)
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "answer\n");
    EXPECT_EQ(out, "Q: answer\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, Prompt_NoTrailingNewlineOnPrompt) {
    auto [out, rc] = runRyWithStdin(R"(case input("prefix"):
    Ok(opt):
        case opt:
            Some(_s): print("done")
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "x\n");
    // The prompt "prefix" must not be followed by a newline before "done".
    EXPECT_EQ(out, "prefixdone\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, Prompt_EmptyPromptBehavesLikeNoArg) {
    auto [out, rc] = runRyWithStdin(R"(case input(""):
    Ok(opt):
        case opt:
            Some(s): print(s)
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "zzz\n");
    EXPECT_EQ(out, "zzz\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, Prompt_EOFReturnsOkNone) {
    // EOF distinction works for the prompt variant too.
    auto [out, rc] = runRyWithStdin(R"(case input("> "):
    Ok(opt):
        case opt:
            Some(s): print("line:" + s)
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)",
        "");
    EXPECT_EQ(out, "> EOF\n");
    EXPECT_EQ(rc, 0);
}

TEST(InputBuiltin, Reject_TooManyArgs) {
    // input() accepts at most 1 argument; 2+ must fail at compile time.
    auto [out, rc] = runRyWithStdin(
        "print(input(\"a\", \"b\"))\n",
        "");
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("input() takes 0 or 1 arguments"), std::string::npos)
        << "stderr=" << out;
}

TEST(InputBuiltin, Reject_NonStringPrompt) {
    // input(prompt) requires str; passing int must fail at compile time.
    auto [out, rc] = runRyWithStdin(
        "print(input(42))\n",
        "");
    EXPECT_NE(rc, 0);
    EXPECT_NE(out.find("input() prompt must be str"), std::string::npos)
        << "stderr=" << out;
}
