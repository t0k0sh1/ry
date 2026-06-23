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
// process's stdin. Returns {stdout+stderr, exit_code}. Mirrors the helper in
// tests/test_input_builtin.cpp; copied locally because helper extraction is
// out of scope for #1850.
static std::pair<std::string, int> runRyWithStdin(const std::string &ry_source,
                                                  const std::string &stdin_data) {
    namespace fs = std::filesystem;
    auto tmp = fs::temp_directory_path() /
               ("ry_readline_test_" + std::to_string(getpid()) + "_" +
                std::to_string(reinterpret_cast<uintptr_t>(&ry_source)) + ".ry");
    {
        std::ofstream ofs(tmp);
        ofs << ry_source;
    }

    int pipeIn[2];
    int pipeOut[2];
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
        // argv[0] must be the full path: find_share_dir uses
        // fs::path(exe_path).parent_path(), and on Linux fs::canonical("") fails
        // (unlike macOS), breaking stdlib resolution under RY_ENV=internal where
        // the global ~/.ry/share fallback is skipped. See PR #1869 / issue
        // discussion.
        execl(RY_BINARY_PATH, RY_BINARY_PATH, "run", tmp.c_str(), nullptr);
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

// Common driver: prints "line:<s>" / "EOF" / "ERR:<msg>" for one readLine call.
static const char *driver_print_line = R"(from ry.io import readLine
case readLine():
    Ok(opt):
        case opt:
            Some(s): print("line:" + s)
            None: print("EOF")
    Err(e): print("ERR:" + e.message)
)";

TEST(ReadLineBuiltin, EOFReturnsOkNone) {
    auto [out, rc] = runRyWithStdin(driver_print_line, "");
    EXPECT_EQ(out, "EOF\n");
    EXPECT_EQ(rc, 0);
}

TEST(ReadLineBuiltin, EmptyLineReturnsOkSomeEmpty) {
    auto [out, rc] = runRyWithStdin(R"(from ry.io import readLine
case readLine():
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

TEST(ReadLineBuiltin, NormalLineReturnsOkSome) {
    auto [out, rc] = runRyWithStdin(driver_print_line, "hello\n");
    EXPECT_EQ(out, "line:hello\n");
    EXPECT_EQ(rc, 0);
}

TEST(ReadLineBuiltin, MultiLineReturnsSomeSomeNone) {
    auto [out, rc] = runRyWithStdin(R"(from ry.io import readLine
fn one():
    case readLine():
        Ok(opt):
            case opt:
                Some(s): print("line:" + s)
                None: print("EOF")
        Err(e): print("ERR:" + e.message)
one()
one()
one()
)",
        "a\nb\n");
    EXPECT_EQ(out, "line:a\nline:b\nEOF\n");
    EXPECT_EQ(rc, 0);
}

TEST(ReadLineBuiltin, MissingTrailingNewlineReturnsSomeThenNone) {
    auto [out, rc] = runRyWithStdin(R"(from ry.io import readLine
fn one():
    case readLine():
        Ok(opt):
            case opt:
                Some(s): print("line:" + s)
                None: print("EOF")
        Err(e): print("ERR:" + e.message)
one()
one()
)",
        "bar");
    EXPECT_EQ(out, "line:bar\nEOF\n");
    EXPECT_EQ(rc, 0);
}
