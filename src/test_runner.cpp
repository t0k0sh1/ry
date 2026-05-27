#include "ry/test_runner.hpp"
#include "ry/jit_runner.hpp"
#include "ry/self_update.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#ifdef __APPLE__
#include <crt_externs.h>
#define RY_ENVIRON (*_NSGetEnviron())
#else
extern "C" { extern char **environ; }
#define RY_ENVIRON environ
#endif
#include <algorithm>
#include <cerrno>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <deque>
#include <filesystem>
#include <mutex>
#include <spawn.h>
#include <sys/wait.h>
#include <thread>
#include <unistd.h>
#include <unordered_set>
#include <vector>
#include <llvm/Support/raw_ostream.h>

namespace fs = std::filesystem;

namespace ry {

std::vector<std::string> findTestFiles(const std::string &root_dir) {
    static const std::unordered_set<std::string> skip_dirs = {
        ".git", "build", "node_modules"
    };
    std::vector<std::string> files;
    std::error_code ec;
    auto it = fs::recursive_directory_iterator(
        root_dir, fs::directory_options::skip_permission_denied, ec);
    if (ec) return files;
    for (auto end = fs::recursive_directory_iterator(); it != end; it.increment(ec)) {
        if (ec) { ec.clear(); continue; }
        auto &entry = *it;
        if (entry.is_directory() && skip_dirs.count(entry.path().filename().string())) {
            it.disable_recursion_pending();
            continue;
        }
        if (!entry.is_regular_file()) continue;
        auto path = entry.path().string();
        if (path.size() >= 8 && path.compare(path.size() - 8, 8, ".test.ry") == 0) {
            files.push_back(path);
        }
    }
    std::sort(files.begin(), files.end());
    return files;
}

static int runTestFilesSequential(const std::vector<std::string> &test_files,
                                  const char *argv0, bool skip_global_lib,
                                  bool coverage, bool outline) {
    CoverageState cs;
    if (coverage) resetCoverageState(cs);
    int total_failed = 0;
    for (const auto &tf : test_files) {
        try {
            int failed = runRyFile(tf, /*test_mode=*/true, argv0, skip_global_lib,
                                   coverage, outline, coverage ? &cs : nullptr);
            total_failed += failed;
        } catch (const DiagnosticError &e) {
            llvm::errs() << e.what();
            ++total_failed;
        } catch (const std::exception &e) {
            llvm::errs() << "Error in " << tf << ": " << e.what() << "\n";
            ++total_failed;
        }
    }
    int total_files = static_cast<int>(test_files.size());
    if (!outline && total_files > 1) {
        std::printf("\n%d test files executed, %d total failures\n",
                    total_files, total_failed);
    }
    if (coverage) emitCoverageReport(cs);
    return total_failed > 0 ? 1 : 0;
}

struct TestFileResult {
    std::string filepath;
    int exit_code;
    std::string output;
};

// Run a single test file in a subprocess, capturing stdout+stderr.
// Uses posix_spawn instead of fork to be safe from multi-threaded contexts.
static TestFileResult runTestFileSubprocess(const std::string &filepath,
                                            const std::string &exe_path) {
    TestFileResult result;
    result.filepath = filepath;
    result.exit_code = -1;

    int pipefd[2];
    if (pipe(pipefd) == -1) {
        result.output = "Error: pipe() failed: " + std::string(strerror(errno)) + "\n";
        return result;
    }

    posix_spawn_file_actions_t actions;
    int rc = posix_spawn_file_actions_init(&actions);
    if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, pipefd[0]);
    if (rc == 0) rc = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDOUT_FILENO);
    if (rc == 0) rc = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDERR_FILENO);
    if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, pipefd[1]);
    if (rc != 0) {
        posix_spawn_file_actions_destroy(&actions);
        close(pipefd[0]);
        close(pipefd[1]);
        result.output = "Error: posix_spawn_file_actions failed: " + std::string(strerror(rc)) + "\n";
        return result;
    }

    const char *argv[] = {exe_path.c_str(), "test", filepath.c_str(), nullptr};
    pid_t pid;
    int err = posix_spawn(&pid, exe_path.c_str(), &actions, nullptr,
                          const_cast<char *const *>(argv), RY_ENVIRON);
    posix_spawn_file_actions_destroy(&actions);

    if (err != 0) {
        close(pipefd[0]);
        close(pipefd[1]);
        result.output = "Error: posix_spawn() failed: " + std::string(strerror(err)) + "\n";
        return result;
    }

    close(pipefd[1]);
    char buf[4096];
    ssize_t n;
    while ((n = read(pipefd[0], buf, sizeof(buf))) > 0 || (n == -1 && errno == EINTR)) {
        if (n > 0) result.output.append(buf, static_cast<size_t>(n));
    }
    close(pipefd[0]);

    int status = 0;
    pid_t wp;
    while ((wp = waitpid(pid, &status, 0)) == -1) {
        if (errno != EINTR) break;
    }

    result.exit_code = -1;
    if (wp != -1) {
        if (WIFEXITED(status))
            result.exit_code = WEXITSTATUS(status);
        else if (WIFSIGNALED(status))
            result.exit_code = 128 + WTERMSIG(status);
    }

    return result;
}

static int runTestFilesParallel(const std::vector<std::string> &test_files,
                                const std::string &exe_path, int parallelism) {
    size_t num_files = test_files.size();
    std::vector<TestFileResult> results(num_files);

    // Work queue: indices into test_files
    std::deque<size_t> work_queue;
    for (size_t i = 0; i < num_files; ++i)
        work_queue.push_back(i);

    std::mutex queue_mutex;
    std::mutex progress_mutex;
    int completed = 0;

    auto worker = [&]() {
        while (true) {
            size_t idx;
            {
                std::lock_guard<std::mutex> lock(queue_mutex);
                if (work_queue.empty()) return;
                idx = work_queue.front();
                work_queue.pop_front();
            }
            results[idx] = runTestFileSubprocess(test_files[idx], exe_path);
            {
                std::lock_guard<std::mutex> lock(progress_mutex);
                ++completed;
                std::fprintf(stderr, "\r\033[K[%d/%zu] Running tests...",
                             completed, num_files);
            }
        }
    };

    size_t num_workers = std::min(static_cast<size_t>(parallelism), num_files);
    std::vector<std::thread> threads;
    threads.reserve(num_workers);

    auto wall_start = std::chrono::steady_clock::now();
    for (size_t i = 0; i < num_workers; ++i)
        threads.emplace_back(worker);
    for (auto &t : threads)
        t.join();
    auto wall_end = std::chrono::steady_clock::now();
    double wall_elapsed = std::chrono::duration<double>(wall_end - wall_start).count();

    std::fprintf(stderr, "\r\033[K");

    int total_failed = 0;
    for (const auto &r : results) {
        if (r.exit_code != 0) {
            std::printf("\n\033[31m[FAIL exit=%d] %s\033[0m\n",
                        r.exit_code, r.filepath.c_str());
            ++total_failed;
        }
        std::fputs(r.output.c_str(), stdout);
    }

    std::printf("\n%zu test files executed, %d total failures (%.2fs, %zu workers)\n",
                num_files, total_failed, wall_elapsed, num_workers);
    return total_failed > 0 ? 1 : 0;
}

int runTestFiles(const std::vector<std::string> &test_files,
                 const char *argv0, bool skip_global_lib,
                 bool parallel, bool coverage, bool outline) {
    if (outline || !parallel || test_files.size() <= 1) {
        return runTestFilesSequential(test_files, argv0, skip_global_lib, coverage, outline);
    }
    std::string exe_path = ry::self_update::detail::get_executable_path();
    if (exe_path.empty()) {
        llvm::errs() << "Warning: cannot resolve executable path, falling back to sequential\n";
        return runTestFilesSequential(test_files, argv0, skip_global_lib, coverage, false);
    }
    unsigned hw = std::thread::hardware_concurrency();
    int parallelism = static_cast<int>(std::min({hw, 8u, static_cast<unsigned>(test_files.size())}));
    if (parallelism < 1) parallelism = 1;
    return runTestFilesParallel(test_files, exe_path, parallelism);
}

int discoverAndRunTests(const std::string &dir, const char *argv0,
                        bool skip_global_lib, bool parallel,
                        bool coverage, bool outline) {
    auto test_files = findTestFiles(dir);
    if (test_files.empty()) {
        llvm::errs() << "No *.test.ry files found in " << dir << "\n";
        return 1;
    }
    return runTestFiles(test_files, argv0, skip_global_lib, parallel, coverage, outline);
}

} // namespace ry
