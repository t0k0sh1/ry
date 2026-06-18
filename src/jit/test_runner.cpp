#include "ry/jit/test_runner.hpp"
#include "ry/cli/self_update.hpp"
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

struct TestFileResult {
    std::string filepath;
    int exit_code;
    std::string output;
};

// Run a single test file in a subprocess, capturing stdout+stderr.
// Uses posix_spawn instead of fork to be safe from multi-threaded contexts.
// When `outline` is true, `--outline` is inserted into the child argv so the
// child's CodeGen produces outline output (#2236).
static TestFileResult runTestFileSubprocess(const std::string &filepath,
                                            const std::string &exe_path,
                                            bool outline) {
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

    // argv は stack のみで構築する。#2236 で heap 経由の std::vector に倒した
    // ところ collection_meta_propagation.test.ry の並列実行下 flake (#1895)
    // が ~8% から ~31% に跳ね、KNOWLEDGE.md「Subprocess-runner perturbation
    // note」記載のとおり stack-only が必須条件になった。
    const char *argv[5] = {exe_path.c_str(), "test", nullptr, nullptr, nullptr};
    int slot = 2;
    if (outline) argv[slot++] = "--outline";
    argv[slot] = filepath.c_str();
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

// Subprocess fan-out: each test file runs in its own child process so the
// JIT teardown 6-step suppression (KNOWLEDGE.md "LLVM ORC JIT intermittent
// teardown crash") holds (1 source = 1 process). Parent JIT-free; only
// posix_spawn / pipe-read / waitpid. Worker count = 1 still goes through
// this path so the dispatcher is uniform (#2234).
static int runTestFilesSubprocessFanOut(const std::vector<std::string> &test_files,
                                        const std::string &exe_path,
                                        int parallelism,
                                        bool outline) {
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
            results[idx] = runTestFileSubprocess(test_files[idx], exe_path, outline);
            {
                std::lock_guard<std::mutex> lock(progress_mutex);
                ++completed;
                if (!outline) {
                    std::fprintf(stderr, "\r\033[K[%d/%zu] Running tests...",
                                 completed, num_files);
                }
            }
        }
    };

    size_t num_workers = std::min(static_cast<size_t>(parallelism), num_files);
    std::vector<std::thread> threads;
    threads.reserve(num_workers);

    // Outline モードでは fan-out summary / progress 行をすべて suppress し、
    // 出力は per-file outline のみに揃える(pre-#2234 sequential 経路の
    // `if (!outline && total_files > 1)` を踏襲、#2236)。wall clock も outline 時
    // は採取しない(2 回の clock_gettime + FP 計算が無駄になるため)。
    const char *worker_label = num_workers == 1 ? "worker" : "workers";
    std::chrono::steady_clock::time_point wall_start{};
    if (!outline) {
        std::fprintf(stderr, "Running %zu test files with %zu %s...\n",
                     num_files, num_workers, worker_label);
        wall_start = std::chrono::steady_clock::now();
    }

    for (size_t i = 0; i < num_workers; ++i)
        threads.emplace_back(worker);
    for (auto &t : threads)
        t.join();

    if (!outline) std::fprintf(stderr, "\r\033[K");

    int total_failed = 0;
    for (const auto &r : results) {
        if (r.exit_code != 0) {
            if (!outline) {
                std::printf("\n\033[31m[FAIL exit=%d] %s\033[0m\n",
                            r.exit_code, r.filepath.c_str());
            }
            ++total_failed;
        }
        std::fputs(r.output.c_str(), stdout);
    }

    if (!outline) {
        double wall_elapsed = std::chrono::duration<double>(
            std::chrono::steady_clock::now() - wall_start).count();
        std::printf("\n%zu test files executed, %d total failures (%.2fs, %zu %s)\n",
                    num_files, total_failed, wall_elapsed, num_workers, worker_label);
    }
    return total_failed > 0 ? 1 : 0;
}

unsigned computeDefaultWorkers(unsigned hw_concurrency) {
    // hw == 0 (取得失敗) と hw == 1 はどちらも 1 を返す。
    // 減算より前にガードしないと unsigned underflow で巨大値になる。
    if (hw_concurrency <= 1) return 1u;
    return hw_concurrency - 1u;
}

int computeParallelism(int requested_workers, std::size_t test_file_count) {
    unsigned base;
    if (requested_workers > 0) {
        base = static_cast<unsigned>(requested_workers);
    } else {
        base = computeDefaultWorkers(std::thread::hardware_concurrency());
    }
    if (static_cast<std::size_t>(base) > test_file_count) {
        base = static_cast<unsigned>(test_file_count);
    }
    int parallelism = static_cast<int>(base);
    if (parallelism < 1) parallelism = 1;
    return parallelism;
}

// All multi-file paths go through subprocess fan-out (#2234). The in-process
// runRyFile loop is gone — even worker=1 spawns one subprocess per file. This
// is what makes the 6-step JIT teardown suppression hold (1 source = 1 process):
// the parent never JITs and child exits via _exit, so leaks are reclaimed by
// the OS instead of accumulating across files (KNOWLEDGE.md "LLVM ORC JIT
// intermittent teardown crash").
//
// `--coverage` / `--trace` must be disabled (with a warning) at the call site
// before reaching here when the target is multi-file — coverage requires
// cross-process aggregation and trace risks clobbering the shared trace-out
// file across concurrent subprocesses. `--outline` is now forwarded via argv
// (#2236): each child subprocess receives `{exe, "test", "--outline",
// filepath}` when outline=true, and the parent additionally suppresses its
// own fan-out summary / progress lines so the aggregated stdout is per-file
// outline only — content-equivalent to the pre-#2234 sequential-loop output
// (per-file outline preserved; child stdout+stderr are pipe-merged at the
// parent rather than the old separate stdout/stderr streams).
int runTestFiles(const std::vector<std::string> &test_files,
                 int parallel_workers,
                 bool outline) {
    std::string exe_path = ry::self_update::detail::get_executable_path();
    if (exe_path.empty()) {
        llvm::errs()
            << "Error: cannot resolve executable path for subprocess test runner\n";
        return 1;
    }
    int parallelism = computeParallelism(parallel_workers, test_files.size());
    return runTestFilesSubprocessFanOut(test_files, exe_path, parallelism, outline);
}

int discoverAndRunTests(const std::string &dir,
                        int parallel_workers,
                        bool outline) {
    auto test_files = findTestFiles(dir);
    if (test_files.empty()) {
        llvm::errs() << "No *.test.ry files found in " << dir << "\n";
        return 1;
    }
    return runTestFiles(test_files, parallel_workers, outline);
}

} // namespace ry
