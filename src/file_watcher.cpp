#include "ry/file_watcher.hpp"
#include <atomic>
#include <chrono>
#include <csignal>
#include <cstdio>
#include <ctime>
#include <thread>
#include <unordered_set>
#include <vector>

namespace fs = std::filesystem;

namespace ry {

MtimeMap collectRyFileMtimes(const std::string &root_dir) {
    static const std::unordered_set<std::string> skip_dirs = {
        ".git", "build", "node_modules"
    };
    MtimeMap mtimes;
    std::error_code ec;
    auto it = fs::recursive_directory_iterator(
        root_dir, fs::directory_options::skip_permission_denied, ec);
    if (ec) return mtimes;
    for (auto end = fs::recursive_directory_iterator(); it != end; it.increment(ec)) {
        if (ec) { ec.clear(); continue; }
        auto &entry = *it;
        if (entry.is_directory() &&
            skip_dirs.count(entry.path().filename().string())) {
            it.disable_recursion_pending();
            continue;
        }
        if (!entry.is_regular_file()) continue;
        auto path = entry.path();
        if (path.extension() == ".ry") {
            std::error_code mtime_ec;
            auto mtime = fs::last_write_time(path, mtime_ec);
            if (!mtime_ec) {
                mtimes[path.string()] = mtime;
            }
        }
    }
    return mtimes;
}

static std::atomic<bool> g_watch_stop{false};

static void watch_sigint_handler(int) {
    g_watch_stop.store(true, std::memory_order_relaxed);
}

/// Detect changed files between old and new snapshots.
/// Returns paths that are new, deleted, or have different mtimes.
static std::vector<std::string> detectChanges(const MtimeMap &old_snap,
                                               const MtimeMap &new_snap) {
    std::vector<std::string> changed;
    // New or modified files
    for (const auto &[path, mtime] : new_snap) {
        auto it = old_snap.find(path);
        if (it == old_snap.end() || it->second != mtime) {
            changed.push_back(path);
        }
    }
    // Deleted files
    for (const auto &[path, _] : old_snap) {
        if (new_snap.find(path) == new_snap.end()) {
            changed.push_back(path);
        }
    }
    return changed;
}

static std::string currentTimeString() {
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);
    char buf[16];
    std::strftime(buf, sizeof(buf), "%H:%M:%S", std::localtime(&t));
    return buf;
}

void watchAndRunTests(const std::string &root_dir,
                      std::function<int()> run_tests) {
    // Install SIGINT handler, saving the previous one for restoration
    g_watch_stop.store(false, std::memory_order_relaxed);
    struct sigaction sa{}, old_sa{};
    sa.sa_handler = watch_sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGINT, &sa, &old_sa);

    std::printf("[%s] Watch mode started. Monitoring *.ry files in %s\n",
                currentTimeString().c_str(), root_dir.c_str());
    std::printf("Press Ctrl+C to exit.\n\n");

    // Initial snapshot and run
    auto snapshot = collectRyFileMtimes(root_dir);
    run_tests();

    // Poll loop
    while (!g_watch_stop.load(std::memory_order_relaxed)) {
        std::this_thread::sleep_for(std::chrono::milliseconds(300));
        if (g_watch_stop.load(std::memory_order_relaxed)) break;

        auto new_snap = collectRyFileMtimes(root_dir);
        if (detectChanges(snapshot, new_snap).empty()) continue;

        // Debounce: wait 200ms and re-scan to coalesce rapid changes
        std::this_thread::sleep_for(std::chrono::milliseconds(200));
        if (g_watch_stop.load(std::memory_order_relaxed)) break;
        new_snap = collectRyFileMtimes(root_dir);
        auto changed = detectChanges(snapshot, new_snap);

        // Clear screen
        std::printf("\033[2J\033[H");

        auto time_str = currentTimeString();
        for (const auto &path : changed) {
            std::printf("[%s] Change detected: %s\n", time_str.c_str(),
                        path.c_str());
        }
        std::printf("\n");

        snapshot = std::move(new_snap);
        run_tests();
    }

    // Restore previous SIGINT handler
    sigaction(SIGINT, &old_sa, nullptr);
    std::printf("\nWatch mode terminated.\n");
}

} // namespace ry
