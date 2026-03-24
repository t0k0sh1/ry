#pragma once
#include <filesystem>
#include <functional>
#include <string>
#include <unordered_map>

namespace ry {

using MtimeMap = std::unordered_map<std::string, std::filesystem::file_time_type>;

/// Collect last-write-times of all *.ry files under root_dir.
/// Skips .git, build, node_modules directories.
MtimeMap collectRyFileMtimes(const std::string &root_dir);

/// Watch *.ry files under root_dir and re-run tests on change.
/// 1. Runs run_tests() immediately.
/// 2. Polls for mtime changes every 300ms with 200ms debounce.
/// 3. Clears screen and prints changed files + timestamp before re-run.
/// 4. Exits on SIGINT (Ctrl+C).
void watchAndRunTests(const std::string &root_dir,
                      std::function<int()> run_tests);

} // namespace ry
