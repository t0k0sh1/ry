#pragma once
#include <cstddef>
#include <string>
#include <vector>

namespace ry {

std::vector<std::string> findTestFiles(const std::string &root_dir);

// Returns the default worker count for parallel test execution when the user
// passes -p without an explicit N. Computes hw_concurrency - 1 to leave one
// core free, with a floor of 1 (hw == 0 / hw == 1 both return 1, also guarding
// against unsigned underflow at hw == 0).
unsigned computeDefaultWorkers(unsigned hw_concurrency);

// Returns the effective worker count for parallel test execution.
// requested_workers <= 0 means "use computeDefaultWorkers(hardware_concurrency())"
// (i.e. hw - 1, minimum 1). The result is clamped to test_file_count
// (when non-zero) and floored at 1.
int computeParallelism(int requested_workers, std::size_t test_file_count);

// Subprocess fan-out dispatcher (#2234). Spawns one child process per test
// file via posix_spawn — even at worker=1 — so the parent never JITs and
// each child exits via _exit, matching the "1 source = 1 process" invariant
// the 6-step JIT teardown suppression depends on.
//
// `parallel_workers` controls only the worker count:
//   0 → computeDefaultWorkers(hardware_concurrency()) = hw-1 (min 1)
//   N → N (clamped to test_file_count, floored at 1)
//
// `--coverage` / `--trace` / `--outline` are NOT forwarded to the child
// (runTestFileSubprocess argv is `{exe, "test", filepath}` only); the call
// site disables them with a warning when the target is multi-file.
int runTestFiles(const std::vector<std::string> &test_files,
                 int parallel_workers = 0);

int discoverAndRunTests(const std::string &dir,
                        int parallel_workers = 0);

} // namespace ry
