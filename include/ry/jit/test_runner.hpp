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
// requested_workers == 0 means "use computeDefaultWorkers(hardware_concurrency())"
// (i.e. hw - 1, minimum 1). The result is clamped to test_file_count
// (when non-zero) and floored at 1.
int computeParallelism(int requested_workers, std::size_t test_file_count);

int runTestFiles(const std::vector<std::string> &test_files,
                 const char *argv0, bool skip_global_lib,
                 bool parallel, bool coverage = false, bool outline = false,
                 int parallel_workers = 0);

int discoverAndRunTests(const std::string &dir, const char *argv0,
                        bool skip_global_lib, bool parallel,
                        bool coverage = false, bool outline = false,
                        int parallel_workers = 0);

} // namespace ry
