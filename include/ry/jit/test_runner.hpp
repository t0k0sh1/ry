#pragma once
#include <string>
#include <vector>

namespace ry {

std::vector<std::string> findTestFiles(const std::string &root_dir);

int runTestFiles(const std::vector<std::string> &test_files,
                 const char *argv0, bool skip_global_lib,
                 bool parallel, bool coverage = false, bool outline = false);

int discoverAndRunTests(const std::string &dir, const char *argv0,
                        bool skip_global_lib, bool parallel,
                        bool coverage = false, bool outline = false);

} // namespace ry
