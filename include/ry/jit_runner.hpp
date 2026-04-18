#pragma once
#include <string>
#include <vector>

namespace ry {

struct CoverageState {
    std::vector<std::string> filenames;
    int file_count = 0;
    int file_id_offset = 0;
};

void resetCoverageState(CoverageState &cs);
void emitCoverageReport(const CoverageState &cs);

int runRySource(const std::string &src, const std::string &source_name,
                const std::string &referrer_dir, bool test_mode,
                const char *argv0, bool skip_global_lib,
                bool coverage_mode = false, bool outline_mode = false,
                CoverageState *cs = nullptr, bool emit_llvm_ir = false);

int runRyFile(const std::string &filepath, bool test_mode,
              const char *argv0, bool skip_global_lib,
              bool coverage_mode = false, bool outline_mode = false,
              CoverageState *cs = nullptr, bool emit_llvm_ir = false);

} // namespace ry
