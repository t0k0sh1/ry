#pragma once

#include <cstdint>

extern "C" {
    void __ry_coverage_register_line(int32_t file_id, int32_t line);
    void __ry_coverage_hit(int32_t file_id, int32_t line);
    void __ry_coverage_reset();
    void __ry_coverage_report_summary(const char *const *filenames, int32_t file_count);
}
