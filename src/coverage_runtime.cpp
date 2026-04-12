#include "ry/coverage_runtime.hpp"

#include <algorithm>
#include <cstdio>
#include <mutex>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>


namespace ry {

// file_id -> set of coverable line numbers
static std::unordered_map<int32_t, std::unordered_set<int32_t>> g_coverable_lines;
// file_id -> (line -> hit count)
static std::unordered_map<int32_t, std::unordered_map<int32_t, uint64_t>> g_hit_counts;
static std::mutex g_coverage_mutex;

extern "C" {

void __ry_coverage_register_line(int32_t file_id, int32_t line) {
    std::lock_guard<std::mutex> lock(g_coverage_mutex);
    g_coverable_lines[file_id].insert(line);
    g_hit_counts[file_id][line]; // default-init to 0
}

void __ry_coverage_hit(int32_t file_id, int32_t line) {
    std::lock_guard<std::mutex> lock(g_coverage_mutex);
    auto file_it = g_hit_counts.find(file_id);
    if (file_it != g_hit_counts.end()) {
        auto line_it = file_it->second.find(line);
        if (line_it != file_it->second.end())
            ++line_it->second;
    }
}

void __ry_coverage_reset() {
    std::lock_guard<std::mutex> lock(g_coverage_mutex);
    g_coverable_lines.clear();
    g_hit_counts.clear();
}

void __ry_coverage_report_summary(const char *const *filenames, int32_t file_count) {
    std::lock_guard<std::mutex> lock(g_coverage_mutex);
    if (file_count <= 0 || !filenames) return;

    struct FileStats {
        std::string name;
        int32_t total_lines;
        int32_t covered_lines;
    };

    std::vector<FileStats> stats;
    int32_t grand_total = 0;
    int32_t grand_covered = 0;
    size_t max_name_len = 0;

    for (int32_t fid = 0; fid < file_count; ++fid) {
        // Skip files with empty name (e.g., stdlib files filtered out)
        if (!filenames[fid] || filenames[fid][0] == '\0')
            continue;
        auto it = g_coverable_lines.find(fid);
        if (it == g_coverable_lines.end() || it->second.empty())
            continue;

        int32_t total = static_cast<int32_t>(it->second.size());
        int32_t covered = 0;
        auto hit_it = g_hit_counts.find(fid);
        if (hit_it != g_hit_counts.end()) {
            for (int32_t line : it->second) {
                auto line_it = hit_it->second.find(line);
                if (line_it != hit_it->second.end() && line_it->second > 0)
                    ++covered;
            }
        }

        std::string name = filenames[fid];
        if (name.length() > max_name_len)
            max_name_len = name.length();

        stats.push_back({std::move(name), total, covered});
        grand_total += total;
        grand_covered += covered;
    }

    if (stats.empty()) return;

    // Sort by filename
    std::sort(stats.begin(), stats.end(),
              [](const FileStats &a, const FileStats &b) { return a.name < b.name; });

    std::printf("\nTest Coverage Summary:\n");

    for (const auto &s : stats) {
        double pct = s.total_lines > 0
            ? 100.0 * s.covered_lines / s.total_lines : 0.0;
        std::printf("  %-*s  %5.1f%%  (%d/%d lines)\n",
                    static_cast<int>(max_name_len), s.name.c_str(),
                    pct, s.covered_lines, s.total_lines);
    }

    // Separator
    int sep_len = static_cast<int>(max_name_len) + 24;
    std::printf("  ");
    for (int i = 0; i < sep_len; ++i) std::putchar('-');
    std::putchar('\n');

    double grand_pct = grand_total > 0
        ? 100.0 * grand_covered / grand_total : 0.0;
    std::printf("  %-*s  %5.1f%%  (%d/%d lines)\n",
                static_cast<int>(max_name_len), "Total",
                grand_pct, grand_covered, grand_total);
}

} // extern "C"

} // namespace ry
