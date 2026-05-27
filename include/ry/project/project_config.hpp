#pragma once

#include <optional>
#include <string>
#include <utility>
#include <unordered_map>
#include <vector>


namespace ry {

struct ProjectConfig {
    std::string name;
    std::string version;
    std::string entry;
    std::string src_dir;
    std::optional<std::string> dev_stdlib_dir;
    /// Public `[paths]` entries in key order (excludes `_`-prefixed keys; `_dev_stdlib` is stored in
    /// `dev_stdlib_dir` only). Used for load→serialize round-trip.
    std::vector<std::pair<std::string, std::string>> paths_entries;
    /// Project-relative directory roots from `[paths]` (excluding `_dev_stdlib` and `_`-prefixed keys),
    /// sorted by key name, deduplicated by normalized relative path.
    std::vector<std::string> path_search_roots;
    std::unordered_map<std::string, std::string> scripts;
};

class ProjectConfigParser {
public:
    static ProjectConfig load(const std::string &toml_content);
    static std::string serialize(const ProjectConfig &config);
};

std::optional<std::string> findProjectRoot(const std::string &start_dir = "");

/// Returns the package root for the source file at @p file_path. The package
/// boundary is the nearest ancestor directory containing a `package.toml`.
/// Returns std::nullopt if the file is not contained in any package, or if
/// @p file_path is empty.
std::optional<std::string> packageRootOfFile(const std::string &file_path);

/// Returns true if two source files belong to the same package. Two files that
/// are both outside any `package.toml` are considered to share a single
/// anonymous package (so existing tests that do not write a `package.toml`
/// continue to observe same-package visibility).
bool inSamePackage(const std::string &file_a, const std::string &file_b);

int cmd_init();
int cmd_new(int argc, char *argv[]);
int cmd_run(int argc, char *argv[]);

} // namespace ry
