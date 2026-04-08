#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>


namespace ry {

struct ProjectConfig {
    std::string name;
    std::string version;
    std::string entry;
    std::string src_dir;
    std::optional<std::string> dev_stdlib_dir;
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

int cmd_init();
int cmd_new(int argc, char *argv[]);
int cmd_run(int argc, char *argv[]);

} // namespace ry
