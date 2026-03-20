#pragma once

#include <optional>
#include <string>

struct ProjectConfig {
    std::string name;
    std::string version;
    std::string entry;
    std::string src_dir;
};

class ProjectConfigParser {
public:
    static ProjectConfig load(const std::string &toml_content);
    static std::string serialize(const ProjectConfig &config);
};

std::optional<std::string> findProjectRoot(const std::string &start_dir = "");

int cmd_init();
