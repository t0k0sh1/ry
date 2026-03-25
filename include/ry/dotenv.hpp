#pragma once

#include <string>
#include <utility>
#include <vector>

namespace ry {

/// Parse .env file content into key-value pairs.
std::vector<std::pair<std::string, std::string>>
parseDotEnv(const std::string &content);

/// Load .env file from the given directory into the process environment.
/// Does nothing if the file does not exist. Existing env vars are not
/// overwritten.
void loadDotEnv(const std::string &dir);

} // namespace ry
