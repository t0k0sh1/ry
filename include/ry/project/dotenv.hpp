#pragma once

#include <string>
#include <utility>
#include <vector>

namespace ry {

/// Resolve RY_ENV aliases to canonical short names.
/// "production" -> "prod", "development" -> "dev", others pass through.
std::string resolveRyEnv(const std::string &value);

/// Parse .env file content into key-value pairs.
std::vector<std::pair<std::string, std::string>>
parseDotEnv(const std::string &content);

/// Load .env file(s) from the given directory into the process environment.
/// When env_name is non-empty, loads .env.<env_name> first, then .env.
/// When env_name is "prod", skips all .env loading (security).
/// When env_name is empty, loads only .env (backward compatible).
/// Existing env vars are not overwritten.
void loadDotEnv(const std::string &dir, const std::string &env_name = "");

} // namespace ry
