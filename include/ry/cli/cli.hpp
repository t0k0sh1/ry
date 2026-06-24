#pragma once
#include <string>

namespace ry {
namespace cli {

bool isHelpFlag(const char *arg);
bool hasHelpFlag(int argc, char *argv[], int start = 1);
bool isKnownSubcommand(const char *arg);
std::string resolveEntryPoint(bool require);

/// If \p arg is a bare `*.ry` name (no parent path), resolve it under the nearest project: first
/// the project root (next to `package.toml`), then each `[paths]` directory in key order — first
/// match wins. On success sets \p out_path to an absolute path and returns true.
/// For bare `*.ry` failures, \p err is usually set (print and exit non-zero). Empty \p err means
/// the argument was not treated as a bare `*.ry` resolve attempt (e.g. wrong suffix).
bool tryResolveBareRyFile(const std::string &arg, std::string &out_path, std::string &err);

void printMainHelp();
void printTestHelp();
void printInitHelp();
void printNewHelp();
void printFmtHelp();
void printRunHelp();
void printSelfUpdateHelp();
void printDocsHelp();

// Parse --env= flag or RY_ENV env var. Returns true if skip_global_lib should be set.
bool parseRyEnv(int &argc, char **&argv);

// Parse --trace / --trace-out= / --emit-llvm-ir / --strict-any flags.
// --strict-any does not surface as an out-param; it setenv-s RY_STRICT_ANY
// so subprocesses spawned by `ry test` inherit it (single propagation
// channel — see src/jit/jit_runner.cpp).
void parseGlobalFlags(int &argc, char **&argv, bool &trace_enabled, std::string &trace_out,
                      bool &emit_llvm_ir);

} // namespace cli
} // namespace ry
