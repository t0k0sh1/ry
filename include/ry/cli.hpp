#pragma once
#include <string>

namespace ry {
namespace cli {

bool isHelpFlag(const char *arg);
bool hasHelpFlag(int argc, char *argv[], int start = 1);
bool isKnownSubcommand(const char *arg);
std::string resolveEntryPoint(bool require);

void printMainHelp();
void printTestHelp();
void printInitHelp();
void printNewHelp();
void printFmtHelp();
void printRunHelp();
void printSelfUpdateHelp();

// Parse --env= flag or RY_ENV env var. Returns true if skip_global_lib should be set.
bool parseRyEnv(int &argc, char **&argv);

// Parse --trace / --trace-out= flags, updating trace_enabled and trace_out.
void parseGlobalFlags(int &argc, char **&argv, bool &trace_enabled, std::string &trace_out);

} // namespace cli
} // namespace ry
