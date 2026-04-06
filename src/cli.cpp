#include "ry/cli.hpp"
#include "ry/project_config.hpp"
#include "ry/paths.hpp"
#include "ry/dotenv.hpp"
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <llvm/Support/raw_ostream.h>

namespace fs = std::filesystem;

namespace ry {
namespace cli {

bool isHelpFlag(const char *arg) {
    return std::strcmp(arg, "-h") == 0 || std::strcmp(arg, "--help") == 0;
}

bool hasHelpFlag(int argc, char *argv[], int start) {
    for (int i = start; i < argc; ++i)
        if (isHelpFlag(argv[i])) return true;
    return false;
}

bool isKnownSubcommand(const char *arg) {
    static const char *known[] = {
        "test", "init", "new", "fmt", "self-update", "run", nullptr
    };
    for (const char **p = known; *p; ++p)
        if (std::strcmp(arg, *p) == 0) return true;
    return false;
}

std::string resolveEntryPoint(bool require) {
    auto root = findProjectRoot();
    if (!root) {
        if (require) llvm::errs() << "Error: package.toml not found. Run 'ry init' first.\n";
        return "";
    }
    std::ifstream f(fs::path(*root) / "package.toml");
    if (!f) {
        if (require) llvm::errs() << "Error: cannot open package.toml\n";
        return "";
    }
    std::string content((std::istreambuf_iterator<char>(f)),
                         std::istreambuf_iterator<char>{});
    ProjectConfig config;
    try {
        config = ProjectConfigParser::load(content);
    } catch (const std::exception &e) {
        if (require) llvm::errs() << "Error: failed to parse package.toml: " << e.what() << "\n";
        return "";
    }
    if (config.entry.empty()) {
        if (require) llvm::errs() << "Error: no 'entry' field in package.toml\n";
        return "";
    }
    std::string path = (fs::path(*root) / config.entry).string();
    if (!fs::exists(path)) {
        if (require) llvm::errs() << "Error: entry file not found: " << config.entry << "\n";
        return "";
    }
    return path;
}

void printMainHelp() {
    llvm::outs() << "ry " << RY_VERSION << "\n\n";
    llvm::outs() << "Usage:\n";
    llvm::outs() << "  ry                                   Run entry point from package.toml\n";
    llvm::outs() << "  ry <file.ry> [args...]               Run a Ry script\n";
    llvm::outs() << "  echo '<code>' | ry -c                Run code from stdin\n";
    llvm::outs() << "  ry test [options] [<file> | <dir>]   Run tests\n";
    llvm::outs() << "  ry init                              Initialize a project\n";
    llvm::outs() << "  ry new <project-name>                Create a new project\n";
    llvm::outs() << "  ry run [<script-name>]               Run a project script\n";
    llvm::outs() << "  ry fmt [options] [<file> | <dir>]    Format source files\n";
    llvm::outs() << "  ry self-update [options]              Update ry itself\n";
    llvm::outs() << "\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  -c               Read and execute code from stdin\n";
    llvm::outs() << "  -h, --help       Show this help\n";
    llvm::outs() << "  -v, --version    Show version\n";
    llvm::outs() << "  --env=<env>      Set environment (production|development|internal)\n";
    llvm::outs() << "  --trace          Emit structured internal trace as JSON Lines\n";
    llvm::outs() << "  --trace-out=PATH Write trace output to PATH (or '-' for stderr)\n";
    llvm::outs() << "\n";
    llvm::outs() << "Run 'ry <command> --help' for more information on a command.\n";
}

void printTestHelp() {
    llvm::outs() << "Usage: ry test [options] [<file.ry> | <dir>]\n\n";
    llvm::outs() << "Run test files (*.test.ry).\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  -p, --parallel       Run tests in parallel\n";
    llvm::outs() << "  -w, --watch          Watch for changes and re-run\n";
    llvm::outs() << "  --coverage, --cov    Collect coverage information\n";
    llvm::outs() << "  --outline            Print describe/it structure without running tests\n";
    llvm::outs() << "  --trace              Emit structured internal trace as JSON Lines\n";
    llvm::outs() << "  --trace-out=PATH     Write trace output to PATH (or '-' for stderr)\n";
    llvm::outs() << "  -h, --help           Show this help\n";
}

void printInitHelp() {
    llvm::outs() << "Usage: ry init\n\n";
    llvm::outs() << "Initialize a new Ry project in the current directory.\n";
    llvm::outs() << "Creates a package.toml and basic project structure.\n";
}

void printNewHelp() {
    llvm::outs() << "Usage: ry new <project-name>\n\n";
    llvm::outs() << "Create a new Ry project in a new directory.\n";
}

void printFmtHelp() {
    llvm::outs() << "Usage: ry fmt [options] [<file> | <dir>]\n\n";
    llvm::outs() << "Format Ry source files.\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  --check      Check formatting without modifying files\n";
    llvm::outs() << "  -h, --help   Show this help\n";
}

void printRunHelp() {
    llvm::outs() << "Usage: ry run [<script-name>]\n\n";
    llvm::outs() << "Run a script defined in package.toml [scripts] section.\n\n";
    llvm::outs() << "If no script name is given, lists all available scripts.\n";
}

void printSelfUpdateHelp() {
    llvm::outs() << "Usage: ry self-update [--nightly | <version>]\n\n";
    llvm::outs() << "Update ry to a newer version.\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  (no args)    Update to latest stable release\n";
    llvm::outs() << "  --nightly    Update to latest nightly/prerelease\n";
    llvm::outs() << "  <version>    Update to a specific version (e.g. v0.0.1)\n";
    llvm::outs() << "  -h, --help   Show this help\n";
}

static void applyRyEnv(const char *value, bool &skip_global_lib) {
    std::string canonical = ry::resolveRyEnv(value);
    setenv("RY_ENV", canonical.c_str(), 1);
    if (canonical == "internal")
        skip_global_lib = true;
}

bool parseRyEnv(int &argc, char **&argv) {
    bool skip_global_lib = false;
    // --env=<value> CLI flag takes precedence over RY_ENV env var
    if (argc >= 2 && std::strncmp(argv[1], "--env=", 6) == 0) {
        applyRyEnv(argv[1] + 6, skip_global_lib);
        // Remove --env flag from argv, preserving argv[0] (program name)
        argv[1] = argv[0];
        ++argv;
        --argc;
    } else if (const char *env = std::getenv("RY_ENV")) {
        applyRyEnv(env, skip_global_lib);
    }
    return skip_global_lib;
}

void parseGlobalFlags(int &argc, char **&argv, bool &trace_enabled, std::string &trace_out) {
    while (argc >= 2) {
        if (std::strcmp(argv[1], "--trace") == 0) {
            trace_enabled = true;
        } else if (std::strncmp(argv[1], "--trace-out=", 12) == 0) {
            trace_enabled = true;
            trace_out = argv[1] + 12;
        } else {
            break;
        }

        argv[1] = argv[0];
        ++argv;
        --argc;
    }
}

} // namespace cli
} // namespace ry
