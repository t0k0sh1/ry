#include "ry/cli/cli.hpp"
#include "ry/project/project_config.hpp"
#include "ry/project/paths.hpp"
#include "ry/project/dotenv.hpp"
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
        "test", "init", "new", "fmt", "self-update", "run", "docs", nullptr
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

bool tryResolveBareRyFile(const std::string &arg, std::string &out_path, std::string &err) {
    err.clear();
    fs::path parsed(arg);
    if (parsed.has_parent_path()) return false;
    // Only resolve source filenames (*.ry); bare tokens without .ry stay "unknown command" vs subcommands.
    static constexpr const char kRySuffix[] = ".ry";
    constexpr size_t kRySuffixLen = sizeof(kRySuffix) - 1;
    if (arg.size() < kRySuffixLen ||
        arg.compare(arg.size() - kRySuffixLen, kRySuffixLen, kRySuffix) != 0) {
        return false;
    }

    auto root = findProjectRoot();
    if (!root) {
        err = "Error: no such file: " + arg + "\n";
        return false;
    }

    std::ifstream f(fs::path(*root) / "package.toml");
    if (!f) {
        err = "Error: failed to read package.toml while resolving " + arg + "\n";
        return false;
    }

    std::string content((std::istreambuf_iterator<char>(f)),
                         std::istreambuf_iterator<char>{});
    ProjectConfig config;
    try {
        config = ProjectConfigParser::load(content);
    } catch (const std::exception &e) {
        err = std::string("Error: failed to parse package.toml: ") + e.what() + "\n";
        return false;
    }

    auto try_file = [&](const fs::path &candidate) -> bool {
        std::error_code ec;
        if (!fs::is_regular_file(candidate, ec) || ec) return false;
        std::error_code ec2;
        fs::path abs = fs::weakly_canonical(candidate, ec2);
        if (ec2) return false;
        out_path = abs.string();
        return true;
    };

    // 1) Project root (next to package.toml), then 2) [paths] entries in key order — first match wins.
    if (try_file(fs::path(*root) / arg)) return true;
    for (const auto &rel_root : config.path_search_roots) {
        if (try_file(fs::path(*root) / rel_root / arg)) return true;
    }

    err = "Error: no such file: " + arg + "\nSearched:\n";
    err += "  " + (fs::path(*root) / arg).string() + "\n";
    for (const auto &rel_root : config.path_search_roots) {
        err += "  " + (fs::path(*root) / rel_root / arg).string() + "\n";
    }
    return false;
}

void printMainHelp() {
    llvm::outs() << "ry " << RY_VERSION << "\n\n";
    llvm::outs() << "Usage:\n";
    llvm::outs() << "  echo '<code>' | ry -c                Run code from stdin\n";
    llvm::outs() << "  ry test [options] [<file> | <dir>]   Run tests\n";
    llvm::outs() << "  ry init                              Initialize a project\n";
    llvm::outs() << "  ry new <project-name>                Create a new project\n";
    llvm::outs() << "  ry run [<name> | <file.ry>] [args...] Run a project script or Ry file\n";
    llvm::outs() << "  ry fmt [options] [<file> | <dir>]    Format source files\n";
    llvm::outs() << "  ry self-update [options]              Update ry itself\n";
    llvm::outs() << "  ry docs [options]                    Generate static HTML API documentation\n";
    llvm::outs() << "\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  -c               Read and execute code from stdin\n";
    llvm::outs() << "  -h, --help       Show this help\n";
    llvm::outs() << "  -v, --version    Show version\n";
    llvm::outs() << "  --env=<env>      Set environment (production|development|internal)\n";
    llvm::outs() << "  --trace          Emit structured internal trace as JSON Lines\n";
    llvm::outs() << "  --trace-out=PATH Write trace output to PATH (or '-' for stderr)\n";
    llvm::outs() << "  --emit-llvm-ir   Emit unoptimized LLVM IR to stdout and exit\n";
    llvm::outs() << "  --strict-any     Enable strict-any semantics (mirrors RY_STRICT_ANY=1);\n";
    llvm::outs() << "                   place BEFORE the subcommand (e.g. `ry --strict-any test ...`)\n";
    llvm::outs() << "\n";
    llvm::outs() << "Run 'ry <command> --help' for more information on a command.\n";
}

void printTestHelp() {
    llvm::outs() << "Usage: ry test [options] [<file.ry> | <dir>]\n\n";
    llvm::outs() << "Run test files (*.test.ry).\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  -p [N], --parallel [N]\n";
    llvm::outs() << "                       Run tests in parallel; optional positive integer N\n";
    llvm::outs() << "                       selects the worker count (default: hardware_concurrency() - 1, minimum 1)\n";
    llvm::outs() << "  -w, --watch          Watch for changes and re-run\n";
    llvm::outs() << "  --coverage, --cov    Collect coverage information\n";
    llvm::outs() << "  --outline            Print describe/it structure without running tests\n";
    llvm::outs() << "  --trace              Emit structured internal trace as JSON Lines\n";
    llvm::outs() << "  --trace-out=PATH     Write trace output to PATH (or '-' for stderr)\n";
    llvm::outs() << "  -h, --help           Show this help\n";
    llvm::outs() << "\n";
    llvm::outs() << "Global options that affect `ry test` go BEFORE the subcommand:\n";
    llvm::outs() << "  ry --strict-any test <file>    Run tests under strict-any semantics\n";
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
    llvm::outs() << "Usage: ry run [<name> | <file.ry> | -- [args...]] [args...]\n\n";
    llvm::outs() << "Run a project script defined in package.toml [scripts], or a Ry source file.\n\n";
    llvm::outs() << "  ry run                          List all available scripts\n";
    llvm::outs() << "  ry run <name>                   Run [scripts].<name>, or fall back to <name>.ry\n";
    llvm::outs() << "                                  resolved via [paths] (scripts take precedence)\n";
    llvm::outs() << "  ry run <file.ry> [args...]      Run a Ry source file (bare name, relative,\n";
    llvm::outs() << "                                  or absolute path) with positional arguments\n";
    llvm::outs() << "  ry run -- [args...]             Run the project entry point with arguments\n";
}

void printSelfUpdateHelp() {
    llvm::outs() << "Usage: ry self-update [<version>]\n\n";
    llvm::outs() << "Update ry to a newer version.\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  (no args)    Update to latest stable release\n";
    llvm::outs() << "  <version>    Update to a specific version (e.g. v0.0.1)\n";
    llvm::outs() << "  -h, --help   Show this help\n";
}

void printDocsHelp() {
    llvm::outs() << "Usage: ry docs [options]\n\n";
    llvm::outs() << "Generate static HTML documentation for the current project.\n";
    llvm::outs() << "Reads @doc directives from .ry files under [paths].src and writes\n";
    llvm::outs() << "HTML pages plus a docs.json manifest into the output directory.\n\n";
    llvm::outs() << "Options:\n";
    llvm::outs() << "  --out <path>        Output directory (default: docs/api)\n";
    llvm::outs() << "  --format <fmt>      Output format; only 'html' is supported\n";
    llvm::outs() << "  --emit-json         Emit docs.json (also emitted by default)\n";
    llvm::outs() << "  --include-private   Include declarations without @public\n";
    llvm::outs() << "  --clean             Delete files listed in .ry-docs-manifest\n";
    llvm::outs() << "  -h, --help          Show this help\n";
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

void parseGlobalFlags(int &argc, char **&argv, bool &trace_enabled, std::string &trace_out,
                      bool &emit_llvm_ir) {
    while (argc >= 2) {
        if (std::strcmp(argv[1], "--trace") == 0) {
            trace_enabled = true;
        } else if (std::strncmp(argv[1], "--trace-out=", 12) == 0) {
            trace_enabled = true;
            trace_out = argv[1] + 12;
        } else if (std::strcmp(argv[1], "--emit-llvm-ir") == 0) {
            emit_llvm_ir = true;
        } else if (std::strcmp(argv[1], "--strict-any") == 0) {
            // #2319: see cli.hpp — flag is mirrored to RY_STRICT_ANY.
            setenv("RY_STRICT_ANY", "1", 1);
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
