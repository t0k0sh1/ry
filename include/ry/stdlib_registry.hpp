#pragma once

#include <string>
#include <unordered_map>
#include <vector>


namespace llvm {
class Value;
} // namespace llvm

namespace ry {

class CodeGen;
struct CallExpr;

// --- Stdlib module dispatch registry ---

struct StdlibPackageEntry {
    const char *package_name;
    const char *decl_path;
    llvm::Value *(*dispatch)(CodeGen &cg, const CallExpr &e);
    int priority; // lower values are dispatched first (default 100)
};

// --- Native constant registry ---

enum class NativeConstantKind { Value, Infinity, NaN };

struct NativeConstantEntry {
    NativeConstantKind kind;
    double value; // used only when kind == Value
};

// Singleton registry for self-registering stdlib modules and constants.
// Uses Construct On First Use idiom to avoid static initialization order issues.
class StdlibRegistry {
  public:
    static StdlibRegistry &instance();

    void registerPackage(const StdlibPackageEntry &entry);
    void registerConstant(const char *name, NativeConstantEntry entry);

    // Returns modules sorted by priority (lower first).
    const std::vector<StdlibPackageEntry> &packages();
    const std::unordered_map<std::string, NativeConstantEntry> &
    constants() const {
        return constants_;
    }

  private:
    StdlibRegistry() = default;
    std::vector<StdlibPackageEntry> packages_;
    std::unordered_map<std::string, NativeConstantEntry> constants_;
    bool sorted_ = false;
};

// --- Resource kind registry ---

// Runtime-extensible resource type registry. Each stdlib module registers
// its opaque resource types at static initialization time. Replaces the
// former hardcoded ResourceKind enum in codegen.hpp.
class ResourceKindRegistry {
  public:
    static constexpr int NONE = -1;
    static ResourceKindRegistry &instance();

    // Register a new resource kind. Returns the assigned integer ID.
    int registerKind(const char *typeName, const char *dtorName,
                     const char *cleanupFnName, const char *library);

    // Lookup resource kind ID by Ry type name. Returns NONE if not found.
    int lookupByTypeName(const std::string &typeName) const;

    struct Info {
        std::string typeName;
        const char *dtorName;
        const char *cleanupFnName;
        const char *library;
    };

    const Info *getInfo(int id) const;
    int count() const { return static_cast<int>(entries_.size()); }

  private:
    ResourceKindRegistry() = default;
    std::vector<Info> entries_;
    std::unordered_map<std::string, int> name_to_id_;
};

// Self-registration macros for stdlib modules.
// Modules are dispatched in priority order (lower values first).
// Default priority is 100. Use RY_REGISTER_STDLIB_PACKAGE_PRIO for
// modules that must be dispatched before others sharing the same
// function names (e.g. net::listen must be tried before http::listen).
#define RY_REGISTER_STDLIB_PACKAGE(pkg_name, decl, fn)                         \
    RY_REGISTER_STDLIB_PACKAGE_PRIO(pkg_name, decl, fn, 100)

#define RY_REGISTER_STDLIB_PACKAGE_PRIO(pkg_name, decl, fn, prio)              \
    static llvm::Value *fn(CodeGen &, const CallExpr &);                       \
    namespace {                                                                \
    struct StdlibReg_##pkg_name {                                              \
        StdlibReg_##pkg_name() {                                               \
            StdlibRegistry::instance().registerPackage(                         \
                {#pkg_name, decl, fn, prio});                                   \
        }                                                                      \
    } stdlib_reg_##pkg_name##_;                                                \
    }

} // namespace ry
