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
    // Symbol-derivation policy used by emitGenericNativeCall when this
    // package is the matched lib for a `@native("<pkg>")` call. false =
    // preserve the Ry identifier as-is (`__ry_<pkg>_<name>`); true =
    // convert camelCase → snake_case before joining
    // (`encodeUrlSafe` → `__ry_<pkg>_encode_url_safe`). Hand-written
    // dispatchers (`emitTableDrivenNativeCall`) ignore this and use the
    // entry's `rtNameOverride` / `rtSuffix` instead.
    bool snake_case_symbols = false;
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

    // O(1) lookup by package name. Returns nullptr if not registered.
    // Caches the name → entry index on first call; safe because
    // packages_ is append-only and immutable after static init.
    const StdlibPackageEntry *findPackage(const std::string &name);

  private:
    StdlibRegistry() = default;
    std::vector<StdlibPackageEntry> packages_;
    std::unordered_map<std::string, NativeConstantEntry> constants_;
    std::unordered_map<std::string, size_t> package_index_;
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
    // `errorChannelLibrary` (optional, defaults to `library`) overrides the
    // module name used to derive the resource's last-error runtime symbol
    // (`__ry_<errorChannelLibrary>_get_last_error`). Used by resources whose
    // error channel is distinct from their owning library — e.g. TlsStream
    // lives in the http library (linkage) but reports errors through
    // `__ry_tls_get_last_error` (#2339, Installment 2-b).
    int registerKind(const char *typeName, const char *dtorName,
                     const char *cleanupFnName, const char *library,
                     const char *errorChannelLibrary = nullptr);

    // Lookup resource kind ID by Ry type name. Returns NONE if not found.
    int lookupByTypeName(const std::string &typeName) const;

    struct Info {
        std::string typeName;
        const char *dtorName;
        const char *cleanupFnName;
        const char *library;
        const char *errorChannelLibrary;  // never null; defaults to `library`
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
    RY_REGISTER_STDLIB_PACKAGE_FULL(pkg_name, decl, fn, 100, false)

#define RY_REGISTER_STDLIB_PACKAGE_PRIO(pkg_name, decl, fn, prio)              \
    RY_REGISTER_STDLIB_PACKAGE_FULL(pkg_name, decl, fn, prio, false)

// Register a package whose runtime symbols use snake_case naming. The
// generic-dispatch path converts camelCase Ry identifiers to snake_case
// before deriving `__ry_<pkg>_<name>`. Used by modules whose C-side
// runtime predates Ry's camelCase convention (e.g. base64). New
// modules should keep `false` and name their C symbols to match the Ry
// identifier exactly.
#define RY_REGISTER_STDLIB_PACKAGE_NAMING(pkg_name, decl, fn, snake_case)      \
    RY_REGISTER_STDLIB_PACKAGE_FULL(pkg_name, decl, fn, 100, snake_case)

// Underlying registration macro. Prefer one of the wrappers above.
#define RY_REGISTER_STDLIB_PACKAGE_FULL(pkg_name, decl, fn, prio, snake_case)  \
    static llvm::Value *fn(CodeGen &, const CallExpr &);                       \
    namespace {                                                                \
    struct StdlibReg_##pkg_name {                                              \
        StdlibReg_##pkg_name() {                                               \
            StdlibRegistry::instance().registerPackage(                         \
                {#pkg_name, decl, fn, prio, snake_case});                      \
        }                                                                      \
    } stdlib_reg_##pkg_name##_;                                                \
    }

} // namespace ry
