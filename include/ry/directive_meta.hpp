#pragma once

#include "ry/ast.hpp"
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace ry {

// Bitmask of statement/declaration kinds that a directive may be applied to.
enum class DirectiveTarget : uint8_t {
    Function  = 1 << 0,
    Record    = 1 << 1,
    Field     = 1 << 2,
    Statement = 1 << 3,  // AssignStmt, CallStmt
    ForLoop   = 1 << 4,
};

inline uint8_t asTarget(DirectiveTarget t) { return static_cast<uint8_t>(t); }
inline uint8_t operator|(DirectiveTarget a, DirectiveTarget b) { return asTarget(a) | asTarget(b); }
inline uint8_t operator|(uint8_t a, DirectiveTarget b) { return a | asTarget(b); }
inline bool hasTarget(uint8_t mask, DirectiveTarget t) { return (mask & asTarget(t)) != 0; }

// When the directive takes effect.
enum class DirectiveStage : uint8_t {
    CompileTime,
    Runtime,
};

// Signature metadata for a single built-in directive.
struct DirectiveSignature {
    std::string name;
    uint8_t allowed_targets;       // bitmask of DirectiveTarget
    DirectiveStage stage;
    int min_positional = 0;
    int max_positional = 0;        // -1 = unlimited
    std::vector<std::string> named_params;  // allowed named parameter names
    // Optional per-directive validation. Called after generic checks pass.
    // Throws std::runtime_error if validation fails.
    void (*custom_validator)(const std::string &, const std::vector<DirectiveArg> &) = nullptr;
};

// Returns the registry of all built-in directive signatures.
const std::unordered_map<std::string, DirectiveSignature> &builtinDirectiveRegistry();

// Validate a directive's argument list against its signature.
// Throws std::runtime_error if validation fails.
// directiveName: the name of the directive (e.g. "native", "inline")
// args: the parsed argument list
void validateDirectiveArgs(const std::string &directiveName,
                           const std::vector<DirectiveArg> &args);

}  // namespace ry
