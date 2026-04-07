#include "ry/directive_meta.hpp"
#include <stdexcept>

namespace ry {

// ===== Directive helper implementations =====

std::string getDirectivePositionalArg(const std::vector<Directive> &directives,
                                      std::string_view name) {
    for (const auto &d : directives) {
        if (d.name == name) {
            for (const auto &a : d.args) {
                if (!a.name.has_value() && a.value) {
                    if (auto *s = std::get_if<StringExpr>(&a.value->data))
                        return s->value;
                }
            }
            return "";
        }
    }
    return "";
}

const ExprNode *getDirectiveNamedArg(const std::vector<Directive> &directives,
                                     std::string_view directive_name,
                                     std::string_view arg_name) {
    for (const auto &d : directives) {
        if (d.name == directive_name) {
            for (const auto &a : d.args) {
                if (a.name.has_value() && *a.name == arg_name && a.value)
                    return a.value.get();
            }
            return nullptr;
        }
    }
    return nullptr;
}

// Returns the first positional ExprNode from a directive arg list, or nullptr.
static const ExprNode *firstPositionalExpr(const std::vector<DirectiveArg> &args) {
    for (const auto &a : args)
        if (!a.name.has_value() && a.value) return a.value.get();
    return nullptr;
}

// ===== Built-in directive registry =====

const std::unordered_map<std::string, DirectiveSignature> &builtinDirectiveRegistry() {
    using T = DirectiveTarget;
    static const std::unordered_map<std::string, DirectiveSignature> registry = {
        {"deprecated", {"deprecated",
            T::Function | T::Record | T::Field | T::Statement,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"reason"}}},

        {"native", {"native",
            T::Function | T::Statement,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/1, {},
            [](const std::string &, const std::vector<DirectiveArg> &args) {
                if (const ExprNode *p = firstPositionalExpr(args)) {
                    if (auto *s = std::get_if<StringExpr>(&p->data)) {
                        if (s->value.empty())
                            throw std::runtime_error("@native library name must not be empty");
                    } else {
                        throw std::runtime_error("@native expects a string literal argument");
                    }
                }
            }}},

        {"inline", {"inline",
            asTarget(T::Function),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"mode"}}},

        {"parallel", {"parallel",
            asTarget(T::ForLoop),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {}}},

        {"each", {"each",
            T::Statement | T::Function,
            DirectiveStage::CompileTime,
            /*min_pos=*/1, /*max_pos=*/1, {}}},

        {"property", {"property",
            T::Statement | T::Function,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"count"}}},

        {"const", {"const",
            asTarget(T::Statement),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {}}},

        {"it", {"it",
            asTarget(T::Function),
            DirectiveStage::CompileTime,
            /*min_pos=*/1, /*max_pos=*/1, {},
            [](const std::string &, const std::vector<DirectiveArg> &args) {
                if (const ExprNode *p = firstPositionalExpr(args)) {
                    if (!std::get_if<StringExpr>(&p->data))
                        throw std::runtime_error("@it expects a string literal description");
                }
            }}},

        {"describe", {"describe",
            asTarget(T::Function),
            DirectiveStage::CompileTime,
            /*min_pos=*/1, /*max_pos=*/1, {},
            [](const std::string &, const std::vector<DirectiveArg> &args) {
                if (const ExprNode *p = firstPositionalExpr(args)) {
                    if (!std::get_if<StringExpr>(&p->data))
                        throw std::runtime_error("@describe expects a string literal description");
                }
            }}},
    };
    return registry;
}

// ===== Directive argument validation =====

void validateDirectiveArgs(const std::string &directiveName,
                           const std::vector<DirectiveArg> &args) {
    const auto &registry = builtinDirectiveRegistry();
    auto it = registry.find(directiveName);
    if (it == registry.end())
        throw std::runtime_error("unknown directive '@" + directiveName + "'");

    const DirectiveSignature &sig = it->second;

    // Count positional and named args
    int positional = 0;
    for (const auto &a : args) {
        if (!a.name.has_value()) {
            ++positional;
        } else {
            // Check named arg is allowed
            bool found = false;
            for (const auto &np : sig.named_params)
                if (np == *a.name) { found = true; break; }
            if (!found)
                throw std::runtime_error(
                    "unknown named argument '" + *a.name +
                    "' for directive '@" + directiveName + "'");
        }
    }

    // Check positional count
    if (positional < sig.min_positional)
        throw std::runtime_error(
            "@" + directiveName + " requires at least " +
            std::to_string(sig.min_positional) + " positional argument(s)");
    if (sig.max_positional >= 0 && positional > sig.max_positional)
        throw std::runtime_error(
            "@" + directiveName + " accepts at most " +
            std::to_string(sig.max_positional) + " positional argument(s)");

    // Per-directive custom validation
    if (sig.custom_validator)
        sig.custom_validator(directiveName, args);
}

}  // namespace ry
