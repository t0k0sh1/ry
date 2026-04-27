#include "ry/directive_meta.hpp"
#include <algorithm>
#include <stdexcept>
#include <unordered_set>

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
        {"native", {"native",
            T::Function | T::Statement,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/1, {}, /*positional_param_names=*/{},
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

        {"each", {"each",
            T::Statement | T::Function,
            DirectiveStage::CompileTime,
            /*min_pos=*/1, /*max_pos=*/1, {}, /*positional_param_names=*/{}}},

        {"property", {"property",
            T::Statement | T::Function,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"count"}, /*positional_param_names=*/{}}},

        {"deprecated", {"deprecated",
            T::Function | T::Record | T::Field | T::Statement,
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"reason"}, /*positional_param_names=*/{}}},

        {"inline", {"inline",
            asTarget(T::Function),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {"mode"}, /*positional_param_names=*/{}}},

        {"parallel", {"parallel",
            asTarget(T::ForLoop),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {}, /*positional_param_names=*/{}}},

        {"const", {"const",
            asTarget(T::Statement),
            DirectiveStage::CompileTime,
            /*min_pos=*/0, /*max_pos=*/0, {}, /*positional_param_names=*/{}}},
    };
    return registry;
}

// ===== Directive target name mapping =====

uint8_t directiveTargetMask(std::string_view name) {
    if (name == "function")  return asTarget(DirectiveTarget::Function);
    if (name == "record")    return asTarget(DirectiveTarget::Record);
    if (name == "field")     return asTarget(DirectiveTarget::Field);
    if (name == "statement") return asTarget(DirectiveTarget::Statement);
    if (name == "for")       return asTarget(DirectiveTarget::ForLoop);
    return 0;
}

// ===== Directive argument validation =====

void validateDirectiveSignature(const std::string &directiveName,
                                const std::vector<DirectiveArg> &args,
                                const DirectiveSignature &sig) {
    auto contains = [](const std::vector<std::string> &v, const std::string &n) {
        return std::find(v.begin(), v.end(), n) != v.end();
    };

    int positional = 0;
    std::unordered_set<std::string> named_seen;
    for (const auto &a : args) {
        if (!a.name.has_value()) {
            ++positional;
        } else {
            if (!contains(sig.positional_param_names, *a.name) &&
                !contains(sig.named_params, *a.name))
                throw std::runtime_error(
                    "unknown named argument '" + *a.name +
                    "' for directive '@" + directiveName + "'");
            if (!named_seen.insert(*a.name).second)
                throw std::runtime_error(
                    "duplicate named argument '" + *a.name +
                    "' for directive '@" + directiveName + "'");
        }
    }

    if (positional < sig.min_positional)
        throw std::runtime_error(
            "@" + directiveName + " requires at least " +
            std::to_string(sig.min_positional) + " positional argument(s)");
    if (sig.max_positional >= 0 && positional > sig.max_positional)
        throw std::runtime_error(
            "@" + directiveName + " accepts at most " +
            std::to_string(sig.max_positional) + " positional argument(s)");

    for (size_t i = 0; i < sig.positional_param_names.size(); ++i) {
        const std::string &pname = sig.positional_param_names[i];
        bool by_pos = static_cast<int>(i) < positional;
        bool by_name = named_seen.count(pname) > 0;
        if (by_pos && by_name)
            throw std::runtime_error(
                "argument '" + pname + "' for directive '@" + directiveName +
                "' provided both positionally and by name");
        if (!by_pos && !by_name)
            throw std::runtime_error(
                "@" + directiveName + " missing required argument '" + pname + "'");
    }

    if (sig.custom_validator)
        sig.custom_validator(directiveName, args);
}

void validateDirectiveArgs(const std::string &directiveName,
                           const std::vector<DirectiveArg> &args) {
    const auto &registry = builtinDirectiveRegistry();
    auto it = registry.find(directiveName);
    if (it == registry.end())
        throw std::runtime_error("unknown directive '@" + directiveName + "'");
    validateDirectiveSignature(directiveName, args, it->second);
}

}  // namespace ry
