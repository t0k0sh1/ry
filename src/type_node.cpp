#include "ry/ast.hpp"

std::string TypeNode::toString() const {
    return std::visit([](const auto &v) -> std::string {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, BasicType>) {
            return v.name;
        } else if constexpr (std::is_same_v<T, GenericType>) {
            std::string result = v.name + "<";
            for (size_t i = 0; i < v.type_args.size(); ++i) {
                if (i > 0) result += ", ";
                result += v.type_args[i]->toString();
            }
            result += ">";
            return result;
        } else if constexpr (std::is_same_v<T, ArrayType>) {
            return "[" + v.element_type->toString() + "; " + std::to_string(v.size) + "]";
        } else if constexpr (std::is_same_v<T, TupleType>) {
            std::string result = "(";
            for (size_t i = 0; i < v.elements.size(); ++i) {
                if (i > 0) result += ", ";
                result += v.elements[i]->toString();
            }
            result += ")";
            return result;
        } else if constexpr (std::is_same_v<T, FnType>) {
            std::string result = "fn(";
            for (size_t i = 0; i < v.param_types.size(); ++i) {
                if (i > 0) result += ", ";
                result += v.param_types[i]->toString();
            }
            result += ")";
            if (v.return_type)
                result += " -> " + v.return_type->toString();
            return result;
        } else if constexpr (std::is_same_v<T, UnionType>) {
            std::string result;
            for (size_t i = 0; i < v.components.size(); ++i) {
                if (i > 0) result += " | ";
                result += v.components[i]->toString();
            }
            return result;
        } else if constexpr (std::is_same_v<T, OptionalType>) {
            return v.inner->toString() + "?";
        } else if constexpr (std::is_same_v<T, RangeType>) {
            return v.start + ".." + v.end;
        }
    }, data);
}

TypeNodePtr TypeNode::makeBasic(std::string name) {
    auto node = std::make_unique<TypeNode>();
    node->data = BasicType{std::move(name)};
    return node;
}

TypeNodePtr TypeNode::makeGeneric(std::string name, std::vector<TypeNodePtr> args) {
    auto node = std::make_unique<TypeNode>();
    node->data = GenericType{std::move(name), std::move(args)};
    return node;
}

TypeNodePtr TypeNode::makeArray(TypeNodePtr elem, uint64_t size) {
    auto node = std::make_unique<TypeNode>();
    node->data = ArrayType{std::move(elem), size};
    return node;
}

TypeNodePtr TypeNode::makeTuple(std::vector<TypeNodePtr> elems) {
    auto node = std::make_unique<TypeNode>();
    node->data = TupleType{std::move(elems)};
    return node;
}

TypeNodePtr TypeNode::makeFn(std::vector<TypeNodePtr> params, TypeNodePtr ret) {
    auto node = std::make_unique<TypeNode>();
    node->data = FnType{std::move(params), std::move(ret)};
    return node;
}

TypeNodePtr TypeNode::makeUnion(std::vector<TypeNodePtr> comps) {
    auto node = std::make_unique<TypeNode>();
    node->data = UnionType{std::move(comps)};
    return node;
}

TypeNodePtr TypeNode::makeOptional(TypeNodePtr inner) {
    auto node = std::make_unique<TypeNode>();
    node->data = OptionalType{std::move(inner)};
    return node;
}

TypeNodePtr TypeNode::makeRange(std::string start, std::string end) {
    auto node = std::make_unique<TypeNode>();
    node->data = RangeType{std::move(start), std::move(end)};
    return node;
}

TypeNodePtr TypeNode::clone(const TypeNodePtr &src) {
    if (!src) return nullptr;
    return std::visit([](const auto &v) -> TypeNodePtr {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, BasicType>) {
            return makeBasic(v.name);
        } else if constexpr (std::is_same_v<T, GenericType>) {
            std::vector<TypeNodePtr> args;
            for (auto &a : v.type_args) args.push_back(clone(a));
            return makeGeneric(v.name, std::move(args));
        } else if constexpr (std::is_same_v<T, ArrayType>) {
            return makeArray(clone(v.element_type), v.size);
        } else if constexpr (std::is_same_v<T, TupleType>) {
            std::vector<TypeNodePtr> elems;
            for (auto &e : v.elements) elems.push_back(clone(e));
            return makeTuple(std::move(elems));
        } else if constexpr (std::is_same_v<T, FnType>) {
            std::vector<TypeNodePtr> params;
            for (auto &p : v.param_types) params.push_back(clone(p));
            return makeFn(std::move(params), clone(v.return_type));
        } else if constexpr (std::is_same_v<T, UnionType>) {
            std::vector<TypeNodePtr> comps;
            for (auto &c : v.components) comps.push_back(clone(c));
            return makeUnion(std::move(comps));
        } else if constexpr (std::is_same_v<T, OptionalType>) {
            return makeOptional(clone(v.inner));
        } else if constexpr (std::is_same_v<T, RangeType>) {
            return makeRange(v.start, v.end);
        }
    }, src->data);
}
