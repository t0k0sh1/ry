[English](README.md) | [日本語](ja/README.md) | [繁體中文](zh/README.md)

# Ry Language Documentation

Ry is a simple programming language based on LLVM JIT. It adopts Python-style indentation block syntax, combining static typing with type inference for an easy-to-use design.

---

## Tutorial

If you are new to Ry, please read through these in order.

| # | Title | Contents |
|---|-------|----------|
| 01 | [Getting Started](tutorial/01-getting-started.md) | Setup, Hello World, comments |
| 02 | [Variables and Types](tutorial/02-variables-and-types.md) | Variable declarations, constants, type annotations, basic types |
| 03 | [Operators](tutorial/03-operators.md) | Arithmetic, comparison, logical, bitwise, compound assignment operators |
| 04 | [Control Flow](tutorial/04-control-flow.md) | if/elif/else, while, for/range, break/continue |
| 05 | [Functions](tutorial/05-functions.md) | fn definitions, recursion, overloading, lambdas, UFCS |
| 06 | [Structs and Enums](tutorial/06-structs.md) | type definitions, field access, methods, enum |
| 07 | [Collections](tutorial/07-collections.md) | Tuples, lists, maps, sets |
| 08 | [Advanced Features](tutorial/08-advanced.md) | Closures, operator overloading, Option type |
| 09 | [Modules](tutorial/09-modules.md) | Module splitting with from/import |

For setup and build instructions, see [01 - Getting Started](tutorial/01-getting-started.md).

---

## Reference

For detailed language specifications, see the reference pages below.

| Page | Contents |
|------|----------|
| [Types and Type Rules](reference/types.md) | All types, type promotion rules, type conversion |
| [Operators and Precedence](reference/operators.md) | All operators and precedence table |
| [Control Flow](reference/control-flow.md) | Complete grammar for if, while, for |
| [Functions, Lambdas, UFCS, Operator Overloading](reference/functions.md) | All forms of function definitions |
| [Structs and Enums](reference/structs.md) | Complete grammar for type and enum definitions |
| [Tuples, Lists, Maps, Sets](reference/collections.md) | Collection type operations |
| [Built-in Functions](reference/builtins.md) | print, len, Some, range, etc. |
| [String Functions](reference/builtins-string.md) | contains, find, replace, split, join, etc. |
| [Package System](reference/packages.md) | from/import syntax, directory packages, std, RY_HOME |
| [Testing](reference/testing.md) | Testing with describe/it/expect |
| [Project Management](reference/project.md) | ry init and ry.toml specification |
| [Design by Contract](reference/contracts.md) | require, ensure, invariant, old, result |
| [Directives](reference/directives.md) | @deprecated and compile-time metadata |
| [Error List](reference/errors.md) | Compile errors and runtime errors |
