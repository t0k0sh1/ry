[English](README.md) | [日本語](ja/README.md) | [繁體中文](zh/README.md)

# Ry Language Documentation

Ry is a simple programming language based on LLVM JIT. It adopts Python-style indentation block syntax, combining static typing with type inference for an easy-to-use design.

---

## Tutorial

If you are new to Ry, please read through these in order.

| # | Title | Contents |
|---|-------|----------|
| 01 | [Getting Started](tutorial/01-getting-started.md) | Setup, Hello World, comments |
| 02 | [Variables and Types](tutorial/02-variables-and-types.md) | Variable declarations, constants, type annotations, basic types, f-strings, type casting |
| 03 | [Operators](tutorial/03-operators.md) | Arithmetic, comparison, logical, bitwise, compound assignment operators |
| 04 | [Control Flow](tutorial/04-control-flow.md) | if/else, case, while, for/range, break/continue |
| 05 | [Functions](tutorial/05-functions.md) | function definitions, recursion, overloading, default args, lambdas, closures, higher-order, UFCS |
| 06 | [Records and Enums](tutorial/06-records.md) | type definitions, field access, enum, ADT, generic enums, operator overloading |
| 07 | [Collections and Iterators](tutorial/07-collections.md) | Tuples, lists, maps, sets, lazy iterators |
| 08 | [Error Handling](tutorial/08-error-handling.md) | Option, Result, ? operator, design by contract |
| 09 | [Packages](tutorial/09-modules.md) | Packages, std library, directory packages |
| 10 | [Concurrency](tutorial/10-concurrency.md) | async/await, @parallel, threads, networking |
| 11 | [Testing](tutorial/11-testing.md) | describe/it/expect, matchers, mock/verify, contract testing |
| 12 | [Building a Project](tutorial/12-building-a-project.md) | Hands-on CLI project combining all features |

For setup and build instructions, see [01 - Getting Started](tutorial/01-getting-started.md).

---

## Reference

For detailed language specifications, see the reference pages below.

| Page | Contents |
|------|----------|
| [Types and Type Rules](reference/types.md) | All types, type promotion rules, type conversion |
| [Operators and Precedence](reference/operators.md) | All operators and precedence table |
| [Control Flow](reference/control-flow.md) | Complete grammar for if/else, case, while, for |
| [Functions, Lambdas, UFCS, Operator Overloading](reference/functions.md) | All forms of function definitions |
| [Records and Enums](reference/structs.md) | Complete grammar for record and enum definitions |
| [Tuples, Lists, Maps, Sets](reference/collections.md) | Collection type operations |
| [Built-in Functions](reference/builtins.md) | print, length, Some, range, etc. |
| [String Functions](reference/builtins-string.md) | contains, find, replace, split, join, etc. |
| [Regular Expressions](reference/regex.md) | regex_match, regex_search, regex_replace, regex_split, regex_find_all |
| [Math Functions](reference/math.md) | PI, E, sqrt, sin, cos, abs, floor, ceil, round, etc. |
| [I/O Functions](reference/io.md) | read_text, write_text, exists, read_bytes, to_bytes, etc. |
| [JSON](reference/json.md) | parse, stringify, get, at, to_str, to_int, etc. |
| [Network (TCP)](reference/net.md) | bind, listen, accept, connect, send/receive/close for TCP sockets |
| [HTTP Server](reference/http.md) | listen, method, path, header, body, response |
| [Base64](reference/base64.md) | encode, decode, encode_url_safe, decode_url_safe |
| [Path](reference/path.md) | join, basename, dirname, extension, resolve, is_absolute |
| [Filesystem](reference/filesystem.md) | list_dir, walk, glob_files, copy, move, remove, make_dir, chmod, symlink |
| [Thread](reference/thread.md) | thread_spawn, thread_join, Lock, RWLock, Semaphore, Barrier, AtomicInt, AtomicBool |
| [GC](reference/gc.md) | collect, enable, disable, set_threshold — cycle collector for ARC |
| [Package System](reference/packages.md) | from/import syntax, directory packages, std, RY_HOME |
| [Testing](reference/testing.md) | Testing with describe/it/expect |
| [Project Management](reference/project.md) | ry init and package.toml specification |
| [Design by Contract](reference/contracts.md) | require, ensure, invariant, old, result |
| [Directives](reference/directives.md) | @deprecated and compile-time metadata |
| [Error List](reference/errors.md) | Compile errors and runtime errors |
