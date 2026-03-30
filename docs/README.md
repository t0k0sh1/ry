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
| 04 | [Control Flow](tutorial/04-control-flow.md) | if/else, when, while, for/range, break/continue |
| 05 | [Functions](tutorial/05-functions.md) | fn definitions, recursion, overloading, lambdas, UFCS |
| 06 | [Records and Enums](tutorial/06-records.md) | type definitions, field access, methods, enum |
| 07 | [Collections](tutorial/07-collections.md) | Tuples, lists, maps, sets |
| 08 | [Advanced Features](tutorial/08-advanced.md) | Closures, operator overloading, Option type |
| 09 | [Packages](tutorial/09-modules.md) | Packages, std library, directory packages |
| 10 | [Design by Contract](tutorial/10-contracts.md) | require, ensure, invariant, old, result |
| 11 | [Testing](tutorial/11-testing.md) | describe/it/expect, matchers, mock/verify |

For setup and build instructions, see [01 - Getting Started](tutorial/01-getting-started.md).

---

## Reference

For detailed language specifications, see the reference pages below.

| Page | Contents |
|------|----------|
| [Types and Type Rules](reference/types.md) | All types, type promotion rules, type conversion |
| [Operators and Precedence](reference/operators.md) | All operators and precedence table |
| [Control Flow](reference/control-flow.md) | Complete grammar for if/else, when, while, for |
| [Functions, Lambdas, UFCS, Operator Overloading](reference/functions.md) | All forms of function definitions |
| [Structs and Enums](reference/structs.md) | Complete grammar for type and enum definitions |
| [Tuples, Lists, Maps, Sets](reference/collections.md) | Collection type operations |
| [Built-in Functions](reference/builtins.md) | print, length, Some, range, etc. |
| [String Functions](reference/builtins-string.md) | contains, find, replace, split, join, etc. |
| [Regular Expressions](reference/regex.md) | regex_match, regex_search, regex_replace, regex_split, regex_find_all |
| [Math Functions](reference/math.md) | PI, E, sqrt, sin, cos, abs, floor, ceil, round, etc. |
| [I/O Functions](reference/io.md) | read_text, write_text, file_exists, read_bytes, str_to_bytes, etc. |
| [JSON](reference/json.md) | parse, stringify, json_get, json_at, json_str, json_int, etc. |
| [Network (TCP)](reference/net.md) | bind, listen, accept, connect, send/recv/close for TCP sockets |
| [HTTP Server](reference/http.md) | http_listen, http_method, http_path, http_header, http_body, http_response |
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
