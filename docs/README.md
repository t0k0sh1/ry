# Ry Language Documentation

Ry is a simple programming language based on LLVM JIT. It adopts Python-style indentation block syntax, combining static typing with type inference for an easy-to-use design.

---

## Reference

For detailed language specifications, see the reference pages below.

| Page | Contents |
|------|----------|
| [Naming Conventions](reference/naming.md) | camelCase / PascalCase rules, approved abbreviations, acronym rule |
| [Types and Type Rules](reference/types.md) | All types, type promotion rules, type conversion |
| [Operators and Precedence](reference/operators.md) | All operators and precedence table |
| [Control Flow](reference/control-flow.md) | Complete grammar for if/else, case, while, for |
| [Functions, Lambdas, UFCS, Operator Overloading](reference/functions.md) | All forms of function definitions |
| [Records and Enums](reference/records.md) | Complete grammar for record and enum definitions |
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
| [Design by Contract](reference/contracts.md) | require, ensure, invariant |
| [Directives](reference/directives.md) | @deprecated and other compile-time instructions |
| [Error List](reference/errors.md) | Compile errors and runtime errors |
