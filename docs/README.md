# Ry Language Documentation

Ry is a simple programming language based on LLVM JIT. It adopts Python-style indentation block syntax, combining static typing with type inference for an easy-to-use design.

---

## Guides

Topic-oriented walkthroughs for designing Ry code. Reference pages remain the source of truth for syntax and semantics; guides focus on the "how to think about it" layer.

| Page | Contents |
|------|----------|
| [Visibility](guide/visibility.md) | Package-internal default, `@public`, cross-package import behavior, known limitations |

---

## Reference

For detailed language specifications, see the reference pages below.

| Page | Contents |
|------|----------|
| [Glossary](reference/glossary.md) | Module, Package, `package.toml`, stdlib — canonical terminology used across the reference |
| [Naming Conventions](reference/naming.md) | camelCase / PascalCase rules, approved abbreviations, acronym rule |
| [Types and Type Rules](reference/types.md) | All types, type promotion rules, type conversion |
| [Operators and Precedence](reference/operators.md) | All operators and precedence table |
| [Control Flow](reference/control-flow.md) | Complete grammar for if/else, case, while, for |
| [Functions, Lambdas, UFCS, Operator Overloading](reference/functions.md) | All forms of function definitions |
| [Records and Enums](reference/records.md) | Complete grammar for record and enum definitions |
| [Tuples, Lists, Maps, Sets](reference/collections.md) | Collection type operations |
| [Built-in Functions](reference/builtins.md) | print, len, Some, range, etc. |
| [String Functions](reference/builtins-string.md) | contains, find, replace, split, join, etc. |
| [Regular Expressions](reference/regex.md) | regexMatch, regexSearch, regexReplace, regexSplit, regexFindAll |
| [Math Functions](reference/math.md) | PI, E, sqrt, sin, cos, abs, floor, ceil, round, etc. |
| [I/O Functions](reference/io.md) | readText, writeText, exists, readBytes, toBytes, etc. |
| [JSON](reference/json.md) | parse, stringify, get, at, toStr, toInt, etc. |
| [Network (TCP)](reference/net.md) | bind, listen, accept, connect, send/receive/close for TCP sockets |
| [HTTP Server](reference/http.md) | listen, method, path, header, body, response |
| [Base64](reference/base64.md) | encode, decode, encodeUrlSafe, decodeUrlSafe |
| [Path](reference/path.md) | join, basename, dirname, ext, resolve, isAbsolute |
| [Filesystem](reference/filesystem.md) | listDir, walk, glob, copy, move, remove, mkdir, chmod, symlink |
| [Thread](reference/thread.md) | threadSpawn, threadJoin, Lock, RWLock, Semaphore, Barrier, AtomicInt, AtomicBool |
| [GC](reference/gc.md) | collect, enable, disable, setThreshold — cycle collector for ARC |
| [Module System](reference/modules.md) | from/import syntax, directory modules, std, RY_HOME |
| [Testing](reference/testing.md) | Testing with describe/it/expect |
| [Project Management](reference/project.md) | ry init and package.toml specification |
| [Design by Contract](reference/contracts.md) | require, ensure, invariant |
| [Directives](reference/directives.md) | @deprecated and other compile-time instructions |
| [Error List](reference/errors.md) | Compile errors and runtime errors |

---

## Architecture (for contributors)

Internal architecture references for working on the Ry compiler and runtime. These describe layer boundaries, the LLVM IR emission boundary, and the runtime boundary — they are not needed to write Ry programs.

| Page | Contents |
|------|----------|
| [Compiler Layers](architecture/compiler-layers.md) | lexer → parser → AST → module → sema → codegen → runtime boundary dependency direction |
| [Codegen Terminology](architecture/codegen-terminology.md) | Canonical vocabulary for the codegen stack: layers, the runtime / emission boundaries, and handle naming |
| [LLVM IR Emission Boundary](architecture/llvm-ir-emission-boundary.md) | The shared-library boundary for the LLVM IR emission layer, now implemented in Rust (#1949 / #1950 / #1993) |
| [Runtime Boundary](architecture/runtime-abi-boundary.md) | `__ry_*` `extern "C"` surface categorized by core/native for Rust migration planning |
