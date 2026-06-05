# Runtime Boundary

This document categorizes the runtime boundary so that selected native runtime modules can later be rewritten in Rust behind stable `extern "C"` symbols. It is a deliverable of issue #1820 (v0.0.26 stage 2) and the planning reference for incremental Rust migration. The orthogonal LLVM IR emission boundary is documented in [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md).

## Contract

- **All runtime entry points are `extern "C"` symbols prefixed `__ry_`.** This is the stable contract: codegen lowers user-level operations into calls to these symbols, and the runtime owns their implementation.
- **The boundary is C-only.** Parameters and return types are limited to scalar types (`int64_t`, `double`, `bool` as `int8_t`), opaque pointers, and POD structs whose layout is fixed by `include/ry/runtime/{core,native}/*.hpp`. LLVM types must not appear on either side.
- **Memory ownership crosses the boundary via the ARC contract.** Allocations made by the runtime carry an `ArcHeader` (or `StringHeader` for `str`) at a fixed negative offset; codegen emits the matching retain/release calls. See `.claude/rules/codegen-arc-cow.md` for the full contract.

## Two-category split

The runtime is organized into two top-level subdirectories, established by the #1829 reorganization. The split directly maps to Rust-migration prioritization.

### Core runtime (`src/runtime/core/`, `include/ry/runtime/core/`)

Memory-safety-critical primitives shared by every Ry program. These see the highest exercise from the test suite and the fuzzer harnesses, and they are the largest source of subtle bugs (off-by-one, lifetime, race) — making them attractive Rust-migration targets where the borrow checker pays the most dividend.

| Module | Source | Surface |
| --- | --- | --- |
| ARC | `arc_counter.cpp` | Retain / release / atomic refcount ops; cycle-detection hooks |
| Error | `error.cpp` | `__ry_set_last_error` / `__ry_get_last_error` thread-local channel |
| String | `string.cpp`, `utf8.cpp` | UTF-8 string ops, `StringHeader`-backed allocations, codepoint-level slicing |
| Hash | `hash.cpp` | Hash-table primitives backing `Map<K, V>` / `Set<T>` |
| Any | `any.cpp`, `any_typed_coll.cpp` | `RyAny` arithmetic / comparison / type dispatch, descriptor trampolines |
| Allocation | `alloc.cpp` (header) | Core `ArcHeader`-aware allocators (`__ry_arc_alloc`, `__ry_arc_free`) |
| Parallel | `parallel.cpp` | Task spawn / join / `@parallel for` worker pool |
| Regex | `regex.cpp`, `regex_parser.cpp` | Regex compilation and matching |
| Sort | `sort.cpp` | List sort primitives |
| Print | `print.cpp` | Buffered stdout writing |

### Native modules (`src/runtime/native/`, `include/ry/runtime/native/`)

I/O-bound and platform-coupled modules. These are migration candidates of lower priority (less safety-critical than core, but well-isolated and good Rust candidates due to async/error-handling ergonomics).

| Module | Source | Surface |
| --- | --- | --- |
| Convert | `convert.cpp` | Type coercion (`__ry_str_to_int`, `__ry_str_to_float`) |
| Base64 | `base64.cpp` | Encode/decode variants |
| Net | `net.cpp` | TCP/TLS sockets, `__ry_bind` / `__ry_connect` / send/recv |
| Thread | `thread.cpp` | User-facing thread spawn/join, locks, condition variables |
| IO | `io.cpp` | File handle open/read/write/close, `__ry_io_file_*` |
| Filesystem | `filesystem.cpp` | Path/directory operations |
| Path | `path.cpp` | Path string manipulation (join/dirname/basename) |
| GC | `gc.cpp` | User-facing GC hooks |
| HTTP | `http/` | HTTP client/server (uses Net internally) |
| JSON | `json.cpp` | JSON parser/serializer |
| Testing | `testing.cpp` | Mock/spy infrastructure, assertion helpers |

## Rust migration readiness criteria

A native runtime module is a direct Rust-migration candidate when **all** of the following hold:

1. **Parameters and return types are scalar or opaque-pointer only.** Modules that touch fixed-layout POD structs (e.g. `IoFileHandle`) require the Rust side to mirror the layout via `#[repr(C)]`; this is feasible but adds layout-coupling.
2. **No internal LLVM dependency.** The runtime headers in `include/ry/runtime/{core,native}/` are LLVM-free; modules that quietly reach into codegen state are not migration-ready.
3. **No transitive C++ template dependency.** STL containers (`std::vector`, `std::unordered_map`) inside the implementation are acceptable because they do not cross the boundary; they can be replaced module-locally during the rewrite.
4. **The module's failure path uses the `__ry_set_last_error` channel uniformly.** Codegen wraps `Result<T, Error>` at the boundary by reading `__ry_get_last_error` (see [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) category 3); modules that bypass this channel (e.g. embedding panic in the runtime) break the contract.

### First migration candidates

By the criteria above, the highest-readiness native modules are:

- **Base64** (`base64.cpp`) — pure data transformation, no platform coupling, scalar/pointer-only boundary.
- **Convert** (`convert.cpp`) — string-to-number parsing, well-isolated, uses `__ry_set_last_error`.
- **Path** (`path.cpp`) — string manipulation, no I/O.
- **JSON** (`json.cpp`) — parsing/serialization, scalar/pointer-only boundary.

Higher-friction migration targets:

- **Net** / **HTTP** / **Thread** — depend on platform threading primitives; the Rust side needs an async runtime or a `std::thread` bridge.
- **IO** / **Filesystem** — POD struct layout coupling (`IoFileHandle` with raw `FILE*`).
- **Testing** — touches the test-mode codegen surface; not a pure runtime module.

Core modules are higher migration value (memory-safety dividend) but higher implementation risk (deeper interaction with `ArcHeader` and codegen-emitted call sites). They should be sequenced after at least one native module has been migrated end-to-end to validate the toolchain.

## Related documents

- [Compiler Layers](compiler-layers.md) — layer ordering and dependency direction.
- [Codegen Terminology](codegen-terminology.md) — canonical vocabulary (this page defines the runtime boundary).
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the orthogonal LLVM IR emission boundary on the codegen side.
- `.claude/rules/runtime-memory-safety.md` — runtime memory-safety rules (forbidden functions, OOM handling, NULL checks).
