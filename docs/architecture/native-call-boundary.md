# Native Call Boundary

This document is the working hypothesis for the **native-call boundary** — the layer where the codegen lowering side decides which `__ry_*` runtime symbol to call, what to wrap the result in, what error channel to consult, and what resource kind to tag — as a precondition for migrating the upper codegen layer (statement / expression / lowering) to Rust.

This is a **working hypothesis, not a graduation document**. The final descriptor shape and migration shape are recorded only after the pilot in the follow-on implementation issues lands, per [Layer Graduation Workflow](layer-graduation-workflow.md) §"When to write the graduation document".

Issue #2231 is the **design** issue; implementation lives in the follow-on issues listed in §"Follow-up implementation issues".

## Why this boundary exists (and is distinct from emission)

The emission boundary's "(ii) discipline" — **prefer generic LLVM primitives over coarse, semantics-carrying descriptors** — is recorded in [Codegen Layering Plan](codegen-layering-plan.md) §"Lowered IR vocabulary" alongside the A–E pilot decisions that established it. The reconciliation specific to this boundary is:

- **The (ii) discipline applies to the *emission* boundary** — the layer that constructs LLVM IR. The reason coarse descriptors were rejected there is that an `IRBuilder<>::Create*` consumer should not need to know that a list-`take` op is being lowered; it just needs `alloca` + `load` + `gep` + `icmp` + `br`.
- **The native-call boundary is the *lowering* side's output, not emission's input.** The lowered IR vocabulary in [Codegen Layering Plan](codegen-layering-plan.md) already includes the `RuntimeCall` op — "a Ry-level call resolved to a `__ry_*` runtime symbol with a structured signature and return-wrapping policy". The descriptor proposed here is the **direct extension** of that op with the fields the lowering layer already computes (error channel, resource kind, byte-list gate, mockability). The descriptor never crosses into emission; it terminates inside the lowering layer's dispatch logic and dissolves into a `ry_emit_runtime_call` boundary call plus the (already-migrated) `Result` / resource-tagging composite ops.
- **The descriptor's existence is justified by the current code, not invented.** The implicit descriptor already exists, scattered: `NativeDispatchEntry` (`include/ry/codegen_native_dispatch.hpp:21-41`), `NativeFnSignature` (`include/ry/codegen.hpp:62`), `ResourceKindRegistry` (`src/codegen_call_io.cpp:9-31` and `src/codegen_call_thread.cpp:9-19`), per-dispatcher `customEmitter` lambdas, and manual `used_native_libraries_.insert(...)` calls. Consolidating these into a single declarative form is a refactor of existing state, not new semantic surface.
- **The (ii) discipline still applies — to what goes *into* the descriptor.** Anything that requires type introspection at lowering time (the `math::abs` int-vs-float overload, `json::stringify` polymorphism, `thread::threadSpawn` LLVM `Function` synthesis) cannot be a declarative descriptor field. Those cases are split out as **compiler-builtins**, leaving the descriptor strictly declarative. This is the same principle, applied to a different boundary: declarative selectors → descriptor; type-driven dispatch → compiler builtin.

## Distinct from `runtime-abi-boundary.md`

[Runtime Boundary](runtime-abi-boundary.md) owns the `__ry_*` `extern "C"` ABI between runtime libraries and everything else (the contract, the core/native split, Rust-migration readiness). It does not own dispatch selection on the codegen side. This document owns codegen → native **dispatch selection**: which symbol gets called, with what wrapping, for a given Ry-level `@native` invocation. The two documents are co-maintained but non-overlapping; this one does not re-list the runtime modules or duplicate the ABI contract.

## Gap reframing

Issue #2231 proposes that natives be "packaged as libraries by stdlib module unit". That artifact model **already exists** — see [Runtime Boundary](runtime-abi-boundary.md) §"Native modules" for the canonical module list, and the `add_ry_native_lib` macro in `CMakeLists.txt` plus the `RY_NATIVE_LIBS` list for the eleven `libry_<mod>.{dylib,so}` build targets. The JIT loader consumes `CodeGen::getRequiredLibraries()` and resolves each through `find_native_library` in `src/project/paths.cpp`. This document does not propose building the artifact model; it proposes consolidating the dispatch coupling that the artifact model has accumulated. The four real gaps are:

- **(G1) Make the implicit descriptor explicit.** Replace the scattered `NativeDispatchEntry` + `NativeFnSignature` + `ResourceKindRegistry` + `customEmitter` form with one `NativeCallDescriptor` consumed by one dispatch path.
- **(G2) Make library identity declaration-driven.** Today `convert.ry` uses bare `@native` but ships a separate `libry_convert.dylib`. The descriptor-driven inference rule below makes the library identity expressible from declaration state alone, so any code path that *does* build a descriptor can register the library automatically. The `used_native_libraries_.insert("convert")` calls hard-coded inside `emitBuiltinConversion` sit on a different code path — Pattern B (defined below) — that does not build descriptors and is **explicitly out of scope** (see §"Pattern B carve-out").
- **(G3) Decouple codegen from per-module `customEmitter` knowledge.** The A2 `customEmitter` lambdas that just bundle "return wrapping + error channel + resource tag" (most of base64 / path / io / net / http) belong in descriptor fields. The ones that require type introspection (`math::abs` overload, `json::stringify` polymorphism, `thread::threadSpawn` Function synthesis) belong with compiler builtins.
- **(G4) Document the exceptions.** Four artifact-model anomalies must stay in scope of the descriptor's contract even though they do not fit: `testing` is loaded via `absoluteSymbols` (in `src/jit/jit_runner.cpp`), not `dlopen`; `http` duplicates `src/runtime/core/hash.cpp` into its shared library (`CMakeLists.txt` `add_ry_native_lib(http ...)`); `math` has no shared library at all; `runtime_internal` ships its symbols inside `ry_lib` plus an explicit `absoluteSymbols` guard for Linux.

## `NativeCallDescriptor`

The descriptor's field-level shape (intent, not C++ representation — concrete types are an implementation-issue concern):

| Field | Shape | Source today |
|---|---|---|
| `module_name` | string | derived from the declaration's containing module (filesystem-driven) |
| `library_name` | optional string | `@native("<lib>")` tag if present (rule (a) below); else inferred per rule (b); absent for `ry_lib`-resolved symbols |
| `exported_symbol` | string | currently the `rtSuffix` field in `NativeDispatchEntry`, or `__ry_<pkg>_<name>` for Pattern C |
| `signature` | reuse existing `NativeFnSignature` | already defined in `include/ry/codegen.hpp` — keep the field, do not duplicate |
| `return_wrapping` | `ReturnWrapping` enum | already exists; `Direct` / `ResultPtr` / `ResultStatus` / `ResultOutParam` / `BoolFromI64` / `ResultPtrWithListMeta` |
| `error_channel` | string | currently per-emitter (`__ry_get_last_error` default; 6 module-specific channels enumerated below) |
| `resource_kind` | optional registry index | currently a per-emitter call to `addResourceKind(res, rk_X)` after `wrapPtrAsResult` |
| `require_list_u8_arg` | optional arg index | already exists in `NativeDispatchEntry` |
| `mockable` | bool | currently inferred from `test_mode_` + customEmitter path |
| `overload_group_id` | optional group id | enables multi-arity grouping (current `math_table` ad-hoc) |

**Not included** (these stay outside the descriptor):

- **`customEmitter` callback.** The whole point of consolidation; entries that need it become compiler-builtins.
- **Type-driven dispatch.** Anything that needs to inspect the static type of an argument to pick the symbol — `math::abs(int)` vs `math::abs(float)`, `json::stringify` over arbitrary Ry values, `thread::threadSpawn`'s body-typed result. Those move to compiler builtins (§"Native vs compiler builtin").
- **`first-class @native thunk` metadata.** `materializeNativeThunk` (`src/codegen_lambda.cpp`) is a *consumer* of descriptors, not a descriptor field — it synthesizes a `CallExpr` and routes it back through the descriptor-driven dispatch path. The thunk path is unchanged in shape; only the descriptor it consumes changes.

## Library inference rule

The two existing `@native` forms — bare and `@native("<lib>")` — are both kept (user judgment, 2026-06-19). Library identity is resolved per:

- **(a) Explicit tag wins.** If the declaration is `@native("<lib>")`, `library_name = "<lib>"`.
- **(b) Otherwise key on the declaring module.** Let `<M>` be the module the declaration sits in (the `share/std/<M>.ry` or `share/std/<M>/<M>.ry` file path). If a `libry_<M>` build artifact exists in `RY_NATIVE_LIBS`, set `library_name = "<M>"`; otherwise leave it absent (the symbol resolves through `ry_lib` and the in-process `DynamicLibrarySearchGenerator`, no dlopen).

Worked examples:

- `share/std/convert.ry` declares bare `@native`. Module `convert`. `libry_convert` is in `RY_NATIVE_LIBS`. → `library_name = "convert"`. Any **descriptor-driven** dispatch path consuming this declaration registers the library automatically. Pattern B's `int`/`float` interception is unaffected (see §"Pattern B carve-out").
- `share/std/str.ry` declares bare `@native`. Module `str`. No `libry_str` artifact. → absent. Symbols resolve through the in-process search generator. No dispatch change.
- `share/std/math/math.ry` declares bare `@native`. No `libry_math` artifact. → absent. Inline / libc emission; no library load.
- `share/std/io/io.ry` declares `@native("io")`. → `library_name = "io"` by rule (a).

**Why not symbol-prefix inference.** A tempting alternative is "parse `__ry_<prefix>_*` and look `<prefix>` up against registered packages." It is **structurally broken** in this codebase: `convert`'s workhorse symbols are `__ry_str_to_int` / `__ry_str_to_float` (prefix `str`), `net`'s sockets are `__ry_bind` / `__ry_connect` (no module prefix), and the camelCase / snake_case split documented in `.claude/rules/docs-reference-conventions.md` further fragments the prefix. The declaration's module is the only invariant that survives the historical symbol-naming inconsistency.

**Caveat.** Rule (b) keys on "the declaring module has a `libry_<M>` build artifact", not "this specific function uses that artifact." Rule (b) attaches `library_name = "convert"` to a descriptor for `str(v: int)`, but Pattern B never builds a descriptor for that call (§"Pattern B carve-out"), so the attachment is inert. If a future change routes such calls through descriptor dispatch, an explicit `@native("")` empty-tag opt-out (or a function-level `@native(symbol="...")` override) is the documented escape.

**Symbol naming consistency** is intentionally not addressed by this document. The mixed camelCase (`filesystem` / `path` / `gc` / `json`) and snake_case (`base64` / `io` / `net` / `str`) convention is recorded as a follow-up question, not resolved here. An optional `@native("io", symbol="__ry_io_file_open")` opt-in field is reserved as future syntax; the v1 descriptor derives `exported_symbol` from the declaration name per the current per-module convention.

## Current dispatch path inventory

Four patterns coexist today. The descriptor consumes A1 / Pattern C cleanly, audits A2 entry-by-entry, and leaves Pattern B (compiler builtins) untouched.

- **Pattern A1 — table-driven, no `customEmitter`.** `NativeDispatchEntry` with `customEmitter == nullptr`. Tables: `io_table` (`src/codegen_call_io.cpp:340`), `math_table` (`src/codegen_call.cpp:1484`), `path_table` (`src/codegen_call_io.cpp:1215`), `base64_table` (`src/codegen_call_base64.cpp:9`). **Mechanical descriptor conversion** — every field maps 1:1.
- **Pattern A2 — table-driven with `customEmitter`.** Same `NativeDispatchEntry` but the `customEmitter` field is set; the table's arity / type-check gate is bypassed (per `.claude/rules/codegen-stdlib-dispatcher.md`). Tables containing A2 entries: `math_table` (`abs` / `floor` / `ceil` / `round` / `log` / `pow` / `isNan` / `isInf` / `digits`), `json_table` (`src/codegen_call_json.cpp:173`), `net_table` (`src/codegen_call_io.cpp:547`, all entries), `http_table` (`src/codegen_call_io.cpp:1129`, all entries), `thread_table` (`src/codegen_call_thread.cpp:635`, all entries). **Audit case-by-case** in installment 3 (§"Migration order").
- **Pattern B — hand-written `emitBuiltin*` helpers.** Name-keyed dispatch, no `.ry` declaration, no `NativeFnSignature` consultation. Examples: `emitBuiltinConversion` / `emitBuiltinQuery` / `emitBuiltinCore` / `emitBuiltinString` / `emitBuiltinHigherOrder` / `emitBuiltinCollection` / `emitBuiltinIterator` / `emitBuiltinResult` / `emitBuiltinOption` / `emitBuiltinSetOps` / `emitBuiltinRegex`. The reserved-name guard at `include/ry/builtin_names.hpp:10` (`kReservedBuiltinFunctionNames`) prevents user fns from shadowing them. **Out of descriptor scope** — these are not `@native` and the boundary does not touch them.
- **Pattern C — `emitGenericNativeCall` fallback.** `src/codegen_call_native.cpp:488`. Looks up `native_lib_index_[callee]` → iterates `native_fn_sigs_["<pkg>::<name>"]` for arity / type match → infers `ReturnWrapping` from `sig.returnTypeName`. **Most mechanical descriptor conversion** — the inference already runs on declaration state.

## Cross-cutting concerns absorbed into the descriptor

This section gives the current code locations for each descriptor field whose source isn't already named in the table above.

- **Error channels (7 today).** `__ry_get_last_error` (default), `__ry_net_get_last_error`, `__ry_tls_get_last_error`, `__ry_http_get_last_error`, `__ry_base64_get_last_error`, `__ry_convert_get_last_error`, `__ry_regex_get_last_error`. Each becomes the `error_channel` field of its descriptor. The `emitPtrToResult` deprecation rule applies here — see `.claude/rules/codegen-stdlib-dispatcher.md`.
- **Resource kinds.** `ResourceKindRegistry::registerKind` calls in `src/codegen_call_io.cpp` and `src/codegen_call_thread.cpp` keep their static registration; their integer keys become `resource_kind` values. Manual `addResourceKind(res, rk_X)` after each `wrapPtrAsResult` is replaced by descriptor-driven dispatch (one `resource_kind` lookup).
- **`used_native_libraries_` registration.** The "insert at the top of every dispatcher" rule (`.claude/rules/codegen-stdlib-dispatcher.md`, #1856) becomes unnecessary for descriptor-driven dispatch: each invocation registers its descriptor's `library_name` automatically, and the dispatcher contract becomes a one-line invariant ("the descriptor registers its library"). Pattern B handlers that call into a separate `libry_<mod>` library (the four hand-written inserts inside `emitBuiltinConversion` and `emitBuiltinCore`, `src/codegen_call.cpp`) are **not** covered by this consolidation; they remain hand-written by design, see §"Pattern B carve-out" below.
- **`Result<T, Error>` wrapping.** The five helpers in `src/codegen_call_dispatch.cpp` (`emitResultBranch`, `buildErrorFromRuntime`, `wrapPtrAsResult`, `wrapStatusAsResult`, `emitPtrToResult`) become the descriptor-driven dispatch's single Result-wrapping path, parameterized by `return_wrapping` + `error_channel`.
- **Mock / spy.** `emitNativeCustomEmitterMockDispatch` (in `src/codegen_call_native.cpp`) consults `descriptor.mockable`. The v1 limitation (customEmitter-path natives do not record args for spy assertions) is a separate follow-up — descriptor-driven dispatch consolidates the call shape and makes arg recording feasible, but the v2 record format is out of scope here.
- **First-class native thunk.** `materializeNativeThunk` (in `src/codegen_lambda.cpp`) already synthesizes a `CallExpr` and routes it through `emitExpr`. Once `emitExpr` consumes descriptors, the thunk path inherits descriptor dispatch automatically; the thunk implementation does not change.

## Native vs compiler builtin (single discriminator)

A symbol is a **native call** iff the `__ry_*` symbol and the wrapping policy are **mechanically derivable from the declaration** (the `@native` declaration plus its containing module and parameter types map to a static table entry with no hand-written name → symbol mapping) — i.e. the descriptor selects the call. Everything else is a **compiler builtin**.

Compiler-builtin cases — what stays in `emitBuiltin*`:

- Pattern B handlers in general. Most have no `@native` declaration at all; the few that do — `int(s)` / `float(s)` / `str(v)` declared in `share/std/convert.ry` — treat the declaration as **spec-only documentation**, not the dispatch path (the symbols `__ry_str_to_int` / `__ry_str_to_float` / `valueToString` are not name-derivable from `int` / `float` / `str`, and the existing `kReservedBuiltinFunctionNames` guard claims the name for Pattern B before any descriptor lookup runs).
- A2 entries where the dispatch decision is type-driven: `math::abs` (int / float overload), `math::log` / `math::pow` (int / float multi-arity), `json::stringify` / `json::stringifySafe` (polymorphic over Ry value type), `thread::threadSpawn` / `threadJoin` (synthesizes an `llvm::Function` thunk, see `pickNativeOverloadByCallShape` at `src/codegen_call_native.cpp:733`).
- Any `emitBuiltin*` that constructs `llvm::StructType` from Ry-level type metadata (`enumerate` / `zip` build per-call tuple `StructType`s).

The migration follows this discriminator entry-by-entry; the audit deliverable is the A2 reclassification table in installment 3.

## Pattern B carve-out: hand-written handlers calling into separate libraries

A small set of Pattern B handlers (compiler builtins, no descriptor) calls into a `libry_<mod>` symbol that lives outside `ry_lib`. Today this is expressed as a hand-written `used_native_libraries_.insert("<mod>")` at the top of the relevant branch:

| Caller | Library registered | Why Pattern B intercepts |
|---|---|---|
| `emitBuiltinConversion`'s `int(s)` → `__ry_str_to_int` (`src/codegen_call.cpp:21`) | `convert` | name-keyed reserved builtin; `convert.ry` declaration is spec-only documentation, not the dispatch path |
| `emitBuiltinConversion`'s `float(s)` → `__ry_str_to_float` (`src/codegen_call.cpp:42`) | `convert` | same |
| `emitBuiltinCore`'s `input(...)` → `__ry_io_read_line` / `__ry_io_input_prompt` (`src/codegen_call.cpp:615`) | `io` | bare builtin; no `import io` required, so library registration must happen here |
| `emitBuiltinCore`'s `close(f)` → `__ry_io_file_close` (`src/codegen_call.cpp:726`) | `io` | bare builtin; same |

These four sites are **not** removable by the descriptor-driven inference rule above, because Pattern B handlers never build a descriptor for the call. Three resolutions are possible; none are chosen here:

- **Keep as carve-out (preferred for v1).** Pattern B remains hand-written; the `.claude/rules/codegen-stdlib-dispatcher.md` rule "Stdlib dispatchers must insert their module library at the top" applies to these handlers as a documented exception. Retire the rule's *general* statement when descriptor-driven dispatch lands, but keep the carve-out clause for these four sites.
- **Add a symbol → library reverse map.** Build a `__ry_*` symbol-to-library index from descriptor registration data so Pattern B handlers can call `registerLibraryForSymbol("__ry_str_to_int")` instead of hard-coding "convert". This is a separate mechanism, not a descriptor consumer.
- **Promote `int` / `float` / `input` / `close` into descriptor scope.** Re-route the Pattern B intercepts through the descriptor-driven dispatch path. This requires the descriptor to support the `int`/`float` Result-out-param wrapping shape and the `close` resource-kind dispatch shape, both of which are already in scope of the descriptor's `return_wrapping` / `resource_kind` fields — but it crosses the "reserved builtin" boundary and risks user-visible behavior differences.

The carve-out is recorded so that follow-on issues do not inherit the false assumption that descriptor consolidation removes these inserts automatically.

## Pilot (landed in #2337)

- **Primary: `base64`.** Eight `@native("base64")` declarations in `share/std/base64/base64.ry` chosen because they exercise three descriptor fields at once — `return_wrapping` (`decode` / `decodeBytes` return `Result<...>`), `error_channel` (`__ry_base64_get_last_error`), `require_list_u8_arg` (`encodeBytes` / `encodeBytesUrlSafe` gate the arg-0 byte list). `resource_kind` is intentionally absent: base64 is pure data transformation, which keeps the descriptor surface minimal while still proving the three fields simpler modules omit. The base64 table was retired earlier in #2285; #2337 lifted the three fields from dispatch-time inference in `emitGenericNativeCall` into descriptor population at `@native` declaration time, with `out_param_type_name` carried alongside `return_wrapping` for the `ResultOutParam` shape sibling consumers need.
- **Confirmation: `path`.** Six `@native("path")` declarations in `share/std/path/path.ry`. 5×`ResultPtr` + 1×`BoolFromI64`, `error_channel = __ry_path_get_last_error`, no `require_list_u8_arg`, no resource. `path::join` uses the arity-suffix runtime symbol convention (`__ry_path_join2/3/4`); `emitGenericNativeCall` detects this at consume time from the sig key carrying multiple overloads with different arities (no descriptor field needed for the pilot — today only `join` triggers it, all other multi-arity `@native` fns route through custom-emitter dispatchers). `dispatchPath` is now a bare-`return nullptr` stub matching `dispatchBase64`.
- **Installment 2: `io` / `filesystem` / `net` / `http`.** Resource-coupled modules. Each adds `resource_kind` plus the module-specific error channel. `http` carries the `hash.cpp` duplication carve-out (in `CMakeLists.txt`'s `add_ry_native_lib(http ...)`) — flagged but not resolved here.
- **Installment 3 carve-outs.** Move A2 type-driven entries to compiler builtins: `math::abs / log / pow / floor / ceil / round / digits` → `emitBuiltinMath`; `json::stringify / stringifySafe` → `emitBuiltinJson`; `thread::threadSpawn / threadJoin` → `emitBuiltinThread`.

## Migration order

The coarse sequence; §"Follow-up implementation issues" below refines this into specific issue-sized installments.

1. Introduce `NativeCallDescriptor` + the library inference rule. No call-site changes.
2. Mechanical: Pattern A1 → descriptor (math A1 entries / path / base64 / io A1 entries).
3. Audit: Pattern A2 → split into descriptor (declarative) vs compiler-builtin (type-driven). Produces the reclassification table.
4. Pattern C `emitGenericNativeCall` → descriptor-driven shape (inference moves into descriptor construction, not the dispatch).
5. Pilot: base64 → path. IR byte-exact verification per the discipline in `codegen-layering-plan.md` (the `#2026` rule).
6. Installment 2: io / filesystem / net / http — `resource_kind` + module error channels.
7. Installment 3: carve-outs — math / json / thread A2 type-driven entries become compiler builtins.
8. Upper codegen migration: the lowering side becomes the only consumer of descriptors; descriptor production is descriptor input, descriptor dispatch is mechanical. Native knowledge does not leak into the new layer.

## Out of scope (recorded, not decided)

- Symbol-naming consistency (camelCase vs snake_case across modules).
- `@native` first-class thunk Rust migration.
- Mock/spy v2 argument recording across customEmitter callers (current v1 limitation at `src/codegen_call_native.cpp:797`).
- `http`'s `src/runtime/core/hash.cpp` duplication in `libry_http`.
- `testing`'s `absoluteSymbols` loader path (intentional carve-out, no descriptor coverage needed).
- Cleanup of the Pattern B → `libry_<mod>` carve-out's four hand-written `used_native_libraries_` inserts (resolution choice deferred to the cleanup-stage implementation issue, see §"Pattern B carve-out").

## Follow-up implementation issues

These issues are **drafts**, not yet filed. Per the AGENTS.md "User Permission Gates" policy, GitHub issues are filed one at a time via `/git-create-issue` after this design document merges, not autonomously. The order matches §"Migration order".

1. `NativeCallDescriptor` C++ type + library inference rule (rule (a)/(b) above) + parser wiring for the optional `@native("<lib>")` tag (already accepted today; only the descriptor's `library_name` field is new). No call-site changes.
2. Pattern A1 → descriptor mechanical conversion (io / math A1 entries / path / base64 tables).
3. Pattern A2 audit + reclassification (per-entry "descriptor" vs "compiler-builtin" split, producing the audit table in installment 3).
4. Pattern C `emitGenericNativeCall` → descriptor-driven rewrite (inference moves into descriptor construction).
5. **Pilot: base64** (descriptor's three fields — `return_wrapping` / `error_channel` / `require_list_u8_arg` — exercised in one slice, IR byte-exact verified per the `#2026` rule).
6. **Pilot confirmation: path** (six A1 entries, no wrapping / no resource).
7. Installment 2-a: io / filesystem (resource_kind + module error channels).
8. Installment 2-b: net / http (resource_kind + multi-channel `__ry_net_*` / `__ry_tls_*` / `__ry_http_*` error channels).
9. Installment 3-a carve-out: math A2 type-driven entries → `emitBuiltinMath` (Pattern B).
10. Installment 3-b carve-out: `json::stringify` / `json::stringifySafe` → `emitBuiltinJson`.
11. Installment 3-c carve-out: `thread::threadSpawn` / `threadJoin` → `emitBuiltinThread`.
12. `used_native_libraries_` consolidation (descriptor-driven registration, retire the general `.claude/rules/codegen-stdlib-dispatcher.md` rule, keep the §"Pattern B carve-out" exception).
13. Mock / spy v2 argument recording across customEmitter callers (current v1 limitation at `src/codegen_call_native.cpp:797`); separate from descriptor work.
14. Upper codegen migration kickoff: descriptor consumers move to Rust; native knowledge stays in C++ descriptor producers only. This is the issue that #2231 protects, and it is **out of scope of this design**.

## Related documents

- [Codegen Layering Plan](codegen-layering-plan.md) — the lowered IR vocabulary that `NativeCallDescriptor` extends (`RuntimeCall` op).
- [Runtime Boundary](runtime-abi-boundary.md) — the orthogonal `__ry_*` ABI between runtime libraries and codegen; this document does not duplicate its module tables.
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the (ii)-discipline emission boundary; reconciled in §"Why this boundary exists".
- [Layer Graduation Workflow](layer-graduation-workflow.md) — graduation criteria; this document is explicitly **not** a graduation document.
- `.claude/rules/codegen-stdlib-dispatcher.md` — current dispatcher contract; planned retirement once descriptor-driven dispatch lands.
