# Native Call Boundary

This document records the lowering-side boundary for Ry `@native` calls. It decides which `__ry_*` runtime symbol to call, what library must be loaded, what wrapping is required, and what resource metadata is attached.

This is distinct from the [Runtime Boundary](runtime-abi-boundary.md), which owns the `__ry_*` ABI itself, and from the [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md), which owns `ry_emit_*` IR construction.

## Purpose

The native-call boundary makes the implicit stdlib dispatch table explicit. It consolidates state that used to be scattered across:

- native dispatch tables
- native signatures
- resource-kind registries
- manual `used_native_libraries_` inserts
- custom emitter wrappers for Result/Option/resource behavior

The descriptor is consumed by lowering. It does not cross into the emission layer as a Ry-semantic descriptor; lowering eventually dissolves it into generic emission calls such as `ry_emit_runtime_call` plus composite wrapping operations.

## `NativeCallDescriptor`

Current descriptor fields are conceptually:

| Field | Meaning |
|---|---|
| `module_name` | declaring stdlib module |
| `library_name` | explicit `@native("<lib>")`, inferred module library, or absent for process-resolved symbols |
| `exported_symbol` | runtime symbol override for legacy names; empty means derive from convention |
| `signature` | existing native function signature |
| `return_wrapping` | direct, Result pointer/status/out-param, Option nullable pointer, iterator/resource wrappers |
| `error_channel` | module-specific or default last-error function |
| `resource_kind` | resource tag to attach to returned handles |
| `handle_param_index` / `handle_resource_kind` | resource parameter validation and dispatch disambiguation |
| `nul_checks` | ordered NUL-safety checks before the call |
| `iterator_elem_type_name` | element type for iterator-returning natives |
| `any_by_ptr_param_index` / `any_by_ptr_alloca_hint` | `any`-typed param slots whose C ABI takes `const RyAny *`; descriptor consumer wraps to `any` (if needed), alloca + store, and substitutes the pointer. The hint preserves the pre-migration alloca SSA name byte-exactly. |
| `synthetic_trailing_i64` | optional trailing `i64` constant injected after the Ry args; covers the "2-arg overload routes to the same C symbol as 3-arg with a default sentinel" pattern (e.g. `json::dump` injecting `indent = -1`) |
| `call_name_hint` | SSA name hint for the runtime call result, used verbatim when set and `e.callee` otherwise; reproduces pre-migration call names such as `lockAcquire_status` or `json_dump_status` byte-exactly |
| `mockable` | whether test mock/spy dispatch can intercept the call |
| `overload_group_id` | grouping for multi-arity overload families |

Concrete C++ representation belongs to the implementation, not this document.

## Library Inference

Library identity is resolved from declarations:

1. `@native("<lib>")` wins.
2. Bare `@native` keys on the declaring module. If `libry_<module>` exists in `RY_NATIVE_LIBS`, the descriptor uses that library.
3. Otherwise the symbol resolves through the main process.

Do not infer the library by parsing the C symbol prefix. Historical symbols are inconsistent (`__ry_str_to_int`, `__ry_bind`, mixed camelCase/snake_case modules), while the declaring module is stable.

## Descriptor Scope

A call is descriptor-driven when the runtime symbol and wrapping policy are mechanically derivable from the declaration plus static descriptor tables.

The following stay outside the descriptor:

- compiler builtins with name-keyed dispatch
- type-driven dispatch such as numeric `math::abs`, polymorphic JSON `stringify` / `stringifySafe`, or thread thunk synthesis
- first-class native thunk metadata; thunks consume descriptors but are not descriptor fields
- hand-written emitters that synthesize control flow, such as 3+-arg HTTP `listen`
- parametric callees such as `json::load[T]` whose runtime symbol depends on a type argument parsed from the callee string

Functions that previously sat in the "hand-written emitter" group have been migrated to the descriptor path as the needed capabilities were added; `json::dump` joined in #2481 via `any_by_ptr_*` and `synthetic_trailing_i64`. `json5::dump` is tracked in #2482 and reuses the same fields.

These exceptions are compiler builtins or custom lowering paths, not runtime ABI exceptions.

## Runtime Symbol Convention

Default runtime symbol derivation is `__ry_<module>_<function>`. Legacy names are represented by descriptor overrides rather than by changing the user-facing `@native` syntax.

Module symbol naming consistency is intentionally separate from this boundary. If the naming convention changes, update descriptor overrides and native dispatch tests together.

## Invariants

- Descriptor construction is declaration-driven.
- Descriptor dispatch registers its required library automatically.
- Resource tagging is driven by declared return/parameter types and descriptor fields, not by scattered call-site code.
- Error wrapping uses the descriptor's error channel.
- Entries that need Ry type introspection before choosing a symbol are compiler builtins, not descriptor entries.
- The descriptor must not become an emission-layer coarse op.

## Related Documents

- [Codegen Layering Plan](codegen-layering-plan.md)
- [Runtime Boundary](runtime-abi-boundary.md)
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md)
