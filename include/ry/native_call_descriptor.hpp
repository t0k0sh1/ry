#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "ry/codegen_native_dispatch.hpp"  // CodeGenReturnWrapping
#include "ry/stdlib_registry.hpp"  // ResourceKindRegistry::NONE

namespace ry {

// NativeCallDescriptor: declarative @native call shape (foundation for
// docs/architecture/native-call-boundary.md follow-up #1, tracking #2299).
//
// v1 (#2335) stored only `library_name`. The pilot in #2337 adds the
// three declarative fields the architecture doc §"Pilot" specifies —
// `return_wrapping`, `error_channel`, `require_list_u8_arg` — plus
// `out_param_type_name` which `inferReturnWrapping` returns alongside
// `return_wrapping` (filesystem's `Result<int>` / `Result<bool>` entries
// need it for `ResultOutParam`, and filesystem shares the consume path).
//
// Installment 2-a (#2338) adds `resource_kind` for resource-coupled
// modules (io, filesystem, net, http, thread). When non-NONE the consume
// path attaches the kind to the wrapped result via addResourceKind.
//
// Installment 2-c (#2381) adds more fields so the remaining
// handle-coupled / NUL-checked / file-coupled custom emitters in
// dispatchIO / dispatchNet / dispatchHttp can retire and consume the
// descriptor-driven path. See struct fields for the per-field intent.

// Installment 2-c (#2381): per-NUL-check spec. A descriptor's `nul_checks`
// vector orders them outer-to-inner (the first entry becomes the outer
// emitResultBranch). httpRequest needs two nested checks (method first,
// url second) so a vector — not a bitmask — is required to control the
// nesting order. The static-global counter for `err_global_prefix` is
// shared across callees that reuse the same prefix (e.g. query / cookie
// / formField all use "http_opt_nul") so byte-exact regressions hold
// against the pre-migration customEmitter IR.
struct NativeNulCheckSpec {
    int param_index = -1;        // 0-based arg idx; must be < arity
    std::string hint;            // SSA name hint, e.g. "hdr_key"
    std::string err_global_prefix;  // static-global symbol prefix (NO leading dot)
    std::string err_message;     // full Ry-visible error message
};

struct NativeCallDescriptor {
    std::optional<std::string> library_name;

    // Pilot fields (#2337): populated at @native declaration time so
    // emitGenericNativeCall consumes pre-computed values instead of
    // re-inferring them at every call site.
    CodeGenReturnWrapping return_wrapping = CodeGenReturnWrapping::Direct;
    std::string out_param_type_name;   // non-empty only for ResultOutParam
    std::string error_channel;         // e.g. "__ry_base64_get_last_error"; empty = none
    int require_list_u8_arg = -1;      // arg index requiring List<u8>; -1 = no check

    // Installment 2-a (#2338): ResourceKindRegistry index for resource-
    // returning natives (e.g. io::open returns Result<File, Error>; the
    // descriptor carries rk_file so the consume path tags the wrapped
    // result automatically). NONE (-1) means "not a resource".
    int resource_kind = ResourceKindRegistry::NONE;

    // Installment 2-c (#2381): handle-coupled / NUL-checked / file-coupled
    // overload metadata.
    //
    // - `handle_param_index`: first parameter typed as a registered
    //   ResourceKind (-1 = no handle). Drives the emit-time type check
    //   and resource-library linkage for non-ResultPtr returns.
    // - `handle_resource_kind`: cached ResourceKindRegistry id for the
    //   handle param's declared type so emit-time avoids a second lookup.
    // - `exported_symbol`: override for the convention-derived runtime
    //   symbol (e.g. io::open -> "__ry_io_file_open"). Empty = derive.
    // - `nul_checks`: ordered NUL-check specs (see NativeNulCheckSpec).
    // - `iterator_elem_type_name`: Iterator<T>'s element type spelling
    //   used when wrapping == IteratorFromHandle.
    int handle_param_index = -1;
    int handle_resource_kind = ResourceKindRegistry::NONE;
    std::string exported_symbol;
    std::vector<NativeNulCheckSpec> nul_checks;
    std::string iterator_elem_type_name;

    // #2481: per-overload "wrap an `any`-typed param slot as
    // (wrapInAny? + alloca + store -> ptr)" — required by json::dump
    // (and json5::dump, #2482) whose C ABI takes `const RyAny *`.
    // `any_by_ptr_param_index` is the 0-based arg index (-1 = none).
    // `any_by_ptr_alloca_hint` is the SSA name hint for the alloca
    // (empty -> derive `<callee>_any_in`); set explicitly for byte-
    // exact IR preservation under migration.
    int any_by_ptr_param_index = -1;
    std::string any_by_ptr_alloca_hint;

    // #2481: append a synthetic trailing `i64` constant to the C call
    // when the C ABI takes more args than the Ry declaration. Used by
    // json::dump's 2-arg overload to inject `indent = -1` so both
    // overloads route to a single `__ry_json_dump_file(ptr, ptr, i64)`
    // symbol. Empty `optional` = no injection.
    std::optional<int64_t> synthetic_trailing_i64;

    // #2481: SSA name hint for the runtime-call result. The pre-migration
    // emitters (thread sync ops, json::dump) named their `i64 status` call
    // with package- or overload-specific forms (e.g. `lockAcquire_status`,
    // `json_dump_status`); the descriptor-driven path uses this hint
    // verbatim so byte-exact IR is preserved under migration. Empty -> use
    // `e.callee` (the existing default for all other entries).
    std::string call_name_hint;
};

// inferLibraryName: derives the descriptor's library_name from the
// declaration state per docs/architecture/native-call-boundary.md
// §"Library inference rule":
//
//   (a) explicit tag wins — non-empty `directiveTag` (the `@native("<lib>")`
//       argument) is returned as-is.
//   (b) module-keyed fallback — when `directiveTag` is empty and
//       `declaringModule` matches a name in `knownNativeLibs()`, returns
//       `declaringModule`. Otherwise returns nullopt (the symbol resolves
//       through `ry_lib` + the in-process search generator, no dlopen).
//
// Pure function — testable directly without driving a full compile.
std::optional<std::string> inferLibraryName(const std::string &directiveTag,
                                            const std::string &declaringModule);

// inferReturnWrapping: derives the descriptor's return_wrapping (and the
// out-param type name for ResultOutParam) from the Ry return type spelling.
// Pure function — moved here from the anonymous namespace in
// codegen_call_native.cpp so the descriptor population path (codegen_fn.cpp)
// and the dispatch consume path (emitGenericNativeCall) share one source.
//
// Returns {wrapping, out_param_type_name}. out_param_type_name is non-empty
// only when wrapping == ResultOutParam.
// extractResultOkType: returns the trimmed Ok type from "Result<T, Error>"
// (e.g. "File" from "Result<File, Error>"), or an empty string when the
// input does not name a Result<T, _>. Pure depth-aware comma finder used
// by inferReturnWrapping, inferResourceKind, and the return-type metadata
// propagation path in emitGenericNativeCall — keep one copy.
std::string extractResultOkType(const std::string &returnTypeName);

std::pair<CodeGenReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnTypeName);

// inferResourceKind: extracts the inner type name from a return type
// spelling and looks it up against ResourceKindRegistry. Returns the
// registered kind id when the inner type names a registered resource
// (e.g. "Result<File, Error>" -> rk_file), or ResourceKindRegistry::NONE
// otherwise. Mirrors inferReturnWrapping in shape (pure, declaration-
// time) so codegen_fn.cpp populates both at @native registration time.
//
// Result wrapping is the only currently-supported carrier: a bare
// resource return type (e.g. `fn open() -> File`) would also resource-
// tag the value, but no stdlib uses that shape today. Direct returns
// are intentionally excluded to keep the inference's blast radius
// scoped to error-throwable resource constructors.
int inferResourceKind(const std::string &returnTypeName);

// knownNativeLibs: mirror of CMakeLists.txt's RY_NATIVE_LIBS list
// (12 entries: base64, path, convert, filesystem, gc, testing, io,
// json, json5, net, thread, http). Drives rule (b) inference. The
// CMake list is the source of truth; this C++ copy is hand-maintained.
// See tests/test_native_call_descriptor.cpp's KnownNativeLibsLocalLiteral
// test for the local-only consistency guard.
const std::unordered_set<std::string> &knownNativeLibs();

// Installment 2-c (#2381): per-overload override carrying the runtime
// symbol name, NUL-check param bitmask, wrapping override, iterator
// element type, and static-global naming prefix for handle-coupled /
// NUL-checked / file-coupled `@native` declarations whose default
// convention-derivation in emitGenericNativeCall does not match the
// hand-written customEmitter shape. The table lives in
// src/codegen_native_call_descriptor.cpp.
struct NativeOverloadKey {
    std::string package;       // e.g. "io"
    std::string callee;        // e.g. "open"
    std::vector<std::string> param_types;  // e.g. {"str", "str"}
};

struct NativeOverloadOverride {
    std::string exported_symbol;
    std::vector<NativeNulCheckSpec> nul_checks;
    std::string iterator_elem_type_name;
    CodeGenReturnWrapping wrapping_override = CodeGenReturnWrapping::Direct;
    bool wrapping_overridden = false;

    // #2481: see NativeCallDescriptor for semantics.
    int any_by_ptr_param_index = -1;
    std::string any_by_ptr_alloca_hint;
    std::optional<int64_t> synthetic_trailing_i64;
    std::string call_name_hint;
};

// lookupNativeOverloadOverride: returns the override entry matching
// (package, callee, param_types) exactly, or nullopt when no override is
// registered. Matching is by full param-type list because some entries
// (setTimeout, body, header, ...) overload on resource type so arity-
// only matching is ambiguous.
std::optional<NativeOverloadOverride> lookupNativeOverloadOverride(
    const std::string &package, const std::string &callee,
    const std::vector<std::string> &paramTypes);

// inferHandleParamIndex: returns the first parameter index whose declared
// type spelling resolves to a registered ResourceKind, or -1 when none.
// Used by the descriptor populator to flag handle-coupled overloads
// without an explicit annotation.
int inferHandleParamIndex(const std::vector<std::string> &paramTypes);

}  // namespace ry
