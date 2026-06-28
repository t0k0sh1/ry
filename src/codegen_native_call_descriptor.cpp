#include "ry/native_call_descriptor.hpp"

#include "ry/stdlib_registry.hpp"

#include <cstddef>
#include <initializer_list>

namespace ry {

namespace {

// Installment 2-c (#2381): per-overload override table. Each row
// describes one handle-coupled / NUL-checked / file-coupled `@native`
// declaration whose runtime symbol or NUL-check policy cannot be
// derived from the `@native(<lib>) fn <name>(...)` declaration alone.
// The table is consulted at descriptor population time; the matching
// override fields are copied onto the NativeCallDescriptor so
// emitGenericNativeCall's consume path stays branchless on the table.
//
// Key: (package, callee, exact param-type list). Param-type list is the
// disambiguator for overloads (e.g. body(HttpRequest) -> __ry_http_body
// vs body(HttpClientResponse) -> __ry_http_client_body).
struct NulCheckLit {
    int param_index;
    const char *hint;
    const char *err_global_prefix;
    const char *err_message;
};

struct OverrideEntry {
    const char *package;
    const char *callee;
    std::initializer_list<const char *> param_types;

    const char *exported_symbol;          // nullptr = derive from convention
    std::initializer_list<NulCheckLit> nul_checks;  // outer-to-inner order
    const char *iterator_elem_type_name;  // empty = no iterator wrapping
    CodeGenReturnWrapping wrapping_override;
    bool wrapping_overridden;

    // #2481: any-by-ptr param wrapping. -1 = none. When set, the
    // descriptor consumer treats the indexed `any`-typed param slot as
    // `(wrapInAny? + alloca + store -> ptr)` to match the C ABI's
    // `const RyAny*` expectation. `any_by_ptr_alloca_hint` is the SSA
    // name hint for the alloca; nullptr/"" -> derive `<callee>_any_in`.
    // In-class defaults so existing kOverrides entries need no edits.
    int any_by_ptr_param_index = -1;
    const char *any_by_ptr_alloca_hint = nullptr;

    // #2481: synthetic trailing `i64` constant injection. When engaged,
    // the descriptor consumer appends the value as an `i64` arg after
    // the Ry args. Used by json::dump's 2-arg overload to inject
    // indent=-1.
    std::optional<int64_t> synthetic_trailing_i64;

    // #2481: SSA name hint for the runtime-call result (see
    // NativeCallDescriptor::call_name_hint). nullptr/"" -> use `e.callee`.
    const char *call_name_hint = nullptr;
};

// Hand-maintained table. Order matters only for readability — lookup is
// linear and the first match wins. ~30 entries total today.
static const OverrideEntry kOverrides[] = {
    // -------- io: file-coupled overloads (resource: File) --------
    {"io", "open",       {"str", "str"},
     "__ry_io_file_open", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"io", "readAll",    {"File"},
     "__ry_io_file_read_all", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // readLine(f: File) -> Result<Option<str>, Error>: out-param + Option-wrap on ok
    {"io", "readLine",   {"File"},
     "__ry_io_file_read_line", {}, "",
     CodeGenReturnWrapping::ResultOutParamOption, true},

    // readLine() 0-arg stdin variant
    {"io", "readLine",   {},
     "__ry_io_read_line", {}, "",
     CodeGenReturnWrapping::ResultOutParamOption, true},

    {"io", "writeText",  {"File", "str"},
     "__ry_io_file_write_text", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // lines(f: File) -> Iterator<str>: synthesize next-fn calling exported_symbol
    {"io", "lines",      {"File"},
     "__ry_io_file_read_line", {}, "str",
     CodeGenReturnWrapping::IteratorFromHandle, true},

    // -------- net: handle-coupled overloads --------
    {"net", "listen",       {"TcpListener", "int"},
     "__ry_listen", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "accept",       {"TcpListener"},
     "__ry_accept", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "listenerPort", {"TcpListener"},
     "__ry_listener_port", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "shutdown",     {"TcpListener"},
     "__ry_tcp_listener_shutdown", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"net", "setTimeout",        {"TcpStream", "int"},
     "__ry_tcp_set_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "setReceiveTimeout", {"TcpStream", "int"},
     "__ry_tcp_set_recv_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "setSendTimeout",    {"TcpStream", "int"},
     "__ry_tcp_set_send_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"net", "setTimeout",        {"TlsStream", "int"},
     "__ry_tls_set_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "setReceiveTimeout", {"TlsStream", "int"},
     "__ry_tls_set_recv_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"net", "setSendTimeout",    {"TlsStream", "int"},
     "__ry_tls_set_send_timeout", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // -------- http: handle-coupled (no NUL) --------
    {"http", "method", {"HttpRequest"},
     "__ry_http_method", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "path",   {"HttpRequest"},
     "__ry_http_path", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"http", "body",      {"HttpRequest"},
     "__ry_http_body", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "body",      {"HttpClientResponse"},
     "__ry_http_client_body", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "bodyBytes", {"HttpRequest"},
     "__ry_http_body_bytes", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "bodyBytes", {"HttpClientResponse"},
     "__ry_http_client_body_bytes", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // -------- http: handle-coupled + NUL-check + Option<T> --------
    {"http", "header", {"HttpRequest", "str"},
     "__ry_http_header",
     {{1, "hdr_key", "http_hdr_nul", "header: key contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},
    {"http", "header", {"HttpClientResponse", "str"},
     "__ry_http_client_header",
     {{1, "hdr_key", "http_hdr_nul", "header: key contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},

    {"http", "query", {"HttpRequest", "str"},
     "__ry_http_query",
     {{1, "http_qry", "http_opt_nul", "query: key contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},
    {"http", "cookie", {"HttpRequest", "str"},
     "__ry_http_cookie",
     {{1, "http_ck", "http_opt_nul", "cookie: key contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},
    {"http", "formField", {"HttpRequest", "str"},
     "__ry_http_form_field",
     {{1, "http_ff", "http_opt_nul", "formField: key contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},

    // queryAll / cookies / formFields — Direct Map<str,str>
    {"http", "queryAll",   {"HttpRequest"},
     "__ry_http_query_all", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "cookies",    {"HttpRequest"},
     "__ry_http_cookies", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"http", "formFields", {"HttpRequest"},
     "__ry_http_form_fields", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"http", "formFile", {"HttpRequest", "str"},
     "__ry_http_form_file",
     {{1, "ff_key", "http_ff_nul", "formFile: name contains embedded NUL"}},
     "", CodeGenReturnWrapping::OptionFromNullablePtr, true},

    // httpGet / httpPost / httpRequest — URL/method NUL-check + ResultPtr
    {"http", "httpGet", {"str"},
     "__ry_http_get",
     {{0, "get_url", "http_get_url_nul", "httpGet: url contains embedded NUL"}},
     "", CodeGenReturnWrapping::Direct, false},
    {"http", "httpPost", {"str", "str", "Map<str, str>"},
     "__ry_http_post",
     {{0, "post_url", "http_post_url_nul", "httpPost: url contains embedded NUL"}},
     "", CodeGenReturnWrapping::Direct, false},
    {"http", "httpRequest", {"str", "str", "Map<str, str>", "str"},
     "__ry_http_client_request",
     // Outer = method (idx 0), inner = url (idx 1).
     {{0, "req_method", "http_req_method_nul",
       "httpRequest: method contains embedded NUL"},
      {1, "req_url",    "http_req_url_nul",
       "httpRequest: url contains embedded NUL"}},
     "", CodeGenReturnWrapping::Direct, false},

    {"http", "status", {"HttpClientResponse"},
     "__ry_http_client_status", {}, "",
     CodeGenReturnWrapping::Direct, false},

    {"http", "httpClientResponseFree", {"HttpClientResponse"},
     "__ry_http_client_response_free", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // -------- thread: sync-primitive new (bare resource return) --------
    // #2393: replaces emitThreadSyncNew (0-arg) custom emitters. The
    // `Direct` wrapping + descriptor.resource_kind (populated via
    // inferResourceKind's bare-return arm) drives the addResourceKind tag
    // in emitGenericNativeCall's Direct path. exported_symbol overrides
    // the convention-derived `__ry_thread_<callee>` (the runtime symbols
    // predate the package prefix — they live under `__ry_lock_*` /
    // `__ry_rwlock_*` directly).
    {"thread", "lockNew",        {},
     "__ry_lock_new", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"thread", "rwlockNew",      {},
     "__ry_rwlock_new", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // -------- thread: sync-primitive new (Result<resource> return) -----
    // semaphoreNew / barrierNew take an int and return Result<resource,
    // Error>. The descriptor's wrapping stays ResultPtr (auto-derived from
    // inferReturnWrapping on `Result<Semaphore, Error>`) and resource_kind
    // is auto-set via inferResourceKind's existing Result<T,Error> arm —
    // the override carries only the runtime-symbol mapping (the convention-
    // derived `__ry_thread_semaphoreNew` does not match the actual
    // `__ry_semaphore_new`).
    {"thread", "semaphoreNew",   {"int"},
     "__ry_semaphore_new", {}, "",
     CodeGenReturnWrapping::Direct, false},
    {"thread", "barrierNew",     {"int"},
     "__ry_barrier_new", {}, "",
     CodeGenReturnWrapping::Direct, false},

    // -------- thread: sync-primitive ops (Result<Unit, Error>) ---------
    // Status-returning sync ops route through ResultStatus +
    // wrapStatusAsResult. The default `__ry_get_last_error` channel (set
    // via codegen_fn.cpp's kHasModuleLastError exclusion + the
    // emitGenericNativeCall fallback) matches the pre-migration call to
    // `wrapStatusAsResult(status)` with no explicit errFn arg. The
    // `call_name_hint` field reproduces the pre-migration `<callee>_status`
    // SSA name byte-exactly (#2481 hint mechanism).
    {"thread", "lockAcquire",      {"Lock"},
     "__ry_lock_acquire", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "lockAcquire_status"},
    {"thread", "lockRelease",      {"Lock"},
     "__ry_lock_release", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "lockRelease_status"},
    {"thread", "rwlockReadLock",   {"RWLock"},
     "__ry_rwlock_read_lock", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "rwlockReadLock_status"},
    {"thread", "rwlockWriteLock",  {"RWLock"},
     "__ry_rwlock_write_lock", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "rwlockWriteLock_status"},
    {"thread", "rwlockUnlock",     {"RWLock"},
     "__ry_rwlock_unlock", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "rwlockUnlock_status"},
    {"thread", "semaphoreAcquire", {"Semaphore"},
     "__ry_semaphore_acquire", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "semaphoreAcquire_status"},
    {"thread", "semaphoreRelease", {"Semaphore"},
     "__ry_semaphore_release", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "semaphoreRelease_status"},
    {"thread", "barrierWait",      {"Barrier"},
     "__ry_barrier_wait", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     -1, nullptr, std::nullopt, "barrierWait_status"},

    // -------- thread: sync-primitive *Free (ResourceFree wrapping) ----
    // The *Free entries don't dial a runtime fn — they emit emitResourceFree's
    // null-check + ARC-release sequence. `exported_symbol` is unused by the
    // ResourceFree path; the destructor lives in ResourceKindRegistry under
    // the rk_<resource> kind, looked up via desc.handle_resource_kind.
    {"thread", "lockFree",        {"Lock"},
     "", {}, "",
     CodeGenReturnWrapping::ResourceFree, true},
    {"thread", "rwlockFree",      {"RWLock"},
     "", {}, "",
     CodeGenReturnWrapping::ResourceFree, true},
    {"thread", "semaphoreFree",   {"Semaphore"},
     "", {}, "",
     CodeGenReturnWrapping::ResourceFree, true},
    {"thread", "barrierFree",     {"Barrier"},
     "", {}, "",
     CodeGenReturnWrapping::ResourceFree, true},

    // -------- json: descriptor-driven dump (#2481) --------
    // The 2-arg and 3-arg overloads both route to a single C symbol
    // `__ry_json_dump_file(ptr file, ptr any_value, i64 indent_or_neg1)`.
    // The 2-arg form synthesizes `indent = -1`; the 3-arg form passes
    // the user's indent. The `any`-typed value slot (index 1) is
    // wrapped via the descriptor's `any_by_ptr_param_index` mechanism
    // (`wrapInAny` if not already `any`, then alloca+store, pass as
    // ptr) to match the C ABI's `const RyAny*` expectation. The File
    // handle (index 0) is auto-detected as `rk_file` via
    // `inferHandleParamIndex`, which causes `emitGenericNativeCall` to
    // also link `libry_io` (required: `__ry_json_dump_file` internally
    // calls `__ry_io_file_write_text`). `ResultStatus` + the default
    // `__ry_get_last_error` channel reproduces the pre-migration
    // `wrapStatusAsResult(status)` call exactly. `any_by_ptr_alloca_hint`
    // and the per-package callNameHint extension in
    // `emitGenericNativeCall` preserve the SSA names `json_dump_in` /
    // `json_dump_status` for byte-exact IR regression.
    {"json", "dump", {"File", "any"},
     "__ry_json_dump_file", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     /*any_by_ptr_param_index=*/1,
     /*any_by_ptr_alloca_hint=*/"json_dump_in",
     /*synthetic_trailing_i64=*/std::optional<int64_t>{-1},
     /*call_name_hint=*/"json_dump_status"},
    {"json", "dump", {"File", "any", "int"},
     "__ry_json_dump_file", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     /*any_by_ptr_param_index=*/1,
     /*any_by_ptr_alloca_hint=*/"json_dump_in",
     /*synthetic_trailing_i64=*/std::nullopt,
     /*call_name_hint=*/"json_dump_status"},

    // -------- json5: descriptor-driven dump (#2482) --------
    // Mirrors json::dump above (identical ABI); symbol is __ry_json5_dump_file.
    {"json5", "dump", {"File", "any"},
     "__ry_json5_dump_file", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     /*any_by_ptr_param_index=*/1,
     /*any_by_ptr_alloca_hint=*/"json5_dump_in",
     /*synthetic_trailing_i64=*/std::optional<int64_t>{-1},
     /*call_name_hint=*/"json5_dump_status"},
    {"json5", "dump", {"File", "any", "int"},
     "__ry_json5_dump_file", {}, "",
     CodeGenReturnWrapping::ResultStatus, true,
     /*any_by_ptr_param_index=*/1,
     /*any_by_ptr_alloca_hint=*/"json5_dump_in",
     /*synthetic_trailing_i64=*/std::nullopt,
     /*call_name_hint=*/"json5_dump_status"},
};

bool paramListMatches(std::initializer_list<const char *> expected,
                      const std::vector<std::string> &actual) {
    if (expected.size() != actual.size()) return false;
    auto eit = expected.begin();
    for (std::size_t i = 0; i < actual.size(); ++i, ++eit) {
        if (actual[i] != *eit) return false;
    }
    return true;
}

}  // namespace

const std::unordered_set<std::string> &knownNativeLibs() {
    // Mirror of CMakeLists.txt:386 RY_NATIVE_LIBS. Keep these in sync.
    // KnownNativeLibsLocalLiteral test catches a local-only edit drift
    // (this list vs the test's expected literal); the CMake cross-file
    // invariant is hand-maintained and is not caught by any automated
    // guard today.
    static const std::unordered_set<std::string> kKnownNativeLibs = {
        "base64",
        "path",
        "convert",
        "filesystem",
        "gc",
        "testing",
        "io",
        "json",
        "json5",
        "net",
        "thread",
        "http",
    };
    return kKnownNativeLibs;
}

std::optional<std::string> inferLibraryName(const std::string &directiveTag,
                                            const std::string &declaringModule) {
    if (!directiveTag.empty())
        return directiveTag;
    if (!declaringModule.empty() && knownNativeLibs().count(declaringModule))
        return declaringModule;
    return std::nullopt;
}

std::string extractResultOkType(const std::string &returnType) {
    if (returnType.size() <= 7 || returnType.substr(0, 7) != "Result<")
        return {};
    int depth = 0;
    std::size_t commaPos = std::string::npos;
    for (std::size_t i = 7; i < returnType.size(); ++i) {
        if (returnType[i] == '<') ++depth;
        else if (returnType[i] == '>') --depth;
        else if (returnType[i] == ',' && depth == 0) { commaPos = i; break; }
    }
    if (commaPos == std::string::npos) return {};
    std::string okType = returnType.substr(7, commaPos - 7);
    while (!okType.empty() && okType.back() == ' ') okType.pop_back();
    return okType;
}

std::pair<CodeGenReturnWrapping, std::string>
inferReturnWrapping(const std::string &returnType) {
    using RW = CodeGenReturnWrapping;

    std::string okType = extractResultOkType(returnType);
    if (!okType.empty()) {
        if (okType == "Unit")  return {RW::ResultStatus, ""};
        if (okType == "int")   return {RW::ResultOutParam, "int"};
        if (okType == "float") return {RW::ResultOutParam, "float"};
        if (okType == "bool")  return {RW::ResultOutParam, "int"};
        // str, List<...>, Map<...>, or any pointer type → ResultPtr
        return {RW::ResultPtr, ""};
    }
    // Result<...> with no Ok type, or a non-Result return type.
    if (returnType.size() > 7 && returnType.substr(0, 7) == "Result<")
        return {RW::Direct, ""};

    if (returnType == "bool") return {RW::BoolFromI64, ""};

    // str, int, float, Unit, or any other type → Direct
    return {RW::Direct, ""};
}

int inferResourceKind(const std::string &returnType) {
    // Two carriers populate `resource_kind`:
    //
    // (1) `Result<T, Error>` where `T` is a registered resource — the
    //     wrapped Ok value is tagged after `wrapPtrAsResult` (the pre-2393
    //     net/http/io constructor pattern).
    //
    // (2) #2393: bare resource return (e.g. `lockNew() -> Lock`,
    //     `atomicIntNew() -> AtomicInt`). Used by thread constructors whose
    //     runtime symbols return the resource pointer directly without a
    //     Result wrapper. The Direct path in `emitGenericNativeCall` consumes
    //     this to call `addResourceKind` after the call. Stdlib audit
    //     (2026-06-25): only the thread package's *New entries hit this arm
    //     — no other @native fn returns a bare resource type today, so the
    //     extension is IR-neutral for existing Pattern C consumers.
    auto &registry = ResourceKindRegistry::instance();
    std::string okType = extractResultOkType(returnType);
    if (!okType.empty())
        return registry.lookupByTypeName(okType);
    return registry.lookupByTypeName(returnType);
}

// Installment 2-c (#2381): scan declared params for the first resource
// type registered with ResourceKindRegistry. Returns -1 when no param is
// a registered resource. The first-match convention matches dispatchIO /
// dispatchNet / dispatchHttp's existing customEmitter pattern where the
// handle is always the leading argument.
int inferHandleParamIndex(const std::vector<std::string> &paramTypes) {
    auto &registry = ResourceKindRegistry::instance();
    for (std::size_t i = 0; i < paramTypes.size(); ++i) {
        if (registry.lookupByTypeName(paramTypes[i]) != ResourceKindRegistry::NONE)
            return static_cast<int>(i);
    }
    return -1;
}

// Installment 2-c (#2381): linear scan over kOverrides. Returns the
// first match on (package, callee, param_types) by value, or nullopt
// when no override is registered. Linear scan is fine — the table has
// ~30 entries and the lookup runs at @native declaration time (once).
std::optional<NativeOverloadOverride> lookupNativeOverloadOverride(
    const std::string &package, const std::string &callee,
    const std::vector<std::string> &paramTypes) {

    for (const auto &entry : kOverrides) {
        if (package != entry.package) continue;
        if (callee != entry.callee) continue;
        if (!paramListMatches(entry.param_types, paramTypes)) continue;

        NativeOverloadOverride out;
        out.exported_symbol = entry.exported_symbol ? entry.exported_symbol : "";
        for (const auto &nc : entry.nul_checks) {
            NativeNulCheckSpec spec;
            spec.param_index = nc.param_index;
            spec.hint = nc.hint ? nc.hint : "";
            spec.err_global_prefix = nc.err_global_prefix ? nc.err_global_prefix : "";
            spec.err_message = nc.err_message ? nc.err_message : "";
            out.nul_checks.push_back(std::move(spec));
        }
        out.iterator_elem_type_name = entry.iterator_elem_type_name
            ? entry.iterator_elem_type_name : "";
        out.wrapping_override = entry.wrapping_override;
        out.wrapping_overridden = entry.wrapping_overridden;
        out.any_by_ptr_param_index = entry.any_by_ptr_param_index;
        out.any_by_ptr_alloca_hint = entry.any_by_ptr_alloca_hint
            ? entry.any_by_ptr_alloca_hint : "";
        out.synthetic_trailing_i64 = entry.synthetic_trailing_i64;
        out.call_name_hint = entry.call_name_hint
            ? entry.call_name_hint : "";
        return out;
    }
    return std::nullopt;
}

}  // namespace ry
