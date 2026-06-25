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
    // Only Result<T, Error> carriers are considered today. A bare resource
    // return type (e.g. `fn open() -> File`) is not in stdlib use; gating
    // on Result<...> keeps the inference's blast radius narrow.
    std::string okType = extractResultOkType(returnType);
    if (okType.empty()) return ResourceKindRegistry::NONE;
    return ResourceKindRegistry::instance().lookupByTypeName(okType);
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
        return out;
    }
    return std::nullopt;
}

}  // namespace ry
