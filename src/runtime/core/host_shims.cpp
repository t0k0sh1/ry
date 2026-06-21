// Thin extern "C" wrappers around the header-only inline allocation helpers
// (makeString / makeStringUninit / makeByteList / makeEmptyIOList) so the
// Rust cdylib `libry_base64.{dylib,so}` (and future siblings — convert / path
// / json / ...) can call them across the FFI boundary. The host inline
// versions stay unchanged; these shims preserve the single-source
// allocation semantics (arc_alloc + counter increment + memcpy + null
// terminator) on the C++ side, satisfying the
// "自前再構築禁止、ハンドル受領のみ" discipline from #2282 §3.
//
// Compiled into `ry_lib` (CMakeLists.txt around the `add_library(ry_lib
// STATIC ...)` source list) so the symbols are exported from the host
// process; the Rust cdylib resolves them at runtime via
// -undefined dynamic_lookup (macOS) / -rdynamic (Linux), the same path
// __ry_arc_alloc_counted and __ry_set_last_error already use.

#include "ry/runtime/core/string.hpp"
#include "ry/runtime/native/io.hpp"

#include <cstddef>
#include <cstdint>

extern "C" {

const char *__ry_host_make_string(const char *src, int64_t byte_len) {
    return ry::makeString(src, static_cast<size_t>(byte_len));
}

char *__ry_host_make_string_uninit(int64_t byte_len) {
    return ry::makeStringUninit(static_cast<size_t>(byte_len));
}

void *__ry_host_make_byte_list(const uint8_t *bytes, int64_t len) {
    return ry::makeByteList(bytes, len);
}

void *__ry_host_make_empty_io_list() {
    return ry::makeEmptyIOList();
}

}
