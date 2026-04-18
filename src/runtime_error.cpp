// Thread-local error message buffer for Result-based error reporting.
// This is infrastructure shared by all native libs, so it lives in ry_lib
// (the main executable). Native shared libraries resolve these symbols from
// the process at runtime via -undefined dynamic_lookup (macOS) or
// -rdynamic (Linux).

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_string.hpp"

#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace ry {

static thread_local char last_error_buf[512] = {0};

extern "C" void __ry_set_last_error(const char *msg) {
    snprintf(last_error_buf, sizeof(last_error_buf), "%s", msg);
}

extern "C" const char *__ry_get_last_error() {
    // Return a StringHeader-managed copy so it can be stored as a Ry str.
    return makeString(last_error_buf, strlen(last_error_buf));
}

} // namespace ry
