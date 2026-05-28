#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/native/http/http_error.hpp"
#include "ry/runtime/core/string.hpp"

#include <cstdarg>
#include <cstdio>


namespace ry {

static thread_local char http_last_error_buf[512] = {0};

void setHttpLastError(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsnprintf(http_last_error_buf, sizeof(http_last_error_buf), fmt, args);
    va_end(args);
}

extern "C" const char *__ry_http_get_last_error() {
    return makeString(http_last_error_buf, strlen(http_last_error_buf));
}

// Called from JIT'd code to check if a Ry string contains an embedded NUL.
extern "C" int64_t __ry_http_str_has_nul(const char *s) {
    return hasEmbeddedNul(s) ? 1 : 0;
}

} // namespace ry
