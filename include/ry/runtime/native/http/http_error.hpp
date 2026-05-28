#pragma once

#include <cstdarg>


namespace ry {

// Cross-TU shared error buffer for the http native library.
// runtime_http.cpp, runtime_http_client.cpp, and runtime_http_multipart.cpp
// all include this header and call setHttpLastError; only runtime_http_error.cpp
// defines the buffer and the extern "C" accessor.

void setHttpLastError(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

} // namespace ry
