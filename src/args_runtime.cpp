#include "ry/args_runtime.hpp"
#include "ry/runtime_string.hpp"
#include <cstdlib>
#include <cstring>


namespace ry {

static int g_argc = 0;
static const char* const* g_argv = nullptr;

extern "C" void __ry_args_init(int argc, const char* const* argv) {
    g_argc = argc;
    g_argv = argv;
}

extern "C" int __ry_args_count() {
    return g_argc;
}

// Returns a StringHeader-managed str handle (not a raw argv pointer).
// Callers can safely call byte_len() / length() on the returned value.
extern "C" const char* __ry_args_get(int index) {
    const char *raw = (index < 0 || index >= g_argc) ? "" : g_argv[index];
    return makeString(raw, strlen(raw));
}

// Wraps getenv() result in a StringHeader-managed str handle.
// Returns nullptr when the variable is not set (caller converts to None/default).
extern "C" const char* __ry_env_get(const char *key) {
    const char *val = getenv(key); // key has null terminator — safe for getenv
    if (!val) return nullptr;
    return makeString(val, strlen(val));
}

} // namespace ry
