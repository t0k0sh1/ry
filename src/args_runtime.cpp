#include "ry/args_runtime.hpp"


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

extern "C" const char* __ry_args_get(int index) {
    if (index < 0 || index >= g_argc) return "";
    return g_argv[index];
}

} // namespace ry
