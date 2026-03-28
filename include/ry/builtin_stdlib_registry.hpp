#pragma once

#define RY_BUILTIN_STDLIB_PACKAGES(X) \
    X(math,   "lib/std/math/math.ry",     emitBuiltinMath) \
    X(io,     "lib/std/io/io.ry",         emitBuiltinIO) \
    X(net,    "lib/std/net/net.ry",       emitBuiltinNet) \
    X(http,   "lib/std/http/http.ry",     emitBuiltinHttp) \
    X(json,   "lib/std/json/json.ry",     emitBuiltinJson) \
    X(base64, "lib/std/base64/base64.ry", emitBuiltinBase64) \
    X(path,   "lib/std/path/path.ry",     emitBuiltinPath) \
    X(filesystem, "lib/std/filesystem/filesystem.ry", emitBuiltinFilesystem) \
    X(thread, "lib/std/thread/thread.ry", emitBuiltinThread)

#define RY_BUILTIN_STDLIB_CONSTANTS(X) \
    X(math, PI,  Value,    3.141592653589793) \
    X(math, E,   Value,    2.718281828459045) \
    X(math, Inf, Infinity, 0.0) \
    X(math, NaN, NaN,      0.0)
