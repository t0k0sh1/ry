#pragma once

#define RY_BUILTIN_STDLIB_PACKAGES(X) \
    X(math,   "share/std/math/math.ry",     emitBuiltinMath) \
    X(io,     "share/std/io/io.ry",         emitBuiltinIO) \
    X(net,    "share/std/net/net.ry",       emitBuiltinNet) \
    X(http,   "share/std/http/http.ry",     emitBuiltinHttp) \
    X(json,   "share/std/json/json.ry",     emitBuiltinJson) \
    X(path,   "share/std/path/path.ry",     emitBuiltinPath) \
    X(thread, "share/std/thread/thread.ry", emitBuiltinThread)

#define RY_BUILTIN_STDLIB_CONSTANTS(X) \
    X(math, PI,  Value,    3.141592653589793) \
    X(math, E,   Value,    2.718281828459045) \
    X(math, Inf, Infinity, 0.0) \
    X(math, NaN, NaN,      0.0)
