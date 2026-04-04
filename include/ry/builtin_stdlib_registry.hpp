#pragma once

#define RY_BUILTIN_STDLIB_PACKAGES(X) \
    X(math,   "share/std/math/math.ry",     emitBuiltinMath) \
    X(io,     "share/std/io/io.ry",         emitBuiltinIO) \
    X(net,    "share/std/net/net.ry",       emitBuiltinNet) \
    X(http,   "share/std/http/http.ry",     emitBuiltinHttp) \
    X(json,   "share/std/json/json.ry",     emitBuiltinJson) \
    X(base64, "share/std/base64/base64.ry", emitBuiltinBase64) \
    X(path,   "share/std/path/path.ry",     emitBuiltinPath) \
    X(filesystem, "share/std/filesystem/filesystem.ry", emitBuiltinFilesystem) \
    X(thread, "share/std/thread/thread.ry", emitBuiltinThread) \
    X(gc,     "share/std/gc/gc.ry",         emitBuiltinGc)

#define RY_BUILTIN_STDLIB_CONSTANTS(X) \
    X(math, PI,  Value,    3.141592653589793) \
    X(math, E,   Value,    2.718281828459045) \
    X(math, Inf, Infinity, 0.0) \
    X(math, NaN, NaN,      0.0)
