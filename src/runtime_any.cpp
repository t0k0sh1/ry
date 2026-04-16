#include "ry/runtime_any.hpp"
#include "ry/runtime_string.hpp"
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>


namespace ry {

static_assert(sizeof(RyAny) == 16, "RyAny must be 16 bytes to match LLVM anyTy_");
static_assert(offsetof(RyAny, data) == 8, "RyAny::data must be at offset 8");

// ===== Helpers =====

static const char *tagName(int64_t tag) {
    switch (tag) {
    case static_cast<int64_t>(RyAnyTag::Int):   return "int";
    case static_cast<int64_t>(RyAnyTag::Float): return "float";
    case static_cast<int64_t>(RyAnyTag::Bool):  return "bool";
    case static_cast<int64_t>(RyAnyTag::Str):   return "str";
    case static_cast<int64_t>(RyAnyTag::Unit):  return "Unit";
    default:        return "unknown";
    }
}

static int64_t extractInt(const RyAny *a) {
    int64_t v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

static double extractFloat(const RyAny *a) {
    double v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

static const char *extractStr(const RyAny *a) {
    const char *v;
    memcpy(&v, a->data, sizeof(v));
    return v;
}

static void makeInt(RyAny *r, int64_t v) {
    r->tag = static_cast<int64_t>(RyAnyTag::Int);
    memcpy(r->data, &v, sizeof(v));
}

static void makeFloat(RyAny *r, double v) {
    r->tag = static_cast<int64_t>(RyAnyTag::Float);
    memcpy(r->data, &v, sizeof(v));
}

static void makeStr(RyAny *r, const char *v) {
    r->tag = static_cast<int64_t>(RyAnyTag::Str);
    memcpy(r->data, &v, sizeof(v));
}

static double toFloat(const RyAny *a) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Float)) return extractFloat(a);
    return static_cast<double>(extractInt(a));
}

static bool isNumericTag(int64_t tag) {
    return tag == static_cast<int64_t>(RyAnyTag::Int) || tag == static_cast<int64_t>(RyAnyTag::Float);
}

static bool hasNaN(const RyAny *a, const RyAny *b) {
    if (!isNumericTag(a->tag) || !isNumericTag(b->tag)) return false;
    if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && std::isnan(extractFloat(a))) return true;
    if (b->tag == static_cast<int64_t>(RyAnyTag::Float) && std::isnan(extractFloat(b))) return true;
    return false;
}

static void repeatStr(RyAny *result, const char *s, int64_t n) {
    if (n <= 0) {
        makeStr(result, makeString("", 0));
        return;
    }
    size_t len = static_cast<size_t>(stringByteLen(s));
    if (len > 0 && (static_cast<uint64_t>(n) > SIZE_MAX ||
                    static_cast<size_t>(n) > SIZE_MAX / len)) {
        fprintf(stderr, "runtime error: string repeat overflow\n");
        exit(1);
    }
    size_t count = static_cast<size_t>(n);
    char *buf = makeStringUninit(len * count);
    for (size_t i = 0; i < count; i++)
        memcpy(buf + i * len, s, len);
    // NUL at buf[len*count] already set by makeStringUninit
    makeStr(result, buf);
}

// ===== String conversion (#225) =====

// Format a double using %g precision but append ".0" for whole-number values
// so that `3.0` prints as "3.0" instead of "3" (Python-compatible, #808).
// Precision stays at %g (~6 digits) to match existing test expectations like
// `to_str(3.14) == "3.14"`.
extern "C" const char *__ry_any_fmt_float(double x) {
    char tmp[64];
    snprintf(tmp, sizeof(tmp), "%g", x);
    // Skip ".0" correction for NaN/Inf ("nan", "inf", "-nan", "-inf") and for
    // values already containing a decimal point or exponent.
    bool needsDotZero = true;
    for (const char *p = tmp; *p; ++p) {
        if (*p == '.' || *p == 'e' || *p == 'E' ||
            *p == 'n' || *p == 'N' || *p == 'i' || *p == 'I') {
            needsDotZero = false;
            break;
        }
    }
    if (needsDotZero) {
        size_t len = strlen(tmp);
        if (len + 3 < sizeof(tmp)) {
            tmp[len] = '.';
            tmp[len + 1] = '0';
            tmp[len + 2] = '\0';
        }
    }
    return makeString(tmp, strlen(tmp));
}

extern "C" const char *__ry_any_to_string(const RyAny *a) {
    switch (a->tag) {
    case static_cast<int64_t>(RyAnyTag::Int): {
        char tmp[32];
        int n = snprintf(tmp, sizeof(tmp), "%lld", (long long)extractInt(a));
        return makeString(tmp, static_cast<size_t>(n > 0 ? n : 0));
    }
    case static_cast<int64_t>(RyAnyTag::Float):
        return __ry_any_fmt_float(extractFloat(a));
    case static_cast<int64_t>(RyAnyTag::Bool):
        return extractInt(a) ? makeString("true", 4) : makeString("false", 5);
    case static_cast<int64_t>(RyAnyTag::Str):
        return extractStr(a); // already a StringHeader handle — do NOT copy
    case static_cast<int64_t>(RyAnyTag::Unit):
        return makeString("Unit", 4);
    default:
        fprintf(stderr,
                "runtime error: __ry_any_to_string: unsupported any tag %lld\n",
                (long long)a->tag);
        exit(1);
    }
}

extern "C" const char *__ry_print_str_quote_escape(const char *raw);

extern "C" const char *__ry_any_to_string_in_collection(const RyAny *a) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Str)) {
        return __ry_print_str_quote_escape(extractStr(a));
    }
    return __ry_any_to_string(a);
}

// ===== Type error (#224) =====

extern "C" void __ry_any_type_error(const char *op, int64_t tag_a, int64_t tag_b) {
    fprintf(stderr, "runtime error: operator %s not supported for %s and %s\n",
            op, tagName(tag_a), tagName(tag_b));
    exit(1);
}

// ===== Arithmetic operators (#221) =====

extern "C" void __ry_any_add(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        makeInt(result, extractInt(a) + extractInt(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, extractFloat(a) + extractFloat(b));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, toFloat(a) + toFloat(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Str) || b->tag == static_cast<int64_t>(RyAnyTag::Str)) {
        if (a->tag != static_cast<int64_t>(RyAnyTag::Str) && a->tag != static_cast<int64_t>(RyAnyTag::Int) && a->tag != static_cast<int64_t>(RyAnyTag::Float) && a->tag != static_cast<int64_t>(RyAnyTag::Bool))
            __ry_any_type_error("+", a->tag, b->tag);
        if (b->tag != static_cast<int64_t>(RyAnyTag::Str) && b->tag != static_cast<int64_t>(RyAnyTag::Int) && b->tag != static_cast<int64_t>(RyAnyTag::Float) && b->tag != static_cast<int64_t>(RyAnyTag::Bool))
            __ry_any_type_error("+", a->tag, b->tag);
        // Non-Str tags allocate a new StringHeader via __ry_any_to_string;
        // Str extracts the existing handle (must NOT be freed).
        bool a_alloc = (a->tag != static_cast<int64_t>(RyAnyTag::Str));
        bool b_alloc = (b->tag != static_cast<int64_t>(RyAnyTag::Str));
        const char *sa = __ry_any_to_string(a);
        const char *sb = __ry_any_to_string(b);
        size_t la = static_cast<size_t>(stringByteLen(sa));
        size_t lb = static_cast<size_t>(stringByteLen(sb));
        char *buf = makeStringUninit(la + lb);
        memcpy(buf, sa, la); // NOLINT(bugprone-not-null-terminated-result) — NUL at buf[la+lb] set by makeStringUninit
        memcpy(buf + la, sb, lb);
        if (a_alloc) freeStringSlot(const_cast<char *>(sa));
        if (b_alloc) freeStringSlot(const_cast<char *>(sb));
        makeStr(result, buf);
    } else {
        __ry_any_type_error("+", a->tag, b->tag);
    }
}

extern "C" void __ry_any_sub(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        makeInt(result, extractInt(a) - extractInt(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, extractFloat(a) - extractFloat(b));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, toFloat(a) - toFloat(b));
    } else {
        __ry_any_type_error("-", a->tag, b->tag);
    }
}

extern "C" void __ry_any_mul(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        makeInt(result, extractInt(a) * extractInt(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, extractFloat(a) * extractFloat(b));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, toFloat(a) * toFloat(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Str) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        repeatStr(result, extractStr(a), extractInt(b));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Str)) {
        repeatStr(result, extractStr(b), extractInt(a));
    } else {
        __ry_any_type_error("*", a->tag, b->tag);
    }
}

extern "C" void __ry_any_div(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        int64_t av = extractInt(a), bv = extractInt(b);
        if (bv == 0) {
            fprintf(stderr, "runtime error: division by zero\n");
            exit(1);
        }
        makeFloat(result, static_cast<double>(av) / static_cast<double>(bv));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, extractFloat(a) / extractFloat(b));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, toFloat(a) / toFloat(b));
    } else {
        __ry_any_type_error("/", a->tag, b->tag);
    }
}

extern "C" void __ry_any_mod(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        int64_t bv = extractInt(b);
        if (bv == 0) {
            fprintf(stderr, "runtime error: modulo by zero\n");
            exit(1);
        }
        // Floor modulo: r = a % b; if (r != 0 && sign(r) != sign(b)) r += b
        int64_t av = extractInt(a);
        int64_t r = av % bv;
        if (r != 0 && ((r ^ bv) < 0)) r += bv;
        makeInt(result, r);
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        double r = std::fmod(extractFloat(a), extractFloat(b));
        if (r != 0.0 && ((r < 0) != (extractFloat(b) < 0))) r += extractFloat(b);
        makeFloat(result, r);
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        double fa = toFloat(a), fb = toFloat(b);
        double r = std::fmod(fa, fb);
        if (r != 0.0 && ((r < 0) != (fb < 0))) r += fb;
        makeFloat(result, r);
    } else {
        __ry_any_type_error("%", a->tag, b->tag);
    }
}

extern "C" void __ry_any_floordiv(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        int64_t av = extractInt(a), bv = extractInt(b);
        if (bv == 0) {
            fprintf(stderr, "runtime error: division by zero\n");
            exit(1);
        }
        int64_t q = av / bv;
        if ((av ^ bv) < 0 && q * bv != av) q--;
        makeInt(result, q);
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, std::floor(extractFloat(a) / extractFloat(b)));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, std::floor(toFloat(a) / toFloat(b)));
    } else {
        __ry_any_type_error("//", a->tag, b->tag);
    }
}

extern "C" void __ry_any_pow(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        makeFloat(result, std::pow(static_cast<double>(extractInt(a)),
                                   static_cast<double>(extractInt(b))));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, std::pow(extractFloat(a), extractFloat(b)));
    } else if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
               (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        makeFloat(result, std::pow(toFloat(a), toFloat(b)));
    } else {
        __ry_any_type_error("**", a->tag, b->tag);
    }
}

extern "C" void __ry_any_neg(RyAny *result, const RyAny *a) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        makeInt(result, -extractInt(a));
    } else if (a->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        makeFloat(result, -extractFloat(a));
    } else {
        fprintf(stderr, "runtime error: unary - not supported for %s\n", tagName(a->tag));
        exit(1);
    }
}

// ===== Comparison operators (#222) =====

extern "C" int64_t __ry_any_eq(const RyAny *a, const RyAny *b) {
    if (a->tag == b->tag) {
        switch (a->tag) {
        case static_cast<int64_t>(RyAnyTag::Int):   return extractInt(a) == extractInt(b) ? 1 : 0;
        case static_cast<int64_t>(RyAnyTag::Float): return extractFloat(a) == extractFloat(b) ? 1 : 0;
        case static_cast<int64_t>(RyAnyTag::Bool):  return extractInt(a) == extractInt(b) ? 1 : 0;
        case static_cast<int64_t>(RyAnyTag::Str): {
            const char *sa = extractStr(a); const char *sb = extractStr(b);
            int64_t la = stringByteLen(sa), lb = stringByteLen(sb);
            return (la == lb && memcmp(sa, sb, static_cast<size_t>(la)) == 0) ? 1 : 0;
        }
        case static_cast<int64_t>(RyAnyTag::Unit):  return 1;
        default:        return 0;
        }
    }
    if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
        (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        return toFloat(a) == toFloat(b) ? 1 : 0;
    }
    return 0;
}

extern "C" int64_t __ry_any_ne(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return __ry_any_eq(a, b) ? 0 : 1;
}

static int64_t orderCompare(const char *op, const RyAny *a, const RyAny *b) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Int)) {
        int64_t av = extractInt(a), bv = extractInt(b);
        return (av > bv) - (av < bv);
    }
    if (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) {
        double av = extractFloat(a), bv = extractFloat(b);
        return (av > bv) - (av < bv);
    }
    if ((a->tag == static_cast<int64_t>(RyAnyTag::Int) && b->tag == static_cast<int64_t>(RyAnyTag::Float)) ||
        (a->tag == static_cast<int64_t>(RyAnyTag::Float) && b->tag == static_cast<int64_t>(RyAnyTag::Int))) {
        double av = toFloat(a), bv = toFloat(b);
        return (av > bv) - (av < bv);
    }
    if (a->tag == static_cast<int64_t>(RyAnyTag::Str) && b->tag == static_cast<int64_t>(RyAnyTag::Str)) {
        const char *sa = extractStr(a); const char *sb = extractStr(b);
        int64_t la = stringByteLen(sa), lb = stringByteLen(sb);
        int64_t minLen = la < lb ? la : lb;
        int r = memcmp(sa, sb, static_cast<size_t>(minLen));
        if (r != 0) return r > 0 ? 1 : -1;
        return (la > lb) ? 1 : (la < lb) ? -1 : 0;
    }
    __ry_any_type_error(op, a->tag, b->tag);
    return 0; // unreachable
}

extern "C" int64_t __ry_any_lt(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return orderCompare("<", a, b) < 0 ? 1 : 0;
}

extern "C" int64_t __ry_any_le(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return orderCompare("<=", a, b) <= 0 ? 1 : 0;
}

extern "C" int64_t __ry_any_gt(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return orderCompare(">", a, b) > 0 ? 1 : 0;
}

extern "C" int64_t __ry_any_ge(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return orderCompare(">=", a, b) >= 0 ? 1 : 0;
}

} // namespace ry
