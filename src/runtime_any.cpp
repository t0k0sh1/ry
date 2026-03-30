#include "ry/runtime_any.hpp"
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>

static_assert(sizeof(RyAny) == 16, "RyAny must be 16 bytes to match LLVM anyTy_");
static_assert(offsetof(RyAny, data) == 8, "RyAny::data must be at offset 8");

// ===== Helpers =====

static const char *tagName(int64_t tag) {
    switch (tag) {
    case TAG_INT:   return "int";
    case TAG_FLOAT: return "float";
    case TAG_BOOL:  return "bool";
    case TAG_STR:   return "str";
    case TAG_UNIT:  return "Unit";
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
    r->tag = TAG_INT;
    memcpy(r->data, &v, sizeof(v));
}

static void makeFloat(RyAny *r, double v) {
    r->tag = TAG_FLOAT;
    memcpy(r->data, &v, sizeof(v));
}

static void makeStr(RyAny *r, const char *v) {
    r->tag = TAG_STR;
    memcpy(r->data, &v, sizeof(v));
}

static double toFloat(const RyAny *a) {
    if (a->tag == TAG_FLOAT) return extractFloat(a);
    return static_cast<double>(extractInt(a));
}

static bool isNumericTag(int64_t tag) {
    return tag == TAG_INT || tag == TAG_FLOAT;
}

static bool hasNaN(const RyAny *a, const RyAny *b) {
    if (!isNumericTag(a->tag) || !isNumericTag(b->tag)) return false;
    if (a->tag == TAG_FLOAT && std::isnan(extractFloat(a))) return true;
    if (b->tag == TAG_FLOAT && std::isnan(extractFloat(b))) return true;
    return false;
}

static char *checkedMalloc(size_t size) {
    char *p = static_cast<char *>(malloc(size));
    if (!p) {
        fprintf(stderr, "runtime error: out of memory\n");
        exit(1);
    }
    return p;
}

static void repeatStr(RyAny *result, const char *s, int64_t n) {
    if (n <= 0) {
        char *buf = checkedMalloc(1);
        buf[0] = '\0';
        makeStr(result, buf);
        return;
    }
    size_t len = strlen(s);
    if (len > 0 && (static_cast<uint64_t>(n) > SIZE_MAX ||
                    static_cast<size_t>(n) > SIZE_MAX / len)) {
        fprintf(stderr, "runtime error: string repeat overflow\n");
        exit(1);
    }
    char *buf = checkedMalloc(len * n + 1);
    for (int64_t i = 0; i < n; i++)
        memcpy(buf + i * len, s, len);
    buf[len * n] = '\0';
    makeStr(result, buf);
}

// ===== String conversion (#225) =====

extern "C" const char *__ry_any_to_string(const RyAny *a) {
    switch (a->tag) {
    case TAG_INT: {
        char *buf = checkedMalloc(32);
        snprintf(buf, 32, "%lld", (long long)extractInt(a));
        return buf;
    }
    case TAG_FLOAT: {
        char *buf = checkedMalloc(64);
        snprintf(buf, 64, "%g", extractFloat(a));
        return buf;
    }
    case TAG_BOOL:
        return extractInt(a) ? "true" : "false";
    case TAG_STR:
        return extractStr(a);
    case TAG_UNIT:
        return "Unit";
    default:
        fprintf(stderr,
                "runtime error: __ry_any_to_string: unsupported any tag %lld\n",
                (long long)a->tag);
        exit(1);
    }
}

// ===== Type error (#224) =====

extern "C" void __ry_any_type_error(const char *op, int64_t tag_a, int64_t tag_b) {
    fprintf(stderr, "runtime error: operator %s not supported for %s and %s\n",
            op, tagName(tag_a), tagName(tag_b));
    exit(1);
}

// ===== Arithmetic operators (#221) =====

extern "C" void __ry_any_add(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        makeInt(result, extractInt(a) + extractInt(b));
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, extractFloat(a) + extractFloat(b));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, toFloat(a) + toFloat(b));
    } else if (a->tag == TAG_STR || b->tag == TAG_STR) {
        if (a->tag != TAG_STR && a->tag != TAG_INT && a->tag != TAG_FLOAT && a->tag != TAG_BOOL)
            __ry_any_type_error("+", a->tag, b->tag);
        if (b->tag != TAG_STR && b->tag != TAG_INT && b->tag != TAG_FLOAT && b->tag != TAG_BOOL)
            __ry_any_type_error("+", a->tag, b->tag);
        bool a_alloc = (a->tag == TAG_INT || a->tag == TAG_FLOAT);
        bool b_alloc = (b->tag == TAG_INT || b->tag == TAG_FLOAT);
        const char *sa = __ry_any_to_string(a);
        const char *sb = __ry_any_to_string(b);
        size_t la = strlen(sa), lb = strlen(sb);
        char *buf = checkedMalloc(la + lb + 1);
        memcpy(buf, sa, la);
        memcpy(buf + la, sb, lb + 1);
        if (a_alloc) free(const_cast<char *>(sa));
        if (b_alloc) free(const_cast<char *>(sb));
        makeStr(result, buf);
    } else {
        __ry_any_type_error("+", a->tag, b->tag);
    }
}

extern "C" void __ry_any_sub(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        makeInt(result, extractInt(a) - extractInt(b));
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, extractFloat(a) - extractFloat(b));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, toFloat(a) - toFloat(b));
    } else {
        __ry_any_type_error("-", a->tag, b->tag);
    }
}

extern "C" void __ry_any_mul(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        makeInt(result, extractInt(a) * extractInt(b));
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, extractFloat(a) * extractFloat(b));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, toFloat(a) * toFloat(b));
    } else if (a->tag == TAG_STR && b->tag == TAG_INT) {
        repeatStr(result, extractStr(a), extractInt(b));
    } else if (a->tag == TAG_INT && b->tag == TAG_STR) {
        repeatStr(result, extractStr(b), extractInt(a));
    } else {
        __ry_any_type_error("*", a->tag, b->tag);
    }
}

extern "C" void __ry_any_div(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        int64_t av = extractInt(a), bv = extractInt(b);
        if (bv == 0) {
            fprintf(stderr, "runtime error: division by zero\n");
            exit(1);
        }
        makeFloat(result, static_cast<double>(av) / static_cast<double>(bv));
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, extractFloat(a) / extractFloat(b));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, toFloat(a) / toFloat(b));
    } else {
        __ry_any_type_error("/", a->tag, b->tag);
    }
}

extern "C" void __ry_any_mod(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
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
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        double r = std::fmod(extractFloat(a), extractFloat(b));
        if (r != 0.0 && ((r < 0) != (extractFloat(b) < 0))) r += extractFloat(b);
        makeFloat(result, r);
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        double fa = toFloat(a), fb = toFloat(b);
        double r = std::fmod(fa, fb);
        if (r != 0.0 && ((r < 0) != (fb < 0))) r += fb;
        makeFloat(result, r);
    } else {
        __ry_any_type_error("%", a->tag, b->tag);
    }
}

extern "C" void __ry_any_floordiv(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        int64_t av = extractInt(a), bv = extractInt(b);
        if (bv == 0) {
            fprintf(stderr, "runtime error: division by zero\n");
            exit(1);
        }
        int64_t q = av / bv;
        if ((av ^ bv) < 0 && q * bv != av) q--;
        makeInt(result, q);
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, std::floor(extractFloat(a) / extractFloat(b)));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, std::floor(toFloat(a) / toFloat(b)));
    } else {
        __ry_any_type_error("//", a->tag, b->tag);
    }
}

extern "C" void __ry_any_pow(RyAny *result, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        makeFloat(result, std::pow(static_cast<double>(extractInt(a)),
                                   static_cast<double>(extractInt(b))));
    } else if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        makeFloat(result, std::pow(extractFloat(a), extractFloat(b)));
    } else if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
               (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        makeFloat(result, std::pow(toFloat(a), toFloat(b)));
    } else {
        __ry_any_type_error("**", a->tag, b->tag);
    }
}

extern "C" void __ry_any_neg(RyAny *result, const RyAny *a) {
    if (a->tag == TAG_INT) {
        makeInt(result, -extractInt(a));
    } else if (a->tag == TAG_FLOAT) {
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
        case TAG_INT:   return extractInt(a) == extractInt(b) ? 1 : 0;
        case TAG_FLOAT: return extractFloat(a) == extractFloat(b) ? 1 : 0;
        case TAG_BOOL:  return extractInt(a) == extractInt(b) ? 1 : 0;
        case TAG_STR:   return strcmp(extractStr(a), extractStr(b)) == 0 ? 1 : 0;
        case TAG_UNIT:  return 1;
        default:        return 0;
        }
    }
    if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
        (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        return toFloat(a) == toFloat(b) ? 1 : 0;
    }
    return 0;
}

extern "C" int64_t __ry_any_ne(const RyAny *a, const RyAny *b) {
    if (hasNaN(a, b)) return 0;
    return __ry_any_eq(a, b) ? 0 : 1;
}

static int64_t orderCompare(const char *op, const RyAny *a, const RyAny *b) {
    if (a->tag == TAG_INT && b->tag == TAG_INT) {
        int64_t av = extractInt(a), bv = extractInt(b);
        return (av > bv) - (av < bv);
    }
    if (a->tag == TAG_FLOAT && b->tag == TAG_FLOAT) {
        double av = extractFloat(a), bv = extractFloat(b);
        return (av > bv) - (av < bv);
    }
    if ((a->tag == TAG_INT && b->tag == TAG_FLOAT) ||
        (a->tag == TAG_FLOAT && b->tag == TAG_INT)) {
        double av = toFloat(a), bv = toFloat(b);
        return (av > bv) - (av < bv);
    }
    if (a->tag == TAG_STR && b->tag == TAG_STR) {
        return strcmp(extractStr(a), extractStr(b));
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
