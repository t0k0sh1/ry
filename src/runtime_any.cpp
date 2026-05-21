#include "ry/runtime_alloc.hpp"
#include "ry/runtime_any.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"
#include <charconv>
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
    case static_cast<int64_t>(RyAnyTag::Int):    return "int";
    case static_cast<int64_t>(RyAnyTag::Float):  return "float";
    case static_cast<int64_t>(RyAnyTag::Bool):   return "bool";
    case static_cast<int64_t>(RyAnyTag::Str):    return "str";
    case static_cast<int64_t>(RyAnyTag::Unit):   return "Unit";
    case static_cast<int64_t>(RyAnyTag::List):   return "List";
    case static_cast<int64_t>(RyAnyTag::Map):    return "Map";
    case static_cast<int64_t>(RyAnyTag::Set):    return "Set";
    case static_cast<int64_t>(RyAnyTag::Record): return "Record";
    case static_cast<int64_t>(RyAnyTag::Enum):   return "Enum";
    default:        return "unknown";
    }
}

static const RyRecordDescriptor *recordBoxDescriptor(const void *dataPtr) {
    const RyRecordDescriptor *desc = nullptr;
    memcpy(&desc, dataPtr, sizeof(const RyRecordDescriptor *));
    return desc;
}

static const void *recordBoxFieldsPtr(const void *dataPtr) {
    return static_cast<const char *>(dataPtr) + sizeof(const RyRecordDescriptor *);
}

static const RyEnumDescriptor *enumBoxDescriptor(const void *dataPtr) {
    const RyEnumDescriptor *desc = nullptr;
    memcpy(&desc, dataPtr, sizeof(const RyEnumDescriptor *));
    return desc;
}

static const void *enumBoxFieldsPtr(const void *dataPtr) {
    return static_cast<const char *>(dataPtr) + sizeof(const RyEnumDescriptor *);
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
        ry_runtime_trap_exit();
    }
    size_t count = static_cast<size_t>(n);
    char *buf = makeStringUninit(len * count);
    for (size_t i = 0; i < count; i++)
        memcpy(buf + i * len, s, len);
    // NUL at buf[len*count] already set by makeStringUninit
    makeStr(result, buf);
}

// ===== C++ constructors for runtime-side `any` synthesis (#1698) =====

RyAny anyFromUnit() {
    RyAny r{};
    r.tag = static_cast<int64_t>(RyAnyTag::Unit);
    memset(r.data, 0, sizeof(r.data));
    return r;
}

RyAny anyFromInt(int64_t v) {
    RyAny r{};
    makeInt(&r, v);
    return r;
}

RyAny anyFromFloat(double v) {
    RyAny r{};
    makeFloat(&r, v);
    return r;
}

RyAny anyFromBool(int64_t v) {
    RyAny r{};
    r.tag = static_cast<int64_t>(RyAnyTag::Bool);
    int64_t bv = v ? 1 : 0;
    memcpy(r.data, &bv, sizeof(bv));
    return r;
}

RyAny anyFromStr(const char *handle) {
    RyAny r{};
    makeStr(&r, handle);
    return r;
}

RyAny anyFromListOfAny(void *list_header) {
    RyAny r{};
    r.tag = static_cast<int64_t>(RyAnyTag::List);
    memcpy(r.data, &list_header, sizeof(list_header));
    return r;
}

RyAny anyFromMapStrAny(void *map_header) {
    RyAny r{};
    r.tag = static_cast<int64_t>(RyAnyTag::Map);
    memcpy(r.data, &map_header, sizeof(map_header));
    return r;
}

// ===== String conversion (#225) =====

// Common implementation for float/double shortest-round-trip formatting.
// std::to_chars overload 3 (no format arg, C++17) gives the fewest decimal
// digits that round-trip exactly — "3.14" stays "3.14", "0.1 + 0.2" becomes
// "0.30000000000000004" (#1031). A trailing ".0" is appended for whole-number
// values so they are visually distinct from int (Python-compatible, #808).
// Separate extern "C" wrappers for double and float avoid FPExt, which would
// change the shortest representation (e.g. "3.14f32" → "3.140000104904175").
template <typename T>
static const char *fmt_float_impl(T x) {
    char tmp[64];
    auto [ptr, ec] = std::to_chars(tmp, tmp + sizeof(tmp), x);
    // Skip ".0" correction for NaN/Inf ("nan", "inf", "-nan", "-inf") and for
    // values already containing a decimal point or exponent.
    bool needsDotZero = true;
    for (const char *p = tmp; p != ptr; ++p) {
        if (*p == '.' || *p == 'e' || *p == 'E' ||
            *p == 'n' || *p == 'N' || *p == 'i' || *p == 'I') {
            needsDotZero = false;
            break;
        }
    }
    size_t finalLen = static_cast<size_t>(ptr - tmp);
    if (needsDotZero && finalLen + 3 < sizeof(tmp)) {
        tmp[finalLen]     = '.';
        tmp[finalLen + 1] = '0';
        finalLen += 2;
    }
    return makeString(tmp, finalLen);
}

extern "C" const char *__ry_any_fmt_float(double x) { return fmt_float_impl(x); }
extern "C" const char *__ry_any_fmt_f32(float x)   { return fmt_float_impl(x); }

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
    // Element-type metadata is lost when a collection is wrapped in `any`, so
    // we cannot render element values here. Emit an opaque marker; user code
    // that needs full pretty-printing must unwrap first via `let xs: List<T> = anyVal`.
    case static_cast<int64_t>(RyAnyTag::List):
        return makeString("<List>", 6);
    case static_cast<int64_t>(RyAnyTag::Map):
        return makeString("<Map>", 5);
    case static_cast<int64_t>(RyAnyTag::Set):
        return makeString("<Set>", 5);
    case static_cast<int64_t>(RyAnyTag::Record): {
        // The box layout is [descriptor ptr (8B)][record fields...]; the
        // any.data[8] slot holds the data-region pointer, so the descriptor
        // is the first 8 bytes at that address.
        const void *dataPtr = nullptr;
        memcpy(&dataPtr, a->data, sizeof(dataPtr));
        const RyRecordDescriptor *desc = recordBoxDescriptor(dataPtr);
        const char *typeName = desc->type_name;
        size_t nameLen = strlen(typeName);
        char *buf = makeStringUninit(nameLen + 2);
        buf[0] = '<';
        memcpy(buf + 1, typeName, nameLen); // NOLINT(bugprone-not-null-terminated-result) — buf[nameLen+2] NUL set by makeStringUninit
        buf[nameLen + 1] = '>';
        return buf;
    }
    case static_cast<int64_t>(RyAnyTag::Enum): {
        // Same `<TypeName>` shape as Record. Enum payload metadata is opaque
        // here — the descriptor's type_name is the only reliable identity
        // post-erasure, so pretty-printing the payload requires unwrapping
        // back to the typed binding first.
        const void *dataPtr = nullptr;
        memcpy(&dataPtr, a->data, sizeof(dataPtr));
        const RyEnumDescriptor *desc = enumBoxDescriptor(dataPtr);
        const char *typeName = desc->type_name;
        size_t nameLen = strlen(typeName);
        char *buf = makeStringUninit(nameLen + 2);
        buf[0] = '<';
        memcpy(buf + 1, typeName, nameLen); // NOLINT(bugprone-not-null-terminated-result) — buf[nameLen+2] NUL set by makeStringUninit
        buf[nameLen + 1] = '>';
        return buf;
    }
    default:
        fprintf(stderr,
                "runtime error: __ry_any_to_string: unsupported any tag %lld\n",
                (long long)a->tag);
        ry_runtime_trap_exit();
    }
}

extern "C" const char *__ry_print_str_quote_escape(const char *raw);

extern "C" const char *__ry_any_to_string_in_collection(const RyAny *a) {
    if (a->tag == static_cast<int64_t>(RyAnyTag::Str)) {
        return __ry_print_str_quote_escape(extractStr(a));
    }
    return __ry_any_to_string(a);
}

// ===== Record-in-any box destructor trampoline (#1797) =====
//
// `emitArcRelease` binds a compile-time FunctionCallee for the dtor. Per-record
// dtors are emitted per type, so we cannot resolve them at the call site —
// instead the codegen stores the per-type LLVM dtor into the record descriptor
// at module-init time, and the ARC release path always points to this single
// trampoline. The trampoline loads the descriptor from offset 0 of the data
// region and dispatches to the per-type dtor.
extern "C" void __ry_arc_dtor_record_dispatch(void *dataPtr) {
    const RyRecordDescriptor *desc = recordBoxDescriptor(dataPtr);
    if (desc && desc->dtor) {
        desc->dtor(dataPtr);
    }
}

// Walk `actual->parent_desc` until either `expected` is found or the chain
// terminates (nullptr). The same descriptor global is reused across all
// instances of a record type, so pointer identity == record-type identity —
// the walk therefore equates to a runtime answer to "is `actual` a subtype of
// `expected`?" (#1802).
extern "C" int64_t __ry_record_is_subtype_desc(const RyRecordDescriptor *actual,
                                                const RyRecordDescriptor *expected) {
    while (actual != nullptr) {
        if (actual == expected) return 1;
        actual = actual->parent_desc;
    }
    return 0;
}

// ===== Enum-in-any box destructor trampoline (#1798) =====
//
// Mirrors `__ry_arc_dtor_record_dispatch`: the codegen stores the per-type
// LLVM enum dtor into the descriptor at module-init time, and the ARC release
// path always points to this single trampoline. The trampoline loads the
// descriptor from offset 0 of the data region and dispatches to the per-type
// dtor — no-op for simple enums (no payload to release), per-variant ARC
// field release for ADT / Option / Result.
extern "C" void __ry_arc_dtor_enum_dispatch(void *dataPtr) {
    const RyEnumDescriptor *desc = enumBoxDescriptor(dataPtr);
    if (desc && desc->dtor) {
        desc->dtor(dataPtr);
    }
}

// ===== Type error (#224) =====

extern "C" void __ry_any_type_error(const char *op, int64_t tag_a, int64_t tag_b) {
    fprintf(stderr, "runtime error: operator %s not supported for %s and %s\n",
            op, tagName(tag_a), tagName(tag_b));
    ry_runtime_trap_exit();
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
        // IEEE 754: int/0 → ±inf, 0/0 → nan (#1023)
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
            ry_runtime_trap_exit();
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
            ry_runtime_trap_exit();
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
        ry_runtime_trap_exit();
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
        case static_cast<int64_t>(RyAnyTag::List):
        case static_cast<int64_t>(RyAnyTag::Map):
        case static_cast<int64_t>(RyAnyTag::Set): {
            // Element-type metadata is erased on wrap, so the runtime
            // cannot recover the per-element stride (1 / 8 / 16 bytes)
            // needed for a safe byte-equality check on the data buffer.
            // List<bool> would read 8x too much (OOB) and List<any>
            // would under-compare (16-byte slots compared at 8-byte
            // stride). All three container kinds therefore unify on
            // pointer identity: two `any` values compare equal iff
            // they hold the *same* underlying header. Deep equality
            // requires unwrapping back to a typed binding.
            const void *pa = nullptr, *pb = nullptr;
            memcpy(&pa, a->data, sizeof(const void *));
            memcpy(&pb, b->data, sizeof(const void *));
            return pa == pb ? 1 : 0;
        }
        case static_cast<int64_t>(RyAnyTag::Record): {
            // Each record type has exactly one descriptor global, so descriptor
            // pointer identity == record-type identity. Different record types
            // therefore compare unequal even when their fields coincidentally
            // hold the same bytes. Same-type values dispatch to the per-type
            // field-wise eq function the codegen emits into desc->eq.
            const void *dpa = nullptr, *dpb = nullptr;
            memcpy(&dpa, a->data, sizeof(const void *));
            memcpy(&dpb, b->data, sizeof(const void *));
            const RyRecordDescriptor *descA = recordBoxDescriptor(dpa);
            const RyRecordDescriptor *descB = recordBoxDescriptor(dpb);
            if (descA != descB) return 0;
            return descA->eq(recordBoxFieldsPtr(dpa), recordBoxFieldsPtr(dpb));
        }
        case static_cast<int64_t>(RyAnyTag::Enum): {
            // Same descriptor-identity rule as Record: each enum type has one
            // descriptor global, so distinct enum types compare unequal. The
            // codegen-emitted per-type eq fn does the deep payload compare
            // (discriminant + per-variant fields for ADT, has_value + payload
            // for Option, is_ok + ok/err for Result, plain i64 eq for simple).
            const void *dpa = nullptr, *dpb = nullptr;
            memcpy(&dpa, a->data, sizeof(const void *));
            memcpy(&dpb, b->data, sizeof(const void *));
            const RyEnumDescriptor *descA = enumBoxDescriptor(dpa);
            const RyEnumDescriptor *descB = enumBoxDescriptor(dpb);
            if (descA != descB) return 0;
            return descA->eq(enumBoxFieldsPtr(dpa), enumBoxFieldsPtr(dpb));
        }
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
