#include "ry/ry_layout.hpp"
#include "ry/runtime_alloc.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"
#include "ry/test_runtime.hpp"
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <random>
#include <string>
#include <unistd.h>
#include <unordered_map>
#include <vector>


namespace ry {

static int g_passed = 0;
static int g_failed = 0;
static int g_describe_depth = 0;
static std::string g_current_it;
static bool g_current_it_failed = false;

static std::string currentIndent(int extra = 0) {
    return std::string(static_cast<size_t>(g_describe_depth * 2 + extra), ' ');
}

// async-signal-safe buffer for the timeout handler in main.cpp
static char g_current_it_buf[256];
const char *__ry_test_current_it_name() { return g_current_it_buf; }

// Per-call argument record for argument-level mock verification
// (#1677, #1703, #1704, #1705, #1706).
// Mock kind tag space — keep in sync with codegen_call_user.cpp:
//   1 = int      (raw i64)                                          — v1 (#1677)
//   2 = float    (bitcast f64 -> i64)                               — v1 (#1677)
//   3 = bool     (zext  i1  -> i64)                                 — v1 (#1677)
//   4 = str      (ptr   -> i64, retain handle)                      — v1 (#1677)
//   5 = opaque   (placeholder, never matches a verify query)
//   6 = list     (ptr to MockListSnapshot)                          — #1703
//   7 = set      (ptr to MockListSnapshot, unordered compare)       — #1704
//   8 = map      (ptr to MockMapSnapshot, unordered key->value cmp) — #1705
//   9 = record   (ptr to MockRecordSnapshot, per-slot compare)      — #1706
//  10 = tuple    (ptr to MockTupleSnapshot, per-slot compare)       — #1706
//  11 = fn       (ptr to MockFnSnapshot; pointer-equality on        — #1707
//                {thunk_ptr, env_ptr})
struct MockArg {
    int64_t kind;
    int64_t value;
};
struct MockCallRecord {
    std::vector<MockArg> args;
};

// Per-collection-arg snapshot for verifyCalledWith (#1703, #1704).
// element_kind: 1=int, 2=float, 3=bool, 4=str (mock kind space).
// element_size: per-element stride supplied by codegen via DataLayout.
// data: deep-copied buffer of len * element_size bytes; for str elements,
// each char* slot is a retained Ry string handle.
// Shape is shared between kind 6 (list) and kind 7 (set) — the kind tag
// on the enclosing MockArg selects ordered vs unordered comparison.
struct MockListSnapshot {
    int64_t len;
    int64_t element_size;
    int8_t  element_kind;
    void   *data;
};

// Mirror of the codegen-emitted SetHeader layout
// (`{i64 len, i64 cap, ptr elems, i64 bucket_count, ptr buckets}`). Only
// `len` and `elems` are read for snapshotting — `elems[0..len-1]` is the
// dense, dedup'd element array.
struct SetHeader {
    int64_t len;
    int64_t cap;
    void   *elems;
    int64_t bucket_count;
    void   *buckets;
};

// Mirror of the codegen-emitted MapHeader layout
// (`{i64 len, i64 cap, ptr keys, ptr vals, i64 bucket_count, ptr buckets}`).
// Only `len`, `keys`, and `vals` are read for snapshotting — both arrays are
// the dense, dedup'd parallel storage indexed by the same i in `[0, len)`.
struct MockMapHeaderView {
    int64_t len;
    int64_t cap;
    void   *keys;
    void   *vals;
    int64_t bucket_count;
    void   *buckets;
};

// Per-Map<K, V> argument snapshot for verifyCalledWith (#1705). Keys and vals
// are deep-copied parallel buffers; the kind tag (1=int, 2=float, 3=bool,
// 4=str, mock kind space) on each side selects retain/release and comparison
// strategy in mockArgEqual's kind-8 branch. Stored separately from
// MockListSnapshot because parallel keys+vals storage cannot be represented
// by a single (data, len, element_size) tuple.
struct MockMapSnapshot {
    int64_t len;
    int64_t key_size;
    int64_t val_size;
    int8_t  key_kind;
    int8_t  val_kind;
    void   *keys;
    void   *vals;
};

// Per-record argument snapshot for verifyCalledWith (#1706). type_name is a
// non-owning pointer to a codegen-emitted global string (the record type name);
// codegen caches one global per record name via cachedGlobalString, so the
// pointer outlives every snapshot built from it. field_kinds and field_values
// are deep-copied per-call into heap arrays so future mutations of the source
// record cannot affect what verifyCalledWith later compares. Each kind=4 (str)
// slot in field_values holds a retained Ry string handle. The fields-as-i64
// per-slot layout (vs whole-struct memcmp) is required because LLVM struct
// padding makes byte-level compare report false negatives on equal-by-value
// records.
struct MockRecordSnapshot {
    const char *type_name;
    int64_t     field_count;
    int8_t     *field_kinds;
    int64_t    *field_values;
};

// Per-tuple argument snapshot for verifyCalledWith (#1706). Tuples have no
// declared name; identity is (arity + per-element kinds). element_kinds and
// element_values are deep-copied per-call into heap arrays. Each kind=4 (str)
// slot in element_values holds a retained Ry string handle.
struct MockTupleSnapshot {
    int64_t  arity;
    int8_t  *element_kinds;
    int64_t *element_values;
};

// Per-fn argument snapshot for verifyCalledWith (#1707). Identity is the
// pair {thunk_ptr, env_ptr} extracted from the uniform closure struct
// `{thunk, env, env_dtor}` produced by wrapAsUniformClosure (env_dtor is
// determined by thunk and omitted). Pointer equality only — no ARC retain
// (issue #1707 explicit). Two source-level identical-but-independently-
// constructed lambdas produce different forwarding/capturing thunks via
// forwarding_thunk_cache_ / capturing_thunk_cache_ keyed on `realFn`, so
// they compare not-equal as required by AC 2.
struct MockFnSnapshot {
    void *thunk_ptr;
    void *env_ptr;
};

// Mock registry. fn_ptr is the replacement function pointer (plain fn ptr
// or closure thunk). env_ptr / env_dtor are non-null only for capture-based
// closures (#1678): the dispatch site reads env_ptr via __ry_mock_get_env
// and calls thunk(env, args...) instead of thunk(args...).
struct MockEntry {
    void *fn_ptr = nullptr;
    void *env_ptr = nullptr;
    void (*env_dtor)(void *) = nullptr;
    int64_t call_count = 0;
    std::vector<MockCallRecord> calls;
    std::vector<char *> retained_str_args;
    std::vector<MockListSnapshot *> retained_list_snapshots;
    std::vector<MockMapSnapshot *> retained_map_snapshots;
    std::vector<MockRecordSnapshot *> retained_record_snapshots;
    std::vector<MockTupleSnapshot *> retained_tuple_snapshots;
    std::vector<MockFnSnapshot *> retained_fn_snapshots;
};
static std::unordered_map<std::string, MockEntry> g_mock_registry;

// Lightweight retain/release for str handles inside the mock registry.
// We do not link against the codegen-emitted IR retain/release helpers from
// C++, so reproduce the protocol locally: respect ARC_IMMORTAL, atomically
// adjust strong_count at offset -STRING_HEADER_SIZE, and free via
// freeStringSlot when the last reference drops.
static inline int64_t *strStrongCountPtr(const char *handle) {
    return reinterpret_cast<int64_t *>(
        const_cast<char *>(handle) -
        static_cast<ptrdiff_t>(STRING_HEADER_SIZE));
}

static void mockRetainStr(const char *handle) {
    if (!handle) return;
    int64_t *strong = strStrongCountPtr(handle);
    int64_t cur = __atomic_load_n(strong, __ATOMIC_RELAXED);
    if (cur == ARC_IMMORTAL) return;
    __atomic_fetch_add(strong, 1, __ATOMIC_SEQ_CST);
}

static void mockReleaseStr(char *handle) {
    if (!handle) return;
    int64_t *strong = strStrongCountPtr(handle);
    int64_t cur = __atomic_load_n(strong, __ATOMIC_RELAXED);
    if (cur == ARC_IMMORTAL) return;
    int64_t prev = __atomic_fetch_sub(strong, 1, __ATOMIC_SEQ_CST);
    if (prev == 1) freeStringSlot(handle);
}

// Build a deep-copy snapshot of a List<T> argument. The element buffer is
// duplicated with checked_malloc so future mutations of the source list
// (CoW or in-place) cannot affect what verifyCalledWith later compares.
// For str elements, each char* slot is retained so the StringHeader stays
// alive until the snapshot is released.
static MockListSnapshot *makeMockListSnapshot(void *listHeaderPtr,
                                                int64_t element_kind,
                                                int64_t element_size) {
    auto *snap = new MockListSnapshot{};
    snap->element_size = element_size;
    snap->element_kind = static_cast<int8_t>(element_kind);
    snap->len = 0;
    snap->data = nullptr;
    if (!listHeaderPtr) return snap;
    auto *header = static_cast<ListHeader *>(listHeaderPtr);
    int64_t len = header->len;
    snap->len = len;
    if (len > 0 && element_size > 0) {
        size_t totalBytes = static_cast<size_t>(len) *
                            static_cast<size_t>(element_size);
        snap->data = checked_malloc(totalBytes);
        std::memcpy(snap->data, header->data, totalBytes);
        if (element_kind == 4) {
            // str: retain each element handle.
            char **handles = reinterpret_cast<char **>(snap->data);
            for (int64_t i = 0; i < len; ++i)
                mockRetainStr(handles[i]);
        }
    }
    return snap;
}

// Build a deep-copy snapshot of a Set<T> argument. Reads the dense element
// array (`SetHeader.elems[0..len-1]`) — already dedup'd by the runtime — so
// we never iterate the bucket array. Layout-wise this is a parallel copy of
// makeMockListSnapshot rather than a `void*` -> `ListHeader*` cast: List and
// Set headers share the `len` offset by coincidence, but `data` for List
// vs `elems` for Set sit at different offsets, and casting one as the other
// would be strict-aliasing UB on the field that does differ.
static MockListSnapshot *makeMockSetSnapshot(void *setHeaderPtr,
                                              int64_t element_kind,
                                              int64_t element_size) {
    auto *snap = new MockListSnapshot{};
    snap->element_size = element_size;
    snap->element_kind = static_cast<int8_t>(element_kind);
    snap->len = 0;
    snap->data = nullptr;
    if (!setHeaderPtr) return snap;
    auto *header = static_cast<SetHeader *>(setHeaderPtr);
    int64_t len = header->len;
    snap->len = len;
    if (len > 0 && element_size > 0) {
        size_t totalBytes = static_cast<size_t>(len) *
                            static_cast<size_t>(element_size);
        snap->data = checked_malloc(totalBytes);
        std::memcpy(snap->data, header->elems, totalBytes);
        if (element_kind == 4) {
            char **handles = reinterpret_cast<char **>(snap->data);
            for (int64_t i = 0; i < len; ++i)
                mockRetainStr(handles[i]);
        }
    }
    return snap;
}

static void freeMockListSnapshot(MockListSnapshot *snap) {
    if (!snap) return;
    if (snap->data) {
        if (snap->element_kind == 4) {
            char **handles = reinterpret_cast<char **>(snap->data);
            for (int64_t i = 0; i < snap->len; ++i)
                mockReleaseStr(handles[i]);
        }
        std::free(snap->data);
    }
    delete snap;
}

// Build a deep-copy snapshot of a Map<K, V> argument. Reads MapHeader.{keys,vals}
// in parallel for [0, len). For str slots on either side, each char* is retained
// so the StringHeader stays alive until the snapshot is released. The map is
// already dedup'd by the runtime, so each key appears exactly once — comparison
// in mockArgEqual exploits that for an O(n^2) "find key, then check value" loop.
static MockMapSnapshot *makeMockMapSnapshot(void *mapHeaderPtr,
                                              int64_t key_kind, int64_t key_size,
                                              int64_t val_kind, int64_t val_size) {
    auto *snap = new MockMapSnapshot{};
    snap->len = 0;
    snap->key_size = key_size;
    snap->val_size = val_size;
    snap->key_kind = static_cast<int8_t>(key_kind);
    snap->val_kind = static_cast<int8_t>(val_kind);
    snap->keys = nullptr;
    snap->vals = nullptr;
    if (!mapHeaderPtr) return snap;
    auto *header = static_cast<MockMapHeaderView *>(mapHeaderPtr);
    int64_t len = header->len;
    snap->len = len;
    if (len > 0) {
        if (key_size > 0) {
            size_t keyBytes = static_cast<size_t>(len) *
                              static_cast<size_t>(key_size);
            snap->keys = checked_malloc(keyBytes);
            std::memcpy(snap->keys, header->keys, keyBytes);
            if (key_kind == 4) {
                char **handles = reinterpret_cast<char **>(snap->keys);
                for (int64_t i = 0; i < len; ++i)
                    mockRetainStr(handles[i]);
            }
        }
        if (val_size > 0) {
            size_t valBytes = static_cast<size_t>(len) *
                              static_cast<size_t>(val_size);
            snap->vals = checked_malloc(valBytes);
            std::memcpy(snap->vals, header->vals, valBytes);
            if (val_kind == 4) {
                char **handles = reinterpret_cast<char **>(snap->vals);
                for (int64_t i = 0; i < len; ++i)
                    mockRetainStr(handles[i]);
            }
        }
    }
    return snap;
}

static void freeMockMapSnapshot(MockMapSnapshot *snap) {
    if (!snap) return;
    if (snap->keys) {
        if (snap->key_kind == 4) {
            char **handles = reinterpret_cast<char **>(snap->keys);
            for (int64_t i = 0; i < snap->len; ++i)
                mockReleaseStr(handles[i]);
        }
        std::free(snap->keys);
    }
    if (snap->vals) {
        if (snap->val_kind == 4) {
            char **handles = reinterpret_cast<char **>(snap->vals);
            for (int64_t i = 0; i < snap->len; ++i)
                mockReleaseStr(handles[i]);
        }
        std::free(snap->vals);
    }
    delete snap;
}

// Build a deep-copy snapshot of a record argument (#1706). type_name is
// non-owning (codegen-emitted global). field_kinds + field_values arrays are
// duplicated with checked_malloc; each kind=4 (str) slot retains its handle.
static MockRecordSnapshot *makeMockRecordSnapshot(const char *type_name,
                                                    int64_t field_count,
                                                    const int8_t *kinds,
                                                    const int64_t *values) {
    auto *snap = new MockRecordSnapshot{};
    snap->type_name = type_name;
    snap->field_count = field_count;
    snap->field_kinds = nullptr;
    snap->field_values = nullptr;
    if (field_count > 0) {
        snap->field_kinds = static_cast<int8_t *>(
            checked_malloc(static_cast<size_t>(field_count) * sizeof(int8_t)));
        snap->field_values = static_cast<int64_t *>(
            checked_malloc(static_cast<size_t>(field_count) * sizeof(int64_t)));
        std::memcpy(snap->field_kinds, kinds,
                    static_cast<size_t>(field_count) * sizeof(int8_t));
        std::memcpy(snap->field_values, values,
                    static_cast<size_t>(field_count) * sizeof(int64_t));
        for (int64_t i = 0; i < field_count; ++i) {
            if (snap->field_kinds[i] == 4) {
                auto *handle =
                    reinterpret_cast<char *>(snap->field_values[i]);
                mockRetainStr(handle);
            }
        }
    }
    return snap;
}

static void freeMockRecordSnapshot(MockRecordSnapshot *snap) {
    if (!snap) return;
    if (snap->field_values) {
        for (int64_t i = 0; i < snap->field_count; ++i) {
            if (snap->field_kinds && snap->field_kinds[i] == 4) {
                auto *handle =
                    reinterpret_cast<char *>(snap->field_values[i]);
                mockReleaseStr(handle);
            }
        }
        std::free(snap->field_values);
    }
    if (snap->field_kinds) std::free(snap->field_kinds);
    delete snap;
}

// Build a deep-copy snapshot of a tuple argument (#1706). Identity is shape
// only (arity + per-element kinds); no type_name. Same retain/release rules
// for kind=4 slots as record/list snapshots.
static MockTupleSnapshot *makeMockTupleSnapshot(int64_t arity,
                                                  const int8_t *kinds,
                                                  const int64_t *values) {
    auto *snap = new MockTupleSnapshot{};
    snap->arity = arity;
    snap->element_kinds = nullptr;
    snap->element_values = nullptr;
    if (arity > 0) {
        snap->element_kinds = static_cast<int8_t *>(
            checked_malloc(static_cast<size_t>(arity) * sizeof(int8_t)));
        snap->element_values = static_cast<int64_t *>(
            checked_malloc(static_cast<size_t>(arity) * sizeof(int64_t)));
        std::memcpy(snap->element_kinds, kinds,
                    static_cast<size_t>(arity) * sizeof(int8_t));
        std::memcpy(snap->element_values, values,
                    static_cast<size_t>(arity) * sizeof(int64_t));
        for (int64_t i = 0; i < arity; ++i) {
            if (snap->element_kinds[i] == 4) {
                auto *handle =
                    reinterpret_cast<char *>(snap->element_values[i]);
                mockRetainStr(handle);
            }
        }
    }
    return snap;
}

static void freeMockTupleSnapshot(MockTupleSnapshot *snap) {
    if (!snap) return;
    if (snap->element_values) {
        for (int64_t i = 0; i < snap->arity; ++i) {
            if (snap->element_kinds && snap->element_kinds[i] == 4) {
                auto *handle =
                    reinterpret_cast<char *>(snap->element_values[i]);
                mockReleaseStr(handle);
            }
        }
        std::free(snap->element_values);
    }
    if (snap->element_kinds) std::free(snap->element_kinds);
    delete snap;
}

// Build an fn-arg snapshot (#1707). thunk_ptr / env_ptr are extracted by the
// caller from the uniform closure struct `{thunk, env, env_dtor}`. Pointer
// equality only — no ARC retain on env (issue explicit; closure env lifetime
// is owned by the caller's scope).
static MockFnSnapshot *makeMockFnSnapshot(void *thunk_ptr, void *env_ptr) {
    auto *snap = new MockFnSnapshot{};
    snap->thunk_ptr = thunk_ptr;
    snap->env_ptr = env_ptr;
    return snap;
}

static void freeMockFnSnapshot(MockFnSnapshot *snap) { delete snap; }

extern "C" {
void __ry_arc_free_counted(void *header_ptr);
}

// Capture-closure env release for mock registry (#1678). The env is the
// ARC-managed closure data pointer; ArcHeader (16 bytes: strong_count +
// weak_count) sits at offset -ARC_HEADER_SIZE. We reproduce the standard ARC
// release sequence rather than calling into codegen helpers because this code
// path runs from C++.
static void mockReleaseClosureEnv(void *env_ptr, void (*env_dtor)(void *)) {
    if (!env_ptr) return;
    int64_t *strong = reinterpret_cast<int64_t *>(
        static_cast<char *>(env_ptr) - static_cast<ptrdiff_t>(ARC_HEADER_SIZE));
    int64_t cur = __atomic_load_n(strong, __ATOMIC_RELAXED);
    if (cur == ARC_IMMORTAL) return;
    int64_t prev = __atomic_fetch_sub(strong, 1, __ATOMIC_SEQ_CST);
    if (prev == 1) {
        if (env_dtor) env_dtor(env_ptr);
        __ry_arc_free_counted(
            static_cast<char *>(env_ptr) - static_cast<ptrdiff_t>(ARC_HEADER_SIZE));
    }
}

extern "C" {

void __ry_test_describe_begin(const char *name) {
    std::printf("%s%s\n", currentIndent().c_str(), name);
    ++g_describe_depth;
}

void __ry_test_describe_end() {
    if (g_describe_depth > 0) --g_describe_depth;
}

// ASan slows synchronization primitives ~3-10x; extend timeout to avoid
// flaky SIGALRM in concurrency tests on CI.
#if defined(__SANITIZE_ADDRESS__) || (defined(__has_feature) && __has_feature(address_sanitizer))
static constexpr unsigned kTestTimeoutSec = 300;
#else
static constexpr unsigned kTestTimeoutSec = 60;
#endif

void __ry_test_it_begin(const char *name) {
    g_current_it = name;
    g_current_it_failed = false;
    // Copy to async-signal-safe buffer for timeout handler
    std::snprintf(g_current_it_buf, sizeof(g_current_it_buf), "%s", name);
    alarm(kTestTimeoutSec);
}

void __ry_test_it_end() {
    alarm(0);
    __ry_mock_clear_all();
    const auto indent = currentIndent();
    if (g_current_it_failed) {
        std::printf("%s\033[31m- %s\033[0m\n", indent.c_str(), g_current_it.c_str());
        ++g_failed;
    } else {
        std::printf("%s\033[32m+ %s\033[0m\n", indent.c_str(), g_current_it.c_str());
        ++g_passed;
    }
    std::fflush(stdout);
    g_current_it.clear();
}

void __ry_test_expect_fail(int line, const char *actual, const char *expected) {
    g_current_it_failed = true;
    std::printf("%s\033[31mline %d: expected %s, got %s\033[0m\n", currentIndent(2).c_str(), line, expected, actual);
}

void __ry_test_fail(int line, const char *msg) {
    g_current_it_failed = true;
    const auto indent = currentIndent(2);
    if (msg && msg[0] != '\0') {
        std::printf("%s\033[31mline %d: %s\033[0m\n", indent.c_str(), line, msg);
    } else {
        std::printf("%s\033[31mline %d: test failed\033[0m\n", indent.c_str(), line);
    }
}

int __ry_test_summary() {
    std::printf("\n%d passed, %d failed\n", g_passed, g_failed);
    std::fflush(stdout);
    int result = g_failed;
    // Reset for potential multiple invocations
    g_passed = 0;
    g_failed = 0;
    return result;
}

// ===== Mock support =====

void __ry_mock_set(const char *name, void *fn_ptr) {
    auto &entry = g_mock_registry[name];
    mockReleaseClosureEnv(entry.env_ptr, entry.env_dtor);
    for (char *s : entry.retained_str_args) mockReleaseStr(s);
    for (auto *snap : entry.retained_list_snapshots) freeMockListSnapshot(snap);
    for (auto *snap : entry.retained_map_snapshots) freeMockMapSnapshot(snap);
    for (auto *snap : entry.retained_record_snapshots) freeMockRecordSnapshot(snap);
    for (auto *snap : entry.retained_tuple_snapshots) freeMockTupleSnapshot(snap);
    for (auto *snap : entry.retained_fn_snapshots) freeMockFnSnapshot(snap);
    entry = MockEntry{};
    entry.fn_ptr = fn_ptr;
}

void __ry_mock_set_closure(const char *name, void *thunk_ptr,
                            void *env_ptr, void (*env_dtor)(void *)) {
    auto &entry = g_mock_registry[name];
    mockReleaseClosureEnv(entry.env_ptr, entry.env_dtor);
    for (char *s : entry.retained_str_args) mockReleaseStr(s);
    for (auto *snap : entry.retained_list_snapshots) freeMockListSnapshot(snap);
    for (auto *snap : entry.retained_map_snapshots) freeMockMapSnapshot(snap);
    for (auto *snap : entry.retained_record_snapshots) freeMockRecordSnapshot(snap);
    for (auto *snap : entry.retained_tuple_snapshots) freeMockTupleSnapshot(snap);
    for (auto *snap : entry.retained_fn_snapshots) freeMockFnSnapshot(snap);
    entry = MockEntry{};
    entry.fn_ptr = thunk_ptr;
    entry.env_ptr = env_ptr;
    entry.env_dtor = env_dtor;
}

void *__ry_mock_get(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it != g_mock_registry.end()) return it->second.fn_ptr;
    return nullptr;
}

void *__ry_mock_get_env(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it != g_mock_registry.end()) return it->second.env_ptr;
    return nullptr;
}

int64_t __ry_mock_get_call_count(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it != g_mock_registry.end()) return it->second.call_count;
    return 0;
}

void __ry_mock_increment_call(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it != g_mock_registry.end()) ++it->second.call_count;
}

void *__ry_mock_begin_call_record(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it == g_mock_registry.end()) return nullptr;
    it->second.calls.emplace_back();
    return &it->second.calls.back();
}

void __ry_mock_store_arg(void *record, int64_t kind, int64_t value,
                          const char *mockName) {
    if (!record) return;
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({kind, value});
    if (kind == 4 && mockName) {
        // str arg: retain to keep the handle alive until __ry_mock_clear_all.
        char *handle = reinterpret_cast<char *>(value);
        if (handle) {
            mockRetainStr(handle);
            auto it = g_mock_registry.find(mockName);
            if (it != g_mock_registry.end())
                it->second.retained_str_args.push_back(handle);
        }
    }
}

void __ry_mock_store_arg_list(void *record, void *listHeaderPtr,
                                int64_t element_kind, int64_t element_size,
                                const char *mockName) {
    if (!record) return;
    auto *snap = makeMockListSnapshot(listHeaderPtr, element_kind, element_size);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({6, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_list_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_list_snapshot(void *listHeaderPtr, int64_t element_kind,
                                     int64_t element_size) {
    return makeMockListSnapshot(listHeaderPtr, element_kind, element_size);
}

void __ry_mock_store_arg_set(void *record, void *setHeaderPtr,
                              int64_t element_kind, int64_t element_size,
                              const char *mockName) {
    if (!record) return;
    auto *snap = makeMockSetSnapshot(setHeaderPtr, element_kind, element_size);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({7, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_list_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_set_snapshot(void *setHeaderPtr, int64_t element_kind,
                                    int64_t element_size) {
    return makeMockSetSnapshot(setHeaderPtr, element_kind, element_size);
}

void __ry_mock_store_arg_map(void *record, void *mapHeaderPtr,
                              int64_t key_kind, int64_t key_size,
                              int64_t val_kind, int64_t val_size,
                              const char *mockName) {
    if (!record) return;
    auto *snap = makeMockMapSnapshot(mapHeaderPtr, key_kind, key_size,
                                      val_kind, val_size);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({8, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_map_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_map_snapshot(void *mapHeaderPtr,
                                    int64_t key_kind, int64_t key_size,
                                    int64_t val_kind, int64_t val_size) {
    return makeMockMapSnapshot(mapHeaderPtr, key_kind, key_size, val_kind, val_size);
}

// Recording side: store a record argument as a per-call snapshot (#1706).
// type_name points to a codegen-emitted global string and outlives the
// snapshot. kinds and values are caller-allocated buffers (typically stack
// alloca'd in the call site); makeMockRecordSnapshot deep-copies both.
void __ry_mock_store_arg_record(void *record, const char *type_name,
                                 int64_t field_count, const int8_t *kinds,
                                 const int64_t *values, const char *mockName) {
    if (!record) return;
    auto *snap = makeMockRecordSnapshot(type_name, field_count, kinds, values);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({9, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_record_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_record_snapshot(const char *type_name,
                                      int64_t field_count,
                                      const int8_t *kinds,
                                      const int64_t *values) {
    return makeMockRecordSnapshot(type_name, field_count, kinds, values);
}

// Recording side: store a tuple argument as a per-call snapshot (#1706).
// kinds and values are caller-allocated buffers; makeMockTupleSnapshot
// deep-copies both.
void __ry_mock_store_arg_tuple(void *record, int64_t arity,
                                const int8_t *kinds, const int64_t *values,
                                const char *mockName) {
    if (!record) return;
    auto *snap = makeMockTupleSnapshot(arity, kinds, values);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({10, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_tuple_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_tuple_snapshot(int64_t arity, const int8_t *kinds,
                                      const int64_t *values) {
    return makeMockTupleSnapshot(arity, kinds, values);
}

// Recording side: store an fn-typed argument as a per-call {thunk, env}
// snapshot (#1707). thunk_ptr / env_ptr are extracted by codegen from the
// uniform closure struct produced by wrapAsUniformClosure. Pointer equality
// only; no retain.
void __ry_mock_store_arg_fn(void *record, void *thunk_ptr, void *env_ptr,
                              const char *mockName) {
    if (!record) return;
    auto *snap = makeMockFnSnapshot(thunk_ptr, env_ptr);
    auto *rec = static_cast<MockCallRecord *>(record);
    rec->args.push_back({11, reinterpret_cast<int64_t>(snap)});
    if (mockName) {
        auto it = g_mock_registry.find(mockName);
        if (it != g_mock_registry.end())
            it->second.retained_fn_snapshots.push_back(snap);
    }
}

void *__ry_mock_make_fn_snapshot(void *thunk_ptr, void *env_ptr) {
    return makeMockFnSnapshot(thunk_ptr, env_ptr);
}

static bool mockArgEqual(const MockArg &recorded, int64_t expectedKind,
                          int64_t expectedValue) {
    if (recorded.kind != expectedKind) return false;
    if (recorded.kind == 4) {
        const char *a = reinterpret_cast<const char *>(recorded.value);
        const char *b = reinterpret_cast<const char *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        int64_t la = stringByteLen(a);
        int64_t lb = stringByteLen(b);
        if (la != lb) return false;
        return memcmp(a, b, static_cast<size_t>(la)) == 0;
    }
    if (recorded.kind == 6) {
        auto *a = reinterpret_cast<MockListSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockListSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        if (a->len != b->len) return false;
        if (a->element_kind != b->element_kind) return false;
        if (a->len == 0) return true;
        if (a->element_kind == 4) {
            char **ha = reinterpret_cast<char **>(a->data);
            char **hb = reinterpret_cast<char **>(b->data);
            for (int64_t i = 0; i < a->len; ++i) {
                const char *sa = ha[i];
                const char *sb = hb[i];
                if (sa == sb) continue;
                if (!sa || !sb) return false;
                int64_t la = stringByteLen(sa);
                int64_t lb = stringByteLen(sb);
                if (la != lb) return false;
                if (std::memcmp(sa, sb, static_cast<size_t>(la)) != 0)
                    return false;
            }
            return true;
        }
        if (a->element_size != b->element_size) return false;
        return std::memcmp(a->data, b->data,
                            static_cast<size_t>(a->len) *
                                static_cast<size_t>(a->element_size)) == 0;
    }
    if (recorded.kind == 7) {
        // Set: unordered membership comparison. Both sides are dedup'd by
        // the runtime, so equal `len` + every element of `a` having a
        // structurally-equal counterpart in `b` is sufficient. O(n^2) is
        // fine for testing (no production hot path).
        auto *a = reinterpret_cast<MockListSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockListSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        if (a->len != b->len) return false;
        if (a->element_kind != b->element_kind) return false;
        if (a->len == 0) return true;
        if (a->element_kind == 4) {
            char **ha = reinterpret_cast<char **>(a->data);
            char **hb = reinterpret_cast<char **>(b->data);
            for (int64_t i = 0; i < a->len; ++i) {
                const char *sa = ha[i];
                int64_t la = sa ? stringByteLen(sa) : 0;
                bool found = false;
                for (int64_t j = 0; j < b->len; ++j) {
                    const char *sb = hb[j];
                    if (sa == sb) { found = true; break; }
                    if (!sa || !sb) continue;
                    int64_t lb = stringByteLen(sb);
                    if (la != lb) continue;
                    if (std::memcmp(sa, sb, static_cast<size_t>(la)) == 0) {
                        found = true;
                        break;
                    }
                }
                if (!found) return false;
            }
            return true;
        }
        if (a->element_size != b->element_size) return false;
        const auto stride = static_cast<size_t>(a->element_size);
        const char *ad = static_cast<const char *>(a->data);
        const char *bd = static_cast<const char *>(b->data);
        for (int64_t i = 0; i < a->len; ++i) {
            bool found = false;
            for (int64_t j = 0; j < b->len; ++j) {
                if (std::memcmp(ad + static_cast<size_t>(i) * stride,
                                 bd + static_cast<size_t>(j) * stride,
                                 stride) == 0) {
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }
        return true;
    }
    if (recorded.kind == 8) {
        // Map: unordered key->value comparison. Both sides are dedup'd by
        // the runtime hash table, so each key appears exactly once. For
        // every key in a we look it up in b (linear scan), then compare
        // the corresponding value. O(n^2) total — fine for testing.
        auto *a = reinterpret_cast<MockMapSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockMapSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        if (a->len != b->len) return false;
        if (a->key_kind != b->key_kind) return false;
        if (a->val_kind != b->val_kind) return false;
        if (a->len == 0) return true;
        // Non-str slots compare by raw bytes — sizes must match. Str slots
        // compare via stringByteLen + memcmp regardless of element_size.
        if (a->key_kind != 4 && a->key_size != b->key_size) return false;
        if (a->val_kind != 4 && a->val_size != b->val_size) return false;

        auto strSlotEq = [](void *base_a, void *base_b, int64_t i, int64_t j) {
            const char *sa = reinterpret_cast<char **>(base_a)[i];
            const char *sb = reinterpret_cast<char **>(base_b)[j];
            if (sa == sb) return true;
            if (!sa || !sb) return false;
            int64_t la = stringByteLen(sa);
            int64_t lb = stringByteLen(sb);
            if (la != lb) return false;
            return std::memcmp(sa, sb, static_cast<size_t>(la)) == 0;
        };
        auto byteSlotEq = [](void *base_a, void *base_b, int64_t i, int64_t j,
                              int64_t stride_bytes) {
            const auto stride = static_cast<size_t>(stride_bytes);
            return std::memcmp(
                static_cast<const char *>(base_a) + static_cast<size_t>(i) * stride,
                static_cast<const char *>(base_b) + static_cast<size_t>(j) * stride,
                stride) == 0;
        };

        for (int64_t i = 0; i < a->len; ++i) {
            int64_t j = -1;
            for (int64_t k = 0; k < b->len; ++k) {
                bool keyMatch = a->key_kind == 4
                    ? strSlotEq(a->keys, b->keys, i, k)
                    : byteSlotEq(a->keys, b->keys, i, k, a->key_size);
                if (keyMatch) { j = k; break; }
            }
            if (j < 0) return false;
            bool valMatch = a->val_kind == 4
                ? strSlotEq(a->vals, b->vals, i, j)
                : byteSlotEq(a->vals, b->vals, i, j, a->val_size);
            if (!valMatch) return false;
        }
        return true;
    }
    if (recorded.kind == 9) {
        // Record: identity is type_name + per-slot kinds + per-slot values.
        // Both type_name pointers come from cachedGlobalString and are stable
        // for the run, but the same name can be emitted from two distinct
        // compile units; compare via strcmp for safety. Per-slot compare
        // mirrors the kind-tag space of scalar/str args.
        auto *a = reinterpret_cast<MockRecordSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockRecordSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        if (a->field_count != b->field_count) return false;
        if (a->type_name != b->type_name) {
            if (!a->type_name || !b->type_name) return false;
            if (std::strcmp(a->type_name, b->type_name) != 0) return false;
        }
        for (int64_t i = 0; i < a->field_count; ++i) {
            if (a->field_kinds[i] != b->field_kinds[i]) return false;
            if (a->field_kinds[i] == 4) {
                const char *sa =
                    reinterpret_cast<const char *>(a->field_values[i]);
                const char *sb =
                    reinterpret_cast<const char *>(b->field_values[i]);
                if (sa == sb) continue;
                if (!sa || !sb) return false;
                int64_t la = stringByteLen(sa);
                int64_t lb = stringByteLen(sb);
                if (la != lb) return false;
                if (std::memcmp(sa, sb, static_cast<size_t>(la)) != 0)
                    return false;
            } else {
                if (a->field_values[i] != b->field_values[i]) return false;
            }
        }
        return true;
    }
    if (recorded.kind == 11) {
        // Fn: identity is the pair {thunk_ptr, env_ptr} extracted from the
        // uniform closure struct (#1707). For non-capturing fns the env is
        // null on both sides; for capturing closures the env pointer is the
        // ARC-managed env data ptr, which is unique per `let g = makeAdder(5)`
        // invocation. forwarding_thunk_cache_ / capturing_thunk_cache_ ensure
        // that a single source-level lambda always maps to one thunk, so:
        //   - same source-level fn passed twice → match
        //   - different source-level fns → different thunks → no match
        //   - same `makeAdder` invoked twice → same thunk + different envs → no match
        auto *a = reinterpret_cast<MockFnSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockFnSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        return a->thunk_ptr == b->thunk_ptr && a->env_ptr == b->env_ptr;
    }
    if (recorded.kind == 10) {
        // Tuple: identity is (arity + per-element kinds + per-element values).
        // No declared name; arity mismatch is a compile-time reject (Stage 2)
        // but we still defensively compare here.
        auto *a = reinterpret_cast<MockTupleSnapshot *>(recorded.value);
        auto *b = reinterpret_cast<MockTupleSnapshot *>(expectedValue);
        if (a == b) return true;
        if (!a || !b) return false;
        if (a->arity != b->arity) return false;
        for (int64_t i = 0; i < a->arity; ++i) {
            if (a->element_kinds[i] != b->element_kinds[i]) return false;
            if (a->element_kinds[i] == 4) {
                const char *sa =
                    reinterpret_cast<const char *>(a->element_values[i]);
                const char *sb =
                    reinterpret_cast<const char *>(b->element_values[i]);
                if (sa == sb) continue;
                if (!sa || !sb) return false;
                int64_t la = stringByteLen(sa);
                int64_t lb = stringByteLen(sb);
                if (la != lb) return false;
                if (std::memcmp(sa, sb, static_cast<size_t>(la)) != 0)
                    return false;
            } else {
                if (a->element_values[i] != b->element_values[i]) return false;
            }
        }
        return true;
    }
    return recorded.value == expectedValue;
}

int64_t __ry_mock_count_matching_calls(const char *name, int64_t numArgs,
                                        const int64_t *kinds,
                                        const int64_t *values) {
    auto it = g_mock_registry.find(name);
    int64_t matched = 0;
    if (it != g_mock_registry.end()) {
        for (const auto &call : it->second.calls) {
            if (static_cast<int64_t>(call.args.size()) != numArgs) continue;
            bool ok = true;
            for (int64_t i = 0; i < numArgs; ++i) {
                if (!mockArgEqual(call.args[static_cast<size_t>(i)],
                                  kinds[i], values[i])) {
                    ok = false;
                    break;
                }
            }
            if (ok) ++matched;
        }
    }
    // Take ownership of caller-supplied snapshots regardless of whether the
    // mock is registered, so the verify path is leak-free even when the mock
    // was never set. kind 6 (list) and kind 7 (set) share MockListSnapshot;
    // kind 8 (map) uses MockMapSnapshot — separate free path. kind 9 (record)
    // and kind 10 (tuple) each have their own free helper.
    for (int64_t i = 0; i < numArgs; ++i) {
        if (kinds[i] == 6 || kinds[i] == 7) {
            freeMockListSnapshot(
                reinterpret_cast<MockListSnapshot *>(values[i]));
        } else if (kinds[i] == 8) {
            freeMockMapSnapshot(
                reinterpret_cast<MockMapSnapshot *>(values[i]));
        } else if (kinds[i] == 9) {
            freeMockRecordSnapshot(
                reinterpret_cast<MockRecordSnapshot *>(values[i]));
        } else if (kinds[i] == 10) {
            freeMockTupleSnapshot(
                reinterpret_cast<MockTupleSnapshot *>(values[i]));
        } else if (kinds[i] == 11) {
            freeMockFnSnapshot(
                reinterpret_cast<MockFnSnapshot *>(values[i]));
        }
    }
    return matched;
}

void __ry_mock_clear_all() {
    for (auto &kv : g_mock_registry) {
        auto &entry = kv.second;
        mockReleaseClosureEnv(entry.env_ptr, entry.env_dtor);
        for (char *s : entry.retained_str_args) mockReleaseStr(s);
        for (auto *snap : entry.retained_list_snapshots) freeMockListSnapshot(snap);
        for (auto *snap : entry.retained_map_snapshots) freeMockMapSnapshot(snap);
        for (auto *snap : entry.retained_record_snapshots) freeMockRecordSnapshot(snap);
        for (auto *snap : entry.retained_tuple_snapshots) freeMockTupleSnapshot(snap);
        for (auto *snap : entry.retained_fn_snapshots) freeMockFnSnapshot(snap);
    }
    g_mock_registry.clear();
}

// ===== Property-based test support =====

static std::mt19937_64 &getRng() {
    static std::mt19937_64 rng(std::random_device{}());
    return rng;
}

void __ry_test_prop_init_rng() {
    getRng(); // ensure initialization
}

int64_t __ry_test_rand_int() {
    std::uniform_int_distribution<int64_t> dist(-1000, 1000);
    return dist(getRng());
}

double __ry_test_rand_float() {
    std::uniform_real_distribution<double> dist(-1000.0, 1000.0);
    return dist(getRng());
}

int64_t __ry_test_rand_bool() {
    std::uniform_int_distribution<int64_t> dist(0, 1);
    return dist(getRng());
}

const char *__ry_test_rand_str() {
    std::uniform_int_distribution<int> lenDist(0, 20);
    std::uniform_int_distribution<int> charDist(32, 126); // printable ASCII
    auto &rng = getRng();
    int len = lenDist(rng);
    char *buf = makeStringUninit(static_cast<size_t>(len));
    for (int i = 0; i < len; ++i)
        buf[i] = static_cast<char>(charDist(rng));
    return buf;
}

int64_t __ry_test_it_is_failed() {
    return g_current_it_failed ? 1 : 0;
}

const char *__ry_test_indent(int extra) {
    static char buf[256];
    int len = g_describe_depth * 2 + extra;
    if (len < 0) len = 0;
    if (len >= static_cast<int>(sizeof(buf))) len = static_cast<int>(sizeof(buf)) - 1;
    std::memset(buf, ' ', static_cast<size_t>(len));
    buf[len] = '\0';
    return buf;
}

} // extern "C"

} // namespace ry
