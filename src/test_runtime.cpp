#include "ry/ry_layout.hpp"
#include "ry/runtime_alloc.hpp"
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

// Per-call argument record for argument-level mock verification (#1677).
// kind: 1=int, 2=float, 3=bool, 4=str, 5=opaque (unsupported in verifyCalledWith).
// value: raw 64-bit payload — i64 bits for int/bool, bitcast(f64) for float,
// pointer-as-i64 for str (the handle is retained at record time).
struct MockArg {
    int64_t kind;
    int64_t value;
};
struct MockCallRecord {
    std::vector<MockArg> args;
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
    entry = MockEntry{};
    entry.fn_ptr = fn_ptr;
}

void __ry_mock_set_closure(const char *name, void *thunk_ptr,
                            void *env_ptr, void (*env_dtor)(void *)) {
    auto &entry = g_mock_registry[name];
    mockReleaseClosureEnv(entry.env_ptr, entry.env_dtor);
    for (char *s : entry.retained_str_args) mockReleaseStr(s);
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
    return recorded.value == expectedValue;
}

int64_t __ry_mock_count_matching_calls(const char *name, int64_t numArgs,
                                        const int64_t *kinds,
                                        const int64_t *values) {
    auto it = g_mock_registry.find(name);
    if (it == g_mock_registry.end()) return 0;
    int64_t matched = 0;
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
    return matched;
}

void __ry_mock_clear_all() {
    for (auto &kv : g_mock_registry) {
        auto &entry = kv.second;
        mockReleaseClosureEnv(entry.env_ptr, entry.env_dtor);
        for (char *s : entry.retained_str_args) mockReleaseStr(s);
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
