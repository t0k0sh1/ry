#include "ry/runtime_alloc.hpp"
#include "ry/test_runtime.hpp"
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <random>
#include <string>
#include <unistd.h>
#include <unordered_map>


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

// Mock registry
struct MockEntry { void *fn_ptr; int64_t call_count; };
static std::unordered_map<std::string, MockEntry> g_mock_registry;

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
    int result = g_failed;
    // Reset for potential multiple invocations
    g_passed = 0;
    g_failed = 0;
    return result;
}

// ===== Mock support =====

void __ry_mock_set(const char *name, void *fn_ptr) {
    g_mock_registry[name] = {fn_ptr, 0};
}

void *__ry_mock_get(const char *name) {
    auto it = g_mock_registry.find(name);
    if (it != g_mock_registry.end()) return it->second.fn_ptr;
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

void __ry_mock_clear_all() {
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
    char *buf = static_cast<char*>(checked_malloc(static_cast<size_t>(len) + 1));
    for (int i = 0; i < len; ++i)
        buf[i] = static_cast<char>(charDist(rng));
    buf[len] = '\0';
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
