#include "ry/test_runtime.hpp"
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <random>
#include <string>
#include <unordered_map>

static int g_passed = 0;
static int g_failed = 0;
static std::string g_current_describe;
static std::string g_current_it;
static bool g_current_it_failed = false;

// Mock registry
struct MockEntry { void *fn_ptr; int64_t call_count; };
static std::unordered_map<std::string, MockEntry> g_mock_registry;

extern "C" {

void __ry_test_describe_begin(const char *name) {
    g_current_describe = name;
    std::printf("%s\n", name);
}

void __ry_test_describe_end() {
    g_current_describe.clear();
}

void __ry_test_it_begin(const char *name) {
    g_current_it = name;
    g_current_it_failed = false;
}

void __ry_test_it_end() {
    __ry_mock_clear_all();
    if (g_current_it_failed) {
        std::printf("  \033[31m- %s\033[0m\n", g_current_it.c_str());
        ++g_failed;
    } else {
        std::printf("  \033[32m+ %s\033[0m\n", g_current_it.c_str());
        ++g_passed;
    }
    g_current_it.clear();
}

void __ry_test_expect_fail(int line, const char *actual, const char *expected) {
    g_current_it_failed = true;
    std::printf("    \033[31mline %d: expected %s, got %s\033[0m\n", line, expected, actual);
}

void __ry_test_fail(int line, const char *msg) {
    g_current_it_failed = true;
    if (msg && msg[0] != '\0') {
        std::printf("    \033[31mline %d: %s\033[0m\n", line, msg);
    } else {
        std::printf("    \033[31mline %d: test failed\033[0m\n", line);
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
    char *buf = static_cast<char*>(std::malloc(len + 1));
    for (int i = 0; i < len; ++i)
        buf[i] = static_cast<char>(charDist(rng));
    buf[len] = '\0';
    return buf;
}

int64_t __ry_test_it_is_failed() {
    return g_current_it_failed ? 1 : 0;
}

} // extern "C"
