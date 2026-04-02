#include "test_codegen_common.hpp"
#include <cstdint>
#include <cstdlib>
#include <cstddef>
#include <cstring>

// ============================================================
//  ARC Header layout tests (pure C++ — validates the memory
//  layout contract that codegen_arc.cpp relies on)
// ============================================================

namespace {

struct ArcHeader {
    int64_t strong_count;
    int64_t weak_count;
};

static_assert(sizeof(ArcHeader) == 16, "ARC header must be 16 bytes");
static_assert(offsetof(ArcHeader, strong_count) == 0, "strong_count at offset 0");
static_assert(offsetof(ArcHeader, weak_count) == 8, "weak_count at offset 8");

// Simulate arc_alloc: malloc(16 + dataSize), init counts
void *arcAlloc(int64_t dataSize) {
    void *p = std::malloc(static_cast<size_t>(16 + dataSize));
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = 1;
    hdr->weak_count = 0;
    return p;
}

// Simulate arc_get_data_ptr: header + 16
void *arcGetDataPtr(void *header) {
    return static_cast<char *>(header) + 16;
}

static constexpr int64_t ARC_IMMORTAL_VAL = INT64_MAX;

// Simulate arc_retain (non-atomic, with immortal check)
void arcRetain(void *header) {
    auto *hdr = static_cast<ArcHeader *>(header);
    if (hdr->strong_count == ARC_IMMORTAL_VAL) return;
    hdr->strong_count += 1;
}

// Simulate arc_release (non-atomic, with immortal check); returns true if freed
bool arcRelease(void *header) {
    auto *hdr = static_cast<ArcHeader *>(header);
    if (hdr->strong_count == ARC_IMMORTAL_VAL) return false;
    hdr->strong_count -= 1;
    if (hdr->strong_count == 0) {
        std::free(header);
        return true;
    }
    return false;
}

} // anonymous namespace

// ===== Layout & allocation =====

TEST(ArcInfraTest, HeaderSize) {
    EXPECT_EQ(sizeof(ArcHeader), 16u);
}

TEST(ArcInfraTest, AllocInitializesRefCounts) {
    void *p = arcAlloc(32);
    auto *hdr = static_cast<ArcHeader *>(p);
    EXPECT_EQ(hdr->strong_count, 1);
    EXPECT_EQ(hdr->weak_count, 0);
    std::free(p);
}

TEST(ArcInfraTest, DataPointerOffset) {
    void *p = arcAlloc(64);
    auto *data = arcGetDataPtr(p);
    // Data should be at exactly header + 16
    EXPECT_EQ(static_cast<char *>(data) - static_cast<char *>(p), 16);
    // Write through data pointer and read back
    std::memset(data, 0xAB, 64);
    auto *bytes = static_cast<uint8_t *>(data);
    EXPECT_EQ(bytes[0], 0xAB);
    EXPECT_EQ(bytes[63], 0xAB);
    std::free(p);
}

TEST(ArcInfraTest, RetainIncrementsCount) {
    void *p = arcAlloc(8);
    auto *hdr = static_cast<ArcHeader *>(p);
    EXPECT_EQ(hdr->strong_count, 1);
    arcRetain(p);
    EXPECT_EQ(hdr->strong_count, 2);
    arcRetain(p);
    EXPECT_EQ(hdr->strong_count, 3);
    std::free(p);
}

TEST(ArcInfraTest, ReleaseDecrementsCount) {
    void *p = arcAlloc(8);
    auto *hdr = static_cast<ArcHeader *>(p);
    arcRetain(p); // strong = 2
    arcRetain(p); // strong = 3
    EXPECT_EQ(hdr->strong_count, 3);
    EXPECT_FALSE(arcRelease(p)); // strong = 2
    EXPECT_EQ(hdr->strong_count, 2);
    EXPECT_FALSE(arcRelease(p)); // strong = 1
    EXPECT_EQ(hdr->strong_count, 1);
    EXPECT_TRUE(arcRelease(p));  // strong = 0 → freed
    // p is now freed; do not access
}

TEST(ArcInfraTest, SingleOwnerRelease) {
    void *p = arcAlloc(16);
    // Single owner: alloc gives strong=1, release frees immediately
    EXPECT_TRUE(arcRelease(p));
}

// ===== Codegen structural tests =====
// Verify that the ArcHeader LLVM type is created correctly
// by compiling a trivial program and inspecting the CodeGen state.

TEST_F(CodeGenTest, ArcHeaderTypeExists) {
    auto tsm = compileSource("print(1)");
    tsm.withModuleDo([](Module &mod) {
        auto *arcHeaderTy = StructType::getTypeByName(mod.getContext(), "ArcHeader");
        ASSERT_NE(arcHeaderTy, nullptr);
        EXPECT_EQ(arcHeaderTy->getNumElements(), 2u);  // { i64, i64 }
    });
}

// ===== Data integrity through ARC header =====

TEST(ArcInfraTest, DataIntegrityThroughHeader) {
    // Allocate, write structured data after header, verify it survives retain/release
    void *p = arcAlloc(sizeof(int64_t) * 2);
    auto *data = static_cast<int64_t *>(arcGetDataPtr(p));
    data[0] = 42;
    data[1] = 99;

    arcRetain(p); // strong = 2

    // Data must be intact after retain
    EXPECT_EQ(data[0], 42);
    EXPECT_EQ(data[1], 99);

    EXPECT_FALSE(arcRelease(p)); // strong = 1, not freed
    // Data must still be intact
    EXPECT_EQ(data[0], 42);
    EXPECT_EQ(data[1], 99);

    EXPECT_TRUE(arcRelease(p)); // strong = 0 → freed
}

// ===== Immortal sentinel tests =====

TEST(ArcInfraTest, ImmortalSentinelSkipsRetain) {
    void *p = arcAlloc(16);
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = ARC_IMMORTAL_VAL;

    arcRetain(p);
    EXPECT_EQ(hdr->strong_count, ARC_IMMORTAL_VAL);

    std::free(p);
}

TEST(ArcInfraTest, ImmortalSentinelSkipsRelease) {
    void *p = arcAlloc(16);
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = ARC_IMMORTAL_VAL;

    EXPECT_FALSE(arcRelease(p));
    EXPECT_EQ(hdr->strong_count, ARC_IMMORTAL_VAL);

    std::free(p);
}

TEST(ArcInfraTest, GetHeaderFromData) {
    void *p = arcAlloc(32);
    void *data = arcGetDataPtr(p);
    // Getting header from data should be data - 16
    void *backToHeader = static_cast<char *>(data) - 16;
    EXPECT_EQ(backToHeader, p);
    std::free(p);
}

// ===== Integration: collection literals under ARC (functional) =====

TEST_F(CodeGenTest, ListLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("x = [1, 2, 3]\nprint(length(x))"), "3\n");
}

TEST_F(CodeGenTest, MapLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(length(m))"), "2\n");
}

TEST_F(CodeGenTest, SetLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("s: Set<int> = {1, 2, 3}\nprint(length(s))"), "3\n");
}

TEST_F(CodeGenTest, StringInterpolationWithArc) {
    EXPECT_EQ(runSource("x = 42\nprint(f\"value: {x}\")"),
              "value: 42\n");
}

TEST_F(CodeGenTest, CollectionVariableBinding) {
    EXPECT_EQ(runSource("x = [1, 2, 3]\ny = x\nprint(length(y))"), "3\n");
}

TEST_F(CodeGenTest, CollectionInFunction) {
    EXPECT_EQ(runSource("function get_length(lst: List<int>) -> int:\n  return length(lst)\n\nx = [10, 20, 30]\nprint(get_length(x))"), "3\n");
}

// ===== Additional ARC edge-case and regression tests =====

// Retain/release symmetry: after N retains there must be N+1 releases to free.
TEST(ArcInfraTest, RetainReleaseSymmetry) {
    void *p = arcAlloc(8);
    auto *hdr = static_cast<ArcHeader *>(p);

    constexpr int N = 100;
    for (int i = 0; i < N; ++i)
        arcRetain(p);

    EXPECT_EQ(hdr->strong_count, N + 1);

    for (int i = 0; i < N; ++i)
        EXPECT_FALSE(arcRelease(p));

    EXPECT_EQ(hdr->strong_count, 1);
    EXPECT_TRUE(arcRelease(p));  // freed
}

// Zero-byte data allocation: header must still be 16 bytes.
TEST(ArcInfraTest, ZeroSizeDataAlloc) {
    void *p = arcAlloc(0);
    auto *hdr = static_cast<ArcHeader *>(p);
    EXPECT_EQ(hdr->strong_count, 1);
    EXPECT_EQ(hdr->weak_count, 0);
    // Data pointer is header + 16; writing nothing should not crash.
    void *data = arcGetDataPtr(p);
    EXPECT_EQ(static_cast<char *>(data) - static_cast<char *>(p), 16);
    std::free(p);
}

// Weak count does not affect retain/release of strong count.
TEST(ArcInfraTest, WeakCountIndependentOfStrong) {
    void *p = arcAlloc(8);
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->weak_count = 5;

    arcRetain(p);  // strong: 1 -> 2
    EXPECT_EQ(hdr->strong_count, 2);
    EXPECT_EQ(hdr->weak_count, 5);  // unchanged

    EXPECT_FALSE(arcRelease(p));  // strong: 2 -> 1
    EXPECT_EQ(hdr->strong_count, 1);
    EXPECT_EQ(hdr->weak_count, 5);  // still unchanged

    // Manually free since we won't fully release.
    std::free(p);
}

// Verify that data written at maximum offset in the data area is accessible.
TEST(ArcInfraTest, DataAreaBoundary) {
    const size_t dataSize = 1024;
    void *p = arcAlloc(dataSize);
    auto *data = static_cast<uint8_t *>(arcGetDataPtr(p));
    // Write at the very last byte.
    data[dataSize - 1] = 0xBE;
    EXPECT_EQ(data[dataSize - 1], 0xBE);
    std::free(p);
}

// ARC codegen: append to a list and verify length grows.
TEST_F(CodeGenTest, ListAppendGrowsLength) {
    EXPECT_EQ(runSource(
        "xs: List<int> = []\n"
        "xs = appended(xs, 1)\n"
        "xs = appended(xs, 2)\n"
        "print(length(xs))\n"
    ), "2\n");
}

// ARC codegen: nested list access works.
TEST_F(CodeGenTest, NestedListAccess) {
    EXPECT_EQ(runSource(
        "outer = [[1, 2], [3, 4]]\n"
        "print(outer[0][1])\n"
    ), "2\n");
}

// ARC codegen: passing a map to a function and accessing a key.
TEST_F(CodeGenTest, MapPassedToFunction) {
    EXPECT_EQ(runSource(
        "function lookup(m: Map<str, int>, k: str) -> int:\n"
        "  return m[k]\n"
        "m = {\"x\": 99}\n"
        "print(lookup(m, \"x\"))\n"
    ), "99\n");
}