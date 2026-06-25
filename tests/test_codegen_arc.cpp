// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.

#include "test_codegen_common.hpp"
#include "ry/ry_layout.hpp"
#include <cstdlib>
#include <cstddef>
#include <cstring>


using namespace ry;

extern "C" int64_t *__ry_arc_counter_address();
extern "C" int64_t  __ry_runtime_internal_arc_live_count();
extern "C" void    *__ry_arc_alloc_counted(int64_t total_size);
extern "C" void     __ry_arc_free_counted(void *header_ptr);
// ===== [internal] ARC Header layout (pure C++ — validates the memory layout contract that codegen_arc.cpp relies on) =====

namespace {

struct ArcHeader {
    int64_t strong_count;
    int64_t weak_count;
};

static_assert(sizeof(ArcHeader) == 16, "ARC header must be 16 bytes");
static_assert(offsetof(ArcHeader, strong_count) == 0, "strong_count at offset 0");
static_assert(offsetof(ArcHeader, weak_count) == 8, "weak_count at offset 8");

// Simulate arc_alloc: malloc(ARC_HEADER_SIZE + dataSize), init counts
void *arcAlloc(int64_t dataSize) {
    void *p = std::malloc(static_cast<size_t>(ARC_HEADER_SIZE) + static_cast<size_t>(dataSize));
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = 1;
    hdr->weak_count = 0;
    return p;
}

// Simulate arc_get_data_ptr: header + ARC_HEADER_SIZE
void *arcGetDataPtr(void *header) {
    return static_cast<char *>(header) + ARC_HEADER_SIZE;
}


// Simulate arc_retain (non-atomic, with immortal check)
void arcRetain(void *header) {
    auto *hdr = static_cast<ArcHeader *>(header);
    if (hdr->strong_count == ARC_IMMORTAL) return;
    hdr->strong_count += 1;
}

// Simulate arc_release (non-atomic, with immortal check); returns true if freed
bool arcRelease(void *header) {
    auto *hdr = static_cast<ArcHeader *>(header);
    if (hdr->strong_count == ARC_IMMORTAL) return false;
    hdr->strong_count -= 1;
    if (hdr->strong_count == 0) {
        std::free(header);
        return true;
    }
    return false;
}

} // anonymous namespace

// ===== [internal] Layout & allocation =====

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

// ===== [internal] Codegen structural tests =====
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

// ===== [internal] Data integrity through ARC header =====

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

// ===== [internal] Immortal sentinel tests =====

TEST(ArcInfraTest, ImmortalSentinelSkipsRetain) {
    void *p = arcAlloc(16);
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = ARC_IMMORTAL;

    arcRetain(p);
    EXPECT_EQ(hdr->strong_count, ARC_IMMORTAL);

    std::free(p);
}

TEST(ArcInfraTest, ImmortalSentinelSkipsRelease) {
    void *p = arcAlloc(16);
    auto *hdr = static_cast<ArcHeader *>(p);
    hdr->strong_count = ARC_IMMORTAL;

    EXPECT_FALSE(arcRelease(p));
    EXPECT_EQ(hdr->strong_count, ARC_IMMORTAL);

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

TEST(ArcInfraTest, ArcCounterAddressMatchesRuntimeCount) {
    int64_t *counter = __ry_arc_counter_address();
    ASSERT_NE(counter, nullptr);

    const int64_t before = __atomic_load_n(counter, __ATOMIC_RELAXED);
    EXPECT_EQ(before, __ry_runtime_internal_arc_live_count());

    void *p = __ry_arc_alloc_counted(32);
    ASSERT_NE(p, nullptr);
    EXPECT_EQ(__atomic_load_n(counter, __ATOMIC_RELAXED), before + 1);
    EXPECT_EQ(__ry_runtime_internal_arc_live_count(), before + 1);

    __ry_arc_free_counted(p);
    EXPECT_EQ(__atomic_load_n(counter, __ATOMIC_RELAXED), before);
    EXPECT_EQ(__ry_runtime_internal_arc_live_count(), before);
}

// ===== [contract] Integration: collection literals under ARC (functional) =====

TEST_F(CodeGenTest, ListLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("x = [1, 2, 3]\nprint(len(x))"), "3\n");
}

TEST_F(CodeGenTest, MapLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("m = {\"a\": 1, \"b\": 2}\nprint(len(m))"), "2\n");
}

TEST_F(CodeGenTest, SetLiteralProducesCorrectLength) {
    EXPECT_EQ(runSource("s: Set<int> = {1, 2, 3}\nprint(len(s))"), "3\n");
}

TEST_F(CodeGenTest, StringInterpolationWithArc) {
    EXPECT_EQ(runSource("x = 42\nprint(f\"value: {x}\")"),
              "value: 42\n");
}

TEST_F(CodeGenTest, CollectionVariableBinding) {
    EXPECT_EQ(runSource("x = [1, 2, 3]\ny = x\nprint(len(y))"), "3\n");
}

TEST_F(CodeGenTest, CollectionInFunction) {
    EXPECT_EQ(runSource("fn getLength(lst: List<int>) -> int:\n  return len(lst)\n\nx = [10, 20, 30]\nprint(getLength(x))"), "3\n");
}
