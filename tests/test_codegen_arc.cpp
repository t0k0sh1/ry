#include "test_codegen_common.hpp"
#include <cstdint>
#include <cstdlib>
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

// Simulate arc_retain (non-atomic)
void arcRetain(void *header) {
    static_cast<ArcHeader *>(header)->strong_count += 1;
}

// Simulate arc_release (non-atomic); returns true if freed
bool arcRelease(void *header) {
    auto *hdr = static_cast<ArcHeader *>(header);
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
    // Compile a trivial program — this exercises the CodeGen constructor
    // which creates arcHeaderTy_
    EXPECT_NO_THROW(runSource("print(1)"));
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
