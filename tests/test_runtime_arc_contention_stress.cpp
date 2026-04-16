#include "ry/ry_layout.hpp"
#include "ry/runtime_alloc.hpp"

#include <gtest/gtest.h>

#include <cstdint>
#include <cstdlib>
#include <thread>
#include <vector>


// ARC contention stress tests (#872)
//
// These tests validate the atomic retain/release operations that
// emitArcRetain(hdr, /*atomic=*/true) and emitArcRelease emit in
// the @parallel for thunk (see src/codegen_arc.cpp and #630).
//
// The LLVM IR uses `atomicrmw add/sub seqcst` on the int64_t
// strong_count field at the start of the ARC header block.  This
// file replicates those ops using __atomic_fetch_add / _sub with
// __ATOMIC_SEQ_CST so TSan can verify there are no races.
//
// Why C++ instead of a Ry spec test?
// ConcurrencySpecSuite already exercises the JIT-compiled atomic
// ARC path.  These tests complement it by exercising the raw atomic
// layer directly and are part of the required TSan gate
// (build-tsan/ry_tests).


namespace {

// Simulated ARC header layout (matches ry_layout.hpp).
struct ArcHeader {
    int64_t strong_count;
    int64_t weak_count;
};

static_assert(sizeof(ArcHeader) == ry::ARC_HEADER_SIZE,
              "ArcHeader size must match ARC_HEADER_SIZE");

// Allocate a standalone ARC header on the heap.
ArcHeader *allocHeader(int64_t initialStrong) {
    auto *h = static_cast<ArcHeader *>(
        ry::checked_malloc(sizeof(ArcHeader)));
    h->strong_count = initialStrong;
    h->weak_count = 0;
    return h;
}

// Atomic retain: atomicrmw add seqcst — mirrors emitArcRetain(hdr, true).
inline void atomicRetain(ArcHeader *h) {
    __atomic_fetch_add(&h->strong_count, int64_t{1}, __ATOMIC_SEQ_CST);
}

// Atomic release: atomicrmw sub seqcst — mirrors emitArcRelease(hdr, true).
// Returns the OLD strong_count (object is dead when old == 1).
inline int64_t atomicRelease(ArcHeader *h) {
    return __atomic_fetch_sub(&h->strong_count, int64_t{1}, __ATOMIC_SEQ_CST);
}

constexpr int kNumWorkers   = 16;
constexpr int kIterPerWorker = 10'000;

} // namespace


// Many threads concurrently retain and then release the same ARC
// header.  Final strong_count must equal the initial value.
//
// This is the C++ analogue of the @parallel for ARC stress test in
// concurrency.test.ry — exercising the same atomicrmw pattern that
// the JIT emits, but directly from C++ so TSan instruments it
// without JIT interference.
TEST(RuntimeArcContentionStress, ConcurrentRetainReleasePairs) {
    ArcHeader *hdr = allocHeader(1);

    auto worker = [&]() {
        for (int i = 0; i < kIterPerWorker; ++i) {
            atomicRetain(hdr);
            atomicRelease(hdr);
        }
    };

    std::vector<std::thread> workers;
    workers.reserve(kNumWorkers);
    for (int i = 0; i < kNumWorkers; ++i)
        workers.emplace_back(worker);
    for (auto &t : workers)
        t.join();

    int64_t final_count;
    __atomic_load(&hdr->strong_count, &final_count, __ATOMIC_SEQ_CST);
    EXPECT_EQ(final_count, int64_t{1});

    std::free(hdr);
}


// Many threads concurrently retain the same ARC header (simulating
// a @parallel for begin — each worker retains before using the
// value), then all release (simulating scope exit).
// Simulates @parallel for begin (all workers retain) then end (all release).
// The two-phase split lets us snapshot the intermediate count.
TEST(RuntimeArcContentionStress, ConcurrentBatchRetainThenBatchRelease) {
    ArcHeader *hdr = allocHeader(1);

    {
        std::vector<std::thread> workers;
        workers.reserve(kNumWorkers);
        for (int i = 0; i < kNumWorkers; ++i)
            workers.emplace_back([&]() { atomicRetain(hdr); });
        for (auto &t : workers)
            t.join();
    }

    int64_t afterRetain;
    __atomic_load(&hdr->strong_count, &afterRetain, __ATOMIC_SEQ_CST);
    EXPECT_EQ(afterRetain, int64_t{1 + kNumWorkers});

    {
        std::vector<std::thread> workers;
        workers.reserve(kNumWorkers);
        for (int i = 0; i < kNumWorkers; ++i)
            workers.emplace_back([&]() { atomicRelease(hdr); });
        for (auto &t : workers)
            t.join();
    }

    int64_t afterRelease;
    __atomic_load(&hdr->strong_count, &afterRelease, __ATOMIC_SEQ_CST);
    EXPECT_EQ(afterRelease, int64_t{1});

    std::free(hdr);
}


// Immortal sentinel layout check.
//
// The JIT codegen guards atomicrmw retain/release with
// `strong_count == ARC_IMMORTAL` so immortal objects are never mutated.
// This test verifies the sentinel value is INT64_MAX, which is what
// makes the arithmetic-overflow approach detectable under TSan if the
// guard were ever skipped: a seq-cst add of 1 to INT64_MAX wraps to
// INT64_MIN — observable as a distinct, wrong value.
//
// The concurrent behaviour of the guard is exercised in practice by
// the JIT-compiled paths tested in ConcurrencySpecSuite; here we
// confirm the constant itself is correct.
TEST(RuntimeArcContentionStress, ImmortalSentinelNotCorrupted) {
    static_assert(ry::ARC_IMMORTAL == INT64_MAX,
                  "ARC_IMMORTAL must be INT64_MAX for wrap-detection to work");
    EXPECT_EQ(ry::ARC_IMMORTAL, INT64_MAX);
}
