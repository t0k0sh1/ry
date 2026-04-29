#include "ry/runtime_thread.hpp"

#include <gtest/gtest.h>

#include <atomic>
#include <thread>
#include <vector>


// Lock high-contention stress tests (#872)
//
// Exercises __ry_lock_acquire / __ry_lock_release under heavy
// contention from many threads.  Complements the Ry-level test in
// tests/spec/concurrency_stress.test.ry (which uses 4 threads via
// threadSpawn) by calling the C runtime functions directly so TSan
// instruments the critical section itself.
//
// All tests are part of the required build-tsan/ry_tests gate.


namespace {

constexpr int kNumWorkers     = 8;
constexpr int kIterPerWorker  = 10'000;

} // namespace


// kNumWorkers threads each acquire the lock, increment a shared
// (non-atomic) counter, then release.  Mutual exclusion guarantees
// the final counter equals kNumWorkers * kIterPerWorker.
//
// A non-atomic int64_t counter is intentional: under TSan, any
// unsynchronised increment would be reported as a data race, which
// proves that the lock actually serialises accesses.
TEST(RuntimeLockStress, MutualExclusionUnderHighContention) {
    void *lock = ry::__ry_lock_new();
    ASSERT_NE(lock, nullptr);

    int64_t counter = 0;  // protected by `lock`

    auto worker = [&]() {
        for (int i = 0; i < kIterPerWorker; ++i) {
            ASSERT_EQ(ry::__ry_lock_acquire(lock), int64_t{0});
            ++counter;
            ASSERT_EQ(ry::__ry_lock_release(lock), int64_t{0});
        }
    };

    std::vector<std::thread> workers;
    workers.reserve(kNumWorkers);
    for (int i = 0; i < kNumWorkers; ++i)
        workers.emplace_back(worker);
    for (auto &t : workers)
        t.join();

    EXPECT_EQ(counter, static_cast<int64_t>(kNumWorkers) * kIterPerWorker);

    ry::__ry_lock_free(lock);
}


// Single-threaded baseline: one thread acquires and releases the same
// lock many times in a row (no contention).  Verifies that sequential
// acquire/release cycles leave the lock in a consistent state (this is
// not a reentrancy test — the lock is always fully released before the
// next acquire).
TEST(RuntimeLockStress, SequentialReacquire) {
    void *lock = ry::__ry_lock_new();
    ASSERT_NE(lock, nullptr);

    constexpr int kRounds = 10'000;
    for (int i = 0; i < kRounds; ++i) {
        ASSERT_EQ(ry::__ry_lock_acquire(lock), int64_t{0});
        ASSERT_EQ(ry::__ry_lock_release(lock), int64_t{0});
    }

    ry::__ry_lock_free(lock);
}


// Multiple locks, one per worker — no contention between threads
// but each thread calls acquire/release many times.  Verifies that
// independent Lock instances don't share internal state.
TEST(RuntimeLockStress, IndependentLocksNoInterference) {
    std::vector<void *> locks(kNumWorkers);
    for (auto &l : locks) {
        l = ry::__ry_lock_new();
        ASSERT_NE(l, nullptr);
    }

    std::atomic<int64_t> total{0};

    std::vector<std::thread> workers;
    workers.reserve(kNumWorkers);
    for (int i = 0; i < kNumWorkers; ++i) {
        workers.emplace_back([&locks, &total, i]() {
            void *myLock = locks[static_cast<size_t>(i)];
            int64_t local = 0;
            for (int j = 0; j < kIterPerWorker; ++j) {
                ASSERT_EQ(ry::__ry_lock_acquire(myLock), int64_t{0});
                ++local;
                ASSERT_EQ(ry::__ry_lock_release(myLock), int64_t{0});
            }
            total.fetch_add(local, std::memory_order_relaxed);
        });
    }
    for (auto &t : workers)
        t.join();

    EXPECT_EQ(total.load(), static_cast<int64_t>(kNumWorkers) * kIterPerWorker);

    for (auto *l : locks)
        ry::__ry_lock_free(l);
}
