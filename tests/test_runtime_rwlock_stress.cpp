#include "ry/runtime_thread.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_io.hpp"

#include <gtest/gtest.h>

#include <atomic>
#include <cstring>
#include <stdexcept>
#include <thread>
#include <vector>


// `__ry_thread_cleanup` is an internal helper (runs the ThreadHandle
// destructor after ensuring the worker has been joined) and is not
// exposed via runtime_thread.hpp. Forward-declare it for the error-path
// test so the handle does not leak under ASan.
namespace ry {
extern "C" void __ry_thread_cleanup(void *thread_ptr);
} // namespace ry


// Stress test for #871 — RWLock unlock dispatch under contention.
//
// Exercises the extern "C" runtime primitives in src/runtime_thread.cpp
// directly. The Ry-language harness cannot be used here because
// runTestSource (tests/test_codegen_common.hpp) bypasses the module
// loader, so `from thread import ...` fails inside specs it runs.
//
// Each worker takes many shared locks, then exclusive locks, on the
// same RWLock. The shared → exclusive transition within a single
// thread verifies that `__ry_rwlock_unlock` dispatches to `unlock()`
// (not `unlock_shared()`) after the thread-local count drops to zero;
// a mis-dispatch would corrupt std::shared_mutex state.
//
// Runs under the required `build-tsan/ry_tests` step and doubles as a
// shared_mutex contention stress for TSan.


namespace {

constexpr int kNumWorkers = 4;
constexpr int kReadsPerWorker = 40;
constexpr int kWritesPerWorker = 10;

} // namespace

TEST(RuntimeRWLockStress, UnlockDispatchUnderContention) {
    void *rw = ry::__ry_rwlock_new();
    ASSERT_NE(rw, nullptr);

    std::atomic<int64_t> reads{0};
    std::atomic<int64_t> writes{0};

    auto worker = [&]() {
        for (int i = 0; i < kReadsPerWorker; ++i) {
            ASSERT_EQ(ry::__ry_rwlock_read_lock(rw), 0);
            reads.fetch_add(1, std::memory_order_relaxed);
            ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);
        }
        for (int i = 0; i < kWritesPerWorker; ++i) {
            ASSERT_EQ(ry::__ry_rwlock_write_lock(rw), 0);
            writes.fetch_add(1, std::memory_order_relaxed);
            ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);
        }
    };

    std::vector<std::thread> workers;
    workers.reserve(kNumWorkers);
    for (int i = 0; i < kNumWorkers; ++i)
        workers.emplace_back(worker);
    for (auto &t : workers)
        t.join();

    EXPECT_EQ(reads.load(), static_cast<int64_t>(kNumWorkers * kReadsPerWorker));
    EXPECT_EQ(writes.load(), static_cast<int64_t>(kNumWorkers * kWritesPerWorker));

    ry::__ry_rwlock_free(rw);
}

// Nested-read stress: the same thread takes multiple overlapping read
// locks on the same RWLock and releases them in LIFO order. This
// specifically exercises the thread-local counter's increment/decrement
// logic — the entry's count should only be erased when it drops back
// to zero, matching the semantics of the previous map-based tracker.
TEST(RuntimeRWLockStress, NestedReadLockPerThread) {
    void *rw = ry::__ry_rwlock_new();
    ASSERT_NE(rw, nullptr);

    std::atomic<int> reached{0};

    auto worker = [&]() {
        for (int i = 0; i < 30; ++i) {
            ASSERT_EQ(ry::__ry_rwlock_read_lock(rw), 0);
            ASSERT_EQ(ry::__ry_rwlock_read_lock(rw), 0);
            ASSERT_EQ(ry::__ry_rwlock_read_lock(rw), 0);
            reached.fetch_add(1, std::memory_order_relaxed);
            ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);
            ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);
            ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);
        }
    };

    std::vector<std::thread> workers;
    workers.reserve(kNumWorkers);
    for (int i = 0; i < kNumWorkers; ++i)
        workers.emplace_back(worker);
    for (auto &t : workers)
        t.join();

    EXPECT_EQ(reached.load(), kNumWorkers * 30);

    // Workers unlock all three levels before returning, so by the time
    // join() completes their thread-local entries are gone. A following
    // write_lock on the main thread must succeed.
    ASSERT_EQ(ry::__ry_rwlock_write_lock(rw), 0);
    ASSERT_EQ(ry::__ry_rwlock_unlock(rw), 0);

    ry::__ry_rwlock_free(rw);
}

namespace {

// Entry function passed to `__ry_thread_spawn` that throws a
// std::runtime_error. Exercises the `has_error` / `error_msg` publish
// path (the worker's `catch(std::exception&)` arm).
void throwing_entry(void * /*env*/, void * /*result_buf*/) {
    throw std::runtime_error("worker-boom-#871");
}

// Entry function that throws a non-std::exception type. Exercises the
// worker's `catch(...)` arm, which publishes the fallback message.
void unknown_throw_entry(void * /*env*/, void * /*result_buf*/) {
    throw 42;
}

} // namespace

// Regression for the ThreadHandle::has_error release/acquire publish
// contract (#871). If the worker-side release or the join-side acquire
// were dropped, TSan would catch it on this path. If the error-field
// wiring regressed functionally, thread_join would return 0 instead of
// surfacing the error.
TEST(RuntimeThreadError, JoinSurfacesStdExceptionFromWorker) {
    void *t = ry::__ry_thread_spawn(&throwing_entry, nullptr, 0);
    ASSERT_NE(t, nullptr);

    EXPECT_EQ(ry::__ry_thread_join(t, nullptr), -1);

    const char *msg = ry::__ry_get_last_error();
    ASSERT_NE(msg, nullptr);
    EXPECT_STREQ(msg, "worker-boom-#871");

    ry::__ry_thread_cleanup(t);
    ry::arc_free(t);
}

TEST(RuntimeThreadError, JoinSurfacesUnknownExceptionFromWorker) {
    void *t = ry::__ry_thread_spawn(&unknown_throw_entry, nullptr, 0);
    ASSERT_NE(t, nullptr);

    EXPECT_EQ(ry::__ry_thread_join(t, nullptr), -1);

    const char *msg = ry::__ry_get_last_error();
    ASSERT_NE(msg, nullptr);
    EXPECT_STREQ(msg, "unknown thread error");

    ry::__ry_thread_cleanup(t);
    ry::arc_free(t);
}
