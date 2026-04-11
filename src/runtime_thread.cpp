#include "ry/runtime_thread.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_io.hpp" // __ry_set_last_error

#include <atomic>
#include <condition_variable>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_map>


namespace ry {

namespace {

// ===== Handle types =====

// Maximum worker return value size in bytes. MVP fits int64/double/bool in
// 8 bytes; widening (e.g. for sum types) is tracked in #878.
static constexpr int64_t kThreadResultSlotBytes = 8;

// Tri-state join lifecycle. Uses a single atomic so both __ry_thread_join
// (explicit) and __ry_thread_cleanup (ARC dtor) can coordinate without a
// mutex. Transitions:
//
//   kJoinNotStarted --CAS-> kJoinInProgress   (thread_join claims ownership)
//   kJoinInProgress --store-> kJoinCompleted  (thread_join finishes its work)
//   kJoinNotStarted --CAS-> kJoinCompleted    (cleanup claims + completes)
//
// The kJoinInProgress phase exists specifically so cleanup can wait for an
// in-flight explicit join to finish before destroying the ThreadHandle.
// Under Ry's ARC invariant the caller of thread_join always holds a live
// reference, so cleanup should never observe kJoinInProgress in practice —
// but the wait is a cheap defense against future compiler/ARC changes.
enum JoinState : uint8_t {
    kJoinNotStarted = 0,
    kJoinInProgress = 1,
    kJoinCompleted = 2,
};

struct ThreadHandle {
    std::thread thread;
    // Worker error fields (#871 — thread error sync hardening).
    //
    // Publish/subscribe contract: the worker writes `error_msg` FIRST, then
    // stores `has_error = true` with `memory_order_release`. Any reader
    // must `acquire`-load `has_error` before reading `error_msg`; if that
    // load returns `true`, the release/acquire pair guarantees the string
    // write is visible.
    //
    // Reading `error_msg` without observing `has_error=true` is undefined
    // (the string may still be in its default-constructed state or
    // partially written). `__ry_thread_join` follows this contract; any
    // future pre-join polling path must follow it too.
    std::string error_msg;
    std::atomic<bool> has_error{false};
    std::atomic<uint8_t> join_state{kJoinNotStarted};
    int64_t result_size = 0;       // 0 = Unit, kThreadResultSlotBytes otherwise
    alignas(kThreadResultSlotBytes) unsigned char result[kThreadResultSlotBytes] = {0};

    ~ThreadHandle() {
        if (thread.joinable())
            thread.detach();
    }
};

struct LockHandle {
    std::mutex mu;
};

struct RWLockHandle {
    std::shared_mutex mu;
};

// Per-thread shared-lock counts keyed by RWLockHandle. `__ry_rwlock_unlock`
// consults this table (thread-private, no mutex required) to dispatch
// between `unlock_shared()` and `unlock()`. Using a namespace-scope
// `thread_local` eliminates the two-step lock/update window #871 fixed.
// See KNOWLEDGE.md (Runtime / Memory, "RWLock dispatch state") for the
// deadlock analysis of the rejected shared-mutex alternative.
//
// Caveat: an unbalanced `rwlock_read_lock` leaves a stale entry that
// could be misinterpreted if the RWLock address is later reused — a
// program bug the previous implementation also did not defend against.
static thread_local std::unordered_map<RWLockHandle *, int>
    tls_rwlock_read_counts;

struct SemaphoreHandle {
    std::mutex mu;
    std::condition_variable cv;
    int64_t count;

    explicit SemaphoreHandle(int64_t n) : count(n) {}
};

struct BarrierHandle {
    std::mutex mu;
    std::condition_variable cv;
    int64_t target;
    int64_t arrived = 0;
    int64_t generation = 0;

    explicit BarrierHandle(int64_t n) : target(n) {}
};

} // namespace

// ===== Thread =====

extern "C" void *__ry_thread_spawn(__ry_thread_entry_fn entry, void *env, int64_t result_size) {
    // Defensive bound: reject out-of-range result_size before anything can
    // memcpy from the fixed inline slot. Current codegen only passes 0 or 8.
    if (result_size < 0 || result_size > kThreadResultSlotBytes) {
        __ry_set_last_error("thread_spawn: invalid result size");
        return nullptr;
    }
    void *mem = arc_alloc(sizeof(ThreadHandle));
    if (!mem) {
        __ry_set_last_error("failed to allocate ThreadHandle");
        return nullptr;
    }
    auto *handle = new (mem) ThreadHandle;
    handle->result_size = result_size;
    handle->thread = std::thread([entry, env, handle]() {
        std::unique_ptr<void, decltype(&std::free)> env_guard(env, &std::free);
        void *result_buf = handle->result_size > 0 ? handle->result : nullptr;
        try {
            entry(env, result_buf);
        } catch (const std::exception &ex) {
            handle->error_msg = ex.what();
            handle->has_error.store(true, std::memory_order_release);
        } catch (...) {
            handle->error_msg = "unknown thread error";
            handle->has_error.store(true, std::memory_order_release);
        }
    });
    return handle;
}

extern "C" int64_t __ry_thread_join(void *thread_ptr, void *out_buf) {
    auto *handle = static_cast<ThreadHandle *>(thread_ptr);
    if (!handle) {
        __ry_set_last_error("thread_join: null thread handle");
        return -1;
    }
    // Claim the in-progress slot. Fails if another caller already claimed
    // or completed the join, which signals double-join.
    uint8_t expected = kJoinNotStarted;
    if (!handle->join_state.compare_exchange_strong(
            expected, kJoinInProgress, std::memory_order_acq_rel)) {
        __ry_set_last_error("thread already joined");
        return -1;
    }
    // RAII guard: publish kJoinCompleted on every exit path so a racing
    // cleanup waiting in its spin loop can make progress and destroy the
    // handle safely.
    struct FinalizeJoin {
        std::atomic<uint8_t> *state;
        ~FinalizeJoin() {
            state->store(kJoinCompleted, std::memory_order_release);
        }
    } finalizer{&handle->join_state};

    try {
        if (handle->thread.joinable())
            handle->thread.join();
    } catch (const std::exception &ex) {
        __ry_set_last_error(ex.what());
        return -1;
    }
    // thread.join() already establishes happens-before on the worker's
    // writes, so a plain load would be sound here. The explicit acquire
    // documents the publish/subscribe contract on the ThreadHandle and
    // stays correct for any future pre-join polling path (#871).
    if (handle->has_error.load(std::memory_order_acquire)) {
        __ry_set_last_error(handle->error_msg.c_str());
        return -1;
    }
    // Defensive bound: result_size is normally set by __ry_thread_spawn's
    // checked path, but re-validate here so any ABI mismatch or corrupted
    // handle cannot drive a wild memcpy against an 8-byte inline slot.
    if (out_buf &&
        handle->result_size > 0 &&
        handle->result_size <= kThreadResultSlotBytes)
        std::memcpy(out_buf, handle->result, static_cast<size_t>(handle->result_size));
    return 0;
}

// ARC cleanup: auto-join if no explicit thread_join() ran first, and wait
// for any in-flight thread_join() to complete before destroying the handle.
// The wait exists as a defensive guard: under Ry's ARC invariant the caller
// of thread_join always holds a live reference to the handle throughout the
// call, so cleanup should never observe kJoinInProgress in practice.
extern "C" void __ry_thread_cleanup(void *thread_ptr) {
    auto *handle = static_cast<ThreadHandle *>(thread_ptr);
    if (!handle) return;
    uint8_t expected = kJoinNotStarted;
    if (handle->join_state.compare_exchange_strong(
            expected, kJoinCompleted, std::memory_order_acq_rel)) {
        // Nobody else is joining; do it ourselves before destroying.
        try {
            if (handle->thread.joinable())
                handle->thread.join();
        } catch (...) {
            // Swallow exceptions to prevent them from crossing extern "C" boundary.
        }
    } else {
        // Either an in-flight explicit join (kJoinInProgress) or a prior
        // completed one (kJoinCompleted). In the in-progress case, wait
        // for the joiner to publish kJoinCompleted before destroying.
        while (handle->join_state.load(std::memory_order_acquire) ==
               kJoinInProgress) {
            std::this_thread::yield();
        }
    }
    handle->~ThreadHandle();
}

// ===== Lock =====

extern "C" void *__ry_lock_new() {
    void *mem = arc_alloc(sizeof(LockHandle));
    if (!mem) return nullptr;
    return new (mem) LockHandle;
}

extern "C" int64_t __ry_lock_acquire(void *lock_ptr) {
    if (!lock_ptr) { __ry_set_last_error("lock_acquire: null handle"); return -1; }
    auto *lock = static_cast<LockHandle *>(lock_ptr);
    try {
        lock->mu.lock();
    } catch (const std::exception &ex) {
        __ry_set_last_error(ex.what());
        return -1;
    }
    return 0;
}

extern "C" int64_t __ry_lock_release(void *lock_ptr) {
    if (!lock_ptr) { __ry_set_last_error("lock_release: null handle"); return -1; }
    auto *lock = static_cast<LockHandle *>(lock_ptr);
    lock->mu.unlock();
    return 0;
}

extern "C" void __ry_lock_cleanup(void *lock_ptr) {
    if (!lock_ptr) return;
    static_cast<LockHandle *>(lock_ptr)->~LockHandle();
}

extern "C" void __ry_lock_free(void *lock_ptr) {
    if (!lock_ptr) return;
    __ry_lock_cleanup(lock_ptr);
    arc_free(lock_ptr);
}

// ===== RWLock =====

extern "C" void *__ry_rwlock_new() {
    void *mem = arc_alloc(sizeof(RWLockHandle));
    if (!mem) return nullptr;
    return new (mem) RWLockHandle;
}

extern "C" int64_t __ry_rwlock_read_lock(void *rwlock_ptr) {
    if (!rwlock_ptr) { __ry_set_last_error("rwlock_read_lock: null handle"); return -1; }
    auto *rwlock = static_cast<RWLockHandle *>(rwlock_ptr);

    // Reserve the tracking slot BEFORE acquiring the shared lock so there
    // is no window where the lock is held but the tracker lacks an entry.
    // Nested read locks skip the allocation entirely via `find`.
    auto it = tls_rwlock_read_counts.find(rwlock);
    if (it == tls_rwlock_read_counts.end()) {
        try {
            it = tls_rwlock_read_counts.emplace(rwlock, 0).first;
        } catch (const std::exception &ex) {
            __ry_set_last_error(ex.what());
            return -1;
        }
    }

    try {
        rwlock->mu.lock_shared();
    } catch (const std::exception &ex) {
        // A zero count uniquely identifies the entry we just created: any
        // pre-existing nested lock would have second > 0. Erase only in
        // that case so nested rollback doesn't drop a valid outer count.
        if (it->second == 0) tls_rwlock_read_counts.erase(it);
        __ry_set_last_error(ex.what());
        return -1;
    }

    it->second++;
    return 0;
}

extern "C" int64_t __ry_rwlock_write_lock(void *rwlock_ptr) {
    if (!rwlock_ptr) { __ry_set_last_error("rwlock_write_lock: null handle"); return -1; }
    auto *rwlock = static_cast<RWLockHandle *>(rwlock_ptr);
    try {
        rwlock->mu.lock();
    } catch (const std::exception &ex) {
        __ry_set_last_error(ex.what());
        return -1;
    }
    return 0;
}

extern "C" int64_t __ry_rwlock_unlock(void *rwlock_ptr) {
    if (!rwlock_ptr) { __ry_set_last_error("rwlock_unlock: null handle"); return -1; }
    auto *rwlock = static_cast<RWLockHandle *>(rwlock_ptr);

    // Dispatch via thread-local ownership (#871). No shared mutex is
    // required because each thread only reads/writes its own entry.
    auto it = tls_rwlock_read_counts.find(rwlock);
    if (it != tls_rwlock_read_counts.end() && it->second > 0) {
        it->second--;
        if (it->second == 0)
            tls_rwlock_read_counts.erase(it);
        rwlock->mu.unlock_shared();
        return 0;
    }
    rwlock->mu.unlock();
    return 0;
}

extern "C" void __ry_rwlock_cleanup(void *rwlock_ptr) {
    if (!rwlock_ptr) return;
    static_cast<RWLockHandle *>(rwlock_ptr)->~RWLockHandle();
}

extern "C" void __ry_rwlock_free(void *rwlock_ptr) {
    if (!rwlock_ptr) return;
    __ry_rwlock_cleanup(rwlock_ptr);
    arc_free(rwlock_ptr);
}

// ===== Semaphore (C++17 compatible: mutex + cv) =====

extern "C" void *__ry_semaphore_new(int64_t count) {
    if (count < 0) {
        __ry_set_last_error("semaphore_new: count must be non-negative");
        return nullptr;
    }
    void *mem = arc_alloc(sizeof(SemaphoreHandle));
    if (!mem) return nullptr;
    return new (mem) SemaphoreHandle(count);
}

extern "C" int64_t __ry_semaphore_acquire(void *sem_ptr) {
    if (!sem_ptr) { __ry_set_last_error("semaphore_acquire: null handle"); return -1; }
    auto *sem = static_cast<SemaphoreHandle *>(sem_ptr);
    std::unique_lock<std::mutex> lock(sem->mu);
    sem->cv.wait(lock, [sem]() { return sem->count > 0; });
    --sem->count;
    return 0;
}

extern "C" int64_t __ry_semaphore_release(void *sem_ptr) {
    if (!sem_ptr) { __ry_set_last_error("semaphore_release: null handle"); return -1; }
    auto *sem = static_cast<SemaphoreHandle *>(sem_ptr);
    {
        std::lock_guard<std::mutex> lock(sem->mu);
        ++sem->count;
    }
    sem->cv.notify_one();
    return 0;
}

extern "C" void __ry_semaphore_cleanup(void *sem_ptr) {
    if (!sem_ptr) return;
    static_cast<SemaphoreHandle *>(sem_ptr)->~SemaphoreHandle();
}

extern "C" void __ry_semaphore_free(void *sem_ptr) {
    if (!sem_ptr) return;
    __ry_semaphore_cleanup(sem_ptr);
    arc_free(sem_ptr);
}

// ===== Barrier (C++17 compatible: mutex + cv) =====

extern "C" void *__ry_barrier_new(int64_t count) {
    if (count <= 0) {
        __ry_set_last_error("barrier_new: count must be positive");
        return nullptr;
    }
    void *mem = arc_alloc(sizeof(BarrierHandle));
    if (!mem) return nullptr;
    return new (mem) BarrierHandle(count);
}

extern "C" int64_t __ry_barrier_wait(void *barrier_ptr) {
    if (!barrier_ptr) { __ry_set_last_error("barrier_wait: null handle"); return -1; }
    auto *barrier = static_cast<BarrierHandle *>(barrier_ptr);
    std::unique_lock<std::mutex> lock(barrier->mu);
    int64_t gen = barrier->generation;
    ++barrier->arrived;
    if (barrier->arrived == barrier->target) {
        barrier->arrived = 0;
        ++barrier->generation;
        lock.unlock();
        barrier->cv.notify_all();
    } else {
        barrier->cv.wait(lock, [barrier, gen]() {
            return barrier->generation != gen;
        });
    }
    return 0;
}

extern "C" void __ry_barrier_cleanup(void *barrier_ptr) {
    if (!barrier_ptr) return;
    static_cast<BarrierHandle *>(barrier_ptr)->~BarrierHandle();
}

extern "C" void __ry_barrier_free(void *barrier_ptr) {
    if (!barrier_ptr) return;
    __ry_barrier_cleanup(barrier_ptr);
    arc_free(barrier_ptr);
}

// ===== AtomicInt =====

extern "C" void *__ry_atomic_int_new(int64_t value) {
    void *mem = arc_alloc(sizeof(std::atomic<int64_t>));
    if (!mem) return nullptr;
    return new (mem) std::atomic<int64_t>(value);
}

extern "C" int64_t __ry_atomic_int_load(void *a) {
    if (!a) return 0;
    return static_cast<std::atomic<int64_t> *>(a)->load(std::memory_order_seq_cst);
}

extern "C" void __ry_atomic_int_store(void *a, int64_t value) {
    if (!a) return;
    static_cast<std::atomic<int64_t> *>(a)->store(value, std::memory_order_seq_cst);
}

extern "C" int64_t __ry_atomic_int_add(void *a, int64_t delta) {
    if (!a) return 0;
    return static_cast<std::atomic<int64_t> *>(a)->fetch_add(delta, std::memory_order_seq_cst);
}

extern "C" int64_t __ry_atomic_int_sub(void *a, int64_t delta) {
    if (!a) return 0;
    return static_cast<std::atomic<int64_t> *>(a)->fetch_sub(delta, std::memory_order_seq_cst);
}

extern "C" int64_t __ry_atomic_int_cas(void *a, int64_t expected, int64_t desired) {
    if (!a) return 0;
    auto *atom = static_cast<std::atomic<int64_t> *>(a);
    return atom->compare_exchange_strong(expected, desired, std::memory_order_seq_cst) ? 1 : 0;
}

extern "C" void __ry_atomic_int_free(void *a) {
    if (!a) return;
    arc_free(a);
}

extern "C" void __ry_atomic_int_cleanup(void *a) {
    // std::atomic has trivial destructor, nothing to clean up
    (void)a;
}

// ===== AtomicBool =====

extern "C" void *__ry_atomic_bool_new(int64_t value) {
    void *mem = arc_alloc(sizeof(std::atomic<bool>));
    if (!mem) return nullptr;
    return new (mem) std::atomic<bool>(value != 0);
}

extern "C" int64_t __ry_atomic_bool_load(void *a) {
    if (!a) return 0;
    return static_cast<std::atomic<bool> *>(a)->load(std::memory_order_seq_cst) ? 1 : 0;
}

extern "C" void __ry_atomic_bool_store(void *a, int64_t value) {
    if (!a) return;
    static_cast<std::atomic<bool> *>(a)->store(value != 0, std::memory_order_seq_cst);
}

extern "C" void __ry_atomic_bool_free(void *a) {
    if (!a) return;
    arc_free(a);
}

extern "C" void __ry_atomic_bool_cleanup(void *a) {
    (void)a;
}

} // namespace ry
