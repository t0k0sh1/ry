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

struct ThreadHandle {
    std::thread thread;
    std::string error_msg;
    bool has_error = false;

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
    // Track per-thread lock mode so a single rwlock_unlock() can dispatch correctly.
    // Key: thread id, Value: count of shared locks held by that thread.
    std::mutex mode_mu;
    std::unordered_map<std::thread::id, int> shared_holders;
};

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

extern "C" void *__ry_thread_spawn(__ry_thread_entry_fn entry, void *env) {
    void *mem = arc_alloc(sizeof(ThreadHandle));
    if (!mem) {
        __ry_set_last_error("failed to allocate ThreadHandle");
        return nullptr;
    }
    auto *handle = new (mem) ThreadHandle;
    handle->thread = std::thread([entry, env, handle]() {
        std::unique_ptr<void, decltype(&std::free)> env_guard(env, &std::free);
        try {
            entry(env);
        } catch (const std::exception &ex) {
            handle->error_msg = ex.what();
            handle->has_error = true;
        } catch (...) {
            handle->error_msg = "unknown thread error";
            handle->has_error = true;
        }
    });
    return handle;
}

extern "C" int64_t __ry_thread_join(void *thread_ptr) {
    auto *handle = static_cast<ThreadHandle *>(thread_ptr);
    if (!handle) {
        __ry_set_last_error("thread_join: null thread handle");
        return -1;
    }
    try {
        if (handle->thread.joinable())
            handle->thread.join();
    } catch (const std::exception &ex) {
        __ry_set_last_error(ex.what());
        return -1;
    }
    if (handle->has_error) {
        __ry_set_last_error(handle->error_msg.c_str());
        return -1;
    }
    return 0;
}

// ARC cleanup: join if still joinable, destruct handle (no memory free)
extern "C" void __ry_thread_cleanup(void *thread_ptr) {
    auto *handle = static_cast<ThreadHandle *>(thread_ptr);
    if (!handle) return;
    try {
        if (handle->thread.joinable())
            handle->thread.join();
    } catch (...) {
        // Swallow exceptions to prevent them from crossing extern "C" boundary.
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
    try {
        rwlock->mu.lock_shared();
    } catch (const std::exception &ex) {
        __ry_set_last_error(ex.what());
        return -1;
    }
    // Track shared ownership after lock_shared succeeds — done outside try
    // so a map allocation failure doesn't leave the shared lock held.
    try {
        std::lock_guard<std::mutex> g(rwlock->mode_mu);
        rwlock->shared_holders[std::this_thread::get_id()]++;
    } catch (...) {
        rwlock->mu.unlock_shared();
        __ry_set_last_error("rwlock_read_lock: internal tracking failure");
        return -1;
    }
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
    {
        std::lock_guard<std::mutex> g(rwlock->mode_mu);
        auto it = rwlock->shared_holders.find(std::this_thread::get_id());
        if (it != rwlock->shared_holders.end() && it->second > 0) {
            it->second--;
            if (it->second == 0)
                rwlock->shared_holders.erase(it);
            rwlock->mu.unlock_shared();
            return 0;
        }
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
