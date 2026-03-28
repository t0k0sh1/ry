#pragma once
#include <cstdint>

using __ry_thread_entry_fn = void (*)(void *);

extern "C" {

// Thread
void *__ry_thread_spawn(__ry_thread_entry_fn entry, void *env);
int64_t __ry_thread_join(void *thread);

// Lock (mutex)
void *__ry_lock_new();
int64_t __ry_lock_acquire(void *lock);
int64_t __ry_lock_release(void *lock);

// RWLock
void *__ry_rwlock_new();
int64_t __ry_rwlock_read_lock(void *rwlock);
int64_t __ry_rwlock_write_lock(void *rwlock);
int64_t __ry_rwlock_unlock(void *rwlock);

// Semaphore
void *__ry_semaphore_new(int64_t count);
int64_t __ry_semaphore_acquire(void *sem);
int64_t __ry_semaphore_release(void *sem);

// Barrier
void *__ry_barrier_new(int64_t count);
int64_t __ry_barrier_wait(void *barrier);

// AtomicInt
void *__ry_atomic_int_new(int64_t value);
int64_t __ry_atomic_int_load(void *a);
void __ry_atomic_int_store(void *a, int64_t value);
int64_t __ry_atomic_int_add(void *a, int64_t delta);
int64_t __ry_atomic_int_sub(void *a, int64_t delta);
int64_t __ry_atomic_int_cas(void *a, int64_t expected, int64_t desired);

// AtomicBool
void *__ry_atomic_bool_new(int64_t value);
int64_t __ry_atomic_bool_load(void *a);
void __ry_atomic_bool_store(void *a, int64_t value);

}
