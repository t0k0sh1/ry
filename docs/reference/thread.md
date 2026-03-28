[English](thread.md)

# Thread Reference

The `thread` package provides OS-level threading primitives for CPU-bound parallel workloads and fine-grained concurrency control.

## Types

| Type | Description |
|------|-------------|
| `Thread` | Opaque handle for an OS thread |
| `Lock` | Opaque handle for a mutex (mutual exclusion lock) |
| `RWLock` | Opaque handle for a read-write lock |
| `Semaphore` | Opaque handle for a counting semaphore |
| `Barrier` | Opaque handle for a thread barrier |
| `AtomicInt` | Opaque handle for an atomic 64-bit integer |
| `AtomicBool` | Opaque handle for an atomic boolean |

All types are opaque pointers. Use the corresponding `*_new()` functions to create instances.

## Import

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
```

## Thread

| Function | Signature | Description |
|----------|-----------|-------------|
| `thread_spawn` | `(body: fn() -> Unit) -> Thread` | Creates and starts a new OS thread executing `body`. Captured variables are copied by value. |
| `thread_join` | `(thread: Thread) -> Result<Unit, Error>` | Waits for the thread to finish. Returns `Err` if the thread raised an error. The thread handle is consumed after join. |

### Example

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add

counter = atomic_int_new(0)
t = thread_spawn(fn():
  atomic_int_add(counter, 1)
)
thread_join(t)
print(atomic_int_load(counter))  # 1
```

> **Note:** Captured variables are copied into the thread's environment. For primitive types (int, float, bool, str), this produces an independent copy. For opaque handle types (Lock, AtomicInt, etc.), the pointer is copied, sharing the underlying resource — which is the intended behavior for synchronization primitives.

## Lock (Mutex)

| Function | Signature | Description |
|----------|-----------|-------------|
| `lock_new` | `() -> Lock` | Creates a new mutex. |
| `lock_acquire` | `(lock: Lock) -> Result<Unit, Error>` | Acquires the lock. Blocks until available. |
| `lock_release` | `(lock: Lock) -> Result<Unit, Error>` | Releases the lock. |

## RWLock (Read-Write Lock)

| Function | Signature | Description |
|----------|-----------|-------------|
| `rwlock_new` | `() -> RWLock` | Creates a new read-write lock. |
| `rwlock_read_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Acquires a shared read lock. Multiple readers allowed. |
| `rwlock_write_lock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Acquires an exclusive write lock. Blocks until all readers and writers release. |
| `rwlock_unlock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Releases the lock (shared or exclusive). |

## Semaphore

| Function | Signature | Description |
|----------|-----------|-------------|
| `semaphore_new` | `(count: int) -> Semaphore` | Creates a semaphore with the given initial count. |
| `semaphore_acquire` | `(sem: Semaphore) -> Result<Unit, Error>` | Decrements the semaphore. Blocks if count is zero. |
| `semaphore_release` | `(sem: Semaphore) -> Result<Unit, Error>` | Increments the semaphore and wakes a waiting thread. |

## Barrier

| Function | Signature | Description |
|----------|-----------|-------------|
| `barrier_new` | `(count: int) -> Barrier` | Creates a barrier that synchronizes `count` threads. |
| `barrier_wait` | `(barrier: Barrier) -> Result<Unit, Error>` | Blocks until all `count` threads have called `barrier_wait`. |

## AtomicInt

| Function | Signature | Description |
|----------|-----------|-------------|
| `atomic_int_new` | `(value: int) -> AtomicInt` | Creates an atomic integer with the given initial value. |
| `atomic_int_load` | `(a: AtomicInt) -> int` | Atomically reads the value. |
| `atomic_int_store` | `(a: AtomicInt, value: int) -> Unit` | Atomically writes the value. |
| `atomic_int_add` | `(a: AtomicInt, delta: int) -> int` | Atomically adds `delta` and returns the **previous** value. |
| `atomic_int_sub` | `(a: AtomicInt, delta: int) -> int` | Atomically subtracts `delta` and returns the **previous** value. |
| `atomic_int_cas` | `(a: AtomicInt, expected: int, desired: int) -> bool` | Compare-and-swap: if current value equals `expected`, sets to `desired` and returns `true`; otherwise returns `false`. |

## AtomicBool

| Function | Signature | Description |
|----------|-----------|-------------|
| `atomic_bool_new` | `(value: bool) -> AtomicBool` | Creates an atomic boolean with the given initial value. |
| `atomic_bool_load` | `(a: AtomicBool) -> bool` | Atomically reads the value. |
| `atomic_bool_store` | `(a: AtomicBool, value: bool) -> Unit` | Atomically writes the value. |

## Comparison with async/await

| Feature | `async`/`await` | `thread` package |
|---------|----------------|-----------------|
| Execution model | Work-stealing thread pool | Dedicated OS threads |
| Best for | I/O-bound tasks, many lightweight tasks | CPU-bound tasks, fine-grained control |
| Synchronization | Automatic via `await` | Manual via Lock, Semaphore, etc. |
| Overhead | Low (task scheduling) | Higher (OS thread creation) |
