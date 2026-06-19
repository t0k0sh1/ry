# Thread Reference

The `thread` module provides OS-level threading primitives for CPU-bound parallel workloads and fine-grained concurrency control.

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

All types are opaque pointers managed by ARC (Automatic Reference Counting). Use the corresponding `*_new()` functions to create instances. Resources are automatically cleaned up when no longer referenced — explicit `*_free()` calls are optional but supported for immediate cleanup.

## Import

```ry
from thread import threadSpawn, threadJoin, lockNew, lockAcquire, lockRelease
```

## Thread

| Function | Signature | Description |
|----------|-----------|-------------|
| `threadSpawn` | `(body: fn() -> T) -> Thread` | Creates and starts a new OS thread executing `body`. Captured variables are copied by value. `T` may be `Unit`, `int`, `float`, or `bool`; see the limitations section below. |
| `threadJoin` | `(thread: Thread) -> Result<T, Error>` | Waits for the thread to finish and returns the worker's value in `Ok(value)`. Joining an already-joined `Thread` returns `Err("thread already joined")`. |

### Example — side-effect worker (`Unit`)

```ry
from thread import threadSpawn, threadJoin, atomicIntNew, atomicIntLoad, atomicIntAdd

counter = atomicIntNew(0)
t = threadSpawn(():
  atomicIntAdd(counter, 1)
)
threadJoin(t)
print(atomicIntLoad(counter))  # 1
```

### Example — value-returning worker (`int` / `float` / `bool`)

```ry
from thread import threadSpawn, threadJoin

t = threadSpawn(() => 42)
case threadJoin(t):
  Ok(v):
    print(v)       # 42
  Err(e):
    print(e.message)

# Captures work with value-returning workers too:
x = 10
t2 = threadSpawn(() => x * x)
case threadJoin(t2):
  Ok(v):
    print(v)       # 100
  Err(_):
    print("error")
```

> **Note:** Captured variables are copied into the thread's environment. For primitive types (int, float, bool, str), this produces an independent copy. For opaque handle types (Lock, AtomicInt, etc.), the pointer is copied, sharing the underlying resource — which is the intended behavior for synchronization primitives.

### Limitations (MVP)

- **Return type.** Workers may return `Unit`, `int`, `float`, or `bool`. ARC-managed types (`str`, `List`, `Map`, `Set`, records) and sum types (`Option`, `Result`, enums) are not yet supported; passing such a worker produces a codegen error pointing at a follow-up issue.
- **Lambda body shape.** Only expression-bodied lambdas (`() => <expr>`) may return a value. Block-bodied lambdas can still be used for `Unit` workers but cannot carry a non-`Unit` return value yet.
- **Variable-reference workers.** Passing a function value (named `fn`, a lambda bound to a variable, or a closure with captured outer variables) is supported for the same return types as the inline-lambda form (`Unit` / `int` / `float` / `bool`) and `threadJoin` surfaces the returned value. ARC-managed and sum types remain rejected at codegen time, matching the **Return type** limitation above.
- **Panics.** A runtime panic inside the worker (e.g. division by zero, array out-of-bounds, contract violation) terminates the entire process. It does not currently surface as `Err` from `threadJoin` — this requires a separate refactor of Ry's panic mechanism and is tracked as a follow-up issue.

## Lock (Mutex)

| Function | Signature | Description |
|----------|-----------|-------------|
| `lockNew` | `() -> Lock` | Creates a new mutex. |
| `lockAcquire` | `(lock: Lock) -> Result<Unit, Error>` | Acquires the lock. Blocks until available. |
| `lockRelease` | `(lock: Lock) -> Result<Unit, Error>` | Releases the lock. |
| `lockFree` | `(lock: Lock) -> Unit` | Immediately frees the lock. Optional — ARC handles cleanup automatically. |

## RWLock (Read-Write Lock)

| Function | Signature | Description |
|----------|-----------|-------------|
| `rwlockNew` | `() -> RWLock` | Creates a new read-write lock. |
| `rwlockReadLock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Acquires a shared read lock. Multiple readers allowed. |
| `rwlockWriteLock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Acquires an exclusive write lock. Blocks until all readers and writers release. |
| `rwlockUnlock` | `(rwlock: RWLock) -> Result<Unit, Error>` | Releases the lock (shared or exclusive). |
| `rwlockFree` | `(rwlock: RWLock) -> Unit` | Immediately frees the read-write lock. Optional — ARC handles cleanup automatically. |

## Semaphore

| Function | Signature | Description |
|----------|-----------|-------------|
| `semaphoreNew` | `(count: int) -> Result<Semaphore, Error>` | Creates a semaphore with the given initial count. Returns `Err` if count is negative. |
| `semaphoreAcquire` | `(sem: Semaphore) -> Result<Unit, Error>` | Decrements the semaphore. Blocks if count is zero. |
| `semaphoreRelease` | `(sem: Semaphore) -> Result<Unit, Error>` | Increments the semaphore and wakes a waiting thread. |
| `semaphoreFree` | `(sem: Semaphore) -> Unit` | Immediately frees the semaphore. Optional — ARC handles cleanup automatically. |

## Barrier

| Function | Signature | Description |
|----------|-----------|-------------|
| `barrierNew` | `(count: int) -> Result<Barrier, Error>` | Creates a barrier that synchronizes `count` threads. Returns `Err` if count is not positive. |
| `barrierWait` | `(barrier: Barrier) -> Result<Unit, Error>` | Blocks until all `count` threads have called `barrierWait`. |
| `barrierFree` | `(barrier: Barrier) -> Unit` | Immediately frees the barrier. Optional — ARC handles cleanup automatically. |

## AtomicInt

| Function | Signature | Description |
|----------|-----------|-------------|
| `atomicIntNew` | `(value: int) -> AtomicInt` | Creates an atomic integer with the given initial value. |
| `atomicIntLoad` | `(atomic: AtomicInt) -> int` | Atomically reads the value. |
| `atomicIntStore` | `(atomic: AtomicInt, value: int) -> Unit` | Atomically writes the value. |
| `atomicIntAdd` | `(atomic: AtomicInt, delta: int) -> int` | Atomically adds `delta` and returns the **previous** value. |
| `atomicIntSub` | `(atomic: AtomicInt, delta: int) -> int` | Atomically subtracts `delta` and returns the **previous** value. |
| `atomicIntCas` | `(atomic: AtomicInt, expected: int, desired: int) -> bool` | Compare-and-swap: if current value equals `expected`, sets to `desired` and returns `true`; otherwise returns `false`. |
| `atomicIntFree` | `(atomic: AtomicInt) -> Unit` | Immediately frees the atomic integer. Optional — ARC handles cleanup automatically. |

## AtomicBool

| Function | Signature | Description |
|----------|-----------|-------------|
| `atomicBoolNew` | `(value: bool) -> AtomicBool` | Creates an atomic boolean with the given initial value. |
| `atomicBoolLoad` | `(atomic: AtomicBool) -> bool` | Atomically reads the value. |
| `atomicBoolStore` | `(atomic: AtomicBool, value: bool) -> Unit` | Atomically writes the value. |
| `atomicBoolFree` | `(atomic: AtomicBool) -> Unit` | Immediately frees the atomic boolean. Optional — ARC handles cleanup automatically. |

## Comparison with async/await

| Feature | `async`/`await` | `thread` module |
|---------|----------------|-----------------|
| Execution model | Work-stealing thread pool | Dedicated OS threads |
| Best for | I/O-bound tasks, many lightweight tasks | CPU-bound tasks, fine-grained control |
| Synchronization | Automatic via `await` | Manual via Lock, Semaphore, etc. |
| Overhead | Low (task scheduling) | Higher (OS thread creation) |
