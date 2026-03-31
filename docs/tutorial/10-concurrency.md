[English](10-concurrency.md) | [日本語](../ja/tutorial/10-concurrency.md) | [繁體中文](../zh/tutorial/10-concurrency.md)

# Concurrency

[<- Prev: Packages](09-modules.md) | [Next: Testing ->](11-testing.md)

Ry provides three models for concurrent and parallel execution: **async/await** for lightweight I/O-bound tasks, **@parallel** for data-parallel loops, and **OS threads** for CPU-bound workloads with fine-grained control.

---

## async/await

`Task<T>` is the runtime handle for concurrent work. Use `async function` to declare functions that return tasks, `await` inside another `async function` to wait for a task, and `block_on(task)` from synchronous context.

### Defining Async Functions

```python
async function add(a: int, b: int) -> int:
    return a + b
```

Calling an `async function` returns a `Task<T>` immediately — the work starts in the background:

```python
t: Task<int> = add(20, 22)
```

### Waiting for Results

From **synchronous** code, use `block_on()`:

```python
print(block_on(t))             # 42
print(block_on(add(1, 2)))     # 3
```

From **async** code, use `await`:

```python
async function double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2

print(block_on(double_add(3, 4)))   # 14
```

### Composing Async Functions

You can chain async operations naturally:

```python
async function fetch_score() -> int:
    return 42

async function process() -> str:
    score = await fetch_score()
    return f"Score: {score * 2}"

print(block_on(process()))   # Score: 84
```

> **Why async/await?** It lets you write concurrent code that reads like sequential code. The runtime schedules tasks efficiently on a thread pool — ideal for I/O-bound work where you spend most of the time waiting.

> **Common mistake**: Using `await` in a non-async function causes a compile error. Use `block_on()` instead when calling from synchronous context.

---

## @parallel

The `@parallel` directive parallelizes counted `for` loops across multiple CPU cores:

```python
@parallel
for i in range(8):
    print(i)
```

### Restrictions

`@parallel for` is designed for independent iterations. It rejects:
- **`break` and `continue`** — there is no meaningful ordering across parallel iterations.
- **Writes to outer mutable variables** — this would cause data races.

Only counted loops (`range(...)` or integer `..` ranges) are supported.

> **Why @parallel?** It is the simplest way to parallelize independent work. No locks, no atomics — just add the directive and the runtime handles distribution.

---

## OS Threads

For CPU-bound tasks or when you need fine-grained synchronization, use the `thread` module to create OS threads.

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add
```

### Spawning Threads

`thread_spawn` takes a closure and starts a new OS thread:

```python
counter = atomic_int_new(0)

t = thread_spawn(function():
    atomic_int_add(counter, 1)
)

thread_join(t)
print(atomic_int_load(counter))   # 1
```

> **Note**: Captured variables are copied into the thread. For primitive types (`int`, `str`, etc.), this produces an independent copy. For opaque handle types (`AtomicInt`, `Lock`, etc.), the underlying resource is shared — which is exactly what you want for synchronization.

### Shared State with Atomics

`AtomicInt` provides thread-safe integer operations without locks:

```python
from thread import thread_spawn, thread_join, atomic_int_new, atomic_int_load, atomic_int_add

counter = atomic_int_new(0)

t1 = thread_spawn(function():
    atomic_int_add(counter, 1)
)
t2 = thread_spawn(function():
    atomic_int_add(counter, 1)
)

thread_join(t1)
thread_join(t2)
print(atomic_int_load(counter))   # 2
```

### Mutual Exclusion with Locks

When you need to protect a critical section (more than a single atomic operation), use a `Lock`:

```python
from thread import thread_spawn, thread_join, lock_new, lock_acquire, lock_release
from thread import atomic_int_new, atomic_int_load, atomic_int_add

lock = lock_new()
counter = atomic_int_new(0)

t = thread_spawn(function():
    lock_acquire(lock)
    # Critical section: only one thread at a time
    atomic_int_add(counter, 1)
    lock_release(lock)
)

lock_acquire(lock)
atomic_int_add(counter, 1)
lock_release(lock)

thread_join(t)
print(atomic_int_load(counter))   # 2
```

### Other Synchronization Primitives

The `thread` module also provides:

| Primitive | Use Case |
|-----------|----------|
| `RWLock` | Multiple readers or one writer (read-heavy workloads) |
| `Semaphore` | Limit concurrent access to a resource |
| `Barrier` | Wait until N threads reach the same point |
| `AtomicBool` | Thread-safe boolean flag |

See [Thread Reference](../reference/thread.md) for the full API.

---

## Networking (TCP Sockets)

Ry provides TCP socket support through the `net` module. Network operations return `Result` types (from [Error Handling](08-error-handling.md)) since connections can fail.

```python
from net import bind, listen, accept, connect, listener_port
from io import to_bytes, bytes_to_str

async function echo_server(server: TcpListener) -> str:
    match accept(server):
        case Ok(conn):
            match receive(conn, 4096):
                case Ok(data):
                    match send(conn, data):
                        case Ok(_):
                            ...
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(conn)
        case Err(e):
            ...
    close(server)
    return "done"
```

See [Network Reference](../reference/net.md) for the full TCP API.

---

## Choosing the Right Model

| Feature | `async`/`await` | `@parallel` | `thread` module |
|---------|----------------|-------------|-----------------|
| Best for | I/O-bound tasks | Data-parallel loops | CPU-bound, fine-grained control |
| Overhead | Low (task scheduling) | Low (automatic) | Higher (OS thread creation) |
| Synchronization | Automatic via `await` | Not needed (independent iterations) | Manual (Lock, Semaphore, etc.) |
| Complexity | Medium | Low | High |

---

## Common Mistakes

1. **Using `await` outside `async function`**: Use `block_on()` from synchronous context instead.
2. **Writing to outer variables in `@parallel`**: This is rejected at compile time to prevent data races.
3. **Forgetting `thread_join`**: If the main program exits before a thread finishes, the thread's work may be lost.
4. **Sharing primitive variables between threads**: Primitives are copied into closures. Use `AtomicInt` or `Lock` for shared state.

---

## Exercises

1. **async/await**: Write two `async function` functions — one that returns a first name and one that returns a last name. Write a third `async function` that `await`s both and returns the full name. Use `block_on()` to print the result.

2. **Threads with atomics**: Spawn 5 threads that each add 10 to a shared `AtomicInt`. After joining all threads, verify the counter is 50.

---

[<- Prev: Packages](09-modules.md) | [Next: Testing ->](11-testing.md)
