### Fixed

- `rwlock_unlock` now dispatches between shared and exclusive release via
  a `thread_local` counter per RWLock, eliminating the two-step window in
  `rwlock_read_lock` where `std::shared_mutex::lock_shared()` was held
  but the tracking map had not yet been updated. Under the previous
  implementation an unlock that observed the transient state would have
  fallen through to exclusive `unlock()`, corrupting `std::shared_mutex`
  state. (#871, follow-up to #630 P1)
- `ThreadHandle::has_error` is now a `std::atomic<bool>`; the worker
  thread's catch blocks store it with `memory_order_release` after
  writing `error_msg`, and `thread_join` loads it with
  `memory_order_acquire`. This makes the error-field publish/subscribe
  contract explicit, TSan-friendly, and robust for any future pre-join
  error polling path. (#871, follow-up to #630 P1)
