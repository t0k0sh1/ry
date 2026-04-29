---
name: tsan-known-issues
description: TSan (ThreadSanitizer) known issues and policy for the ry project — required vs warn-only job split, LargeMmapAllocator upstream bug, and parallel_for_depth_ design rationale. Use when TSan CI fails, when adding concurrency features, or when evaluating whether a TSan failure is a real race or an upstream TSan allocator bug.
allowed-tools: Bash
---

# TSan Known Issues

Reference for ThreadSanitizer policy and known infrastructure limitations in the ry project.

---

### TSan job — C++ tests required, Ry self-tests warn-only

**Source**: #868 (warn-only landed), #874 (split policy)
**Tags**: tsan, sanitizer, ci, concurrency

**Rule**: The C++ TSan step (`./build-tsan/ry_tests`) is required
and gates #630's P0 race fixes via `ConcurrencySpecSuite` (which
runs `tests/spec/concurrency.test.ry`). The Ry self-test TSan step
(`./build-tsan/ry test -p`) runs as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed — see the separate `LargeMmapAllocator` entry. A PR that
introduces a new race must fix it in the same PR; warn-only is a
workaround for the TSan runtime bug, not tolerance for real races.
`parallel_for_depth_` only propagates to ARC ops emitted **inside**
the thunk body, not through helper calls; if a helper-alias race
surfaces, widen via #872 rather than blindly expanding the flag.

---

### TSan `LargeMmapAllocator` CHECK on Linux self-test runs

**Source**: #868 / #874
**Tags**: tsan, sanitizer, ci, ubuntu, gotcha

**Rule**: Keep `./build-tsan/ry test -p` as `continue-on-error: true`
until upstream https://github.com/google/sanitizers/issues/1716
is fixed: the self-test driver aborts inside TSan's
`LargeMmapAllocator::Deallocate` — a runtime bug, not a race in
ry code. Pinning `ubuntu-22.04` and lowering `vm.mmap_rnd_bits=28`
were both tried. `./build-tsan/ry_tests` (which includes
`ConcurrencySpecSuite`) runs cleanly on the same binary, so that
is the required gate for #630's race fixes. macOS is unaffected.

---

### `@parallel for` captures must be retained AND ARC-backed inside the thunk

**Source**: #630 / #874
**Tags**: parallel-for, arc, cow, codegen, fnscope, gotcha

**Rule**: For `@parallel for` / `threadSpawn` captures: before
`FnScope` clears the flags, snapshot each `isArcManaged(src)` /
`arc_backed_vars_.count(src)`; re-apply on the thunk's `dst`
alloca (propagateMeta's own `markArcManaged` branch silently
no-ops under FnScope clearing); emit an atomic
`emitArcRetain(hdr, true)` at entry, matched by `popScope()`
before the terminator while `parallel_for_depth_ > 0`. Without
the retain, CoW's `> 1` check fails and workers mutate shared
buffers in place (libmalloc `POINTER BEING FREED WAS NOT
ALLOCATED`). Repro: `tests/spec/concurrency.test.ry`
"parent list is unchanged when workers mutate captured list";
impl: `src/codegen_stmt_loop.cpp::emitParallelForRange`.
