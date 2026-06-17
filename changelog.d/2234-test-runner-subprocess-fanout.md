### refactor(test-runner): unify `ry test` on per-file subprocess fan-out (no in-process loop) (#2234)

The in-process sequential loop that ran multiple test files in the parent `runRyFile` call (`runTestFilesSequential`) is removed. `ry test` on a directory or auto-discovery target now always goes through `posix_spawn` fan-out — even at worker=1 — so each test file runs in its own child process that exits via `_exit`. This is what the 6-step JIT teardown suppression (`LLJIT` / `CodeGen` / `Program` AST leak + `_exit` past the C++ static destructor chain) structurally requires: "1 source = 1 process". The in-process loop had accumulated suppression leaks across files (`std::bad_alloc` at ~42/181 in `Parser::parseTypeNameSingle` → `TypeNode::makeTuple` empirically reproducible).

`-p` semantics are now uniform — only the worker count changes between cases:

| Invocation | Workers | Path |
|---|---|---|
| `ry test` (no `-p`) | 1 | subprocess fan-out, one at a time |
| `ry test -p` (no `N`) | `computeDefaultWorkers(hw) = max(1, hw-1)` | subprocess fan-out |
| `ry test -p N` | `N` | subprocess fan-out |

Single-file direct execution (`ry test foo.test.ry`) still JITs in the parent process — no recursive subprocess is needed when there is only one file.

Multi-file (directory / auto-discovery) `--coverage` / `--trace` / `--outline` are now warned and disabled at the call site: the per-file subprocess argv is `{exe, "test", filepath}` only, so these flags cannot be honored across the boundary without structured IPC (out of scope). Use them with a single file (`ry test path/to/foo.test.ry --coverage`) where they continue to work unchanged. The old multi-file `--coverage` had not been usable in practice anyway — it tripped `std::bad_alloc` after ~42 files. The old "`--coverage` with `--parallel` falls back to sequential" wording is replaced; the new wording names "single-file only". The user's `-p N` value is preserved even when one of these flags is disabled — once the feature is off, throttling workers to 1 buys nothing functional and would silently penalise `ry test -p N --coverage` for no gain (the issue's literal "worker=1 強制" wording was an artifact of the pre-#2234 sequential-fallback path).

`runTestFiles` / `discoverAndRunTests` signatures drop `bool parallel`, `const char *argv0`, `bool skip_global_lib`, `bool coverage`, and `bool outline` — none were forwarded to the child subprocess. Only `int parallel_workers` remains.

Follow-ups carved out as separate issues: #2235 (`@it` driver-fn codegen split), #2236 (multi-file `--outline` via subprocess argv extension), #2237 (Linux CI `-p` removal under the new semantics), #2238 (`.claude/rules` write-up of "per-file process is the only legal isolation unit"). The 6-step JIT teardown suppression (#742 family) remains masked, not fixed.
