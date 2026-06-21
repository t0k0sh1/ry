# JSONL Run Logs

This document specifies the structured run-log format emitted by
`scripts/export-run-logs.sh`. The format wraps existing ry test
commands (`ry test`, `ry test --trace`, `ry_tests`) into a stable JSON
Lines stream so downstream tooling — primarily future LoRA / aLoRA
training in the ry-code project (`t0k0sh1/ry-code#50`, `t0k0sh1/ry-code#51`)
— can consume real run data without depending on the ANSI-colored
human-readable stdout of the test binaries. Issue #1731 introduced the
pipeline.

## Non-goals

- No training or dataset construction. This document covers the raw
  log shape only.
- No CI integration. Logs are produced on demand by a developer running
  the wrapper script locally.
- No `ry` binary changes. The wrapper consumes the existing CLI surface
  unchanged.
- No upload or remote storage. Output is always written to the local
  filesystem under a git-ignored directory.

## Generating logs

```
scripts/export-run-logs.sh [--trace] <target> [<target>...]
scripts/export-run-logs.sh --target=ry_tests
```

`<target>` is a `*.test.ry` file or a directory; directories are
expanded recursively and each file is run as a separate `ry test
<single-file>` invocation so that `--trace` remains usable and per-file
stdout / stderr / exit codes can be captured cleanly. Directory
fan-out via `ry test <dir>` is intentionally avoided: that mode merges
output streams across files and silently disables `--trace`.

| Env var       | Default                              | Purpose                              |
|---------------|--------------------------------------|--------------------------------------|
| `RY_BUILD_DIR`| `build-rust/` if present else `build/` | location of `ry` and `ry_tests`     |
| `RY_LOG_DIR`  | `.ry-eval/runs`                      | output root directory                |

Dependency: `jq`. It is used both to emit JSON Lines records and as the
sub-second wall-clock source (`jq -n 'now * 1000 | floor'`). The script
exits 1 with a clear message if `jq` is missing.

## Directory layout

```
.ry-eval/runs/<run-id>/
  run.jsonl
  artifacts/
    <attempt>-<slug>.stdout.txt
    <attempt>-<slug>.stderr.txt
    <attempt>-<slug>.trace.jsonl   # only if --trace
    gtest.json                     # only if --target=ry_tests
```

- `<run-id>` = `YYYYMMDD-HHMMSS-<short-sha>-<microseconds>` (UTC). The
  microsecond component is a nonce that prevents two invocations within
  the same wall-clock second from colliding on the run directory. When
  the working directory is not a git repo or git is unavailable,
  `<short-sha>` is the literal `nogit`.
- `<attempt>` is the per-run 1-based invocation counter, zero-padded to
  four digits. It guarantees per-invocation uniqueness even if the same
  target is passed more than once in a single run.
- `<slug>` is derived from the target path with `/` and `.` replaced by
  `-` and the trailing `.test.ry` suffix stripped. The `ry_tests` mode
  uses the literal slug `gtest`.
- `RY_BUILD_DIR`, `RY_LOG_DIR`, and positional `<target>` paths must be
  repo-relative. Absolute paths or paths that escape the repo root via
  `..` are rejected (env vars fail the run, positional targets are
  skipped with a warning) so the privacy contract for the JSONL metadata
  fields holds even under misconfiguration.
- The root directory `.ry-eval/` is git-ignored.

## Record schema

Every record is one JSON object on its own line. `schema_version` is
the string `"1"`.

### `run_meta`

Emitted once, as the first record of `run.jsonl`.

| Field            | Type            | Source / notes                                                    |
|------------------|-----------------|-------------------------------------------------------------------|
| `record_type`    | `"run_meta"`    | literal                                                           |
| `schema_version` | `"1"`           | literal                                                           |
| `run_id`         | string          | the `<run-id>` above                                              |
| `started_at`     | string          | RFC3339 UTC (e.g. `2026-06-21T12:34:56Z`)                         |
| `host_os`        | string          | `uname -s` only — no hostname or username                         |
| `git_sha`        | string          | `git rev-parse HEAD`, or `""` if unavailable                      |
| `git_branch`     | string          | `git rev-parse --abbrev-ref HEAD`, or `""`                        |
| `git_dirty`      | bool            | `true` iff `git status --porcelain` is non-empty                  |
| `ry_build_dir`   | string          | the resolved build directory, relative to repo root               |
| `ry_version`     | string \| null  | first line of `ry --version`, or `null` if it cannot be obtained  |

### `command`

Emitted once per ry test or `ry_tests` invocation.

| Field               | Type                | Notes                                                                              |
|---------------------|---------------------|------------------------------------------------------------------------------------|
| `record_type`       | `"command"`         | literal                                                                            |
| `schema_version`    | `"1"`               | literal                                                                            |
| `run_id`            | string              | same value as the preceding `run_meta`                                             |
| `target`            | string              | repo-relative path to the `.test.ry` file, or the literal `"ry_tests"`             |
| `command`           | array of strings    | full argv that was executed                                                        |
| `exit_code`         | int                 |                                                                                    |
| `duration_ms`       | int                 | wall-clock duration                                                                |
| `started_at`        | string              | RFC3339 UTC                                                                        |
| `finished_at`       | string              | RFC3339 UTC                                                                        |
| `stdout_path`       | string              | repo-relative path to the raw stdout artifact                                      |
| `stderr_path`       | string              | repo-relative path to the raw stderr artifact                                      |
| `stdout_byte_count` | int                 | size in bytes of the stdout artifact                                               |
| `trace_path`        | string \| null      | repo-relative path to the trace JSONL, or `null` when `--trace` was not requested  |
| `gtest_json_path`   | string \| null      | repo-relative path to the gtest JSON output, or `null` when not in `ry_tests` mode |
| `summary`           | object \| null      | `{passed, failed, skipped, todo}` integers parsed from `ry test` stdout (after ANSI stripping), or `null` for `ry_tests` and when the summary line cannot be parsed |

The stdout / stderr / trace artifact files are written **verbatim** —
no truncation. `ry test <single-file>` and `ry_tests` produce bounded
output for the supported targets (no streaming or long-running modes),
so `stdout_byte_count` is the size signal and there is no
`stdout_truncated` field. If unbounded output ever becomes a concern,
a real head+tail cap and a meaningful flag should be added together,
bumping `schema_version`.

## Versioning policy

`schema_version` is a string so future minor versions like `"1.1"`
remain comparable to consumers.

- **Additive changes** to a record type (new optional fields, new
  record types) keep the same major version. Consumers must tolerate
  unknown fields.
- **Breaking changes** (field rename, field removal, type change,
  semantics change) bump the major component and add a migration note
  to this document.

## Privacy expectations

The JSONL **metadata fields** are deliberately scrubbed:

- Paths recorded in fields like `stdout_path`, `stderr_path`,
  `trace_path`, `gtest_json_path`, `target`, `ry_build_dir` are
  repo-relative — never absolute.
- `host_os` is the output of `uname -s` only. No hostname, username,
  `$HOME`, `$USER`, or environment dump appears anywhere in the JSONL.

The **artifact files**, however, are captured verbatim from the child
processes and may still contain locally-identifying content that the
wrapper does not strip:

- `stdout.txt` and `stderr.txt` may embed absolute paths or usernames
  in error messages, panic backtraces, or `assert!` output produced by
  the underlying binary.
- `trace.jsonl` events such as `call.enter` carry a `file` field whose
  value is whatever path the runtime received — often a repo-relative
  path, but not guaranteed across all entry points.
- `gtest.json` is produced by Google Test and follows its own schema;
  it is passed through unchanged.

Consumers must treat artifact contents as untrusted local data and
apply their own scrubbing before publishing or training on them. Do
not store secrets, customer data, or non-public absolute paths in
inputs that flow through the wrapped commands.
