# libFuzzer Harnesses

Coverage-guided fuzzing for the ry compiler and runtime using [LLVM libFuzzer](https://llvm.org/docs/LibFuzzer.html) combined with ASan and UBSan.

## Targets

| Target | Entry point | What it covers |
|---|---|---|
| `fuzz_parser` | `ry::Lexer` + `ry::Parser::parseProgram()` | Lexer + parser pipeline on arbitrary byte sequences |
| `fuzz_json` | `__ry_json_parse` | JSON parser on arbitrary byte sequences |
| `fuzz_utf8` | `__ry_utf8_len_n`, `__ry_utf8_char_at_checked`, `__ry_utf8_reverse`, `__ry_utf8_substring` | Bounded UTF-8 walker functions |

**Regex is not fuzzed** — `RegexParser::parse()` calls `exit(1)` on malformed patterns, which terminates the libFuzzer process. See the follow-up issue for the planned refactor.

## Building

libFuzzer requires Clang. Apple Clang on macOS does **not** ship the fuzzer runtime; use the LLVM Homebrew tap instead.

### macOS

```bash
# Requires: brew install llvm@21
SDKROOT=$(xcrun --show-sdk-path) \
CC=/opt/homebrew/opt/llvm@21/bin/clang \
CXX=/opt/homebrew/opt/llvm@21/bin/clang++ \
    cmake --preset fuzz

cmake --build build-fuzz
```

### Linux

The `fuzz` preset works without extra env vars when `/usr/local/llvm/bin/clang` (installed by the project's `setup-llvm` action) is in `$PATH`. On CI, `CC`/`CXX` are set explicitly by the `fuzz` workflow job.

```bash
CC=/usr/local/llvm/bin/clang CXX=/usr/local/llvm/bin/clang++ cmake --preset fuzz
cmake --build build-fuzz
```

## Running

```bash
# Short smoke test (1 run per corpus entry)
./build-fuzz/fuzz_parser -runs=1 tests/fuzz/corpus/parser

# Timed run (60 seconds, memory limit 512 MB, save crashes to regressions/)
ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1 \
UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
./build-fuzz/fuzz_parser \
    -max_total_time=60 \
    -rss_limit_mb=512 \
    -artifact_prefix=tests/fuzz/regressions/parser/ \
    tests/fuzz/corpus/parser

# Same pattern for fuzz_json and fuzz_utf8
```

`-rss_limit_mb=512` prevents OOM — compiler-input harnesses can peak high.

## Directories

```
tests/fuzz/
├── fuzz_parser.cpp          # Harness: lexer + parser
├── fuzz_json.cpp            # Harness: JSON parser
├── fuzz_utf8.cpp            # Harness: bounded UTF-8 walkers
├── corpus/
│   ├── parser/              # Seed inputs for fuzz_parser (*.ry snippets)
│   ├── json/                # Seed inputs for fuzz_json
│   └── utf8/                # Seed inputs for fuzz_utf8 (text + binary)
└── regressions/
    ├── parser/              # Saved crash inputs for fuzz_parser
    ├── json/                # Saved crash inputs for fuzz_json
    └── utf8/                # Saved crash inputs for fuzz_utf8
```

**Corpus policy**: hand-curated seeds that cover representative input shapes. libFuzzer augments the corpus with discovered interesting cases at runtime (in-memory only; the on-disk corpus is the starting point). When a regression is fixed, add the crashing input to both `regressions/<name>/` (for reference) and `corpus/<name>/` (so the fuzzer starts from it).

## When a crash is found

1. The harness writes the crashing input to `-artifact_prefix` (default: `./crash-<sha1>`).
2. Reproduce: `./build-fuzz/fuzz_<name> <crash-file>`.
3. Copy the crash file to `tests/fuzz/regressions/<name>/`.
4. Copy the crash file to `tests/fuzz/corpus/<name>/` (seed for future runs).
5. File a GitHub issue if the bug is in existing code (not introduced by the current PR).
6. Fix and verify: re-run the crash file, confirm exit 0.

## Adding a new harness

1. Create `tests/fuzz/fuzz_<name>.cpp` with `extern "C" int LLVMFuzzerTestOneInput(...)`.
2. Create `tests/fuzz/corpus/<name>/` with representative seed files.
3. Create `tests/fuzz/regressions/<name>/.gitkeep`.
4. Register in `CMakeLists.txt`: `add_ry_fuzz_target(fuzz_<name> tests/fuzz/fuzz_<name>.cpp ...)` inside the `if(ENABLE_FUZZER)` block.
5. Add a step to the `fuzz` job in `.github/workflows/ci.yml`.
