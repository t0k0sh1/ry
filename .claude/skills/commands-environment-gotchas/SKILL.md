---
name: commands-environment-gotchas
description: Reference for non-obvious command invocation mistakes — wrong flags, missing env vars, shell syntax traps, and gh/git/cmake pitfalls discovered during ry development. Use when you are about to run a command that previously failed in a non-obvious way, or when troubleshooting a command that exits unexpectedly.
allowed-tools: Bash
---

# Commands / Environment Gotchas

This skill records command/environment mistakes that were non-obvious to diagnose. Each entry has a `Wrong → Correct → Why` triple so the same mistake is not repeated.

---

### Record corrected command invocations (meta-rule)

**Source**: implementation experience (ongoing)
**Tags**: commands, environment, tooling, meta

**Rule**: When a command turns out to be wrong (bad flag, wrong path,
missing env var, outdated syntax) and you find the correct form, add
an entry below with a `Wrong → Correct → Why` triple. Examples of
what qualifies:

- forgetting `ASAN_OPTIONS=detect_container_overflow=0` on
  `build-asan/ry_tests`
- forgetting `RY_ENV=internal` when running a globally-installed `ry`
- calling `gh pr view` without `--repo` in a fork context
- using a wrong `cmake --preset` name
- heredoc / quoting / escaping mistakes in shell snippets

Skip: plain typos and mistakes that anyone would catch immediately.

**How to add a new entry**: every time you iterate on a command and
the second invocation works, ask "was the fix non-obvious?". If yes,
write a 3-line entry under this section with a descriptive subheading.

---

### Testing stdlib changes: run from the project root, not from /tmp/

**Source**: #1130 implementation (base64 `List<u8>` overloads)
**Tags**: commands, environment, stdlib, dev-stdlib, module-loader

**Wrong**: `printf '...\n' > /tmp/b64_smoke.ry && ./build/ry /tmp/b64_smoke.ry`
→ Error: `'encodeBytes' not found in module 'base64'` (verbatim shape per `src/module/module_loader.cpp` after #1483; the function name was also renamed `encode_bytes` → `encodeBytes` in #1415)

**Correct**: `./build/ry test tests/spec/base64.test.ry` (or any `.ry` inside the repo)

**Why**: `./build/ry` resolves the stdlib path via `package.toml`'s hidden `[paths]._dev_stdlib` key, which requires a `package.toml` somewhere in the ancestor directory chain. Files under `/tmp/` have no such ancestor, so the module loader falls back to `~/.ry/share/std` (the globally-installed stdlib), which does not contain the newly added declarations. The trace event to look for: `"resolved_path":"/Users/.../.ry/share/std"` instead of `"resolved_path":"/Users/.../Workspace/ry-2/share/std"`.

**Note on `RY_ENV=internal`**: The `_dev_stdlib` resolution above runs automatically for repo-local `./build/ry` invocations — `RY_ENV=internal` is **not** required for routine development. The env var exists for additional isolation (e.g. forcing the in-repo stdlib path even when the working directory has no `package.toml` ancestor chain, or when a globally-installed `ry` would otherwise win); use it only when you explicitly need that override.

---

### ASan + UBSan: `detect_container_overflow=0` and `-fno-sanitize=vptr,function` are not optional

**Source**: AGENTS.md historical note (migrated from inline sanitizer section, 2026-05-28)
**Tags**: sanitizer, asan, ubsan, cmake, preset, llvm, false-positive

**Rule**: When invoking `./build-asan/ry_tests` or `./build-asan/ry test -p`, always set:

```bash
ASAN_OPTIONS=detect_container_overflow=0 \
UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-asan/ry_tests
```

The `asan` preset itself already builds with `-fno-sanitize=vptr,function`.

**Why**:

- **`detect_container_overflow=0`**: ASan's container-overflow detection requires every C++ allocation to participate in the same ASan-instrumented heap. The repo links against the system LLVM libraries (`/usr/local/llvm/...`) which are **not** built with ASan, so any `std::vector` / `std::string` flowing across the boundary trips false-positive container-overflow reports. Disabling this single check leaves the bulk of ASan (heap UAF, OOB, double-free, leaks) intact.
- **`-fno-sanitize=vptr,function`** (preset, not env): The project builds with `-fno-rtti`, which strips typeinfo that UBSan's `vptr` check requires. The `function` check trips on LLVM's C-style function-pointer casts (e.g. `reinterpret_cast<void*>(&Func)` patterns inside ORC JIT plumbing) that are well-defined in practice but not provable to UBSan. Excluding both keeps the rest of UBSan (signed overflow, null deref, alignment, etc.) effective.

**How to apply**: If a fresh contributor reports "ASan flagged container-overflow on a trivial vector push" or "UBSan halted on a function-pointer cast in JIT teardown", the answer is one of these two — point them at this entry rather than chasing the false positive into LLVM internals. The TSan build uses a separate preset (`build-tsan/`); see `KNOWLEDGE.md` §サニタイザー既知問題 / TSan.

---

### `gh issue edit --label` replaces all labels; use `--add-label` / `--remove-label`

**Source**: #1144 (2026-04-18)
**Tags**: gh, issue, label, wip, workflow

**Wrong**: `gh issue edit <n> --label wip`
→ **Replaces** the entire label set with `["wip"]`. All other labels (`bug`, `enhancement`, milestone-shadow labels, etc.) are silently deleted.

**Correct**:
- Add a label: `gh issue edit <n> --add-label wip`
- Remove a label: `gh issue edit <n> --remove-label wip`
- Create with labels (safe — no pre-existing labels): `gh issue create --label enhancement --label documentation`

**Why**: `--label` in `gh issue edit` is a *set* operation, not an *append*. The flag name is misleading because `gh issue create --label` is safe (empty initial state). The asymmetry bites every time you remember the create syntax and apply it to edit.

**How to apply**: Use `git-claim-issue` skill for `wip` attachment (enforces `--add-label` internally). Use `git-close-pr` Step 7 for `wip` removal (enforces `--remove-label` internally). Never call `gh issue edit --label` directly for additive changes.

---

### bash `set -u` with empty array: use `"${arr[@]+"${arr[@]}"}"` not `"${arr[@]}"`

**Source**: #1165 Docker run.sh (2026-04-18)
**Tags**: commands, bash, shell, docker

**Wrong**: `docker run ... "${ENV_ARGS[@]}" ...` with `set -euo pipefail` and `ENV_ARGS=()`
→ Error: `ENV_ARGS[@]: unbound variable` when the array is empty

**Correct**: `docker run ... "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" ...`

**Why**: bash's `set -u` (nounset) treats an empty array expansion `"${arr[@]}"` as an
unbound variable. The idiom `"${arr[@]+"${arr[@]}"}"` uses parameter expansion with a
default — it expands to nothing when the array is empty, and to the full array contents
when non-empty. This is the standard POSIX-compatible workaround for `set -u` + optional
arrays in shell scripts.

---

### `ry -c` reads from stdin, not argv

**Source**: #1269 manual repro (2026-04-21)
**Tags**: commands, cli, ry, stdin

**Wrong**: `./build/ry -c 'print(1)'`
→ Silently prints nothing and exits 0. The positional argument after `-c` is ignored — the compiler reads an empty stdin, parses zero statements, and succeeds.

**Correct**: `printf 'print(1)\n' | ./build/ry -c` (or `echo 'print(1)' | ./build/ry -c`)

**Why**: `ry -c` follows a different convention from `python -c` / `sh -c`. It takes the source code on **stdin**, not as the next argv element. The `--help` output shows `echo '<code>' | ry -c` but this is easy to miss if you habitually reach for `-c 'snippet'` from shell/Python muscle memory. Particularly dangerous because the wrong form exits 0 with no output instead of erroring, so a failed manual repro looks like "compiler accepted the invalid program" when in fact no program was fed in at all.

**How to apply**: For one-off Ry snippets use a heredoc-to-pipe or write a scratch file under the project root (not `/tmp/` — see the `_dev_stdlib` gotcha above).

---

### Skill `allowed-tools` must cover all Bash commands the skill body prescribes

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: skill, allowed-tools, claude-code, review-feedback

**Rule**: Every Bash command that a SKILL.md step instructs the agent to run must be covered by an entry in `allowed-tools`. A common pitfall is listing only `gh pr:*`/`gh run:*`/`git branch:*` while the skill body also calls `cmake`, `clang-tidy`, `cppcheck`, `scan-build`, `find`, etc. At runtime the agent will be blocked from running those uncovered commands, silently breaking the step.

When the reproduction command set is open-ended (e.g. "run the CI job's corresponding local command"), use `Bash` (unrestricted) rather than a long enumeration of prefixes that will grow stale.

**How to verify**: grep the skill body for bare Bash commands not covered by the `allowed-tools` line.

---

### `gh run list --branch` returns all runs on a branch, not just the PR head commit

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: github-actions, gh-cli, review-feedback, gotcha

**Rule**: `gh run list --branch <name>` includes runs from every commit on that branch. In a CI investigation or re-run tool, this causes reruns and log analysis for commits unrelated to the PR being investigated.

Always filter by the PR's `headRefOid` (head commit SHA) immediately after the `gh run list` call:

```bash
gh run list --branch <headRefName> --limit 20 \
  --json databaseId,headSha,name,status,conclusion,workflowName \
  | jq --arg sha "<headRefOid>" '[.[] | select(.headSha == $sha)]'
```

Alternatively, derive run IDs directly from `detailsUrl` in the `gh pr checks` output (`grep -oE '/runs/([0-9]+)' | grep -oE '[0-9]+'`).

---

### Skill SKILL.md: keep `owner` and `repo` as separate variables when downstream steps use `{owner}`/`{repo}` individually

**Source**: PR #1148 CodeRabbit review / issue #1152 (2026-04-19)
**Tags**: skill, gh-cli, review-feedback

**Rule**: Using `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` and storing the result as both `owner` and `repo` is correct only when the downstream code treats the combined value as a single placeholder (e.g. `repos/$FULL/...`). When downstream steps separately substitute `{owner}` and `{repo}` (REST paths like `repos/{owner}/{repo}/pulls/{PR}/...` or GraphQL `repository(owner: "<owner>", name: "<repo>")`), the combined string causes doubled path segments (e.g. `repos/t0k0sh1/ry/ry/pulls/...`) or an incorrect GraphQL `owner` argument. In that case, fetch them separately: `OWNER=$(gh repo view --json owner --jq '.owner.login')` / `REPO=$(gh repo view --json name --jq '.name')`. When writing or reviewing a skill step that stores repository coordinates, verify whether downstream uses the value as one unit or as two — they require different fetch forms.

---

### macOS: Apple clang PCH と LLVM clang-tidy は互換性がない

**Source**: #1404 (2026-04-27)
**Tags**: cmake, clang-tidy, pch, macos, static-analysis, homebrew

**Wrong**: macOS で `cmake --preset default` (Apple clang で configure) の後そのまま `clang-tidy -p build --quiet` を実行する → `error: PCH file built from a different branch ((clang-apple) vs (clang))` で失敗

**Correct**: `build/` を削除して LLVM clang を CC/CXX に明示し、`SDKROOT` を渡してから再 configure する:

```bash
rm -rf build
SDKROOT=$(xcrun --show-sdk-path) \
CC=/opt/homebrew/opt/llvm@21/bin/clang \
CXX=/opt/homebrew/opt/llvm@21/bin/clang++ \
    cmake --preset default && cmake --build build
find src -name '*.cpp' | xargs /opt/homebrew/opt/llvm@21/bin/clang-tidy -p build --quiet
```

**Why**: CMake は configure 時のコンパイラに紐づいた PCH (`.gch`) を生成する。macOS デフォルトの `cmake --preset default` は Apple clang で PCH を作るが、Homebrew LLVM clang-tidy は upstream LLVM のバージョン文字列を期待するため、Apple clang 由来の PCH を読めない。CC/CXX を LLVM に固定すれば configure 時から LLVM PCH になり、clang-tidy が読める。

`SDKROOT` を渡さないと LLVM clang は libc++ ヘッダ検索パスから C ヘッダ (`stddef.h`, `cmath` など) を見つけられず、`<cstddef> tried including <stddef.h> but didn't find libc++'s <stddef.h> header` で失敗する。Apple clang は PATH 経由で macOS SDK を自動解決するが、Homebrew LLVM clang は明示が必要。

CI (Linux) では `/usr/local/llvm/bin/clang` が `cc` / `c++` symlink を介して PATH に入るため同じ問題は発生しない。

---

### macOS: SDK ヘッダ更新後の PCH staleness はフルリビルド不要 — `.pch` だけ削除して再ビルド

**Source**: #1724 セルフ検証 (2026-05-14)
**Tags**: cmake, ninja, pch, macos, sdk-update, sanitizer

**Wrong**: macOS SDK が xcode-select / Xcode 更新で差し替わった後、既存の `build-asan` / `build-tsan` を `cmake --build` すると `fatal error: file '.../MacOSX.sdk/usr/include/AvailabilityVersions.h' has been modified since the precompiled header '.../cmake_pch.hxx.pch' was built: size changed (was 31882, now 32391)` で失敗する。frustration から `rm -rf build-asan && cmake --preset asan && cmake --build build-asan` するのは時間の無駄（数分かかる）。

**Correct**: 該当ビルドツリーの `.pch` だけを削除して再ビルドする (数十秒で完了):

```bash
find build-asan -name 'cmake_pch.hxx.pch*' -delete
cmake --build build-asan
# 同様に build-tsan / build / build-fuzz も SDK 更新後は同じ症状が出る
find build-tsan -name 'cmake_pch.hxx.pch*' -delete
cmake --build build-tsan
```

**Why**: PCH は生成時に SDK ヘッダのファイルサイズ・mtime をスナップショットとして埋め込む。SDK ヘッダが入れ替わるとこのスナップショットと実体が一致しなくなり、PCH 読み込み時に上記の `fatal error` が発生する。CMake / Ninja は SDK ヘッダの変更を依存関係として追跡しない（システムヘッダは通常 `-MD` の出力に含まれない）ため、`.pch` だけが古い状態のまま残る。

`.pch` だけ削除すれば、次の `cmake --build` で PCH が再生成され、それ以降の翻訳単位は新しい PCH を使う。`build-asan/` 全体を消すと configure からやり直しになり、CMake のキャッシュ生成と LLVM ライブラリ依存の検出に数分かかる。`.pch` だけなら数十秒で済む。

上記の Apple clang vs LLVM clang PCH 互換性の entry はコンパイラ違いによる PCH 読み込み拒否の話で、こちらは SDK ヘッダ更新による staleness — 別の症状で別の対処なので両方を区別して記録する。**この対処は SDK 更新が原因の場合のみ有効**：preset / コンパイラ / フラグ変更を伴う場合は `.pch` 削除では不足するので preset の再 configure が必要。

---

### `cmd 2>&1 | tail -N` は ninja の失敗を silently mask する — exit code を別途確保せよ

**Source**: #1724 セルフ検証 (2026-05-14)
**Tags**: bash, pipefail, ninja, cmake, masked-failure, build-validation

**Wrong**:

```bash
cmake --build build-asan 2>&1 | tail -10
# tail が exit 0 を返すため bash の $? は 0
# 実際は ninja が exit 1 で PCH staleness エラーを出していたのに気付かない
# 結果: 古いバイナリで ASan テストを実行 → 既に修正したはずのバグが再現する
```

**Correct**: `set -o pipefail` を有効にするか、`tail` を通さず exit code を直接確認する:

```bash
# Option A: pipefail を有効化（zsh/bash 両対応）
set -o pipefail
cmake --build build-asan 2>&1 | tail -10

# Option B: tail を通さない（推奨 — エラー出力が完全に見える）
cmake --build build-asan

# Option C: exit code を別変数に保存
cmake --build build-asan 2>&1 | tail -10
# ↑ pipefail なしだと $? は 0
build_status=${PIPESTATUS[0]}  # bash 専用; zsh は $pipestatus[1]
[[ $build_status -eq 0 ]] || { echo "build failed"; exit 1; }
```

**Why**: bash / zsh の pipeline はデフォルトで「最後の要素の exit code」を返す。`cmd | tail` の場合、`cmd` が exit 1 でも `tail` は exit 0 で終わるので pipeline 全体は 0 と報告される。`set -o pipefail` を設定するとパイプ内で最初に失敗した要素の exit code が伝播する。

ninja は失敗時に短いエラー要約 (5-10 行) を末尾に出力するため `| tail -10` で見ようとしがちだが、これがまさに罠。`tail` が exit code を握り潰すので、Claude Code の Bash ツールが「成功」と判断してしまい、後続の sanitizer テスト実行が古いバイナリで走り、結果として「テストは pass したが実は古いコードのまま」という silent regression を生む。検出は downstream 症状（spec test が修正前のエラーメッセージを出す等）でしか起きず、デバッグに時間を浪費する。

---

### `printf "%s" "$big_var" | grep -q ...` silently misses matches under `set -o pipefail`

**Source**: #1617 PR review (CodeRabbit, 2026-05-08)
**Tags**: bash, pipefail, sigpipe, grep, large-output, masked-failure

**Wrong**:

```bash
set -euo pipefail
output="$(some_command)"           # output > pipe buffer (~64 KiB)
if printf '%s' "$output" | grep -qE 'PATTERN'; then
  has_match=1
fi
# has_match stays 0 even when $output clearly contains PATTERN
```

**Correct** (drop the upstream writer): use a here-string, or pure-bash `[[ =~ ]]`:

```bash
if grep -qE 'PATTERN' <<< "$output"; then has_match=1; fi
# or
if [[ "$output" =~ PATTERN ]]; then has_match=1; fi
```

**Why**: `grep -q` exits 0 on the first match and closes its end of the pipe. The upstream `printf` is still writing more bytes; the kernel delivers SIGPIPE → `printf` exits 141. Under `set -o pipefail` the pipeline's exit status is the **rightmost non-zero** exit code → the pipeline reports failure → the `if` branch never fires.

The bug only triggers when `$output` exceeds the pipe buffer (≈64 KiB on Linux/macOS); small outputs short-circuit before SIGPIPE can be delivered, which is why this kind of detection logic looks correct in tests but masks failures on real-world large inputs. In #1617 it caused three `tests/spec/*.test.ry` files containing `ERROR` nodes to be silently classified as PASS by `editor/tree-sitter/check.sh`'s self-verification (`pass=138`) until the pattern was switched to a here-string. Here-strings and `[[ =~ ]]` have no upstream writer, so SIGPIPE cannot occur.

---

### Subprocess GoogleTest: rebuild the `ry` executable, not just `ry_tests`

**Source**: #1424 implementation (2026-05-05)
**Tags**: cmake, ninja, googletest, subprocess, fork, exec, test-isolation

**Wrong**: After modifying `src/jit/jit_runner.cpp`, run `cmake --build build --target ry_tests` to retest. Subprocess tests like `DeprecatedWarningsTest` keep failing as if no source change had taken effect.

**Correct**: `cmake --build build` (no `--target`) — Ninja will relink both the test binary **and** the `ry` executable.

**Why**: Tests under `tests/test_emit_llvm_ir.cpp`, `tests/test_help.cpp`, and `tests/test_deprecated_warnings.cpp` use a `fork()` + `execv(RY_BINARY_PATH, ...)` pattern (the `RunResult` / `runRy()` helper). They invoke the on-disk `./build/ry` binary, **not** the `runRySource()` C++ symbol linked into `ry_tests`. The `ry_tests` target depends on `ry_lib.a` (the static library), but it does **not** depend on the `ry` executable. So `--target ry_tests` skips relinking `./build/ry`, and the subprocess test forks an old binary that still has the pre-fix behavior. The failure mode is confusing because in-process tests on the same code change pass — only the subprocess tests see a stale binary. Always rebuild without `--target` (or run `--target ry_tests --target ry`) when subprocess tests are in scope.

---

### GraphQL `viewer` is a root Query field, not a `Repository` field

**Source**: PR #1937 (2026-05-28, `/git-close-pr` dogfood)
**Tags**: gh, graphql, viewer, query-type

**Wrong**: `gh api graphql -f query='{ repository(owner: "...", name: "...") { pullRequest(number: N) { ... } } viewer { login } } }'`
→ `Field 'viewer' doesn't exist on type 'Repository'`. The closing braces visually suggest `viewer` is outside `repository(...)` but the brace count is off by one — `viewer` ends up nested inside `repository`.

**Correct**: `gh api graphql -f query='{ repository(owner: "...", name: "...") { pullRequest(number: N) { ... } } viewer { login } }'` (one less `}` after the `pullRequest` close so `viewer` sits at the root Query level alongside `repository`).

**Why**: GitHub GraphQL exposes `viewer` only on the root `Query` type. Mis-nested braces are easy to miss because the editor's brace matcher highlights the wrong pair. When composing multi-block queries, count `{ ... }` pairs after the outermost `repository(...)` block before pasting into `gh api graphql -f`.

---

### `gh pr checks --json` uses `bucket`/`state`/`link`, not `status`/`conclusion`/`detailsUrl`

**Source**: PR #1937 (2026-05-28, `/git-close-pr` dogfood)
**Tags**: gh, pr-checks, json-fields

**Wrong**: `gh pr checks <PR> --json name,status,conclusion,detailsUrl`
→ `Unknown JSON field: "status"`. Naming is borrowed from `gh run list --json` (which *does* expose `status` / `conclusion`), but `gh pr checks` uses different fields.

**Correct**: `gh pr checks <PR> --json name,bucket,state,link`

**Why**: `gh pr checks --json` field set is `bucket, completedAt, description, event, link, name, startedAt, state, workflow`. Aggregate pass/fail logic should use `bucket`: values are `pass` (SUCCESS / SKIPPED / NEUTRAL), `fail`, `pending`, `cancel`, `skipping`, `stale`. `state` carries the underlying conclusion string (e.g. `SUCCESS`, `FAILURE`); `link` replaces `detailsUrl`. Run `gh pr checks --help` to list the current field set before composing the flag.

---

### `gh pr view --json mergeable` is conflict-only; `mergeStateStatus` carries CI / branch-protection state

**Source**: PR #1937 (2026-05-28, `/git-close-pr` dogfood)
**Tags**: gh, pr-view, mergeable, merge-state-status, branch-protection

**Wrong**: gating merge on `mergeable` alone — e.g. `gh pr view <PR> --json mergeable --jq .mergeable` returns `MERGEABLE` so the script proceeds, then `gh pr merge` rejects with "Pull request is not mergeable" because branch protection still requires green CI / reviews.

**Correct**: read both fields and treat `mergeStateStatus ∈ {CLEAN, HAS_HOOKS}` as the actual gate:

```bash
gh pr view <PR> --json mergeable,mergeStateStatus --jq '"\(.mergeable) \(.mergeStateStatus)"'
# Accept only: "MERGEABLE CLEAN" or "MERGEABLE HAS_HOOKS"
```

**Why**: `mergeable` is the merge-conflict judgment only — it returns `MERGEABLE` / `CONFLICTING` / `UNKNOWN` based on whether the base/head can be three-way merged. `mergeStateStatus` is the comprehensive UI state and exposes `CLEAN`, `HAS_HOOKS` (both ready), `BLOCKED` (required checks not green / required reviews missing / signed-commit policy), `BEHIND` (head behind base), `DIRTY` (conflicts; mirrors `mergeable: CONFLICTING`), `DRAFT`, `UNKNOWN`, `UNSTABLE` (non-required failures). CI-pending and branch-protection states show up as `BLOCKED` while `mergeable` stays `MERGEABLE`, so skipping the second check lets the merge call proceed and fail noisily. The `gh pr merge` documentation says "the merge will not happen unless the pull request is in a mergeable state" without spelling out which API field that maps to — the answer is `mergeStateStatus`, not `mergeable`.

**Note (where the strict gate applies)**: This `mergeStateStatus ∈ {CLEAN, HAS_HOOKS}` gate belongs at the actual merge call (e.g. `git-close-pr` Step 6 / `gh pr merge`). Pre-check steps should distinguish structural blockers (`DIRTY` / `mergeable: CONFLICTING`) from transient states (`BLOCKED` while CI runs, `BEHIND`, `UNSTABLE`, `UNKNOWN`, `DRAFT`) that subsequent steps will resolve. See `git-close-pr` Step 1 (structural-only pre-check, warn-and-proceed for transient states) vs Step 6 (strict gate before merge). Issue #1956 (2026-05-29) — applying the strict gate at Step 1 wrongly serialized CI completion with review handling.

---

### In-container `RY_LLVM_EMIT_IMPL_RUST=ON` verification: pull the `ry-ci` image, don't use `docker/run.sh`'s dev image

**Source**: #1998 (2026-06-03, Sub-issue 4 self-verification)
**Tags**: docker, ry-llvm-emit, rust, cdylib, flag-on, fuzz, in-container, cargo

**Wrong**: verifying a `RY_LLVM_EMIT_IMPL_RUST=ON` build inside the local `docker/run.sh` dev image (`ry-linux-dev:latest`). Two failures: (a) the locally-built dev image can lag the published `ry-ci:llvm-21` base and predate the baked Rust toolchain — `cargo`/`rustc`/`/opt/cargo` don't exist and corrosion's configure fails with `rustc not found`; (b) even on a current image, `docker/run.sh`'s presets are all flag-OFF and `entrypoint.sh` pre-builds `cmake --preset <p>` (flag OFF) before dispatching any command, forcing a wasteful double build. macOS host can't substitute either: the `fuzz` preset rejects AppleClang (requires real Clang) and macOS-host fuzzing has libFuzzer/SDKROOT friction (#1865).

**Correct**: pull the current CI image and run it directly as root (GitHub runs container jobs as root; `/opt/cargo` is root-writable there — note it may not exist until cargo's first build creates it), source bind-mounted read-only, build into a **named volume** (not under the repo mount — sidesteps the macOS Mach-O leak guard and persists across chunked builds when one `cmake --build` exceeds the foreground time budget):

```bash
docker pull ghcr.io/t0k0sh1/ry-ci:llvm-21
docker run --rm -v "$PWD:/src:ro" -v ry-fuzz-rust-ci-build:/build \
  --entrypoint bash ghcr.io/t0k0sh1/ry-ci:llvm-21 -c '
    cmake -S /src -B /build -G Ninja -DCMAKE_BUILD_TYPE=Debug \
      -DENABLE_FUZZER=ON -DENABLE_ASAN=ON -DENABLE_UBSAN=ON \
      -DRY_LLVM_EMIT_IMPL_RUST=ON -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
    cmake --build /build --target fuzz_parser fuzz_json fuzz_utf8 fuzz_io_open'
```

`LLVM_SYS_211_PREFIX=/usr/local/llvm` is baked as ENV (preserved under `--entrypoint bash`), so llvm-sys/corrosion find the shared libLLVM the flag requires (the image builds LLVM with `LLVM_BUILD_LLVM_DYLIB=ON`). The cdylib lands at `/build/lib/libry_llvm_emit.so` (`file` → ELF shared object), proving the Rust side links on Linux.

**Why it recurs**: the fuzz CI job stays disabled, so this in-container run is the *only* validation of the flag-ON fuzz build (it is NOT covered by the test/asan/tsan rust matrix legs, which exercise `ry`/`ry_tests`, not the fuzz harnesses). Sub-issue 5 (the cutover) and any future cdylib work need the same procedure. If Docker disk fills mid-pull (`no space left on device`), `docker builder prune -a -f` reclaims shared build cache without touching images/volumes.
