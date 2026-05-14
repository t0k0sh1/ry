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
→ Error: `'encodeBytes' not found in module 'base64'` (verbatim shape per `src/module_loader.cpp` after #1483; the function name was also renamed `encode_bytes` → `encodeBytes` in #1415)

**Correct**: `./build/ry test tests/spec/base64.test.ry` (or any `.ry` inside the repo)

**Why**: `./build/ry` resolves the stdlib path via `package.toml`'s hidden `[paths]._dev_stdlib` key, which requires a `package.toml` somewhere in the ancestor directory chain. Files under `/tmp/` have no such ancestor, so the module loader falls back to `~/.ry/share/std` (the globally-installed stdlib), which does not contain the newly added declarations. The trace event to look for: `"resolved_path":"/Users/.../.ry/share/std"` instead of `"resolved_path":"/Users/.../Workspace/ry-2/share/std"`.

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

**How to apply**: Use `git-claim-issue` skill for `wip` attachment (enforces `--add-label` internally). Use `git-merge-pr` Step 5 for `wip` removal (enforces `--remove-label` internally). Never call `gh issue edit --label` directly for additive changes.

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
**Tags**: skill, allowed-tools, claude-code, ci-investigate, review-feedback

**Rule**: Every Bash command that a SKILL.md step instructs the agent to run must be covered by an entry in `allowed-tools`. A common pitfall is listing only `gh pr:*`/`gh run:*`/`git branch:*` while the skill body also calls `cmake`, `clang-tidy`, `cppcheck`, `scan-build`, `find`, etc. At runtime the agent will be blocked from running those uncovered commands, silently breaking the step.

When the reproduction command set is open-ended (e.g. "run the CI job's corresponding local command"), use `Bash` (unrestricted) rather than a long enumeration of prefixes that will grow stale.

**How to verify**: grep the skill body for bare Bash commands not covered by the `allowed-tools` line.

---

### `gh run list --branch` returns all runs on a branch, not just the PR head commit

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: github-actions, gh-cli, ci-investigate, review-feedback, gotcha

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

`run_in_background=true` でビルドを実行する場合も同じ罠が当てはまる: BashOutput の `tail -N` ライクな表示は同様に exit code を見落としがち。Bash ツールの戻り値の `<exit_code>` 行を必ず確認する。

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

**Wrong**: After modifying `src/jit_runner.cpp`, run `cmake --build build --target ry_tests` to retest. Subprocess tests like `DeprecatedWarningsTest` keep failing as if no source change had taken effect.

**Correct**: `cmake --build build` (no `--target`) — Ninja will relink both the test binary **and** the `ry` executable.

**Why**: Tests under `tests/test_emit_llvm_ir.cpp`, `tests/test_help.cpp`, and `tests/test_deprecated_warnings.cpp` use a `fork()` + `execv(RY_BINARY_PATH, ...)` pattern (the `RunResult` / `runRy()` helper). They invoke the on-disk `./build/ry` binary, **not** the `runRySource()` C++ symbol linked into `ry_tests`. The `ry_tests` target depends on `ry_lib.a` (the static library), but it does **not** depend on the `ry` executable. So `--target ry_tests` skips relinking `./build/ry`, and the subprocess test forks an old binary that still has the pre-fix behavior. The failure mode is confusing because in-process tests on the same code change pass — only the subprocess tests see a stale binary. Always rebuild without `--target` (or run `--target ry_tests --target ry`) when subprocess tests are in scope.
