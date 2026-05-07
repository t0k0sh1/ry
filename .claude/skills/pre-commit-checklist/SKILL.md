---
name: pre-commit-checklist
description: 作業完了前チェックリスト — ドキュメント反映 / CHANGELOG / rules+skills 更新 / 全テスト / ASan+UBSan / TSan / libFuzzer / バックグラウンドタスク / ラベル整理。Use when 作業完了前 / 実装完了 / 修正完了 / 機能追加完了 / 機能修正完了 / マージ前 / PR を出す前 / セルフ検証 / 動作確認 / サニタイザー実行 / テスト実行 / チェックリスト / 完了前に何をすべき のとき。フィーチャー開発の終盤では常に fire する。
allowed-tools: Bash
---

# Pre-commit Checklist

Mandatory checklist to run before declaring a task complete. Covers documentation, CHANGELOG, knowledge-base updates, full tests, sanitizers, fuzzing, background-task hygiene, and label policy.

> **Source-of-truth note**: previously in `AGENTS.md` §"作業完了前チェックリスト"; relocated by #1384.

タスクの完了前に、以下を必ず実行すること。

## 0. 変更種別 × スキップ可否マトリクス

実装着手前に変更ファイルセットを取得し、下表でスキップ可能なセクションを特定する。

```bash
git diff --name-only origin/main
```

> 上記コマンドは未コミット変更も含む（働き木 vs `origin/main`）。`origin/main` を fetch していない場合は先に `git fetch origin main` を実行する。`origin/main` が無い（detached HEAD / fresh clone）場合は `git diff --name-only HEAD` で代替する。

| 変更種別 | §1 Doc | §2 CHANGELOG | §3 全テスト | §3.5 Sanitizer | §3.6 libFuzzer |
|---|---|---|---|---|---|
| `.md` / `docs/` のみ | ✓ | skip | skip | skip | skip |
| `changelog.d/` のみ | skip | ✓ | skip | skip | skip |
| `.claude/` のみ | skip | skip | skip | skip | skip |
| `tests/` のみ | review | review | ✓ | ✓ | パーサ系のみ |
| parser/lexer/json/utf8/string 系を含む※ | ✓ | ✓ | ✓ | ✓ | ✓ |
| その他コード変更 | ✓ | ✓ | ✓ | ✓ | skip |

**凡例**: `✓` = 実行必須 / `skip` = 省略可（PR description に記録） / `review` = 担当者判断（judgment call）

※ 「parser/lexer/json/utf8/string 系」 = `src/(parser|lexer|runtime_json|runtime_utf8|runtime_string)*` または `include/ry/(parser|lexer|runtime_json|runtime_string).hpp` を変更した場合。**`runtime_string*` は `fuzz_json` と `fuzz_utf8` 双方の依存先**のため必ずこの行に該当する。

**補足**:

- 複数行に該当する変更は **各列の最も厳しい要求を採用** する（= `✓` 優先 / 次点 `review` / 最後 `skip`）。
- マトリクス対象外で **常時必須**: §2.5（rules/skills 更新, 該当時のみ） / §3.5.5（Static Analysis） / §3.6.5（tree-sitter Grammar Regression Check, 該当時のみ） / §3.7（background hygiene） / §4（Label Cleanup, no-op directive）。
- **`.md` / `docs/` のみの PR** では実質的に §4（Label Cleanup）が唯一の必須アクション。§1 は doc 編集自体で satisfied、§2-§3.6 は全てスキップ可、§3.7 は常時暗黙必須だが sanity check のみで実アクションを伴わない。
- **`changelog.d/` のみの PR** も同様に §4 が唯一の必須アクション。§2 は fragment 編集自体で satisfied、§1 / §3-§3.6 は全てスキップ可。
- マトリクスの `✓` のうち、自身を編集対象とするセクション（`.md`/`docs/` 行の §1、`changelog.d/` 行の §2）は **編集自体が satisfy 条件** であり、Skip if の bash も `skip` 判定を返す（自編集 = 完了）。

> **記録義務**: いずれかのセクションをスキップした場合、PR description（CHANGELOG fragment があればそちらでも可）に `Skipped §X — <理由>` を必ず記録すること。skip ログは将来の audit（regression 発生時の再現）に必要。

## 1. Documentation Update Check (English only)

> **Skip if** — 変更ファイルが `.claude/` または `changelog.d/` のみ（= ユーザーに見える変更が発生していない）:
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|changelog\.d/)' | head -1
> ```
>
> 出力が空ならスキップ可。スキップ時は PR description に `Skipped §1 — no user-visible change` を記録。

機能の**追加・変更・削除**を行った場合、**英語ドキュメントのみ**を更新する。

**判断基準**: ドキュメントに現在記載があるかではなく、**ユーザーが知るべき内容かどうか**で更新要否を判断する。新機能・挙動変更・新オプションなど、ユーザーに影響する変更は必ずドキュメントに反映すること。

対象と確認観点:

- **`docs/reference/`** — 型・演算子・制御構文・関数・コレクション・組み込み関数・エラーなどの仕様変更があれば該当ファイルを更新
- **`docs/README.md`** — ドキュメント目次の更新（新ページ追加時）
- **`README.md`** — 以下の内容に関わる変更があれば更新（詳細は docs/ に委譲）:
  - Features（言語機能の追加・変更）
  - Sample Code（新機能のデモに適したコード変更）
  - Installation（インストール方法の変更）
  - Usage（CLI コマンドの追加・変更）

反映が不要と判断した場合は、その理由を明示すること（内部リファクタリングのみ、テスト追加のみ、等）。

## 2. CHANGELOG Update Check

> **Skip if** — 変更ファイルが `.claude/` / `docs/` / トップレベル `*.md` / `changelog.d/` のみ（= `feat:` / `fix:` / 破壊的変更ではない）:
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|docs/|changelog\.d/|[^/]+\.md$)' | head -1
> ```
>
> 出力が空ならスキップ可。スキップ時は PR description に `Skipped §2 — no user-visible change` を記録。

ユーザーに影響のある変更（`feat:`, `fix:`, 破壊的変更）を行った場合、`changelog.d/` にフラグメントファイルを作成する。

**ファイル名**: `changelog.d/{issue番号}-{slug}.md`（例: `changelog.d/545-546-list-improvements.md`）

**内容**: `### Added` / `### Changed` / `### Fixed` / `### Removed` セクションのみを記述する。複数カテゴリにまたがる場合は 1 ファイルに複数セクションを含める。

```markdown
### Added

- Empty list literal `[]` is now supported with type annotation (#545)

### Fixed

- Some bugfix description (#545)
```

> **注意**: `CHANGELOG.md` を直接編集しないこと。フラグメントファイルはリリース準備時（現状は手動。`/release-orchestrator` 参照）に `scripts/assemble-changelog.sh` で CHANGELOG.md に集約される。

内部リファクタリング・テスト追加・CI 変更のみの場合はフラグメント作成不要。

## 2.5. .claude/rules/ + .claude/skills/ Update Check

今回の作業で以下のいずれかが発生した場合、`.claude/rules/` または `.claude/skills/` に追記する:

1. **新しい拒否ブランチ・検証チェックを追加した** — 将来の回帰を防ぐテストルール entry が既にあるか確認し、無ければ `.claude/rules/tests-rejection-tdd.md` に追加する:
   ```bash
   git diff origin/<base> -- 'src/**' 'include/**' \
     | grep -nE '^\+.*(codegenError|parserError|return std::nullopt)'
   ```
   hit した各拒否ブランチに対して、直接トリガーする回帰テストが存在することを確認する（合法ケースのテストでは代替不可）。
2. **実装中に非自明な落とし穴を発見した** — 例: 特定の LLVM API の罠、opaque pointer 周りの注意点、ARC retain/release の順序依存等。編集中ファイルの path-scope に該当する `.claude/rules/<name>.md` （例: ARC は `codegen-arc-cow.md`、type/metadata は `codegen-type-and-metadata.md`、runtime memory は `runtime-memory-safety.md`）に追加
3. **採用しなかった設計判断がある** — なぜその案を選ばなかったかを書いておくと、将来同じ検討を繰り返さずに済む。該当 path rule に追加
4. **コマンド・環境変数・シェル構文のミスをリカバリした** — 実行したコマンドが間違っていて失敗したが 2 回目以降で正解に辿り着いた場合、以下に該当するなら `.claude/skills/commands-environment-gotchas/SKILL.md` に追記する:
   - フラグや引数の組み合わせを間違えて失敗した
   - 必要な環境変数（`ASAN_OPTIONS`, `RY_ENV` 等）を忘れて失敗した
   - `cmake --preset` 名や path を間違えた
   - `gh` / `git` コマンドの subcommand / flag を間違えた
   - heredoc / quoting / escaping を間違えた

   修正が自明でなかった（＝ドキュメントに書いていない、直感に反する）場合のみ記録する。単なるタイポは不要。
5. **PR レビューで横断的なパターンを指摘された** — 複数 path で再発しうる論点は `.claude/skills/pr-review-recurring-patterns/SKILL.md` に追記する

追記不要と判断した場合は、その理由を明示する（純粋なバグ修正で再発しない、その PR 限りの local な指摘、等）。

## 3. Run All Tests

> **Skip if** — 変更ファイルが `.claude/` / `docs/` / トップレベル `*.md` / `changelog.d/` のみ（= 実行コードに影響なし）:
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|docs/|changelog\.d/|[^/]+\.md$)' | head -1
> ```
>
> 出力が空ならスキップ可。スキップ時は PR description に `Skipped §3 — no source code change` を記録。

全テストを実行して成功を確認する。

```bash
cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p
```

テストが失敗した場合は、原因を修正してから作業完了とすること。

## 3.5. Sanitizer Verification

> **Skip if** — §3 と同条件（実行コードに影響なし）:
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|docs/|changelog\.d/|[^/]+\.md$)' | head -1
> ```
>
> 出力が空ならスキップ可。スキップ時は PR description に `Skipped §3.5 — no source code change` を記録。

**ASan + UBSan**（メモリ安全性 + 未定義動作）:

```bash
cmake --preset asan && cmake --build build-asan && \
  ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry_tests && \
  ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry test -p
```

ASan / UBSan が検出した問題は原因を修正してから作業完了とする。これらのエラーを残したままコミットしてはならない。

**TSan**（スレッド安全性）:

```bash
cmake --preset tsan && cmake --build build-tsan && \
  TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry_tests && \
  TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry test -p
```

C++ TSan テスト (`ry_tests`) は required で、`ConcurrencySpecSuite` (= `tests/spec/concurrency.test.ry` stress test) を検証する。Ry self-test (`ry test -p`) は TSan `LargeMmapAllocator` CHECK 問題 (upstream #1716) により warn-only — ローカルでも CI でも C++ テストが clean run していれば本 PR スコープでは OK とする。race が検出された場合 (C++ / self-test どちらでも) は本 PR スコープ内で修正すること。既知 race として扱って先送りしてはならない。`/tsan-known-issues` の `LargeMmapAllocator` entry を参照。#630 の audit に無い新規 race パターンを発見した場合は新規 concurrency issue を起票し、再現テストを `tests/spec/concurrency*.test.ry` に追加する。

## 3.5.5. Static Analysis

CI の `lint` / `clang-tidy` / `scan-build` ジョブを push 前にローカルで再現する。設定・抑制ルール・誤検知対処の詳細は `/static-analysis-tools` に委譲。

**clang-tidy** (required):

```bash
find src -name '*.cpp' | xargs /opt/homebrew/opt/llvm@21/bin/clang-tidy -p build --quiet
```

**PCH 互換性**: macOS で `cmake --preset default` (Apple clang が PCH 生成) の後に LLVM clang-tidy を実行すると `PCH file built from a different branch` で失敗することがある。`build/` を削除して LLVM clang を CC/CXX に明示してから再 configure する (`SDKROOT` も必須):

```bash
rm -rf build
SDKROOT=$(xcrun --show-sdk-path) \
CC=/opt/homebrew/opt/llvm@21/bin/clang \
CXX=/opt/homebrew/opt/llvm@21/bin/clang++ \
    cmake --preset default && cmake --build build
```

詳細は `/commands-environment-gotchas` の PCH entry を参照。

**cppcheck** (required):

```bash
cppcheck --enable=warning,performance,portability --std=c++17 --error-exitcode=1 \
    --suppressions-list=.cppcheck-suppressions --inline-suppr \
    -i build -i build-asan -i build-tsan \
    -j "$(nproc 2>/dev/null || sysctl -n hw.logicalcpu)" --quiet \
    src/ include/
```

**scan-build** (warn-only — 強く推奨):

CI は `continue-on-error: true` で warn-only 運用中。ローカルでも警告即修正は不要だが、新規 null-dereference / use-after-free / division-by-zero が検出された場合は同 PR で対処することを強く推奨する。

scan-build は Homebrew LLVM 21 に同梱されているが PATH には入らない。フルパス `/opt/homebrew/opt/llvm@21/bin/scan-build` で呼び出す:

```bash
/opt/homebrew/opt/llvm@21/bin/scan-build \
    --use-analyzer=/opt/homebrew/opt/llvm@21/bin/clang \
    --use-cc=/opt/homebrew/opt/llvm@21/bin/clang \
    --use-c++=/opt/homebrew/opt/llvm@21/bin/clang++ \
    cmake --preset default

/opt/homebrew/opt/llvm@21/bin/scan-build \
    --use-analyzer=/opt/homebrew/opt/llvm@21/bin/clang \
    --use-cc=/opt/homebrew/opt/llvm@21/bin/clang \
    --use-c++=/opt/homebrew/opt/llvm@21/bin/clang++ \
    -o /tmp/scan-build-report --status-bugs cmake --build build
```

scan-build はビルドをラップするため `build/` の状態が変わる場合がある。以降のステップでビルドが必要なら §3 のコマンドで再ビルドする。

clang-tidy / cppcheck で失敗した場合は原因を修正してから作業完了とする。よくある失敗パターン (`performance-inefficient-string-concatenation` 等) と canonical workaround は `.claude/rules/build-warning-flags.md` を参照。

## 3.6. libFuzzer Fuzzing

> **Skip if** — 変更ファイルに parser/lexer/json/utf8/string 系を **含まない**:
>
> ```bash
> git diff --name-only origin/main | grep -E '(src/(parser|lexer|runtime_(json|utf8|string))|include/ry/(parser|lexer|runtime_(json|string)))' | head -1
> ```
>
> 出力が **空ならスキップ可**。出力があれば該当 fuzzer を実行する。スキップ時は PR description に `Skipped §3.6 — no parser/lexer/json/utf8/string change` を記録。
>
> **Fuzzer mapping**:
>
> - parser / lexer 系 → `fuzz_parser`
> - json 系 → `fuzz_json`（`runtime_string*` を変更した場合は `fuzz_utf8` も合わせて実行）
> - utf8 / string 系 → `fuzz_utf8`（`runtime_string*` を変更した場合は `fuzz_json` も合わせて実行）
> - **`tests/` のみ変更** で対象テストが parser/lexer/json/utf8/string 系の場合は該当 fuzzer のみ実行（judgment call）。不確かな場合は全 3 ターゲット実行を推奨。

**CI ジョブは無効のため、フィーチャーブランチで必ずローカル実行すること。** ハーネス要件・既知制限の詳細は `/libfuzzer-harness` を参照。

```bash
# macOS（build-fuzz/ が既にある場合はビルドをスキップ可）
SDKROOT=$(xcrun --show-sdk-path) CC=/opt/homebrew/opt/llvm@21/bin/clang CXX=/opt/homebrew/opt/llvm@21/bin/clang++ \
    cmake --preset fuzz && cmake --build build-fuzz

ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1 \
UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-fuzz/fuzz_parser -max_total_time=60 -rss_limit_mb=512 \
    -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser

ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1 \
UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-fuzz/fuzz_json -max_total_time=60 -rss_limit_mb=512 \
    -artifact_prefix=tests/fuzz/regressions/json/ tests/fuzz/corpus/json

ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1 \
UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-fuzz/fuzz_utf8 -max_total_time=60 -rss_limit_mb=512 \
    -artifact_prefix=tests/fuzz/regressions/utf8/ tests/fuzz/corpus/utf8
```

- 3 ターゲットすべてが 60 秒 exit 0 であることを確認する。
- crash が発見された場合は、現在の PR のコードが直接引き起こしたものは**同 PR で即座に修正**し、既存バグは `/scope-out-issue` の判定フローに従って別 issue を起票する。crash 入力は `tests/fuzz/regressions/<name>/` と `tests/fuzz/corpus/<name>/` の両方に保存すること。

## 3.6.5. tree-sitter Grammar Regression Check

> **Skip if** — 変更ファイルに tree-sitter グラマー / EBNF 仕様 / external scanner を **含まない**:
>
> ```bash
> git diff --name-only origin/main | grep -E '^(docs/grammar\.ebnf$|editor/tree-sitter/(grammar\.js$|src/))' | head -1
> ```
>
> 出力が **空ならスキップ可**。出力があれば再ビルド & 再インストールを実行する。スキップ時は PR description に `Skipped §3.6.5 — no tree-sitter grammar change` を記録。

§0 マトリクスとは独立した常時評価ステップ。`.md` / `docs/` のみの PR でも `docs/grammar.ebnf` を変更していれば発火する（マトリクスの「`.md` / `docs/` のみ」row には載せない理由）。

```bash
./editor/tree-sitter/build.sh
./editor/tree-sitter/install.sh --no-build
```

- `build.sh` 内部で `tree-sitter generate`（`grammar.js` から `parser.c` を再生成）と `tree-sitter build`（`parser.c` + `scanner.c` から `ry.so` をリンク）が両方成功し、`ry.so` が生成されることを確認する。`tree-sitter generate` が失敗すればグラマー側の構文ミスを意味する。
- `install.sh --no-build` が `ry.so` と `queries/*.scm` を Neovim parser ディレクトリへコピーすることを確認する。
- 現状 in-tree グラマーは Ry の全構文をまだカバーしていないため `tree-sitter parse` を実テストファイルに当てると ERROR ノードを含むことがある。これはグラマーの未完成部分であり、§3.6.5 のチェック対象ではない（コーパスベースの回帰テストは [#1633](https://github.com/t0k0sh1/ry/issues/1633) で追跡）。Neovim 等で `.ry` ファイルを開き、構文ハイライトに重大な regression が起きていないかを目視確認することは推奨。

## 3.7. Background Task Residual Check

作業完了を宣言する前に、自分が起動したバックグラウンドタスク・シェルが残存していないことを確認する。

- 全バックグラウンドタスクが完了していることを `BashOutput` / `TaskOutput` で確認する
- 孤立シェルの検出: `ps aux | grep -E "claude|zsh.*cat"` で自分のセッション由来のプロセスを探す
- 残存している場合は `TaskStop` で停止するか、`kill <pid>` でプロセスを終了させてから完了を宣言する
- ゾンビ化の典型例: `run_in_background=true` + heredoc による `zsh` + `cat` の stdin 待ち（詳細は AGENTS.md「Bash コマンドの実行ルール」参照）

## 4. Label Cleanup

**セルフ検証完了時点ではラベルを変更しない。** ラベルの切り替えは PR マージ後、`git-merge-pr` スキル Step 5 が `wip` 除去を自律的に処理する。個別コマンドを直接実行しない。
