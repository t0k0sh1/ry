# ry - 開発ガイドライン

Situational playbooks live in `.claude/skills/`; trigger them by description or by `/<skill-name>`.

## ビルド & テスト

```bash
cmake --preset default                                  # Ninja + LLVM（CMakePresets.json）
cmake --build build                                     # Ninja が自動並列ビルド
./build/ry_tests                                        # C++ テスト (GoogleTest)
./build/ry test -p                                      # Ry セルフテスト (全 *.test.ry)
./build/ry test tests/spec/<file>.test.ry               # 個別ファイル実行
```

> repo 内でビルドした `./build/ry` は `package.toml` の hidden 設定 `[paths]._dev_stdlib` に従ってプロジェクトローカルの `share/std/` を優先する。`RY_ENV=internal` は追加の isolation が必要な場合だけ使う。

## コンパイラ警告フラグ

内部ターゲット（`ry_lib`, `ry`, `ry_tests`, native libs）には厳格な警告フラグが有効化されている:

```text
-Wall -Wextra -Wpedantic -Wconversion -Wshadow
```

- 新規コードは警告ゼロを維持すること
- LLVM / GoogleTest のヘッダは `SYSTEM` include として扱われ、警告対象外
- `-Werror` は現時点では未導入（別 issue）
- フラグは `CMakeLists.txt` の `RY_WARNING_FLAGS` 変数で一元管理し、`target_compile_options(... PRIVATE ...)` で各ターゲットに適用

## FileCheck IR Golden Tests

`tests/filecheck/` ディレクトリに LLVM IR ゴールデンテストを配置する。`ry --emit-llvm-ir <file.ry>` で unoptimized IR を生成し、LLVM FileCheck ツールで宣言的にアサートする。

- **`ry --emit-llvm-ir <file>`**: parser → typecheck → codegen まで実行し、JIT 最適化なしで unoptimized LLVM IR を stdout に出力して終了（実行しない）
- **FileCheck の入手**: macOS は `brew install llvm@21`（`/opt/homebrew/opt/llvm@21/bin/FileCheck`）、Linux は `sudo apt-get install llvm-21-tools`（注意: llvm-mirror tarball に FileCheck は同梱されていないため apt からの取得が必要）
- **ローカル実行**:
  ```bash
  # 単一ゴールデン手動確認
  ./build/ry --emit-llvm-ir tests/filecheck/function_call.ry \
    | /opt/homebrew/opt/llvm@21/bin/FileCheck tests/filecheck/function_call.ry

  # CTest 経由（FileCheck を CMake が自動検出）
  ctest --test-dir build -L filecheck --output-on-failure
  ```
- **ゴールデン追加手順**: `tests/filecheck/<name>.ry` を作成し、先頭の `#` コメントとして `# CHECK: ...` / `# CHECK-NEXT: ...` / `# CHECK-NOT: ...` を記述する（Ry は `#` コメント構文を使用。`//` は Ry 構文エラー）
- **CHECK パターン指針**:
  - LLVM 17+ opaque pointer を前提 — 引数・alloca・load/store はすべて `ptr` 型
  - `--emit-llvm-ir` は unoptimized IR — mem2reg 後のレジスタ化とは異なり、alloca + store + load が残る
  - LLVM バージョンアップ時は goldens の再確認が必要
- CI の `filecheck` ジョブは全 PR で実行（`ry` のみビルドするため高速、`continue-on-error: true` で warn-only 運用中）

## CI: LLVM ツールチェーン (ミラー)

CI は `.github/actions/setup-llvm/` composite action 経由で LLVM を取得する（cache → GitHub Releases mirror → apt フォールバック順）。ミラー構築・バージョンバンプ手順・`--cleanup-tag` 禁止スコープの詳細は `.claude/skills/llvm-mirror-workflow/SKILL.md`（または `/llvm-mirror-workflow`）を参照。

## ナレッジベース (.claude/rules/ + .claude/skills/)

プロジェクトの long-term memory は 2 種類に分けて管理する:

- **`.claude/rules/<name>.md`** — path-scoped rule。frontmatter の `paths:` glob にマッチするファイルを編集するとき自動 load される。codegen / parser / runtime / tests / docs / build / CI などコードのトピック軸で分類済み
- **`.claude/skills/<name>/SKILL.md`** — context-triggered skill。frontmatter の `description:` に書かれたシナリオ（コマンドミスからの復旧、PR レビューでの再発パターン、TSan 既知バグなど）にマッチした時に呼び出される

Claude Code も人間コントリビュータも、これらを読む / 書く / 更新する。

- **読むタイミング**: Plan モードでも実装中でも、該当ファイルを編集すれば対応 rule が自動 load される。手動 grep が必要な場合は `grep -rnE '\*\*Tags\*\*:.*<keyword>' .claude/rules/ .claude/skills/`
- **書くタイミング**:
  1. PR レビュー対応後 — 他 PR にも再発しうる指摘は対応 rule または `.claude/skills/pr-review-recurring-patterns/SKILL.md` に追記
  2. 実装中 — 非自明な事実・落とし穴を発見したら、編集中ファイルの path-scope に該当する `.claude/rules/<name>.md` に追記
  3. Plan 作成中 — 採用しなかった設計判断の理由を該当 rule に追記
  4. コマンド・環境変数のミスをリカバリした時 — `.claude/skills/commands-environment-gotchas/SKILL.md` に追記
- **どこに書くか迷ったら**:
  - 編集中ファイルが特定の path に限定 → `.claude/rules/` の対応 file
  - 横断的なシナリオ・状況依存 → `.claude/skills/` の対応 SKILL.md（既存スキルが無ければ新設）
- **書き方**: 1 つの教訓につき 1 エントリ。各 entry は `### <heading>` で始め、`**Source**:` (issue / PR 番号と日付) / `**Tags**:` (keyword 列) / `**Rule**:` (本文) を必ず書く。既存 entry の format を参照
- **言語**: 英語推奨（CodeRabbit / Codex 等 AI レビュワーも読める）

## ASan + UBSan（Address + UndefinedBehavior Sanitizer）

ローカル開発では ASan と UBSan を同時に有効化してテストを実行する。`asan` preset は `ENABLE_ASAN=ON` と `ENABLE_UBSAN=ON` を両方設定する:

```bash
cmake --preset asan                                     # Debug + ASan + UBSan（build-asan/）
cmake --build build-asan                                # ビルド
ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-asan/ry_tests                               # C++ テスト
ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 \
    ./build-asan/ry test -p                             # Ry セルフテスト
```

> `detect_container_overflow=0` は、ASan なしでビルドされた LLVM ライブラリとの混在で生じる false positive を抑制するために必要。
>
> UBSan は `-fno-sanitize=vptr,function` を付与してビルドされる。前者はプロジェクトが `-fno-rtti` を使うため動作せず、後者は LLVM が C 風の関数ポインタキャストを多用するため false positive の温床になる。

ASan または UBSan が検出した問題（メモリリーク、バッファオーバーフロー、use-after-free、未定義動作等）は必ず解消すること。サニタイザーエラーを残したままコミットしてはならない。

## TSan（ThreadSanitizer）

スレッド安全性の検証には TSan ビルドを使う。TSan は ASan / UBSan と排他で、別ディレクトリ（`build-tsan/`）にビルドされる。ビルドコマンド・required vs warn-only ジョブ分割・upstream TSan allocator バグの詳細は `.claude/skills/tsan-known-issues/SKILL.md`（または `/tsan-known-issues`）を参照。

> 新しい race を導入した場合は同 PR 内で必ず修正すること。warn-only は TSan allocator バグの回避のみであり、実際の race 導入を許容しない。

## libFuzzer（カバレッジガイデッドファジング）

**CI ジョブは現在無効** — フィーチャーブランチのセルフ検証で必ず手動実行すること（`/pre-commit-checklist` §3.6 参照）。クラッシュ入力は `tests/fuzz/regressions/<name>/` と `tests/fuzz/corpus/<name>/` の両方に保存する。ハーネス要件・ビルドコマンド・既知制限は `.claude/skills/libfuzzer-harness/SKILL.md`（または `/libfuzzer-harness`）を参照。

## メモリ安全ルール（C++ ランタイム）

`include/ry/runtime_alloc.hpp` の安全なラッパーを使用すること。以下の関数は新規コードで直接呼び出してはならない:

| 禁止関数 | 代替 | 理由 |
|---------|------|------|
| `malloc` | `checked_malloc` | OOM 時の null 未チェック → segfault |
| `realloc` | `checked_realloc` | OOM 時の null 未チェック |
| `calloc` | `checked_malloc` + `memset` | OOM 時の null 未チェック |
| `strdup` | `checked_strdup` | OOM 時の null 未チェック |
| `strndup` | `checked_strndup` | OOM 時の null 未チェック |
| `malloc(count * sizeof(T))` | `checked_array_malloc(count, sizeof(T))` | 整数オーバーフロー → ヒープバッファオーバーフロー |

その他のルール:
- OOM 時は `oom_abort(n)` のように要求サイズを渡して即座に中断する（nullptr を返すパターンは使わない）
- 外部入力（HTTP リクエスト、JSON パース結果等）を `strcmp` / `strlen` に渡す前に NULL チェックを行う
- CI の `lint` ジョブが禁止関数の直接呼び出しを検出し、新規コードが追加された場合は自動でブロックする

## ワークフロー全体像

1. **issue 確認** — 対象 issue の内容を把握する
2. **issue クレーム** — `git-claim-issue` スキルを起動し、対象 issue に `wip` ラベルを付与する
3. **ナレッジベース参照** — 編集予定の path 周辺の `.claude/rules/<name>.md` と関連 `.claude/skills/<name>/SKILL.md` を一読する（path-scoped rules は実装中も auto-load される）
4. **Plan モード** — 実装計画を立てる
5. **実装** — TDD ベースで開発する
6. **セルフ検証** — テスト実行・ドキュメント反映・`.claude/rules/` または `.claude/skills/` 追記
7. **ユーザー指示を待つ** — 以降の git 操作（commit / push / PR 作成 / マージ）は「責務の分離」セクションに従う

## issue 起点の開発

- **リポジトリ**: `t0k0sh1/ry`
- **開始パターン**:
  - ユーザーが issue 番号または URL を指定 → GitHub MCP で issue を読み取り、内容を把握して Plan モードへ
  - ユーザーが「次の issue を探して」と指示 → open な issue を取得し（`wip` ラベル付きは除外）、バグ優先・効果の高い改善を優先して候補を提示、ユーザーが選択後に Plan モードへ
- **Plan モードとの接続**: issue の内容を仕様として Plan に反映する
- **ラベル運用**: 付与・除去は必ずスキル経由で行う（着手時は `git-claim-issue`、PR マージ後は `git-merge-pr` Step 5。どちらも `--add-label` / `--remove-label` を使い既存ラベルを保持する）

## Plan モードのルール

- **開始条件**: 対象 issue が特定されていること、対象 issue に `wip` ラベルが付与されていること（未付与の場合は `git-claim-issue` スキルを起動して付与してから進む）、かつリモートと最新化されていることを確認する
- **実装計画の最初のタスク**: `main` からフィーチャーブランチを作成（`git-branch-naming` スキル経由）
- **実装計画のスコープ**: セルフ検証まで（git add / commit / push / PR 作成は含めない）
- **実装計画に必ず含めるもの**:
  - 編集予定の path 周辺の `.claude/rules/<name>.md` と関連 `.claude/skills/<name>/SKILL.md` の関連エントリを参照したか（該当エントリがあれば Plan 本文に引用し、どう活用するかを明示する）
  - 仕様通りに実装できていることのセルフ検証タスク
  - 英語ドキュメント（README.md / docs）の更新（または変更不要の確認）
- **スコープ外の問題を発見した場合**: 「責務の分離」セクション「スコープ外の問題を発見した場合の対応ルール」に従う。実装計画内に「スコープ外 issue の起票」タスクを含める

## repo build と stdlib 解決

- repo 内でビルドした `./build/ry` / `./build-current/ry` は、この project の `package.toml` にある hidden 設定 `[paths]._dev_stdlib` を使って project local の `share/std` を参照する
- OS にインストールされた `ry` はこの hidden 設定を無視し、`~/.ry/share/std` を参照する
- `RY_ENV=internal` は追加の isolation 用であり、repo 開発時の通常動作に必須ではない

## 内部挙動の解析に trace を使う

- Ry の内部挙動、コンパイルの流れ、import 解決、JIT 実行、関数呼び出し、分岐選択を把握したい場合は `./build/ry --trace` を優先して使う
- trace は人間向けログではなく JSON Lines の機械可読ストリームとして扱う
- プログラムの標準出力そのものも確認したい場合は `--trace-out=<path>` を使って trace を別ファイルへ逃がす
- テストの解析では `./build/ry test --trace ...` を使う
- trace は冗長になりやすいため、挙動が不明確な場面や根拠が必要な場面で選択的に使う
- trace を使って解析した場合は、Plan や調査結果の要約に「trace で確認した事実」を明示する

例:

```bash
./build/ry --trace app/main.ry
./build/ry --trace-out=/tmp/ry-trace.jsonl app/main.ry
./build/ry test --trace tests/spec
echo 'print(1)' | ./build/ry --trace -c
```

## Bash コマンドの実行ルール

### `run_in_background=true` の使用制限

- ビルド（`cmake --build`）やテスト（`./build/ry_tests`）など、**有限時間で必ず終了することが明らかなコマンド**にのみ使用する
- 以下のパターンは **禁止**（コンテキスト圧縮後に socket FD が失われ、zsh + cat が stdin 待ちで永久に残存する）:

| 禁止パターン | 理由 |
|---|---|
| `run_in_background=true` + ヒアドキュメント (`<<'EOF'`) | `cat` が stdin socket を読み続ける |
| `run_in_background=true` + パイプ末尾の `cat` / `read` | 同上 |
| `run_in_background=true` + タイムアウト未指定 + 長時間コマンド | 圧縮後にプロセスが孤立する |

- 対話的入力を待つコマンド（`cat`、`read`、stdin 待ちになるパイプライン末尾）を `run_in_background` で起動してはならない
- `./build/ry -c <<'EOF' ... EOF` のようなヒアドキュメント入力は必ずフォアグラウンド実行するか、ファイル入力 (`./build/ry script.ry`) に置き換える

### タイムアウトの設定

- `run_in_background=true` を使う場合でも Bash ツールの `timeout` パラメータを必ず設定する
- ビルド系は `timeout: 300000`（5 分）、長時間テストでも `timeout: 600000`（10 分）を上限とする

## Git ブランチ運用ルール

- フィーチャーブランチは `main` から作成し、PR は `main` に向けて作成する。`main` への直接コミットは禁止
- PR マージ前に、未追跡ファイルや未コミットの変更がないか確認すること。ある場合はマージ前にユーザーに報告し、コミットの要否を確認する
- `.serena/` ディレクトリに差分がある場合は、他の変更と一緒にコミットすること

## 責務の分離

### Claude Code が自律的に行うこと

- 実装
- テスト実行
- セルフ検証
- ドキュメント更新
- PR マージ後の `wip` ラベル除去（`git-merge-pr` Step 5 に集約。マージ完了直後に自律実行、ユーザーの指示を待たない。issue クローズは `Closes #xx` キーワードにより GitHub が自動で行う。ただしこれは feature が main に入った記録であり、リリース完了ではない — 「リリースワークフロー」参照）

#### スコープ外の問題を発見した場合の対応ルール

スコープ外の問題を発見したときの判定フロー (Case 1/2/3) と issue 起票手順は `.claude/skills/scope-out-issue/SKILL.md`（または `/scope-out-issue`）参照。

### ユーザーが明示的に指示すること

- 外部レビュー（GitHub PR レビュー等）
- git add / commit / push
- PR 作成

### PR レビュー対応

- **コミット/プッシュの徹底**: 修正内容がコミット・プッシュされていなければ PR に反映されない。レビュー対応の完了時に未コミットの変更があればユーザーに必ず伝え、コミット・プッシュを促すこと
- **Resolve 判断はレビュワーに委ねる**: CodeRabbit は返信内容を自動検証して自分で会話を Resolve するため、Claude Code が先回りで Resolve すると検証フローが機能しない。人間レビュワーのコメントも同様に、返信のみ行い Resolve 判断は委ねる
- **マージ前の未 Resolve チェック**: `git-merge-pr` スキルが自動で未 Resolve 会話を検出し、残っていればマージを中止する

### PR レビューから得た学びの蓄積

PR レビュー（CodeRabbit / Copilot / 人間）で受けた指摘のうち、**他の PR にも再発しうる一般的なパターン**は `.claude/rules/` または `.claude/skills/` に追記する。単発のタイポ修正や、その PR 限りの local な指摘は追記不要。

- 該当ファイルが特定の path-scope（codegen / parser / runtime / tests / docs / build / CI）に収まる → 対応 `.claude/rules/<name>.md` に追記
- 横断的なレビューパターン（複数 path で再発する論点） → `.claude/skills/pr-review-recurring-patterns/SKILL.md` に追記

判断基準:

- 「次回同じミスをしないために記録すべき」と感じたら追記する
- 「この指摘は過去にも受けた気がする」と感じたら、既存 entry を更新する
- 追記はユーザの指示を待たず Claude Code が自律的に行う
- 追記は該当 PR のフィーチャーブランチ内で行い、レビュー対応コミットと一緒にプッシュする

## 作業完了前チェックリスト

タスクの完了前に必ず実行する手順 (ドキュメント反映 / CHANGELOG / rules+skills 更新 / 全テスト / ASan+UBSan / TSan / libFuzzer / バックグラウンドタスク / ラベル整理) は `.claude/skills/pre-commit-checklist/SKILL.md`（または `/pre-commit-checklist`）参照。

## リリースワークフロー

> **注意**: main へのマージ = mainline 取り込みのみ。リリース (タグ push → GitHub Release) は別工程。

リリース起動手順・タグ push 駆動の仕組み・マイルストーン close ポリシーの詳細は `.claude/skills/release-orchestrator/SKILL.md`（または `/release-orchestrator`）参照。feature-complete になったら `/preparing-for-release <X.Y.Z>` を起動する。
