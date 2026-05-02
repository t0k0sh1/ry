# ry - 開発ガイドライン

Situational playbooks live in `.claude/skills/`; trigger them by description or by `/<skill-name>`.

> **用語（v0.0.17）**: 定義は `docs/reference/glossary.md`（#1480）。`module` = `from xxx import ...` の単位; `package` は将来予約; `effectivePackage` / `RY_REGISTER_STDLIB_PACKAGE` / `__ry_<symbol>` は legacy 命名のまま据え置き。

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

コンパイラ警告フラグの詳細は `.claude/rules/build-warning-flags.md` を参照。

## IR ゴールデンテスト

LLVM IR ゴールデンテストの記法・実行手順は `.claude/rules/codegen-llvm-ir-conventions.md` を参照。

## CI: container image (GHCR pre-baked)

CI Linux ジョブは pre-bake コンテナ (`ghcr.io/<owner>/ry-ci:llvm-21`、release.yml の glibc-old ジョブは `ry-ci-glibc-old:llvm-21`) を使用 (#1505)。image build / バージョンバンプ / `rev<N>` tag / ロールバック手順は `.claude/skills/ci-image-workflow/SKILL.md`（または `/ci-image-workflow`）を参照。macOS は Homebrew 継続。

## ナレッジベース (.claude/rules/ + .claude/skills/)

- **`.claude/rules/<name>.md`** — path-scoped rule。frontmatter `paths:` glob に一致するファイル編集時に自動 load
- **`.claude/skills/<name>/SKILL.md`** — context-triggered skill。`description:` にマッチした時に呼び出される
- **読む**: 該当ファイルを編集すれば対応 rule が自動 load。手動 grep: `grep -rnE '\*\*Tags\*\*:.*<keyword>' .claude/rules/ .claude/skills/`
- **書く**: 1 つの教訓 = 1 エントリ。`### <heading>` + `**Source**:` + `**Tags**:` + `**Rule**:` 形式。path 限定なら `.claude/rules/`、横断的なら `.claude/skills/`。英語推奨
- **いつ書く**: PR レビュー対応後 (再発しうる指摘) / 実装中 (非自明な事実) / Plan 中 (採用しなかった設計判断) / コマンドミスのリカバリ時

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

> `detect_container_overflow=0` は ASan なし LLVM ライブラリとの混在 false positive 抑制のため。UBSan は `-fno-sanitize=vptr,function` でビルド (`-fno-rtti` および LLVM の C 風関数ポインタキャスト対策)。

ASan または UBSan が検出した問題（メモリリーク、バッファオーバーフロー、use-after-free、未定義動作等）は必ず解消すること。サニタイザーエラーを残したままコミットしてはならない。

## TSan（ThreadSanitizer）

スレッド安全性の検証には TSan ビルドを使う。TSan は ASan / UBSan と排他で、別ディレクトリ（`build-tsan/`）にビルドされる。ビルドコマンド・required vs warn-only ジョブ分割・upstream TSan allocator バグの詳細は `.claude/skills/tsan-known-issues/SKILL.md`（または `/tsan-known-issues`）を参照。

> 新しい race を導入した場合は同 PR 内で必ず修正すること。warn-only は TSan allocator バグの回避のみであり、実際の race 導入を許容しない。

## libFuzzer（カバレッジガイデッドファジング）

**CI ジョブは現在無効** — フィーチャーブランチのセルフ検証で必ず手動実行すること（`/pre-commit-checklist` §3.6 参照）。クラッシュ入力は `tests/fuzz/regressions/<name>/` と `tests/fuzz/corpus/<name>/` の両方に保存する。ハーネス要件・ビルドコマンド・既知制限は `.claude/skills/libfuzzer-harness/SKILL.md`（または `/libfuzzer-harness`）を参照。

## メモリ安全ルール（C++ ランタイム）

ランタイムメモリ安全ルール (禁止関数テーブル / `oom_abort(n)` / 外部入力の NULL チェック / CI lint 自動ブロック) は `.claude/rules/runtime-memory-safety.md` を参照。

## ワークフロー全体像

issue 確認 → `/git-claim-issue` で `wip` 付与 → ナレッジベース参照 (path-scoped rule は実装中も auto-load) → Plan モード → TDD 実装 → `/pre-commit-checklist` でセルフ検証 → 以降の git 操作 (commit / push / PR / merge) は「責務の分離」に従う。

## issue 起点の開発

- **リポジトリ**: `t0k0sh1/ry`
- **開始**: ユーザーが issue 番号 / URL を指定 → 内容把握 → Plan モード。「次の issue を探して」指示時は open issue 取得 (`wip` 除外)・バグ優先で候補提示 → 選択後に Plan モード
- **ラベル運用**: 付与・除去は必ずスキル経由 (`git-claim-issue` / `git-merge-pr` Step 5 で `--add-label` / `--remove-label` 使用、既存ラベル保持)

## Plan モードのルール

- **開始条件**: 対象 issue の特定・`wip` ラベル付与済み (未付与なら `git-claim-issue` スキルを起動)・リモートと最新化済み
- **実装計画の最初のタスク**: `main` からフィーチャーブランチを作成（`git-branch-naming` スキル経由）
- **実装計画のスコープ**: セルフ検証まで（git add / commit / push / PR 作成は含めない）
- **実装計画に必ず含めるもの**:
  - 編集予定 path の `.claude/rules/<name>.md` / `.claude/skills/<name>/SKILL.md` の関連エントリを参照したか (該当エントリがあれば Plan 本文に引用し、活用方法を明示する)
  - 仕様通りに実装できていることのセルフ検証タスク
  - 英語ドキュメント（README.md / docs）の更新（または変更不要の確認）
  - 用語変更・識別子 rename を含む場合: `/horizontal-sweep` を計画タスクに含める（4 ステップ手順は `.claude/skills/horizontal-sweep/SKILL.md`）
- **スコープ外の問題を発見した場合**: 「責務の分離」セクション「スコープ外の問題を発見した場合の対応ルール」に従う。実装計画内に「スコープ外 issue の起票」タスクを含める
- **TDD サイクルの分割禁止**: Red / Green / Refactor は Plan 上で個別タスクに分割せず、1 つの「TDD サイクル」タスクとしてまとめる（各ケース毎にサイクルを内部で回す）

## repo build と stdlib 解決

repo 内 `./build/ry` / `./build-current/ry` は `package.toml` の hidden 設定 `[paths]._dev_stdlib` で project-local の `share/std/` を参照。OS インストール版は `~/.ry/share/std` を参照。`RY_ENV=internal` は追加の isolation 用であり、repo 開発時の通常動作に必須ではない。

## 内部挙動の解析に trace を使う

trace の使い方 (`--trace` / `--trace-out` / JSON Lines / 内部挙動・import 解決・JIT 実行の解析) は `.claude/skills/ry-trace/SKILL.md`（または `/ry-trace`）を参照。

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

PR レビューで受けた指摘のうち他 PR にも再発しうるパターンは追記する: path-scope に収まれば対応 `.claude/rules/<name>.md`、横断的なら `.claude/skills/pr-review-recurring-patterns/SKILL.md`。追記は自律的に行い、レビュー対応コミットと一緒にプッシュする。単発の local 指摘は追記不要。

## 作業完了前チェックリスト

タスクの完了前に必ず実行する手順 (ドキュメント反映 / CHANGELOG / rules+skills 更新 / 全テスト / ASan+UBSan / TSan / libFuzzer / バックグラウンドタスク / ラベル整理) は `.claude/skills/pre-commit-checklist/SKILL.md`（または `/pre-commit-checklist`）参照。

## リリースワークフロー

> **注意**: main へのマージ = mainline 取り込みのみ。リリース (タグ push → GitHub Release) は別工程。

リリース起動手順・タグ push 駆動の仕組み・マイルストーン close ポリシーの詳細は `.claude/skills/release-orchestrator/SKILL.md`（または `/release-orchestrator`）参照。feature-complete になったら `/preparing-for-release <X.Y.Z>` を起動する。
