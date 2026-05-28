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

## tree-sitter グラマーのビルド & インストール

`docs/grammar.ebnf` / `editor/tree-sitter/grammar.js` / `editor/tree-sitter/src/scanner.c` のいずれかを変更した PR では `ry.so` の再ビルドが必要。build/install コマンド・前提条件・落とし穴 (externals enum 順序 / `mark_end` / `valid_symbols` セマンティクス / highlights.scm) ・検証レシピは `.claude/skills/tree-sitter-grammar-editing/SKILL.md`（または `/tree-sitter-grammar-editing`）と `editor/tree-sitter/README.md`、セルフ検証手順は `/pre-commit-checklist` §3.6.5 を参照。`editor/tree-sitter/grammar.js` / `src/scanner.c` / `queries/*.scm` 編集時は同 skill が path-scoped rule 経由で自動 load される。

## コンパイラ警告フラグ

コンパイラ警告フラグの詳細は `.claude/rules/build-warning-flags.md` を参照。

## IR ゴールデンテスト

LLVM IR ゴールデンテストの記法・実行手順は `.claude/rules/codegen-llvm-ir-conventions.md` を参照。

## CI: container image (GHCR pre-baked)

CI Linux ジョブは pre-bake コンテナ (`ghcr.io/<owner>/ry-ci:llvm-21`、release.yml の glibc-old ジョブは immutable な `ry-ci-glibc-old:llvm-21-rev<N>` に pin) を使用 (#1505, #1508)。image build / バージョンバンプ / `rev<N>` tag / ロールバック / release pin 更新手順は `.claude/skills/ci-image-workflow/SKILL.md`（または `/ci-image-workflow`）を参照。macOS は Homebrew 継続。

## ナレッジベース (.claude/rules/ + .claude/skills/ + .claude/agents/ + KNOWLEDGE.md)

- **`.claude/rules/<name>.md`** — path-scoped rule。frontmatter `paths:` glob に一致するファイル編集時に自動 load
- **`.claude/skills/<name>/SKILL.md`** — context-triggered skill。`description:` にマッチした時に呼び出される
- **`.claude/agents/<name>.md`** — subagent 定義。`Agent` ツールの `subagent_type: <name>` で**独立コンテキスト**として起動する (skills は同一コンテキスト内で実行されるのと対照的)。`/<name>` スラッシュコマンドでは呼び出せない (skill ではなく agent のため)。Plan・設計・実装の批評など、メインの会話履歴から切り離して artifact のみを評価させたいタスクで使う。**並列化したい verification step は subagent を foreground で複数同時起動** (single message に multiple `Agent` tool calls)。Background 実行は禁止 (AGENTS.md §"Bash コマンドの実行ルール" 参照、#1947)。現状の catalog:
    - `.claude/agents/devils-advocate.md` — Plan / 設計レビュー用の批評エージェント
    - `.claude/agents/bug-forensics-analyst.md` — バグの起源判定 / git archaeology / テストギャップ分析 (`/triage-side-finding` Q3 経由で起動)
    - `.claude/agents/sanitizer-runner.md` — ASan+UBSan / TSan を独立 context で実行・分析する subagent (並列化用)
    - `.claude/agents/test-runner.md` — C++ ry_tests + Ry セルフテストを独立 context で実行・失敗解析する subagent (並列化用)
    - `.claude/agents/fuzzer-runner.md` — libFuzzer harness を独立 context で実行・crash 解析する subagent (並列化用)
    - `.claude/agents/pr-review-responder.md` — CodeRabbit / 人間レビュワー指摘の解析・返信生成・修正案作成を行う subagent
- **`KNOWLEDGE.md`** (リポジトリ root) — 未分類知見の暫定バッファ。rules / skills のどれにも該当 entry を持たない新規知見をここに蓄積し、安定後に rules / skills へ昇格させる。フォーマット・grep convention・外部参照ポリシー・「いつ書く」トリガー・昇格手順は `/knowledge-md-management` 参照

## ASan + UBSan（Address + UndefinedBehavior Sanitizer）

ローカル開発では `cmake --preset asan` で ASan + UBSan を同時有効化してテストを実行する。ビルドコマンド・実行時 env (`ASAN_OPTIONS=detect_container_overflow=0` / `UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1`) と各設定の根拠 (LLVM 混在 false positive 抑制 / `-fno-sanitize=vptr,function` の理由) は `.claude/skills/commands-environment-gotchas/SKILL.md`（または `/commands-environment-gotchas`）、セルフ検証手順は `/pre-commit-checklist` §3.5 を参照。

ASan または UBSan が検出した問題（メモリリーク、バッファオーバーフロー、use-after-free、未定義動作等）は必ず解消すること。サニタイザーエラーを残したままコミットしてはならない。

## TSan（ThreadSanitizer）

スレッド安全性の検証は TSan preset を使う。ビルドコマンド (`cmake --preset tsan`) / ASan-UBSan との排他性 (`build-tsan/` 隔離) / required vs warn-only ジョブ分割 / 既知の upstream バグ (LargeMmapAllocator / LLVM ORC teardown / signal-handler `siglongjmp`) は `KNOWLEDGE.md` の `## サニタイザー既知問題` セクション、セルフ検証手順は `/pre-commit-checklist` §3.5 を参照。

> 新しい race を導入した場合は同 PR 内で必ず修正すること。warn-only は TSan allocator バグの回避のみであり、実際の race 導入を許容しない。

## libFuzzer（カバレッジガイデッドファジング）

**CI ジョブは現在無効** — フィーチャーブランチのセルフ検証で必ず手動実行すること（`/pre-commit-checklist` §3.6 参照）。クラッシュ入力は `tests/fuzz/regressions/<name>/` と `tests/fuzz/corpus/<name>/` の両方に保存する。ハーネス要件・ビルドコマンド・既知制限は `.claude/skills/libfuzzer-harness/SKILL.md`（または `/libfuzzer-harness`）を参照。

## メモリ安全ルール（C++ ランタイム）

ランタイムメモリ安全ルール (禁止関数テーブル / `oom_abort(n)` / 外部入力の NULL チェック / CI lint 自動ブロック) は `.claude/rules/runtime-memory-safety.md` を参照。

## ワークフロー全体像

issue 確認 → ナレッジベース参照 (path-scoped rule は実装中も auto-load) → Plan モード (Task 1 = `/git-claim-issue` で `wip` 付与) → TDD 実装 → `/pre-commit-checklist` でセルフ検証 → 以降の git 操作 (commit / push / PR / merge) は「責務の分離」に従う。PR レビュー対応 → CI 確認 → push → マージを 1 コマンドで連鎖実行したい場合は `/git-close-pr` を使う (ブロッカー時は停止)。

## issue 起点の開発

- **リポジトリ**: `t0k0sh1/ry`
- **開始**: ユーザーが issue 番号 / URL を指定 → 内容把握 → Plan モード。「次の issue を探して」指示時は open issue 取得 (`wip` 除外)・バグ優先で候補提示 → 選択後に Plan モード
- **ラベル運用**: 付与・除去は必ずスキル経由 (`git-claim-issue` / `git-close-pr` Step 7 で `--add-label` / `--remove-label` 使用、既存ラベル保持)
- **issue 分割時のスコープ検証**: 派生 issue を起票・分離する判断は `/scope-decomposition` で対称性 (4 軸) / 分割理由 (3 分類) / 派生連鎖警戒 (3 段目以降) を確認する

## Plan モードのルール

- **開始条件**: 対象 issue の特定 (OPEN 状態を `gh issue view <n>` で確認)・リモートと最新化済み (`wip` 付与は Task 1 で実施するため事前付与不要)
- **実装計画の最初のタスク (固定)**:
  - **Task 1**: `/git-claim-issue` で issue に `wip` ラベルを付与する
  - フィーチャーブランチ作成は `/git-push` 初回呼び出し時に自動で行われるため、Plan に独立タスクとして含めない
- **実装計画のスコープ**: セルフ検証まで（git add / commit / push / PR 作成は含めない）
- **計画の抽象度（WHAT/HOW 分離）**: 計画は「何を達成するか」(WHAT) にとどめ、「どう実装するか」(HOW) は実装フェーズに委ねる。過剰な HOW 詳細が計画にあれば `/plan-rubric` で検出する
- **実装計画に必ず含めるもの**:
  - 最初のタスクが固定どおり (Task 1 = `/git-claim-issue`) であること。フィーチャーブランチ作成は `/git-push` 初回呼び出し時に自動で行われる
  - 編集予定 path の `.claude/rules/<name>.md` / `.claude/skills/<name>/SKILL.md` の関連エントリを参照したか (該当エントリがあれば Plan 本文に引用し、活用方法を明示する)
  - 仕様通りに実装できていることのセルフ検証タスク
  - 英語ドキュメント（README.md / docs）の更新（または変更不要の確認）
  - 用語変更・識別子 rename を含む場合: `/horizontal-sweep` を計画タスクに含める（4 ステップ手順は `.claude/skills/horizontal-sweep/SKILL.md`）
- **副次的発見への対応**: 「責務の分離」セクション「副次的発見への対応」に従う (`/triage-side-finding`)。`/triage-side-finding` Q4(b) で「別 issue 起票」と判定された場合のみ、実装計画内に「別 issue 起票」タスクを含める (Q1 再現困難 / Q2 ユーザー指示 → 即時修正と判定された場合は同 PR 内で対処するため計画タスク化不要)。**ただし起票の実行はユーザーの明示許可後** — Plan 内に「別 issue 起票」タスクを含める場合も、Claude Code は起票内容を提示するに留め、ユーザー許可を待つ (「責務の分離」§ユーザーが明示的に指示すること 参照)
- **TDD サイクルの分割禁止**: Red / Green / Refactor は Plan 上で個別タスクに分割せず、1 つの「TDD サイクル」タスクとしてまとめる（各ケース毎にサイクルを内部で回す）

## 内部挙動の解析に trace を使う

trace の使い方 (`--trace` / `--trace-out` / JSON Lines / 内部挙動・import 解決・JIT 実行の解析) は `.claude/skills/ry-trace/SKILL.md`（または `/ry-trace`）を参照。

## Bash コマンドの実行ルール

### Claude 起動の background 実行を全面禁止

Claude (メインエージェント) から起動するあらゆる background 実行を **全面禁止** する (#1947)。例外なし。

**禁止対象:**
- `Bash(run_in_background=true)` の使用 (用途・コマンドを問わず)
- shell の末尾 `&` による background 起動 (`cmake --build &` 等)
- `nohup` / `disown` / その他 detach 手段
- `Agent({run_in_background: true, ...})` (subagent background)
- ビルド (`cmake --build`) / テスト (`./build/ry_tests`) / fuzzer / 長時間処理も含めてすべて foreground 同期実行のみ

**並列化が必要な場合:**
single message に multiple `Agent` tool calls を入れて **subagent を foreground で複数同時起動** する。各 subagent は独立 context で foreground 実行、main agent は全戻り値の同期で待ち合わせる。`/pre-commit-checklist` の各 verification step (sanitizer / test / fuzzer / PR レビュー対応 等) には事前 subagent を `.claude/agents/` に整備済み — 使い分けは catalog (本 AGENTS.md §"ナレッジベース" 参照) で確認。

**Why:** background 実行は task_id 記録漏れリスクが構造的に存在する (Bash 経由は OS プロセステーブルに乗るため OS-level スキャンに頼らざるを得ず、別 Claude Code セッションを誤検出する — #1944)。subagent background は task framework 内で `TaskStop` 可能だが、それでも「使い分け判断ミス」のリスクは残る。バックグラウンド実行という概念を完全に消すことで認知コストとリスクを根本から排除する。並列化は subagent foreground で十分実現できる。

> **補足 (heredoc 入力の独立ルール)**: `./build/ry -c <<'EOF' ... EOF` のようなヒアドキュメント入力は必ず foreground 実行するか、ファイル入力 (`./build/ry script.ry`) に置き換える。background 禁止前から有効な独立ルールで、本節の禁止対象ではない (heredoc + background の hang リスクは歴史的事項)。

### タイムアウトの設定

- Bash ツールの `timeout` パラメータは foreground 実行でも必ず設定する (デフォルト 120,000 ms = 2 分は短い場合がある)
- ビルド系は `timeout: 300000`（5 分）、長時間テストでも `timeout: 600000`（10 分）を上限とする
- 上限を超える処理は script 分割 / ステップ化、または subagent foreground 並列化で対処 (background 化での回避は禁止)

### 一時ファイル作成の禁止

- **プロジェクト内 (リポジトリ作業ツリー配下) での一時ファイル作成は例外なく禁止**。`tmp_*.ry` / リポジトリ直下のスクラッチファイル / 検証用 `*.ry` / scratch script など、後で削除する前提のファイルを作業ツリー内に置いてはならない。ユーザーに「ファイルを作って消すのを繰り返すな」と何度も指摘されている事項
- Ry コードを ad-hoc 検証したい場合は `/ry-playground` skill（`.claude/skills/ry-playground/SKILL.md`）に従い `./build/ry -c <<'EOF' ... EOF` のヒアドキュメント形式で実行する（単一行・複数行いずれも可、single-quoted `'EOF'` でシェル展開抑止）。**ファイルを作成せずインラインで実行する**こと
- 仕様や挙動を pin したい (永続化したい) 場合は `tests/spec/*.test.ry` の本体に直接追記する（spec test は永続資産）
- C++ 側の検証は `tests/test_runtime_*.cpp` に追記してから `./build/ry_tests --gtest_filter=...` で実行する
- **`/tmp` の限定的例外**: GitHub CLI や外部ツールの仕様上、コマンドライン引数 / ヒアドキュメントだけでは渡せず、どうしてもファイルパスを渡す必要がある場合に限り `/tmp/` 配下のファイルを使ってよい。**ただし作成したファイルは削除しない・削除を試みない**こと (`rm /tmp/...` / `unlink` / cleanup trap を書かない)。OS の tmp cleanup に委ねる
- 「最終的に削除するファイル」を意図的に作成して回避策にすることは禁止 (プロジェクト内は完全禁止、`/tmp` でも削除コマンドを書かない)。検証手段としては `/ry-playground` (heredoc) / 既存テストファイル本体への追記 / `/tmp` (削除なし) のいずれかを選ぶ

## Git ブランチ運用ルール

- フィーチャーブランチは `main` から作成し、PR は `main` に向けて作成する。`main` への直接コミットは禁止
- **MUST (例外なし)**: フィーチャーブランチ名に文字列 `main` を含めてはならない。判定はブランチ名を小文字化し英字以外 (`/`, `-`, `_`, 数字、記号など) を全て除去した文字列に対して行い、その中に `m`,`a`,`i`,`n` がこの順で連続出現する場合は違反 (記号・大文字小文字・kebab セグメント境界での迂回不可、`domain-driven` のように自然な単語に偶然含まれる場合も禁止)。理由: `git branch | grep -i main` 等の検索ノイズとスクリプト判定の誤マッチを完全排除するため。違反した場合は `git branch -m <new>` で改名してから push すること
- フィーチャーブランチに main の最新を取り込む際は **`git rebase origin/main`** を使う (履歴の線形性を保つため)。merge commit による合流はしない。具体手順は `/git-push` / `/git-create-pr` / `/git-resolve-conflicts` を参照
- rebase 後の push は **`git push --force-with-lease`** を使う (二回目以降は SHA が書き換わるため force push が必要。`--force-with-lease` は remote の予期しない進行を検知して上書きをブロックする)。`fetch` と `push` の間で `git fetch` を再実行しない (lease 保護が緩む)
- 上記の rebase 方針はフィーチャーブランチへの main 取り込みに限る。`/preparing-for-release` での main 自体の更新は別系統で、`git pull --ff-only origin main` のまま (線形性が保証されているため変更不要)
- PR マージ前に、未追跡ファイルや未コミットの変更がないか確認すること。ある場合はマージ前にユーザーに報告し、コミットの要否を確認する
- `.serena/` ディレクトリに差分がある場合は、他の変更と一緒にコミットすること

## 責務の分離

### Claude Code が自律的に行うこと

- 実装
- テスト実行
- セルフ検証
- ドキュメント更新
- PR マージ後の `wip` ラベル除去（`git-close-pr` Step 7 に集約。マージ完了直後に自律実行、ユーザーの指示を待たない。issue クローズは `Closes #xx` キーワードにより GitHub が自動で行う。ただしこれは feature が main に入った記録であり、リリース完了ではない — 「リリースワークフロー」参照）

#### 副次的発見への対応

副次的な発見 (side finding) を検出したときの early short-circuit フロー (Q1 再現困難 CI 問題 → Q2 ユーザー明示指示 → Q3 `bug-forensics-analyst` → Q4 3 択判定 [即時修正 / 別 issue 起票 / ユーザー確認]) と Issue Creation Steps は `.claude/skills/triage-side-finding/SKILL.md`（または `/triage-side-finding`）参照。

#### 副次的発見の判断優先順位

副次的発見のトリアージ (Q1-Q4) において以下の優先順位を適用する。**本サブセクションのルールは「副次的発見の扱いに関する判断」にのみ適用** — 品質ゲート系ルール (サニタイザーエラー禁止 / TDD サイクル分割禁止 / `main` 直接コミット禁止 / `.serena/` 差分の同時コミット 等) は本サブセクションで override されない。

1. **ユーザー要望優先**: 副次的発見の扱いについてユーザーが明示的に方針指示した場合 (`/triage-side-finding` Q2 = Yes)、判定フローよりユーザー指示を優先する。skill / agent / advisor の判断を根拠にユーザー指示を覆そうとしてはならない。**ただし副次的発見の判断にのみ適用** — サニタイザーエラー禁止 / TDD サイクル分割禁止などの品質ゲートは override しない。
2. **再現困難問題の即時修正**: CI 検出の再現困難なメモリ破壊 / 並行性 race / fuzz crash 等 (`/triage-side-finding` Q1 = Yes に該当) は、起源判定 (regression vs pre-existing) より修正タイミングを優先する。再現中のウィンドウを逃さない原則。**ただし副次的発見の判断にのみ適用**。
3. **分析より修正優先**: Q1 / Q2 該当時は `bug-forensics-analyst` / advisor を呼ばない。意味は「即時修正を選んだあと不要な分析で時間を消費しない」であり、root cause 分析投資原則 (`/plan-rubric` 等) と衝突せず、Q3 経由の分析完了後の修正着手を妨げない。

> **用語注記**: `bug-forensics-analyst` は `.claude/agents/` 配下の subagent (§"ナレッジベース" の catalog 参照、backtick で表記)。advisor は Claude Code 組み込みの advisor tool あるいは外部レビュワーを指す汎用ロールで、`.claude/agents/` に独立ファイルを持たないため backtick なしで表記する。

### ユーザーが明示的に指示すること

- 外部レビュー（GitHub PR レビュー等）
- git add / commit / push
- PR 作成
- **新規 issue の起票 (`gh issue create`)** — Claude Code は起票内容 (理由 / 概要 / 粒度 / 解決確度 / ラベル案 / マイルストーン候補) を提示するに留め、ユーザーの明示許可 (「起票して」 / 「OK」等) を待つ。CI 失敗・サニタイザー検出・fuzz crash 等の repo 全体に影響する事故も口頭 (テキスト) 報告のみで、自律起票しない。詳細手順は `/git-create-issue` 参照
  - **例外**: `preparing-for-release` skill 経由 (Release prep / Release / Cleanup issue) は `/preparing-for-release <X.Y.Z>` のユーザー起動が起票許可を兼ねるため、この許可制の対象外

### PR レビュー対応

- **コミット/プッシュの徹底**: 修正内容がコミット・プッシュされていなければ PR に反映されない。レビュー対応の完了時に未コミットの変更があればユーザーに必ず伝え、コミット・プッシュを促すこと
- **Resolve 判断はレビュワーに委ねる**: CodeRabbit は返信内容を自動検証して自分で会話を Resolve するため、Claude Code が先回りで Resolve すると検証フローが機能しない。人間レビュワーのコメントも同様に、返信のみ行い Resolve 判断は委ねる
- **マージ前の未 Resolve チェック**: `git-close-pr` Step 6 が自動で未 Resolve 会話を検出し、残っていればマージを中止する

### PR レビューから得た学びの蓄積

PR レビューで受けた指摘のうち他 PR にも再発しうるパターンは追記する: path-scope に収まれば対応 `.claude/rules/<name>.md`、横断的なら `.claude/skills/pr-review-recurring-patterns/SKILL.md`。追記は自律的に行い、レビュー対応コミットと一緒にプッシュする。単発の local 指摘は追記不要。

## 作業完了前チェックリスト

タスクの完了前に必ず実行する手順 (ドキュメント反映 / CHANGELOG / rules+skills 更新 / 全テスト / ASan+UBSan / TSan / libFuzzer / バックグラウンドタスク / ラベル整理) は `.claude/skills/pre-commit-checklist/SKILL.md`（または `/pre-commit-checklist`）参照。

## リリースワークフロー

> **注意**: main へのマージ = mainline 取り込みのみ。リリース (タグ push → GitHub Release) は別工程。

リリース起動手順・タグ push 駆動の仕組み・マイルストーン close ポリシーの詳細は `.claude/skills/release-orchestrator/SKILL.md`（または `/release-orchestrator`）参照。feature-complete になったら `/preparing-for-release <X.Y.Z>` を起動する。
