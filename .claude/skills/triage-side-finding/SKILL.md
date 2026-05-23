---
name: triage-side-finding
description: 副次的な発見 (side finding) の扱いを判定するときの early short-circuit フロー (Q1 再現困難 CI 問題 → Q2 ユーザー指示 → Q3 bug-forensics-analyst → Q4 3 択判定 [即時修正 / 別 issue 起票 / ユーザー確認])。Use when 副次的な発見 / side finding / scope / ついでに直したい / 別 issue 起票 / 即時修正 / OPEN PR 依存 / fold-only / orphan issue 防止 / triage / 判定フロー のとき。実装中・セルフ検証中・PR レビュー対応中に副次的な問題が見つかった場合に呼び出す。**起源判定 (回帰 vs 既存バグ) そのものは `bug-forensics-analyst` agent の領域なので、本 skill は Q3 経由でのみ agent を呼ぶ。**
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*), Bash(gh api:*), Agent
---

# Triage Side Finding

副次的な発見 (side finding) を検出したときの中立的なトリアージハブ。3 択判定 — (a) 即時修正 / (b) 別 issue 起票 / (c) ユーザー確認 — に振り分ける。実装中・セルフ検証中・PR レビュー対応中に呼び出す。

> **Source-of-truth note**: previously in `AGENTS.md` §"責務の分離 > スコープ外の問題を発見した場合の対応ルール"; relocated by #1384. 「現在 OPEN な PR への依存」ゲート (Q4(b) Step 1 + fold-only ルール) は #1694 で追加。Q1-Q4 early short-circuit フローへの再設計と `bug-forensics-analyst` 統合は #1752 で導入。

## Design intent

本 skill は **副次的発見を「別 issue 起票」方向に押し流さない** ことを設計目標とする。旧 `scope-out-issue` skill は名前と Case 1/2 default の構造から「スコープアウトする手順」のバイアスを生み、(a) CI 検出の再現困難な問題 (ASan / TSan / UBSan / libFuzzer crash) が後追いで再現できず塩漬けになる、(b) ユーザーが「今直す」と判断してもルール順守側に揺り戻される、という失敗パターンを生んでいた (#1752)。

新フローは Q1 / Q2 で **agent / advisor を呼ばずに即時修正へ short-circuit** させ、再現中のウィンドウを逃さない。Q3 / Q4 は Q1 / Q2 で確定しなかった「分析を要するケース」に限定する。

## Decision Flow

副次的発見を検出したら、以下の順序で判定する。**前段で確定したら後段の処理 (`bug-forensics-analyst` / advisor 呼び出し含む) は実行しない**。

### Q1: 今再現中で、後で再現困難になる問題か?

CI のサニタイザー (ASan / TSan / UBSan) / libFuzzer crash / 並行性 race / 確率的なメモリ破壊など、ローカル環境では確率的にしか再現しないが今この瞬間に観測できているものを指す。

**Falsifiable な判定基準** (いずれかに該当すれば Q1 = Yes):

- CI で検出されたサニタイザー / libFuzzer crash で、ローカル開発環境 (`build-asan/` / `build-tsan/` / `build-fuzz/`) で **3 回試行しても確実に再現しない**
- TSan が race を検出したが、再実行で出ない (TSan の本質的に確率的な性質)
- libFuzzer が新規 crashing input を生成したが、コーパス未保存のため再現難
- CI 上のみで観測され、再実行可能性に保証がない (CI ジョブ retention が短い、ログのみ残存等)

逆に **以下は Q1 = No** (Q2 以降へ進む):

- ローカルで確実に再現でき、いつでも観察可能なバグ
- production 環境のクラッシュレポートで再現手順が手元にある
- 既知の再現性のあるテストケースで露呈した既存バグ

→ **Q1 = Yes**: **即時修正に振る。`bug-forensics-analyst` / advisor は呼ばない**。
   - 再現中のウィンドウを最優先する設計上の判断であり「ついでに直す」ではない (`plan-rubric` の「ついでに直す禁止」と衝突しない、`/plan-rubric` 参照)
   - 必要に応じてクラッシュ入力 / スタックトレースを `tests/fuzz/regressions/` 等に永続化してから修正する
   - フィーチャーブランチに含めて同 PR 内で対処する
→ **Q1 = No**: Q2 へ進む。

### Q2: ユーザーが明示的に方針を指示しているか?

ユーザーから「これは今のブランチで直そう」「別 issue にして」「ここで対処」などの方針指示が出ているか。

**ただし指示に従う前に、必ず以下を 1 メッセージで報告する** (informed consent 化):

- **What**: 修正対象 (どのファイル / 関数 / 何を変える)
- **Where**: 影響範囲 (関連するモジュール / テスト / 依存)
- **推定差分規模**: 概ねの行数 / 触るファイル数
- **依存リスク**: 現 PR scope を実質的に変えないか、OPEN PR への依存がないか

ユーザーがこれを承知した上で指示するなら従う。明らかな品質ゲート違反 (サニタイザーエラーを残す / TDD サイクル分割禁止違反など) は **Q2 の対象外** — それらは AGENTS.md の品質ゲート系ルールが優先する (`AGENTS.md` 「副次的発見の判断優先順位」参照)。

→ **Q2 = Yes**: **ユーザー指示に従う。`bug-forensics-analyst` / advisor は呼ばない** (ルール順守側で「今直す」判断を覆さない)。
→ **Q2 = No**: Q3 へ進む。

### Q3: `bug-forensics-analyst` agent で起源分析

Q1 / Q2 で確定しなかった場合のみ実行する。

```
Agent tool: subagent_type='bug-forensics-analyst'
```

agent は以下を出力する (詳細は `.claude/agents/bug-forensics-analyst.md`):

- 起源判定: regression (現 PR で導入) / pre-existing (現 PR が露呈させたのみ) / pre-existing exposed (条件付きで現 PR が引き金)
- 影響範囲 (どのコードパスが触られるか)
- 既存テストカバレッジのギャップ
- 修正方針の Recommendation (修正コードは書かない)

> **呼び出し元の責務**: agent invoke 時、本 skill の呼び出し元 (= Claude Code 本体) は agent に渡るコンテキスト (PR diff / blame 範囲 / 失敗したテスト出力 等) を会話履歴上で確認可能な状態に保つ。現状の skill / agent はいずれも同一会話セッションから情報を引けるため、明示的なコンテキスト転送なしで動作する想定。

### Q4: 3 択判定

Q3 の分析結果と PR サイズ規律 (`plan-rubric` の 1 issue ≒ 1 PR) を踏まえ、以下から 1 つを選ぶ:

**(a) 即時修正**

- 修正 diff が現 PR の本来 scope と関連し、追加 diff が現 PR と同程度以上にならない
- regression と判定された (現 PR が直接引き起こした)
- 修正方法が単純で副作用リスクが低い

→ フィーチャーブランチに含めて同 PR 内で対処する。

**(b) 別 issue 起票**

- 別関心事 (parser bug と codegen 改善が同時に見つかった等)
- 現 PR scope を実質的に変える規模
- pre-existing で、現 PR の関連は薄い

→ 後続「Issue Creation Steps」へ進む。

**(c) ユーザー確認**

- 設計判断が分かれる (どの fix approach を取るか複数案ある)
- regression / pre-existing の境界が曖昧
- 規模が中間で (a)/(b) どちらも合理的に見える

→ ユーザーに **What / Where / Context / 推定 size / 推奨案 (理由付き)** を提示し、回答を待つ。

## Issue Creation Steps

Q4 = (b) と判定した場合に実行する。

> **重要**: 新規 issue の起票 (`gh issue create`) は **ユーザーの明示許可必須** (AGENTS.md §責務の分離「ユーザーが明示的に指示すること」)。Step 2 で起票内容を提示し、許可を得てから Step 3 以降を実行する。Claude Code は許可なしに `gh issue create` を実行してはならない。

### Step 1: 依存 PR を確認し、fold または escalate する

このルールは **fold-only** である。「依存 PR がマージされてから再起票する」案は**採らない** — 着手不可な orphan issue は backlog 上で「open かつ着手可能」と区別がつかず tracker の signal を低下させ、依存関係も暗黙化してしまうため。

1. **該当 OPEN PR を特定する**:

```bash
gh pr list --state open --json number,title,headRefName
gh pr view <number> --json files,state,headRefName --jq '{state, branch: .headRefName, files: [.files[].path]}'
```

2. **依存判定**:
   - 該当コードパスが現在 OPEN な PR のフィーチャーブランチでしか存在しない (= main にはまだない) → **依存あり** (fold または escalate へ進む)
   - 該当コードパスが `main` 上に既に存在する / 独立して作業可能 → **依存なし** (Step 2 へ進む)

3. **fold する (PR 作者と相談して scope 拡大を提案する)**: 当該 PR の作者と相談し、発見した作業項目を当該 PR のスコープに追加する形で取り込む。一方的にコミットを push するのではなく、PR comment 等で scope creep を提案して同意を得るのが基本。自分が PR 作者であれば自分のフィーチャーブランチに追加コミットしてよい。

4. **escalate する (fold 困難時)**: 以下のいずれかに該当する場合は fold せず、ユーザーに **What** / **Where** / **推定 size** を提示して判断を仰ぐ。
   - 追加作業が当該 PR の本来 scope を実質的に変える (設計判断の見直しを要する等)
   - 追加 diff が当該 PR の既存 diff と同程度以上になる
   - 当該 PR が review 完了済み・マージ直前で再 review コストが高い

ユーザーは独立 issue 起票 / 当該 PR の scope 拡大 / 別フィーチャーブランチ作成等の選択肢から判断する。

**Motivating example (#1692)**: PR #1693 (`verifyCalledWith` v1) が未マージのまま、その v2 拡張として #1692 が起票された。#1692 は PR #1693 で導入される `__ry_mock_store_arg` / kind tag enum / `mockArgEqual` / `__ry_mock_count_matching_calls` を**前提**とするため、PR #1693 マージ前は実装に進めない orphan issue となった。本ゲートにより、今後は同種の起票は fold (PR #1693 への scope 追加) または escalate に振り分けられる。

### Step 2: ユーザー許可確認 (起票内容のプレビュー提示)

Step 1 で「依存なし」と判定された場合のみ実行する (fold / escalate ケースは Step 1 で完結)。

ユーザーに以下の 6 項目を提示し、明示の起票許可 (「起票して」 / 「OK」等) を待つ。**許可が得られるまで `gh issue create` を実行しない**。

| 項目 | 内容 |
|---|---|
| **起票理由** | なぜこの発見を現 PR で対応せず別 issue にするのか (関心事分離 / scope 規模 / pre-existing 等、Q4(b) の判定根拠) |
| **概要** | 1〜3 行の要点 (本文ではなく要旨) |
| **粒度の妥当性** | 1 issue ≒ 1 PR に収まるか、Step 4 の分割対象になりうるか |
| **解決確度** | High (再現手順あり) / Medium (仮説 + 検証手順あり) / Low (仮説のみ) のいずれかと根拠 |
| **ラベル案** | 自動判断 (例: `bug` / `enhancement` / `documentation` / `refactor` 等)。**最低 1 個は必須、空にしない** |
| **マイルストーン候補** | `gh api repos/t0k0sh1/ry/milestones?state=open` で現在 open な milestone を取得し、現開発バージョンの milestone を「候補」として提示。**自動継承しない** — ユーザーが採用 / 別指定 / 未設定を判断する |

提示例:

```text
別 issue 起票の許可をお願いします:

- 起票理由: パーサ修正 (現 PR) と直交する codegen 側のエラーメッセージ改善で、関心事が異なる
- 概要: <module>.<fn> でエラー時の line/column が出ない (仮説あり、修正方針も見えている)
- 粒度: 1 PR で収まる規模 (推定 50〜100 行)
- 解決確度: High (ローカルで再現済み、修正パスも特定済み)
- ラベル案: bug, enhancement
- マイルストーン候補: v0.0.25 (現開発バージョン) / 未設定も可

起票してよろしいですか?
```

ユーザーが許可したら、その回答 (採用 milestone を含む) を Step 3 以降の入力とする。許可されなかった場合は起票せず終了する。

### Step 3: 重複を確認する

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

重複があれば追加 context を comment で添え、必要に応じて `gh issue edit <number> --milestone "<title>"` でマイルストーンを揃える (milestone 変更もユーザー確認後)。Step 4 を skip して Step 6 へ進む。

### Step 4: 分割を判断する

複数の独立した関心事を含む、または見積もりが概ね 1 PR を超える場合は別 issue に分割する。**1 issue ≒ 1 PR** を目標とする。

例: parser bug と codegen 改善が同時に見つかった → 2 issue / 同じ runtime 関数の正しさ修正と性能改善 → 2 issue。

分割すると判断した場合の **分割理由の分類 (機能境界 / 依存関係 / 規模) と対称性チェック (typed↔any / wrap↔unwrap / read↔write / base↔derived)、3 段目派生の警戒** は `/scope-decomposition` REQ-1〜3 を参照。

分割が必要になった場合は分割案 (各 issue のタイトル・粒度) を再度ユーザーに提示し、改めて許可を得る (Step 2 と同等)。

### Step 5: issue を作成する

Step 2 で得た許可と milestone 決定を入力に、`/git-create-issue` skill 経由で起票する。コマンド本体と本文テンプレートは `/git-create-issue` に集約されている。

**重複した許可確認を避けるため**: `/git-create-issue` Step 1 は許可ゲートだが、本 skill 経由で呼ぶ場合は Step 2 で同等の 6 項目プレビューと承認が完了しているため、`/git-create-issue` 側は **Step 1 を skip し Step 2 (重複確認) から開始**する。Step 2 で承認された 6 項目 (起票理由 / 概要 / 粒度 / 解決確度 / ラベル案 / 採用 milestone) をそのまま `gh issue create` の入力にする。`/git-create-issue` 側にも同等の skip 条件が明記されている。

### Step 6: 報告する

ユーザーに issue 番号・タイトル・設定したマイルストーンを報告する。複数 issue を起票したらすべて列挙する。fold / escalate を選択した場合はその旨と対象 OPEN PR 番号を報告する。
