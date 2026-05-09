---
name: scope-out-issue
description: スコープ外の問題を発見したときの判定フロー (Case 1/2/3) と GitHub issue 起票手順 (Step 1-6)、および「現在 OPEN な PR への依存」ゲート (Case 2 で必須・fold-only ルール)。Use when スコープ外 / 別 issue / ついでに直したい / 回帰か既存バグか / issue を立てる / サイドバグ発見 / out of scope / 未マージ PR への依存 / OPEN PR 依存 / 依存 PR チェック / fold-only / 別 issue 化禁止 / dependency-on-open-PR のとき。実装中・セルフ検証中・PR レビュー対応中に副次的な問題が見つかった場合に呼び出す。
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*)
---

# Scope-out Issue

Decision flow and issue-creation procedure for problems discovered outside the current PR's scope. Used during implementation, self-verification, or PR review response.

> **Source-of-truth note**: previously in `AGENTS.md` §"責務の分離 > スコープ外の問題を発見した場合の対応ルール"; relocated by #1384. 「現在 OPEN な PR への依存」ゲート (Case 2 + Step 1) は #1694 で追加。

## Decision Flow

実装中・セルフ検証中・PR レビュー対応中にスコープ外の問題を発見したときは、以下の判定フローに従う。

**Case 1: 現在の変更が直接引き起こした回帰**

現在のブランチで導入されたコードが直接引き起こした回帰のときのみ、フィーチャーブランチで即座に修正する。以下は Case 1 に該当しない（Case 2 として扱う）:

- 既存バグで、現在の変更が露呈させただけのもの
- 周辺コードを読んで気づいたコードスメル・スタイル問題・リファクタリング機会
- 「ついでに直したい」改善
- 以前から壊れていた挙動の間接的な影響

判断に迷ったら Case 2 を default とする。PR サイズの規律を優先する。

**Case 2: それ以外（既存バグ・改善・リファクタリング等）**

スコープ外として現在のブランチでは修正しない。**起票前に必ず「現在 OPEN な PR への依存」を確認**し、以下のいずれかに進む:

- **依存あり** → **Step 1** (fold または escalate、independent issue は起票しない)。発見した作業項目が、現在 OPEN な (= 未マージの) フィーチャー PR で導入される予定のコード / シンボル / データ構造を**前提**とする場合。詳細手順は Step 1 を参照。
- **依存なし** → **Step 2** 以降の通常起票フローへ。発見項目が `main` の既存コードのみで完結する場合。

判定基準:

- 該当コードパスが現在 OPEN な PR のフィーチャーブランチでしか存在しない (= main にはまだない) → **依存あり**
- 該当コードパスが `main` 上に既に存在する / 独立して作業可能 → **依存なし**

判定に確信が持てないときは Case 3 と同様にユーザー escalate する。

**Case 3: 判定が曖昧**

現在の変更が原因か判断できない場合、または Case 2 の依存判定が曖昧な場合は、ユーザーに **What** / **Where** / **Context** を提示して、いま修正するか後回しにするか (Case 2 依存判定なら fold するか独立起票するか) を問い、回答を待ってから次に進む。

## Issue Creation Steps

**Step 1: 依存 PR を確認し、fold または escalate する** (Case 2 で「依存あり」と判定した場合のみ)

このルールは **fold-only** である。「依存 PR がマージされてから再起票する」案は**採らない** — 着手不可な orphan issue は backlog 上で「open かつ着手可能」と区別がつかず tracker の signal を低下させ、依存関係も暗黙化してしまうため。

1. **該当 OPEN PR を特定する**:

```bash
gh pr list --state open --json number,title,headRefName
gh pr view <number> --json files,state,headRefName --jq '{state, branch: .headRefName, files: [.files[].path]}'
```

2. **fold する (PR 作者と相談して scope 拡大を提案する)**: 当該 PR の作者と相談し、発見した作業項目を当該 PR のスコープに追加する形で取り込む。一方的にコミットを push するのではなく、PR comment 等で scope creep を提案して同意を得るのが基本。自分が PR 作者であれば自分のフィーチャーブランチに追加コミットしてよい。

3. **escalate する (fold 困難時)**: 以下のいずれかに該当する場合は fold せず、ユーザーに **What** / **Where** / **推定 size** を提示して判断を仰ぐ。
   - 追加作業が当該 PR の本来 scope を実質的に変える (設計判断の見直しを要する等)
   - 追加 diff が当該 PR の既存 diff と同程度以上になる
   - 当該 PR が review 完了済み・マージ直前で再 review コストが高い

ユーザーは独立 issue 起票 / 当該 PR の scope 拡大 / 別フィーチャーブランチ作成等の選択肢から判断する。

**Motivating example (#1692)**: PR #1693 (`verifyCalledWith` v1) が未マージのまま、その v2 拡張として #1692 が起票された。#1692 は PR #1693 で導入される `__ry_mock_store_arg` / kind tag enum / `mockArgEqual` / `__ry_mock_count_matching_calls` を**前提**とするため、PR #1693 マージ前は実装に進めない orphan issue となった。本ゲートにより、今後は同種の起票は fold (PR #1693 への scope 追加) または escalate に振り分けられる。

**Step 2: マイルストーンを特定する**

新規 issue は現在の PR / ベース issue と同じマイルストーンに揃える。

```bash
gh pr view --json milestone --jq '.milestone.title'
gh issue list --milestone <title> --limit 1
```

**Step 3: 重複を確認する**

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

重複があれば追加 context を comment で添え、必要に応じて `gh issue edit <number> --milestone "<title>"` でマイルストーンを揃える。Step 4 を skip して Step 6 へ進む。

**Step 4: 分割を判断する**

複数の独立した関心事を含む、または見積もりが概ね 1 PR を超える場合は別 issue に分割する。**1 issue ≒ 1 PR** を目標とする。

例: parser bug と codegen 改善が同時に見つかった → 2 issue / 同じ runtime 関数の正しさ修正と性能改善 → 2 issue。

**Step 5: issue を作成する**

```bash
gh issue create \
  --title "<明確で記述的なタイトル>" \
  --milestone "<milestone-title>" \
  --body "$(cat <<'EOF'
## Context

<!-- どのファイル / 関数 / コードパスが関与したか、どの PR / issue 作業中に発見したか -->

## Reproduction

<!-- 最小再現スニペットまたは手順 -->

## Expected vs Actual

**Expected:** <!-- 期待動作 -->
**Actual:** <!-- 実際動作 -->

## Discovery timing

<!-- いずれか: 実装中 / セルフ検証中 / PR レビュー対応中 -->
EOF
)"
```

bug 以外の項目では **Expected vs Actual** を省略してよい。それ以外のセクションは必須。

**Step 6: 報告する**

ユーザーに issue 番号・タイトル・設定したマイルストーンを報告する。複数 issue を起票したらすべて列挙する。fold / escalate を選択した場合はその旨と対象 OPEN PR 番号を報告する。
