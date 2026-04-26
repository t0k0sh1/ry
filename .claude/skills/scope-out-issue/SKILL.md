---
name: scope-out-issue
description: スコープ外の問題を発見したときの判定フロー (Case 1/2/3) と GitHub issue 起票手順 (Step 1-5)。Use when スコープ外 / 別 issue / ついでに直したい / 回帰か既存バグか / issue を立てる / サイドバグ発見 / out of scope のとき。実装中・セルフ検証中・PR レビュー対応中に副次的な問題が見つかった場合に呼び出す。
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*)
---

# Scope-out Issue

Decision flow and issue-creation procedure for problems discovered outside the current PR's scope. Used during implementation, self-verification, or PR review response.

> **Source-of-truth note**: previously in `AGENTS.md` §"責務の分離 > スコープ外の問題を発見した場合の対応ルール"; relocated by #1384.

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

GitHub issue を起票し、現在のブランチでは修正しない。手順は以降の Step 1-5 に従う。

**Case 3: 判定が曖昧**

現在の変更が原因か判断できない場合は、ユーザーに **What** / **Where** / **Context** を提示して、いま修正するか後回しにするかを問い、回答を待ってから次に進む。

## Issue Creation Steps

**Step 1: マイルストーンを特定する**

新規 issue は現在の PR / ベース issue と同じマイルストーンに揃える。

```bash
gh pr view --json milestone --jq '.milestone.title'
gh issue list --milestone <title> --limit 1
```

**Step 2: 重複を確認する**

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

重複があれば追加 context を comment で添え、必要に応じて `gh issue edit <number> --milestone "<title>"` でマイルストーンを揃える。Step 3 を skip して Step 5 へ進む。

**Step 3: 分割を判断する**

複数の独立した関心事を含む、または見積もりが概ね 1 PR を超える場合は別 issue に分割する。**1 issue ≒ 1 PR** を目標とする。

例: parser bug と codegen 改善が同時に見つかった → 2 issue / 同じ runtime 関数の正しさ修正と性能改善 → 2 issue。

**Step 4: issue を作成する**

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

**Step 5: 報告する**

ユーザーに issue 番号・タイトル・設定したマイルストーンを報告する。複数 issue を起票したらすべて列挙する。
