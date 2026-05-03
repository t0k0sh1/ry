---
name: release-orchestrator
description: リリース起動手順 — マイルストーン feature-complete 後に /preparing-for-release を起動し、Release prep + Release + Release cleanup issue を進める。タグ push 駆動の release.yml 自動ビルドの概要も含む。Use when "リリース" / "タグ push" / "リリース手順" / "milestone close" / "バージョンリリース" / "v0.x.y" を扱うとき。
allowed-tools: Bash(gh issue:*), Bash(gh milestone:*), Bash(git tag:*), Bash(git push:*)
---

# Release Orchestrator

Entry-point reference for the ry release flow. Routes the user to `/preparing-for-release` and explains the tag-push driven `release.yml` mechanism plus the milestone-close policy.

> **Source-of-truth note**: previously in `AGENTS.md`; relocated by #1384. AGENTS.md retains a one-paragraph summary; full detail lives here.

## Overview

> **注意**: main へのマージ = mainline 取り込みのみ。リリース (タグ push → GitHub Release) は別工程。

リリースは tag push 駆動。`v*.*.*` glob にマッチするタグ (`v0.0.14` 等) を `main` にプッシュすると `.github/workflows/release.yml` が自動でビルド・テスト・GitHub Release 作成を行う。glob は prerelease タグ (`v0.0.14-rc.1` 等) も拾うため、`build` ジョブ先頭で `^v[0-9]+\.[0-9]+\.[0-9]+$` を厳密検証して non-semver は失敗させる。

ローカルビルドの `ry -v` は `-DRY_VERSION` 未指定時に `0.0.0` を返す (既定値)。CI ビルドは `-DRY_VERSION=${GITHUB_REF_NAME#v}` で版番号が注入される。`workflow_dispatch` も維持してあるが、CI 障害時のリトライ用途に限る (`github.ref_type == 'tag'` ガードあり)。

## When to start

リリース対象バージョン `v<X.Y.Z>` のマイルストーンが feature-complete (= 配下の全 issue が close 済み) になったときに起動する。

## Hand-off

1. `/preparing-for-release <X.Y.Z>` を起動する。スキルが当該マイルストーンに以下 3 つの issue を作成する:
   - **Release prep: v<X.Y.Z>** — `changelog.d/` を `CHANGELOG.md` に集約し `[X.Y.Z] - YYYY-MM-DD` セクションを確定させる作業。通常の issue 駆動フロー (claim → feature branch → PR → merge) で実施
   - **Release: v<X.Y.Z>** — prep が merge された後、マイルストーンに残 issue が無いことを確認してタグを push する作業
   - **Release cleanup: v<X.Y.Z>** — タグ push 後、`release.yml` 完了・GitHub Release 公開を確認し、マイルストーンを close する作業 (verification-only、branch・PR 不要)
2. Release prep issue を通常通り進める (`git-claim-issue` → Plan → 実装 → `git-merge-pr`)
3. Release prep PR が main にマージされたら Release issue に着手し、その手順に従ってタグを push する
4. Release issue が close されたら、同マイルストーン内の Release cleanup issue に着手し、`release.yml` 完了・GitHub Release 公開を確認してからマイルストーンを close する (→「マイルストーン close ポリシー」)

## マイルストーン close ポリシー

milestone close は「全 issue close ≠ リリース完了」の原則のもと、リリース成果物 (タグ + GitHub Release) の公開確認後に行う。具体的な手順は Release cleanup issue に従うこと。
