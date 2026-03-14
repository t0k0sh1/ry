#!/usr/bin/env bash
set -euo pipefail

branch=$(git rev-parse --abbrev-ref HEAD)

if [ "$branch" = "main" ] || echo "$branch" | grep -qE '^v[0-9]+\.[0-9]+\.[0-9]+$'; then
  echo "❌ Commit blocked: '$branch' は保護ブランチです。"
  echo "   フィーチャーブランチを作成してからコミットしてください。"
  echo "   例: git checkout -b feat/your-feature"
  exit 1
fi
