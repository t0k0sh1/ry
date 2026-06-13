#!/usr/bin/env bash
# Mark an open issue as in progress without replacing its existing labels.
#
# Exit codes:
#   0  claimed successfully
#   2  already claimed (`wip` present)
#   3  issue is not open

set -euo pipefail

REPO="${RY_GITHUB_REPO:-t0k0sh1/ry}"
raw_issue="${1:-}"

if [[ ! "$raw_issue" =~ ^#?[1-9][0-9]*$ ]]; then
  echo "usage: scripts/claim-issue.sh '#<issue-number>'" >&2
  exit 1
fi

issue="${raw_issue#\#}"
state="$(gh issue view "$issue" --repo "$REPO" --json state --jq .state)"
title="$(gh issue view "$issue" --repo "$REPO" --json title --jq .title)"
before_labels="$(gh issue view "$issue" --repo "$REPO" --json labels --jq '.labels[].name')"

if [[ "$state" != "OPEN" ]]; then
  echo "claim blocked: issue #$issue is not open (state: $state)." >&2
  exit 3
fi

if grep -qxF 'wip' <<< "$before_labels"; then
  echo "claim blocked: issue #$issue is already marked wip." >&2
  exit 2
fi

gh issue edit "$issue" --repo "$REPO" --add-label wip >/dev/null
after_labels="$(gh issue view "$issue" --repo "$REPO" --json labels --jq '.labels[].name')"

if ! grep -qxF 'wip' <<< "$after_labels"; then
  echo "claim failed: wip was not added to issue #$issue." >&2
  exit 1
fi

while IFS= read -r label; do
  [[ -z "$label" ]] && continue
  if ! grep -qxF -- "$label" <<< "$after_labels"; then
    echo "claim failed: existing label '$label' disappeared from issue #$issue." >&2
    exit 1
  fi
done <<< "$before_labels"

echo "claimed issue #$issue: $title"
