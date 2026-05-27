#!/usr/bin/env bash
# §3.7 Background Task Residual Check — detect orphan shells (run_in_background
# + heredoc pattern: zsh + cat blocked on stdin). Stop them via TaskStop or kill
# before declaring complete. Active claude CLI processes and this script itself
# are intentionally excluded — only the zsh+cat zombie matters.
set -euo pipefail

orphans="$(ps aux | grep -E 'zsh.*cat' | grep -v grep || true)"
if [[ -n "$orphans" ]]; then
  echo "==> Orphan processes detected:"
  echo "$orphans"
  exit 1
fi
echo "==> No orphan processes"
