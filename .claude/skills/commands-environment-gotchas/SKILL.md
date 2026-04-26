---
name: commands-environment-gotchas
description: Reference for non-obvious command invocation mistakes — wrong flags, missing env vars, shell syntax traps, and gh/git/cmake pitfalls discovered during ry development. Use when you are about to run a command that previously failed in a non-obvious way, or when troubleshooting a command that exits unexpectedly.
allowed-tools: Bash
---

# Commands / Environment Gotchas

This skill records command/environment mistakes that were non-obvious to diagnose. Each entry has a `Wrong → Correct → Why` triple so the same mistake is not repeated.

---

### Record corrected command invocations (meta-rule)

**Source**: implementation experience (ongoing)
**Tags**: commands, environment, tooling, meta

**Rule**: When a command turns out to be wrong (bad flag, wrong path,
missing env var, outdated syntax) and you find the correct form, add
an entry below with a `Wrong → Correct → Why` triple. Examples of
what qualifies:

- forgetting `ASAN_OPTIONS=detect_container_overflow=0` on
  `build-asan/ry_tests`
- forgetting `RY_ENV=internal` when running a globally-installed `ry`
- calling `gh pr view` without `--repo` in a fork context
- using a wrong `cmake --preset` name
- heredoc / quoting / escaping mistakes in shell snippets

Skip: plain typos and mistakes that anyone would catch immediately.

**How to add a new entry**: every time you iterate on a command and
the second invocation works, ask "was the fix non-obvious?". If yes,
write a 3-line entry under this section with a descriptive subheading.

---

### Testing stdlib changes: run from the project root, not from /tmp/

**Source**: #1130 implementation (base64 `List<u8>` overloads)
**Tags**: commands, environment, stdlib, dev-stdlib, module-loader

**Wrong**: `printf '...\n' > /tmp/b64_smoke.ry && ./build/ry /tmp/b64_smoke.ry`
→ Error: `'encode_bytes' not found in package 'base64'`

**Correct**: `./build/ry test tests/spec/base64.test.ry` (or any `.ry` inside the repo)

**Why**: `./build/ry` resolves the stdlib path via `package.toml`'s hidden `[paths]._dev_stdlib` key, which requires a `package.toml` somewhere in the ancestor directory chain. Files under `/tmp/` have no such ancestor, so the module loader falls back to `~/.ry/share/std` (the globally-installed stdlib), which does not contain the newly added declarations. The trace event to look for: `"resolved_path":"/Users/.../.ry/share/std"` instead of `"resolved_path":"/Users/.../Workspace/ry-2/share/std"`.

---

### `gh issue edit --label` replaces all labels; use `--add-label` / `--remove-label`

**Source**: #1144 (2026-04-18)
**Tags**: gh, issue, label, wip, workflow

**Wrong**: `gh issue edit <n> --label wip`
→ **Replaces** the entire label set with `["wip"]`. All other labels (`bug`, `enhancement`, milestone-shadow labels, etc.) are silently deleted.

**Correct**:
- Add a label: `gh issue edit <n> --add-label wip`
- Remove a label: `gh issue edit <n> --remove-label wip`
- Create with labels (safe — no pre-existing labels): `gh issue create --label enhancement --label documentation`

**Why**: `--label` in `gh issue edit` is a *set* operation, not an *append*. The flag name is misleading because `gh issue create --label` is safe (empty initial state). The asymmetry bites every time you remember the create syntax and apply it to edit.

**How to apply**: Use `git-claim-issue` skill for `wip` attachment (enforces `--add-label` internally). Use `git-merge-pr` Step 5 for `wip` removal (enforces `--remove-label` internally). Never call `gh issue edit --label` directly for additive changes.

---

### bash `set -u` with empty array: use `"${arr[@]+"${arr[@]}"}"` not `"${arr[@]}"`

**Source**: #1165 Docker run.sh (2026-04-18)
**Tags**: commands, bash, shell, docker

**Wrong**: `docker run ... "${ENV_ARGS[@]}" ...` with `set -euo pipefail` and `ENV_ARGS=()`
→ Error: `ENV_ARGS[@]: unbound variable` when the array is empty

**Correct**: `docker run ... "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" ...`

**Why**: bash's `set -u` (nounset) treats an empty array expansion `"${arr[@]}"` as an
unbound variable. The idiom `"${arr[@]+"${arr[@]}"}"` uses parameter expansion with a
default — it expands to nothing when the array is empty, and to the full array contents
when non-empty. This is the standard POSIX-compatible workaround for `set -u` + optional
arrays in shell scripts.

---

### `ry -c` reads from stdin, not argv

**Source**: #1269 manual repro (2026-04-21)
**Tags**: commands, cli, ry, stdin

**Wrong**: `./build/ry -c 'print(1)'`
→ Silently prints nothing and exits 0. The positional argument after `-c` is ignored — the compiler reads an empty stdin, parses zero statements, and succeeds.

**Correct**: `printf 'print(1)\n' | ./build/ry -c` (or `echo 'print(1)' | ./build/ry -c`)

**Why**: `ry -c` follows a different convention from `python -c` / `sh -c`. It takes the source code on **stdin**, not as the next argv element. The `--help` output shows `echo '<code>' | ry -c` but this is easy to miss if you habitually reach for `-c 'snippet'` from shell/Python muscle memory. Particularly dangerous because the wrong form exits 0 with no output instead of erroring, so a failed manual repro looks like "compiler accepted the invalid program" when in fact no program was fed in at all.

**How to apply**: For one-off Ry snippets use a heredoc-to-pipe or write a scratch file under the project root (not `/tmp/` — see the `_dev_stdlib` gotcha above).

---

### Skill `allowed-tools` must cover all Bash commands the skill body prescribes

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: skill, allowed-tools, claude-code, ci-investigate, review-feedback

**Rule**: Every Bash command that a SKILL.md step instructs the agent to run must be covered by an entry in `allowed-tools`. A common pitfall is listing only `gh pr:*`/`gh run:*`/`git branch:*` while the skill body also calls `cmake`, `clang-tidy`, `cppcheck`, `scan-build`, `find`, etc. At runtime the agent will be blocked from running those uncovered commands, silently breaking the step.

When the reproduction command set is open-ended (e.g. "run the CI job's corresponding local command"), use `Bash` (unrestricted) rather than a long enumeration of prefixes that will grow stale.

**How to verify**: grep the skill body for bare Bash commands not covered by the `allowed-tools` line.

---

### `gh run list --branch` returns all runs on a branch, not just the PR head commit

**Source**: #1045 (2026-04-16, CodeRabbit review)
**Tags**: github-actions, gh-cli, ci-investigate, review-feedback, gotcha

**Rule**: `gh run list --branch <name>` includes runs from every commit on that branch. In a CI investigation or re-run tool, this causes reruns and log analysis for commits unrelated to the PR being investigated.

Always filter by the PR's `headRefOid` (head commit SHA) immediately after the `gh run list` call:

```bash
gh run list --branch <headRefName> --limit 20 \
  --json databaseId,headSha,name,status,conclusion,workflowName \
  | jq --arg sha "<headRefOid>" '[.[] | select(.headSha == $sha)]'
```

Alternatively, derive run IDs directly from `detailsUrl` in the `gh pr checks` output (`grep -oE '/runs/([0-9]+)' | grep -oE '[0-9]+'`).

---

### Skill SKILL.md: keep `owner` and `repo` as separate variables when downstream steps use `{owner}`/`{repo}` individually

**Source**: PR #1148 CodeRabbit review / issue #1152 (2026-04-19)
**Tags**: skill, gh-cli, review-feedback

**Rule**: Using `gh repo view --json owner,name --jq '.owner.login + "/" + .name'` and storing the result as both `owner` and `repo` is correct only when the downstream code treats the combined value as a single placeholder (e.g. `repos/$FULL/...`). When downstream steps separately substitute `{owner}` and `{repo}` (REST paths like `repos/{owner}/{repo}/pulls/{PR}/...` or GraphQL `repository(owner: "<owner>", name: "<repo>")`), the combined string causes doubled path segments (e.g. `repos/t0k0sh1/ry/ry/pulls/...`) or an incorrect GraphQL `owner` argument. In that case, fetch them separately: `OWNER=$(gh repo view --json owner --jq '.owner.login')` / `REPO=$(gh repo view --json name --jq '.name')`. When writing or reviewing a skill step that stores repository coordinates, verify whether downstream uses the value as one unit or as two — they require different fetch forms.
