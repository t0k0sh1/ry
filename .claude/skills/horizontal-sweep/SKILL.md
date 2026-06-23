---
name: horizontal-sweep
description: "Terminology change / identifier rename / bulk sed replacement / derivative table / pkg / package / camelCase migration / sweep / horizontal sweep / lexical derivative / blind-spot avoidance. Use when a single term or identifier must be renamed across multiple file types (`include/`, `src/`, `share/std/`, `tests/spec/`, `tests/test_*.cpp`, `.claude/`, `docs/`). **Always run this 4-step procedure** instead of per-site `Read` + `Edit` — the latter is the pattern that produced the #1466 / #1482 / #1487 / #1490 sweep gaps."
allowed-tools: Read, Grep, Glob, Bash
metadata:
  short-description: 4-step procedure for horizontal terminology / identifier rename
---

# Horizontal Sweep

On macOS, BSD `sed` does not support `\b`; use `perl` or BSD word-boundary syntax. In zsh, drive newline-separated path loops with `while read -r` rather than relying on scalar word splitting.

Terminology and identifier renames must use batch grep + `sed -i ''`, not per-site `Read` + `Edit`. The latter produced #1466 / #1482 / #1487 / #1490 sweep gaps.

## When to use

- New term replaces existing one (e.g., `package` → `module`)
- Naming-convention enforcement requires renaming identifiers
- A `@native` function rename touches stdlib `.ry`, C++ runtime, dispatcher, and tests
- Any rename spanning more than two file types

## Cross-references

| Rule entry | Source | Contribution |
|---|---|---|
| `.claude/rules/tests-spec-conventions.md` "Naming-convention sweeps must include the implicit form" | #1466 / #1468 | Step 2 Pattern A — Ry implicit binding regex |
| `.claude/rules/docs-reference-conventions.md` "Doc-wide identifier migrations need multi-pattern sweeps" | #1444 | Step 2 Pattern C — declaration / assignment forms |
| `.claude/rules/docs-reference-conventions.md` "Terminology sweeps must include lexical derivatives" | #1482 | Step 1 derivative table + Step 2 Pattern B |
| `.claude/rules/tests-cpp-conventions.md` "Renaming stdlib @native functions" | #1414 | Step 2 Pattern D — `runSource("...")` embedded Ry |

## Carve-outs

Read `docs/reference/glossary.md` before terminology sweeps. Preserve its legacy internal identifiers.

## Steps

### Step 1: Build the canonical + derivative word table

Use `docs/reference/glossary.md` for terminology and `docs/reference/naming.md` for casing and approved abbreviations.

Worked example for `package` → `module`:

| Form | Replacement |
|---|---|
| `package` | `module` |
| `Package` | `Module` |
| `pkg` | `mod` |
| `pkgs` | `mods` |

### Step 2: Run multi-pattern grep

File-type coverage (every rename must inspect all unless explicitly out of scope):

- `include/**/*.hpp`, `src/**/*.cpp`
- `share/std/**/*.ry`
- `tests/spec/**/*.test.ry`
- `tests/test_*.cpp` — GoogleTest names AND `runSource("...")` embedded Ry (#1414)
- `.claude/rules/`, `.claude/skills/`
- `docs/`, `AGENTS.md`, `README.md`, `CHANGELOG.md`

Run all four patterns. Do not stop at Pattern B. Substitute `<old>`, `<deriv>`, `<token>` from Step 1.

```bash
# A — Ry implicit binding (#1466). ^\s* matches both module-global (column 0) and indented forms.
#     Body uses [a-zA-Z0-9_] so multi-underscore names are not truncated.
grep -rEn --include='*.ry' '^\s*[a-z][a-zA-Z0-9_]*<token>[a-zA-Z0-9_]*\s*[:=]' tests/spec/ share/std/

# B — derivative OR (#1482). Include both lower and capitalized forms.
grep -rnE '\b(<old>|<Old>|<deriv>|<Deriv>)\b' include/ src/ share/std/ tests/ .claude/ docs/

# C — declaration / assignment forms in docs (#1444).
grep -rnE 'fn [a-z]+_[a-z]+\b' docs/reference/
grep -rnE '^\s*[a-z][a-zA-Z0-9]*_[a-z][a-zA-Z0-9_]*\s*[:=]' docs/reference/

# D — C++ embedded Ry (#1414). Filter STL false positives.
grep -rnE '\b<old>\b' tests/test_*.cpp src/ include/ \
  | grep -vE 'std::filesystem::|fs::'
```

### Step 3: Bulk replace with `sed -i`

Build the file list first, exclude carve-outs, then `xargs sed`. Never use `Read` + `Edit` per file.

```bash
# Build file list and exclude carve-outs.
grep -rlE '\b(<old>|<deriv>)\b' include/ src/ share/std/ tests/ .claude/ docs/ \
  | grep -vE 'effectivePackage|RY_REGISTER_STDLIB_PACKAGE|__ry_' \
  > /tmp/sweep_targets.txt

# macOS (development): sed -i '' (empty backup-suffix arg required)
xargs sed -i '' 's/\b<old>\b/<new>/g' < /tmp/sweep_targets.txt

# Linux (CI container): sed -i (no arg)
xargs sed -i 's/\b<old>\b/<new>/g' < /tmp/sweep_targets.txt

# Verify
git diff --stat
git diff | grep -E '^[-+]' | head -50
```

#### Anti-patterns (do NOT do these)

| Pattern | Why it fails |
|---|---|
| `Read` file → `Edit` per occurrence | #1466 root cause — skips pre-grep, misses implicit bindings and module-globals |
| Grep canonical word only (e.g., `[Pp]ackage`) | #1482 — `pkg` / `Pkg` slip past |
| Grep `*.ry` only | #1414 — `runSource("...")` embedded Ry in C++ tests is missed |
| `^\s+` anchor (one-or-more spaces) | Misses module-global declarations at column 0 (#1468) |
| Body regex `[a-zA-Z0-9]+` (no underscore) | Truncates names like `cow_global_box` mid-identifier |
| Blind `s/<term>/<one-sub>/` on a **polysemous** term | #2027 — `ABI` meant boundary-label **and** struct-layout **and** binary-stability **and** function-signature; single `→ boundary` mangled the latter three. Classify each hit's meaning first and reword per sense, or carve out. Authoritative set: `git diff \| grep -nE '^-.*\bTERM\b'`. |

### Step 4: Re-verify until zero hits

```bash
for pat in '<old>' '<Old>' '<deriv>' '<Deriv>'; do
  hits=$(grep -rnE "\\b$pat\\b" include/ src/ share/std/ tests/ .claude/ docs/ \
    | grep -vE 'effectivePackage|RY_REGISTER_STDLIB_PACKAGE|__ry_' | wc -l)
  echo "$pat: $hits"
done
```

If source files (`*.cpp`, `*.hpp`, `*.ry`) were modified, build + test:

```bash
cmake --build build && ./build/ry_tests && ./build/ry test -p
```

> **macOS**: use `cmake --build build-rust` and `./build-rust/ry`; see AGENTS.md "Build And Test".

For `docs/` / `.claude/` only changes, hand off to `/pre-commit-checklist`.
