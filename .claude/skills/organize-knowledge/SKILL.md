---
name: organize-knowledge
description: Organize and refactor KNOWLEDGE.md by detecting stale, outdated, duplicate, and extractable entries. Presents candidates grouped by action type (delete, update, merge, extract-to-rules, extract-to-skills) and applies approved changes. Use when the user wants to clean up, reorganize, or refactor KNOWLEDGE.md.
allowed-tools: Read, Edit, Write, Glob, Grep, Bash(grep:*), Bash(wc:*), Bash(git log:*), Bash(ls:*), Bash(test:*)
metadata:
  short-description: Organize and refactor KNOWLEDGE.md
---

# Organize Knowledge

Systematically organize `KNOWLEDGE.md` by detecting stale, outdated, duplicate, and extractable entries. All destructive changes require user confirmation.

## Context

- Current branch: !`git branch --show-current`
- Repository: !`gh repo view --json owner,name --jq '.owner.login + "/" + .name'`
- Entry count: !`grep -c '^### ' KNOWLEDGE.md`
- Line count: !`wc -l < KNOWLEDGE.md`

## Inputs

User input: $ARGUMENTS

If the user specifies a scope (e.g., a section name like "Codegen", or an action type like "stale only"), limit analysis to that scope. Otherwise, analyze all entries and all 5 action types.

## Steps

### Step 1: Parse KNOWLEDGE.md into structured entries

Read `KNOWLEDGE.md` with `Read`.

Parse each entry by scanning line by line:
- `## SectionName` lines set the current section
- `### EntryTitle` lines start a new entry
- `---` lines or the next `## ` line end a section
- Each entry's body extends from the `### ` heading to the line before the next `### `, `## `, or `---`

For each entry, extract:
- **Title**: the `### ` heading text
- **Section**: the parent `## ` heading (Testing, Codegen, Parser / Lexer, Runtime / Memory, Build / CI, Documentation, Commands / Environment gotchas, Stdlib, Review feedback patterns)
- **Line range**: start line number and end line number (needed for Edit operations later)
- **Source**: the `**Source**:` field value (PR number, date, context)
- **Tags**: the `**Tags**:` field value as a list
- **Referenced file paths**: extract all paths matching patterns like `src/*.cpp`, `src/*.hpp`, `include/ry/*.hpp`, `tests/*.cpp`, `tests/spec/*.test.ry`, `share/std/*.ry`, `docs/**/*.md`, `CMakeLists.txt`, `.github/**` mentioned anywhere in the entry body. Include paths in backticks and code blocks.
- **Referenced identifiers**: extract function names, class names, method names mentioned in backticks (e.g., `emitRuntimeError`, `wrapInUnion`, `propagateTypeMeta`)
- **Body text**: full markdown content between this heading and the next entry/section boundary

Confirm the parsed entry count matches the Context entry count. If not, report the discrepancy and investigate.

### Step 2: Build file inventory

Run the following `Glob` calls **in parallel** to build an inventory of existing files:

1. `src/**/*.cpp`
2. `src/**/*.hpp`
3. `include/**/*.hpp`
4. `tests/**/*`
5. `share/std/**/*.ry`
6. `docs/**/*.md`
7. `.github/**/*`
8. `CMakeLists.txt`

Combine all results into a single set of known file paths. This inventory is used in Step 3 for fast existence checks without per-file I/O.

### Step 3: Analyze entries for staleness and outdatedness

For each parsed entry:

#### 3a. Staleness detection (Action 1 — Delete candidates)

Check every referenced file path against the file inventory from Step 2:
- If a referenced file does NOT exist in the inventory, check if it was renamed by searching for the basename with `Glob` (e.g., if `src/codegen_foo.cpp` is missing, search `src/**/codegen_foo*`)
- If the file is gone and no renamed equivalent found, mark the entry as a **delete candidate**

Check key referenced identifiers with `Grep`:
- For function names that are central to the entry's rule (not every mentioned function — focus on the 1-2 most important ones), search the codebase
- If the core identifier no longer exists anywhere, mark as a **delete candidate**

**Classification**:
- **Stale**: Referenced file(s) deleted AND core identifier(s) gone → Delete candidate
- **Potentially outdated**: File exists but specific identifier/pattern is gone → feeds into 3b

#### 3b. Outdatedness detection (Action 2 — Update candidates)

For entries whose referenced files exist but whose content may have drifted:

1. **Code pattern verification**: For entries citing specific code patterns or behaviors (e.g., "`isStringValue()` returns true for any `ptrTy_` value"), use `Grep` to find the current implementation and check if the described behavior still holds
2. **API signature changes**: For entries describing specific function signatures, verify the current implementation still matches
3. **Line number drift**: For entries citing specific line numbers, `Read` the file and check if the referenced code is still at that line

**Practical scope**: Focus on entries with specific, verifiable claims (function signatures, code patterns, line numbers). Skip entries that are purely abstract design rules without concrete code references — these are not efficiently verifiable.

Mark entries with detected drift as **update candidates**, noting what specifically changed.

### Step 4: Detect duplicates (Action 3 — Merge candidates)

Compare all entry pairs:

1. **Tag overlap**: Compute the tag intersection for each pair. Entries sharing 3 or more tags are merge candidates
2. **Title keyword similarity**: Compare entry titles after removing common stopwords. Titles covering the same concept (e.g., multiple entries about the same function or pattern) are merge candidates
3. **Cross-references**: Entries that explicitly reference each other (e.g., "See also entry X") are candidates for merging or noting the relationship
4. **Same-topic cluster**: Entries from the same PR or implementation that cover related sub-topics

**Exceptions — do NOT merge**:
- "Review feedback patterns" entries that are lightweight pointers to other entries — these are intentionally separate
- Entries that cover different aspects of the same broad topic (e.g., two entries about ARC that cover different invariants)

For each merge candidate pair, draft the proposed merged entry showing how the two entries would be combined.

### Step 5: Classify extraction candidates (Actions 4 and 5)

#### 5a. Extract to `.claude/rules/` (Action 4)

Identify entries that are **file-pattern-specific rules** — single invariants that should be checked whenever specific files are edited. Criteria:

- The entry's rule applies whenever a specific file or narrow file pattern is edited
- The rule is an invariant (always true), not context-dependent
- The entry has a clear "How to verify" with a targeted grep/check

For each candidate, draft the `.claude/rules/<name>.md` file:

```markdown
---
description: <When this rule applies — one sentence>
globs: <file glob pattern, e.g., src/runtime_any.cpp>
---

<Rule content condensed from the KNOWLEDGE.md entry. Include the actionable rule and essential context only.>
```

Proposed filename: kebab-case derived from the rule topic (e.g., `float-formatter-precision.md`, `ubsan-flags.md`).

**Be conservative**: Only extract entries where the file-pattern trigger is clear and the rule is self-contained. If unsure, leave in KNOWLEDGE.md.

#### 5b. Extract to `.claude/skills/` (Action 5)

Identify entries that describe **multi-step procedures** triggered by specific developer actions. Criteria:

- The entry describes a workflow or checklist (not a single rule)
- The entry is triggered by a specific action ("when adding a new X", "when changing Y")
- The entry could stand alone as a guided procedure

**Be very conservative**: Most KNOWLEDGE.md entries are single rules, not procedures. Only extract if the entry genuinely describes a multi-step checklist that would benefit from being a skill.

For each candidate, draft the proposed skill structure (name, description, steps).

### Step 6: Present findings for approval

Display all findings grouped by action type in a structured format:

```text
## KNOWLEDGE.md Organization Report

**Entries analyzed**: N | **Lines**: N

### 1. Delete candidates (stale entries)

(No candidates found.)

OR:

| # | Entry | Section | Lines | Reason |
|---|-------|---------|-------|--------|
| D1 | "Title" | Codegen | 113-150 | `src/foo.cpp` no longer exists; `emitFoo` not found in codebase |

### 2. Update candidates (outdated entries)

| # | Entry | Section | Lines | Issue |
|---|-------|---------|-------|-------|
| U1 | "Title" | Codegen | 317-343 | `emitRuntimeError` signature changed: now takes 3 args, entry says 2 |

### 3. Merge candidates (duplicate entries)

| # | Entries | Tags overlap | Rationale |
|---|---------|--------------|-----------|
| M1 | "Entry A" (L178) + "Entry B" (L669) | codegen, metadata, propagation | Both describe propagateTypeMeta requirements |

<collapsed: proposed merged entry>

### 4. Extract to .claude/rules/

| # | Entry | Target glob | Proposed file |
|---|-------|-------------|---------------|
| R1 | "UBSan must disable vptr..." | CMakeLists.txt | ubsan-flags.md |

<collapsed: proposed rules file content>

### 5. Extract to .claude/skills/

| # | Entry | Trigger | Proposed skill |
|---|-------|---------|----------------|
| S1 | "Adding a new type kind..." | Adding a new type kind | add-type-kind |

<collapsed: proposed skill outline>
```

If no candidates are found for any category, display "(No candidates found.)" for that category.

Then ask:

> Review the candidates above. Reply with:
> - `all` to approve all candidates
> - Specific IDs to approve (e.g., `D1, U1, M1, R1`)
> - `none` to skip all
> - Category approval (e.g., `all rules, skip merges`)
> - Editing instructions for specific candidates (e.g., `R1: change glob to include cmake/**`)

**Wait for user response before proceeding.**

### Step 7: Apply approved changes and report

Apply changes in this order to avoid line-number conflicts:

#### 7a. Extractions (non-destructive first)

For each approved extraction:

1. **Rules**: Create `.claude/rules/<name>.md` using `Write`. If `.claude/rules/` directory does not exist, the first `Write` will create it.
2. **Skills**: Create `.claude/skills/<name>/SKILL.md` using `Write`.
3. **Update KNOWLEDGE.md**: For each extracted entry, replace the full entry body with a short pointer:
   ```markdown
   ### [Original title]

   > **Extracted** to `.claude/rules/<name>.md` (or `.claude/skills/<name>/SKILL.md`).
   > Original source: #NNN. See the extracted file for the current rule.
   ```
   Use `Edit` to replace the entry body (from the line after `### ` to the entry boundary).

#### 7b. Merges

For each approved merge:

1. Read `KNOWLEDGE.md` with `Read` to get current line numbers
2. Replace the **surviving entry** (the first/earlier one) with the merged content using `Edit`
3. Delete the **merged-away entry** (the later one) using `Edit` — replace the full entry (from `### ` to entry boundary) with empty string

Process merges one at a time, re-reading after each to get accurate line numbers.

#### 7c. Updates

For each approved update:

1. Read `KNOWLEDGE.md` with `Read`
2. Edit the specific lines that need correction using `Edit`

#### 7d. Deletes (bottom-to-top)

Sort delete candidates by line number in **descending order** (highest first) to preserve line numbers for earlier deletions.

For each approved deletion:

1. Read `KNOWLEDGE.md` with `Read`
2. Display the exact content that will be removed
3. Delete the full entry (from `### ` heading to entry boundary) using `Edit` — replace with empty string
4. Clean up any resulting double-blank-lines

#### 7e. Summary

After all changes are applied, display:

```text
## Summary

- **Entries analyzed**: N
- **Stale entries deleted**: N
- **Outdated entries updated**: N
- **Entries merged**: N (M source entries → K combined entries)
- **Entries extracted to .claude/rules/**: N
- **Entries extracted to .claude/skills/**: N
- **KNOWLEDGE.md**: before N lines → after N lines

Changes applied. NOT committed or pushed.
```

**Important**: Do NOT commit or push. The user will do so explicitly.

## Why this skill exists

KNOWLEDGE.md grows organically through PR reviews and implementation work. Without periodic maintenance, entries become stale (referencing deleted code), outdated (describing changed behavior), redundant (multiple entries covering the same topic), or misplaced (file-specific rules that belong in `.claude/rules/`, procedures that belong in `.claude/skills/`). This skill provides systematic cleanup with user oversight, keeping the project's long-term memory accurate and well-organized.
