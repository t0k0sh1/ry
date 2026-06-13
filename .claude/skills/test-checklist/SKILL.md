---
name: test-checklist
description: Check recurring ry-specific test omissions before writing or reviewing tests.
allowed-tools: Read, Grep, Glob, Bash(git diff:*), Bash(git log:*)
metadata:
  short-description: Ry-specific test perspectives
---

# Test Checklist

Use before implementation and during test review.

## Core Perspectives

| Area | Check |
|---|---|
| Acceptance | Normal case, preserved sibling, direct reject branch |
| Types | Concrete, generic, alias, `any`, nested collection, enum / record |
| Values | Empty, one, many, zero, negative, min / max, embedded NUL, UTF-8 |
| Ownership | Copy, alias, mutation, loop iteration, return, scope exit |
| Control flow | Branch, loop, lambda, nested expression, early return |
| Ordering | Declaration / emission order and before / after use |
| Metadata | Load, extract, PHI, branch merge, wrap / unwrap |
| Errors | Error type, message, exit path, preserved runtime detail |
| Environment | Default, Linux, macOS, sanitizer-specific masking |
| Regression quality | Test fails before fix for the intended reason |

## Ry-Specific Checks

- Annotation and implicit-binding variants.
- Module-global and local forms.
- Imported alias and qualified access.
- Empty collection with explicit type.
- Nested collection and pointer-backed element.
- Mutation inside loops and parallel work.
- String byte length versus UTF-8 character length.
- C++ embedded Ry source when runtime or stdlib names change.
- Positive case beside each new rejection.
- Existing rejection test flipped, not deleted, when behavior is legalized.
- ARC leak tests use the established live-count pattern.

## Rejection Changes

- Every new validation or rejection branch needs a test that triggers it directly.
- When narrowing accepted input, add a positive test for the preserved sibling form.
- When legalizing input, flip matching rejection tests to acceptance tests instead of deleting them.
- A downstream error does not prove the intended rejection branch ran; isolate the expected detector.
- A defensive branch may omit a direct test only when no supported API can reach it. Document the reason and add a test if it later becomes reachable.

## Review

1. Map each changed branch to a test.
2. Confirm failures indicate the intended condition.
3. Remove redundant cases.
4. Run the narrow test, then the required suite from `/pre-commit-checklist`.
