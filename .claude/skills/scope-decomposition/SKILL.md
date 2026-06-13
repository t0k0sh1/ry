---
name: scope-decomposition
description: Decide whether issue work or a derivative finding should be split.
allowed-tools: Read, Grep, Glob
---

# Scope Decomposition

## REQ-1: Symmetry

Before splitting, compare the proposed pieces across:

- User-visible behavior.
- Implementation ownership.
- Verification method.
- Delivery dependency.

Keep coupled pieces together.

## REQ-2: Valid Split Reasons

Split only when at least one applies:

- Independent user value.
- Independent implementation and verification.
- Materially lower review or delivery risk.

Do not split only to reduce apparent task size.

## REQ-3: Derivative Chain

- Avoid third-degree derivative issues.
- Fold dependent follow-ups into the nearest owning issue.
- Record the dependency when a separate issue is necessary.

## REQ-4: After Scope Commitment

- Re-sweep the target issue for omitted symmetric work.
- Do not shrink or split the target issue after implementation scope is committed.
- Route orthogonal discoveries through `/triage-side-finding`.

## REQ-5: Oversized Issue Before Scope Commitment

- Determine all proposed pieces before filing.
- Present one combined split preview.
- Each piece must independently satisfy REQ-1 and REQ-2.
- Proceed only after user approval.
