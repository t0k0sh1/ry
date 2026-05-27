---
name: tdd-cycle
description: TDD flow (Red-Green-Refactor) — existing-code changes and new features. Always fires during feature development; use when adding a feature, fixing a bug, refactoring, or writing tests. Also fires on Japanese triggers 新機能追加, 機能を追加, 機能追加, 既存機能修正, 既存コードの変更, バグ修正, 修正する, 実装する, 機能を変更, フィーチャー開発, TDD, テスト駆動, 先にテストを書く, リファクタリング, テスト作成.
allowed-tools: Read, Grep, Glob
---

# TDD Cycle

## Existing Code Changes

1. Ensure a test detects the change (write one first if needed).
2. Apply the change; existing tests fail.
3. Add tests for the new spec.
4. Confirm old-spec fails, new-spec passes.
5. Delete the failing old-spec tests.
6. Refactor.

## New Feature Addition

Standard TDD (Red-Green-Refactor). Run a full cycle per test case — not just the happy path.

## Cross-reference

- **Test design step**: `/test-design-techniques` → `/test-checklist`.
- **Before completion**: `/pre-commit-checklist`.
