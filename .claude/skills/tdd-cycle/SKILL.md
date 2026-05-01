---
name: tdd-cycle
description: TDD ベースの開発プロセス — 既存コードの変更時と新機能追加時の両フロー (Red-Green-Refactor)。Use when 新機能追加 / 機能を追加 / 機能追加 / 既存機能修正 / 既存コードの変更 / バグ修正 / 修正する / 実装する / 機能を変更 / フィーチャー開発 / TDD / テスト駆動 / 先にテストを書く / リファクタリング / テスト作成 のとき。フィーチャー開発のメインフローでは常に fire する。
allowed-tools: Read, Grep, Glob
---

# TDD Cycle

The TDD-based development process used in the ry project. Defines **when** to write tests during feature work; for **what** test perspectives to cover, invoke `/test-checklist`.

> **Source-of-truth note**: previously in `AGENTS.md`; relocated by #1384.

## Context

ry follows a strict TDD cycle for both new-feature and existing-code-change paths. The cycle is required by `AGENTS.md` "Plan モードのルール" — the Plan must include a self-verification task that demonstrates the test→code→refactor sequence happened.

For test perspectives (annotation variants, mutation-in-loop, embedded NUL, type-cross boundary, workaround masking, error-message-text gaps), invoke `/test-checklist` at the "テスト作成" step.

## Existing Code Changes

1. 変更を検出できるテストが存在することを確認（なければ先に作成）
2. コード変更を実施（既存テストが失敗する状態になる）
3. 変更後の仕様に基づくテストを追加
4. 変更前仕様テスト失敗 & 変更後仕様テスト成功を確認
5. 失敗しているテスト（変更前仕様）を削除
6. リファクタリング

## New Feature Addition

1. 変更後の仕様に基づくテストを作成（失敗することを確認）
2. 実装してテスト成功を確認
3. リファクタリング

## Cross-reference

- **Plan-mode contract**: `AGENTS.md` §"Plan モードのルール" — the Plan must include test-first verification tasks.
- **Test design**: invoke `/test-design-techniques` to enumerate cases via 5 deductive techniques (equivalence partitioning, boundary value analysis, state transition, decision table, pairwise) at the start of the "テスト作成" step.
- **Test perspectives**: invoke `/test-checklist` at the start of the "テスト作成" step in either mode above (use after `/test-design-techniques` to verify ry-specific recurring omissions).
- **Pre-commit verification**: invoke `/pre-commit-checklist` after refactoring is complete.
