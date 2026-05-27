---
name: tdd-cycle
description: TDD timing rules for ry — write a detector test BEFORE changing existing code; delete old-spec tests AFTER the new spec passes. Fires on 新機能追加, 既存機能修正, バグ修正, 修正する, 実装する, TDD, テスト駆動, 先にテストを書く, リファクタリング, テスト作成.
allowed-tools: Read, Grep, Glob
---

# TDD Cycle

## Existing Code Changes

Detector test first; delete old-spec tests only after the new spec passes.

## New Feature Addition

Red-Green-Refactor per test case.

## Cross-reference

`/test-design-techniques` and `/test-checklist` at the テスト作成 step.
