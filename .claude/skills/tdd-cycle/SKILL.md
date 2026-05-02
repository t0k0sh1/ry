---
name: tdd-cycle
description: TDD ベースの開発プロセス — 既存コードの変更時と新機能追加時の両フロー (Red-Green-Refactor)。Use when 新機能追加 / 機能を追加 / 機能追加 / 既存機能修正 / 既存コードの変更 / バグ修正 / 修正する / 実装する / 機能を変更 / フィーチャー開発 / TDD / テスト駆動 / 先にテストを書く / リファクタリング / テスト作成 のとき。フィーチャー開発のメインフローでは常に fire する。
allowed-tools: Read, Grep, Glob
---

# TDD Cycle

## Existing Code Changes

1. 変更を検出できるテストが存在することを確認（なければ先に作成）
2. コード変更を実施（既存テストが失敗する状態になる）
3. 変更後の仕様に基づくテストを追加
4. 変更前仕様テスト失敗 & 変更後仕様テスト成功を確認
5. 失敗しているテスト（変更前仕様）を削除
6. リファクタリング

## New Feature Addition

一般的な TDD (Red-Green-Refactor) に従う。各テストケース毎にサイクルを内部で回し、ハッピーパス 1 ケースで完了扱いにしない。

## Cross-reference

- **テスト観点**: テスト作成段階で `/test-design-techniques` → `/test-checklist` の順に呼ぶ
- **完了前**: `/pre-commit-checklist`
