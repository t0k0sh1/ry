---
name: plan-rubric
description: 計画の抽象度（WHAT/HOW 分離）・スコープ・テスト可能性・依存表明を 4 軸で評価する pass/fail ルーブリック。Plan モードの ExitPlanMode 直前に呼び出して計画をレビューする。Use when 計画レビュー / 計画抽象度 / WHAT/HOW / Plan レビュー / 計画の粒度 / 計画が詳細すぎ / HOW 漏れ / プラン批評 のとき。/test-design-techniques の Plan 内呼び出し手順と devils-advocate を Plan 批評モードで起動する手順を含む。
allowed-tools: Read, Grep, Glob
---

# Plan Rubric

Plan モードの ExitPlanMode 直前に計画を 4 軸 pass/fail で評価するルーブリック。AGENTS.md §"Plan モードのルール" の「計画の抽象度（WHAT/HOW 分離）」原則を運用するための具体的な検査手順を提供する。

> **This skill does NOT write, edit, or modify any plan or file.** Read-only operations only — guidance and inspection.

---

## Why this skill exists

計画が HOW (関数シグネチャ・行番号・実装手順) まで踏み込むと 2 つの弊害が発生する:

1. **過剰仕様化**: 実装フェーズで別解を選ぶ余地がなくなり、Plan が「実装のカンペ」として消費される。アーキテクチャ判断の機会が失われる
2. **計画の肥大**: HOW の散文が WHAT を超えて伸び、AGENTS.md の 170 行以下サイズ制約 (#1498) や Plan ファイルの可読性を圧迫する

このスキルはその弊害を 4 軸 pass/fail で機械的に検出する。

| 軸 | 何を見るか |
|---|---|
| Axis 1: 抽象度 (WHAT/HOW) | 各タスクが「達成条件」だけを述べているか、それとも実装手段まで固定しているか |
| Axis 2: スコープ | すべてのタスクが issue の受け入れ条件にマッピングされるか |
| Axis 3: テスト可能性 | 各タスクに完了の判定信号 (テストコマンド・期待出力) が示されているか |
| Axis 4: 依存表明 | 編集対象 path に該当する rules/skills が Plan 本文で引用されているか |

---

## When to invoke

- **ExitPlanMode 直前**: 計画ファイルが完成し、ユーザー承認を取る前
- **「計画レビューしたい」「WHAT/HOW」「計画抽象度」** が話題になったとき
- **PR レビューで「計画が詳細すぎた」「実装の自由度が無かった」指摘を受けた後**: 再発防止のため次回 Plan モードで自走的に呼ぶ

---

## The Four Axes

### Axis 1: 抽象度 — WHAT/HOW 分離 (Pass / Fail)

**Pass**: 各タスクが「観察可能な達成条件」を述べる。  
**Fail**: タスクが特定の関数名・ファイル行番号・引数・実装決定を含む。

判定テスト: 実装者が *別の* 実装パスを選んでもタスクが満たせるか? 満たせない (パスが固定されている) なら HOW である。

#### OK / NG examples (ry-specific)

**ペア 1: stdlib モジュール追加 (crypto sha256)**

NG (HOW 漏れ):
```
タスク: `__ry_crypto_sha256(const char*)` に ARC-safe `RcStr` overload を追加し、
        `add_ry_native_lib(crypto ...)` 呼び出しを CMakeLists.txt の L42 に挿入する
```
理由: 関数シグネチャ・引数型・CMakeLists.txt の挿入行が固定されている。実装者が別の ARC 設計を選べない。`/stdlib-module-add` の 5 ステップは実装フェーズで自然に誘導される情報なので Plan 本文に書く必要はない。

OK (WHAT 止まり):
```
タスク: `crypto` stdlib モジュールに sha256 API を追加し、`/stdlib-module-add` の
        5 ステップを実施する。有効入力 / 空文字列 / マルチバイト UTF-8 を受理し、
        拒否ブランチを直接トリガするテストでセルフ検証する
```
理由: 達成すべき API の存在・テスト観点を述べるだけ。シグネチャ・行番号・C symbol は実装フェーズで決める。

---

**ペア 2: codegen エラー報告改善 (List + List 受理)**

NG (HOW 漏れ):
```
タスク: `emitArithmeticOp` の str-vs-non-str reject の直前 (L138) に
        `List + List` 分岐を挿入し、`emitListConcat` を呼び出す
```
理由: 関数名・行番号・呼び出し先 API を Plan 段階で固定している。`codegen-llvm-ir-conventions.md` (path-scoped rule) は `src/codegen_*.cpp` 編集時に自動 load されるので、dispatch 順序の制約は実装時にカバーされる。

OK (WHAT 止まり):
```
タスク: `List + List` 演算を型エラーなく受理する。
        `codegen-llvm-ir-conventions` の dispatch-order ルールを遵守し、
        既存 IR ゴールデンテストが pass することをセルフ検証する
```
理由: 受理という達成条件と、依存する rule の遵守を述べるだけ。挿入位置は実装フェーズで判断する。

---

### Axis 2: スコープ (Pass / Fail)

**Pass**: すべてのタスクが issue の受け入れ条件にマッピングできる。  
**Fail**: 計画段階で発見した副次的問題に対する `/triage-side-finding` の判定 (Q1-Q4) が Plan 本文に記録されていない。

副次的な問題を見つけたら `/triage-side-finding` で判定する。**Q1 (再現困難な CI 検出問題) / Q2 (ユーザー明示指示) に該当する場合は「設計上の即時修正」として同 PR で対処し、本ルールでいう「ついでに直す」には該当しない**。Q3 経由で起源分析した結果 Q4(a) 即時修正と判定された場合のみ計画タスクに織り込み、Q4(b) と判定された場合は別 issue 起票タスクとして Plan 本文に明記する。Q1-Q4 判定なしの「ついでに直す」は許容しない。

副次的発見が `/triage-side-finding` Q4(b) 「別 issue 起票」と判定された場合、**起票内容を組み立てる前に `/scope-decomposition` REQ-1 (対称性 4 軸) / REQ-2 (分割理由 3 分類) / REQ-3 (派生連鎖警戒) を適用する**。

加えて Pass 条件として: **Plan 本文に `/scope-decomposition` REQ-4 (Plan 時の再走査) の実施結果が記録されているか** を確認する。「対称性 gap 無し」の明示、または gap がある場合は Plan 本文「スコープ外」セクションへの記録、いずれかが必要。記録がない場合は Fail (ExitPlanMode 前に REQ-4 走査を実施して Plan 本文に追記する)。

---

### Axis 3: テスト可能性 (Pass / Fail)

**Pass**: 各タスクに完了判定の信号 (具体的なテストコマンド・grep パターン・期待出力) が示されている。  
**Fail**: タスクが「修正する」「実装する」だけで終わっていて、何をもって完了とするかが書かれていない。

---

### Axis 4: 依存表明 (Pass / Fail)

**Pass**: タスクが触る path に該当する `.claude/rules/` または `.claude/skills/` エントリを Plan 本文に明示的に引用している。  
**Fail**: `src/parser*.cpp` を編集するのに `parser-conventions.md` への参照がない / stdlib モジュールを追加するのに `/stdlib-module-add` への参照がない。

> **AGENTS.md L86 との差別化**: AGENTS.md の「実装計画に必ず含めるもの」は **義務** (rules/skills を参照したか) を述べる規範。Axis 4 は Plan 本文に **引用文字列が存在するか** を確認するチェックポイント。path-scoped rule は実装中に自動 load されるが、Plan 段階で引用しておくことで設計判断の根拠が記録される。

---

## /test-design-techniques の Plan 内呼び出し手順

`/test-design-techniques` (`.claude/skills/test-design-techniques/SKILL.md`) の "When to invoke" には「Plan モード: while designing test plans for a feature, to estimate coverage breadth」と既に記載されている。Plan 内で次のように消費する:

1. 各 TDD タスク (新機能追加 / 既存コードの変更) の **テスト可能性 (Axis 3)** を埋めるため、適用する技法名を **1 行** 記載する。
   例: `境界値分析 (BVA) + 等値分割: parser numeric literal の型クロス境界`
2. 技法名の列挙は WHAT — 具体的なテストケースの展開は HOW であり、実装フェーズの Red ステップで `/test-design-techniques` を invoke してそこで行う。
3. Plan に「テストケース 7 個を列挙」のような HOW は書かない。「BVA で境界を網羅し、`/test-checklist` の P1–P8 で ry 固有パターンを確認する」が WHAT 表現の正しい形。

この分離により、Plan は技法選択の意思 (WHAT) を残しつつ、ケースの羅列 (HOW) を実装フェーズに委ねる。`/tdd-cycle` Cross-reference の「テスト作成段階で `/test-design-techniques` → `/test-checklist`」と整合する。

---

## devils-advocate の Plan レビュー呼び出し

devils-advocate (`.claude/agents/devils-advocate.md`) は Phase 1-4 (Reconstruction → Multi-Angle Attack → Prioritization → Constructive Synthesis) の構造を持つ批評エージェント。Plan を独立コンテキストで批評する critic として呼び出す。

### 呼び出すべきタイミング

- **ExitPlanMode の直前**: 計画ファイルが完成し、ユーザー承認の前
- 変更対象が複数コンポーネントにまたがる、またはアーキテクチャ判断を含む場合
- 新 API / 新 skill / AGENTS.md / `.claude/agents/` 変更を含む場合

### Skip してよいケース

- 単一ファイルのバグ修正 (スコープが自明で設計判断がない)
- 既存 `/tdd-cycle` の Red-Green-Refactor のみで完結するタスク
- 文言修正・typo 直し

Skip 判定の根拠を Plan 本文に 1 行残す (例: `devils-advocate skip: 単一 cpp ファイルの bug fix のため`)。

### 呼び出し方法

`Agent` ツールで `subagent_type: devils-advocate` を指定する。`/devils-advocate` というスラッシュコマンドは存在しない (skill ではなく agent のため)。

```
Agent tool 呼び出し例:
  subagent_type: devils-advocate
  prompt: |
    以下の Plan を批評してください。WHAT/HOW 分離 (Axis 1) と
    副次的発見の扱い (Axis 2) を中心に、Phase 1-4 で検討してください。

    [plan ファイルの本文を貼る]
```

### 出力の消費パターン

| devils-advocate 出力 | 対応 |
|---|---|
| Critical 反論あり | 計画を修正してから ExitPlanMode |
| Significant 反論のみ | Plan 本文に reviewer comment として残し、実装フェーズで判断 |
| Minor 反論のみ | 無視して ExitPlanMode |
| 反論なし (ステルマンが成立) | そのまま ExitPlanMode |

---

## Cross-reference

- **`AGENTS.md` §"Plan モードのルール"** — 「計画の抽象度（WHAT/HOW 分離）」bullet が本スキルへの入口
- **`/test-design-techniques`** — Plan 内では技法名 1 行記載、実装フェーズで Red ステップで展開
- **`/test-checklist`** — `/test-design-techniques` の inductive 補完。Plan 内では参照のみ、実行は実装フェーズ
- **`/tdd-cycle`** — Plan 内の TDD タスクは Red-Green-Refactor を分割せず 1 タスクにまとめる (AGENTS.md §"Plan モードのルール" の規約と一致)
- **`.claude/agents/devils-advocate.md`** — Plan 批評モード (Phase 1-4)
- **`/triage-side-finding`** — Axis 2 で検出した副次的発見の Q1-Q4 判定 (Q1 再現困難 CI 問題 / Q2 ユーザー明示指示 / Q3 `bug-forensics-analyst` / Q4 3 択判定) と Issue Creation Steps
- **`/pre-commit-checklist`** — 実装フェーズ後の完了前検証 (Plan の検証対象ではない)

---

## Notes

- このスキルは **read-only** で、Plan ファイルや AGENTS.md を編集しない。pass/fail を出力するのみ。
- 4 軸はすべて pass/fail 二値。点数制やテンプレートは導入しない (ルーブリック自体が HOW を処方するメタ矛盾を避けるため)。
- Plan ファイルは `/Users/t0k0sh1/.claude/plans/<issue#>-<random>.md` にあり、Plan モード中は読み取れる。
