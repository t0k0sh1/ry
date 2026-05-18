---
name: scope-decomposition
description: issue を分割・派生起票するときの事前検証 rubric (対称性 4 軸 / 分割理由 3 分類 / 派生連鎖警戒 / Plan 時再走査)。起票時 (REQ-1〜3) と Plan モード時 (REQ-4) の責任境界を top-level section で分離。Use when issue 分割 / scope-out / 派生 issue / 分離 / 別 issue 起票 / 対称性チェック / 派生連鎖 / 3 段目派生 / スコープ検証 のとき。
allowed-tools: Read, Grep, Glob
---

# Scope Decomposition Rubric

issue を**分割・派生起票**するときの事前検証 rubric。「派生 issue がさらに派生する」連鎖 (#1697 → #1797 → #1802 のような三段派生) を防ぐため、起票時と Plan モード時の 2 つの check point を提供する。

> **This skill does NOT write, edit, or modify any issue or file.** Read-only operations only — guidance and inspection.

---

## Why this skill exists

派生 issue が**さらに派生**することで、当初の機能完成像が複数 PR にまたがり、「完成」が曖昧になる失敗パターンがある。

### 事例: #1697 → #1797 → #1802 の三段派生 (#1804 で identification)

1. **#1697** (any holds collections) — 元々の `any` 拡張 issue。実装中に「record support は SSA struct value を `any.data[8]` に格納できない → heap-boxing が必要 → 規模が大きい」と判断し record support を **#1797 として分離**
2. **#1797** (any holds records) — Plan 作成時に「typed path ↔ `any` path」の対称性チェックで、`any` 経由の cross-type unwrap (`let p: Parent = anyHoldingChild`) には codegen に struct field projection utility が無い gap を発見 → **#1802 として scope-out**
3. **#1802** (cross-type unwrap from any) — 3 段目派生

3 段目の派生 (#1802) は **Plan 作成時点では識別できた** が、**#1797 起票時点では識別していなかった**。record support の主要障壁を「SSA struct value を格納できない」だけと認識し、subtype projection という独立した codegen 課題が裏側にあることを意識していなかったため。本 skill はこの種の gap を**起票時に走査**する rubric を提供する。

---

## When to invoke

| タイミング | 呼ぶ場面 | 適用する REQ |
|---|---|---|
| **起票時** | 副次的発見が `/triage-side-finding` Q4(b) で「別 issue 起票」と判定された後、`gh issue create` を実行する前 / 既存 issue の分離判断時 | REQ-1, REQ-2, REQ-3 |
| **Plan モード時** | `EnterPlanMode` 後、対象 issue のスコープを再走査するとき | REQ-4 |

「issue 分割」「scope-out」「派生 issue」「分離」「対称性チェック」「3 段目派生」が話題になったときも発火する (description キーワード参照)。

---

## 起票時のチェック (REQ-1, REQ-2, REQ-3)

issue を**分離する側 (= 新規 issue)** と**残す側 (= 元 issue)** の両方を対象に走査する。

### REQ-1: 対称性チェック (4 軸)

以下 4 軸の対称性を一度走査する:

| 軸 | 内容 |
|---|---|
| **typed path ↔ `any` path** | typed path で動く機能が `any` 経由でも動くか |
| **wrap ↔ unwrap** | 入出力のどちらかしか実装しないと不整合になる箇所はないか |
| **read ↔ write** | 読み取りだけ / 書き込みだけ実装すると non-orthogonal になる箇所はないか |
| **base ↔ derived** | 継承関係を持つ型 (record `<` parent / enum variant / union 等) で base/derived のどちらかしかカバーしないと不完全になる箇所はないか |

**Gap が見つかった場合**: issue 本文に「**スコープ外**」セクションを設け、**事前に明示**する。Plan モードで初めて発見するのではなく、起票時に書く。

具体例 (#1797 事例の再走査): #1797 起票時に **base↔derived 対称性軸**を走査していれば、subtype coercion が typed path では動くが `any` path では codegen utility 不在で動かないという非対称が事前検出できた (詳細は下記 §Reference)。

### REQ-2: 分割理由の明示化と分類

issue を分割するときは、分割理由を明示し以下のいずれに該当するか分類する:

| 分類 | 例 | 評価 |
|---|---|---|
| (a) **機能境界での分離** | SSA struct を格納できない問題と subtype projection 問題は独立した codegen 課題 | ✓ 健全 |
| (b) **依存関係での分離** | A が B の codegen utility を必要とし、B が未実装 | ✓ 健全 |
| (c) **規模での分離** | 修正量が多いので分けたい | ⚠ 危険信号 |

**(c) と判断した場合**: その奥に (a) か (b) の根拠が**本当にある**かを再検証する。

- (a)/(b) の根拠が見つかれば、分類を更新して分割する
- (a)/(b) の根拠が見つからなければ**分割しない** (規模が大きいまま 1 PR で実装するか、設計を見直す)

二値の禁止ではなく、(c) を depth-first で (a)/(b) に展開する検証フロー。

### REQ-3: 派生連鎖の警戒 (3 段目以降)

issue を起票する際、**親 issue の派生から派生する場合**は本文に明記する:

```markdown
**派生元**: #1697 → #1797 (本 issue は #1797 からの派生)
```

**3 段目以降の派生**を起票するときは、起票前に「**親 issue 群を 1 つにまとめ直す**判断が無いか」を一度検討する。

- 派生が連鎖する場合、最初の分割判断が機能境界ではなかった可能性がある
- 「親 issue 群をまとめ直す」とは、過去の派生 issue を CLOSED にし機能境界を引き直した新規 issue 1 つに統合する操作を指す
- まとめ直さずに 3 段目を起票する判断には、REQ-2 の (a) / (b) どちらの根拠で正当化されるかを issue 本文に書く

---

## Plan モード時のチェック (REQ-4)

Plan モードで実装計画を立てるとき、対象 issue のスコープを再走査する。

### REQ-4: Plan 時の再走査手順

1. **REQ-1 の対称性チェックを改めて回す**
   - 起票時に走査済みでも Plan 時に再走査する
   - 起票後にコードが変わって対称性が失われている / 起票時の走査が浅かった、のいずれもありうる
2. **Gap が発見された場合の処理**:
   - **Plan の「スコープ外」セクションに明記**してから実装着手
   - **gap を別 issue として起票**する判断は、REQ-2 の (a)/(b) のどちらに該当するかを Plan 本文に書く
   - gap が小さく同 PR で対処可能なら `/triage-side-finding` Q4(a) (即時修正) として Plan に組み込む
3. **REQ-3 派生連鎖警戒の再確認**:
   - 対象 issue が既に 3 段目派生に該当する場合、Plan 開始前にユーザーへ「親 issue 群統合の検討」を提示する選択肢を残す

---

## Reference: #1697 → #1797 → #1802 を rubric で再走査するとどうなるか

本 skill が #1797 起票時に存在していたら以下が起こったはず:

| Step | 適用 REQ | 検出される事項 |
|---|---|---|
| #1697 → #1797 分離時 | REQ-2 | (a) 機能境界 — heap-boxing は SSA struct を格納できない問題と独立 (✓ 健全) |
| #1797 起票時 | REQ-1 (base↔derived) | `any` 経由の subtype coercion を typed path と対称に動かすには codegen に struct field projection utility が必要、と起票時点で明示できた |
| #1797 起票時 | (REQ-1 で発見した gap) | issue 本文「スコープ外」セクションに「`any` 経由の cross-type unwrap は本 issue 範囲外、別途 issue 起票予定」と事前明記できた |
| #1797 → #1802 分離時 | REQ-3 | #1697 → #1797 は 2 段目、#1797 → #1802 は **3 段目派生**に該当 → 「親 issue 群統合」検討の入口 |
| #1797 → #1802 分離時 | REQ-2 | (a) 機能境界で正当 (subtype projection は独立 codegen 課題)。3 段目だが REQ-2 (a) で正当化される |

**結論**: REQ-1 (base↔derived) が #1797 起票時に走査されていれば、subtype projection の不在は**事前に「スコープ外」として明記**でき、#1802 は Plan モードで初めて発見されるのではなく **#1797 起票時に既に予告された別 issue** として扱えた。これにより「Plan 中に gap 発見 → さらに別 issue 起票」という派生の連鎖が、起票時点での明示によって**予測された分離**に変わる。

---

## Cross-reference

- **`AGENTS.md` §"issue 起点の開発"** — 「issue 分割時のスコープ検証」bullet が本 skill への入口
- **`/plan-rubric` Axis 2 (スコープ)** — Plan モード時の発火導線。`/triage-side-finding` Q4(b) 判定後に本 skill REQ-1〜3 を適用する手順が埋め込まれている
- **`/triage-side-finding` Q4(b) Step 4** — 分割判断時の参照リンクが置かれている
- **`/git-claim-issue`** — `wip` ラベル付与は本 skill とは別工程
- **`/knowledge-md-management`** — 本 skill 適用で見つかった新規知見の蓄積先 (path-scope に収まれば `.claude/rules/`、横断的なら `.claude/skills/`)

---

## Notes

- このスキルは **read-only** で、issue / Plan ファイル / コードを編集しない。rubric の適用結果は issue 本文 / Plan 本文に呼び出し元が書き込む
- REQ-1 の 4 軸は ry 固有の典型例を列挙したもので、新規軸を追加する判断は本 skill の改訂で対応する (ad-hoc に増やさない)
- REQ-3 の「親 issue 群統合」は実例運用が浅いため、ユーザー判断が入る場合がある。skill 単体での自動判定は行わない
