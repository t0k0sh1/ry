---
name: knowledge-md-management
description: KNOWLEDGE.md (未分類知見の暫定バッファ) の運用ガイド — 責任分解点・entry フォーマット・grep convention・外部参照禁止・rules/skills への昇格基準。Use when 新たな知見を得たとき / KNOWLEDGE.md に書く / KNOWLEDGE.md を参照 / ナレッジ追加 / 教訓を残す / 経験を記録 / 既存 rules/skills を更新するか判断 / 昇格 / 切り出し / 肥大化整理 のとき。実装中・PR レビュー対応中・Plan 中に新規知見を蓄積する際、または既存知見を grep で参照する際に呼び出す。
allowed-tools: Bash
---

# Knowledge MD Management

`KNOWLEDGE.md` (リポジトリ root) は、新たな教訓のうち既存 rules / skills のどれにも該当 entry を持たないものを一時的に蓄積するためのバッファ。安定後は `.claude/rules/<name>.md` または `.claude/skills/<name>/SKILL.md` へ昇格させる。

## When to use

- 作業中に新たな教訓・非自明な事実・没案・コマンドミスのリカバリ等を記録する必要が生じたとき
- 既存知見ベースを横断的に grep で参照したいとき (path-scoped auto-load では拾えない知見が KNOWLEDGE.md にある可能性があるため)
- KNOWLEDGE.md の entry を `.claude/rules/` または `.claude/skills/` に昇格 (切り出し) させるとき

## 1. 責任分解点: どこに書くか (REQ-1)

新規知見を得たときの判断フロー:

1. **対応する path-scoped rule または skill に既に該当 entry がある** (同じトピックで既に書かれている)
   → そのファイルに追記する。KNOWLEDGE.md には書かない
2. **どの rule / skill にも該当 entry がない、まったく新しい知見**
   → KNOWLEDGE.md に追記する (暫定バッファ)
3. **後日 KNOWLEDGE.md の entry が安定した、または新しいテーマとして固まった**
   → §4 の昇格手順に従い、`.claude/rules/` または `.claude/skills/` に切り出す

「該当 entry があるか」の判定は §2 の grep convention に従う。判定に迷った場合は KNOWLEDGE.md を default とする (後で昇格しやすい)。

## 2. Entry format & how to read (REQ-1, REQ-2)

### Entry format

各 entry は既存 rules / skills と同じフォーマットに統一する (grep 一貫性のため):

```markdown
### <短く具体的な heading>

**Source**: <PR / issue / commit など出典>
**Tags**: <空白区切りキーワード>
**Rule**: <教訓本文。何をすべき/避けるべきかを 1-3 段落>
```

`**Tags**:` 行は必須 — これがないと grep convention で発見できない。

### How to read

実装着手時 / レビュー時 / 関連知見の確認時には、Tags ベースで全知見ベース (rules + skills + KNOWLEDGE) を横断検索する:

```bash
grep -rnE '\*\*Tags\*\*:.*<keyword>' .claude/rules/ .claude/skills/ KNOWLEDGE.md
```

`KNOWLEDGE.md` は path-scoped auto-load の仕組みを持たないため (rule のような frontmatter `paths:` glob を持たない)、上記の grep を **明示的に** 実行する必要がある。編集ファイルから自動 trigger されない点に注意。

## 3. 外部参照ポリシー (REQ-3)

KNOWLEDGE.md の **個別 entry を指す参照** を、AGENTS.md / `.claude/rules/<*>.md` / `.claude/skills/<*>/SKILL.md` / `.claude/agents/<*>.md` から作成してはならない。理由: KNOWLEDGE.md の編集 (entry の追加・削除・順序変更・移植) で参照が容易に dangling 化する。実例として `.claude/rules/` 配下に 6 行の dangling reference が残存した実績がある (現在は別 issue で整理予定)。

### 禁止される参照パターン

`KNOWLEDGE` または `KNOWLEDGE.md` というトークンに続けて、以下のいずれかの形を取る参照は禁止:

| パターン | 形式 | 失敗する理由 |
|---|---|---|
| 行番号参照 | 大文字 `L` + 整数 | entry 追加/削除で行番号がずれる |
| 行近似参照 | 単語 `line` + 数値 | 同上 |
| 位置参照 | 単語 `entry` + 方向語 (above / below / here) | 順序変更で意味が逆転する |
| heading 名参照 | KNOWLEDGE.md の `### ...` 見出しを直接引用 | entry 移植・rename で broken |

要するに: 「KNOWLEDGE.md の特定の場所」を指す書き方すべてが禁止対象。

### 許容される参照

| パターン | 例 | OK な理由 |
|---|---|---|
| skill 自体へのメタ参照 | ``KNOWLEDGE.md の運用は `/knowledge-md-management` 参照`` | 個別 entry を指していない (skill 全体を指す) |
| 集合体としての言及 | AGENTS.md「ナレッジベース」節での bullet 項目 | KNOWLEDGE.md 全体を扱っているだけ |

要するに: 「KNOWLEDGE.md にナレッジが蓄積される」という一般的な言及は OK、特定 entry の場所を指すのは NG。

### Self-check

新規にナレッジベース文書を書くときは、以下で違反していないか確認:

```bash
grep -nE 'KNOWLEDGE(\.md)?\s*(L[0-9]+|line\s+|entry\s+(above|below|here))' \
  AGENTS.md .claude/rules/*.md .claude/skills/*/SKILL.md .claude/agents/*.md
```

ヒットした行はすべて修正対象。

## 4. rules / skills への昇格 (REQ-4)

### 昇格判定の目安

以下のいずれかに該当したら昇格を検討する:

- **個別 entry トリガー** (一次的): 単一 entry が安定し (内容が落ち着き、書き直しが必要そうにない)、永続的な所属先 (rule または skill) が特定できた → 即座に昇格
- **bulk トリガー** (二次的、定期見直し): KNOWLEDGE.md 全体が **10 entry 超** または **400 行超** に達した → 全 entry を見直して昇格候補を洗い出す

bulk トリガーの定量閾値は default 値であり、運用経験で調整可能。

### 移植先の判定: rules vs skills

| 条件 | 移植先 |
|---|---|
| frontmatter `paths:` glob で対象ファイルを絞れる (特定 path / 特定実装に依存する知見) | `.claude/rules/<name>.md` |
| 手順・意図・横断的なポリシー (path に依存しない、または複数 path にまたがる) | `.claude/skills/<name>/SKILL.md` |
| PR レビューで再発する論点の meta-index | `.claude/skills/pr-review-recurring-patterns/SKILL.md` |
| コマンド・環境変数・シェル構文のミスのリカバリ | `.claude/skills/commands-environment-gotchas/SKILL.md` |

### 移植時の cleanup

- KNOWLEDGE.md 側の元 entry は **完全削除** する。pointer stub (例: 「移植済み: 〜.md 参照」) を残してはならない
- 理由: stub 自体が将来 stale な参照になりうる。移植先 entry が rename / 削除された場合に発見が困難で、§3 の禁止パターンと同質の問題を生む
- 移植事実は PR description / commit message にのみ記録する (KNOWLEDGE.md 内には残さない)

### 移植 PR の典型 structure

1. KNOWLEDGE.md から該当 entry を削除
2. 移植先ファイルに entry を追加 (既存 entry 群と style を一致させる)
3. PR description に「KNOWLEDGE.md → `<destination>` に移植」と記録
4. `/pre-commit-checklist` でセルフ検証
