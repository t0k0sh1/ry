# Test Taxonomy

C++ / Ry の test を「何を守っているか」で 3 カテゴリに分類し、可視化する。テストはコスト資産であり、削減ではなく **navigability の向上** が目的。

## カテゴリ

### contract / black-box

公開 API や層間契約に対する観察的検証。実装詳細に依存しない。

- 入力 → 戻り値・AST 形状・例外型・診断メッセージのみを assert する
- リファクタで内部実装が変わっても、契約が同じなら fail しない
- 例: `parseStr("x = 1")` を呼び `Program.size() == 1` を assert する parser test、`from math import pow` の振る舞いを assert する spec test、CLI の exit code / stdout を assert する subprocess test

### internal / white-box

内部ヘルパー・不変量・lookahead・ownership・lowering metadata など、実装詳細に踏み込む検証。

- 内部関数を直接呼ぶ、内部状態を覗く、特定の IR 形状を assert する
- 実装に依存することが前提なので、対象実装の変更で fail することは想定内
- C++ test として書くときは、何の内部を測っているか header コメントで明記する
- 例: ARC live-count 計測、IR pattern 検証、`__ry_*` runtime symbol を直接呼ぶ stress test

### regression

特定の過去バグ・review 指摘の再発を防ぐ guard。contract level でも internal level でも成立する。

- 関連 issue / PR 番号を必ず明示する (`#NNNN` を tag またはコメントに残す)
- 規約上の安定性を「将来 refactor で消されないため」のメタ情報として持つ
- 例: `tests/test_regression_2246_str_metadata_gate.cpp`、`tests/test_regression_2248_positive_str_predicate.cpp`

## 配置・命名規約

### 1 ファイルにまとめる場合: section header に tag を置く

```cpp
// ===== [contract] type パーサーテスト =====
// ===== [contract] using statement (#1817) =====   // feat-add — issue 番号は spec の出典
// ===== [regression #1748] wildcard import rejection =====
// ===== [internal] parser lookahead state probe =====
```

- ファイル冒頭に taxonomy index コメント (本文書への link と、当該ファイルで使うタグの凡例) を置く。
- 既存 section header の format (`// ===== Title =====` / `// ---- Title ----` / `// ====...====` 等) は **`// ===== [tag] Title =====` に統一**する。

### 単一 issue 専任の regression は独立ファイル

`tests/test_regression_<issue>_<desc>.cpp` を作る既存運用を継続する。

### 単一スコープで section header が立たないファイルは whole-file tag

ファイル全体が単一テーマで複数の section header に分かれない場合 (例: subprocess CLI で 1 つの bug を pin する `tests/test_codegen_call_json_reject.cpp`、`__ry_lock_acquire` / `__ry_lock_release` の contention 1 本だけを assert する `tests/test_runtime_lock_stress.cpp`)、ファイル冒頭の taxonomy index コメントに `Whole-file dominant tag: [contract] / [regression #NNNN] / [internal]` の 1 行を加える。section header の `// ===== [tag] Title =====` 行は追加しない (新たな見出し階層の捏造を避けるため)。

```cpp
// Test taxonomy: docs/reference/test-taxonomy.md
// Section header tags: [contract] / [regression #NNNN] / [internal].
// Per-test exceptions use an inline `// [regression: #NNNN]` comment.
// Whole-file tag: [regression #1941] (subprocess-based emitJsonLoad type rejection).
```

### Section 内で混在するときは inline tag

Section の dominant category を採り、別カテゴリの個別テストの直上にコメントを 1 行追加する:

```cpp
// [regression: #1450] enforce camelCase on tuple-destructure LHS
TEST(ParserTest, ParenTupleDestructRejectsSnakeCase) { ... }
```

### Issue 番号の意味分け

`(#NNNN)` を section title に残すときの意味は以下:

- `[contract] X (#NNNN)` — `#NNNN` は **spec / feature を追加した出典 issue**。テスト群はそのスペックの正常系・異常系を網羅する。
- `[regression #NNNN] X` — `#NNNN` は **発見されたバグ / review 指摘の発生元**。テスト群は同じ症状の再発を gate する。

「fix:」「bug:」プレフィックスを持つ issue は基本 `[regression]` 候補。「feat:」「chore:」プレフィックスは基本 `[contract]` (新規スペックの本来テスト) と扱う。

## 重複統合の安全ルール

2 つのテストを 1 つに統合してよいのは、以下 4 条件 **すべて** を満たすときのみ:

1. 同じ parser / API entrypoint を呼ぶ
2. 入力が意味的に等価 (lexical 差はあっても spec 上同じ branch を踏む)
3. assert する性質と粒度が同じ (例: 一方が AST を見ていて他方が例外を見ているなら統合不可)
4. 守っている edge case が同じ (lookahead / indentation / precedence 等の interaction を区別している場合は別物とみなす)

判断に迷ったら **残す**。テスト本数の削減自体は目的ではない。

## カテゴリ分布は層によって偏る

3 カテゴリを抽象的に並べているが、テスト対象の層によって分布は自然に偏る。
`tests/test_parser.cpp` (#1831) / `tests/test_codegen*.cpp` / `tests/test_runtime*.cpp`
への pilot 適用 (#2383) で観測した分布:

| 層 | 主な観察手段 | section 数 (contract / regression / internal) |
|---|---|---|
| parser | `parseProgram()` → AST / 例外 (黒箱) | 47 / 4 / 0 |
| codegen + ABI / native-descriptor | `runSource` / `runTestSource` → 標準出力 / 例外 (黒箱) が支配的。ARC ヘッダ・IR 形状・extern "C" emit ABI guard・native call descriptor 等の内部観察は局所的に発生 | 273 / 26 / 22 |
| runtime | `__ry_*` runtime symbol を直接呼ぶ、layout struct を手で組み立てる (白箱) | 0 / 8 / 78 |

`[internal]` が実際に立ったケース:

- codegen 層: `tests/test_codegen_arc.cpp` (ARC ヘッダ layout / 構造的 IR 検証), `tests/test_codegen_directive.cpp` (`ry::util::deriveRuntimeFnName` / `NativeFnSignature` registry を直接呼ぶ helper test), `tests/test_codegen_net.cpp` (`__ry_tcp_send` / `__ry_send_all` / `__ry_tcp_receive` / `__ry_tcp_set_timeout` を直接呼ぶ unit test), `tests/test_codegen_gc_visit.cpp` (test-only flag で GC visitor IR 発行を強制し marker 形を assert)。
- ABI / native-descriptor 層: `tests/test_header_layout.cpp` (C++ ↔ Rust collection / ARC header struct layout parity), `tests/test_emit_abi_guards.cpp` (extern "C" `ry_emit_*` ABI の NULL / range guard を直接呼ぶ), `tests/test_native_call_descriptor.cpp` (`inferLibraryName` / `CodeGen::getNativeCallDescriptors` の内部 descriptor 状態を assert)。
- runtime 層: `tests/test_runtime_string.cpp` / `tests/test_runtime_any.cpp` / `tests/test_runtime_gc.cpp` / `tests/test_runtime_json.cpp` / `tests/test_runtime_json5.cpp` / `tests/test_runtime_http.cpp` / `tests/test_runtime_utf8.cpp` / `tests/test_regex_runtime.cpp` / `tests/test_abi_layout.cpp` ほぼ全件が `__ry_*` を直接呼ぶか layout struct を手で組み立てる。`tests/test_runtime_lock_stress.cpp` / `tests/test_runtime_rwlock_stress.cpp` / `tests/test_runtime_arc_contention_stress.cpp` / `tests/test_runtime_print.cpp` / `tests/test_runtime_io_file.cpp` / `tests/test_coverage_runtime.cpp` は section header が立たない単一スコープのファイルだが whole-file tag として `[internal]` を付している。

scope 注: `tests/test_emit_llvm_ir.cpp` は CLI subprocess test (`ry emit-llvm-ir` の出力を assert する) で、codegen 内部状態ではなく CLI 観測の黒箱テストである。codegen 層の `tests/test_codegen_*.cpp` glob からも外れる位置付けのため、今回の taxonomy 適用範囲から外している。

parser が `[contract]` 支配的、runtime が `[internal]` 支配的、codegen はその中間で `[contract]` を主軸に局所的に `[internal]` が立つ、という予測 (旧 doc) を観察で裏付けた形となった。

## "Contract" の語義衝突

本 taxonomy の `[contract]` (= black-box) と、Ry の言語機能 **Design by Contract (`require:` / `ensure:`)** は別物。衝突する section header は両方の context が読める形に書く (例: `// ===== [contract] Design by Contract 言語機能 (require / ensure) =====`)。

## 関連

- `.claude/skills/test-checklist/SKILL.md` — テスト作成時の perspective チェック
- `.claude/rules/tests-cpp-conventions.md` — C++ テストの実装規約
- 適用済み: `tests/test_parser.cpp` (#1831), `tests/test_codegen*.cpp` + `tests/test_runtime*.cpp` + `tests/test_abi_layout.cpp` + `tests/test_coverage_runtime.cpp` + `tests/test_regex_runtime.cpp` + `tests/test_header_layout.cpp` + `tests/test_emit_abi_guards.cpp` + `tests/test_native_call_descriptor.cpp` (#2383)
