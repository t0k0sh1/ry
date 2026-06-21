# RFC: effect-block (`try:` expression)

- **Status**: Draft
- **Target**: v0.0.30
- **Issue**: #1702
- **Related**: `?` 演算子 (`docs/reference/operators.md`), `using` 文 (`docs/reference/control-flow.md`)

## 動機

Ry の `?` 演算子は関数全体を propagation scope にする。長いチェーンになると視覚ノイズが減って読みやすいが、副作用として「関数より下の階層に独立した propagation scope を作りたい」場合に専用 helper 関数を切り出すしかない。

たとえば次の chain を `it(...)` テストブロック内、`for` ループ内の per-item 処理、あるいは異なる scope で完結させたいとき、

```ry
fn loadName(path: str) -> Result<str, Error>:
  let f       = io.open(path, "r")?
  let cfg     = json.load(f)?
  let name: str = cfg["name"]?
  return Ok(name)
```

`?` は最寄りの関数戻り値型に dispatch されるので、helper 関数として切り出さない限り「この部分だけ Err なら局所的に処理する」ような構造が組めない。Rust の `try { ... }` 式や Haskell の `do` 記法と同じ問題意識。

## 提案: 案 A `try:` ブロック式 (採用)

```ry
fn loadName(path: str) -> Result<str, Error>:
  return try:
    let f       = io.open(path, "r")?
    let cfg     = json.load(f)?
    let name: str = cfg["name"]?
    name           # 最終式が Ok(name) として block の値になる
```

`try:` は式 (block-as-expression) として実装する。`?` の意味だけを「関数 early return」から「囲む `try:` block 脱出」に切り替える。

### 構文

```ebnf
try_expr ::= 'try' ':' try_body
try_body ::= conditional_expr                                       (* inline form *)
           | NEWLINE INDENT stmt* conditional_expr NEWLINE DEDENT   (* block form *)
```

`try` は hard keyword (`async` / `await` / `using` と同じ慣行)。`primary_expression` に `try_expr` を追加するので、`return try: ...` / `let x: Result<T,E> = try: ...` / 関数引数位置などあらゆる式位置で使える。

### Semantics

- block の **最終式** が `Ok(...)` (Result 文脈) または `Some(...)` (Option 文脈) で wrap されて block 全体の値になる。`if`-block-expression の tail-expression と同じ規律。
- block 内の `?` は囲む `try:` の merge BB に分岐する (`CreateBr`)。関数の `CreateRet` ではない。block の値は `Err(e)` / `None` になる。
- block の文脈 (Result/Option の判別と `Err` 型) は次の順で解決する:
  1. 囲む `return` の関数戻り値型 (`return try: ...`)
  2. `let` LHS の型注釈 (`let x: Result<T,E> = try: ...`)
- 上記の context が不在の `let x = try:` は v0.0.30 ではコンパイルエラー (`'try:' block requires type context: annotate 'let x: Result<T,E> = try:' or use within a typed function return`)。`?` operand から推論する two-pass emission は v0.0.31 以降に繰り延べる。
- 入れ子の `try:` は最内側優先 (`?` は最も近い囲む `try:` に escape)。
- `try:` block 内の `return` は関数を脱出する (block の Ok-wrap を bypass)。Rust の `try { return X }` と同じ。明示的に `?` を書かないことが escape hatch として機能する: `let x = io.open(path, "r")` (?なし) は通常通り `Result<File, Error>` を返し、block 内で普通に評価される。
- Result の `?` と Option の `?` を同じ block 内に混在させたらコンパイルエラー。一つの `try:` block は Result か Option のどちらか一方。
- `try:` block と `using` 文は直交。`try:` 内に `using r = expr:` を入れると、`?` で block を脱出するとき `using` の close も走る (`emitScopeCleanupToDepth` が depth ベースで cleanup する)。

### Lowering

- AST: `TryBlockExpr { vector<StmtNode> body; ExprPtr tail; }` を新設。
- Codegen は `try_scope_stack_` (`vector<TryScope>`) を CodeGen に保持。`TryScope` は `err_merge_bb` / `scope_depth` / `err_phi` / `is_option` を含む。
- `emitExprVariant(TryBlockExpr)`: `try.body` → tail emit → `try.ok.merge` / `try.err.merge` → `try.merge` PHI の BB 構造。`try.err.merge` の中身 (Result/Option struct の構築) は `T` が tail emit 後に決まるので **deferred-wrap** で埋める (PHI 自体は body emit 前に置き、`?` site から `addIncoming` 経由で raw `E` を受け取る)。
- `emitExprVariant(ErrorPropagateExpr)` の先頭に `if (!try_scope_stack_.empty()) { ... block escape ... } else { ... 既存の fn-return ベース ... }` を追加。既存の `fn_->getReturnType()` 経路は stack が空のとき byte-identical に維持される。

## 不採用案

### 案 B: `do <Effect>:` ブロック (Haskell 風)

```ry
do Result:
  let f    <- io.open(path, "r")
  let cfg  <- json.load(f)
  pure(cfg["name"])
```

任意のモナド (Result / Option / Future / List / IO / ユーザー定義 effect 型) を統一的に扱う一般化。**不採用理由**: 現 Ry には type class / trait / interface 機構が存在しない。`<T: Bound>` のサブタイプ制約 (`src/codegen_fn_generic.cpp`) は record の継承ベースのみで、`bind` / `pure` を持つ任意の型を dispatch する基盤が無い。型クラス機構の導入は本 RFC の scope を大きく超え、別 RFC で扱う。

### 案 C: `with-result:` / `with-option:` scope

```ry
with-result:
  let f    = io.open(path, "r")?
  let cfg  = json.load(f)?
  return name
```

`?` の意味を scope で切り替える構文糖。**不採用理由**: 案 A の構文バリエーション (semantic は明示 `?` + scope 脱出で同じ)。`try:` のほうが Rust / Swift / JS の `try { ... }` と同じ綴りで学習コストが低く、Option/Result の判別を構文で固定しないぶん柔軟。案 C の scope-by-name (`with-result:` / `with-option:`) は逆に Result/Option を構文に焼き付けるので将来 Future / Stream など他の effect への一般化を阻む。

## 案 B (将来) への余地

案 B (`do <Effect>:`) は将来取り組む可能性がある。本 RFC はその余地を阻まない:

- `do` / `with` / `pure` / `<-` をキーワードとして予約しない (普通の識別子として通常通り扱う)。案 B 着手時に予約する。
- `try:` の semantic は案 B の特殊ケース (`do Result:` / `do Option:` 相当) と互換に設計する。`try:` が裏で持つ `try_scope_stack_` の概念は、将来 type class 機構が入った時点で「Effect dispatch table」に一般化できる。

案 B 本格採用には先行する設計議論として以下が必要 (本 RFC の scope 外):

- 型クラス / trait / interface 機構の設計と実装 (`bind` / `pure` を method として持つ任意の型の dispatch)
- `<-` 演算子の構文と意味 (assign-and-bind)
- ユーザー定義 effect 型と組み合わせたときの型推論アルゴリズム

## 不解決事項 / 将来作業

- **Type context 不在の推論** (v0.0.31+ 繰り延べ): `let x = try:` で annotation 無しの場合、`?` operand から `E` を推論するのは two-pass emission で実装可能。本 RFC では明示エラーで guide する。
- **bare 末尾 `try:`**: 関数本体末尾の `return` 無し `try:` (式が文として置かれる形) は v0.0.30 ではサポートしない (明示エラー)。`return try:` のみが第一級。
- **case-arm との合成**: case-expression arm body 内の `try:` は parse 上は自然に取れるはずだが、tail-expression の expression-first speculative parse (`parseCaseExprArmBody` #1891) との相互作用は実装時に check する。
- **将来の case-style `try ... else`**: Swift 風の `try expr else default` (失敗時の default 値) は本 RFC では扱わない。必要なら別 issue。

## 受け入れ基準 (v0.0.30 実装)

- 構文・semantics は本 RFC「構文」「Semantics」セクションで定義した通り。
- 既存の `?` 演算子は `try_scope_stack_` が空のときの fallback として byte-identical に維持。
- `tests/spec/try_block.test.ry` に Result Ok/Err、Option Some/None、入れ子、`using` との合成、type context 不在のエラー、Result/Option 混在エラー、escape hatch (`?` なし) を網羅。
- `tests/spec/result.test.ry` / `option_propagate.test.ry` / `result_chain.test.ry` / `result_coerce.test.ry` などの既存 `?` テスト全 pass。
- ASan/UBSan で leak / use-after-free 無し。
