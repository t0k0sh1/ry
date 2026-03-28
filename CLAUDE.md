# ry - Claude Code 開発ガイドライン

## ビルド & テスト

```bash
cmake --preset default                                  # Ninja + LLVM（CMakePresets.json）
cmake --build build                                     # Ninja が自動並列ビルド
./build/ry_tests                                        # C++ テスト (GoogleTest)
RY_ENV=internal ./build/ry test                         # Ry セルフテスト (全 *.test.ry)
RY_ENV=internal ./build/ry test tests/spec/<file>.test.ry # 個別ファイル実行
```

> **`RY_ENV=internal`**: グローバル (`~/.ry/lib`) をスキップし、プロジェクトローカルの `lib/std/` を使用する。言語開発時は常にこれを付けること。

## ワークフロー全体像

1. **issue 確認** — 対象 issue の内容を把握する
2. **`wip` ラベル付与** — 対象 issue に `wip` ラベルを付ける
3. **Plan モード** — 実装計画を立てる
4. **実装** — TDD ベースで開発する
5. **セルフ検証** — テスト実行・ドキュメント反映・ラベル整理チェック
6. **ユーザー指示を待つ** — 以降の操作は「責務の分離」セクションに従う

## issue 起点の開発

- **リポジトリ**: `t0k0sh1/ry`
- **開始パターン**:
  - ユーザーが issue 番号または URL を指定 → GitHub MCP で issue を読み取り、内容を把握して Plan モードへ
  - ユーザーが「次の issue を探して」と指示 → open な issue を取得し（`wip` ラベル付きは除外）、バグ優先・効果の高い改善を優先して候補を提示、ユーザーが選択後に Plan モードへ
- **Plan モードとの接続**: issue の内容を仕様として Plan に反映する
- **ラベル運用**:
  - issue に着手する時点で `wip` ラベルを付与する
  - 作業完了時に `wip` ラベルを外し、issue をクローズする

## Plan モードのルール

- **開始条件**: 対象 issue が特定されていること、対象 issue に `wip` ラベルが付与されていること、リリースブランチ `vx.x.x` にいること、かつリモートと最新化されていることを確認する
- **実装計画の最初のタスク**: フィーチャーブランチの作成
- **実装計画のスコープ**: セルフ検証まで（git add / commit / push / PR 作成は含めない）
- **実装計画に必ず含めるもの**:
  - 仕様通りに実装できていることのセルフ検証タスク
  - README.md / docs の更新（または変更不要の確認）

## TDD ベースの開発プロセス

### 既存コードの変更時

1. 変更を検出できるテストが存在することを確認（なければ先に作成）
2. コード変更を実施（既存テストが失敗する状態になる）
3. 変更後の仕様に基づくテストを追加
4. 変更前仕様テスト失敗 & 変更後仕様テスト成功を確認
5. 失敗しているテスト（変更前仕様）を削除
6. リファクタリング

### 新機能追加時

1. 変更後の仕様に基づくテストを作成（失敗することを確認）
2. 実装してテスト成功を確認
3. リファクタリング

## stdlib パッケージの追加手順

新しい標準ライブラリパッケージ（例: `crypto`）を追加するための手順。

### 1. Ry 宣言ファイル作成

`lib/std/<pkg>/<pkg>.ry` に `@native` 宣言を記述する。ディレクトリスキャンで自動認識されるため、`manifest.json` の更新は不要。

```ry
@native
fn sha256(data: str) -> str
```

### 2. C++ ランタイム実装

`src/runtime_<pkg>.cpp` に `extern "C"` 関数を実装する。関数名は `__ry_<pkg>_<name>` の規約に従う。

```cpp
extern "C" const char *__ry_crypto_sha256(const char *data) { ... }
```

### 3. Codegen dispatcher 作成

`src/codegen_call_<pkg>.cpp` を作成し、`include/ry/codegen.hpp` に宣言を追加する。

```cpp
// codegen_call_<pkg>.cpp
llvm::Value *CodeGen::emitBuiltin<Pkg>(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee)) return nullptr;
    // ... dispatch logic ...
}
```

共通ヘルパー（`codegen_call.cpp` に実装済み）を活用する:

| ヘルパー | 用途 |
|---------|------|
| `wrapPtrAsResult(ptr, errFn)` | nullable ptr → `Result<T, Error>` |
| `wrapStatusAsResult(status, errFn)` | int status → `Result<Unit, Error>` |
| `emitResultBranch(isErr, resTy, buildOk, buildErr)` | カスタム Result 構築 |
| `buildErrorFromRuntime(errFn)` | ランタイムから Error struct を構築 |

### 4. Dispatcher 登録

`src/codegen_call.cpp` の `stdlib_dispatchers` 配列に 1 行追加する。

```cpp
static const StdlibDispatcher stdlib_dispatchers[] = {
    ...
    &CodeGen::emitBuiltin<Pkg>,  // ← 追加
};
```

### 5. ビルド設定

`CMakeLists.txt` の `ry_lib` に `src/runtime_<pkg>.cpp` と `src/codegen_call_<pkg>.cpp` を追加する。

### 定数の追加

`src/codegen_call.cpp` の `native_constant_registry` に 1 行追加するだけでよい。`codegen_stmt.cpp` の変更は不要。

### 既存パッケージへの関数追加

既存パッケージに関数を追加する場合は、以下の 3 箇所を変更する:

1. `lib/std/<pkg>/<pkg>.ry` — `@native fn` 宣言を追加
2. `src/runtime_<pkg>.cpp` — C++ 実装を追加
3. `src/codegen_call_<pkg>.cpp` — dispatch case を追加

## Git ブランチ運用ルール

- コミット前に現在のブランチを確認し、`main` または `vx.x.x` 形式のブランチにいる場合はコミットを行わないこと
- コミット・PR 作成時は、常に現在のブランチから新しいフィーチャーブランチを作成すること
- PR のマージ先は、作業開始時のブランチ（分岐元）とする
- PR を非デフォルトブランチ（`vx.x.x` 等）にマージした場合、GitHub の `Closes #xx` による自動クローズは動作しない。ラベル整理は「作業完了前チェックリスト」に従うこと

## 責務の分離

### Claude Code が自律的に行うこと

- 実装
- テスト実行
- セルフ検証
- ドキュメント更新
- 作業中に現在の PR スコープ外の問題を発見した場合、ユーザーに内容を提示し、承認を得てから GitHub issue を作成する
- スコープ外とした改善項目について、既存の対応 issue があるか検索する。あれば該当 issue にコメントし、なければ新規 issue を作成する
- PR レビュー（Copilot、人間問わず）で指摘されたもののうち、現在のスコープ外だが対応が必要なものは積極的に issue 化する。ただし、既存の issue と重複しないよう `gh search issues` 等で事前にチェックすること

### ユーザーが明示的に指示すること

- 外部レビュー（GitHub PR レビュー等）
- git add / commit / push
- PR 作成

## 作業完了前チェックリスト

タスクの完了前に、以下を必ず実行すること。

### 1. ドキュメント反映チェック

機能の**追加・変更・削除**を行った場合、`docs/` 配下のドキュメントに反映すべき内容がないか確認する。

対象と確認観点:

- **`docs/reference/`** — 型・演算子・制御構文・関数・コレクション・組み込み関数・エラーなどの仕様変更があれば該当ファイルを更新
- **`docs/tutorial/`** — ユーザー向け新機能があれば関連するチュートリアルを更新
- **`docs/README.md`** — ドキュメント目次の更新（新ページ追加時）
- **`README.md`** — 特徴一覧やサンプルコードに大きな変更がある場合のみ更新（詳細は docs/ に委譲）

反映が不要と判断した場合は、その理由を明示すること（内部リファクタリングのみ、テスト追加のみ、等）。

### 2. 全テスト実行

全テストを実行して成功を確認する。

```bash
cmake --preset default && cmake --build build && ./build/ry_tests && RY_ENV=internal ./build/ry test
```

テストが失敗した場合は、原因を修正してから作業完了とすること。

### 3. ラベル整理

**セルフ検証完了時点ではラベルを変更しない。** ラベルの切り替えは PR マージ時に行う:
- PR が `vx.x.x` ブランチにマージされた時点で、対象 issue の `wip` ラベルを外し issue をクローズする
- PR を非デフォルトブランチにマージした場合、`Closes #xx` による自動クローズは動作しないため、手動で issue をクローズすること
