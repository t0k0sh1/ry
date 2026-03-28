# ry - 開発ガイドライン

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
  - 英語ドキュメント（README.md / docs）の更新（または変更不要の確認）

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

`lib/std/<pkg>/<pkg>.ry` に `@native` 宣言を記述する。`manifest.json` の更新は不要だが、宣言ファイルの追加だけでは package は使えるようにならない。

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

### 4. Central registry 登録

`include/ry/builtin_stdlib_registry.hpp` に package を 1 行追加する。dispatcher 配列と native constant registry はここから導出される。

```cpp
X(crypto, "lib/std/crypto/crypto.ry", emitBuiltinCrypto)
```

### 5. ビルド設定

`CMakeLists.txt` の `ry_lib` に `src/runtime_<pkg>.cpp` と `src/codegen_call_<pkg>.cpp` を追加する。

### 6. テスト追加

- package import テストを追加する
- 代表的な native function の実行テストを追加する
- 必要なら declaration file / native constant の registry 整合テストも追加する

### 定数の追加

`lib/std/<pkg>/<pkg>.ry` に `@native @const` 宣言を追加し、`include/ry/builtin_stdlib_registry.hpp` の constant 一覧にも 1 行追加する。`codegen_stmt.cpp` の変更は不要。

### 既存パッケージへの関数追加

既存パッケージに関数を追加する場合は、以下の 4 箇所を確認する:

1. `lib/std/<pkg>/<pkg>.ry` — `@native fn` 宣言を追加
2. `src/runtime_<pkg>.cpp` — C++ 実装を追加
3. `src/codegen_call_<pkg>.cpp` — dispatch case を追加
4. テスト — selective import と実行ケースを追加

## repo build と stdlib 解決

- repo 内でビルドした `./build/ry` / `./build-current/ry` は、この project の `package.toml` にある hidden 設定 `[paths]._dev_stdlib` を使って project local の `lib/std` を参照する
- OS にインストールされた `ry` はこの hidden 設定を無視し、`~/.ry/lib/std` を参照する
- `RY_ENV=internal` は追加の isolation 用であり、repo 開発時の通常動作に必須ではない

## Git ブランチ運用ルール

- コミット前に現在のブランチを確認し、`main` または `vx.x.x` 形式のブランチにいる場合はコミットを行わないこと
- コミット・PR 作成時は、常に現在のブランチから新しいフィーチャーブランチを作成すること
- PR のマージ先は、作業開始時のブランチ（分岐元）とする
- PR を非デフォルトブランチ（`vx.x.x` 等）にマージした場合、GitHub の `Closes #xx` による自動クローズは動作しない。ラベル整理は「作業完了前チェックリスト」に従うこと
- リリース時は `vx.x.x` を `main` にマージする PR を作成する。詳細は「リリース準備ワークフロー」を参照

## 責務の分離

### Claude Code が自律的に行うこと

- 実装
- テスト実行
- セルフ検証
- ドキュメント更新

#### スコープ外の問題を発見した場合の対応ルール

実装中・セルフ検証中・PR レビュー対応中など、あらゆる場面で以下のルールを適用する。

1. **当該変更に起因する不具合・バグ → フィーチャーブランチで対応する**
   - 変更前には発生していなかった不具合やバグは、基本的に当該変更が間接的にでも影響していると判断し、現在のフィーチャーブランチ内で修正すること
   - スコープ外であっても、変更によって引き起こされたバグは先送りしない

2. **将来的に対応が必要な改善項目 → issue を作成し報告する**
   - ユーザーに確認せず自律的に issue を作成する
   - 既存の対応 issue と重複しないよう `gh search issues` 等で事前にチェックすること。既存 issue があればコメントを追加する
   - 作成した issue はユーザーに報告すること（issue 番号とタイトルを提示）

### ユーザーが明示的に指示すること

- 外部レビュー（GitHub PR レビュー等）
- git add / commit / push
- PR 作成

## 作業完了前チェックリスト

タスクの完了前に、以下を必ず実行すること。

### 1. ドキュメント反映チェック（英語のみ）

機能の**追加・変更・削除**を行った場合、**英語ドキュメントのみ**を更新する。翻訳（ja/zh）と PDF 生成はリリース準備時に行う（「リリース準備ワークフロー」参照）。

**判断基準**: ドキュメントに現在記載があるかではなく、**ユーザーが知るべき内容かどうか**で更新要否を判断する。新機能・挙動変更・新オプションなど、ユーザーに影響する変更は必ずドキュメントに反映すること。

対象と確認観点:

- **`docs/reference/`** — 型・演算子・制御構文・関数・コレクション・組み込み関数・エラーなどの仕様変更があれば該当ファイルを更新
- **`docs/tutorial/`** — ユーザー向け新機能があれば関連するチュートリアルを更新
- **`docs/README.md`** — ドキュメント目次の更新（新ページ追加時）
- **`README.md`** — 以下の内容に関わる変更があれば更新（詳細は docs/ に委譲）:
  - Features（言語機能の追加・変更）
  - Sample Code（新機能のデモに適したコード変更）
  - Installation（インストール方法の変更）
  - Usage（CLI コマンドの追加・変更）

反映が不要と判断した場合は、その理由を明示すること（内部リファクタリングのみ、テスト追加のみ、等）。

### 2. CHANGELOG 更新チェック

ユーザーに影響のある変更（`feat:`, `fix:`, 破壊的変更）を行った場合、`CHANGELOG.md` の `[Unreleased]` セクションに変更内容を追記する。

カテゴリ:

- **Added** — 新機能
- **Changed** — 既存機能の変更
- **Fixed** — バグ修正
- **Removed** — 削除された機能

内部リファクタリング・テスト追加・CI 変更のみの場合は追記不要。

### 3. 全テスト実行

全テストを実行して成功を確認する。

```bash
cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p
```

テストが失敗した場合は、原因を修正してから作業完了とすること。

### 4. ラベル整理

**セルフ検証完了時点ではラベルを変更しない。** ラベルの切り替えは PR マージ時に行う:
- PR が `vx.x.x` ブランチにマージされた時点で、対象 issue の `wip` ラベルを外し issue をクローズする
- PR を非デフォルトブランチにマージした場合、`Closes #xx` による自動クローズは動作しないため、手動で issue をクローズすること

## リリース準備ワークフロー

`vx.x.x` ブランチを `main` にマージしてリリースする前に、以下の準備を行う。

### フロー

1. `vx.x.x` から `chore/pre-release-vx.x.x` ブランチを作成
2. `VERSION` ファイルをリリースバージョンに更新（例: `0.0.5`）
3. `CHANGELOG.md` の `[Unreleased]` を `[x.x.x] - YYYY-MM-DD` に変更し、新しい空の `[Unreleased]` セクションを追加。末尾の比較リンクも更新する
4. 翻訳と PDF 生成を実施（下記参照）
5. `chore/pre-release-vx.x.x` を `vx.x.x` にマージ
6. `vx.x.x` を `main` にマージする PR を作成・マージ

### 翻訳（英語 → ja/zh）

通常開発で更新された英語ドキュメントの差分を他言語に反映する。

対象:
- `docs/reference/` → `docs/ja/reference/`, `docs/zh/reference/`
- `docs/tutorial/` → `docs/ja/tutorial/`, `docs/zh/tutorial/`
- `docs/README.md` → `docs/ja/README.md`, `docs/zh/README.md`
- `README.md` → `README.ja.md`, `README.zh.md`

### PDF 生成

```bash
cd docs && bash generate-pdf.sh
```

6 つの PDF（`tutorial-{en,ja,zh}.pdf`, `reference-{en,ja,zh}.pdf`）が更新される。

### リリースノート

GitHub Release のリリースノートは `CHANGELOG.md` の該当バージョンセクションから自動抽出される（`.github/workflows/release.yml`）。リリース準備時に `CHANGELOG.md` の内容が正確であることを確認すること。
