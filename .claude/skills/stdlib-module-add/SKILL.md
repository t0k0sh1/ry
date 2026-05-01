---
name: stdlib-module-add
description: 新しい標準ライブラリモジュール (@native) の追加手順 (5 ステップ + 定数追加 + 既存モジュールへの関数追加)。Use when stdlib モジュール追加 / 新しい標準ライブラリ / @native 宣言 / runtime_<mod>.cpp / add_ry_native_lib / 定数の追加 / share/std/<mod>/<mod>.ry を扱うとき。
allowed-tools: Read, Grep, Glob, Bash
---

# Stdlib Module Add

Procedure for adding a new standard library module (e.g. `crypto`) to the ry project, plus the recipes for adding constants and extending existing modules.

> **Source-of-truth note**: previously in `AGENTS.md`; relocated by #1384.
>
> **Terminology** (per `docs/reference/glossary.md`, introduced in v0.0.17 by #1480): the unit imported via `from xxx import ...` is called a **module** — a single `.ry` file or directory. The word "package" is reserved for the future `ry add` external-library feature and is unimplemented today. Older internal C++ identifiers (`effectivePackage`, `RY_REGISTER_STDLIB_PACKAGE`, `__ry_<symbol>` ABI) keep the legacy "package" naming for ABI/source-stability reasons; refer to those identifiers verbatim when editing code, but use "module" in prose.

## Steps

新しい標準ライブラリモジュール（例: `crypto`）を追加するための手順。

### 1. Ry 宣言ファイル作成

`share/std/<mod>/<mod>.ry` に `@native("mod")` 宣言を記述する。`manifest.json` の更新は不要だが、宣言ファイルの追加だけでは module は使えるようにならない。

```ry
@native("crypto")
fn sha256(data: str) -> str
```

### 2. C++ ランタイム実装

`src/runtime_<mod>.cpp` に `extern "C"` 関数を実装する。関数名は `__ry_<mod>_<name>` の規約に従う。

```cpp
extern "C" const char *__ry_crypto_sha256(const char *data) { ... }
```

### 3. ビルド設定

`CMakeLists.txt` で `add_ry_native_lib(mod src/runtime_<mod>.cpp)` を追加して共有ライブラリを作成する。`RY_NATIVE_LIBS` リストにも追加して `ry` と `ry_tests` にリンクする。

### 4. Codegen dispatcher（カスタムロジックが必要な場合のみ）

単純な関数（引数をそのまま渡してランタイムを呼ぶだけ）は `emitGenericNativeCall` が自動処理するため、codegen ファイルの作成は不要。

リソーストラッキング、受信者型dispatch、Option wrapping 等のカスタムロジックが必要な場合は:
1. `src/codegen_call_<mod>.cpp` を作成し、`RY_REGISTER_STDLIB_PACKAGE` マクロで自己登録 + `NativeDispatchEntry` テーブル + free function `custom_emitter` を定義
2. opaque リソース型がある場合は `ResourceKindRegistry::instance().registerKind(...)` で静的初期化時にリソース種別を登録
3. `CMakeLists.txt` の `ry_lib` にソースファイルを追加

共通ヘルパー（`codegen_call_dispatch.cpp` に実装済み）を活用する:

| ヘルパー | 用途 |
|---------|------|
| `wrapPtrAsResult(ptr, errFn)` | nullable ptr → `Result<T, Error>` |
| `wrapStatusAsResult(status, errFn)` | int status → `Result<Unit, Error>` |
| `emitResultBranch(isErr, resTy, buildOk, buildErr)` | カスタム Result 構築 |
| `buildErrorFromRuntime(errFn)` | ランタイムから Error struct を構築 |

### 5. テスト追加

- module import テストを追加する
- 代表的な native function の実行テストを追加する
- 必要なら declaration file / native constant の registry 整合テストも追加する

## Adding Constants

`share/std/<mod>/<mod>.ry` に `@const` 宣言を追加する。通常は `@native("mod")` を使うが、`math` のように個別の shared library を持たないモジュールでは bare `@native` を使う（詳細は `.claude/rules/stdlib-module-additions.md` を参照）。dispatch ファイル内で `StdlibRegistry::instance().registerConstant(...)` を静的初期化時に呼び出す（registry 本体は `include/ry/stdlib_registry.hpp` の `StdlibRegistry` クラスで、`src/codegen_call.cpp` 内の `MathConstReg` が具体例）。`codegen_stmt.cpp` の変更は不要。

## Adding Functions to Existing Modules

既存モジュールに関数を追加する場合は、以下の箇所を確認する:

1. `share/std/<mod>/<mod>.ry` — `@native("mod") fn` 宣言を追加
2. `src/runtime_<mod>.cpp` — C++ 実装を追加
3. `src/codegen_call_<mod>.cpp` — カスタム dispatch が必要なら custom_emitter を追加（単純な関数は不要）
4. テスト — selective import と実行ケースを追加
