---
name: static-analysis-tools
description: Clang-Tidy / Cppcheck / scan-build (Clang Static Analyzer) の設定・ローカル実行コマンド・CI ジョブ・抑制ルール。Use when "clang-tidy 実行" / "cppcheck 警告" / "scan-build" / "静的解析" / "NOLINT" / "lint 失敗" / static analyzer falsy positive を扱うとき。
allowed-tools: Bash
---

# Static Analysis Tools

Reference for Clang-Tidy, Cppcheck, and Clang Static Analyzer (scan-build) configuration and invocation in the ry project.

> **Source-of-truth note**: previously in `AGENTS.md`; relocated by #1384.

## Clang-Tidy

プロジェクトルートの `.clang-tidy` でチェック設定を管理する。CI の `clang-tidy` ジョブが全 `src/*.cpp` ファイルに対して実行する。

```text
有効: bugprone-*, performance-*, cert-*, 選択的 modernize-*
除外: bugprone-easily-swappable-parameters, cert-err58-cpp 等（詳細は .clang-tidy 参照）
```

- `HeaderFilterRegex` はプロジェクトヘッダ (`include/ry/`) のみに制限
- LLVM / GoogleTest ヘッダは SYSTEM include のため自動除外
- `compile_commands.json` は `CMAKE_EXPORT_COMPILE_COMMANDS=ON` で自動生成（`build/` 内）
- CI は event に応じて build スコープを切り替える (#1741):
  - **pull request**: `cmake --build build --target ry --parallel` で fast build (`src/main.cpp` + `ry_lib` ≒ ~76 TU)。`ry_tests` / native plugin / fuzz の TU build は省略
  - **push to main**: `cmake --build build --parallel` で full build (all target)
  - **注意**: `--target ry` は **build step のみ** narrowing する。clang-tidy 解析は両 event とも `find src -name '*.cpp'` で得られる全 90 ファイル (ry_lib に含まれない 14 TU を含む) を並列解析する
- 解析は `xargs -0 -n 1 -P "$(nproc)"` で TU 並列実行 (#1741)。`-n 1` は必須 — 省くと xargs が全 .cpp を 1 つの clang-tidy 呼び出しにまとめてしまい、`-P` の並列度が無効化される
- ローカル実行は Docker 経由に統一する (issue #1865 — macOS で PCH 互換性問題 / Homebrew LLVM PATH を回避):
  ```bash
  ./docker/run.sh static-analysis clang-tidy
  ```
- 新規コードは Clang-Tidy 警告ゼロを維持すること

### Platform-specific false positives (libc++ vs libstdc++)

**Source**: #1405 (2026-04-27)
**Tags**: clang-tidy, bugprone-exception-escape, libc++, libstdc++, noexcept, platform-specific

**Context**: macOS Homebrew LLVM (libc++) と Linux apt LLVM (libstdc++) は、`bugprone-exception-escape` などの noexcept 推論で挙動が異なる。libc++ のほうが保守的で、std container の move-assignment / `resize()` / lambda の operator() を `noexcept` と推論しないことがある。結果、Linux CI では green でも macOS ローカルでは error になる構成が生じる。

**抑制方針**:

1. **真に noexcept な処理**: `noexcept` を明示する。例: container move-assignment のみで構成された destructor は `~Foo() noexcept;` と宣言・定義の両方を coupled で更新する。これは仕様通りの noexcept なので NOLINT より好ましい。ただし `resize()` のような libc++ が常に保守的に推論する操作を含む場合は、`noexcept` でも警告は消えないため、加えて `// NOLINTNEXTLINE` が必要。
2. **プロセス境界・watcher・スレッドエントリ**: `std::terminate` 動作が許容済みの箇所は `// NOLINTNEXTLINE(bugprone-exception-escape): <理由>` で抑制する。理由文には「process boundary」「watcher lambda」「thread entry」など具体的なコンテキストを書く。

**抑制すべきでないケース**: 通常の関数で例外を投げうるコードを noexcept と宣言・抑制すること。例外発生で `std::terminate` するため、プロセス境界以外では呼び出し側が捕捉できる例外設計を維持する。

**ローカル検証**: `./docker/run.sh static-analysis clang-tidy` で Linux + libstdc++ 環境を再現する (issue #1865)。Docker 経由なら toolchain を Apple clang ↔ Homebrew LLVM ↔ Linux LLVM で揃える必要がなく PCH 互換性問題も発生しない。

**参照例**:
- `src/codegen.cpp` の `CodeGen::FnScope::~FnScope() noexcept` — `noexcept` 明示 + NOLINTNEXTLINE 併用（destructor 末尾の `resize()` を libc++ が throwing と推論）
- `src/main.cpp` の `main()` と watcher lambda — NOLINTNEXTLINE で抑制（process boundary）

## Cppcheck

プロジェクトルートの `.cppcheck-suppressions` で抑制設定を管理する。CI の `lint` ジョブが `src/` と `include/` に対して実行する。

```text
有効: warning, performance, portability
除外: .cppcheck-suppressions に記載（詳細はファイル参照）
```

- `compile_commands.json` は使用しない（ビルド不要で高速実行）
- ソースコード内の `// cppcheck-suppress <id>` コメントも有効（`--inline-suppr`）
- ローカル実行は Docker 経由に統一する (issue #1865):
  ```bash
  ./docker/run.sh static-analysis cppcheck
  ```
- 新規コードは Cppcheck 警告ゼロを維持すること

## Clang Static Analyzer (scan-build)

CI の `scan-build` ジョブがシンボリック実行ベースのパス感度解析を実行する。Clang-Tidy / Cppcheck では検出しづらい null 参照・use-after-free・memory leak・未初期化変数・dead store を検出する。

CI は event に応じて分析スコープを切り替える (#1738):
- **pull request**: `--target ry --parallel` で fast scan (`src/main.cpp` + `ry_lib` ≒ ~76 TU)。test と native plugin の TU は除外し、PR フィードバックを高速化する。
- **push to main**: `--parallel` 付きの full scan (all target)。test / native plugin / fuzz も含めた広いカバレッジを維持する。

両 event とも `continue-on-error: true` (warn-only) 運用中。

- `scan-build` は CI コンテナ (`ghcr.io/<owner>/ry-ci:llvm-21`) の LLVM 21 source build に同梱されており、`/usr/local/llvm/bin/scan-build` から利用可能
- `compile_commands.json` は使用しない（scan-build がビルドをラップして解析する）
- ローカル実行は Docker 経由に統一する (issue #1865 — macOS で scan-build が PATH 外 / Homebrew 依存を回避)。fast scan (`ry` target のみ、PR と同等) を実行:
  ```bash
  ./docker/run.sh static-analysis scan-build
  ```
  HTML レポートは bind-mount 経由でホスト側の `build-scan-docker/scan-build-report/<timestamp>/index.html` に生成される（コンテナ内 path は `/workspace/build-scan/scan-build-report/`）。コンテナ終了後もホストに残るので、ブラウザ等で直接開いて確認できる
- 全 3 ツールを一括実行する場合: `./docker/run.sh static-analysis all`
- `scan-build` および `all` は専用の `build-scan-docker/`（host）↔ `build-scan/`（container）で analyzer wrapper ビルドを行うため、`build-docker/` は無傷で残る。続けて `./docker/run.sh default ...` を実行する際に `rm -rf` 等は不要。レポートだけ捨てたい場合は `build-scan-docker/` を削除する
- false positive の抑制は `#ifndef __clang_analyzer__` でインライン抑制する（clang-tidy の `// NOLINT` と同様の粒度）
- CI は warn-only 運用 (`continue-on-error: true`)。新規 null-dereference / use-after-free / division-by-zero などが検出されたら同 PR で対処することを強く推奨する
