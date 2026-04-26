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
- ローカル実行: `find src -name '*.cpp' | xargs clang-tidy -p build --quiet`
- 新規コードは Clang-Tidy 警告ゼロを維持すること

## Cppcheck

プロジェクトルートの `.cppcheck-suppressions` で抑制設定を管理する。CI の `lint` ジョブが `src/` と `include/` に対して実行する。

```text
有効: warning, performance, portability
除外: .cppcheck-suppressions に記載（詳細はファイル参照）
```

- `compile_commands.json` は使用しない（ビルド不要で高速実行）
- ソースコード内の `// cppcheck-suppress <id>` コメントも有効（`--inline-suppr`）
- ローカル実行: `cppcheck --enable=warning,performance,portability --std=c++17 --suppressions-list=.cppcheck-suppressions --inline-suppr -i build -i build-asan -i build-tsan -j "$(nproc)" --quiet src/ include/`
- 新規コードは Cppcheck 警告ゼロを維持すること

## Clang Static Analyzer (scan-build)

CI の `scan-build` ジョブがシンボリック実行ベースのパス感度解析を実行する。Clang-Tidy / Cppcheck では検出しづらい null 参照・use-after-free・memory leak・未初期化変数・dead store を検出する。

- `scan-build` は `clang-tools-21` apt パッケージに同梱（mirror tarball にも含まれる）
- `compile_commands.json` は使用しない（scan-build がビルドをラップして解析する）
- ローカル実行:
  ```bash
  scan-build --use-analyzer=/usr/local/llvm/bin/clang \
             --use-cc=/usr/local/llvm/bin/clang \
             --use-c++=/usr/local/llvm/bin/clang++ \
             cmake --preset default
  scan-build --use-analyzer=/usr/local/llvm/bin/clang \
             --use-cc=/usr/local/llvm/bin/clang \
             --use-c++=/usr/local/llvm/bin/clang++ \
             -o /tmp/scan-build-report \
             --status-bugs \
             cmake --build build
  # HTML レポートが /tmp/scan-build-report/<timestamp>/index.html に生成される
  ```
- false positive の抑制は `#ifndef __clang_analyzer__` でインライン抑制する（clang-tidy の `// NOLINT` と同様の粒度）
- 新規コードは scan-build 警告ゼロを維持すること
