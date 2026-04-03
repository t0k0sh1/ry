@AGENTS.md

# ry - Claude Code 開発ガイドライン

## ビルド & テスト

```bash
cmake --preset default                                  # Ninja + LLVM（CMakePresets.json）
cmake --build build                                     # Ninja が自動並列ビルド
./build/ry_tests                                        # C++ テスト (GoogleTest)
./build/ry test -p                                      # Ry セルフテスト (全 *.test.ry)
./build/ry test tests/spec/<file>.test.ry               # 個別ファイル実行
```

> repo 内でビルドした `./build/ry` は `package.toml` の hidden 設定 `[paths]._dev_stdlib` に従ってプロジェクトローカルの `lib/std/` を優先する。`RY_ENV=internal` は追加の isolation が必要な場合だけ使う。

## ASan（AddressSanitizer）

ローカル開発では ASan を有効にしてテストを実行する:

```bash
cmake --preset asan                                     # Debug + ASan（build-asan/）
cmake --build build-asan                                # ビルド
ASAN_OPTIONS=detect_container_overflow=0 ./build-asan/ry_tests      # C++ テスト（ASan 有効）
ASAN_OPTIONS=detect_container_overflow=0 ./build-asan/ry test -p    # Ry セルフテスト（ASan 有効）
```

> `detect_container_overflow=0` は、ASan なしでビルドされた LLVM ライブラリとの混在で生じる false positive を抑制するために必要。

ASan が検出した問題（メモリリーク、バッファオーバーフロー、use-after-free 等）は必ず解消すること。ASan エラーを残したままコミットしてはならない。
