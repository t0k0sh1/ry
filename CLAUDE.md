@AGENTS.md

# ry - Claude Code 開発ガイドライン

## ビルド & テスト

```bash
cmake --preset default                                  # Ninja + LLVM（CMakePresets.json）
cmake --build build                                     # Ninja が自動並列ビルド
./build/ry_tests                                        # C++ テスト (GoogleTest)
RY_ENV=internal ./build/ry test -p                       # Ry セルフテスト (全 *.test.ry)
RY_ENV=internal ./build/ry test tests/spec/<file>.test.ry # 個別ファイル実行
```

> **`RY_ENV=internal`**: グローバル (`~/.ry/lib`) をスキップし、プロジェクトローカルの `lib/std/` を使用する。言語開発時は常にこれを付けること。
