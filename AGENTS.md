# ry - エージェント向けプロジェクトガイド

このドキュメントは、AIエージェントが ry プロジェクトで作業する際の参照情報です。

## プロジェクト概要

- **Ry**: 純粋関数型言語のコンパイラ／ランタイム
- **実装言語**: C++
- **基盤**: LLVM（コード生成）、MLIR（GPU 処理対応）
- **ライセンス**: MIT
- **ビルドシステム**: CMake 3.20+

## ディレクトリ構成

```
ry/
├── CMakeLists.txt    # ビルド設定
├── src/
│   └── main.cpp      # メインソース
├── build/            # ビルド出力（生成される）
└── README.md
```

## ビルド方法

### 前提条件

- **CMake** 3.20 以上
- **LLVM**: ビルド済みの llvm-project が必要
- **C++ コンパイラ**: 対応する C++17 以上

### ビルド手順

```bash
# 1. build ディレクトリへ移動
cd build

# 2. CMake で設定（LLVM_DIR を llvm-project のビルドパスに指定）
cmake .. -DLLVM_DIR=$HOME/Workspace/llvm-project/build/lib/cmake/llvm

# 3. ビルド実行
cmake --build .
```

初回ビルド時は `build` ディレクトリが存在しない場合があるため、事前に作成する：

```bash
mkdir -p build
cd build
cmake .. -DLLVM_DIR=$HOME/Workspace/llvm-project/build/lib/cmake/llvm
cmake --build .
```

### LLVM のパスについて

`LLVM_DIR` は llvm-project をビルドしたディレクトリ内の `lib/cmake/llvm` を指す必要があります。環境に応じて以下を調整してください：

- デフォルト例: `$HOME/Workspace/llvm-project/build/lib/cmake/llvm`
- 別の場所にビルドしている場合: `<llvm-build-path>/lib/cmake/llvm`

## 実行方法

ビルド後、実行ファイルは `build/ry` に生成されます。

```bash
# プロジェクトルートから
./build/ry

# または build ディレクトリ内から
cd build
./ry
```

## 技術スタック

- **LLVM**: コード生成の基盤
- **MLIR**: GPU 処理のサポート
- **LLVM コンポーネント**: `core`, `support`
- **リンクライブラリ**: LLVMCore, LLVMSupport, LLVMBinaryFormat, LLVMRemarks, LLVMBitstreamReader, LLVMTargetParser, LLVMDemangle など

## 開発時の注意事項

1. **src/main.cpp の存在**: CMakeLists.txt は `src/main.cpp` をエントリポイントとして参照しています。このファイルが存在しないとビルドに失敗します。
2. **build ディレクトリ**: `.gitignore` に含まれていないため、ビルド成果物がリポジトリに含まれる可能性があります。必要に応じて `build/` を `.gitignore` に追加することを検討してください。
3. **LLVM のビルド**: ry をビルドする前に、llvm-project をビルドしておく必要があります。
4. **言語機能の変更時**: 言語機能を追加・変更・削除した場合は、必ず `docs/` と `examples/` への反映を行うこと。
5. **リファクタリング優先**: 新機能を追加する前に、既存コードをリファクタリングして機能追加に適した形にしてから着手すること。
