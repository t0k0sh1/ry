# ry

## build

```
cd build
cmake .. -DLLVM_DIR=$HOME/Workspace/llvm-project/build/lib/cmake/llvm
cmake --build .
```

## 使い方

- **REPL**: 引数なしで起動し、stdin が TTY のとき対話モード（`-i` で強制起動も可）
  - `ry> ` プロンプトで式を入力
  - `let x = 5` または `let x = 5 in x` でトップレベル定義（以降の行で `x` を参照可能）
  - `exit` または `quit` で終了、Ctrl+D でも終了
- **式の評価**: `./ry "1 + 2"` または `echo "1 + 2" | ./ry`
- **ファイル実行**: `./ry examples/arithmetic.ry` でサンプルを実行
