# Ry 言語ドキュメント

Ry は LLVM JIT ベースのシンプルなプログラミング言語です。Python スタイルのインデントブロック構文を採用し、静的型付けと型推論を組み合わせた使いやすい設計になっています。

---

## チュートリアル

Ry を初めて使う方はこちらから順番に読み進めてください。

| # | タイトル | 内容 |
|---|----------|------|
| 01 | [はじめに](tutorial/01-getting-started.md) | 環境構築・Hello World・コメント |
| 02 | [変数と型](tutorial/02-variables-and-types.md) | 変数宣言・定数・型アノテーション・基本型 |
| 03 | [演算子](tutorial/03-operators.md) | 算術・比較・論理・ビット・複合代入演算子 |
| 04 | [制御構文](tutorial/04-control-flow.md) | if/elif/else・while・for/range・break/continue |
| 05 | [関数定義](tutorial/05-functions.md) | fn 定義・再帰・オーバーロード・ラムダ・UFCS |
| 06 | [構造体](tutorial/06-structs.md) | type 定義・フィールドアクセス・メソッド |
| 07 | [コレクション](tutorial/07-collections.md) | タプル・リスト・マップ |
| 08 | [高度な機能](tutorial/08-advanced.md) | クロージャ・演算子オーバーロード・Option 型 |
| 09 | [モジュール](tutorial/09-modules.md) | from/import によるモジュール分割 |

環境構築とビルド方法は [01 - はじめに](tutorial/01-getting-started.md) を参照してください。

---

## リファレンス

言語仕様の詳細はリファレンスを参照してください。

| ページ | 内容 |
|--------|------|
| [型一覧・型規則](reference/types.md) | 全型の説明・型昇格ルール・型変換 |
| [演算子一覧・優先順位](reference/operators.md) | 全演算子と優先順位表 |
| [制御構文](reference/control-flow.md) | if・while・for の完全な文法 |
| [関数・ラムダ・UFCS・演算子オーバーロード](reference/functions.md) | 関数定義の全形式 |
| [構造体](reference/structs.md) | type 定義の完全な文法 |
| [タプル・リスト・マップ](reference/collections.md) | コレクション型の操作方法 |
| [組み込み関数](reference/builtins.md) | print・len・Some・unwrap 等 |
| [モジュールシステム](reference/modules.md) | from/import の文法と探索ルール |
| [エラー一覧](reference/errors.md) | コンパイルエラーと実行時エラーの説明 |
