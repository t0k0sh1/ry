[English](../README.md) | [日本語](README.md) | [繁體中文](../zh/README.md)

# Ry 言語ドキュメント

Ry は LLVM JIT ベースのシンプルなプログラミング言語です。Python スタイルのインデントブロック構文を採用し、静的型付けと型推論を組み合わせた使いやすい設計になっています。

---

## チュートリアル

Ry を初めて使う方はこちらから順番に読み進めてください。

| # | タイトル | 内容 |
|---|----------|------|
| 01 | [はじめに](tutorial/01-getting-started.md) | 環境構築・Hello World・コメント |
| 02 | [変数と型](tutorial/02-variables-and-types.md) | 変数宣言・定数・型アノテーション・基本型・f-string・型キャスト |
| 03 | [演算子](tutorial/03-operators.md) | 算術・比較・論理・ビット・複合代入演算子 |
| 04 | [制御構文](tutorial/04-control-flow.md) | if/else, when・while・for/range・break/continue |
| 05 | [関数定義](tutorial/05-functions.md) | function 定義・再帰・オーバーロード・デフォルト引数・ラムダ・クロージャ・高階関数・UFCS |
| 06 | [Record と列挙型](tutorial/06-records.md) | type 定義・フィールドアクセス・enum・ADT・ジェネリック enum・演算子オーバーロード |
| 07 | [コレクションとイテレータ](tutorial/07-collections.md) | タプル・リスト・マップ・セット・遅延イテレータ |
| 08 | [エラーハンドリング](tutorial/08-error-handling.md) | Option・Result・`?` 演算子・契約による設計 |
| 09 | [パッケージ](tutorial/09-modules.md) | パッケージ・std ライブラリ・ディレクトリパッケージ |
| 10 | [並行処理](tutorial/10-concurrency.md) | async/await・@parallel・スレッド・ネットワーキング |
| 11 | [テスト](tutorial/11-testing.md) | describe/it/expect・マッチャー・mock/verify・契約テスト |
| 12 | [プロジェクト構築](tutorial/12-building-a-project.md) | 全機能を組み合わせたハンズオン CLI プロジェクト |

環境構築とビルド方法は [01 - はじめに](tutorial/01-getting-started.md) を参照してください。

---

## リファレンス

言語仕様の詳細はリファレンスを参照してください。

| ページ | 内容 |
|--------|------|
| [型一覧・型規則](reference/types.md) | 全型の説明・型昇格ルール・型変換 |
| [演算子一覧・優先順位](reference/operators.md) | 全演算子と優先順位表 |
| [制御構文](reference/control-flow.md) | if/else・match・when・while・for の完全な文法 |
| [関数・ラムダ・UFCS・演算子オーバーロード](reference/functions.md) | 関数定義の全形式 |
| [構造体・列挙型](reference/structs.md) | type 定義・enum 定義の完全な文法 |
| [タプル・リスト・マップ・セット](reference/collections.md) | コレクション型の操作方法 |
| [組み込み関数](reference/builtins.md) | print・length・Some・range 等 |
| [文字列操作関数](reference/builtins-string.md) | contains・find・replace・split・join 等 |
| [正規表現](reference/regex.md) | regex_match・regex_search・regex_replace・regex_split・regex_find_all |
| [数学関数](reference/math.md) | PI・E・sqrt・sin・cos・abs・floor・ceil・round 等 |
| [I/O 関数](reference/io.md) | read_text・write_text・exists・read_bytes・to_bytes 等 |
| [JSON](reference/json.md) | parse・stringify・get・at・to_str・to_int 等 |
| [ネットワーク（TCP）](reference/net.md) | bind・listen・accept・connect・TCP ソケットの send/receive/close |
| [HTTP サーバー](reference/http.md) | listen・method・path・header・body・response |
| [Base64](reference/base64.md) | encode・decode・encode_url_safe・decode_url_safe |
| [Path](reference/path.md) | join・basename・dirname・extension・resolve・is_absolute |
| [ファイルシステム](reference/filesystem.md) | list_dir・walk・glob_files・copy・move・remove・make_dir・chmod・symlink |
| [スレッド](reference/thread.md) | thread_spawn・thread_join・Lock・RWLock・Semaphore・Barrier・AtomicInt・AtomicBool |
| [GC](reference/gc.md) | collect・enable・disable・set_threshold — ARC 用サイクルコレクタ |
| [パッケージシステム](reference/packages.md) | from/import の文法・ディレクトリパッケージ・std・RY_HOME |
| [テスト機能](reference/testing.md) | describe/it/expect によるテスト |
| [プロジェクト管理](reference/project.md) | ry init・package.toml の仕様 |
| [契約による設計](reference/contracts.md) | require・ensure・invariant・old・result |
| [ディレクティブ](reference/directives.md) | @deprecated とコンパイル時メタデータ |
| [エラー一覧](reference/errors.md) | コンパイルエラーと実行時エラーの説明 |
