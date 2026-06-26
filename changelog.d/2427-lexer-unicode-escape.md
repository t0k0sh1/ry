### Added

- Ry 文字列リテラルに `\u{HHHH}` Unicode escape を実装 (`src/lexer/lexer.cpp`)。1 〜 6 桁の hex を `{...}` で囲み、対応する Unicode scalar value を UTF-8 として decode する (例: `"\u{1F600}"` → 😀 = `\xF0\x9F\x98\x80`)。regular string (~672)、block string (~582)、f-string (~829) の 3 つの escape switch 全てから共通の `decodeUnicodeEscape` + `appendUtf8` ヘルパを呼ぶ実装で乖離を防止。バリデーションは `0x10FFFF` を上限、surrogate range `0xD800..0xDFFF`、`\u{}` 空、`\u41` (`{`欠落)、`\u{41` (`}`欠落)、`\u{ZZZ}` (非 hex)、`\u{1234567}` (7 桁以上) を全て構造化エラーに変換する。raw string (`r"..."`) は escape 非処理の既存契約を維持し `\u{...}` を literal の 10 byte として保持する。

### Fixed

- `ry fmt` が `\u{HHHH}` Unicode escape を含む合法な Ry プログラムを `unknown escape sequence '\u'` で reject していたバグを修正 (#2427)。PR #2373 (closes #2326) の "Notes on known-fmt issues" で fmt 単独の欠陥として宣言されていたが、原因は fmt と `ry run` が共有する lexer が `\u` 自体を escape として認識していなかったことで (fmt 専用 string parser は存在せず `Formatter::escapeString` の default arm に decode 済み UTF-8 byte を素通しさせるだけ)、lexer 側に escape を実装したことで自動的に fmt も close criteria を満たすようになった。fmt は decode 済み literal UTF-8 (`"😀"`) を出力するため、二度目の format pass は固定点となり (`Formatter::verifyFormatting` で idempotency 検証) round-trip も保持される。 (#2427)
