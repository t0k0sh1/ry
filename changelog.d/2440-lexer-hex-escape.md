### Added

- Ry 文字列リテラルに `\xNN` hex escape を実装 (`src/lexer/lexer.cpp`)。`\xNN` は厳密に 2 桁の hex digit を要求し、対応する単一バイト (0x00 〜 0xFF) を文字列に追加する (例: `"\x41"` → `"A"`, `"\xFF"` → 単一バイト `0xFF`)。`\u{HHHH}` が UTF-8 encode 済みの Unicode code point を生成するのと異なり、`\xNN` は生のバイト 1 つを生成するため、`\x80` 〜 `\xFF` 単独では valid UTF-8 にならない点も既存の `\0` と同じ扱い。regular string、block string、f-string の 3 つの escape switch 全てから共通の `decodeHexEscape` ヘルパを呼ぶ実装で乖離を防止。バリデーションは EOF mid-escape、hex digit 不足 (例: `"\x4"`)、非 hex digit (例: `"\xZZ"`, `"\x4Z"`) を構造化エラーに変換する。raw string (`r"..."`) は escape 非処理の既存契約を維持し `\x41` を literal の 4 byte として保持する。

### Fixed

- 文字列リテラル中の `\xNN` hex escape が `unknown escape sequence '\x'` で reject されていたバグを修正 (#2440)。`docs/reference/builtins-string.md:20` は `\xNN` が標準 escape 集合に含まれると明記していたが、PR #2427 (`\u{HHHH}`) と同様に lexer の 3 つの escape switch のいずれにも `case 'x':` が存在しなかった。`ry fmt` も同じ lexer を共有するため自動的に `\xNN` を受理するようになり、decode 済み literal byte を出力する (例: `"\x41"` → `"A"`)。`\xFF` のように非 UTF-8 single byte に decode される escape は fmt 出力後に source file が非 UTF-8 になるが、format 自体は exit 0 で idempotent (`\u{FF}` が valid な 2-byte UTF-8 に decode されるのと対照的)。
