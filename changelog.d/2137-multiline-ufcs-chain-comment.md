### Fixed

- Parser: a `#` comment-only line inserted between hops of a multiline UFCS chain (e.g. `xs\n    # skip empty\n    .iter()\n    .toList()`) no longer breaks the chain with `unexpected token`. The lexer now suppresses the trailing `Newline` of comment-only lines so the chain's drain loop sees the same token stream as the comment-free form; blank-line separators are still rejected as before. Follow-up to #2121 / #2136. (#2137)
