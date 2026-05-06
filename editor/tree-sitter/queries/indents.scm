; tree-sitter-ry indent queries
;
; Capture names follow nvim-treesitter rewrite (main-branch) conventions
; (see runtime/queries/python/indents.scm in nvim-treesitter for prior art):
; @indent.begin  — open a new indent level for nodes spanning multiple lines.
; @indent.branch — re-align the current line with the parent on this token
;                  (used for `else` and for closing brackets on their own line).
;
; The `#set! indent.immediate 1` directive makes the capture fire as soon
; as the parent node is recognized, even before its body span becomes
; multi-line. This is required for "press <CR> after `if cond:` to bump
; indent +1" — without it, the freshly typed single-line statement would
; not yet meet `@indent.begin`'s multi-line precondition.
;
; Ry is indentation-sensitive: INDENT / DEDENT / _newline are produced by
; src/scanner.c. The scanner suppresses INDENT/DEDENT inside ( [ { because
; the parser does not request them during expression parsing — that is
; why bracketed nodes get their own @indent.begin and the closing tokens
; get @indent.branch (so the closer on its own line returns to the
; opener's column, while the contents are bumped +1).
;
; Note: inside f-string interpolation, the closing brace is parsed as
; `_fstring_mid` / `_fstring_end`, NOT the anonymous "}" literal, so the
; `}` @indent.branch capture below never matches inside an f-string.
;
; Known limitation: a parenthesized single expression spanning multiple
; lines (e.g. `s = (\n  1\n  + 2\n)`) is parsed via the hidden grammar
; rule `_parenthesized`, which tree-sitter inlines into the parent
; expression. Hidden rules cannot be matched by capture queries, so the
; contents of such a parenthesized expression do NOT get auto-indented
; and the closing `)` does NOT auto-dedent. Multi-element tuple literals
; (`(a, b)` via `tuple_literal`) and call argument lists (`f(\n  a,\n)`
; via `call_expression`) are real grammar rules and ARE handled.

; ---------- Always-block-opening nodes ----------
; These nodes always introduce a `: INDENT body DEDENT` block, so we set
; indent.immediate to fire at parse time of the head, not after the body
; span exists.

((function_declaration body: (function_body)) @indent.begin
  (#set! indent.immediate 1))

((record_declaration) @indent.begin
  (#set! indent.immediate 1))

((record_invariant) @indent.begin
  (#set! indent.immediate 1))

((enum_declaration) @indent.begin
  (#set! indent.immediate 1))

((contract_clause) @indent.begin
  (#set! indent.immediate 1))

((if_statement) @indent.begin
  (#set! indent.immediate 1))

((while_statement) @indent.begin
  (#set! indent.immediate 1))

((for_statement) @indent.begin
  (#set! indent.immediate 1))

((case_match_statement) @indent.begin
  (#set! indent.immediate 1))

((case_cond_statement) @indent.begin
  (#set! indent.immediate 1))

((case_match_expression) @indent.begin
  (#set! indent.immediate 1))

((case_cond_expression) @indent.begin
  (#set! indent.immediate 1))

; ---------- Inline-or-block nodes ----------
; case_arm / case_cond_arm have both inline (`pat => expr`) and block
; (`pat =>: INDENT stmts DEDENT`) forms; lambda_expression with a block
; body is only considered when the body field is a block. Bare
; @indent.begin (without immediate) fires only when the node already
; spans multiple lines, which naturally matches the block forms.

(case_arm) @indent.begin

(case_cond_arm) @indent.begin

(lambda_expression body: (block)) @indent.begin

; ---------- Branch keyword ----------
; `else` / `else if` should align with the parent `if`. Capturing the
; bare "else" keyword inside if_statement covers both cases (Ry has no
; `elif`; `else if` is two tokens).

(if_statement "else" @indent.branch)

; ---------- Bracketed multi-line nodes ----------
; INDENT/DEDENT are suppressed inside ( [ { , so we need @indent.begin
; on the surrounding node (without indent.immediate) to bump contents +1
; for multi-line bracket literals / argument lists. The bare form fires
; only when the node already spans multiple lines, so single-line
; literals like `[1, 2, 3]` are unaffected.

(list_literal) @indent.begin

(map_literal) @indent.begin

(set_literal) @indent.begin

(tuple_literal) @indent.begin

(call_expression) @indent.begin

(index_expression) @indent.begin

; ---------- Closing brackets ----------
; The closing token of a multi-line bracketed node, on its own line,
; dedents back to the opener's column via @indent.branch (which only
; fires when the captured node starts on the target line).

[
  ")"
  "]"
  "}"
] @indent.branch
