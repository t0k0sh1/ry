/*
 * tree-sitter external scanner for Ry (v0.0.17)
 *
 * Implements the externals declared in grammar.js:
 *
 *   index 0: _indent          virtual block-open token (Python-style)
 *   index 1: _dedent          virtual block-close token
 *   index 2: _newline         virtual statement separator
 *   index 3: _fstring_start   `f"...{`  segment
 *   index 4: _fstring_mid     `}...{`   segment
 *   index 5: _fstring_end     `}..."`   segment (or `f"..."` with no interp)
 *   index 6: _regex_literal   `/pattern/`
 *
 * The grammar.js file references these as `$._indent` etc. tree-sitter
 * matches them by ordinal index in the externals array.
 *
 * Behavior follows src/lexer.cpp from the ry-2 reference implementation:
 *   - Indent stack tracks leading-space columns (tabs not used by Ry).
 *   - Blank / comment-only lines do not change indent state.
 *   - At EOF, all open blocks are closed by emitting DEDENTs.
 *   - Regex `/pattern/` is emitted only when valid_symbols[REGEX_LITERAL] is
 *     set — the parser turns this on at expression positions and off after
 *     value-producing tokens, so `a / b` lexes as division correctly.
 *   - F-string `f"..."` segments are split at every single `{` / `}`; doubled
 *     braces `{{` / `}}` escape to a literal brace and stay inside the segment.
 */

#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum TokenType {
  INDENT,
  DEDENT,
  NEWLINE,
  FSTRING_START,
  FSTRING_MID,
  FSTRING_END,
  REGEX_LITERAL,
};

#define MAX_INDENT_DEPTH 256

typedef struct {
  /* Indent stack: indents[0..indent_count-1] is the active sequence of
   * column thresholds, smallest at index 0 (the implicit 0 is NOT stored;
   * an empty stack means "at top level"). */
  uint16_t indent_count;
  uint16_t indents[MAX_INDENT_DEPTH];

  /* Number of additional DEDENTs to emit on subsequent scan calls without
   * advancing the lexer position. Used when one source-level dedent crosses
   * multiple stack levels (e.g., closing two nested blocks at once). */
  uint16_t pending_dedents;

  /* True after the scanner has emitted a virtual NEWLINE at EOF without
   * consuming any input character. Prevents infinite NEWLINE emission for
   * grammars whose top-level rule accepts repeated NEWLINEs (e.g.
   * `source_file: $ => repeat(choice($._newline, ...))`). Reset whenever
   * a DEDENT is emitted (a new NEWLINE may be needed after the DEDENT to
   * terminate an enclosing statement). */
  bool emitted_eof_newline;
} Scanner;

/* --------------------------------------------------------------------
 * tree-sitter lifecycle
 * ------------------------------------------------------------------ */

void *tree_sitter_ry_external_scanner_create(void) {
  Scanner *s = (Scanner *)calloc(1, sizeof(Scanner));
  return s;
}

void tree_sitter_ry_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_ry_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *s = (Scanner *)payload;
  unsigned size = 0;

  /* pending_dedents (1 byte; capped at 255 — practical depth limit). */
  uint16_t pd = s->pending_dedents > 255 ? 255 : s->pending_dedents;
  buffer[size++] = (char)pd;

  /* emitted_eof_newline (1 byte). */
  buffer[size++] = (char)(s->emitted_eof_newline ? 1 : 0);

  /* indent_count (2 bytes, little-endian). */
  buffer[size++] = (char)(s->indent_count & 0xFF);
  buffer[size++] = (char)((s->indent_count >> 8) & 0xFF);

  /* indents (2 bytes each). Truncate at the serialization buffer limit. */
  for (uint16_t i = 0; i < s->indent_count; i++) {
    if (size + 2 > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) break;
    uint16_t v = s->indents[i];
    buffer[size++] = (char)(v & 0xFF);
    buffer[size++] = (char)((v >> 8) & 0xFF);
  }

  return size;
}

void tree_sitter_ry_external_scanner_deserialize(void *payload,
                                                 const char *buffer,
                                                 unsigned length) {
  Scanner *s = (Scanner *)payload;
  s->indent_count = 0;
  s->pending_dedents = 0;
  s->emitted_eof_newline = false;

  if (length == 0) return;

  unsigned off = 0;
  s->pending_dedents = (uint8_t)buffer[off++];

  if (off >= length) return;
  s->emitted_eof_newline = (buffer[off++] != 0);

  if (off + 2 > length) return;
  uint16_t lo = (uint8_t)buffer[off++];
  uint16_t hi = (uint8_t)buffer[off++];
  s->indent_count = (uint16_t)(lo | (hi << 8));
  if (s->indent_count > MAX_INDENT_DEPTH) s->indent_count = MAX_INDENT_DEPTH;

  for (uint16_t i = 0; i < s->indent_count; i++) {
    if (off + 2 > length) {
      s->indent_count = i;
      break;
    }
    uint16_t l = (uint8_t)buffer[off++];
    uint16_t h = (uint8_t)buffer[off++];
    s->indents[i] = (uint16_t)(l | (h << 8));
  }
}

/* --------------------------------------------------------------------
 * lexer helpers
 * ------------------------------------------------------------------ */

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

/* --------------------------------------------------------------------
 * Regex literal
 *
 * `/pattern/` — content is any char except unescaped `/` or newline.
 * `\X` escape passes through verbatim (the runtime regex compiler validates
 * the actual escape semantics; the lexer only needs to not terminate the
 * literal on `\/`).
 * ------------------------------------------------------------------ */

static bool scan_regex(TSLexer *lexer) {
  if (lexer->lookahead != '/') return false;
  advance(lexer);  /* opening '/' */

  bool empty = true;
  while (lexer->lookahead != 0 &&
         lexer->lookahead != '\n' && lexer->lookahead != '\r') {
    if (lexer->lookahead == '/') break;
    if (lexer->lookahead == '\\') {
      advance(lexer);
      if (lexer->lookahead == 0 ||
          lexer->lookahead == '\n' || lexer->lookahead == '\r') {
        return false;  /* unterminated escape */
      }
      advance(lexer);
    } else {
      advance(lexer);
    }
    empty = false;
  }

  if (lexer->lookahead != '/' || empty) return false;
  advance(lexer);  /* closing '/' */
  lexer->result_symbol = REGEX_LITERAL;
  return true;
}

/* --------------------------------------------------------------------
 * F-string segment
 *
 * Reads from current position (just past `f"` for is_start, just past `}`
 * for continuation) until either:
 *   - single `{` -> emit FSTRING_START / FSTRING_MID (interp follows)
 *   - single `"` -> emit FSTRING_END (literal complete)
 *
 * `{{` / `}}` escape to literal brace and continue the segment.
 * `\X` escape passes through (runtime validates actual semantics).
 * Unmatched single `}` mid-segment is an error.
 * Newline mid-segment is an error (no multi-line f-strings).
 * ------------------------------------------------------------------ */

static bool scan_fstring_segment(TSLexer *lexer, bool is_start) {
  /* Continuation after an interpolation: the parser may call us with
   * lookahead at the `}` that closes the prior interp (consume it,
   * then read the trailing segment), OR with lookahead at `"` when
   * the grammar's optional interp clause was not taken — i.e. an
   * f-string with no interpolation at all (e.g. `f"hello"`). In the
   * `"` case fall through and let the main loop emit FSTRING_END. */
  if (!is_start && lexer->lookahead == '}') {
    advance(lexer);
  }
  while (lexer->lookahead != 0) {
    if (lexer->lookahead == '\n' || lexer->lookahead == '\r') {
      return false;  /* unterminated f-string */
    }
    if (lexer->lookahead == '\\') {
      advance(lexer);
      if (lexer->lookahead == 0) return false;
      advance(lexer);
      continue;
    }
    if (lexer->lookahead == '{') {
      advance(lexer);
      if (lexer->lookahead == '{') {
        advance(lexer);  /* `{{` -> literal `{` */
        continue;
      }
      lexer->result_symbol = is_start ? FSTRING_START : FSTRING_MID;
      return true;
    }
    if (lexer->lookahead == '}') {
      advance(lexer);
      if (lexer->lookahead == '}') {
        advance(lexer);  /* `}}` -> literal `}` */
        continue;
      }
      return false;  /* unmatched `}` in f-string */
    }
    if (lexer->lookahead == '"') {
      if (is_start) {
        /* Closing `"` reached without any interpolation. Don't consume it —
         * leave it for the next scan to emit FSTRING_END. The current token
         * is the FSTRING_START segment (`f"...`); the closing `"` is a
         * separate FSTRING_END token per the grammar's
         * seq(_fstring_start, ..., _fstring_end) shape. */
        lexer->result_symbol = FSTRING_START;
        return true;
      }
      advance(lexer);
      lexer->result_symbol = FSTRING_END;
      return true;
    }
    advance(lexer);
  }
  return false;  /* hit EOF before closing `"` */
}

/* --------------------------------------------------------------------
 * Main scan
 * ------------------------------------------------------------------ */

bool tree_sitter_ry_external_scanner_scan(void *payload, TSLexer *lexer,
                                          const bool *valid_symbols) {
  Scanner *s = (Scanner *)payload;

  /* 1. Drain any DEDENTs queued by a prior scan call. These are zero-width
   *    and consume no characters. The indent stack was already popped in
   *    full when the queue was created (see Case B); drain only emits the
   *    queued tokens, it does NOT pop indents again. Emitting a DEDENT
   *    re-arms eof-NEWLINE (any statement enclosing the just-closed block
   *    may now need its terminating NEWLINE). */
  if (s->pending_dedents > 0 && valid_symbols[DEDENT]) {
    s->pending_dedents--;
    s->emitted_eof_newline = false;
    lexer->result_symbol = DEDENT;
    return true;
  }

  /* 2. F-string continuation (after the parser consumed a `}` token,
   *    lookahead is at the first char of the next segment). */
  if ((valid_symbols[FSTRING_MID] || valid_symbols[FSTRING_END]) &&
      lexer->lookahead != '{' && lexer->lookahead != 0) {
    /* Don't intercept if lookahead is `{` — that's an empty interp `}{` which
     * is a parse error anyway, but keep grammar-handled. In practice the
     * parser arrives here only after a real `}` was consumed.
     * NOTE: scan_fstring_segment must succeed unconditionally here because
     * the grammar requires either FSTRING_MID or FSTRING_END to follow the
     * inner expression. */
    return scan_fstring_segment(lexer, /* is_start */ false);
  }

  bool any_block_token = valid_symbols[INDENT] ||
                         valid_symbols[DEDENT] ||
                         valid_symbols[NEWLINE];
  bool fstring_start_possible = valid_symbols[FSTRING_START];
  bool regex_possible = valid_symbols[REGEX_LITERAL];

  /* 3+4. Indent / Dedent / Newline, f-string opener, or regex literal.
   *      If the parser doesn't want any of these, the scanner has nothing
   *      to do — fall back to the internal lexer. The lookahead-based
   *      checks (`f` for f-string, `/` for regex) cannot happen yet
   *      because horizontal whitespace before the trigger char has not
   *      been skipped (the parser may call us at the space before
   *      `f"..."` in `= f"hello"`, or before `/regex/`); we re-check
   *      after skipping. */
  if (!any_block_token && !fstring_start_possible && !regex_possible)
    return false;

  /* Skip horizontal whitespace, newlines, blank lines, and comment-only
   * lines. Track whether we crossed at least one newline — INDENT and
   * NEWLINE require it; DEDENT can also fire without if a prior NEWLINE
   * scan already consumed all newlines and left lookahead at content of
   * an outer-level line. */
  bool seen_newline = false;
  for (;;) {
    if (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
      skip(lexer);
    } else if (lexer->lookahead == '\n') {
      seen_newline = true;
      skip(lexer);
    } else if (lexer->lookahead == '\r') {
      seen_newline = true;
      skip(lexer);
      if (lexer->lookahead == '\n') skip(lexer);
    } else if (lexer->lookahead == '#') {
      while (lexer->lookahead != 0 &&
             lexer->lookahead != '\n' && lexer->lookahead != '\r') {
        skip(lexer);
      }
    } else {
      break;
    }
  }

  /* Regex literal — only after whitespace skipping so that `= /pat/`
   * (with the parser entering at the space) reaches `/`. scan_regex
   * starts from the opening `/` and consumes through the closing `/`. */
  if (regex_possible && lexer->lookahead == '/') {
    if (scan_regex(lexer)) {
      lexer->mark_end(lexer);
      return true;
    }
    return false;
  }


  /* Mark the virtual token boundary at the first non-blank content (or EOF).
   * INDENT/DEDENT/NEWLINE produce zero-width tokens at this position. */
  lexer->mark_end(lexer);

  uint32_t current_col;
  bool at_eof = (lexer->lookahead == 0);
  if (at_eof) {
    current_col = 0;  /* EOF treated as logical column 0 to flush the stack */
  } else {
    current_col = lexer->get_column(lexer);
  }

  uint16_t top = s->indent_count > 0 ? s->indents[s->indent_count - 1] : 0;

  /* Case A: deeper indent — open a new block. INDENT only after a newline
   * and only when the parser actually wants one: mid-line indent jumps
   * don't exist, and at EOF we should not push a new block we'll never
   * close. */
  if (current_col > top && seen_newline && valid_symbols[INDENT] && !at_eof) {
    if (s->indent_count < MAX_INDENT_DEPTH) {
      s->indents[s->indent_count++] = (uint16_t)current_col;
    }
    lexer->result_symbol = INDENT;
    return true;
  }

  /* Case B: shallower indent — close one or more blocks. Does NOT require
   * seen_newline because a prior scan may have consumed the newline as a
   * NEWLINE token, leaving us mid-stream at the next line's content; we
   * still need to close blocks down to the current column. */
  if (current_col < top && valid_symbols[DEDENT]) {
    uint16_t popped = 0;
    while (s->indent_count > 0 &&
           s->indents[s->indent_count - 1] > current_col) {
      s->indent_count--;
      popped++;
    }
    if (popped > 0) {
      s->pending_dedents = (uint16_t)(popped - 1);
      s->emitted_eof_newline = false;
      lexer->result_symbol = DEDENT;
      return true;
    }
  }

  /* Case C: saw a newline (or hit EOF) and the parser wants a statement
   * separator. Indent-aligned-or-mismatched is fine here — Cases A/B took
   * priority for actual indent changes. */
  if (seen_newline && valid_symbols[NEWLINE]) {
    s->emitted_eof_newline = false;  /* real newline resets the eof guard */
    lexer->result_symbol = NEWLINE;
    return true;
  }

  /* Case D: virtual NEWLINE at EOF. Trailing statements that nest inside
   * multi-line expressions (e.g. `return case x: ...`) need a `_newline`
   * to terminate them after the inner block-form expression has consumed
   * all available `\n` characters via its arms. Emit at most one such
   * virtual NEWLINE per EOF position; the guard is reset by every DEDENT
   * (each closed block exposes a new statement that may need terminating)
   * and by every real NEWLINE elsewhere in the file. Without the guard a
   * top-level rule like `repeat($._newline)` would request NEWLINE
   * indefinitely. */
  if (at_eof && valid_symbols[NEWLINE] && !s->emitted_eof_newline) {
    s->emitted_eof_newline = true;
    lexer->result_symbol = NEWLINE;
    return true;
  }

  /* Case E: f-string opener — only reached when no block-token case fired
   * AND the lookahead is `f` AND FSTRING_START is valid. The whitespace
   * skip above leaves the position unchanged when the original lookahead
   * was `f` (since `f` is not whitespace), so we can speculatively probe
   * for `f"`. If the next char is not `"`, we've consumed `f` from the
   * scanner's view but tree-sitter discards the consumed range on `false`
   * return — the parser then falls back to its internal lexer at `f`. */
  if (valid_symbols[FSTRING_START] && lexer->lookahead == 'f') {
    advance(lexer);
    if (lexer->lookahead != '"') return false;
    advance(lexer);
    if (!scan_fstring_segment(lexer, /* is_start */ true)) return false;
    /* The mark_end above (before Case E) was for zero-width block tokens; it
     * left the token end at 'f', so without re-marking after consuming the
     * f-string opener the FSTRING_START token would be zero-width and the
     * parser would re-read 'f' as an identifier. */
    lexer->mark_end(lexer);
    return true;
  }

  return false;
}
