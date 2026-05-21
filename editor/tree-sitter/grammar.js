/**
 * Tree-sitter grammar for Ry (v0.0.17)
 *
 * Conforms to grammar.ebnf in this directory. Verified against
 *   include/ry/lexer.hpp / src/lexer.cpp     (terminals & keyword table)
 *   src/parser*.cpp                          (statement & expression productions)
 *   tests/spec/*.test.ry                     (concrete syntax samples)
 *
 * Indentation handling
 * --------------------
 * Ry is indentation-sensitive. The lexer emits virtual
 * INDENT / DEDENT / NEWLINE tokens that this grammar declares as
 * `externals`. A C external scanner (src/scanner.c) is required for
 * `tree-sitter generate` to produce a buildable parser. This file
 * defines only the abstract grammar; the scanner implementation is
 * out of scope for the grammar definition itself.
 */

// Precedence table — lowest to highest. Mirrors the EBNF section 10 ladder.
const PREC = {
  conditional:    1,   // if-expr, case-expr, lambda
  coalesce:       2,   // ??
  or:             3,   // or
  and:            4,   // and
  not:            5,   // not
  comparison:     6,   // == != < <= > >= in
  bit_or:         7,   // |
  bit_xor:        8,   // ^
  bit_and:        9,   // &
  shift:         10,   // << >> >>>
  range:         11,   // ..
  additive:      12,   // + -
  multiplicative:13,   // * / // %
  power:         14,   // **           (right-assoc)
  unary:         15,   // - + ~ not await weak
  postfix:       16,   // call, index, . ? ++ --, as
  primary:       17,
};

const PRIMITIVE_TYPES = [
  'int', 'float', 'bool', 'str', 'Unit', 'any', 'Error',
  'i8', 'i16', 'i32', 'i64',
  'u8', 'u16', 'u32', 'u64',
  'f32', 'f64',
];

const COMPOUND_ASSIGN_OPS = [
  '+=', '-=', '*=', '/=', '%=',
  '**=', '//=',
  '&=', '|=', '^=',
  '<<=', '>>=',
];

export default grammar({
  name: 'ry',

  // Tokens supplied by the C external scanner (src/scanner.c).
  // Required for context-sensitive lexing — block structure
  // (INDENT/DEDENT/NEWLINE), f-string segment switching, and
  // disambiguating regex literals from the `/` operator. Without
  // src/scanner.c, the generated parser cannot recognize these.
  externals: $ => [
    $._indent,
    $._dedent,
    $._newline,
    $._fstring_start,
    $._fstring_mid,
    $._fstring_end,
    $._regex_literal,
  ],

  extras: $ => [
    $.comment,
    /[ \t\r]/,                  // whitespace; newlines are explicit via _newline
  ],

  // Make `identifier` the lexer's catch-all for any "word" token, so that
  // keywords like `if` lex as themselves rather than as identifiers.
  word: $ => $.identifier,

  conflicts: $ => [
    // LHS of assignment shares prefix with expression `a.b = c` vs `a.b` as expression.
    [$._lvalue, $.qualified_identifier],
    // `(a, b)` — tuple-destructure LHS vs lambda params vs paren-grouped expr.
    [$.tuple_destructure_statement, $.qualified_identifier, $.lambda_param],
    [$.qualified_identifier, $.lambda_param],
    // case-arm identifier: binding pattern `x: ...` vs nullary constructor `Foo: ...`.
    [$.binding_pattern, $.qualified_identifier],
    // `case ... :` can be a statement OR an expression with the same prefix.
    // prec.dynamic biases toward statement form in statement context.
    [$.case_match_statement, $.case_match_expression],
    [$.case_cond_statement, $.case_cond_expression],
  ],

  rules: {

    /* =====================================================================
     * Top level
     * ===================================================================*/
    source_file: $ => repeat(choice(
      $._newline,
      $._top_level_statement,
    )),

    _top_level_statement: $ => choice(
      $.import_statement,
      $.qualified_import_statement,
      $._statement,                          // _statement already includes _declaration
    ),

    /* =====================================================================
     * Imports
     * ===================================================================*/
    // Selective import: `from xxx import name [as alias] [, ...]`
    import_statement: $ => seq(
      'from',
      field('source', $.module_path),
      'import',
      field('items', $.import_list),
      $._newline,
    ),

    // Qualified import: `import xxx [as yyy]` (#1723, #1724).
    // The `as` alias form registers the alias as the effective name
    // (Python-style: `import math as m` makes `m.sqrt(...)` valid and
    // bare `math` undefined). Same-effective-name collisions are rejected.
    qualified_import_statement: $ => seq(
      'import',
      field('module', $.identifier),
      optional(seq('as', field('alias', $.identifier))),
      $._newline,
    ),

    module_path: $ => sep1($.identifier, '.'),

    import_list: $ => choice(
      sep1($.import_item, ','),
      // Multi-line brace tolerance (#1727): NEWLINE tokens fire inside `{}`
      // because the external scanner does not track bracket depth; absorb
      // them around the separator and at the brace boundaries. INDENT /
      // DEDENT are intentionally NOT absorbed — the indent stack must stay
      // clean. Mirrors C++ parser's `skipStructuralTokens` (parser.cpp:352).
      seq(
        '{',
        repeat($._newline),
        bracedSep1($.import_item, ',', $._newline),
        optional(','),
        repeat($._newline),
        '}',
      ),
    ),

    import_item: $ => seq(
      field('name', $.identifier),
      optional(seq('as', field('alias', $.identifier))),
    ),

    /* =====================================================================
     * Declarations
     * ===================================================================*/
    _declaration: $ => seq(
      repeat(seq($.decorator, $._newline)),
      choice(
        $.function_declaration,
        $.record_declaration,
        $.enum_declaration,
        $.type_alias_declaration,
        $.directive_def_declaration,
      ),
    ),

    decorator: $ => seq(
      '@',
      field('name', $.identifier),
      optional(seq(
        '(',
        optional($.decorator_arguments),
        ')',
      )),
    ),

    decorator_arguments: $ => sep1($.decorator_argument, ','),

    decorator_argument: $ => choice(
      seq(field('name', $.identifier), '=', field('value', $._expression)),
      field('value', $._expression),
    ),

    /* ------- Function declaration ------- */
    function_declaration: $ => seq(
      optional('async'),
      'fn',
      field('name', $.function_name),
      optional(field('generics', $.generic_parameters)),
      '(',
      optional(field('parameters', $.parameter_list)),
      ')',
      optional(seq('->', field('return_type', $._type))),
      choice(
        field('body', $.function_body),
        $._newline,                  // @native fn — no body
      ),
    ),

    // `identifier` already greedily absorbs a trailing `!` (see the
    // identifier terminal at the bottom), so no extra suffix is needed
    // for mutating-method names like `sort!`.
    function_name: $ => choice(
      $.identifier,
      $.operator_name,
    ),

    operator_name: $ => seq(
      'operator',
      choice(
        '+', '-', '*', '/', '%', '**', '//',
        '==', '!=', '<', '<=', '>', '>=',
        seq('[', ']', optional('=')),     // operator[]  /  operator[]=
        seq('(', ')'),                    // operator()
        'in', seq('not', 'in'),
        'and', 'or', 'not', 'as',
      ),
    ),

    parameter_list: $ => sep1($.parameter, ','),

    parameter: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $._type),
      optional(seq('=', field('default', $._expression))),
    ),

    generic_parameters: $ => seq(
      '<',
      sep1($.identifier, ','),
      '>',
    ),

    // Live-editing tolerance (#1623): the INDENT body is optional so that
    // partial input (`fn foo():` typed but body not yet started) produces
    // a complete `function_body` named node, allowing indents.scm
    // `@indent.begin` captures to fire. The Ry compiler enforces a
    // non-empty body at compile time. See editor/tree-sitter/README.md
    // §"Live-editing tolerance".
    function_body: $ => seq(
      ':',
      optional(seq(
        $._indent,
        repeat($.contract_clause),
        repeat1($._statement),
        $._dedent,
      )),
    ),

    contract_clause: $ => choice(
      seq(
        'require',
        ':',
        $._indent,
        $._condition_list,
        $._dedent,
      ),
      seq(
        'ensure',
        optional(field('binding', $.identifier)),
        ':',
        $._indent,
        $._condition_list,
        $._dedent,
      ),
    ),

    _condition_list: $ => repeat1(seq($._expression, $._newline)),

    /* ------- Record declaration ------- */
    record_declaration: $ => seq(
      'record',
      field('name', $.identifier),
      optional(field('generics', $.generic_parameters)),
      ':',
      $._indent,
      repeat($._record_member),
      $._dedent,
    ),

    _record_member: $ => choice(
      $.record_field,
      $.record_invariant,
      $._newline,
    ),

    record_field: $ => seq(
      repeat(seq($.decorator, $._newline)),
      field('name', $.identifier),
      ':',
      field('type', $._type),
      optional(seq('=', field('default', $._expression))),
      $._newline,
    ),

    record_invariant: $ => seq(
      'invariant',
      ':',
      $._indent,
      $._condition_list,
      $._dedent,
    ),

    /* ------- Enum declaration (ADT) ------- */
    enum_declaration: $ => seq(
      'enum',
      field('name', $.identifier),
      optional(field('generics', $.generic_parameters)),
      ':',
      $._indent,
      repeat1($.enum_variant),
      $._dedent,
    ),

    enum_variant: $ => seq(
      field('name', $.identifier),
      optional(seq(
        '(',
        optional($.variant_payload),
        ')',
      )),
      $._newline,
    ),

    variant_payload: $ => sep1($.variant_field, ','),

    variant_field: $ => choice(
      seq(field('name', $.identifier), ':', field('type', $._type)),
      field('type', $._type),
    ),

    /* ------- Type alias ------- */
    type_alias_declaration: $ => seq(
      'type',
      field('name', $.identifier),
      optional(field('generics', $.generic_parameters)),
      '=',
      field('type', $._type),
      $._newline,
    ),

    /* ------- @directive definition (#708) ------- */
    directive_def_declaration: $ => seq(
      '@directive',
      '(',
      $.decorator_arguments,
      ')',
      $.function_declaration,
    ),

    /* =====================================================================
     * Types
     * ===================================================================*/
    _type: $ => $.union_type,

    union_type: $ => prec.left(seq(
      $._primary_type,
      repeat(seq('|', $._primary_type)),
    )),

    _primary_type: $ => choice(
      $.primitive_type,
      $.named_type,
      $.tuple_type,
      $.function_type,
      $.weak_type,
      $.option_type,
      $.array_type,
    ),

    primitive_type: $ => choice(...PRIMITIVE_TYPES),

    named_type: $ => prec.right(seq(
      $.identifier,
      optional($.type_arguments),
      repeat(seq(
        '::',
        $.identifier,
        optional($.type_arguments),
      )),
    )),

    type_arguments: $ => seq(
      '<',
      sep1($._type, ','),
      '>',
    ),

    tuple_type: $ => prec(1, seq(
      '(',
      $._type,
      ',',
      optional(sep1($._type, ',')),
      ')',
    )),

    function_type: $ => seq(
      'fn',
      '(',
      optional(sep1($._type, ',')),
      ')',
      '->',
      $._type,
    ),

    weak_type: $ => prec.right(seq(
      'weak',
      $._primary_type,
    )),

    option_type: $ => prec(PREC.postfix, seq($._primary_type, '?')),

    array_type: $ => prec(PREC.postfix, seq(
      $._primary_type,
      '[',
      optional($.integer_literal),
      ']',
    )),

    /* =====================================================================
     * Statements
     * ===================================================================*/
    _statement: $ => choice(
      seq($._simple_statement, $._newline),
      $._compound_statement,
      $._declaration,
    ),

    _simple_statement: $ => choice(
      $.return_statement,
      $.break_statement,
      $.continue_statement,
      $.assignment_statement,
      $.compound_assignment,
      $.typed_binding_statement,
      $.tuple_destructure_statement,
      $.expect_statement,
      $.expression_statement,
    ),

    return_statement: $ => prec.right(seq(
      'return',
      optional(field('value', $._expression)),
    )),

    break_statement: $ => 'break',
    continue_statement: $ => 'continue',

    /* `name = value` and chained-postfix LHS assignment.
       (typed `name: T = value` is a separate rule to avoid LR(1) trouble.) */
    assignment_statement: $ => prec.right(PREC.conditional - 1, seq(
      field('left', $._lvalue),
      '=',
      field('right', $._expression),
    )),

    compound_assignment: $ => prec.right(PREC.conditional - 1, seq(
      field('left', $._lvalue),
      field('operator', choice(...COMPOUND_ASSIGN_OPS)),
      field('right', $._expression),
    )),

    /* `name: Type = value` — implicit-binding form (no `let` keyword) */
    typed_binding_statement: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $._type),
      '=',
      field('value', $._expression),
    ),

    /* Both forms: `(a, b) = expr`  and  `a, b = expr` */
    tuple_destructure_statement: $ => choice(
      seq(
        '(',
        sep1($.identifier, ','),
        ')',
        '=',
        field('value', $._expression),
      ),
      seq(
        $.identifier,
        repeat1(seq(',', $.identifier)),
        '=',
        field('value', $._expression),
      ),
    ),

    expect_statement: $ => seq(
      'expect',
      '(',
      field('actual', $._expression),
      ')',
      '.',
      field('matcher', $.identifier),
      optional(seq(
        '(',
        optional(sep1($._expression, ',')),
        ')',
      )),
    ),

    expression_statement: $ => $._expression,

    _lvalue: $ => choice(
      $.identifier,
      $._lvalue_postfix,
    ),

    _lvalue_postfix: $ => prec(PREC.postfix, choice(
      seq($._lvalue, '.', $.identifier),
      seq($._lvalue, '[', sep1($._expression, ','), ']'),
    )),

    /* ------- Compound statements ------- */
    _compound_statement: $ => choice(
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.case_statement,
      $.using_statement,
    ),

    // Live-editing tolerance (#1623): the consequence / alt_consequence /
    // alternative blocks are optional so that partial input (`if cond:` or
    // `else if other:` or `else:` typed but body not yet started) produces
    // a complete `if_statement` named node, allowing indents.scm
    // `@indent.begin` captures to fire. The trailing `:` after the final
    // `else` is also optional so that `else` typed on its own line is
    // absorbed into the `if_statement` and the existing
    // `(if_statement "else" @indent.branch)` capture in indents.scm
    // dedents to the parent `if`'s column. The Ry compiler enforces a
    // non-empty body at compile time. See editor/tree-sitter/README.md
    // §"Live-editing tolerance".
    // prec.right resolves the ambiguity introduced by relaxing the trailing
    // `else` to allow `:` to be missing: when the parser sees `else if`, it
    // could either continue this if_statement's else-if chain OR end this
    // if_statement (with bare `else`) and start a new top-level `if`.
    // Right-precedence biases toward continuing the chain, which is the
    // expected interpretation.
    if_statement: $ => prec.right(seq(
      'if',
      field('condition', $._expression),
      ':',
      optional(field('consequence', $.block)),
      repeat(seq(
        'else',
        'if',
        field('alt_condition', $._expression),
        ':',
        optional(field('alt_consequence', $.block)),
      )),
      optional(seq(
        'else',
        optional(seq(
          ':',
          optional(field('alternative', $.block)),
        )),
      )),
    )),

    // Live-editing tolerance (#1623): see if_statement note above.
    while_statement: $ => seq(
      'while',
      field('condition', $._expression),
      ':',
      optional(field('body', $.block)),
    ),

    // Live-editing tolerance (#1623): see if_statement note above.
    for_statement: $ => seq(
      'for',
      field('target', $._for_target),
      'in',
      field('iter', $._expression),
      ':',
      optional(field('body', $.block)),
    ),

    _for_target: $ => choice(
      $.identifier,
      seq('(', sep1($.identifier, ','), ')'),
      // bare-comma destructure: `for a, b in iter:`
      seq($.identifier, ',', sep1($.identifier, ',')),
    ),

    // Live-editing tolerance (#1623): see if_statement note above.
    using_statement: $ => seq(
      'using',
      field('name', $.identifier),
      '=',
      field('value', $._expression),
      ':',
      optional(field('body', $.block)),
    ),

    // Two forms: `case <expr>:` (scrutinee form, pattern-matching) and
    // `case:` (no scrutinee, condition-list form — each arm is a boolean
    // condition expression). They use different arm grammars so they are
    // surfaced as distinct rules joined under `case_statement`.
    case_statement: $ => choice(
      $.case_match_statement,
      $.case_cond_statement,
    ),

    // Live-editing tolerance (#1623): the INDENT arm list is optional so
    // that `case x:` typed alone produces a complete case_match_statement
    // named node. See editor/tree-sitter/README.md §"Live-editing tolerance".
    case_match_statement: $ => prec.dynamic(2, seq(
      'case',
      field('scrutinee', $._expression),
      ':',
      optional(seq(
        $._indent,
        repeat1($.case_arm),
        $._dedent,
      )),
    )),

    // Live-editing tolerance (#1623): see case_match_statement note above.
    case_cond_statement: $ => prec.dynamic(2, seq(
      'case',
      ':',
      optional(seq(
        $._indent,
        repeat1($.case_cond_arm),
        $._dedent,
      )),
    )),

    case_arm: $ => seq(
      field('pattern', choice($._pattern, $.or_pattern)),
      optional(seq('if', field('guard', $._expression))),
      ':',
      choice(
        // inline body: single statement on same line as the `:`.
        // The statement parser handles the trailing newline via `_newline`,
        // so do NOT add another `_newline` here (it would consume the next
        // arm's pattern lookahead position).
        field('body', $._statement),
        seq($._indent, repeat1($._statement), $._dedent),
      ),
    ),

    case_cond_arm: $ => seq(
      field('condition', choice($._expression, $.wildcard_pattern)),
      ':',
      choice(
        field('body', $._statement),
        seq($._indent, repeat1($._statement), $._dedent),
      ),
    ),

    block: $ => seq(
      $._indent,
      repeat1($._statement),
      $._dedent,
    ),

    /* =====================================================================
     * Patterns
     * ===================================================================*/
    _pattern: $ => choice(
      $.literal_pattern,
      $.wildcard_pattern,
      $.binding_pattern,
      $.tuple_pattern,
      $.constructor_pattern,
      $.range_pattern,
    ),

    /* `pat | pat | pat` — only valid as the top-level pattern of a
     * case_arm; OR alternatives must not contain bindings. The grammar
     * here permits any nested pattern; the constraint is enforced by
     * the parser at compile time. */
    or_pattern: $ => prec.left(seq(
      $._pattern,
      repeat1(seq('|', $._pattern)),
    )),

    literal_pattern: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      'true',
      'false',
      'none',
      seq('-', choice($.integer_literal, $.float_literal)),
    ),

    wildcard_pattern: $ => '_',

    binding_pattern: $ => $.identifier,

    tuple_pattern: $ => prec(2, seq(
      '(',
      $._pattern,
      ',',
      optional(sep1($._pattern, ',')),
      ')',
    )),

    constructor_pattern: $ => seq(
      $.qualified_identifier,
      optional(seq(
        '(',
        optional(sep1($._pattern, ',')),
        ')',
      )),
    ),

    qualified_identifier: $ => prec.right(seq(
      $.identifier,
      repeat(seq('::', $.identifier)),
    )),

    range_pattern: $ => seq(
      $.literal_pattern,
      '..',
      $.literal_pattern,
    ),

    /* =====================================================================
     * Expressions (precedence climbing — lowest to highest)
     * ===================================================================*/
    _expression: $ => choice(
      $.lambda_expression,
      $.if_expression,
      $.case_expression,
      $._conditional_expression,
    ),

    lambda_expression: $ => prec(PREC.conditional, choice(
      // bare-paren single-param form (#1572):  x => x + 1
      seq(
        field('parameter', $.identifier),
        '=>',
        field('body', $._lambda_body),
      ),
      // parenthesized form: (x, y) => x + y    or    (x: int) -> int => x
      seq(
        field('parameters', $.lambda_params),
        optional(seq('->', field('return_type', $._type))),
        '=>',
        field('body', $._lambda_body),
      ),
      // block-body form: (params) -> Type:\n  <indented body>
      seq(
        field('parameters', $.lambda_params),
        optional(seq('->', field('return_type', $._type))),
        ':',
        field('body', $.block),
      ),
    )),

    lambda_params: $ => seq(
      '(',
      optional(sep1($.lambda_param, ',')),
      ')',
    ),

    lambda_param: $ => seq(
      field('name', $.identifier),
      optional(seq(':', field('type', $._type))),
      optional(seq('=', field('default', $._expression))),
    ),

    _lambda_body: $ => choice($._expression, $.block),

    /* if-expression: `if cond => then else else_expr` (single-line sugar) */
    if_expression: $ => prec.right(PREC.conditional, seq(
      'if',
      field('condition', $._expression),
      '=>',
      field('consequence', $._expression),
      'else',
      field('alternative', $._expression),
    )),

    case_expression: $ => prec(PREC.conditional, choice(
      $.case_match_expression,
      $.case_cond_expression,
    )),

    case_match_expression: $ => prec.dynamic(1, seq(
      'case',
      field('scrutinee', $._expression),
      ':',
      $._indent,
      repeat1($.case_arm),
      $._dedent,
    )),

    case_cond_expression: $ => prec.dynamic(1, seq(
      'case',
      ':',
      $._indent,
      repeat1($.case_cond_arm),
      $._dedent,
    )),

    /* coalesce, logical, comparison, bitwise, shift, range, additive, mul, power */
    _conditional_expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $._postfix_expression,
    ),

    binary_expression: $ => choice(
      prec.right(PREC.coalesce,
        seq(field('left', $._expression), field('operator', '??'),  field('right', $._expression))),
      prec.left(PREC.or,
        seq(field('left', $._expression), field('operator', 'or'),  field('right', $._expression))),
      prec.left(PREC.and,
        seq(field('left', $._expression), field('operator', 'and'), field('right', $._expression))),
      ...[
        ['==', PREC.comparison],
        ['!=', PREC.comparison],
        ['<',  PREC.comparison],
        ['<=', PREC.comparison],
        ['>',  PREC.comparison],
        ['>=', PREC.comparison],
        ['|',  PREC.bit_or],
        ['^',  PREC.bit_xor],
        ['&',  PREC.bit_and],
        ['<<', PREC.shift],
        ['>>', PREC.shift],
        ['>>>',PREC.shift],
        ['..', PREC.range],
        ['+',  PREC.additive],
        ['-',  PREC.additive],
        ['*',  PREC.multiplicative],
        ['/',  PREC.multiplicative],
        ['//', PREC.multiplicative],
        ['%',  PREC.multiplicative],
      ].map(([op, p]) => prec.left(p,
        seq(field('left', $._expression), field('operator', op), field('right', $._expression)),
      )),
      // 'in' / 'not in' membership test
      prec.left(PREC.comparison,
        seq(field('left', $._expression),
            field('operator', choice('in', seq('not', 'in'))),
            field('right', $._expression))),
      // power is right-associative
      prec.right(PREC.power,
        seq(field('left', $._expression), field('operator', '**'), field('right', $._expression))),
    ),

    unary_expression: $ => prec.right(PREC.unary, seq(
      field('operator', choice('-', '+', '~', 'not', 'await', 'weak')),
      field('operand', $._expression),
    )),

    /* ------- Postfix chain: call, index, .field, ?, ++, --, as ------- */
    _postfix_expression: $ => choice(
      $._primary_expression,
      $.call_expression,
      $.index_expression,
      $.field_access,
      $.unwrap_expression,
      $.update_expression,
      $.cast_expression,
    ),

    call_expression: $ => prec(PREC.postfix, seq(
      field('function', $._postfix_expression),
      '(',
      optional(field('arguments', $.argument_list)),
      ')',
    )),

    index_expression: $ => prec(PREC.postfix, seq(
      field('object', $._postfix_expression),
      '[',
      sep1($._expression, ','),
      ']',
    )),

    field_access: $ => prec(PREC.postfix, seq(
      field('object', $._postfix_expression),
      '.',
      field('field', $.identifier),
    )),

    unwrap_expression: $ => prec(PREC.postfix, seq(
      $._postfix_expression,
      '?',
    )),

    update_expression: $ => prec(PREC.postfix, seq(
      $._postfix_expression,
      choice('++', '--'),
    )),

    cast_expression: $ => prec.left(PREC.postfix, seq(
      field('value', $._postfix_expression),
      'as',
      field('type', $._type),
    )),

    argument_list: $ => sep1($.argument, ','),

    argument: $ => choice(
      seq(field('name', $.identifier), '=', field('value', $._expression)),
      field('value', $._expression),
    ),

    /* ------- Primary ------- */
    _primary_expression: $ => choice(
      $._literal,
      $.qualified_identifier,
      $._parenthesized,
      $.tuple_literal,
      $.list_literal,
      $.map_literal,
      $.set_literal,
      $.f_string,
    ),

    _parenthesized: $ => seq('(', $._expression, ')'),

    tuple_literal: $ => choice(
      // 1-tuple `(e,)`
      seq('(', $._expression, ',', ')'),
      // n-tuple `(e1, e2, ...)`  (n >= 2)
      seq('(', $._expression, ',', sep1($._expression, ','), ')'),
    ),

    // Multi-line brace tolerance (#1727): see import_list note above.
    list_literal: $ => seq(
      '[',
      repeat($._newline),
      optional(seq(
        bracedSep1($._expression, ',', $._newline),
        optional(','),
        repeat($._newline),
      )),
      ']',
    ),

    map_literal: $ => seq(
      '{',
      repeat($._newline),
      bracedSep1(
        seq(field('key', $._expression), ':', field('value', $._expression)),
        ',',
        $._newline,
      ),
      optional(','),
      repeat($._newline),
      '}',
    ),

    set_literal: $ => choice(
      seq('{', repeat($._newline), '}'),               // empty (also empty-map; parser decides)
      seq(
        '{',
        repeat($._newline),
        bracedSep1($._expression, ',', $._newline),
        optional(','),
        repeat($._newline),
        '}',
      ),
    ),

    /* =====================================================================
     * Literals
     * ===================================================================*/
    // `Unit` is a type name only (used in `-> Unit`, `Result<Unit, _>`),
    // never a value-level literal — the singleton unit value cannot be
    // written directly in source. Keep it out of `_literal` and let the
    // type rule pick it up via `primitive_type`.
    _literal: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.regex_literal,
      $.boolean_literal,
      $.none_literal,
    ),

    boolean_literal: $ => choice('true', 'false'),
    none_literal:    $ => 'none',

    /* ------- Integer ------- */
    integer_literal: $ => token(seq(
      choice(
        /[0-9][0-9_]*/,
        /0x[0-9a-fA-F][0-9a-fA-F_]*/,
        /0b[01][01_]*/,
      ),
      optional(choice('i8','i16','i32','i64','u8','u16','u32','u64')),
    )),

    /* ------- Float ------- */
    float_literal: $ => token(seq(
      choice(
        seq(/[0-9][0-9_]*/, '.', /[0-9][0-9_]*/, optional(/[eE][+-]?[0-9][0-9_]*/)),
        seq(/[0-9][0-9_]*/, /[eE][+-]?[0-9][0-9_]*/),
      ),
      optional(choice('f32', 'f64')),
    )),

    /* ------- String -------
     * Standard double-quoted with escapes. Single-line only (no raw multi-line). */
    string_literal: $ => token(seq(
      '"',
      repeat(choice(
        /[^"\\\n]/,
        seq('\\', choice(
          /[ntr0\\"']/,
          /x[0-9a-fA-F]{2}/,
          /u\{[0-9a-fA-F]+\}/,
        )),
      )),
      '"',
    )),

    /* ------- Regex -------
     * `/pattern/flags`. Conflicts with `/` (division) — the external
     * scanner emits `_regex_literal` only when the parser state is
     * compatible with starting an expression. */
    regex_literal: $ => $._regex_literal,

    /* ------- f-string -------
     * The lexer (lexer.hpp / lexer.cpp readFStringSegment) emits three
     * specialized tokens for f-strings. The external scanner tracks
     * brace depth across `{...}` interpolations so closing `}` of the
     * interpolation can resume f-string lexing rather than ending a
     * map/set/block. */
    f_string: $ => seq(
      $._fstring_start,
      optional(seq(
        $._expression,
        repeat(seq($._fstring_mid, $._expression)),
      )),
      $._fstring_end,
    ),

    /* =====================================================================
     * Identifier & comment
     * ===================================================================*/

    /* Trailing `!` is greedily absorbed for mutating method names
     * (sort!, append!, reverse!, clear!). The lexer excludes `!=` from
     * this absorption (parser-conventions.md, #1211/#1568). */
    identifier: $ => token(seq(
      /[a-zA-Z_][a-zA-Z0-9_]*/,
      optional('!'),
    )),

    comment: $ => token(seq('#', /.*/)),
  },
});

/* =====================================================================
 * Helpers
 * ===================================================================*/
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

// Like `sep1` but tolerates the given newline token around the separator.
// Used inside brace-delimited expressions (list_literal, map_literal,
// set_literal, braced import_list) where the external scanner emits
// NEWLINE tokens even inside braces (#1727). Only the newline token is
// absorbed — INDENT / DEDENT must not be passed in here, so the scanner's
// indent stack stays clean.
function bracedSep1(rule, separator, newline) {
  return seq(
    rule,
    repeat(seq(repeat(newline), separator, repeat(newline), rule)),
  );
}
