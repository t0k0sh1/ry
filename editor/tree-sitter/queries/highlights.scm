; tree-sitter-ry highlight queries
;
; Capture names follow nvim-treesitter / Helix conventions.
; Reorder/edit per editor preference; later captures override earlier ones.

; ---------- Comments ----------
(comment) @comment

; ---------- Literals ----------
(integer_literal) @number
(float_literal)   @number.float
(string_literal)  @string
(regex_literal)   @string.regexp
(boolean_literal) @boolean
(none_literal)    @constant.builtin

; F-string interpolation: outer quotes/braces are part of the literal,
; inner expressions are expressions and inherit their own captures.
(f_string) @string

; ---------- Keywords ----------
[
  "import"
  "from"
] @keyword.import

; Qualified import: `import math` — module identifier is a namespace.
(qualified_import_statement
  module: (identifier) @module)

(qualified_import_statement
  alias: (identifier) @module)

; Selective import alias: `from x import foo as bar` — capture the alias
; symmetrically with `qualified_import_statement` above. The alias target is
; not always a module (could be a fn / const / type), but we accept the same
; semantic compromise as the qualified-import captures noted below.
(import_item
  alias: (identifier) @module)

; Treat the module access prefix `math.sqrt(...)` / `math.PI` as a namespace
; reference. Tree-sitter cannot distinguish stdlib qualified-call receivers
; from generic field-access objects at the syntactic level, so this falls
; back to (identifier) @variable below for non-import positions. Editors
; that prefer a more conservative highlight may drop these two captures.

[
  "fn"
  "record"
  "enum"
  "type"
  "using"
] @keyword

"return" @keyword.return

[
  "if"
  "else"
  "case"
] @keyword.conditional

[
  "for"
  "while"
  "in"
] @keyword.repeat

(break_statement) @keyword.repeat
(continue_statement) @keyword.repeat

[
  "and"
  "or"
  "not"
  "as"
] @keyword.operator

[
  "async"
  "await"
  "weak"
] @keyword.modifier

[
  "require"
  "ensure"
  "invariant"
  "expect"
] @keyword

; ---------- Types ----------
(primitive_type) @type.builtin

(named_type
  (identifier) @type)

; ---------- Functions ----------
(function_declaration
  name: (function_name (identifier) @function))

(function_declaration
  name: (function_name (operator_name) @function))

(call_expression
  function: (qualified_identifier (identifier) @function.call .))

; Method-call style: x.foo(...) — capture the trailing field as the call.
(call_expression
  function: (field_access
    field: (identifier) @function.method.call))

(parameter
  name: (identifier) @variable.parameter)

(lambda_param
  name: (identifier) @variable.parameter)

; ---------- Decorators / attributes ----------
(decorator
  "@" @attribute
  (identifier) @attribute
  (#set! "priority" 105))  ; outrank the generic `(identifier) @variable` fallback (default priority 100)

; ---------- Records / Enums ----------
(record_declaration
  name: (identifier) @type)

(enum_declaration
  name: (identifier) @type)

(enum_variant
  name: (identifier) @constructor)

(record_field
  name: (identifier) @property)

(variant_field
  name: (identifier) @property)

(constructor_pattern
  (qualified_identifier (identifier) @constructor .))

; ---------- Field access ----------
(field_access
  field: (identifier) @property)

; ---------- Identifiers ----------
; Generic parameter names introduce types.
(generic_parameters (identifier) @type)

; Bindings and fallback identifiers as variables (after more-specific rules above).
(binding_pattern (identifier) @variable)
(identifier) @variable

; ---------- Operators ----------
[
  "+" "-" "*" "/" "%"
  "**" "//"
  "==" "!=" "<" ">" "<=" ">="
  "&" "|" "^" "~"
  "<<" ">>" ">>>"
  "??"
  "=" "+=" "-=" "*=" "/=" "%=" "**=" "//="
  "&=" "|=" "^=" "<<=" ">>="
  "->" "=>"
  ".."
  "?"
  "++"
  "--"
] @operator

; ---------- Punctuation ----------
[
  "," ":" "." "::"
] @punctuation.delimiter

[
  "(" ")"
  "[" "]"
  "{" "}"
] @punctuation.bracket

; ---------- Errors ----------
(ERROR) @error
