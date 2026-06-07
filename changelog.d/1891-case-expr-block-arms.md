### Added

- `case` **expressions** now accept indented-block arms, not only single-line `pattern : value` arms — closing the asymmetry where only "case expression × indented block" was rejected (`unexpected token '\n'`) while the other three case-form / arm-notation combinations parsed. A block arm runs its intermediate statements and yields its **tail expression** as the arm's value (Rust/Scala `match`-style), so an arm that needs a local computation can be written directly:

  ```ry
  r = case x:
      1:
          tmp = x + 10
          tmp * 2          # tail expression — the arm's value
      _ : 0
  ```

  Covers both the subject form (`case x:`) and the no-subject condition form (`case:`), including the latter's `_:` else arm. The tail line is parsed as an expression, so an identifier-starting tail (`tmp * 2`) or a UFCS / method-call tail is accepted without parentheses — unlike `if`-expression block branches, which still require parenthesizing such a tail. A block whose final line produces no value (e.g. an assignment) is rejected at parse time with `case arm block must end with an expression`. Inline and block arms may be mixed within one `case`; the formatter canonicalizes a block arm with no intermediate statements back to the inline `pattern : value` form. (#1891)

### Fixed

- Fixed a pre-existing use-after-free in `if`-expression block branches: a branch that bound an ARC-managed value (`List` / `Map` / `Set` / `str` / `Option` payload) to a local and returned it as the parenthesized tail (e.g. `if c:` … `items = [...]` … `(items)`) released that binding when the branch scope closed, so the result came back freed/empty. The block-tail value is now retained before scope cleanup — the same escape-retain that makes the new `case`-expression block arms above sound. (#1891)
