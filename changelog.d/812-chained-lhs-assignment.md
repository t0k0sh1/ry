### Fixed

- Chained assignment targets are now accepted by the parser and codegen,
  including `list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, and
  compound forms such as `list[i] += v` and `record.field[i] *= v` (#812).
  Previously these raised "expected '=' after index expression" or
  "expected '=' after field name". Compound assignment to a missing map key
  (`m["absent"] += 1`) now produces a clear runtime error instead of
  silently inserting a default value.
