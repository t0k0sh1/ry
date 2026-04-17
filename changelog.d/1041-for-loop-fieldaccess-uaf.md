### Fixed

- For-loop UAF guard now fires for `FieldAccessExpr` iterables
  (e.g. `for x in obj.items: append!(obj.items, ...)`), not only bare
  variable references (#1041).
