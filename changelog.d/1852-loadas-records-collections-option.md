### Added

- Extended `json.loadAs[T](text)` and `json.loadAs[T](File)` to support
  user-defined records (flat and nested), typed collections of records
  (`List<Record>`, `Map<str, Record>`), and `Option<T>`. Each field of
  the parsed JSON object is looked up by name and recursively coerced
  into the declared type; missing fields, wrong-typed fields, and
  unsupported source shapes return `Err(Error{message})` with a
  `loadAs<T>: ...` prefix that locates the failure (e.g.
  `loadAs<Outer>: field 'inner': loadAs<Inner>: field 'age' missing`).
  `Option<T>` accepts JSON `null` as `Ok(None)` and any non-null shape
  as `Ok(Some(_))` (recursively coerced); a primitive source for
  `Option<Record>` errors with `loadAs<Option<X>>: expected null or
  loadAs<X>: expected JSON object`. Previously these targets crashed
  with `_Exit(1)` via the panic-version `unwrapFromAny`; the new
  `tryUnwrapFromAny` sibling routes Result-based propagation
  end-to-end. (#1852)
