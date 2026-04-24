### Fixed

- Lambda return-type inference now correctly narrows `@native` overloads that differ only in ptr-backed argument types (`str` vs `List` vs `Map` vs `Set`). Previously `f = () => length(xs)` failed with "ambiguous @native call in lambda return-type inference". Captured collection variables also retain their source-level element/key/value type metadata so the body dispatches to the correct runtime overload. (#1349)
