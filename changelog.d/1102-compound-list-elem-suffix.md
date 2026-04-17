### Fixed

- `List<u8>` / `List<i8>` compound assignment (`bs += [99]`) no longer raises "list concatenation requires matching element types"; element suffix propagation now covers compound-op branches for both local variables and module-global write-through (#1102)
