### Fixed

- `from <mod> import <Type>` for C++-registered resource types
  (`File` / `TcpListener` / `TcpStream` / `TlsStream` / `HttpRequest` /
  `HttpResponse` / `HttpClientResponse` / `Thread` / `Lock` / `RWLock` /
  `Semaphore` / `Barrier` / `AtomicInt` / `AtomicBool`) and the
  builtin `regex.Match` record now succeeds, restoring symmetry with
  `@native fn` imports. Previously `extractDefinitions` only scanned
  `.ry` AST top-level declarations and rejected names registered via
  `ResourceKindRegistry` or `CodeGen`'s constructor, surfacing a
  misleading "typo? deprecated?" diagnostic. `module_loader.cpp` now
  bypasses the rejection for those names when the import path matches
  the registered `library` (gated by `from_stdlib=true` so local
  `<mod>.ry` shadows continue to enforce the .ry-source name set).
  Alias support (`from io import File as MyFile`) is also wired:
  `TypeAlias` validation in `emitImportAliasStmt` accepts an `orig`
  resolved via `ResourceKindRegistry`, and `registerResourceByTypeName`
  prefixes `resolveTypeAlias` so an aliased type name still receives
  resource-kind metadata for ARC cleanup. Concurrently fixed a
  pre-existing hardcoded `if (resolved == "File")` compare in
  `emitPatternBindingArc` (`src/codegen_match.cpp:795`) by normalising
  through `resolveTypeAlias` first, preventing handle leaks when a
  `Result<MyFile, Error>` is destructured via `Ok(f)`. (#1888)
