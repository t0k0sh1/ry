### Internal

- Added `docs/architecture/implicit-any-paths.md`: a 10-path inventory of implicit `any` creation/propagation sites, each classified as keep / warn / deprecate / remove with pointers to the follow-up migration issues (#2321, #2322, #2323) and the tests that lock the current behavior. Three sub-cases of the implicit `any` → concrete unwrap path (lambda-call arg, named-call arg, `Ok`/`Err` payload into typed `Result` slot) were previously silent and now have spec coverage. (#2320)
- Added `tests/spec/any.test.ry` describe blocks "any unwrap in lambda call" and "any unwrap into Result slot" that capture the runtime behavior of those previously-silent sites so #2321 can stage warnings without regressing the existing roundtrip. (#2320)
