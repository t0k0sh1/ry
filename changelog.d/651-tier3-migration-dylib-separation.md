### Changed

- Stdlib native dispatch migrated to table-driven architecture for net, http, and thread packages (#651)
- Stdlib `.ry` declarations updated from `@native` to `@native("libname")` for dynamic library resolution (#651)
- Stdlib runtime implementations separated from the static compiler library into shared libraries (#651)

### Removed

- Legacy `native_fn_arg_counts_` dispatch guard replaced by `native_fn_sigs_` (#651)
- Removed dedicated codegen dispatch files for base64, filesystem, and gc packages (now handled by generic native dispatch) (#651)
