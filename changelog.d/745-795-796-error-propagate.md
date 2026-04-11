### Added

- `?` operator now accepts `Option<T>` operands in addition to `Result<T, E>`. When used on a `Some(v)` it evaluates to `v`; when used on a `None` the enclosing function returns `None` early. The enclosing function must declare an `Option` return type. `!!` is an alias with identical semantics. (#795)
- `??` operator now accepts `Result<T, E>` on the left-hand side in addition to `Option<T>`. For `Ok(v)` it evaluates to `v`; for `Err(_)` it evaluates to the right-hand default (the error value is discarded). (#796)
- `?` / `!!` can now be used directly at the top level of a script. When the operand is `Err(e)` or `None`, the error message is written to stderr and the process exits with status `1`. `__ry_main__`'s existing return-type contract is unchanged. (#745)

### Changed

- Error messages for `?` and `??` operator misuse now mention both `Option` and `Result` in the offending context.
