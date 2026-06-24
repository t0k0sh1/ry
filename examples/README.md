# Examples

Canonical, runnable examples that demonstrate Ry language features. Used as
documentation samples and as source material for LoRA fine-tuning in the
external `t0k0sh1/ry-code` project.

- The positive set is every `*.ry` file directly under `examples/` (non-recursive).
- Examples that intentionally demonstrate failures, or are under review, belong
  in `examples/negative/` or `examples/review/`; the verification script's
  non-recursive glob excludes them by construction.
- Verify the positive set locally with `bash scripts/check-examples.sh`
  (requires a built `ry` binary in `build-rust/` or `build/`).
