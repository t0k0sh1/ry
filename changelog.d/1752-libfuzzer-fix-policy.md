### Changed

- `pre-commit-checklist` §3.6 (libFuzzer) reframes the crash-handling
  policy to match §3.5 (TSan): hard-to-reproduce crashes (sanitizer /
  fuzz output that does not reliably reproduce locally) are now fixed
  in the current PR to capture the reproduction window, instead of
  being deferred to a separate issue. This aligns with the new
  `/triage-side-finding` Q1 short-circuit (formerly `/scope-out-issue`)
  and removes the prior asymmetry where TSan races required immediate
  fixes but libFuzzer crashes were routed to follow-up issues. Crash
  inputs are still saved to `tests/fuzz/regressions/<name>/` and
  `tests/fuzz/corpus/<name>/` regardless of fix timing. (#1752)
