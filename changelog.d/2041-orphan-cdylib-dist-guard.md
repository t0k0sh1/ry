### Fixed

- A corrosion crate rename (`ry_codegen` → `emit`, #2040) leaves the old
  cdylib (`libry_codegen.{so,dylib}`) as an orphan in a non-clean build
  tree: a non-destructive `cmake` reconfigure self-heals `build.ninja` to
  `libemit` but does not garbage-collect the stale output, and
  `bundle-dist.sh`'s `libry_*` glob would then ship the dead cdylib in the
  release tarball. The packaging path now guards against this structurally
  using a zero-drift discriminator — the emission cdylib is the only native
  lib that links `libLLVM`, while stdlib `libry_*` libs do not:
  `scripts/verify-bundle.sh` (the pre-`tar` release gate) now FAILs if any
  bundled native lib other than `libemit` links `libLLVM`,
  `scripts/bundle-dist.sh` skips such orphans when copying (with a warning),
  and `.claude/skills/pre-commit-checklist/run-tests.sh` removes them from
  the host build tree after each build so a manual `--clean` is no longer
  required to clear the orphan. `ADDITIONAL_CLEAN_FILES` was intentionally
  not used: it is keyed to the current target name and cannot retroactively
  GC a renamed-away output. (#2041)
