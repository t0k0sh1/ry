---
paths:
  - "scripts/bundle-dist.sh"
  - "scripts/verify-bundle.sh"
  - ".github/workflows/release.yml"
---

# Distribution Packaging

### Release binaries bundle shared libLLVM; rpath rewrite happens in the packaging step, not CMake INSTALL_RPATH

**Source**: #2005 (2026-06-03, implementation)
**Tags**: release, packaging, rpath, libLLVM, cutover, distribution

**Context**: After the #1999 C++→Rust cutover, `ry` requires a shared
`libLLVM` at runtime (#1997). `release.yml` packages `build/ry`
**directly** (`cp build/ry dist/ry`), not via `cmake --install`, so the
shipped binary carries whatever rpath the build tree baked in. Without
bundling, the first tag push after cutover publishes a binary that fails to
start where libLLVM is absent (`dyld: Library not loaded` / `libLLVM.so: not
found`).

**Rule**:
- Bundling + rpath rewrite lives in `scripts/bundle-dist.sh` (called from
  `release.yml` Package and the CI `test` / `macos-smoke-rust` jobs);
  `scripts/verify-bundle.sh` asserts the result before `tar`.
- **macOS**: install names are absolute Mach-O paths, so `install_name_tool
  -change <abs> @rpath/...` is required per reference (an added rpath alone
  does not redirect them). Re-sign each rewritten Mach-O with `codesign
  --force --sign -` (install_name_tool invalidates the signature). libLLVM's
  only non-system chain dep is Homebrew `libzstd` → bundle it and `-change`
  libLLVM's reference to `@loader_path/libzstd.1.dylib`.
- **Linux**: set the rpath in CMake, NOT patchelf — `ry-ci-glibc-old` has no
  patchelf (no-apt rule). Use `set_target_properties(ry PROPERTIES
  BUILD_WITH_INSTALL_RPATH ON INSTALL_RPATH "...")`, NOT `BUILD_RPATH`.
  `BUILD_RPATH` is **additive** to CMake's automatic link-path rpath, so the
  shipped binary keeps build-machine-absolute paths (`/usr/local/llvm/lib`,
  `/opt/openssl/lib`, `build/lib`). `BUILD_WITH_INSTALL_RPATH ON` makes the
  build-tree binary link with `INSTALL_RPATH` *instead of* the auto set;
  `INSTALL_RPATH_USE_LINK_PATH` defaults OFF so nothing else is appended.
  libzstd is a **system** lib on the glibc-old image → NOT bundled
  (macOS-bundles / Linux-doesn't asymmetry is intended, matches the AC).
- **Two rpaths on `ry`** cover both layouts: `@loader_path/lib` /
  `$ORIGIN/lib` (tarball unpacked in place) and `@loader_path/../../.ry/lib`
  / `$ORIGIN/../../.ry/lib` (installed: `~/.local/bin/ry` → `~/.ry/lib`, a
  fixed offset since both sit under `$HOME`). `install.sh` and `ry
  self-update` (`install_native_libs` in `src/cli/self_update.cpp`) install
  the bundled libs into `~/.ry/lib` alongside `libry_*`.

**openssl is out of scope**: `ry_lib` PUBLIC-links OpenSSL, so `ry` NEEDs
libssl/libcrypto — but it did identically before the cutover (v0.0.25
parity), the AC targets only libLLVM's chain, and bundling crypto blocks OS
security updates. Clean-room tests hide ONLY libLLVM, leaving openssl
resolvable.

**How to verify**: `readelf -d dist/ry` RUNPATH must be exactly the two
`$ORIGIN` entries — no absolutes. `otool -L dist/ry` (macOS) must show
`@rpath/libLLVM.dylib` and no `llvm@21` / `Cellar` path.

### corrosion IMPORTED targets ignore CMake rpath properties; the cdylib resolves libLLVM via global scope

**Source**: #2005 (2026-06-03, implementation — verified in Linux container)
**Tags**: corrosion, rust, cdylib, rpath, libLLVM, global-scope

**Context**: `emit` is a corrosion-built IMPORTED cargo target.
`set_target_properties(emit PROPERTIES INSTALL_RPATH ...)` is
**silently ignored** — corrosion does not run CMake's link step. In the
Linux container, `readelf -d build/lib/libemit.so` shows `NEEDED
libLLVM.so.X` but **no RUNPATH**.

**Rule**: Do NOT add build.rs / RUSTFLAGS `-rpath` machinery for the cdylib's
libLLVM dependency. It is not needed: `ry` NEEDs libLLVM directly and loads
it into the **global symbol scope** at startup via its own `$ORIGIN/lib`
rpath, which satisfies the cdylib's `libLLVM.so.X` NEEDED by soname.
DT_RUNPATH does not cascade to a dependency-of-a-dependency, but global-scope
satisfaction sidesteps that. On macOS there is no global-scope fallback, so
`bundle-dist.sh` rewrites the cdylib's libLLVM install name to
`@loader_path/libLLVM.dylib` explicitly.

**How to verify**: the clean-room runtime test (hide system libLLVM, run
`echo 'print(3.14)' | dist/ry -c` to exercise the float/ConstantFP path of
#1997) is the arbiter — if it resolves libLLVM from `dist/lib/` and runs, the
global-scope mechanism works. Local Docker runs as uid 1000 and cannot `mv`
the root-owned `/usr/local/llvm` file, so hide libLLVM by dropping its dir
from `LD_LIBRARY_PATH` and assert the POSITIVE (`ldd dist/ry` resolves
libLLVM into `dist/lib/`); the CI `test` container runs as root and uses the
stronger physical `mv`.

### Shell verify helpers: pass the haystack as an argument, never via a pipe (the subshell swallows `fail=1`)

**Source**: #2005 (2026-06-03, implementation — verify-bundle.sh false pass)
**Tags**: shell, bash, subshell, pipe, verification, gotcha

**Context**: `verify-bundle.sh` accumulated failures into a `fail=1` variable
and called helpers as `echo "$haystack" | want "desc" "pattern"`. The
right-hand side of a pipe runs in a **subshell**, so `fail=1` set inside the
helper never reached the parent scope — the script printed `FAIL:` lines and
still `exit 0`. On macOS it stayed hidden (every check happened to pass); on
Linux a real absolute-rpath FAIL was silently dropped and the verifier lied.

**Rule**: A shell helper that mutates a parent-scope flag (accumulator,
counter) must NOT be invoked via `cmd | helper`. Pass the data as an argument
(`helper "desc" "pattern" "$haystack"`) or via a here-string (`helper "desc"
"pattern" <<<"$haystack"`) so the helper runs in the current shell. Applies
to any `|`-fed function that sets a variable the caller reads later.

**How to verify**: a verifier that can FAIL must be exercised against a
known-bad input and observed to `exit 1`. A green run on all-good input does
not prove the FAIL path propagates.

### The emit cdylib is NOT a `libry_*` file — every lib-selection glob must list it explicitly

**Source**: #2040 (2026-06-06, crate rename `ry_codegen` → `emit`)
**Tags**: packaging, install, self-update, cdylib, glob, libemit, rename, blind-spot

**Context**: The bundle / install / self-update paths select native libraries
to ship with a `libry_*` pattern: `bundle-dist.sh`'s
`native_libs=(.../libry_*.*)` copy glob, `install.sh`'s
`for f in .../libry_*.* ...` loop, and `self_update.cpp`'s
`filename.find("libry_") == 0` predicate. While the emission cdylib was named
`libry_codegen` it **incidentally** matched all three (it starts with
`libry_`). Renaming it to `libemit` (#2040) dropped it out of every selector:
the cdylib silently vanished from the bundle/install — and `bundle-dist.sh`
then `set -e`-aborted at the explicit `otool -D "$LIB/libemit.dylib"` (file
never copied). The `ry_codegen` string-sweep could not catch this because the
selectors contain `libry_`, not `ry_codegen` — the regression hides in a
*semantic* coupling (old name happened to start with `libry_`), not a textual
one.

**Rule**: The emit cdylib (`libemit.*`) is the ONE bundled native lib whose
name does not start with `libry_`. Any lib-selection mechanism that ships
native libs must enumerate it **separately** from the `libry_*` selector.
Three call sites, keep them in sync: `scripts/bundle-dist.sh` (copy glob),
`install.sh` (install loop), `src/cli/self_update.cpp`
(`is_bundled_lib` predicate). When renaming the cdylib again, or adding any
new bundled lib whose name is not `libry_<x>`, update all three. Note this is
distinct from `src/project/paths.cpp`'s `"libry_" + mod` construction, which
is for JIT `@native` **stdlib module** loading and correctly never references
the cdylib (the cdylib is link-time, not dlopen'd by module name).

**How to verify**: `bundle-dist.sh <plat> <build> <dist>` then
`verify-bundle.sh <dist> <plat>` — the latter asserts `libemit` presence,
`@rpath` id, and `@loader_path/libLLVM` linkage, so a dropped cdylib FAILs the
bundle gate. For self-update, `tests/test_self_update.cpp`
`CopiesRustCdylibLibemit` writes a fake `libemit.*` and asserts it installs.

### Orphan cdylib after a crate rename: exclude via the libLLVM-linkage discriminator, not `ADDITIONAL_CLEAN_FILES`

**Source**: #2041 (2026-06-07; orphan left by the #2040 `ry_codegen` → `emit` rename)
**Tags**: packaging, cdylib, corrosion, rename, orphan, libLLVM, discriminator, glob, dist

**Context**: A corrosion crate rename (`ry_codegen` → `emit`, #2040; earlier
`ry_llvm_emit` → `ry_codegen`, #2027) leaves the old cdylib
(`libry_codegen.{so,dylib}`) as an **orphan** in a non-clean build tree. A
non-destructive `cmake --preset` reconfigure self-heals `build.ninja` to
`libemit` but does **not** GC the stale output (corrosion's IMPORTED cargo
target output rides on no CMake clean tracking). The orphan then (a) confuses
devs / invites a reflexive `rm -rf`, and (b) is shipped by `bundle-dist.sh`'s
`libry_*` glob into the release tarball. The orphan shares the `libry_` prefix
with the legitimate stdlib native libs (`libry_base64`, …), so it **cannot** be
excluded by name pattern — only by an explicit discriminator.

**Rule**: The emission cdylib is the **only** bundled native lib that links
`libLLVM`; stdlib `libry_*` libs do not. Use this as a zero-drift discriminator
(no `RY_NATIVE_LIBS` enumeration needed, catches any future cdylib-rename
orphan):

- `scripts/verify-bundle.sh` (the pre-`tar` release gate) FAILs if any bundled
  native lib **other than `libemit`** links `libLLVM` (`otool -L` on darwin /
  `readelf -d` NEEDED on linux). This is the structural guarantee.
- `scripts/bundle-dist.sh` skips such orphans when copying (warns, so the stale
  tree is surfaced) — belt-and-braces so a local dirty bundle is clean by
  construction.
- `.claude/skills/pre-commit-checklist/run-tests.sh` removes orphan
  `libry_*` cdylibs from the host `$BUILD_DIR/lib` after each build (a legit
  persistent-script `rm`, same class as its `--clean` path), so a manual
  `--clean` is no longer needed to clear the orphan.

Grep `libLLVM` **case-sensitively** against the dependency listing: it matches
the libLLVM dependency line, never a self-name that contains lowercase `llvm`
(e.g. a hypothetical future `libry_llvm_*`). This is the **exclusion** axis and
is orthogonal to the #2040 entry above, which is the **inclusion** axis (list
`libemit` separately in every selector). Scope note: pre-cutover artifacts that
do not link `libLLVM` are out of scope (not the orphan-cdylib class, and absent
from the CI `build` / local `build-rust` dist sources).

**Rejected — `ADDITIONAL_CLEAN_FILES`**: it is regenerated from the current
config on every configure and is therefore keyed to `libemit`; an orphan is by
definition a name the post-rename config no longer knows, so no forward-keyed
clean mechanism can GC it. (Corrosion IMPORTED targets also ignore CMake
properties — see the rpath entry above — so the property likely never even
registers.) An orphan is "a file from a *previous* configuration": only
enumerate-and-prune, a full wipe, or a catch-at-the-boundary gate can touch it.

**How to verify**: assemble a dist, plant a fake orphan that links `libLLVM`
(`cp dist/lib/libemit.dylib dist/lib/libry_codegen.dylib`), and confirm
`verify-bundle.sh` now exits **1** (a green run on a clean dist does not prove
the FAIL path propagates — exercise the known-bad input, per the verify-helper
entry above). For the sweep, plant the orphan in `$BUILD_DIR/lib` and confirm
`run-tests.sh` removes it while leaving `libemit` and the stdlib `libry_*`
libs intact.
