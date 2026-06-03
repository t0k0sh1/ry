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

**Context**: `ry_llvm_emit` is a corrosion-built IMPORTED cargo target.
`set_target_properties(ry_llvm_emit PROPERTIES INSTALL_RPATH ...)` is
**silently ignored** — corrosion does not run CMake's link step. In the
Linux container, `readelf -d build/lib/libry_llvm_emit.so` shows `NEEDED
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
