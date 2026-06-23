---
paths:
  - "scripts/bundle-dist.sh"
  - "scripts/verify-bundle.sh"
  - ".github/workflows/release.yml"
---

# Distribution Packaging

### Release binaries bundle shared libLLVM; rpath rewrite happens in the packaging step, not CMake INSTALL_RPATH

**Tags**: release, packaging, rpath, libLLVM, cutover, distribution

`release.yml` copies `build/ry` directly (not via `cmake --install`), so the shipped binary carries the build-tree rpath. Without bundling, the binary fails to start where libLLVM is absent.

- Bundling + rpath rewrite lives in `scripts/bundle-dist.sh`; `scripts/verify-bundle.sh` asserts the result before `tar`.
- **macOS**: install names are absolute Mach-O paths — `install_name_tool -change <abs> @rpath/...` is required; an added rpath alone does not redirect them. Re-sign each rewritten Mach-O with `codesign --force --sign -` (install_name_tool invalidates the signature). libLLVM's only non-system chain dep is Homebrew `libzstd` — bundle it and `-change` libLLVM's reference to `@loader_path/libzstd.1.dylib`.
- **Linux**: set the rpath in CMake, NOT patchelf — `ry-ci-glibc-old` has no patchelf (no-apt rule). Use `BUILD_WITH_INSTALL_RPATH ON` with `INSTALL_RPATH`, NOT `BUILD_RPATH`. `BUILD_RPATH` is additive to CMake's automatic link-path rpath, so the shipped binary keeps build-machine-absolute paths (`/usr/local/llvm/lib`, `/opt/openssl/lib`, `build/lib`). `BUILD_WITH_INSTALL_RPATH ON` makes the build-tree binary link with `INSTALL_RPATH` instead of the auto set; `INSTALL_RPATH_USE_LINK_PATH` defaults OFF so nothing else is appended. libzstd is a system lib on the glibc-old image — NOT bundled (intentional macOS/Linux asymmetry).
- **Two rpaths on `ry`** cover both layouts: `@loader_path/lib` / `$ORIGIN/lib` (tarball unpacked in place) and `@loader_path/../../.ry/lib` / `$ORIGIN/../../.ry/lib` (installed: `~/.local/bin/ry` pointing to `~/.ry/lib`, a fixed offset since both sit under `$HOME`). `install.sh` and `ry self-update` (`install_native_libs` in `src/cli/self_update.cpp`) install the bundled libs into `~/.ry/lib` alongside `libry_*`.

openssl is out of scope: bundling crypto blocks OS security updates; clean-room tests hide ONLY libLLVM, leaving openssl resolvable.

### corrosion IMPORTED targets ignore CMake rpath properties; the cdylib resolves libLLVM via global scope

**Tags**: corrosion, rust, cdylib, rpath, libLLVM, global-scope

`set_target_properties(emit PROPERTIES INSTALL_RPATH ...)` is silently ignored — corrosion does not run CMake's link step. `readelf -d build/lib/libemit.so` shows `NEEDED libLLVM.so.X` but no RUNPATH.

Do NOT add `build.rs` / `RUSTFLAGS -rpath` machinery for the cdylib's libLLVM dependency. `ry` NEEDs libLLVM directly and loads it into the global symbol scope at startup via its own `$ORIGIN/lib` rpath, which satisfies the cdylib's `libLLVM.so.X` NEEDED by soname. DT_RUNPATH does not cascade to a dependency-of-a-dependency, but global-scope satisfaction sidesteps that. On macOS there is no global-scope fallback, so `bundle-dist.sh` rewrites the cdylib's libLLVM install name to `@loader_path/libLLVM.dylib` explicitly.

### Shell verify helpers: pass the haystack as an argument, never via a pipe (the subshell swallows `fail=1`)

**Tags**: shell, bash, subshell, pipe, verification, gotcha

The right-hand side of a pipe runs in a subshell, so `fail=1` set inside a helper never reaches the parent scope — the script printed `FAIL:` lines and still exited 0. On Linux a real absolute-rpath FAIL was silently dropped and the verifier lied.

A shell helper that mutates a parent-scope flag must NOT be invoked via `cmd | helper`. Pass the data as an argument (`helper "desc" "pattern" "$haystack"`) or via a here-string (`helper "desc" "pattern" <<<"$haystack"`) so the helper runs in the current shell. Applies to any `|`-fed function that sets a variable the caller reads later.

### The emit cdylib is NOT a `libry_*` file — every lib-selection glob must list it explicitly

**Tags**: packaging, install, self-update, cdylib, glob, libemit, rename, blind-spot

While named `libry_codegen` the cdylib incidentally matched the `libry_*` selector. Renaming to `libemit` dropped it from every selector silently: the cdylib vanished from the bundle/install. The regression hides in a semantic coupling (old name happened to start with `libry_`), not a textual one — a string-sweep on `ry_codegen` cannot catch it.

`libemit.*` is the ONE bundled native lib whose name does not start with `libry_`. Any lib-selection mechanism that ships native libs must enumerate it separately from the `libry_*` selector. Three call sites, keep them in sync: `scripts/bundle-dist.sh` (copy glob), `install.sh` (install loop), `src/cli/self_update.cpp` (`is_bundled_lib` predicate). When renaming the cdylib again, or adding any new bundled lib whose name is not `libry_<x>`, update all three. Note this is distinct from `src/project/paths.cpp`'s `"libry_" + mod` construction, which is for JIT `@native` stdlib module loading and correctly never references the cdylib (the cdylib is link-time, not dlopen'd by module name).

### Orphan cdylib after a crate rename: exclude via the libLLVM-linkage discriminator, not `ADDITIONAL_CLEAN_FILES`

**Tags**: packaging, cdylib, corrosion, rename, orphan, libLLVM, discriminator, glob, dist

A corrosion crate rename leaves the old cdylib as an orphan in a non-clean build tree — a non-destructive reconfigure heals `build.ninja` but does not GC the stale output. The orphan (a) confuses devs, and (b) is shipped by `bundle-dist.sh`'s `libry_*` glob. The orphan shares the `libry_` prefix with the legitimate stdlib native libs and cannot be excluded by name pattern — only by an explicit discriminator.

The emission cdylib is the only bundled native lib that links `libLLVM`; stdlib `libry_*` libs do not. Use this as a zero-drift discriminator:

- `scripts/verify-bundle.sh` FAILs if any bundled native lib other than `libemit` links `libLLVM` (`otool -L` on darwin / `readelf -d` NEEDED on linux). This is the structural guarantee.
- `scripts/bundle-dist.sh` skips such orphans when copying (warns, so the stale tree is surfaced).
- `.claude/skills/pre-commit-checklist/run-tests.sh` removes orphan `libry_*` cdylibs from the host `$BUILD_DIR/lib` after each build.

Grep `libLLVM` case-sensitively: it matches the libLLVM dependency line, never a self-name containing lowercase `llvm`. This is the exclusion axis; the entry above is the inclusion axis (list `libemit` separately in every selector).

`ADDITIONAL_CLEAN_FILES` cannot solve this: it is regenerated from the current config on every configure and keyed to `libemit`; an orphan is by definition a name the post-rename config no longer knows.
