#!/usr/bin/env bash
# §3.5.6 Rust lint — reproduce the CI `lint` job's Rust quality gate locally:
# `cargo fmt --check` + `cargo clippy -- -D warnings` over crates/ry_codegen.
# Run before pushing any change under crates/. (#2015)
#
# Toolchain: rust-toolchain.toml pins the channel to the version baked into the
# CI image, so rustup runs the same rustfmt/clippy CI gates on (no drift).
#
# No --clean flag: there is no CMake build dir to wipe — cargo manages its own
# incremental cache (target/ + the CARGO_HOME volume). (#2042)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

MANIFEST=crates/ry_codegen/Cargo.toml

# `cargo fmt --check` needs no LLVM. `cargo clippy` compiles llvm-sys
# (force-dynamic), so it needs LLVM_SYS_211_PREFIX pointing at a prefix that
# ships a shared libLLVM. The CI image bakes LLVM_SYS_211_PREFIX into its ENV;
# on macOS default to the Homebrew llvm@21 keg (the static-only /usr/local/llvm
# has no libLLVM.dylib — #1997). Override by exporting LLVM_SYS_211_PREFIX.
if [[ -z "${LLVM_SYS_211_PREFIX:-}" ]]; then
  case "$(uname -s)" in
    Darwin) export LLVM_SYS_211_PREFIX="$(brew --prefix llvm@21 2>/dev/null || echo /opt/homebrew/opt/llvm@21)" ;;
    *)      export LLVM_SYS_211_PREFIX=/usr/local/llvm ;;
  esac
fi
echo "==> LLVM_SYS_211_PREFIX=$LLVM_SYS_211_PREFIX" >&2

echo "==> cargo fmt --check ($MANIFEST)" >&2
cargo fmt --manifest-path "$MANIFEST" -- --check

echo "==> cargo clippy -p ry_codegen -- -D warnings" >&2
cargo clippy -p ry_codegen -- -D warnings

echo "==> Rust lint OK" >&2
