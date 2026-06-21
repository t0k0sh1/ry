#!/usr/bin/env bash
# §3.5.6 Rust lint — reproduce the CI `lint` job's Rust quality gate locally:
# `cargo fmt --check` + `cargo clippy -- -D warnings` over every workspace
# crate (was emit-only until #2282 added `native_base64` and generalised
# this script to `--workspace` so future native_* crates land covered
# automatically). Run before pushing any change under crates/. (#2015 / #2282)
#
# Toolchain: rust-toolchain.toml pins the channel to the version baked into the
# CI image, so rustup runs the same rustfmt/clippy CI gates on (no drift).
#
# No --clean flag: there is no CMake build dir to wipe — cargo manages its own
# incremental cache (target/ + the CARGO_HOME volume). (#2042)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

# `cargo fmt --check` needs no LLVM. `cargo clippy` compiles llvm-sys
# (force-dynamic) for the `emit` crate, so it needs LLVM_SYS_211_PREFIX
# pointing at a prefix that ships a shared libLLVM. native_* crates have no
# LLVM dependency and reuse the same env without harm. The CI image bakes
# LLVM_SYS_211_PREFIX into its ENV; on macOS default to the Homebrew llvm@21
# keg (the static-only /usr/local/llvm has no libLLVM.dylib — #1997).
# Override by exporting LLVM_SYS_211_PREFIX.
if [[ -z "${LLVM_SYS_211_PREFIX:-}" ]]; then
  case "$(uname -s)" in
    Darwin) export LLVM_SYS_211_PREFIX="$(brew --prefix llvm@21 2>/dev/null || echo /opt/homebrew/opt/llvm@21)" ;;
    *)      export LLVM_SYS_211_PREFIX=/usr/local/llvm ;;
  esac
fi
echo "==> LLVM_SYS_211_PREFIX=$LLVM_SYS_211_PREFIX" >&2

echo "==> cargo fmt --all -- --check (workspace)" >&2
cargo fmt --all -- --check

echo "==> cargo clippy --workspace --all-targets -- -D warnings" >&2
cargo clippy --workspace --all-targets -- -D warnings

# Structural gate (#2069): the abi boundary layer must emit zero LLVM IR. Pure
# bash/grep — no LLVM needed. Mirrors the CI `lint` job step of the same name.
echo "==> check-emit-abi-no-ir.sh (abi layer emits no IR)" >&2
bash scripts/check-emit-abi-no-ir.sh

echo "==> Rust lint OK" >&2
