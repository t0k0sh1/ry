#!/usr/bin/env bash
# Shared helper for scripts/export-run-logs.sh.
# Source this file (do not exec) and call gen_run_nonce.
#
# gen_run_nonce prints a collision-safe nonce on stdout: 8 bytes from
# /dev/urandom, decoded as one decimal uint64. The wall-clock prefix in
# RUN_ID already orders runs chronologically, so the nonce only has to be
# unique. Reading /dev/urandom in-process means two calls in the SAME shell
# (same PID, same $$) still differ — that's the exact regression #2402 guards
# against, and the reason this lives in a sourceable helper rather than a
# separate subprocess.

gen_run_nonce() {
  od -An -N8 -tu8 /dev/urandom | tr -d ' \n'
}
