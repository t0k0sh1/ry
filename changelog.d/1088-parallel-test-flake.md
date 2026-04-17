### Fixed

- Parallel test runner (`ry test -p`) now prints the failing file path and exit code for any non-zero worker, eliminating silent failure-count increments that were unattributable to a specific file. (#1088)
- Test runtime flushes stdout at every `it` boundary and after the summary, so output is preserved even when a worker exits abnormally. (#1088)
- Fixed an intermittent `~40%` failure rate in `ry test -p` on macOS caused by a crash in `~LLJIT()` during JIT teardown. Extended the existing Linux `(void)jit.release()` workaround to also apply on macOS. (#1088, #742)
