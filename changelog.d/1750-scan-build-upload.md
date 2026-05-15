### Fixed

- The `scan-build` CI job no longer fails when GitHub's artifact API
  returns a transient HTML error page instead of JSON during
  `actions/upload-artifact@v4` retry exhaustion. The
  `Upload scan-build report` step now carries `continue-on-error: true`,
  mirroring the warn-only posture of the sibling `Build + Analyze` step
  — both are best-effort because the scan-build findings backlog is
  still being triaged, and an artifact-upload transient should not
  fail-close the job. (#1750)
