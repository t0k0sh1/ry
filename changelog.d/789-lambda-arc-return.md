### Fixed

- Expression-bodied lambdas now correctly retain ARC references and clean up scope before returning, preventing potential use-after-free when returning captured ARC-managed values (#789)
