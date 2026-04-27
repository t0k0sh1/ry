### Fixed

- User-defined `@directive` declarations now accept required parameters in named-argument form. Previously `@mydir(description="hi")` for `fn mydir(description: str)` was rejected with "unknown named argument"; now both `@mydir("hi")` and `@mydir(description="hi")` are accepted. Mixed positional+named for the same parameter is rejected as a duplicate, and missing required parameters produce a clearer error. (#1397)
