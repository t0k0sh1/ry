### Changed

- The parser now accepts user-defined directives on `for` statements and function-call statements. Previously every user-defined directive at those two sites was rejected at parse time, masking the codegen-level silent-no-op behavior introduced in #1425. The compiler built-in directive `@native` is still rejected at both sites; applying `@parallel` more than once on the same `for` loop is also still rejected. (#1427)
