### Fixed

- `for a, b in xs[0]:` where `xs: List<List<(int, int)>>` now correctly types the second destructured variable `b` as `int` instead of reading raw bytes (#1094)
- `for x in outer[0][0]:` where `outer: List<List<List<int>>>` now correctly iterates all elements instead of running 0 times (#1095)
