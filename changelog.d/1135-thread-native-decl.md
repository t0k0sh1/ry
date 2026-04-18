### Fixed

- thread: align `thread_spawn` / `thread_join` `@native` declarations with their runtime behaviour (supports `int` / `float` / `bool` workers in addition to `Unit`) by using `any` as the declaration-level placeholder (#1135)
