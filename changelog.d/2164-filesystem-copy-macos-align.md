### Fixed

- `copy(src, dst)` now returns `Err` on macOS when the source is a directory or other non-regular file, matching the Linux behavior. Previously the macOS path called `copyfile(... COPYFILE_ALL)` without a pre-check, which silently created an empty destination directory and returned `Ok` for a directory source — a latent data-loss risk if the caller subsequently removed the source. Symbolic links continue to be followed and are accepted when their target is a regular file. (#2164)
