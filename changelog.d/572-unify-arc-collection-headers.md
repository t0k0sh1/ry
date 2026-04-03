### Fixed

- Fix undefined behavior in collection header deallocation where scope cleanup read invalid memory before plain-malloc headers (#572)
- Fix memory leak when collection operation results (appended, slice, etc.) are discarded as expression statements (#572)

### Changed

- All collection headers (List, Set, Map) now use ARC allocation uniformly, ensuring correct reference counting and CoW behavior (#572)
