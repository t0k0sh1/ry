### Fixed

- Fix undefined behavior in collection header deallocation where scope cleanup read invalid memory before plain-malloc headers (#572)

### Changed

- All collection headers (List, Set, Map) now use ARC allocation uniformly, ensuring correct reference counting and CoW behavior (#572)
