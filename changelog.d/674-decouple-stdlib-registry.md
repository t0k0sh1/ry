### Changed

- Stdlib package dispatch now uses self-registering pattern instead of X-macros; adding a new stdlib package with custom codegen no longer requires modifying core compiler headers (#674)
- Resource type tracking is now dynamic via `ResourceKindRegistry` instead of a hardcoded enum; new opaque resource types can be added without modifying `codegen.hpp` (#674)
