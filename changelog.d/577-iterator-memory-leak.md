### Fixed

- Heap corruption after Iterator tests caused by leaked iterator headers and states; iterator memory is now freed at scope exit (#577)
