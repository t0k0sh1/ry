### Changed

- Internal codegen now uses `record` terminology throughout (`RecordInfo`, `record_types_`, `emitRecordConstructor`, `emitRecordComparison`, `findRecordTypeName`, `createRecordVisitFunction`, `recordToString`, `recordHasArcFields`, `arc_field_record_vars_`) to align with the `record` keyword used at the language surface (#816)
- User-visible error messages updated from "struct type" to "record type" (e.g., "unknown record type", "field access on non-record type") (#816)
