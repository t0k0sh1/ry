### Fixed

- Fix `distinct(lst)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when deduplicating collections such as
  `List<Map<str, int>>` or `List<function>`.
  Second-level access on the resulting list (e.g.
  `distinct(xs)[0]["a"]`) now works correctly. (#1241)
