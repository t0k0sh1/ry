### Fixed

- Fix `take(lst, n)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when taking the prefix of collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting list (e.g.
  `take(xs, 2)[0][0]`) now works correctly. (#1240)
