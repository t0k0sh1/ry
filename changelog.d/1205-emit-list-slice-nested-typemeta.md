### Fixed

- Fix `lst[a..b]` and `slice(lst, a, b)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when slicing collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting slice (e.g.
  `slice(xs, 0, 1)[0][0]`) now works correctly. (#1205)
