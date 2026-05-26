# JSON Function Reference

JSON parsing and serialization. All functions require explicit import from `json`.

```ry
from json import load, stringify, stringifySafe, stringifySorted, stringifySortedSafe, dump
```

## Overview

The `json` module decodes JSON text into a caller-specified Ry type and
encodes `any`-typed values back to JSON. `any` carries a runtime tag
(`Null` / `Bool` / `Int` / `Float` / `Str` / `List<any>` /
`Map<str, any>`), so parsed values participate in ordinary ARC and
slot-coercion machinery — there is no opaque handle and no manual
`free` step. `load[T](text)` parses once and coerces the result to `T`;
the type argument is required (#1887 removed the pre-#1852 non-generic
`load(text) -> Result<any, Error>` form, which had no safe accessor
into the resulting `any`). Pick a concrete `T` such as
`load[Map<str, any>]` / `load[List<any>]` / `load[int]` /
`load[Record]` — `load[any]` is intentionally not supported.

`load[T](f: File)` and `dump(f: File, ...)` are File-handle convenience
overloads that fuse `io.readAll` / `io.writeText` with parsing /
stringification in a single step.

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `load[T]` | `(str) -> Result<T, Error>` | Parses a JSON string then coerces to `T` (see "Supported `T` in `load[T]`" below). |
| `load[T]` | `(File) -> Result<T, Error>` | Reads from an open `File` handle, parses, then coerces. Same `T` set as `load[T](str)`. |
| `stringify` | `(any) -> str` | Serializes an `any` value to compact JSON text. |
| `stringify` | `(any, int) -> str` | Pretty-prints with `indent` spaces. `indent < 0` falls back to compact form. |
| `stringifySafe` | `(any) -> Result<str, Error>` | Like `stringify`, but unsupported inputs (non-finite float, typed collection wrapped as `any`, `Set` / record / enum) return `Err(Error)` instead of panicking. |
| `stringifySafe` | `(any, int) -> Result<str, Error>` | Pretty-printing variant of `stringifySafe`. `indent < 0` falls back to compact form. |
| `stringifySorted` | `(any) -> str` | Like `stringify`, but `Map<str, any>` entries (including nested ones) are emitted in byte-lexicographic key order so the output is reproducible across runs. Panic semantics match `stringify`. |
| `stringifySorted` | `(any, int) -> str` | Pretty-printing variant of `stringifySorted`. `indent < 0` falls back to compact form. |
| `stringifySortedSafe` | `(any) -> Result<str, Error>` | Combines `stringifySafe` and `stringifySorted`: sorted-key output, unsupported inputs surface as `Err(Error)`. |
| `stringifySortedSafe` | `(any, int) -> Result<str, Error>` | Pretty-printing variant of `stringifySortedSafe`. `indent < 0` falls back to compact form. |
| `dump` | `(File, any) -> Result<Unit, Error>` | Stringifies `value` compactly and writes to `f`. Equivalent to `io.writeText(f, stringify(value))?`. |
| `dump` | `(File, any, int) -> Result<Unit, Error>` | Pretty-prints with `indent` spaces and writes to `f`. `indent < 0` falls back to compact form. |

## Supported `T` in `load[T]`

This release supports the following `T`:

- Primitive: `int`, `float`, `str`, `bool`
- Homogeneous container: `List<int>` / `List<float>` / `List<str>` /
  `List<bool>` and `Map<str, int>` / `Map<str, float>` / `Map<str, str>` /
  `Map<str, bool>`
- `List<any>` / `Map<str, any>` (no element coercion). Use these when you
  want the loosely-typed shape preserved as `any` payloads (e.g. for
  recursive `case`-walking by element).
- User-defined records (flat and nested) — each field of the JSON object
  is looked up by name and recursively coerced into the declared field
  type. Missing or wrong-typed fields surface as `Err(Error{message})`
  with a `load<Record>: field 'name' ...` prefix
- Typed collections of records: `List<Record>` (JSON array of objects)
  and `Map<str, Record>` (JSON object whose values are objects). Inner
  failures are prefixed with `load<List<Record>>: element <i>: ...` or
  `load<Map<str, Record>>: value '<key>': ...`
- `Option<T>` — JSON `null` produces `Ok(None)`; any other shape is
  recursively coerced into `T` and wrapped in `Ok(Some(_))`. Inner
  failures are prefixed with `load<Option<T>>: expected null or ...`

Unsupported `T` (returns `Err` from the coerce step):

- `T = any` — intentionally rejected (#1887). The pre-#1887 non-generic
  `load() -> Result<any, Error>` form had no safe accessor into the
  payload, which is why it was removed. Use `load[Map<str, any>]` /
  `load[List<any>]` for the JSON-shape-typed equivalents.
- `T = Set<...>` (JSON has no native set representation)
- `T = Result<...>` (no native JSON representation)

## Usage Examples

### Loading into `Map<str, any>` and pattern-matching

```ry
from json import load

case load[Map<str, any>]("{\"name\": \"Alice\", \"age\": 30}"):
  Ok(m):
    case get(m, "name"):
      Some(name): print(name)
      None: print("missing name")
  Err(e):
    print("parse error: " + e.message)
```

### Primitive and container `load[T]`

```ry
from json import load

case load[int]("42"):
  Ok(n): print(n)                  # 42
  Err(e): print("error: " + e.message)

case load[Map<str, str>]("{\"k\":\"v\"}"):
  Ok(m): print(m["k"])             # v
  Err(e): print("error: " + e.message)
```

### `load[Record]` / `load[List<Record>]` / `load[Option<Record>]`

`load[T]` reconstructs records by looking up each declared field in the
parsed JSON object and coercing it into the declared type. Nested
records, `List<Record>`, `Map<str, Record>`, and `Option<Record>` all
work via the same recursive path; the error message tells you which
nested location failed.

```ry
from json import load

record Person { name: str, age: int }

case load[Person]("{\"name\":\"Alice\",\"age\":30}"):
  Ok(p): print(p.name)              # Alice
  Err(e): print(e.message)          # e.g. load<Person>: field 'age' missing

case load[List<Person>]("[{\"name\":\"a\",\"age\":1},{\"name\":\"b\",\"age\":2}]"):
  Ok(xs): print(xs[0].name)         # a
  Err(e): print(e.message)

case load[Option<Person>]("null"):
  Ok(opt):
    case opt:
      Some(_): print("got Some")
      None: print("got None")       # got None
  Err(e): print(e.message)
```

### Compact and pretty stringify

```ry
from json import stringify

print(stringify([1, 2, 3]))        # [1,2,3]

m: Map<str, any> = {}
m["key"] = "value"
m["count"] = 42
print(stringify(m, 2))
# {
#   "key": "value",
#   "count": 42
# }
```

### `stringifySafe` for recoverable encode failures

`stringify` panics (`exit(1)`) when it encounters JSON-incompatible inputs
(non-finite floats, typed collections wrapped as `any`, `Set` / record /
enum). `stringifySafe` returns those failures as `Err(Error)` so callers
can recover.

```ry
from json import stringifySafe
from math import NAN

case stringifySafe(NAN):
  Ok(s): print(s)
  Err(e): print(e.message)
  # stringify: non-finite float cannot be encoded as JSON

m: Map<str, any> = {}
m["pi"] = 3.14
case stringifySafe(m, 2):
  Ok(s): print(s)
  # {
  #   "pi": 3.14
  # }
  Err(e): print(e.message)
```

### `stringifySorted` for reproducible output

`stringify` walks `Map<str, any>` in insertion order, which is
deterministic but depends on how the map was built. `stringifySorted`
emits keys in byte-lexicographic order (matching Python's
`json.dumps(sort_keys=True)` for valid UTF-8). Nested maps are sorted
recursively.

```ry
from json import stringifySorted

m: Map<str, any> = {}
m["c"] = 3
m["a"] = 1
m["b"] = 2
print(stringifySorted(m))           # {"a":1,"b":2,"c":3}
print(stringifySorted(m, 2))
# {
#   "a": 1,
#   "b": 2,
#   "c": 3
# }
```

`stringifySortedSafe` combines both behaviors: sorted output, with
`Err(Error)` instead of panicking on unsupported inputs.

```ry
from json import stringifySortedSafe
from math import INF

m: Map<str, any> = {}
m["b"] = 2
m["a"] = INF
case stringifySortedSafe(m):
  Ok(s): print(s)
  Err(e): print(e.message)
  # stringify: non-finite float cannot be encoded as JSON
```

### Loading from a `File` handle

```ry
from io import open
from json import load

case open("config.json", "r"):
  Ok(f):
    case load[Map<str, any>](f):
      Ok(m):
        print(m["host"])
      Err(e):
        print("parse error: " + e.message)
  Err(e):
    print("open error: " + e.message)
```

`load[T](f: File)` parses a `File` handle and coerces the result to
`T` in one step, mirroring the str-form. The supported `T` set is the
same as `load[T](text)` — see "Supported `T` in `load[T]`" above.

```ry
from io import open
from json import load

case open("config.json", "r"):
  Ok(f):
    case load[Map<str, any>](f):
      Ok(c): print(c["host"])
      Err(e): print("parse error: " + e.message)
  Err(e): print("open error: " + e.message)
```

### Dumping to a `File` handle

```ry
from io import open
from json import dump

case open("out.json", "w"):
  Ok(f):
    v: Map<str, any> = {}
    v["k"] = "v"
    case dump(f, v, 2):                 # 0 / negative indent = compact
      Ok(_): print("written")
      Err(e): print("write error: " + e.message)
  Err(e): print("open error: " + e.message)
```

## Notes

- Lifetime: `load[T]` returns ARC-managed payloads (and `any` slots when
  `T` is `List<any>` / `Map<str, any>`). Codegen emits the matching
  release at scope exit; no manual `free` call exists or is needed.
- `load[int]` accepts JSON floats that are whole numbers
  (`42.0` → `42`); `load[float]` accepts JSON integers
  (`42` → `42.0`).
- Embedded NUL bytes (` `) round-trip: `load` accepts ` ` in
  string values and object keys; `stringify` emits the escape sequence
  ` ` for any NUL byte in a string.
- `load` enforces a maximum nesting depth of **256** for arrays and
  objects (combined). Inputs that exceed this depth return
  `Err(Error{message: "json: maximum nesting depth exceeded"})`
  instead of crashing the runtime. Scalars (strings, numbers, bool,
  null) do not contribute to the depth count.
- `stringify` traverses `Map<str, any>` in **insertion order**. JSON
  itself does not specify object-key ordering, but the encoder is
  deterministic. Use `stringifySorted` (or `stringifySortedSafe`) when
  output must be reproducible across runs that build the same logical
  map via different insertion sequences (snapshot tests, config diffs,
  content-addressed hashing).
- `stringify` and `stringifySorted` panic (`exit(1)` with a diagnostic
  to stderr) when they encounter tags JSON cannot represent:
  non-finite floats (`NaN`, `±Infinity`), `Set<...>`, records, enums,
  or maps keyed by anything other than `str`. The return type
  `-> str` has no `Result` channel, so panic is the only failure mode.
  Use `stringifySafe` / `stringifySortedSafe` (which return
  `Result<str, Error>`) when the caller needs to recover instead of
  abort, or convert unsupported values to representable forms before
  calling the panicking variants.
- **Runtime error — `stringify(value: any)` / `stringifySorted(value: any)` on typed collections**: the
  encoder walks the inner `List` / `Map` buffer assuming a uniform `any`
  element stride (16 bytes). `wrapInAny` preserves the original
  collection header pointer, so passing an `any` constructed from a
  typed collection (`xs: List<int>; v: any = xs; stringify(v)`) would
  read past valid storage. The runtime detects this case via a
  side-table populated at wrap time and panics with a deterministic
  diagnostic instead (or returns `Err(Error{message})` with the same
  text under `stringifySafe` / `stringifySortedSafe`):

  ```text
  stringify: any holds typed collection 'List<int>' — use List<any> / Map<str, any> / Set<any> instead
  ```

  Followed by `exit(1)`. The recorded type name (`'List<int>'`,
  `'Map<str, int>'`, etc.) reflects the source-level Ry type that was
  wrapped. Safe inputs are: `any` payloads parsed via
  `load[Map<str, any>]` / `load[List<any>]`, primitive `any` slots, and
  `List<any>` / `Map<str, any>` constructed directly. To stringify a
  user-built typed collection, box each element into a `List<any>` /
  `Map<str, any>` first (or call `as any` per element when assembling
  the collection).
- **Compile-time error — assigning an `any` payload into a typed
  collection**: writing `xs: List<str> = v` (or `Map<str, int>` /
  `Set<int>` / any typed collection whose element type is not `any`)
  on a `v: any` bound from a `Result<any, _>`-returning function is
  rejected at compile time (#1883). The source value's element type is
  unknown to the compiler at that point, and an unchecked unwrap would
  either segfault (`List<str>` walks the 8-byte typed stride past the
  end of the 16-byte `RyAny` payload) or silently produce garbage
  (`List<int>` reads the `RyAny` tag bytes as the payload). Use one of
  the safe alternatives instead:

  ```ry
  # ❌ rejected at compile time — see error message
  fn loadAny(text: str) -> Result<any, Error>:
    case load[List<any>](text):
      Ok(xs):
        a: any = xs
        return Ok(a)
      Err(e): return Err(e)
  case loadAny(text):
    Ok(v):
      xs: List<str> = v
    Err(_): 0

  # ✅ option 1: use load[T] to parse + coerce in one step (no any in between)
  case load[List<str>](text):
    Ok(xs): print(xs[0])
    Err(e): print(e.message)

  # ✅ option 2: parse into List<any> and case on each element
  case load[List<any>](text):
    Ok(vs):
      for elem in vs:
        opt: Option<str> = elem as str
        case opt:
          Some(s): print(s)
          None: print("not a string")
    Err(_): 0
  ```

  The diagnostic suggests `load[T]` directly: `Cannot assign 'any' to
  typed collection 'List<str>' for variable 'xs': source type is
  unknown. Use 'load[List<str>]' for type-safe parsing, or 'case' on
  each element.` `List<any>` / `Map<str, any>` / `Set<any>` annotations
  remain allowed unconditionally (the payload stride matches). #1887
  removed the non-generic `load(text)` / `load(f)` overloads to make
  this hazard harder to hit (every JSON parse now picks an explicit
  `T`), but the same guard still applies whenever an `any` slot arrives
  from any other `Result<any, _>`-returning source. The check does not
  yet cover the reassignment path (`xs = v` after `xs`
  is already declared) or function-boundary `any` passes with
  mismatched element strides — prefer `load[T]` for those cases too.

## Error message format

`load[T]` returns `Err(Error{message})` on parse failure.
When the failure can be tied to a specific position in the input, the
message includes a human-readable line/column reference followed by the
original byte offset in parentheses:

```text
<reason> at line <L>, column <C> (offset <O>)
```

- `<L>` and `<C>` are 1-based; `<O>` is the 0-based byte index into the input.
- `<C>` counts UTF-8 codepoints, not bytes, matching typical editor column
  numbers and Python's `JSONDecodeError`.
- `\n` increments `<L>` and resets `<C>` to 1. `\r` is treated as an
  ordinary character and bumps `<C>`.

Example messages:

```text
unexpected character 'x' at line 1, column 1 (offset 0)
expected string key at line 1, column 2 (offset 1)
invalid number at line 3, column 5 (offset 24)
```

Errors that have no meaningful position — `unexpected end of input`,
`unterminated string`, `unterminated array`, `unterminated object`,
`json: maximum nesting depth exceeded`, `unpaired high surrogate in
unicode escape`, etc. — are returned as the bare reason text without
the position suffix.

## Out-of-scope (this release)

- `load[any]` — intentionally rejected (#1887). The pre-#1887
  non-generic `load(text)` returned `Result<any, Error>` but exposed
  no safe accessor into the payload, which is why the API was
  consolidated to `load[T]`. Pick a concrete `T` such as
  `load[Map<str, any>]` / `load[List<any>]` to keep the JSON-shape
  typing without committing to a specific element type.
- `load[Set<T>]` — JSON has no native set representation; users
  needing set semantics should `load[List<T>]` first and convert.
- `load[Result<...>]` — Result has no canonical JSON shape and the
  ambiguity (success/failure marker key? Two arms?) is intentionally
  left to the user to encode at a higher level.
