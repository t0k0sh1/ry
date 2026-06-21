# JSON5 Function Reference

JSON5 parsing and serialization. All functions require explicit import from `json5`.

```ry
from json5 import load, stringify, stringifySafe, dump
```

## Overview

The `json5` module mirrors the [`json`](json.md) module's 8-function
surface (`load[T]` / `dump` / `stringify` / `stringifySafe`, each with
two arities) but accepts the [JSON5 spec](https://json5.org) extensions
on input. `json` (RFC 8259 strict) and `json5` coexist as separate
modules so existing strict callers are unaffected; pick `json5` for
configuration files where comments and trailing commas are useful and
pick `json` for wire-protocol payloads where strictness matters.

For the type-coercion semantics of `load[T]` (supported `T` set, error
message format, record / Option behavior) see [`json.md`](json.md) —
they are identical.

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `load[T]` | `(str) -> Result<T, Error>` | Parses a JSON5 string then coerces to `T`. Same `T` set as `json.load[T]`. |
| `load[T]` | `(File) -> Result<T, Error>` | Reads from an open `File` handle, parses, then coerces. |
| `stringify` | `(any[, indent: int][, sortKeys=bool]) -> str` | Serializes an `any` value. Output is strict-JSON-compatible except for non-finite floats (see "Non-finite floats" below). |
| `stringifySafe` | `(any[, indent: int][, sortKeys=bool]) -> Result<str, Error>` | Like `stringify`, but unsupported inputs (typed collection wrapped as `any`, `Set` / record / enum) return `Err(Error)`. Non-finite floats are **not** errors in `json5`. |
| `dump` | `(File, any) -> Result<Unit, Error>` | Stringifies `value` compactly and writes to `f`. |
| `dump` | `(File, any, int) -> Result<Unit, Error>` | Pretty-prints with `indent` spaces. |

## Accepted JSON5 Extensions

The parser accepts every byte sequence that the strict-JSON parser
accepts, plus the JSON5 spec extensions below:

| Extension | Example |
|-----------|---------|
| Line comments | `// comment` |
| Block comments | `/* comment */` (no nesting) |
| Trailing commas in arrays / objects | `[1, 2, 3,]`, `{a: 1,}` |
| Single-quoted strings | `'hello'` |
| Multi-line strings (line continuation) | `'foo\<LF>bar'` → `"foobar"` |
| Unquoted object keys (ASCII identifier) | `{x: 1, _foo: 2, $ref: 3}` |
| Hex integer literals | `0xFF`, `-0xDEADBEEF` |
| Leading / trailing decimal point | `.5`, `5.` |
| IEEE 754 special values | `Infinity`, `-Infinity`, `NaN` |
| Explicit positive sign | `+5`, `+3.14`, `+Infinity` |

Unquoted keys are restricted to the ASCII subset of ECMAScript
`IdentifierName` in this release: first character is a letter,
underscore, or dollar sign; following characters add digits. Unicode
identifier characters are accepted only inside single- or
double-quoted keys.

## Stringify Output Format

`json5.stringify` emits **strict-JSON-compatible** output by default
(double-quoted keys, double-quoted strings, no trailing commas, no
unquoted keys). Every byte produced is also valid input to the strict
`json` module, so

```text
json.load[T](json5.stringify(v))
```

round-trips for any value that strict JSON can represent.

### Non-finite floats

The one behavioral difference vs `json.stringify` is non-finite
`float`:

| Input | `json.stringify` | `json5.stringify` |
|-------|------------------|-------------------|
| `NaN` | `_Exit(1)` (panic) | `"NaN"` |
| `+Infinity` | `_Exit(1)` | `"Infinity"` |
| `-Infinity` | `_Exit(1)` | `"-Infinity"` |

`stringifySafe` mirrors this — `json5.stringifySafe(NaN)` returns
`Ok("NaN")` instead of `Err`. Typed-collection / `Set` / record /
enum rejection is inherited from `json` verbatim: those values have
no JSON5 representation either.

## Usage Examples

### Configuration file with comments and trailing commas

```ry
from json5 import load, stringify

src = "{\n  // tool config\n  name: 'ry',\n  port: 0xFF,\n  ratio: .5,\n}"
case load[Map<str, any>](src):
  Ok(cfg):
    print(stringify(cfg, sortKeys=true))
  Err(e):
    print("parse error: " + e.message)
```

### Reading a typed value

```ry
from json5 import load

case load[int]("+0xFF"):
  Ok(n): print(n)             # 255
  Err(e): print(e.message)
```

### Round-tripping `Infinity`

```ry
from json5 import load, stringify
from math import INF

v: any = INF
s = stringify(v)              # "Infinity"
case load[float](s):
  Ok(_): print("ok")
  Err(e): print(e.message)
```

### Loading into a record

```ry
from json5 import load

record Point:
  x: float
  y: float

case load[Point]("{x: .5, y: -.25}"):
  Ok(p): print(p.x)            # 0.5
  Err(e): print(e.message)
```

## Differences from `json` at a glance

| Aspect | `json` | `json5` |
|--------|--------|---------|
| Comments | rejected | accepted (`//`, `/* */`) |
| Trailing commas | rejected | accepted |
| Single-quoted strings | rejected | accepted |
| Unquoted object keys | rejected | accepted (ASCII identifier) |
| Hex literals | rejected | accepted (`0x...`) |
| Leading/trailing `.` in numbers | rejected | accepted (`.5`, `5.`) |
| `Infinity` / `NaN` literals | rejected | accepted |
| Leading `+` on numbers | rejected | accepted |
| `stringify(NaN)` | panics | `"NaN"` |
| `stringify(±Inf)` | panics | `"Infinity"` / `"-Infinity"` |
| `stringifySafe(NaN)` | `Err` | `Ok("NaN")` |
| Output format | strict JSON | strict JSON (lossless for finite-float values) |

## Related

- [`json`](json.md) — RFC 8259 strict variant; shares `T` coercion table and error format
- [`modules.md`](modules.md) — stdlib module discovery rules
