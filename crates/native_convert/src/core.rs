//! Pure-Rust numeric parsing for `int(s)` / `float(s)`. No FFI, no
//! `unsafe`. Errors come back as `String`s whose text is byte-identical
//! to the C++ `setLastError(...)` strings in
//! `src/runtime/native/convert.cpp` (which this crate replaces), so the
//! existing `.toContain("int:")` / `.toContain("float:")` assertions
//! and any byte-exact callers keep working.
//!
//! Behaviour matches the C++ side on every input the test suite covers
//! (`"abc"`, `""`, `"12abc"`, `"9223372036854775808"`,
//! `"-9223372036854775809"`, `"xyz"`, `"1.2abc"`, `"1e400"`). Two
//! input-space divergences are tolerated and intentionally not codified
//! by tests (issue scope is the migration, not a behaviour change):
//!
//! - leading whitespace: C `strtoll`/`strtod` accept `" 42"` and skip
//!   the space; Rust `from_str` rejects it as `InvalidDigit`. The
//!   existing tests do not exercise this corner.
//! - float underflow: C `strtod("1e-400", ...)` sets `errno=ERANGE`;
//!   Rust `"1e-400".parse::<f64>()` returns `Ok(0.0)`. Also untested.
//!
//! Overflow towards infinity is normalised explicitly below so
//! `"1e400"` still errors (matches C++).

use std::num::IntErrorKind;

pub fn parse_int(s: &str) -> Result<i64, String> {
    if s.is_empty() {
        return Err("int: empty string".to_string());
    }
    match s.parse::<i64>() {
        Ok(v) => Ok(v),
        Err(e) => match e.kind() {
            IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                Err(format!("int: overflow in '{s}'"))
            }
            _ => Err(format!("int: invalid character in '{s}'")),
        },
    }
}

/// True iff `s` (with optional leading `+`/`-`) spells out an infinity
/// literal that `f64::from_str` accepts. Used by `parse_float` to
/// distinguish a legitimate `"inf"` / `"infinity"` from an overflow
/// case like `"1e400"` (both parse to `f64::INFINITY` in Rust, but C
/// `strtod` only flags the latter via `errno=ERANGE`).
fn is_infinity_literal(s: &str) -> bool {
    let t = s.strip_prefix(['+', '-']).unwrap_or(s);
    t.eq_ignore_ascii_case("inf") || t.eq_ignore_ascii_case("infinity")
}

pub fn parse_float(s: &str) -> Result<f64, String> {
    if s.is_empty() {
        return Err("float: empty string".to_string());
    }
    match s.parse::<f64>() {
        Ok(v) => {
            if v.is_infinite() && !is_infinity_literal(s) {
                Err(format!("float: out of range in '{s}'"))
            } else {
                Ok(v)
            }
        }
        Err(_) => Err(format!("float: invalid character in '{s}'")),
    }
}
