//! Pure Rust base64 encode / decode. No FFI, no panics on invalid input —
//! decoding returns `Err(String)` and the abi layer translates it into the
//! `setLastError` + null-return contract.

/// Standard RFC 4648 §4 alphabet.
pub const STD_ALPHABET: &[u8; 64] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// URL-safe RFC 4648 §5 alphabet (`+/` → `-_`).
pub const URL_ALPHABET: &[u8; 64] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

/// Encoded byte length for `input_len` bytes under `pad`. Callers size the
/// output buffer with this before invoking `encode_with_into`.
pub const fn encoded_len(input_len: usize, pad: bool) -> usize {
    if pad {
        4 * input_len.div_ceil(3)
    } else {
        let base = 4 * (input_len / 3);
        match input_len % 3 {
            1 => base + 2,
            2 => base + 3,
            _ => base,
        }
    }
}

/// Encode `input` with `alphabet` directly into `out`. `out.len()` must
/// equal `encoded_len(input.len(), pad)` — typically `out` is the uninit
/// slice returned by `__ry_host_make_string_uninit`, so the encoded bytes
/// land in the final ARC string buffer with no intermediate `Vec` copy.
pub fn encode_with_into(input: &[u8], alphabet: &[u8; 64], pad: bool, out: &mut [u8]) {
    debug_assert_eq!(out.len(), encoded_len(input.len(), pad));
    let len = input.len();
    let mut j = 0;
    let mut i = 0;
    while i < len {
        let a = u32::from(input[i]);
        let b = if i + 1 < len {
            u32::from(input[i + 1])
        } else {
            0
        };
        let c = if i + 2 < len {
            u32::from(input[i + 2])
        } else {
            0
        };
        let triple = (a << 16) | (b << 8) | c;

        out[j] = alphabet[((triple >> 18) & 0x3F) as usize];
        j += 1;
        out[j] = alphabet[((triple >> 12) & 0x3F) as usize];
        j += 1;
        if i + 1 < len {
            out[j] = alphabet[((triple >> 6) & 0x3F) as usize];
            j += 1;
        } else if pad {
            out[j] = b'=';
            j += 1;
        }
        if i + 2 < len {
            out[j] = alphabet[(triple & 0x3F) as usize];
            j += 1;
        } else if pad {
            out[j] = b'=';
            j += 1;
        }
        i += 3;
    }
    debug_assert_eq!(j, out.len());
}

const fn build_decode_table(alphabet: &[u8; 64]) -> [i8; 256] {
    let mut tbl = [-1i8; 256];
    let mut i = 0;
    while i < 64 {
        tbl[alphabet[i] as usize] = i as i8;
        i += 1;
    }
    tbl
}

/// Lookup table for `STD_ALPHABET`, fully const-evaluated at compile time
/// (lives in `.rodata`, zero runtime init cost).
pub static STD_DECODE_TABLE: [i8; 256] = build_decode_table(STD_ALPHABET);

/// Lookup table for `URL_ALPHABET`, fully const-evaluated at compile time.
pub static URL_DECODE_TABLE: [i8; 256] = build_decode_table(URL_ALPHABET);

/// Decode `input` using `decode_tbl`. Validates RFC 4648 §3.2 strict
/// padding (#1594): trailing `=` count must be 0, 1, or 2, and when any
/// padding is present the total length must be a multiple of 4. Error
/// messages exactly mirror the C++ `setLastError` strings in the deleted
/// `src/runtime/native/base64.cpp` so `tests/spec/base64.test.ry`
/// assertions like `expect(e.message).toContain("invalid")` keep passing.
pub fn decode_with(input: &[u8], decode_tbl: &[i8; 256]) -> Result<Vec<u8>, String> {
    let mut len = input.len();

    let mut pad_count = 0;
    while pad_count < len && input[len - 1 - pad_count] == b'=' {
        pad_count += 1;
    }
    if pad_count > 0 {
        if pad_count > 2 {
            return Err(format!(
                "invalid base64: excess padding ({pad_count} padding characters)"
            ));
        }
        if len % 4 != 0 {
            return Err(
                "invalid base64: input length must be a multiple of 4 when padding is present"
                    .to_string(),
            );
        }
    }
    len -= pad_count;

    let mut out = Vec::with_capacity(len * 3 / 4 + 1);
    let mut i = 0;
    while i < len {
        let mut sextet = [0u32; 4];
        let mut count = 0;
        while count < 4 && i < len {
            let val = decode_tbl[input[i] as usize];
            if val < 0 {
                return Err(format!("invalid base64 character at position {i}"));
            }
            sextet[count] = val as u32;
            count += 1;
            i += 1;
        }
        if count < 2 {
            return Err("invalid base64: truncated input".to_string());
        }
        let triple = (sextet[0] << 18) | (sextet[1] << 12) | (sextet[2] << 6) | sextet[3];
        if count >= 2 {
            out.push(((triple >> 16) & 0xFF) as u8);
        }
        if count >= 3 {
            out.push(((triple >> 8) & 0xFF) as u8);
        }
        if count >= 4 {
            out.push((triple & 0xFF) as u8);
        }
    }
    Ok(out)
}
