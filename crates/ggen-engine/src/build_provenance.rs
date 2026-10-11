//! Build-time provenance, embedded by `build.rs`.
//!
//! [`BUILD_PROVENANCE_BLAKE3`] = BLAKE3 over (commit SHA + 6 canonical
//! strata pack digests + ontology triple count). Absent inputs are
//! represented in-band (sha `"unknown"`, zero-digest placeholders, count 0)
//! so a crates.io build records `"source: crates.io"` honestly instead of
//! faking a SHA. See `build.rs` for the full fail-soft contract.

/// The embedded build provenance digest (BLAKE3, raw 32 bytes).
pub const BUILD_PROVENANCE_BLAKE3: [u8; 32] = decode_hex(env!("GGEN_BUILD_PROVENANCE_HEX"));

/// The same digest as lowercase hex.
pub const BUILD_PROVENANCE_HEX: &str = env!("GGEN_BUILD_PROVENANCE_HEX");

/// Git commit SHA at build time, or `"unknown"` for non-git builds
/// (crates.io publishes).
pub const BUILD_COMMIT_SHA: &str = env!("GGEN_BUILD_COMMIT_SHA");

/// Where this build's source came from: `"git"` or `"crates.io"`.
pub const BUILD_SOURCE: &str = env!("GGEN_BUILD_SOURCE");

/// `name=blake3hex` bindings for the 6 canonical strata packs, `;`-separated.
/// Absent packs carry the documented all-zero 64-hex placeholder.
pub const BUILD_STRATA_PACKS: &str = env!("GGEN_BUILD_STRATA_PACKS");

/// Real triple count of the strata ontology at build time (0 if absent).
pub const BUILD_ONTOLOGY_TRIPLES: u32 = parse_u32(env!("GGEN_BUILD_ONTOLOGY_TRIPLES"));

const fn parse_u32(s: &str) -> u32 {
    let bytes = s.as_bytes();
    let mut n: u32 = 0;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if !c.is_ascii_digit() {
            return 0;
        }
        n = n * 10 + (c - b'0') as u32;
        i += 1;
    }
    n
}

/// Human-readable multi-line provenance block for `ggen --version --verbose`.
#[must_use]
pub fn describe() -> String {
    format!(
        "ggen {}\nsource: {}\ncommit: {}\nstrata_packs: {}\nontology_triples: {}\nbuild_provenance_blake3: {}",
        env!("CARGO_PKG_VERSION"),
        BUILD_SOURCE,
        BUILD_COMMIT_SHA,
        BUILD_STRATA_PACKS,
        BUILD_ONTOLOGY_TRIPLES,
        BUILD_PROVENANCE_HEX,
    )
}

const fn hex_val(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'f' => c - b'a' + 10,
        _ => 0,
    }
}

const fn decode_hex(s: &str) -> [u8; 32] {
    let bytes = s.as_bytes();
    let mut out = [0u8; 32];
    let mut i = 0;
    while i < 32 {
        out[i] = hex_val(bytes[2 * i]) * 16 + hex_val(bytes[2 * i + 1]);
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn const_round_trips_through_hex() {
        const HEX_DIGITS: &[u8; 16] = b"0123456789abcdef";
        let mut hex = String::with_capacity(64);
        for b in BUILD_PROVENANCE_BLAKE3 {
            hex.push(HEX_DIGITS[(b >> 4) as usize] as char);
            hex.push(HEX_DIGITS[(b & 0x0f) as usize] as char);
        }
        assert_eq!(hex, BUILD_PROVENANCE_HEX);
    }
}
