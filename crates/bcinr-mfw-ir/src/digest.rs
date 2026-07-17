//! 32-byte BLAKE3 digest newtype shared across the MFW IR.

use std::fmt;

/// A BLAKE3-256 digest, wrapped so call sites can't accidentally mix it up
/// with an arbitrary `[u8; 32]` (a nonce, a key, ...).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Digest(pub [u8; 32]);

impl Digest {
    /// Hash `bytes` with BLAKE3 and wrap the result.
    pub fn hash(bytes: &[u8]) -> Self {
        Digest(*blake3::hash(bytes).as_bytes())
    }

    /// Combine (`self`, `other`) into a new digest by hashing their
    /// concatenation — the same "causal_mix" idea used by
    /// `bcinr-powl-receipt`'s hash-chained frames (prior_hash || frame_bytes),
    /// generalized to two arbitrary digests.
    pub fn mix(&self, other: &Digest) -> Self {
        let mut buf = [0u8; 64];
        buf[..32].copy_from_slice(&self.0);
        buf[32..].copy_from_slice(&other.0);
        Digest::hash(&buf)
    }

    /// All-zero digest — used as a sentinel/root for hash chains, never as a
    /// claim that some content actually hashes to zero.
    pub const ZERO: Digest = Digest([0u8; 32]);

    /// Raw bytes.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl fmt::Debug for Digest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Digest({})", hex_encode(&self.0))
    }
}

impl fmt::Display for Digest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex_encode(&self.0))
    }
}

impl From<[u8; 32]> for Digest {
    fn from(bytes: [u8; 32]) -> Self {
        Digest(bytes)
    }
}

fn hex_encode(bytes: &[u8; 32]) -> String {
    let mut s = String::with_capacity(64);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_is_deterministic() {
        let a = Digest::hash(b"hello");
        let b = Digest::hash(b"hello");
        assert_eq!(a, b);
    }

    #[test]
    fn hash_distinguishes_inputs() {
        let a = Digest::hash(b"hello");
        let b = Digest::hash(b"hellp");
        assert_ne!(a, b);
    }

    #[test]
    fn mix_is_order_sensitive() {
        let a = Digest::hash(b"a");
        let b = Digest::hash(b"b");
        assert_ne!(a.mix(&b), b.mix(&a));
    }

    #[test]
    fn debug_and_display_agree() {
        let d = Digest::hash(b"x");
        assert_eq!(format!("{d:?}"), format!("Digest({d})"));
    }
}
