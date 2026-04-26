//! BLAKE3 evidence helpers — promoted from
//! `chatmangpt-mcpp-v2-cell::speckit-ralph::receipt_surfaces`.
//!
//! Receipts must carry verifiable content hashes. We standardize on BLAKE3
//! with the `"blake3:"` prefix so v2 receipts and canonical MCPP receipts are
//! schema-compatible.

use std::path::Path;

/// Hash a byte string into `"blake3:<hex>"`.
pub fn blake3_str(bytes: &[u8]) -> String {
    let h = blake3::hash(bytes);
    format!("blake3:{}", h.to_hex())
}

/// Hash the contents of a file. Returns `None` if the file is missing or
/// unreadable — receipt evidence is intentionally optional per surface.
pub fn blake3_file(path: &Path) -> Option<String> {
    let bytes = std::fs::read(path).ok()?;
    Some(blake3_str(&bytes))
}

/// Compute a causality chain hash linking the new receipt to its predecessor.
/// `[prev_chain | state_after | accepted_delta | control_pack]` separated
/// by `'|'` — same algorithm as the v2 cell so chains stay verifiable
/// across the promotion boundary.
pub fn causality_chain(
    prev_chain: Option<&str>, state_after: Option<&str>, accepted_delta: Option<&str>,
    control_pack: Option<&str>,
) -> String {
    let parts = [
        prev_chain.unwrap_or(""),
        state_after.unwrap_or(""),
        accepted_delta.unwrap_or(""),
        control_pack.unwrap_or(""),
    ];
    blake3_str(parts.join("|").as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blake3_str_is_stable() {
        let a = blake3_str(b"hello");
        let b = blake3_str(b"hello");
        assert_eq!(a, b);
        assert!(a.starts_with("blake3:"));
        assert_eq!(a.len(), "blake3:".len() + 64);
    }

    #[test]
    fn blake3_str_changes_with_input() {
        assert_ne!(blake3_str(b"a"), blake3_str(b"b"));
    }

    #[test]
    fn blake3_file_missing_returns_none() {
        assert!(blake3_file(Path::new("/nonexistent/path/4f8b/none")).is_none());
    }

    #[test]
    fn causality_chain_changes_with_prev() {
        let a = causality_chain(None, Some("s"), Some("a"), Some("c"));
        let b = causality_chain(Some("blake3:foo"), Some("s"), Some("a"), Some("c"));
        assert_ne!(a, b);
    }
}
