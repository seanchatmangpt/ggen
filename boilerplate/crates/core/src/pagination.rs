use serde::{Deserialize, Serialize};

use crate::{CoreError, Result};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Page {
    pub offset: u64,
    pub limit: u64,
}

impl Page {
    pub fn new(offset: u64, limit: u64) -> Self {
        Self {
            offset,
            limit: limit.min(100),
        }
    }
}

impl Default for Page {
    fn default() -> Self {
        Self::new(0, 20)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PagedResult<T> {
    pub items: Vec<T>,
    pub total: u64,
    pub offset: u64,
    pub limit: u64,
}

impl<T> PagedResult<T> {
    pub fn new(items: Vec<T>, total: u64, page: Page) -> Self {
        Self {
            items,
            total,
            offset: page.offset,
            limit: page.limit,
        }
    }

    pub fn has_next(&self) -> bool {
        self.offset + self.limit < self.total
    }
}

/// Opaque cursor for keyset/cursor-based pagination.
/// The cursor is a hex-encoded string (no external dep needed — uses std only).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cursor(String);

impl Cursor {
    /// Encode an arbitrary string key as an opaque cursor.
    pub fn encode(key: &str) -> Self {
        use std::fmt::Write as _;
        let mut out = String::with_capacity(key.len() * 2);
        for b in key.as_bytes() {
            write!(out, "{:02x}", b).unwrap();
        }
        Self(out)
    }

    /// Decode the cursor back to the original key string.
    pub fn decode(&self) -> Result<String> {
        let hex = &self.0;
        if hex.len() % 2 != 0 {
            return Err(CoreError::validation("invalid cursor"));
        }
        let bytes: std::result::Result<Vec<u8>, _> = (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16))
            .collect();
        let bytes = bytes.map_err(|_| CoreError::validation("invalid cursor"))?;
        String::from_utf8(bytes).map_err(|_| CoreError::validation("invalid cursor encoding"))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Cursor-based page request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CursorPage {
    pub after: Option<Cursor>,
    pub limit: u64,
}

impl CursorPage {
    pub fn new(after: Option<Cursor>, limit: u64) -> Self {
        Self {
            after,
            limit: limit.min(100),
        }
    }

    pub fn first(limit: u64) -> Self {
        Self::new(None, limit)
    }
}

/// Cursor-paginated result set.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CursorPagedResult<T> {
    pub items: Vec<T>,
    pub next_cursor: Option<Cursor>,
    pub has_next: bool,
}

impl<T> CursorPagedResult<T> {
    pub fn new(items: Vec<T>, next_cursor: Option<Cursor>) -> Self {
        let has_next = next_cursor.is_some();
        Self {
            items,
            next_cursor,
            has_next,
        }
    }

    pub fn empty() -> Self {
        Self {
            items: vec![],
            next_cursor: None,
            has_next: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor_encode_decode_roundtrip() {
        let original = "item-uuid-123";
        let cursor = Cursor::encode(original);
        let decoded = cursor.decode().expect("decode must succeed");
        assert_eq!(decoded, original);
    }

    #[test]
    fn cursor_decode_invalid_hex_returns_err() {
        // Odd-length hex string is invalid.
        let bad = Cursor("zz".to_string());
        assert!(bad.decode().is_err(), "non-hex bytes must return Err");

        // Odd number of hex digits.
        let odd = Cursor("abc".to_string());
        assert!(odd.decode().is_err(), "odd-length hex must return Err");
    }

    #[test]
    fn cursor_page_caps_limit_at_100() {
        let page = CursorPage::new(None, 200);
        assert_eq!(page.limit, 100, "limit above 100 must be capped to 100");
    }

    #[test]
    fn cursor_paged_result_has_next_when_cursor_present() {
        let cursor = Cursor::encode("next-key");
        let result: CursorPagedResult<i32> = CursorPagedResult::new(vec![1, 2, 3], Some(cursor));
        assert!(result.has_next, "has_next must be true when next_cursor is Some");
        assert!(result.next_cursor.is_some());
    }

    #[test]
    fn cursor_paged_result_no_next_when_cursor_absent() {
        let result: CursorPagedResult<i32> = CursorPagedResult::new(vec![1, 2], None);
        assert!(!result.has_next, "has_next must be false when next_cursor is None");
        assert!(result.next_cursor.is_none());
    }
}
