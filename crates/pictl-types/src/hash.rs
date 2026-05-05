use serde::{Deserialize, Serialize};

/// BLAKE3 hash wrapper (256-bit = 64 hex characters)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Blake3Hash(String);

impl Blake3Hash {
    /// Create a Blake3Hash from a 64-character hex string
    pub fn from_hex(hex: String) -> Result<Self, String> {
        if hex.len() != 64 {
            return Err(format!("Invalid hash length: {} (expected 64)", hex.len()));
        }
        if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err("Hash contains non-hex characters".to_string());
        }
        Ok(Blake3Hash(hex))
    }

    /// Get the hex representation
    pub fn as_hex(&self) -> &str {
        &self.0
    }

    /// Convert to owned hex string
    pub fn to_hex(&self) -> String {
        self.0.clone()
    }
}

impl std::fmt::Display for Blake3Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Blake3Hash {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Canonical deterministic JSON with sorted keys
pub fn canonical_json<T: serde::Serialize>(value: &T) -> Result<String, serde_json::Error> {
    let json = serde_json::to_value(value)?;
    serde_json::to_string(&sort_json_value(&json))
}

/// Recursively sort all object keys in JSON value for deterministic output
fn sort_json_value(value: &serde_json::Value) -> serde_json::Value {
    match value {
        serde_json::Value::Object(map) => {
            let mut sorted: Vec<_> = map.iter().collect();
            sorted.sort_by(|a, b| a.0.cmp(b.0));
            let mut new_map = serde_json::Map::new();
            for (k, v) in sorted {
                new_map.insert(k.clone(), sort_json_value(v));
            }
            serde_json::Value::Object(new_map)
        }
        serde_json::Value::Array(arr) => {
            serde_json::Value::Array(arr.iter().map(sort_json_value).collect())
        }
        other => other.clone(),
    }
}

/// Compute BLAKE3 hash of bytes, returning 64-char hex string
pub fn blake3_hex(data: &[u8]) -> String {
    let hash = blake3::hash(data);
    hash.to_hex().to_string()
}

/// Compute BLAKE3 hash of a string
pub fn blake3_string(data: &str) -> String {
    blake3_hex(data.as_bytes())
}

/// Compute BLAKE3 hash of concatenated hashes (for combined_hash)
pub fn blake3_combined(hashes: &[&str]) -> String {
    let combined = hashes.join("");
    blake3_hex(combined.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blake3_hash_creation() {
        let hex = "a".repeat(64);
        let hash = Blake3Hash::from_hex(hex.clone()).unwrap();
        assert_eq!(hash.as_hex(), hex);
    }

    #[test]
    fn test_blake3_invalid_length() {
        let result = Blake3Hash::from_hex("a".repeat(128));
        assert!(result.is_err());
    }

    #[test]
    fn test_blake3_string_hash() {
        let hash1 = blake3_string("test");
        let hash2 = blake3_string("test");
        assert_eq!(hash1, hash2);
        assert_eq!(hash1.len(), 64);
    }

    #[test]
    fn test_canonical_json() {
        let mut map1 = serde_json::Map::new();
        map1.insert("z".to_string(), serde_json::json!(1));
        map1.insert("a".to_string(), serde_json::json!(2));

        let mut map2 = serde_json::Map::new();
        map2.insert("a".to_string(), serde_json::json!(2));
        map2.insert("z".to_string(), serde_json::json!(1));

        let val1 = serde_json::Value::Object(map1);
        let val2 = serde_json::Value::Object(map2);

        let json1 = canonical_json(&val1).unwrap();
        let json2 = canonical_json(&val2).unwrap();

        assert_eq!(json1, json2);
        assert!(json1.starts_with(r#"{"a":2"#)); // Keys sorted alphabetically
    }

    #[test]
    fn test_blake3_combined() {
        let hash1 = "a".repeat(64);
        let hash2 = "b".repeat(64);
        let combined = blake3_combined(&[&hash1, &hash2]);
        assert_eq!(combined.len(), 64);
    }
}
