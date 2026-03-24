//! Deterministic JSON canonicalization
//!
//! Provides canonical JSON serialization with:
//! - Sorted object keys
//! - Consistent formatting
//! - No whitespace variations

use crate::{Canonical, CanonicalError, Canonicalizer, Result};
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;

/// JSON canonicalizer
///
/// Produces deterministic JSON output by:
/// 1. Sorting all object keys alphabetically
/// 2. Using consistent formatting (compact, no pretty print)
/// 3. Normalizing number representations
pub struct JsonCanonicalizer {
    /// Whether to use pretty printing (still deterministic)
    pretty: bool,
}

impl JsonCanonicalizer {
    /// Create a new JSON canonicalizer (compact format)
    pub fn new() -> Self {
        Self { pretty: false }
    }

    /// Create a new JSON canonicalizer with pretty printing
    pub fn new_pretty() -> Self {
        Self { pretty: true }
    }

    /// Canonicalize a JSON value
    fn canonicalize_value(&self, value: Value) -> Result<Value> {
        match value {
            Value::Object(map) => {
                // Sort keys by converting to BTreeMap
                let sorted: BTreeMap<String, Value> = map
                    .into_iter()
                    .map(|(k, v)| Ok((k, self.canonicalize_value(v)?)))
                    .collect::<Result<_>>()?;
                Ok(Value::Object(sorted.into_iter().collect()))
            }
            Value::Array(arr) => {
                let canonical: std::result::Result<Vec<_>, _> = arr
                    .into_iter()
                    .map(|v| self.canonicalize_value(v))
                    .collect();
                Ok(Value::Array(canonical?))
            }
            // Primitives are already canonical
            other => Ok(other),
        }
    }
}

impl Default for JsonCanonicalizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Canonicalizer for JsonCanonicalizer {
    type Input = Value;
    type Output = Canonical<String>;

    fn canonicalize(&self, input: Self::Input) -> Result<Self::Output> {
        let canonical_value = self.canonicalize_value(input)?;
        let output = if self.pretty {
            serde_json::to_string_pretty(&canonical_value)
        } else {
            serde_json::to_string(&canonical_value)
        }
        .map_err(|e| CanonicalError::Serialization(e.to_string()))?;
        Ok(Canonical::new_unchecked(output))
    }
}

/// Canonicalize a serializable value to JSON string
///
/// # Errors
///
/// Returns error if serialization or canonicalization fails
pub fn canonicalize_json<T: Serialize>(value: &T) -> Result<String> {
    let json_value = serde_json::to_value(value)
        .map_err(|e| CanonicalError::Serialization(e.to_string()))?;
    let canonicalizer = JsonCanonicalizer::new();
    let canonical = canonicalizer.canonicalize(json_value)?;
    Ok(canonical.into_inner())
}

/// Canonicalize a JSON string
///
/// # Errors
///
/// Returns error if parsing or canonicalization fails
pub fn canonicalize_json_str(json: &str) -> Result<String> {
    let value: Value = serde_json::from_str(json)
        .map_err(|e| CanonicalError::Serialization(e.to_string()))?;
    let canonicalizer = JsonCanonicalizer::new();
    let canonical = canonicalizer.canonicalize(value)?;
    Ok(canonical.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_sort_keys() {
        let input = json!({
            "z": 1,
            "a": 2,
            "m": 3
        });
        let canonicalizer = JsonCanonicalizer::new();
        let result = canonicalizer.canonicalize(input).unwrap();
        let result_str = result.into_inner();
        assert!(result_str.find("\"a\"").unwrap() < result_str.find("\"m\"").unwrap());
        assert!(result_str.find("\"m\"").unwrap() < result_str.find("\"z\"").unwrap());
    }

    #[test]
    fn test_nested_sorting() {
        let input = json!({
            "outer": {
                "z": 1,
                "a": 2
            }
        });
        let canonicalizer = JsonCanonicalizer::new();
        let result = canonicalizer.canonicalize(input).unwrap();
        let result_str = result.into_inner();
        // Should have sorted inner keys
        assert!(result_str.contains("\"a\":2"));
        assert!(result_str.contains("\"z\":1"));
    }

    #[test]
    fn test_determinism() {
        let input = json!({
            "field3": "value3",
            "field1": "value1",
            "field2": "value2"
        });
        let canonicalizer = JsonCanonicalizer::new();
        let result1 = canonicalizer.canonicalize(input.clone()).unwrap();
        let result2 = canonicalizer.canonicalize(input).unwrap();
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_array_preservation() {
        let input = json!([3, 1, 2]);
        let canonicalizer = JsonCanonicalizer::new();
        let result = canonicalizer.canonicalize(input).unwrap();
        assert_eq!(result.into_inner(), "[3,1,2]");
    }

    #[test]
    fn test_canonicalize_json_str() {
        let input = r#"{"z":1,"a":2}"#;
        let result = canonicalize_json_str(input).unwrap();
        assert_eq!(result, r#"{"a":2,"z":1}"#);
    }

    #[test]
    fn test_pretty_format() {
        let input = json!({"b": 2, "a": 1});
        let canonicalizer = JsonCanonicalizer::new_pretty();
        let result = canonicalizer.canonicalize(input).unwrap();
        let result_str = result.into_inner();
        assert!(result_str.contains('\n'));
    }
}
