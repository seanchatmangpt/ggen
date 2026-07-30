//! Deterministic BLAKE3 receipts over canonicalizable architecture values.

use serde::Serialize;

use crate::error::Result;

#[derive(Serialize)]
struct HashInput<'a, T> {
    schema: &'static str,
    kind: &'a str,
    payload: &'a T,
}

/// Hash a serializable payload under an explicit receipt kind.
///
/// Determinism relies on fixed struct field order plus ordered map/set types in
/// public models. Callers should not pass `HashMap` or other unordered values.
pub fn deterministic_hash<T: Serialize>(kind: &str, payload: &T) -> Result<String> {
    let input = HashInput {
        schema: "ggen.architecture.receipt.v1",
        kind,
        payload,
    };
    let bytes = serde_json::to_vec(&input)?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}
