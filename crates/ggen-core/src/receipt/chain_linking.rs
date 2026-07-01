//! Operation linkage between forward and inverse receipts.
//!
//! This module provides cryptographic linking of forward Receipt (O → A) operations
//! to inverse Receipt (A → O) operations via their operation_ids, enabling
//! bi-directional tracing of artifact provenance.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// A cryptographic link between a forward and inverse operation.
///
/// An `OperationLink` binds two operation_ids (UUID v4) into a single
/// tamper-evident proof that two specific operations are related.
/// The link itself has a cryptographic hash computed from both operation_ids.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OperationLink {
    /// UUID v4 of the forward Receipt (O → A).
    pub forward_operation_id: String,

    /// UUID v4 of the inverse Receipt (A → O).
    pub inverse_operation_id: String,

    /// BLAKE3 hex hash of the concatenation:
    /// `BLAKE3(forward_operation_id || inverse_operation_id)`
    ///
    /// This hash is deterministic and tamper-evident: changing either
    /// operation_id invalidates the link.
    pub link_hash: String,

    /// RFC-3339 timestamp when the link was created.
    pub linked_at: DateTime<Utc>,
}

impl OperationLink {
    /// Creates a new operation link between forward and inverse operations.
    ///
    /// Both operation_ids are validated to be UUID v4 format.
    ///
    /// The `link_hash` is computed deterministically as:
    /// ```text
    /// BLAKE3(forward_operation_id || inverse_operation_id)
    /// ```
    ///
    /// # Errors
    ///
    /// Returns `Err` if either operation_id is not a valid UUID v4.
    pub fn create(forward_id: &str, inverse_id: &str) -> Result<Self, String> {
        // Validate both IDs are UUID v4.
        match uuid::Uuid::parse_str(forward_id) {
            Ok(parsed) => {
                if parsed.get_version_num() != 4 {
                    return Err(format!(
                        "forward_operation_id must be UUID v4, got version {}",
                        parsed.get_version_num()
                    ));
                }
            }
            Err(e) => {
                return Err(format!("forward_operation_id is not a valid UUID: {}", e));
            }
        }

        match uuid::Uuid::parse_str(inverse_id) {
            Ok(parsed) => {
                if parsed.get_version_num() != 4 {
                    return Err(format!(
                        "inverse_operation_id must be UUID v4, got version {}",
                        parsed.get_version_num()
                    ));
                }
            }
            Err(e) => {
                return Err(format!("inverse_operation_id is not a valid UUID: {}", e));
            }
        }

        // Compute deterministic link hash.
        let mut hasher = blake3::Hasher::new();
        hasher.update(forward_id.as_bytes());
        hasher.update(inverse_id.as_bytes());
        let link_hash = hasher.finalize().to_hex().to_string();

        Ok(Self {
            forward_operation_id: forward_id.to_string(),
            inverse_operation_id: inverse_id.to_string(),
            link_hash,
            linked_at: Utc::now(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regex for UUID v4 format validation.
    const UUID_V4_PATTERN: &str =
        r"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$";

    fn is_uuid_v4(id: &str) -> bool {
        regex::Regex::new(UUID_V4_PATTERN)
            .map(|re| re.is_match(id))
            .unwrap_or(false)
    }

    #[test]
    fn test_operation_link_create_valid_uuids() {
        let fwd_id = uuid::Uuid::new_v4().to_string();
        let inv_id = uuid::Uuid::new_v4().to_string();

        let link = OperationLink::create(&fwd_id, &inv_id).expect("should create valid link");

        assert_eq!(link.forward_operation_id, fwd_id);
        assert_eq!(link.inverse_operation_id, inv_id);
        assert_eq!(link.link_hash.len(), 64); // BLAKE3 hex is 64 chars
        assert!(link.link_hash.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn test_operation_link_hash_deterministic() {
        let fwd_id = uuid::Uuid::new_v4().to_string();
        let inv_id = uuid::Uuid::new_v4().to_string();

        let link1 = OperationLink::create(&fwd_id, &inv_id).expect("first link should succeed");
        let link2 = OperationLink::create(&fwd_id, &inv_id).expect("second link should succeed");

        // Same inputs must produce identical hashes.
        assert_eq!(link1.link_hash, link2.link_hash);
    }

    #[test]
    fn test_operation_link_hash_changes_with_different_ids() {
        let fwd_id1 = uuid::Uuid::new_v4().to_string();
        let fwd_id2 = uuid::Uuid::new_v4().to_string();
        let inv_id = uuid::Uuid::new_v4().to_string();

        let link1 = OperationLink::create(&fwd_id1, &inv_id).expect("first link should succeed");
        let link2 = OperationLink::create(&fwd_id2, &inv_id).expect("second link should succeed");

        // Different forward IDs must produce different hashes.
        assert_ne!(link1.link_hash, link2.link_hash);
    }

    #[test]
    fn test_operation_link_rejects_invalid_forward_uuid() {
        let inv_id = uuid::Uuid::new_v4().to_string();
        let invalid = "not-a-uuid";

        let result = OperationLink::create(invalid, &inv_id);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("forward_operation_id"));
    }

    #[test]
    fn test_operation_link_rejects_invalid_inverse_uuid() {
        let fwd_id = uuid::Uuid::new_v4().to_string();
        let invalid = "also-not-a-uuid";

        let result = OperationLink::create(&fwd_id, invalid);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("inverse_operation_id"));
    }

    #[test]
    fn test_operation_link_rejects_non_v4_uuid() {
        // UUID v1 (timestamp-based)
        let v1_id = "c106a26a-21bb-11eb-adc1-0242ac120002".to_string();
        let v4_id = uuid::Uuid::new_v4().to_string();

        let result = OperationLink::create(&v1_id, &v4_id);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("UUID v4"));
    }

    #[test]
    fn test_operation_link_timestamp_is_real() {
        let fwd_id = uuid::Uuid::new_v4().to_string();
        let inv_id = uuid::Uuid::new_v4().to_string();

        let before = Utc::now();
        let link = OperationLink::create(&fwd_id, &inv_id).expect("link creation should succeed");
        let after = Utc::now();

        // Timestamp must be within creation window (allowing for system clock jitter).
        assert!(link.linked_at >= before - chrono::Duration::seconds(1));
        assert!(link.linked_at <= after + chrono::Duration::seconds(1));
    }
}
