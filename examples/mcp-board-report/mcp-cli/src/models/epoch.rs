//! Key epoch model

use serde::{Deserialize, Serialize};

/// Key epoch for managing signing key rotation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyEpoch {
    pub epoch_id: String,
    pub public_key_hash: String,
    pub epoch_start: chrono::DateTime<chrono::Utc>,
    pub epoch_end: chrono::DateTime<chrono::Utc>,
    pub is_revoked: bool,
}

impl KeyEpoch {
    /// Create a new key epoch
    pub fn new(epoch_id: String, public_key_hash: String, duration_days: i64) -> Self {
        let now = chrono::Utc::now();
        let end = now + chrono::Duration::days(duration_days);

        Self {
            epoch_id,
            public_key_hash,
            epoch_start: now,
            epoch_end: end,
            is_revoked: false,
        }
    }

    /// Check if epoch is currently valid
    pub fn is_valid(&self) -> bool {
        let now = chrono::Utc::now();
        !self.is_revoked && now >= self.epoch_start && now <= self.epoch_end
    }

    /// Revoke the epoch
    pub fn revoke(&mut self) {
        self.is_revoked = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_epoch_creation() {
        let epoch = KeyEpoch::new(
            "epoch-001".to_string(),
            "pubkeyhash123".to_string(),
            90,
        );

        assert_eq!(epoch.epoch_id, "epoch-001");
        assert!(!epoch.is_revoked);
        assert!(epoch.is_valid());
    }

    #[test]
    fn test_epoch_revocation() {
        let mut epoch = KeyEpoch::new(
            "epoch-001".to_string(),
            "pubkeyhash123".to_string(),
            90,
        );

        epoch.revoke();

        assert!(epoch.is_revoked);
        assert!(!epoch.is_valid());
    }

    #[test]
    fn test_epoch_serialization() {
        let epoch = KeyEpoch::new(
            "epoch-001".to_string(),
            "pubkeyhash123".to_string(),
            90,
        );

        let json = serde_json::to_string(&epoch).unwrap();
        let parsed: KeyEpoch = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.epoch_id, epoch.epoch_id);
        assert_eq!(parsed.public_key_hash, epoch.public_key_hash);
    }
}
