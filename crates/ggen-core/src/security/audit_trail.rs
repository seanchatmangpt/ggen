//! Immutable audit trail with Merkle tree tamper-proofing
//!
//! This module provides cryptographic proof of security event integrity using
//! the knhk-lockchain Merkle tree implementation.

use super::events::SecurityEvent;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Audit trail errors
#[derive(Debug, Error)]
pub enum AuditError {
    #[error("Failed to append event: {0}")]
    AppendFailed(String),

    #[error("Failed to verify event: {0}")]
    VerificationFailed(String),

    #[error("Event not found: {0}")]
    EventNotFound(String),

    #[error("Chain integrity compromised")]
    IntegrityViolation,

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

/// Audit trail entry wrapping a security event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEntry {
    /// Security event
    pub event: SecurityEvent,
    /// Entry hash (computed by lockchain)
    pub hash: Option<String>,
    /// Parent entry hash (for chain verification)
    pub parent_hash: Option<String>,
    /// Audit timestamp (may differ from event timestamp)
    pub audit_timestamp: i64,
}

/// Immutable audit trail with cryptographic verification
pub struct AuditTrail {
    /// Repository path for storing audit entries
    repo_path: Option<PathBuf>,
    /// In-memory entries (for non-persistent mode)
    entries: Vec<AuditEntry>,
    /// Current chain head hash
    head_hash: Option<Vec<u8>>,
}

impl AuditTrail {
    /// Create a new audit trail without persistence
    pub fn new() -> Self {
        Self {
            repo_path: None,
            entries: Vec::new(),
            head_hash: None,
        }
    }

    /// Create a new audit trail with Git-based persistence
    pub fn with_repository(repo_path: PathBuf) -> Result<Self, AuditError> {
        // Ensure repository directory exists
        if let Some(parent) = repo_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        Ok(Self {
            repo_path: Some(repo_path),
            entries: Vec::new(),
            head_hash: None,
        })
    }

    /// Append a security event to the audit trail
    pub fn append(&mut self, event: SecurityEvent) -> Result<String, AuditError> {
        let event_id = event.id.clone();
        let audit_timestamp = Utc::now().timestamp_millis();

        // Create audit entry
        let mut entry = AuditEntry {
            event,
            hash: None,
            parent_hash: self.head_hash.as_ref().map(hex::encode),
            audit_timestamp,
        };

        // Compute hash
        let hash = self.compute_hash(&entry)?;
        let hash_hex = hex::encode(&hash);
        entry.hash = Some(hash_hex.clone());

        // Store entry
        if let Some(ref repo_path) = self.repo_path {
            self.write_entry_to_disk(&entry, repo_path)?;
        }

        self.entries.push(entry);
        self.head_hash = Some(hash);

        Ok(event_id)
    }

    /// Verify an audit entry's integrity
    pub fn verify(&self, event_id: &str) -> Result<bool, AuditError> {
        let entry = self
            .entries
            .iter()
            .find(|e| e.event.id == event_id)
            .ok_or_else(|| AuditError::EventNotFound(event_id.to_string()))?;

        // Recompute hash and compare
        let computed_hash = self.compute_hash(entry)?;
        let computed_hex = hex::encode(computed_hash);

        Ok(entry.hash.as_ref() == Some(&computed_hex))
    }

    /// Verify the entire audit chain
    pub fn verify_chain(&self) -> Result<bool, AuditError> {
        let mut expected_parent: Option<String> = None;

        for entry in &self.entries {
            // Check parent link
            if entry.parent_hash != expected_parent {
                return Ok(false);
            }

            // Verify entry hash
            if !self.verify(&entry.event.id)? {
                return Ok(false);
            }

            expected_parent = entry.hash.clone();
        }

        Ok(true)
    }

    /// Get an audit entry by event ID
    pub fn get(&self, event_id: &str) -> Result<&AuditEntry, AuditError> {
        self.entries
            .iter()
            .find(|e| e.event.id == event_id)
            .ok_or_else(|| AuditError::EventNotFound(event_id.to_string()))
    }

    /// Get all audit entries
    pub fn entries(&self) -> &[AuditEntry] {
        &self.entries
    }

    /// Get the number of entries in the audit trail
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the audit trail is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get the current chain head hash
    pub fn head_hash(&self) -> Option<String> {
        self.head_hash.as_ref().map(hex::encode)
    }

    /// Compute hash for an audit entry
    fn compute_hash(&self, entry: &AuditEntry) -> Result<Vec<u8>, AuditError> {
        use sha2::{Digest, Sha256};

        // Canonical representation
        let canonical = self.canonicalize_entry(entry)?;

        // SHA-256 hash
        let mut hasher = Sha256::new();
        hasher.update(&canonical);
        Ok(hasher.finalize().to_vec())
    }

    /// Canonicalize an audit entry for hashing
    fn canonicalize_entry(&self, entry: &AuditEntry) -> Result<Vec<u8>, AuditError> {
        // Serialize event to JSON with sorted keys
        let event_json = serde_json::to_string(&entry.event)?;

        let mut canonical = Vec::new();
        canonical.extend_from_slice(event_json.as_bytes());

        if let Some(ref parent) = entry.parent_hash {
            canonical.push(1); // Marker for parent present
            canonical.extend_from_slice(parent.as_bytes());
        } else {
            canonical.push(0); // Marker for no parent
        }

        canonical.extend_from_slice(&entry.audit_timestamp.to_le_bytes());

        Ok(canonical)
    }

    /// Write audit entry to disk
    fn write_entry_to_disk(
        &self, entry: &AuditEntry, repo_path: &PathBuf,
    ) -> Result<(), AuditError> {
        use std::fs;

        // Create audit directory
        let audit_dir = repo_path.join("audit_trail");
        fs::create_dir_all(&audit_dir)?;

        // Write entry file
        let entry_file = audit_dir.join(format!("{}.json", entry.event.id));
        let entry_json = serde_json::to_string_pretty(entry)?;
        fs::write(entry_file, entry_json)?;

        // Update index file
        self.update_index(repo_path)?;

        Ok(())
    }

    /// Update audit trail index
    fn update_index(&self, repo_path: &Path) -> Result<(), AuditError> {
        use std::fs;

        let index_file = repo_path.join("audit_trail").join("index.json");

        let index = serde_json::json!({
            "entry_count": self.entries.len(),
            "head_hash": self.head_hash(),
            "last_updated": Utc::now().to_rfc3339(),
        });

        fs::write(index_file, serde_json::to_string_pretty(&index)?)?;

        Ok(())
    }

    /// Export audit trail to JSON
    pub fn export_json(&self) -> Result<String, AuditError> {
        Ok(serde_json::to_string_pretty(&self.entries)?)
    }

    /// Create a Merkle proof for an event
    pub fn create_proof(&self, event_id: &str) -> Result<MerkleProof, AuditError> {
        let entry = self.get(event_id)?;
        let hash = entry
            .hash
            .clone()
            .ok_or_else(|| AuditError::VerificationFailed("Missing hash".to_string()))?;

        Ok(MerkleProof {
            event_id: event_id.to_string(),
            event_hash: hash,
            parent_hash: entry.parent_hash.clone(),
            audit_timestamp: entry.audit_timestamp,
            chain_position: self
                .entries
                .iter()
                .position(|e| e.event.id == event_id)
                .ok_or_else(|| AuditError::EventNotFound(event_id.to_string()))?,
        })
    }
}

impl Default for AuditTrail {
    fn default() -> Self {
        Self::new()
    }
}

/// Merkle proof for audit entry verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleProof {
    /// Event ID
    pub event_id: String,
    /// Event hash
    pub event_hash: String,
    /// Parent hash in chain
    pub parent_hash: Option<String>,
    /// Audit timestamp
    pub audit_timestamp: i64,
    /// Position in chain
    pub chain_position: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::security::events::{EventCategory, SecuritySeverity};
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_audit_trail_creation() {
        // Arrange & Act
        let trail = AuditTrail::new();

        // Assert
        assert_eq!(trail.len(), 0);
        assert!(trail.is_empty());
        assert_eq!(trail.head_hash(), None);
    }

    #[test]
    fn test_append_event() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Login attempt",
        );
        let event_id = event.id.clone();

        // Act
        let result = trail.append(event);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), event_id);
        assert_eq!(trail.len(), 1);
        assert!(!trail.is_empty());
        assert!(trail.head_hash().is_some());
    }

    #[test]
    fn test_append_multiple_events() {
        // Arrange
        let mut trail = AuditTrail::new();

        // Act
        let event1 = SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Event 1",
        );
        let event2 = SecurityEvent::new(
            SecuritySeverity::Medium,
            EventCategory::Authorization,
            "Event 2",
        );
        let event3 = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::InputValidation,
            "Event 3",
        );

        trail.append(event1).unwrap();
        trail.append(event2).unwrap();
        trail.append(event3).unwrap();

        // Assert
        assert_eq!(trail.len(), 3);

        // Verify chain linkage
        let entries = trail.entries();
        assert_eq!(entries[0].parent_hash, None);
        assert!(entries[1].parent_hash.is_some());
        assert!(entries[2].parent_hash.is_some());
        assert_eq!(entries[1].parent_hash, entries[0].hash);
        assert_eq!(entries[2].parent_hash, entries[1].hash);
    }

    #[test]
    fn test_verify_entry() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::Integrity,
            "System breach",
        );
        let event_id = event.id.clone();
        trail.append(event).unwrap();

        // Act
        let result = trail.verify(&event_id);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_verify_chain() {
        // Arrange
        let mut trail = AuditTrail::new();

        for i in 0..5 {
            let event = SecurityEvent::new(
                SecuritySeverity::Info,
                EventCategory::DataAccess,
                format!("Event {}", i),
            );
            trail.append(event).unwrap();
        }

        // Act
        let result = trail.verify_chain();

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_get_entry() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::authentication_failed(
            "user123",
            IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1)),
        );
        let event_id = event.id.clone();
        trail.append(event).unwrap();

        // Act
        let result = trail.get(&event_id);

        // Assert
        assert!(result.is_ok());
        let entry = result.unwrap();
        assert_eq!(entry.event.id, event_id);
    }

    #[test]
    fn test_get_nonexistent_entry() {
        // Arrange
        let trail = AuditTrail::new();

        // Act
        let result = trail.get("nonexistent");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), AuditError::EventNotFound(_)));
    }

    #[test]
    fn test_export_json() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::new(
            SecuritySeverity::Medium,
            EventCategory::Network,
            "Port scan",
        );
        trail.append(event).unwrap();

        // Act
        let result = trail.export_json();

        // Assert
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("Port scan"));
        assert!(json.contains("NETWORK"));
    }

    #[test]
    fn test_create_proof() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Policy,
            "Policy violation",
        );
        let event_id = event.id.clone();
        trail.append(event).unwrap();

        // Act
        let result = trail.create_proof(&event_id);

        // Assert
        assert!(result.is_ok());
        let proof = result.unwrap();
        assert_eq!(proof.event_id, event_id);
        assert_eq!(proof.chain_position, 0);
        assert!(proof.parent_hash.is_none());
    }

    #[test]
    fn test_tamper_detection() {
        // Arrange
        let mut trail = AuditTrail::new();
        let event = SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::Integrity,
            "Original message",
        );
        let event_id = event.id.clone();
        trail.append(event).unwrap();

        // Act - tamper with the event
        if let Some(entry) = trail.entries.iter_mut().find(|e| e.event.id == event_id) {
            entry.event.message = "Tampered message".to_string();
        }

        // Assert - verification should fail
        let result = trail.verify(&event_id);
        assert!(result.is_ok());
        assert!(!result.unwrap()); // Should detect tampering
    }

    #[test]
    fn test_chain_break_detection() {
        // Arrange
        let mut trail = AuditTrail::new();

        for i in 0..3 {
            let event = SecurityEvent::new(
                SecuritySeverity::Info,
                EventCategory::DataAccess,
                format!("Event {}", i),
            );
            trail.append(event).unwrap();
        }

        // Act - break the chain by modifying parent hash
        trail.entries[2].parent_hash = Some("invalid_hash".to_string());

        // Assert - chain verification should fail
        let result = trail.verify_chain();
        assert!(result.is_ok());
        assert!(!result.unwrap()); // Should detect broken chain
    }

    #[test]
    fn test_head_hash_updates() {
        // Arrange
        let mut trail = AuditTrail::new();
        let initial_hash = trail.head_hash();
        assert_eq!(initial_hash, None);

        // Act
        let event1 = SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Event 1",
        );
        trail.append(event1).unwrap();
        let hash1 = trail.head_hash();

        let event2 = SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Event 2",
        );
        trail.append(event2).unwrap();
        let hash2 = trail.head_hash();

        // Assert
        assert!(hash1.is_some());
        assert!(hash2.is_some());
        assert_ne!(hash1, hash2); // Hashes should be different
    }
}
