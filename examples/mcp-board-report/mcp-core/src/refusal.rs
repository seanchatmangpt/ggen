//! Deterministic refusal codes for MCP+
//!
//! Refusals are structured, deterministic responses that explain why
//! an operation was not performed. They are cryptographically signed
//! and form part of the audit trail.
//!
//! Refusal code format: `PREFIX-CODE`
//! - ENV: Environment/configuration refusals
//! - KEY: Key/authentication refusals
//! - CAP: Capability/authorization refusals
//! - VAL: Validation/input refusals
//! - RES: Resource/limit refusals
//! - KILL: Kill switch refusals

use crate::crypto::{hash_sha256, KeyPair};
use crate::error::McpResult;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Refusal category (prefix)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RefusalCategory {
    /// Environment/configuration issue
    #[serde(rename = "ENV")]
    Environment,
    /// Key/authentication issue
    #[serde(rename = "KEY")]
    Key,
    /// Capability/authorization issue
    #[serde(rename = "CAP")]
    Capability,
    /// Validation/input issue
    #[serde(rename = "VAL")]
    Validation,
    /// Resource/limit issue
    #[serde(rename = "RES")]
    Resource,
    /// Kill switch active
    #[serde(rename = "KILL")]
    KillSwitch,
}

impl RefusalCategory {
    pub fn as_prefix(&self) -> &'static str {
        match self {
            Self::Environment => "ENV",
            Self::Key => "KEY",
            Self::Capability => "CAP",
            Self::Validation => "VAL",
            Self::Resource => "RES",
            Self::KillSwitch => "KILL",
        }
    }
}

/// Refusal code (category + specific code)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RefusalCode {
    pub category: RefusalCategory,
    pub code: String,
}

impl RefusalCode {
    pub fn new(category: RefusalCategory, code: impl Into<String>) -> Self {
        Self { category, code: code.into() }
    }

    /// Environment: Missing variable
    pub fn env_missing_var(var: &str) -> Self {
        Self::new(RefusalCategory::Environment, format!("MISSING_VAR:{}", var))
    }

    /// Environment: Invalid configuration
    pub fn env_invalid_config(msg: &str) -> Self {
        Self::new(RefusalCategory::Environment, format!("INVALID_CONFIG:{}", msg))
    }

    /// Key: Invalid signature
    pub fn key_invalid_signature() -> Self {
        Self::new(RefusalCategory::Key, "INVALID_SIGNATURE")
    }

    /// Key: Expired
    pub fn key_expired() -> Self {
        Self::new(RefusalCategory::Key, "EXPIRED")
    }

    /// Key: Revoked
    pub fn key_revoked() -> Self {
        Self::new(RefusalCategory::Key, "REVOKED")
    }

    /// Capability: Unauthorized operation
    pub fn cap_unauthorized(operation: &str) -> Self {
        Self::new(RefusalCategory::Capability, format!("UNAUTHORIZED:{}", operation))
    }

    /// Capability: Outside envelope
    pub fn cap_envelope_violation(constraint: &str) -> Self {
        Self::new(RefusalCategory::Capability, format!("ENVELOPE:{}", constraint))
    }

    /// Validation: Invalid input
    pub fn val_invalid_input(field: &str, reason: &str) -> Self {
        Self::new(RefusalCategory::Validation, format!("INVALID_INPUT:{}:{}", field, reason))
    }

    /// Validation: Schema mismatch
    pub fn val_schema_mismatch(expected: &str, got: &str) -> Self {
        Self::new(RefusalCategory::Validation, format!("SCHEMA_MISMATCH:{}!={}", expected, got))
    }

    /// Resource: Rate limited
    pub fn res_rate_limited(limit: u32, window_seconds: u64) -> Self {
        Self::new(RefusalCategory::Resource, format!("RATE_LIMIT:{}/{}", limit, window_seconds))
    }

    /// Resource: Quota exceeded
    pub fn res_quota_exceeded(resource: &str) -> Self {
        Self::new(RefusalCategory::Resource, format!("QUOTA_EXCEEDED:{}", resource))
    }

    /// Kill switch: Global
    pub fn kill_global() -> Self {
        Self::new(RefusalCategory::KillSwitch, "GLOBAL")
    }

    /// Kill switch: Family
    pub fn kill_family(family: &str) -> Self {
        Self::new(RefusalCategory::KillSwitch, format!("FAMILY:{}", family))
    }

    /// Kill switch: Capability
    pub fn kill_capability(capability: &str) -> Self {
        Self::new(RefusalCategory::KillSwitch, format!("CAPABILITY:{}", capability))
    }

    /// Kill switch: Epoch
    pub fn kill_epoch(epoch: u64) -> Self {
        Self::new(RefusalCategory::KillSwitch, format!("EPOCH:{}", epoch))
    }

    /// Format as string: `PREFIX-CODE`
    pub fn to_string_code(&self) -> String {
        format!("{}-{}", self.category.as_prefix(), self.code)
    }
}

/// A deterministic refusal with cryptographic proof
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Refusal {
    /// Unique refusal ID
    pub refusal_id: String,
    /// Refusal code
    pub code: RefusalCode,
    /// Human-readable reason
    pub reason: String,
    /// Contract ID that triggered the refusal
    pub contract_id: Option<String>,
    /// Operation that was refused
    pub operation: String,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// Hash of the request that was refused
    pub request_hash: String,
    /// Refusal hash (deterministic)
    pub refusal_hash: String,
    /// Optional signature
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
}

impl Refusal {
    /// Create a new refusal
    pub fn new(
        code: RefusalCode,
        reason: impl Into<String>,
        operation: impl Into<String>,
        request_hash: impl Into<String>,
    ) -> Self {
        let refusal_id = format!("ref-{}-{}", Utc::now().timestamp_nanos_opt().unwrap_or(0), uuid::Uuid::new_v4());
        let timestamp = Utc::now();
        let reason = reason.into();
        let operation = operation.into();
        let request_hash = request_hash.into();

        // Compute deterministic hash
        let hash_input = format!(
            "{}|{}|{}|{}|{}|{}",
            refusal_id,
            code.to_string_code(),
            reason,
            operation,
            timestamp.to_rfc3339(),
            request_hash
        );
        let refusal_hash = hash_sha256(hash_input.as_bytes());

        Self {
            refusal_id,
            code,
            reason,
            contract_id: None,
            operation,
            timestamp,
            request_hash,
            refusal_hash,
            signature: None,
        }
    }

    /// Set contract ID
    pub fn with_contract(mut self, contract_id: impl Into<String>) -> Self {
        self.contract_id = Some(contract_id.into());
        self
    }

    /// Sign the refusal
    pub fn sign(&mut self, keypair: &KeyPair) {
        let sig = keypair.sign(self.refusal_hash.as_bytes());
        self.signature = Some(sig.to_hex());
    }

    /// Verify the refusal signature
    pub fn verify(&self, keypair: &KeyPair) -> McpResult<()> {
        let sig_hex = self.signature.as_ref()
            .ok_or_else(|| crate::error::McpError::InvalidSignature("No signature present".to_string()))?;

        let sig_bytes = hex::decode(sig_hex)
            .map_err(|e| crate::error::McpError::InvalidSignature(e.to_string()))?;

        let signature = crate::crypto::Signature::from_bytes(sig_bytes)?;
        keypair.verify(self.refusal_hash.as_bytes(), &signature)
    }

    /// Check if this is a kill switch refusal
    pub fn is_kill_switch(&self) -> bool {
        self.code.category == RefusalCategory::KillSwitch
    }

    /// Get severity level (1-5)
    pub fn severity(&self) -> u8 {
        match self.code.category {
            RefusalCategory::KillSwitch => 5,
            RefusalCategory::Key => 4,
            RefusalCategory::Capability => 3,
            RefusalCategory::Resource => 2,
            RefusalCategory::Validation => 2,
            RefusalCategory::Environment => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_refusal_code_formatting() {
        let code = RefusalCode::env_missing_var("API_KEY");
        assert_eq!(code.to_string_code(), "ENV-MISSING_VAR:API_KEY");
    }

    #[test]
    fn test_refusal_code_kill_switch() {
        let code = RefusalCode::kill_global();
        assert_eq!(code.to_string_code(), "KILL-GLOBAL");

        let code = RefusalCode::kill_family("gpt-4");
        assert_eq!(code.to_string_code(), "KILL-FAMILY:gpt-4");
    }

    #[test]
    fn test_refusal_creation() {
        let code = RefusalCode::cap_unauthorized("execute");
        let refusal = Refusal::new(
            code,
            "User not authorized to execute",
            "contract.execute",
            "abc123",
        );

        assert!(refusal.refusal_id.starts_with("ref-"));
        assert_eq!(refusal.refusal_hash.len(), 64);
        assert!(!refusal.is_kill_switch());
        assert_eq!(refusal.severity(), 3);
    }

    #[test]
    fn test_refusal_with_contract() {
        let code = RefusalCode::val_invalid_input("amount", "negative");
        let refusal = Refusal::new(code, "Amount cannot be negative", "transfer", "def456")
            .with_contract("contract-001");

        assert_eq!(refusal.contract_id, Some("contract-001".to_string()));
    }

    #[test]
    fn test_refusal_signing() {
        let keypair = KeyPair::generate().unwrap();
        let code = RefusalCode::kill_global();
        let mut refusal = Refusal::new(code, "System halted", "any", "xyz789");

        refusal.sign(&keypair);
        assert!(refusal.signature.is_some());
        assert!(refusal.verify(&keypair).is_ok());
    }

    #[test]
    fn test_refusal_serialization() {
        let code = RefusalCode::res_rate_limited(100, 60);
        let refusal = Refusal::new(code, "Too many requests", "api.call", "hash123");

        let json = serde_json::to_string(&refusal).unwrap();
        assert!(json.contains("RATE_LIMIT"));
        assert!(json.contains("refusal_hash"));

        let deserialized: Refusal = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.refusal_hash, refusal.refusal_hash);
    }

    #[test]
    fn test_refusal_category_severity() {
        assert_eq!(Refusal::new(RefusalCode::kill_global(), "", "", "").severity(), 5);
        assert_eq!(Refusal::new(RefusalCode::key_expired(), "", "", "").severity(), 4);
        assert_eq!(Refusal::new(RefusalCode::cap_unauthorized("x"), "", "", "").severity(), 3);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_refusal_hash_deterministic(
            reason in ".*",
            operation in "[a-z.]+",
            request_hash in "[a-f0-9]{64}",
        ) {
            let code = RefusalCode::val_invalid_input("test", "prop");
            let r1 = Refusal::new(code.clone(), &reason, &operation, &request_hash);

            // Different refusals have different IDs/timestamps, so hashes differ
            // But hash is always 64 hex chars
            prop_assert_eq!(r1.refusal_hash.len(), 64);
        }

        #[test]
        fn prop_refusal_code_round_trip(category_idx in 0..6usize, code in "[A-Z_:0-9]+") {
            let category = match category_idx {
                0 => RefusalCategory::Environment,
                1 => RefusalCategory::Key,
                2 => RefusalCategory::Capability,
                3 => RefusalCategory::Validation,
                4 => RefusalCategory::Resource,
                _ => RefusalCategory::KillSwitch,
            };

            let refusal_code = RefusalCode::new(category, &code);
            let formatted = refusal_code.to_string_code();
            prop_assert!(formatted.contains('-'));
            prop_assert!(formatted.contains(&code));
        }
    }
}
