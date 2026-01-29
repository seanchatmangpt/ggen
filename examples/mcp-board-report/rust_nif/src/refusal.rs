//! Deterministic Refusal Module
//!
//! Provides structured refusal codes for envelope violations.
//! Refusals are deterministic - same violation always produces same code.

use serde::{Deserialize, Serialize};

/// Refusal category prefixes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RefusalCategory {
    /// ENV - Envelope bound violations
    Envelope,
    /// KEY - Key/signing failures
    Key,
    /// CAP - Capability violations
    Capability,
    /// VAL - Validation failures
    Validation,
    /// RES - Resource exhaustion
    Resource,
    /// KILL - Kill switch activated
    KillSwitch,
}

impl RefusalCategory {
    pub fn prefix(&self) -> &'static str {
        match self {
            RefusalCategory::Envelope => "ENV",
            RefusalCategory::Key => "KEY",
            RefusalCategory::Capability => "CAP",
            RefusalCategory::Validation => "VAL",
            RefusalCategory::Resource => "RES",
            RefusalCategory::KillSwitch => "KILL",
        }
    }
}

/// Refusal code with category and numeric identifier
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RefusalCode {
    pub category: RefusalCategory,
    pub code: u16,
}

impl RefusalCode {
    pub fn new(category: RefusalCategory, code: u16) -> Self {
        Self { category, code }
    }

    pub fn as_str(&self) -> String {
        format!("{}-{:04}", self.category.prefix(), self.code)
    }

    // Envelope violations (ENV-0001 to ENV-0099)
    pub fn time_exceeded() -> Self { Self::new(RefusalCategory::Envelope, 1) }
    pub fn memory_exceeded() -> Self { Self::new(RefusalCategory::Envelope, 2) }
    pub fn scope_violation() -> Self { Self::new(RefusalCategory::Envelope, 3) }
    pub fn capability_missing() -> Self { Self::new(RefusalCategory::Envelope, 4) }
    pub fn operation_limit() -> Self { Self::new(RefusalCategory::Envelope, 5) }

    // Key failures (KEY-0001 to KEY-0099)
    pub fn signature_invalid() -> Self { Self::new(RefusalCategory::Key, 1) }
    pub fn epoch_expired() -> Self { Self::new(RefusalCategory::Key, 2) }
    pub fn epoch_revoked() -> Self { Self::new(RefusalCategory::Key, 3) }
    pub fn key_not_found() -> Self { Self::new(RefusalCategory::Key, 4) }

    // Capability violations (CAP-0001 to CAP-0099)
    pub fn unauthorized_scope() -> Self { Self::new(RefusalCategory::Capability, 1) }
    pub fn unauthorized_action() -> Self { Self::new(RefusalCategory::Capability, 2) }
    pub fn unauthorized_resource() -> Self { Self::new(RefusalCategory::Capability, 3) }

    // Validation failures (VAL-0001 to VAL-0099)
    pub fn invalid_input() -> Self { Self::new(RefusalCategory::Validation, 1) }
    pub fn schema_violation() -> Self { Self::new(RefusalCategory::Validation, 2) }
    pub fn chain_broken() -> Self { Self::new(RefusalCategory::Validation, 3) }

    // Resource exhaustion (RES-0001 to RES-0099)
    pub fn budget_exhausted() -> Self { Self::new(RefusalCategory::Resource, 1) }
    pub fn rate_limited() -> Self { Self::new(RefusalCategory::Resource, 2) }
    pub fn quota_exceeded() -> Self { Self::new(RefusalCategory::Resource, 3) }

    // Kill switch (KILL-0001 to KILL-0099)
    pub fn global_disabled() -> Self { Self::new(RefusalCategory::KillSwitch, 1) }
    pub fn family_disabled() -> Self { Self::new(RefusalCategory::KillSwitch, 2) }
    pub fn capability_disabled() -> Self { Self::new(RefusalCategory::KillSwitch, 3) }
    pub fn epoch_disabled() -> Self { Self::new(RefusalCategory::KillSwitch, 4) }
}

/// A complete refusal with code, message, and context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Refusal {
    pub code: RefusalCode,
    pub message: String,
    pub envelope_id: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub context: std::collections::HashMap<String, String>,
}

impl Refusal {
    pub fn new(code: RefusalCode, message: String, envelope_id: String) -> Self {
        Self {
            code,
            message,
            envelope_id,
            timestamp: chrono::Utc::now(),
            context: std::collections::HashMap::new(),
        }
    }

    pub fn with_context(mut self, key: &str, value: &str) -> Self {
        self.context.insert(key.to_string(), value.to_string());
        self
    }

    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_refusal_code_format() {
        assert_eq!(RefusalCode::time_exceeded().as_str(), "ENV-0001");
        assert_eq!(RefusalCode::signature_invalid().as_str(), "KEY-0001");
        assert_eq!(RefusalCode::global_disabled().as_str(), "KILL-0001");
    }

    #[test]
    fn test_refusal_creation() {
        let refusal = Refusal::new(
            RefusalCode::time_exceeded(),
            "Execution time exceeded 5000ms limit".to_string(),
            "env-123".to_string(),
        );

        assert_eq!(refusal.code.as_str(), "ENV-0001");
        assert_eq!(refusal.envelope_id, "env-123");
    }

    #[test]
    fn test_refusal_with_context() {
        let refusal = Refusal::new(
            RefusalCode::memory_exceeded(),
            "Memory limit exceeded".to_string(),
            "env-456".to_string(),
        )
        .with_context("limit_bytes", "104857600")
        .with_context("used_bytes", "134217728");

        assert_eq!(refusal.context.len(), 2);
        assert_eq!(refusal.context.get("limit_bytes"), Some(&"104857600".to_string()));
    }

    #[test]
    fn test_refusal_serialization() {
        let refusal = Refusal::new(
            RefusalCode::scope_violation(),
            "Scope violation".to_string(),
            "env-789".to_string(),
        );

        let json = refusal.to_json().unwrap();
        // Verify JSON contains the code components (category: Envelope, code: 3)
        assert!(json.contains("Envelope") && json.contains("\"code\": 3"));
    }

    #[test]
    fn test_all_categories_have_prefixes() {
        assert_eq!(RefusalCategory::Envelope.prefix(), "ENV");
        assert_eq!(RefusalCategory::Key.prefix(), "KEY");
        assert_eq!(RefusalCategory::Capability.prefix(), "CAP");
        assert_eq!(RefusalCategory::Validation.prefix(), "VAL");
        assert_eq!(RefusalCategory::Resource.prefix(), "RES");
        assert_eq!(RefusalCategory::KillSwitch.prefix(), "KILL");
    }
}
