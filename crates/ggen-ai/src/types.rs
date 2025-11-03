//! Type-safe wrappers for ggen-ai
//!
//! This module provides NewType wrappers for string-based identifiers to improve type safety
//! and prevent common errors like passing the wrong ID type to a function.

use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

/// Policy ID newtype for type safety
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PolicyId(String);

impl PolicyId {
    /// Generate a new random policy ID
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }

    /// Create from an existing string
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the inner string reference
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert into the inner string
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl Default for PolicyId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for PolicyId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for PolicyId {
    fn from(s: String) -> Self {
        Self::from_string(s)
    }
}

impl From<&str> for PolicyId {
    fn from(s: &str) -> Self {
        Self::from_string(s.to_string())
    }
}

impl AsRef<str> for PolicyId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Request ID newtype for approval workflows
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RequestId(String);

impl RequestId {
    /// Generate a new random request ID
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }

    /// Create from an existing string
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the inner string reference
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert into the inner string
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl Default for RequestId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for RequestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for RequestId {
    fn from(s: String) -> Self {
        Self::from_string(s)
    }
}

impl From<&str> for RequestId {
    fn from(s: &str) -> Self {
        Self::from_string(s.to_string())
    }
}

impl AsRef<str> for RequestId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Rule ID newtype for policy rules
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RuleId(String);

impl RuleId {
    /// Generate a new random rule ID
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }

    /// Create from an existing string
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the inner string reference
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert into the inner string
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl Default for RuleId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for RuleId {
    fn from(s: String) -> Self {
        Self::from_string(s)
    }
}

impl From<&str> for RuleId {
    fn from(s: &str) -> Self {
        Self::from_string(s.to_string())
    }
}

impl AsRef<str> for RuleId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Decision ID newtype for governance decisions
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct DecisionId(String);

impl DecisionId {
    /// Generate a new random decision ID
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }

    /// Create from an existing string
    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    /// Get the inner string reference
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert into the inner string
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl Default for DecisionId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for DecisionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for DecisionId {
    fn from(s: String) -> Self {
        Self::from_string(s)
    }
}

impl From<&str> for DecisionId {
    fn from(s: &str) -> Self {
        Self::from_string(s.to_string())
    }
}

impl AsRef<str> for DecisionId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_policy_id_creation() {
        let id1 = PolicyId::new();
        let id2 = PolicyId::new();
        assert_ne!(id1, id2, "Each new PolicyId should be unique");
    }

    #[test]
    fn test_policy_id_from_string() {
        let id_str = "test-policy-123".to_string();
        let id = PolicyId::from_string(id_str.clone());
        assert_eq!(id.as_str(), "test-policy-123");
        assert_eq!(id.to_string(), "test-policy-123");
    }

    #[test]
    fn test_policy_id_conversions() {
        let id = PolicyId::from("test-id");
        assert_eq!(id.as_str(), "test-id");

        let id_from_string = PolicyId::from("another-id".to_string());
        assert_eq!(id_from_string.as_str(), "another-id");
    }

    #[test]
    fn test_request_id_creation() {
        let id1 = RequestId::new();
        let id2 = RequestId::new();
        assert_ne!(id1, id2, "Each new RequestId should be unique");
    }

    #[test]
    fn test_request_id_from_string() {
        let id_str = "test-request-456".to_string();
        let id = RequestId::from_string(id_str.clone());
        assert_eq!(id.as_str(), "test-request-456");
    }

    #[test]
    fn test_rule_id_creation() {
        let id1 = RuleId::new();
        let id2 = RuleId::new();
        assert_ne!(id1, id2, "Each new RuleId should be unique");
    }

    #[test]
    fn test_decision_id_creation() {
        let id1 = DecisionId::new();
        let id2 = DecisionId::new();
        assert_ne!(id1, id2, "Each new DecisionId should be unique");
    }

    #[test]
    fn test_serialization() {
        let id = PolicyId::from("test-id");
        let json = serde_json::to_string(&id).expect("Failed to serialize");
        assert_eq!(json, "\"test-id\"");

        let deserialized: PolicyId = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(deserialized, id);
    }

    #[test]
    fn test_id_type_safety() {
        let policy_id = PolicyId::from("policy-123");
        let request_id = RequestId::from("request-123");

        // This test demonstrates that different ID types cannot be accidentally mixed
        // The following would not compile:
        // let _: PolicyId = request_id; // Type error!

        assert_ne!(policy_id.as_str(), request_id.as_str());
    }
}
