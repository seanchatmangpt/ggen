//! Capability Delegation Chains
//!
//! Contracts can delegate a SUBSET of their capabilities to child contracts.
//! Delegation is transitive but can only narrow, never expand.
//!
//! Core axiom: child.capabilities is a subset of parent.capabilities
//!
//! # Example
//! ```rust,ignore
//! use mcp_core::delegation::{DelegationBuilder, DelegationChain};
//! use mcp_core::types::{Capability, Envelope};
//! use mcp_core::crypto::KeyPair;
//!
//! // Parent contract with full capabilities
//! let parent_envelope = Envelope::full_trust();
//! let parent_keypair = KeyPair::generate().unwrap();
//!
//! // Create delegation to child with reduced capabilities
//! let delegation = DelegationBuilder::new("parent-contract", "child-contract")
//!     .capability(Capability::Read)
//!     .capability(Capability::Write)
//!     .max_operations(100)
//!     .expires_in(std::time::Duration::from_secs(3600))
//!     .sign(&parent_keypair);
//!
//! // Build and verify the delegation chain
//! let mut chain = DelegationChain::from_root(&parent_envelope);
//! chain.delegate(delegation).unwrap();
//! chain.verify().unwrap();
//! ```

use crate::crypto::{hash_sha256, KeyPair, Signature};
use crate::error::{McpError, McpResult};
use crate::types::{Capability, Envelope};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// A signed delegation from parent to child contract.
///
/// Delegations encode a capability transfer that can only narrow permissions,
/// never expand them. Each delegation is cryptographically signed by the parent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Delegation {
    /// Unique identifier for this delegation
    pub delegation_id: String,
    /// Contract ID of the delegating parent
    pub parent_contract: String,
    /// Contract ID of the receiving child
    pub child_contract: String,
    /// Capabilities being delegated (must be subset of parent's capabilities)
    pub delegated_capabilities: HashSet<Capability>,
    /// Additional constraints on the delegation
    pub constraints: DelegationConstraints,
    /// Optional expiration time for the delegation
    pub expires_at: Option<DateTime<Utc>>,
    /// Cryptographic signature of the delegation by the parent
    pub signature: String,
    /// Public key hash of the signing parent (for verification)
    pub signer_public_key_hash: String,
}

impl Delegation {
    /// Check if this delegation has expired
    pub fn is_expired(&self) -> bool {
        if let Some(expires_at) = self.expires_at {
            Utc::now() > expires_at
        } else {
            false
        }
    }

    /// Check if a specific capability is delegated
    pub fn has_capability(&self, cap: Capability) -> bool {
        self.delegated_capabilities.contains(&cap)
    }

    /// Compute the hash of delegation data (for signing/verification)
    pub fn compute_hash(&self) -> String {
        let mut caps: Vec<_> = self.delegated_capabilities.iter().collect();
        caps.sort_by_key(|c| c.as_str());
        let caps_str: Vec<_> = caps.iter().map(|c| c.as_str()).collect();

        let hash_input = format!(
            "{}|{}|{}|{:?}|{:?}|{:?}",
            self.delegation_id,
            self.parent_contract,
            self.child_contract,
            caps_str,
            self.constraints,
            self.expires_at.map(|t| t.to_rfc3339())
        );
        hash_sha256(hash_input.as_bytes())
    }

    /// Verify the delegation signature
    pub fn verify_signature(&self, keypair: &KeyPair) -> McpResult<()> {
        // Verify the public key hash matches
        let expected_hash = keypair.public_key_hash();
        if expected_hash != self.signer_public_key_hash {
            return Err(McpError::DelegationError(
                "Signer public key hash mismatch".to_string(),
            ));
        }

        let hash = self.compute_hash();
        let sig_bytes = hex::decode(&self.signature)
            .map_err(|e| McpError::InvalidSignature(e.to_string()))?;
        let signature = Signature::from_bytes(sig_bytes)?;
        keypair.verify(hash.as_bytes(), &signature)
    }
}

/// Constraints that can be placed on a delegation.
///
/// These constraints further restrict what a child contract can do,
/// beyond the capability restrictions.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct DelegationConstraints {
    /// Maximum number of operations the child can perform
    pub max_operations: Option<u64>,
    /// Maximum duration the child can run (in milliseconds)
    pub max_duration_ms: Option<u64>,
    /// Patterns that must be included in child's output
    pub required_patterns: Vec<String>,
    /// Patterns that must NOT appear in child's output
    pub forbidden_patterns: Vec<String>,
    /// Maximum memory usage in bytes
    pub max_memory_bytes: Option<u64>,
    /// Whether the child can further delegate (default: false)
    pub can_delegate: bool,
    /// Maximum depth of further delegation (if can_delegate is true)
    pub max_delegation_depth: Option<u32>,
}

impl DelegationConstraints {
    /// Create empty constraints (no restrictions beyond capabilities)
    pub fn none() -> Self {
        Self::default()
    }

    /// Create restrictive constraints with operation and duration limits
    pub fn restrictive(max_operations: u64, max_duration_ms: u64) -> Self {
        Self {
            max_operations: Some(max_operations),
            max_duration_ms: Some(max_duration_ms),
            ..Default::default()
        }
    }

    /// Check if these constraints are satisfied by child constraints.
    /// Child constraints must be equal or MORE restrictive.
    pub fn is_satisfied_by(&self, child: &DelegationConstraints) -> bool {
        // Check max_operations: child must have same or lower limit
        if let Some(parent_max) = self.max_operations {
            match child.max_operations {
                Some(child_max) if child_max <= parent_max => {}
                None => return false, // Child has no limit but parent does
                _ => return false,
            }
        }

        // Check max_duration_ms: child must have same or lower limit
        if let Some(parent_max) = self.max_duration_ms {
            match child.max_duration_ms {
                Some(child_max) if child_max <= parent_max => {}
                None => return false,
                _ => return false,
            }
        }

        // Check max_memory_bytes: child must have same or lower limit
        if let Some(parent_max) = self.max_memory_bytes {
            match child.max_memory_bytes {
                Some(child_max) if child_max <= parent_max => {}
                None => return false,
                _ => return false,
            }
        }

        // Check forbidden_patterns: child must include all parent's forbidden patterns
        for pattern in &self.forbidden_patterns {
            if !child.forbidden_patterns.contains(pattern) {
                return false;
            }
        }

        // Check required_patterns: child must include all parent's required patterns
        for pattern in &self.required_patterns {
            if !child.required_patterns.contains(pattern) {
                return false;
            }
        }

        // Check delegation depth
        if !self.can_delegate && child.can_delegate {
            return false;
        }

        if let Some(parent_depth) = self.max_delegation_depth {
            match child.max_delegation_depth {
                Some(child_depth) if child_depth < parent_depth => {}
                None if !child.can_delegate => {} // OK if child can't delegate
                _ => return false,
            }
        }

        true
    }
}

/// Delegation chain validator.
///
/// Tracks a chain of delegations from a root contract down to child contracts.
/// Ensures the subset property is maintained throughout the chain.
pub struct DelegationChain {
    /// Capabilities of the root contract (from its envelope)
    root_capabilities: HashSet<Capability>,
    /// Constraints from the root envelope
    root_constraints: DelegationConstraints,
    /// Chain of delegations from root to leaf
    delegations: Vec<Delegation>,
}

impl DelegationChain {
    /// Create a new delegation chain from a root contract's envelope.
    ///
    /// The envelope defines the maximum capabilities that can be delegated.
    pub fn from_root(root_envelope: &Envelope) -> Self {
        let root_capabilities: HashSet<Capability> =
            root_envelope.capabilities.iter().copied().collect();

        let root_constraints = DelegationConstraints {
            max_operations: root_envelope.max_operations,
            max_duration_ms: root_envelope.max_duration_ms,
            max_memory_bytes: root_envelope.max_memory_bytes,
            forbidden_patterns: root_envelope.disallowed_patterns.clone(),
            can_delegate: true,
            max_delegation_depth: None, // Root has no depth limit by default
            ..Default::default()
        };

        Self {
            root_capabilities,
            root_constraints,
            delegations: Vec::new(),
        }
    }

    /// Add a delegation to the chain.
    ///
    /// Validates that:
    /// 1. The delegated capabilities are a subset of the current effective capabilities
    /// 2. The delegation has not expired
    /// 3. The parent can delegate (if not root)
    /// 4. The constraints are more restrictive than parent's
    pub fn delegate(&mut self, delegation: Delegation) -> McpResult<()> {
        // Check expiration
        if delegation.is_expired() {
            return Err(McpError::DelegationExpired(format!(
                "Delegation {} has expired",
                delegation.delegation_id
            )));
        }

        // Get current effective capabilities and constraints
        let effective_caps = self.effective_capabilities();
        let effective_constraints = self.effective_constraints();

        // Verify subset property: delegated capabilities must be subset of effective
        for cap in &delegation.delegated_capabilities {
            if !effective_caps.contains(cap) {
                return Err(McpError::CapabilityViolation(format!(
                    "Cannot delegate capability {:?}: not in parent's capabilities",
                    cap
                )));
            }
        }

        // Check if current level can delegate
        if !self.delegations.is_empty() && !effective_constraints.can_delegate {
            return Err(McpError::DelegationError(
                "Parent contract is not allowed to delegate".to_string(),
            ));
        }

        // Check delegation depth
        if let Some(max_depth) = effective_constraints.max_delegation_depth {
            if self.delegations.len() as u32 >= max_depth {
                return Err(McpError::DelegationError(format!(
                    "Maximum delegation depth {} exceeded",
                    max_depth
                )));
            }
        }

        // Verify constraints are more restrictive
        if !effective_constraints.is_satisfied_by(&delegation.constraints) {
            return Err(McpError::ConstraintViolation(
                "Delegation constraints must be equal or more restrictive than parent's"
                    .to_string(),
            ));
        }

        // Verify parent contract matches chain end
        let expected_parent = if self.delegations.is_empty() {
            None // Root doesn't have a specific parent contract ID in the chain
        } else {
            Some(
                self.delegations
                    .last()
                    .map(|d| d.child_contract.as_str())
                    .unwrap_or_default(),
            )
        };

        if let Some(expected) = expected_parent {
            if delegation.parent_contract != expected {
                return Err(McpError::DelegationError(format!(
                    "Parent contract mismatch: expected '{}', got '{}'",
                    expected, delegation.parent_contract
                )));
            }
        }

        self.delegations.push(delegation);
        Ok(())
    }

    /// Get the effective capabilities at the end of the chain.
    ///
    /// This is the intersection of all delegated capabilities in the chain.
    pub fn effective_capabilities(&self) -> HashSet<Capability> {
        if self.delegations.is_empty() {
            return self.root_capabilities.clone();
        }

        // Start with root capabilities
        let mut caps = self.root_capabilities.clone();

        // Intersect with each delegation's capabilities
        for delegation in &self.delegations {
            caps = caps
                .intersection(&delegation.delegated_capabilities)
                .copied()
                .collect();
        }

        caps
    }

    /// Get the effective constraints at the end of the chain.
    ///
    /// Returns the most restrictive constraints from the chain.
    pub fn effective_constraints(&self) -> DelegationConstraints {
        if self.delegations.is_empty() {
            return self.root_constraints.clone();
        }

        // Start with root constraints, then apply each delegation's constraints
        let mut constraints = self.root_constraints.clone();

        for delegation in &self.delegations {
            let dc = &delegation.constraints;

            // Take the more restrictive max_operations
            constraints.max_operations = match (constraints.max_operations, dc.max_operations) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                (None, None) => None,
            };

            // Take the more restrictive max_duration_ms
            constraints.max_duration_ms = match (constraints.max_duration_ms, dc.max_duration_ms) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                (None, None) => None,
            };

            // Take the more restrictive max_memory_bytes
            constraints.max_memory_bytes = match (constraints.max_memory_bytes, dc.max_memory_bytes)
            {
                (Some(a), Some(b)) => Some(a.min(b)),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                (None, None) => None,
            };

            // Merge forbidden patterns (union)
            for pattern in &dc.forbidden_patterns {
                if !constraints.forbidden_patterns.contains(pattern) {
                    constraints.forbidden_patterns.push(pattern.clone());
                }
            }

            // Merge required patterns (union)
            for pattern in &dc.required_patterns {
                if !constraints.required_patterns.contains(pattern) {
                    constraints.required_patterns.push(pattern.clone());
                }
            }

            // can_delegate must be false if any delegation says so
            constraints.can_delegate = constraints.can_delegate && dc.can_delegate;

            // Take the more restrictive max_delegation_depth
            constraints.max_delegation_depth =
                match (constraints.max_delegation_depth, dc.max_delegation_depth) {
                    (Some(a), Some(b)) => Some(a.min(b)),
                    (Some(a), None) => Some(a),
                    (None, Some(b)) => Some(b),
                    (None, None) => None,
                };
        }

        constraints
    }

    /// Verify the entire delegation chain is valid.
    ///
    /// Checks:
    /// 1. No delegations have expired
    /// 2. Subset property holds at each level
    /// 3. Constraints are properly nested
    pub fn verify(&self) -> McpResult<()> {
        let mut current_caps = self.root_capabilities.clone();
        let mut current_constraints = self.root_constraints.clone();

        for (i, delegation) in self.delegations.iter().enumerate() {
            // Check expiration
            if delegation.is_expired() {
                return Err(McpError::DelegationExpired(format!(
                    "Delegation {} at position {} has expired",
                    delegation.delegation_id, i
                )));
            }

            // Check subset property
            for cap in &delegation.delegated_capabilities {
                if !current_caps.contains(cap) {
                    return Err(McpError::CapabilityViolation(format!(
                        "Delegation {} at position {} violates subset property: {:?} not in parent",
                        delegation.delegation_id, i, cap
                    )));
                }
            }

            // Check constraints
            if !current_constraints.is_satisfied_by(&delegation.constraints) {
                return Err(McpError::ConstraintViolation(format!(
                    "Delegation {} at position {} has less restrictive constraints than parent",
                    delegation.delegation_id, i
                )));
            }

            // Update current capabilities and constraints for next iteration
            current_caps = current_caps
                .intersection(&delegation.delegated_capabilities)
                .copied()
                .collect();

            // Apply constraints
            current_constraints = delegation.constraints.clone();
        }

        Ok(())
    }

    /// Check if a specific capability is allowed at the end of the chain.
    pub fn has_capability(&self, cap: Capability) -> bool {
        self.effective_capabilities().contains(&cap)
    }

    /// Get the chain depth (number of delegations).
    pub fn depth(&self) -> usize {
        self.delegations.len()
    }

    /// Get the child contract at the end of the chain.
    pub fn leaf_contract(&self) -> Option<&str> {
        self.delegations.last().map(|d| d.child_contract.as_str())
    }

    /// Get all delegations in the chain (read-only).
    pub fn delegations(&self) -> &[Delegation] {
        &self.delegations
    }
}

/// Builder for creating delegations.
///
/// Provides a fluent interface for constructing delegations with
/// the correct structure and cryptographic signatures.
pub struct DelegationBuilder {
    parent_contract: String,
    child_contract: String,
    capabilities: HashSet<Capability>,
    constraints: DelegationConstraints,
    expires_at: Option<DateTime<Utc>>,
}

impl DelegationBuilder {
    /// Create a new delegation builder.
    ///
    /// # Arguments
    /// * `parent` - Contract ID of the delegating parent
    /// * `child` - Contract ID of the receiving child
    pub fn new(parent: &str, child: &str) -> Self {
        Self {
            parent_contract: parent.to_string(),
            child_contract: child.to_string(),
            capabilities: HashSet::new(),
            constraints: DelegationConstraints::default(),
            expires_at: None,
        }
    }

    /// Add a capability to delegate.
    pub fn capability(mut self, cap: Capability) -> Self {
        self.capabilities.insert(cap);
        self
    }

    /// Add multiple capabilities to delegate.
    pub fn capabilities(mut self, caps: impl IntoIterator<Item = Capability>) -> Self {
        self.capabilities.extend(caps);
        self
    }

    /// Set the maximum number of operations.
    pub fn max_operations(mut self, max: u64) -> Self {
        self.constraints.max_operations = Some(max);
        self
    }

    /// Set the maximum execution duration in milliseconds.
    pub fn max_duration_ms(mut self, max: u64) -> Self {
        self.constraints.max_duration_ms = Some(max);
        self
    }

    /// Set the maximum memory usage in bytes.
    pub fn max_memory_bytes(mut self, max: u64) -> Self {
        self.constraints.max_memory_bytes = Some(max);
        self
    }

    /// Set an expiration time as a duration from now.
    pub fn expires_in(mut self, duration: std::time::Duration) -> Self {
        self.expires_at = Some(Utc::now() + chrono::Duration::from_std(duration).unwrap());
        self
    }

    /// Set an absolute expiration time.
    pub fn expires_at(mut self, time: DateTime<Utc>) -> Self {
        self.expires_at = Some(time);
        self
    }

    /// Add a required output pattern.
    pub fn require_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.constraints.required_patterns.push(pattern.into());
        self
    }

    /// Add a forbidden output pattern.
    pub fn forbid_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.constraints.forbidden_patterns.push(pattern.into());
        self
    }

    /// Allow the child to further delegate (with optional depth limit).
    pub fn allow_delegation(mut self, max_depth: Option<u32>) -> Self {
        self.constraints.can_delegate = true;
        self.constraints.max_delegation_depth = max_depth;
        self
    }

    /// Disallow further delegation.
    pub fn disallow_delegation(mut self) -> Self {
        self.constraints.can_delegate = false;
        self.constraints.max_delegation_depth = None;
        self
    }

    /// Apply full constraints object.
    pub fn with_constraints(mut self, constraints: DelegationConstraints) -> Self {
        self.constraints = constraints;
        self
    }

    /// Sign and build the delegation.
    ///
    /// The keypair should be the parent contract's signing key.
    pub fn sign(self, keypair: &KeyPair) -> Delegation {
        let delegation_id = format!(
            "del-{}-{}",
            Utc::now().timestamp_nanos_opt().unwrap_or(0),
            uuid::Uuid::new_v4().to_string().split('-').next().unwrap_or("0000")
        );

        let signer_public_key_hash = keypair.public_key_hash();

        let mut delegation = Delegation {
            delegation_id,
            parent_contract: self.parent_contract,
            child_contract: self.child_contract,
            delegated_capabilities: self.capabilities,
            constraints: self.constraints,
            expires_at: self.expires_at,
            signature: String::new(),
            signer_public_key_hash,
        };

        // Compute hash and sign
        let hash = delegation.compute_hash();
        let sig = keypair.sign(hash.as_bytes());
        delegation.signature = sig.to_hex();

        delegation
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_keypair() -> KeyPair {
        KeyPair::from_seed(&[42u8; 32])
    }

    fn full_trust_envelope() -> Envelope {
        Envelope::full_trust()
    }

    fn read_write_envelope() -> Envelope {
        Envelope::default()
            .with_capability(Capability::Read)
            .with_capability(Capability::Write)
            .with_max_operations(1000)
    }

    #[test]
    fn test_delegation_builder_basic() {
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .sign(&keypair);

        assert!(delegation.delegation_id.starts_with("del-"));
        assert_eq!(delegation.parent_contract, "parent");
        assert_eq!(delegation.child_contract, "child");
        assert!(delegation.delegated_capabilities.contains(&Capability::Read));
        assert!(!delegation.signature.is_empty());
    }

    #[test]
    fn test_delegation_builder_multiple_capabilities() {
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("parent", "child")
            .capabilities([Capability::Read, Capability::Write, Capability::Execute])
            .sign(&keypair);

        assert_eq!(delegation.delegated_capabilities.len(), 3);
        assert!(delegation.has_capability(Capability::Read));
        assert!(delegation.has_capability(Capability::Write));
        assert!(delegation.has_capability(Capability::Execute));
    }

    #[test]
    fn test_delegation_builder_with_constraints() {
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .max_operations(100)
            .max_duration_ms(5000)
            .max_memory_bytes(1024 * 1024)
            .require_pattern("output_type:json")
            .forbid_pattern("password")
            .sign(&keypair);

        assert_eq!(delegation.constraints.max_operations, Some(100));
        assert_eq!(delegation.constraints.max_duration_ms, Some(5000));
        assert_eq!(delegation.constraints.max_memory_bytes, Some(1024 * 1024));
        assert!(delegation
            .constraints
            .required_patterns
            .contains(&"output_type:json".to_string()));
        assert!(delegation
            .constraints
            .forbidden_patterns
            .contains(&"password".to_string()));
    }

    #[test]
    fn test_delegation_builder_with_expiration() {
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .expires_in(std::time::Duration::from_secs(3600))
            .sign(&keypair);

        assert!(delegation.expires_at.is_some());
        assert!(!delegation.is_expired());
    }

    #[test]
    fn test_delegation_expiration() {
        let keypair = make_keypair();

        // Create an already expired delegation
        let expired_time = Utc::now() - chrono::Duration::seconds(100);
        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .expires_at(expired_time)
            .sign(&keypair);

        assert!(delegation.is_expired());
    }

    #[test]
    fn test_delegation_signature_verification() {
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .sign(&keypair);

        assert!(delegation.verify_signature(&keypair).is_ok());
    }

    #[test]
    fn test_delegation_signature_wrong_key() {
        let keypair1 = KeyPair::from_seed(&[1u8; 32]);
        let keypair2 = KeyPair::from_seed(&[2u8; 32]);

        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .sign(&keypair1);

        assert!(delegation.verify_signature(&keypair2).is_err());
    }

    #[test]
    fn test_chain_from_root() {
        let envelope = full_trust_envelope();
        let chain = DelegationChain::from_root(&envelope);

        let caps = chain.effective_capabilities();
        assert!(caps.contains(&Capability::Read));
        assert!(caps.contains(&Capability::Write));
        assert!(caps.contains(&Capability::Execute));
        assert!(caps.contains(&Capability::Manage));
        assert!(caps.contains(&Capability::Admin));
    }

    #[test]
    fn test_chain_single_delegation() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        let delegation = DelegationBuilder::new("root", "child")
            .capability(Capability::Read)
            .capability(Capability::Write)
            .sign(&keypair);

        assert!(chain.delegate(delegation).is_ok());

        let caps = chain.effective_capabilities();
        assert!(caps.contains(&Capability::Read));
        assert!(caps.contains(&Capability::Write));
        assert!(!caps.contains(&Capability::Execute));
        assert!(!caps.contains(&Capability::Admin));
    }

    #[test]
    fn test_chain_subset_property_enforced() {
        let envelope = read_write_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // Try to delegate Execute capability which root doesn't have
        let delegation = DelegationBuilder::new("root", "child")
            .capability(Capability::Execute)
            .sign(&keypair);

        let result = chain.delegate(delegation);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::CapabilityViolation(_)));
    }

    #[test]
    fn test_chain_multiple_delegations() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation: root -> child1 (read, write, execute)
        let del1 = DelegationBuilder::new("root", "child1")
            .capabilities([Capability::Read, Capability::Write, Capability::Execute])
            .allow_delegation(Some(2))
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation: child1 -> child2 (read, write only)
        let del2 = DelegationBuilder::new("child1", "child2")
            .capabilities([Capability::Read, Capability::Write])
            .allow_delegation(Some(1))
            .sign(&keypair);
        chain.delegate(del2).unwrap();

        let caps = chain.effective_capabilities();
        assert!(caps.contains(&Capability::Read));
        assert!(caps.contains(&Capability::Write));
        assert!(!caps.contains(&Capability::Execute)); // Lost at child2
        assert_eq!(chain.depth(), 2);
        assert_eq!(chain.leaf_contract(), Some("child2"));
    }

    #[test]
    fn test_chain_cannot_expand_capabilities() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation: root -> child1 (read only)
        let del1 = DelegationBuilder::new("root", "child1")
            .capability(Capability::Read)
            .allow_delegation(Some(1))
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation: child1 -> child2 tries to add Write (should fail)
        let del2 = DelegationBuilder::new("child1", "child2")
            .capabilities([Capability::Read, Capability::Write])
            .sign(&keypair);

        let result = chain.delegate(del2);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::CapabilityViolation(_)));
    }

    #[test]
    fn test_chain_verify() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation allows 2 levels of sub-delegation
        let del1 = DelegationBuilder::new("root", "child1")
            .capability(Capability::Read)
            .allow_delegation(Some(2))
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation is within allowed depth
        let del2 = DelegationBuilder::new("child1", "child2")
            .capability(Capability::Read)
            .allow_delegation(Some(1))
            .sign(&keypair);
        chain.delegate(del2).unwrap();

        assert!(chain.verify().is_ok());
    }

    #[test]
    fn test_chain_rejects_expired_delegation() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        let expired_time = Utc::now() - chrono::Duration::seconds(100);
        let delegation = DelegationBuilder::new("root", "child")
            .capability(Capability::Read)
            .expires_at(expired_time)
            .sign(&keypair);

        let result = chain.delegate(delegation);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::DelegationExpired(_)));
    }

    #[test]
    fn test_chain_parent_contract_mismatch() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation
        let del1 = DelegationBuilder::new("root", "child1")
            .capability(Capability::Read)
            .allow_delegation(Some(1))
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation with wrong parent
        let del2 = DelegationBuilder::new("wrong_parent", "child2")
            .capability(Capability::Read)
            .sign(&keypair);

        let result = chain.delegate(del2);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::DelegationError(_)));
    }

    #[test]
    fn test_chain_delegation_not_allowed() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation: explicitly disallow further delegation
        let del1 = DelegationBuilder::new("root", "child1")
            .capability(Capability::Read)
            .disallow_delegation()
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation should fail
        let del2 = DelegationBuilder::new("child1", "child2")
            .capability(Capability::Read)
            .sign(&keypair);

        let result = chain.delegate(del2);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), McpError::DelegationError(_)));
    }

    #[test]
    fn test_chain_max_delegation_depth() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // First delegation allows 2 levels of sub-delegation
        let del1 = DelegationBuilder::new("root", "child1")
            .capability(Capability::Read)
            .allow_delegation(Some(2))
            .sign(&keypair);
        chain.delegate(del1).unwrap();

        // Second delegation allows 0 more sub-delegations
        let del2 = DelegationBuilder::new("child1", "child2")
            .capability(Capability::Read)
            .allow_delegation(Some(0))
            .sign(&keypair);
        chain.delegate(del2).unwrap();

        // Third delegation should fail (child2 has no further delegation allowed)
        let del3 = DelegationBuilder::new("child2", "child3")
            .capability(Capability::Read)
            .sign(&keypair);

        let result = chain.delegate(del3);
        assert!(result.is_err());
    }

    #[test]
    fn test_constraints_satisfaction() {
        let parent = DelegationConstraints {
            max_operations: Some(100),
            max_duration_ms: Some(5000),
            forbidden_patterns: vec!["secret".to_string()],
            ..Default::default()
        };

        // More restrictive child - should satisfy
        let child_ok = DelegationConstraints {
            max_operations: Some(50),
            max_duration_ms: Some(2000),
            forbidden_patterns: vec!["secret".to_string(), "password".to_string()],
            ..Default::default()
        };
        assert!(parent.is_satisfied_by(&child_ok));

        // Less restrictive child - should NOT satisfy
        let child_bad = DelegationConstraints {
            max_operations: Some(200), // More than parent allows
            max_duration_ms: Some(2000),
            forbidden_patterns: vec!["secret".to_string()],
            ..Default::default()
        };
        assert!(!parent.is_satisfied_by(&child_bad));
    }

    #[test]
    fn test_constraints_missing_forbidden_pattern() {
        let parent = DelegationConstraints {
            forbidden_patterns: vec!["secret".to_string()],
            ..Default::default()
        };

        // Child doesn't include parent's forbidden pattern
        let child = DelegationConstraints::default();
        assert!(!parent.is_satisfied_by(&child));
    }

    #[test]
    fn test_effective_constraints() {
        // Use full trust envelope which allows delegation
        let envelope = full_trust_envelope();

        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        let del = DelegationBuilder::new("root", "child")
            .capability(Capability::Read)
            .max_operations(100)
            .allow_delegation(Some(1))
            .forbid_pattern("password")
            .forbid_pattern("secret")
            .sign(&keypair);
        chain.delegate(del).unwrap();

        let constraints = chain.effective_constraints();
        assert_eq!(constraints.max_operations, Some(100)); // More restrictive
        assert!(constraints.forbidden_patterns.contains(&"password".to_string()));
        assert!(constraints.forbidden_patterns.contains(&"secret".to_string()));
    }

    #[test]
    fn test_has_capability() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        assert!(chain.has_capability(Capability::Admin));

        let del = DelegationBuilder::new("root", "child")
            .capability(Capability::Read)
            .sign(&keypair);
        chain.delegate(del).unwrap();

        assert!(chain.has_capability(Capability::Read));
        assert!(!chain.has_capability(Capability::Admin));
    }

    #[test]
    fn test_delegation_serialization() {
        let keypair = make_keypair();
        let delegation = DelegationBuilder::new("parent", "child")
            .capability(Capability::Read)
            .max_operations(100)
            .sign(&keypair);

        let json = serde_json::to_string(&delegation).unwrap();
        let deserialized: Delegation = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.parent_contract, "parent");
        assert_eq!(deserialized.child_contract, "child");
        assert!(deserialized.delegated_capabilities.contains(&Capability::Read));
    }

    #[test]
    fn test_empty_capabilities_delegation() {
        let envelope = full_trust_envelope();
        let mut chain = DelegationChain::from_root(&envelope);
        let keypair = make_keypair();

        // Empty delegation is valid (no capabilities)
        let del = DelegationBuilder::new("root", "child").sign(&keypair);
        assert!(chain.delegate(del).is_ok());

        let caps = chain.effective_capabilities();
        assert!(caps.is_empty());
    }

    #[test]
    fn test_constraint_can_delegate_inheritance() {
        let parent = DelegationConstraints {
            can_delegate: false,
            ..Default::default()
        };

        // Child tries to allow delegation when parent doesn't
        let child = DelegationConstraints {
            can_delegate: true,
            ..Default::default()
        };
        assert!(!parent.is_satisfied_by(&child));

        // Child also disallows - should satisfy
        let child_ok = DelegationConstraints {
            can_delegate: false,
            ..Default::default()
        };
        assert!(parent.is_satisfied_by(&child_ok));
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn arb_capability() -> impl Strategy<Value = Capability> {
        prop_oneof![
            Just(Capability::Read),
            Just(Capability::Write),
            Just(Capability::Execute),
            Just(Capability::Manage),
            Just(Capability::Admin),
        ]
    }

    fn arb_capability_set() -> impl Strategy<Value = HashSet<Capability>> {
        prop::collection::hash_set(arb_capability(), 0..5)
    }

    proptest! {
        #[test]
        fn prop_subset_is_reflexive(caps in arb_capability_set()) {
            // A set is always a subset of itself
            let subset: HashSet<_> = caps.iter().copied().collect();
            prop_assert!(subset.is_subset(&caps));
        }

        #[test]
        fn prop_effective_caps_subset_of_root(
            root_caps in arb_capability_set(),
            del_caps in arb_capability_set(),
        ) {
            // Use full_trust to avoid resource constraint issues
            let mut envelope = Envelope::full_trust();
            envelope.capabilities = root_caps.iter().copied().collect();

            let mut chain = DelegationChain::from_root(&envelope);
            let keypair = KeyPair::from_seed(&[42u8; 32]);

            // Filter del_caps to only include root caps (valid delegation)
            let valid_caps: HashSet<_> = del_caps.intersection(&root_caps).copied().collect();

            let del = DelegationBuilder::new("root", "child")
                .capabilities(valid_caps)
                .allow_delegation(Some(1))
                .sign(&keypair);
            chain.delegate(del).unwrap();

            let effective = chain.effective_capabilities();
            // Effective capabilities must be subset of root
            prop_assert!(effective.is_subset(&root_caps));
        }

        #[test]
        fn prop_chain_depth_matches_delegations(depth in 0usize..5) {
            let envelope = Envelope::full_trust();
            let mut chain = DelegationChain::from_root(&envelope);
            let keypair = KeyPair::from_seed(&[42u8; 32]);

            for i in 0..depth {
                let parent = if i == 0 { "root".to_string() } else { format!("child{}", i - 1) };
                let child = format!("child{}", i);

                // is_satisfied_by requires child max_depth < parent max_depth
                // Start high (10) and decrement for each level
                let remaining_depth = (10 - i) as u32;
                let del = DelegationBuilder::new(&parent, &child)
                    .capability(Capability::Read)
                    .allow_delegation(Some(remaining_depth))
                    .sign(&keypair);
                chain.delegate(del).unwrap();
            }

            prop_assert_eq!(chain.depth(), depth);
        }

        #[test]
        fn prop_constraints_satisfaction_transitive(
            max_ops_parent in 100u64..1000,
            max_ops_child in 50u64..100,
        ) {
            let parent = DelegationConstraints {
                max_operations: Some(max_ops_parent),
                ..Default::default()
            };

            let child = DelegationConstraints {
                max_operations: Some(max_ops_child),
                ..Default::default()
            };

            // Child with lower max_ops should satisfy parent
            prop_assert!(parent.is_satisfied_by(&child));
        }
    }
}
