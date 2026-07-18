//! Envelope Enforcement Module
//!
//! Provides bounded operation enforcement with deterministic refusals.
//! Envelopes define the operational bounds for sealed contracts.

use crate::refusal::{Refusal, RefusalCode};
use crate::{McpError, McpResult};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Envelope state machine states
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EnvelopeState {
    Initializing,
    Active,
    Exhausted,
    Violated,
    Terminated,
}

/// Resource usage tracking
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ResourceUsage {
    pub elapsed_ms: u64,
    pub memory_bytes: u64,
    pub operation_count: u64,
}

/// Envelope bounds configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvelopeBound {
    pub time_limit_ms: u64,
    pub memory_limit_bytes: u64,
    pub operation_limit: u64,
    pub scopes: HashSet<String>,
    pub capabilities: HashSet<String>,
}

impl Default for EnvelopeBound {
    fn default() -> Self {
        Self {
            time_limit_ms: 5000,
            memory_limit_bytes: 100 * 1024 * 1024, // 100MB
            operation_limit: 1000,
            scopes: HashSet::new(),
            capabilities: HashSet::new(),
        }
    }
}

/// Envelope for bounded contract execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Envelope {
    pub id: String,
    pub contract_id: String,
    pub state: EnvelopeState,
    pub bounds: EnvelopeBound,
    pub usage: ResourceUsage,
    pub created_at: chrono::DateTime<chrono::Utc>,
}

impl Envelope {
    /// Create a new envelope
    pub fn new(id: String, contract_id: String, bounds: EnvelopeBound) -> Self {
        Self {
            id,
            contract_id,
            state: EnvelopeState::Initializing,
            bounds,
            usage: ResourceUsage::default(),
            created_at: chrono::Utc::now(),
        }
    }

    /// Activate the envelope
    pub fn activate(&mut self) -> McpResult<()> {
        if self.state != EnvelopeState::Initializing {
            return Err(McpError::EnvelopeViolation(
                "Can only activate from Initializing state".to_string(),
            ));
        }
        self.state = EnvelopeState::Active;
        Ok(())
    }

    /// Check if operation is within bounds
    pub fn check_operation(
        &self,
        scope: Option<&str>,
        capability: Option<&str>,
    ) -> Result<(), EnvelopeViolation> {
        // Check state
        if self.state != EnvelopeState::Active {
            return Err(EnvelopeViolation::InvalidState(self.state));
        }

        // Check time
        if self.usage.elapsed_ms >= self.bounds.time_limit_ms {
            return Err(EnvelopeViolation::TimeExceeded {
                limit: self.bounds.time_limit_ms,
                used: self.usage.elapsed_ms,
            });
        }

        // Check memory
        if self.usage.memory_bytes >= self.bounds.memory_limit_bytes {
            return Err(EnvelopeViolation::MemoryExceeded {
                limit: self.bounds.memory_limit_bytes,
                used: self.usage.memory_bytes,
            });
        }

        // Check operations
        if self.usage.operation_count >= self.bounds.operation_limit {
            return Err(EnvelopeViolation::OperationLimitExceeded {
                limit: self.bounds.operation_limit,
                count: self.usage.operation_count,
            });
        }

        // Check scope if provided
        if let Some(s) = scope {
            if !self.bounds.scopes.is_empty() && !self.bounds.scopes.contains(s) {
                return Err(EnvelopeViolation::ScopeViolation {
                    requested: s.to_string(),
                    allowed: self.bounds.scopes.iter().cloned().collect(),
                });
            }
        }

        // Check capability if provided
        if let Some(c) = capability {
            if !self.bounds.capabilities.is_empty() && !self.bounds.capabilities.contains(c) {
                return Err(EnvelopeViolation::CapabilityMissing {
                    required: c.to_string(),
                    available: self.bounds.capabilities.iter().cloned().collect(),
                });
            }
        }

        Ok(())
    }

    /// Record resource usage
    pub fn record_usage(&mut self, elapsed_ms: u64, memory_bytes: u64) {
        self.usage.elapsed_ms += elapsed_ms;
        self.usage.memory_bytes = memory_bytes; // Peak memory
        self.usage.operation_count += 1;
    }

    /// Mark envelope as exhausted (graceful completion)
    pub fn exhaust(&mut self) {
        self.state = EnvelopeState::Exhausted;
    }

    /// Mark envelope as violated (error state)
    pub fn violate(&mut self) {
        self.state = EnvelopeState::Violated;
    }

    /// Terminate the envelope
    pub fn terminate(&mut self) {
        self.state = EnvelopeState::Terminated;
    }
}

/// Envelope violation types
#[derive(Debug, Clone)]
pub enum EnvelopeViolation {
    InvalidState(EnvelopeState),
    TimeExceeded { limit: u64, used: u64 },
    MemoryExceeded { limit: u64, used: u64 },
    OperationLimitExceeded { limit: u64, count: u64 },
    ScopeViolation { requested: String, allowed: Vec<String> },
    CapabilityMissing { required: String, available: Vec<String> },
}

impl EnvelopeViolation {
    /// Convert to deterministic refusal
    pub fn to_refusal(&self, envelope_id: &str) -> Refusal {
        match self {
            EnvelopeViolation::TimeExceeded { limit, used } => {
                Refusal::new(
                    RefusalCode::time_exceeded(),
                    format!("Time limit {}ms exceeded (used {}ms)", limit, used),
                    envelope_id.to_string(),
                )
                .with_context("limit_ms", &limit.to_string())
                .with_context("used_ms", &used.to_string())
            }
            EnvelopeViolation::MemoryExceeded { limit, used } => {
                Refusal::new(
                    RefusalCode::memory_exceeded(),
                    format!("Memory limit {} bytes exceeded (used {})", limit, used),
                    envelope_id.to_string(),
                )
                .with_context("limit_bytes", &limit.to_string())
                .with_context("used_bytes", &used.to_string())
            }
            EnvelopeViolation::OperationLimitExceeded { limit, count } => {
                Refusal::new(
                    RefusalCode::operation_limit(),
                    format!("Operation limit {} exceeded (count {})", limit, count),
                    envelope_id.to_string(),
                )
                .with_context("limit", &limit.to_string())
                .with_context("count", &count.to_string())
            }
            EnvelopeViolation::ScopeViolation { requested, allowed } => {
                Refusal::new(
                    RefusalCode::scope_violation(),
                    format!("Scope '{}' not in allowed scopes", requested),
                    envelope_id.to_string(),
                )
                .with_context("requested", requested)
                .with_context("allowed", &allowed.join(","))
            }
            EnvelopeViolation::CapabilityMissing { required, available } => {
                Refusal::new(
                    RefusalCode::capability_missing(),
                    format!("Capability '{}' not available", required),
                    envelope_id.to_string(),
                )
                .with_context("required", required)
                .with_context("available", &available.join(","))
            }
            EnvelopeViolation::InvalidState(state) => {
                Refusal::new(
                    RefusalCode::scope_violation(),
                    format!("Invalid envelope state: {:?}", state),
                    envelope_id.to_string(),
                )
            }
        }
    }
}

/// Builder for creating envelopes
pub struct EnvelopeBuilder {
    contract_id: String,
    time_limit_ms: u64,
    memory_limit_bytes: u64,
    operation_limit: u64,
    scopes: HashSet<String>,
    capabilities: HashSet<String>,
}

impl EnvelopeBuilder {
    pub fn new(contract_id: String) -> Self {
        Self {
            contract_id,
            time_limit_ms: 5000,
            memory_limit_bytes: 100 * 1024 * 1024,
            operation_limit: 1000,
            scopes: HashSet::new(),
            capabilities: HashSet::new(),
        }
    }

    pub fn with_time_bound(mut self, ms: u64) -> Self {
        self.time_limit_ms = ms;
        self
    }

    pub fn with_memory_bound(mut self, bytes: u64) -> Self {
        self.memory_limit_bytes = bytes;
        self
    }

    pub fn with_operation_limit(mut self, limit: u64) -> Self {
        self.operation_limit = limit;
        self
    }

    pub fn with_scopes(mut self, scopes: Vec<String>) -> Self {
        self.scopes = scopes.into_iter().collect();
        self
    }

    pub fn with_capabilities(mut self, capabilities: Vec<String>) -> Self {
        self.capabilities = capabilities.into_iter().collect();
        self
    }

    pub fn build(self) -> Envelope {
        let id = format!("env-{}", uuid::Uuid::new_v4());
        let bounds = EnvelopeBound {
            time_limit_ms: self.time_limit_ms,
            memory_limit_bytes: self.memory_limit_bytes,
            operation_limit: self.operation_limit,
            scopes: self.scopes,
            capabilities: self.capabilities,
        };
        Envelope::new(id, self.contract_id, bounds)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_envelope_creation() {
        let envelope = EnvelopeBuilder::new("contract-123".to_string())
            .with_time_bound(10000)
            .with_memory_bound(50 * 1024 * 1024)
            .build();

        assert_eq!(envelope.state, EnvelopeState::Initializing);
        assert_eq!(envelope.bounds.time_limit_ms, 10000);
    }

    #[test]
    fn test_envelope_activation() {
        let mut envelope = EnvelopeBuilder::new("contract-123".to_string()).build();

        envelope.activate().unwrap();
        assert_eq!(envelope.state, EnvelopeState::Active);
    }

    #[test]
    fn test_envelope_bounds_check() {
        let mut envelope = EnvelopeBuilder::new("contract-123".to_string())
            .with_scopes(vec!["read".to_string(), "write".to_string()])
            .build();

        envelope.activate().unwrap();

        // Valid scope
        assert!(envelope.check_operation(Some("read"), None).is_ok());

        // Invalid scope
        assert!(envelope.check_operation(Some("delete"), None).is_err());
    }

    #[test]
    fn test_envelope_time_violation() {
        let mut envelope = EnvelopeBuilder::new("contract-123".to_string())
            .with_time_bound(100)
            .build();

        envelope.activate().unwrap();
        envelope.usage.elapsed_ms = 150;

        let result = envelope.check_operation(None, None);
        assert!(result.is_err());

        if let Err(EnvelopeViolation::TimeExceeded { limit, used }) = result {
            assert_eq!(limit, 100);
            assert_eq!(used, 150);
        } else {
            panic!("Expected TimeExceeded violation");
        }
    }

    #[test]
    fn test_violation_to_refusal() {
        let violation = EnvelopeViolation::TimeExceeded { limit: 5000, used: 6000 };
        let refusal = violation.to_refusal("env-test");

        assert_eq!(refusal.code.as_str(), "ENV-0001");
        assert!(refusal.message.contains("5000ms"));
        assert!(refusal.context.contains_key("limit_ms"));
    }

    #[test]
    fn test_envelope_state_transitions() {
        let mut envelope = EnvelopeBuilder::new("contract-123".to_string()).build();

        assert_eq!(envelope.state, EnvelopeState::Initializing);

        envelope.activate().unwrap();
        assert_eq!(envelope.state, EnvelopeState::Active);

        envelope.exhaust();
        assert_eq!(envelope.state, EnvelopeState::Exhausted);
    }
}
