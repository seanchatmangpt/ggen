//! Invariant system (Q): Hard-blocking constraints that cannot be violated
//!
//! Invariants define the safety boundaries of ggen's decision-making.
//! Violations are fatal and prevent promotion/execution.

use crate::error::DoDResult;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

/// Unique identifier for invariants
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct InvariantId(Uuid);

impl InvariantId {
    /// Generate a new invariant ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for InvariantId {
    fn default() -> Self {
        Self::new()
    }
}

/// Severity of invariant violations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum InvariantSeverity {
    Warning,
    Error,
    Critical,
}

impl InvariantSeverity {
    /// Is this severity blocking?
    pub fn is_blocking(&self) -> bool {
        matches!(self, InvariantSeverity::Error | InvariantSeverity::Critical)
    }
}

/// An invariant (Q) that must hold
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invariant {
    /// Unique ID
    id: InvariantId,
    /// Human-readable name
    name: String,
    /// Formal predicate (e.g., "decisions must derive from observations")
    predicate: String,
    /// Severity when violated
    severity: InvariantSeverity,
    /// Category (safety, liveness, determinism, etc)
    category: InvariantCategory,
}

/// Category of invariants
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InvariantCategory {
    /// Safety properties (nothing bad happens)
    Safety,
    /// Liveness properties (good things eventually happen)
    Liveness,
    /// Determinism properties (same input always produces same output)
    Determinism,
    /// Idempotence properties (operations can be safely repeated)
    Idempotence,
    /// Isolation properties (tenants don't interfere)
    Isolation,
    /// Causality properties (decisions respect ordering)
    Causality,
    /// Performance properties (timing guarantees)
    Performance,
    /// Governance properties (doctrine compliance)
    Governance,
}

impl Invariant {
    /// Create a new invariant
    pub fn new(
        name: impl Into<String>, predicate: impl Into<String>, severity: InvariantSeverity,
        category: InvariantCategory,
    ) -> Self {
        Self {
            id: InvariantId::new(),
            name: name.into(),
            predicate: predicate.into(),
            severity,
            category,
        }
    }

    /// Get invariant ID
    pub fn id(&self) -> InvariantId {
        self.id
    }

    /// Get name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get predicate
    pub fn predicate(&self) -> &str {
        &self.predicate
    }

    /// Get severity
    pub fn severity(&self) -> InvariantSeverity {
        self.severity
    }

    /// Get category
    pub fn category(&self) -> InvariantCategory {
        self.category
    }

    /// Is this invariant blocking?
    pub fn is_blocking(&self) -> bool {
        self.severity.is_blocking()
    }
}

/// Violation of an invariant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvariantViolation {
    /// Which invariant was violated
    invariant_id: InvariantId,
    /// Invariant name
    invariant_name: String,
    /// Severity of violation
    severity: InvariantSeverity,
    /// Explanation of what went wrong
    explanation: String,
    /// Context where violation occurred
    context: BTreeMap<String, String>,
}

impl InvariantViolation {
    /// Create a new violation
    pub fn new(
        invariant_id: InvariantId, invariant_name: impl Into<String>, severity: InvariantSeverity,
        explanation: impl Into<String>,
    ) -> Self {
        Self {
            invariant_id,
            invariant_name: invariant_name.into(),
            severity,
            explanation: explanation.into(),
            context: BTreeMap::new(),
        }
    }

    /// Add context to the violation
    pub fn with_context(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.context.insert(key.into(), value.into());
        self
    }

    /// Get severity
    pub fn severity(&self) -> InvariantSeverity {
        self.severity
    }

    /// Is this violation blocking?
    pub fn is_blocking(&self) -> bool {
        self.severity.is_blocking()
    }

    /// Get explanation
    pub fn explanation(&self) -> &str {
        &self.explanation
    }
}

/// Checker for invariants
pub struct InvariantChecker {
    invariants: BTreeMap<InvariantId, Invariant>,
}

impl InvariantChecker {
    /// Create a new checker
    pub fn new() -> Self {
        Self {
            invariants: BTreeMap::new(),
        }
    }

    /// Register an invariant
    pub fn register(mut self, invariant: Invariant) -> Self {
        self.invariants.insert(invariant.id(), invariant);
        self
    }

    /// Register many invariants
    pub fn register_many(mut self, invariants: Vec<Invariant>) -> Self {
        for invariant in invariants {
            self.invariants.insert(invariant.id(), invariant);
        }
        self
    }

    /// Get all invariants
    pub fn all(&self) -> Vec<&Invariant> {
        self.invariants.values().collect()
    }

    /// Get all blocking invariants
    pub fn blocking(&self) -> Vec<&Invariant> {
        self.invariants
            .values()
            .filter(|inv| inv.is_blocking())
            .collect()
    }

    /// Get invariants by category
    pub fn by_category(&self, category: InvariantCategory) -> Vec<&Invariant> {
        self.invariants
            .values()
            .filter(|inv| inv.category() == category)
            .collect()
    }

    /// Get an invariant by ID
    pub fn get(&self, id: InvariantId) -> Option<&Invariant> {
        self.invariants.get(&id)
    }

    /// Check for violations
    pub fn check_violations(&self, violations: &[InvariantViolation]) -> DoDResult<()> {
        let blocking_violations: Vec<_> = violations.iter().filter(|v| v.is_blocking()).collect();

        if !blocking_violations.is_empty() {
            let explanation = blocking_violations
                .iter()
                .map(|v| format!("{}: {}", v.invariant_name, v.explanation))
                .collect::<Vec<_>>()
                .join("; ");

            return Err(crate::error::DoDError::InvariantViolation(explanation));
        }

        Ok(())
    }
}

impl Default for InvariantChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invariant_creation() {
        let inv = Invariant::new(
            "determinism",
            "μ(O) must be deterministic",
            InvariantSeverity::Critical,
            InvariantCategory::Determinism,
        );

        assert_eq!(inv.name(), "determinism");
        assert!(inv.is_blocking());
    }

    #[test]
    fn test_invariant_checker() {
        let inv1 = Invariant::new(
            "safety",
            "no unsafe code",
            InvariantSeverity::Critical,
            InvariantCategory::Safety,
        );
        let inv2 = Invariant::new(
            "performance",
            "τ ≤ 8ms",
            InvariantSeverity::Warning,
            InvariantCategory::Performance,
        );

        let checker = InvariantChecker::new().register(inv1).register(inv2);

        assert_eq!(checker.all().len(), 2);
        assert_eq!(checker.blocking().len(), 1);
    }

    #[test]
    fn test_violation_blocking() {
        let violation = InvariantViolation::new(
            InvariantId::new(),
            "test",
            InvariantSeverity::Critical,
            "test violation",
        );

        assert!(violation.is_blocking());
    }

    #[test]
    fn test_violation_checking() {
        let checker = InvariantChecker::new();

        let blocking = InvariantViolation::new(
            InvariantId::new(),
            "test",
            InvariantSeverity::Critical,
            "blocking",
        );

        let result = checker.check_violations(&[blocking]);
        assert!(result.is_err());
    }
}
