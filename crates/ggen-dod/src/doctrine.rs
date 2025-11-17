//! Doctrine_2027 compliance: Automated governance and evolution constraints
//!
//! Doctrine defines the covenants/principles that ggen must respect.
//! Violations prevent release and are fatal.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// A doctrine principle/covenant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoctrinePrinciple {
    /// Name of the principle
    name: String,
    /// Description
    description: String,
    /// Formal check/predicate
    predicate: String,
    /// Whether violation blocks release
    blocks_release: bool,
}

impl DoctrinePrinciple {
    /// Create a new doctrine principle
    pub fn new(
        name: impl Into<String>, description: impl Into<String>, predicate: impl Into<String>,
        blocks_release: bool,
    ) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            predicate: predicate.into(),
            blocks_release,
        }
    }

    /// Get principle name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Does violation block release?
    pub fn blocks_release(&self) -> bool {
        self.blocks_release
    }
}

/// Doctrine_2027 compliance checker
pub struct DoctrineCompliance {
    /// All registered principles
    principles: BTreeMap<String, DoctrinePrinciple>,
    /// Violation records
    violations: Vec<DoctrineViolation>,
}

/// A doctrine violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoctrineViolation {
    /// Which principle was violated
    principle_name: String,
    /// Explanation
    explanation: String,
    /// Is this blocking?
    blocking: bool,
}

impl DoctrineCompliance {
    /// Create new doctrine compliance checker
    pub fn new() -> Self {
        Self {
            principles: BTreeMap::new(),
            violations: Vec::new(),
        }
    }

    /// Register a principle
    pub fn register_principle(mut self, principle: DoctrinePrinciple) -> Self {
        self.principles
            .insert(principle.name().to_string(), principle);
        self
    }

    /// Record a violation
    pub fn record_violation(
        &mut self, principle_name: impl Into<String>, explanation: impl Into<String>,
    ) -> crate::error::DoDResult<()> {
        let principle_name = principle_name.into();
        let explanation = explanation.into();

        let principle = self.principles.get(&principle_name).ok_or_else(|| {
            crate::error::DoDError::DoctrineViolation(format!(
                "unknown principle: {}",
                principle_name
            ))
        })?;

        self.violations.push(DoctrineViolation {
            principle_name,
            explanation,
            blocking: principle.blocks_release(),
        });

        Ok(())
    }

    /// Check compliance
    pub fn check(&self) -> crate::error::DoDResult<()> {
        let blocking_violations: Vec<_> = self.violations.iter().filter(|v| v.blocking).collect();

        if !blocking_violations.is_empty() {
            let explanation = blocking_violations
                .iter()
                .map(|v| format!("{}: {}", v.principle_name, v.explanation))
                .collect::<Vec<_>>()
                .join("; ");

            return Err(crate::error::DoDError::DoctrineViolation(explanation));
        }

        Ok(())
    }

    /// Get all principles
    pub fn principles(&self) -> Vec<&DoctrinePrinciple> {
        self.principles.values().collect()
    }

    /// Get violations
    pub fn violations(&self) -> &[DoctrineViolation] {
        &self.violations
    }

    /// Clear violations (typically for next compliance check)
    pub fn clear_violations(&mut self) {
        self.violations.clear();
    }

    /// Is compliant?
    pub fn is_compliant(&self) -> bool {
        self.violations.iter().all(|v| !v.blocking)
    }
}

impl Default for DoctrineCompliance {
    fn default() -> Self {
        Self::new()
    }
}

/// Create the complete Doctrine_2027 set
pub fn doctrine_2027() -> DoctrineCompliance {
    DoctrineCompliance::new()
        .register_principle(DoctrinePrinciple::new(
            "determinism",
            "All decisions must be deterministic",
            "μ(O) always produces same A for same O, Σ*, Q, Γ",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "idempotence",
            "Idempotent operations must be safe to repeat",
            "μ ∘ μ = μ for operations marked idempotent",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "closed_world",
            "All decisions must derive from O, Σ, Q, Γ",
            "No external mutable state affects decisions",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "no_human_arbitration",
            "Critical path decisions must not require human approval",
            "All proof-carrying checks automated",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "provenance",
            "Every action must have a signed receipt",
            "hash(A) = hash(μ(O)) provably",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "timing_guarantees",
            "τ ≤ 8ms must be enforced where declared",
            "Timing constraints checked at build and runtime",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "tenant_isolation",
            "One tenant cannot affect another",
            "Hard logical separation at all levels",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "no_unsafe_code",
            "No unsafe Rust allowed in critical paths",
            "Enforced by #![deny(unsafe_code)]",
            true,
        ))
        .register_principle(DoctrinePrinciple::new(
            "backwards_compatibility",
            "Schema changes must be backward-compatible",
            "ΔΣ must preserve Σ*-compatible observations",
            false,
        ))
        .register_principle(DoctrinePrinciple::new(
            "observable_governance",
            "All governance decisions must be in Γ",
            "Audit trail is complete and queryable",
            true,
        ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doctrine_2027() {
        let doctrine = doctrine_2027();
        assert!(doctrine.principles().len() >= 10);
    }

    #[test]
    fn test_doctrine_compliance() -> crate::error::DoDResult<()> {
        let mut compliance = doctrine_2027();
        compliance.check()?;
        Ok(())
    }

    #[test]
    fn test_doctrine_violation() {
        let mut compliance = doctrine_2027();
        let _ = compliance.record_violation("determinism", "test violation");

        let is_compliant = compliance.is_compliant();
        assert!(!is_compliant);
    }
}
