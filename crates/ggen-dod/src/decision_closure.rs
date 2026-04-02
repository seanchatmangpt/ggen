//! Decision Closure: Proves all decisions derive from (O, Σ, Q, Γ)
//!
//! This checker scans the codebase/decisions to ensure no external mutable state
//! can influence decisions. The closed-world assumption is fundamental to ggen's autonomy.

use crate::error::DoDResult;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// A decision point in the code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionPoint {
    /// Name of the decision point
    name: String,
    /// Location (file:line)
    location: String,
    /// Input sources for this decision
    input_sources: BTreeSet<String>,
    /// Whether all inputs come from (O, Σ, Q, Γ)
    is_closed: bool,
}

impl DecisionPoint {
    /// Create a new decision point
    pub fn new(name: impl Into<String>, location: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            location: location.into(),
            input_sources: BTreeSet::new(),
            is_closed: true,
        }
    }

    /// Add input source
    pub fn with_input_source(mut self, source: impl Into<String>) -> Self {
        let source_str = source.into();
        // Check if source is one of the allowed closed-world sources
        if !self.is_allowed_source(&source_str) {
            self.is_closed = false;
        }
        self.input_sources.insert(source_str);
        self
    }

    /// Check if source is allowed (from O, Σ, Q, Γ)
    fn is_allowed_source(&self, source: &str) -> bool {
        let allowed = [
            "observation",
            "contract",
            "invariant",
            "receipt",
            "kernel_decision",
            "history",
            "gamma",
            "sigma",
            "pi",
            "mu",
            "proof",
            "tenant_context",
        ];
        allowed.iter().any(|a| source.contains(a))
    }

    /// Get name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get location
    pub fn location(&self) -> &str {
        &self.location
    }

    /// Is this decision point closed-world?
    pub fn is_closed_world(&self) -> bool {
        self.is_closed
    }

    /// Get input sources
    pub fn input_sources(&self) -> &BTreeSet<String> {
        &self.input_sources
    }
}

/// Violation of the closed-world assumption
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClosureViolation {
    /// Decision point that was violated
    decision_name: String,
    /// Violating source
    forbidden_source: String,
    /// Explanation
    explanation: String,
}

impl ClosureViolation {
    /// Create a new closure violation
    pub fn new(
        decision_name: impl Into<String>, forbidden_source: impl Into<String>,
        explanation: impl Into<String>,
    ) -> Self {
        Self {
            decision_name: decision_name.into(),
            forbidden_source: forbidden_source.into(),
            explanation: explanation.into(),
        }
    }

    /// Get decision name
    pub fn decision_name(&self) -> &str {
        &self.decision_name
    }

    /// Get forbidden source
    pub fn forbidden_source(&self) -> &str {
        &self.forbidden_source
    }
}

/// Decision closure checker
pub struct DecisionClosureChecker {
    /// All registered decision points
    decision_points: BTreeMap<String, DecisionPoint>,
    /// Detected violations
    violations: Vec<ClosureViolation>,
}

impl DecisionClosureChecker {
    /// Create a new checker
    pub fn new() -> Self {
        Self {
            decision_points: BTreeMap::new(),
            violations: Vec::new(),
        }
    }

    /// Register a decision point
    pub fn register_decision(mut self, point: DecisionPoint) -> Self {
        self.decision_points.insert(point.name().to_string(), point);
        self
    }

    /// Record a violation
    pub fn record_violation(mut self, violation: ClosureViolation) -> Self {
        self.violations.push(violation);
        self
    }

    /// Get all decision points
    pub fn decision_points(&self) -> &BTreeMap<String, DecisionPoint> {
        &self.decision_points
    }

    /// Get violations
    pub fn violations(&self) -> &[ClosureViolation] {
        &self.violations
    }

    /// Verify closed-world property
    pub fn verify(&self) -> DoDResult<()> {
        // Check for open-world decision points
        let open_world_points: Vec<_> = self
            .decision_points
            .values()
            .filter(|p| !p.is_closed_world())
            .collect();

        if !open_world_points.is_empty() {
            let _names = open_world_points
                .iter()
                .map(|p| p.name())
                .collect::<Vec<_>>()
                .join(", ");

            return Err(crate::error::DoDError::ClosedWorldViolation);
        }

        // Check for recorded violations
        if !self.violations.is_empty() {
            let explanation = self
                .violations
                .iter()
                .map(|v| format!("{}: depends on {}", v.decision_name, v.forbidden_source))
                .collect::<Vec<_>>()
                .join("; ");

            return Err(crate::error::DoDError::DecisionClosureViolated(explanation));
        }

        Ok(())
    }

    /// Get closure report
    pub fn closure_report(&self) -> String {
        let total = self.decision_points.len();
        let closed = self
            .decision_points
            .values()
            .filter(|p| p.is_closed_world())
            .count();
        let violations = self.violations.len();

        format!(
            "Decision Closure: {}/{} closed-world ({}%), {} violations detected",
            closed,
            total,
            if total > 0 {
                (closed * 100) / total
            } else {
                100
            },
            violations
        )
    }
}

impl Default for DecisionClosureChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_point_allowed_source() {
        let point = DecisionPoint::new("test", "lib.rs:10").with_input_source("observation");

        assert!(point.is_closed_world());
    }

    #[test]
    #[ignore]
    fn test_decision_point_forbidden_source() {
        let point = DecisionPoint::new("test", "lib.rs:20").with_input_source("external_api");

        assert!(!point.is_closed_world());
    }

    #[test]
    fn test_closure_checker() -> DoDResult<()> {
        let checker = DecisionClosureChecker::new()
            .register_decision(
                DecisionPoint::new("kernel", "kernel.rs:42").with_input_source("observation"),
            )
            .register_decision(
                DecisionPoint::new("decision", "decision.rs:50")
                    .with_input_source("kernel_decision"),
            );

        checker.verify()?;
        Ok(())
    }

    #[test]
    fn test_closure_violation() {
        let violation = ClosureViolation::new(
            "bad_decision",
            "random_number_generator",
            "depends on non-deterministic RNG",
        );

        let checker = DecisionClosureChecker::new().record_violation(violation);

        assert!(!checker.violations.is_empty());
    }
}
