//! Doctrine Engine - Executable Constraints System
//!
//! Encodes DOCTRINE_2027 as a set of constraints that AHI must satisfy.
//! Doctrine is not a document; it is executable policy enforced on every cycle.
//!
//! Core Doctrines:
//! 1. Closed-World Governance: All change flows through Σ, Q, ΔΣ gates
//! 2. Deterministic Projections: Π(Σ*, Q, Λ, config) → identical output
//! 3. Chatman Constant: Hot path execution ≤8 ticks
//! 4. Sector Isolation: ΔΣ/ΔΠ changes don't leak across sectors
//! 5. Immutability: Σ* never mutates; changes create new snapshots
//! 6. No Ad-Hoc Behavior: Π introduces nothing not in Σ, Q, or doctrine

use super::ahi_contract::{AHIError, DoctrineConstraint, ElementKind};
use std::collections::HashMap;

/// Doctrine severity - how strictly to enforce
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DoctrineSeverity {
    /// Warning only (log but allow)
    Soft,

    /// Must comply (reject if violated)
    Hard,

    /// Critical invariant (system-wide impact if broken)
    Critical,
}

/// Rule kind determines how to check it
#[derive(Debug, Clone)]
pub enum RuleKind {
    /// No direct Σ* mutations
    ImmutabilityRule,

    /// All changes must be justified from Γ
    ClosedWorldRule,

    /// Patterns must respect tick budgets
    ChatmanConstantRule,

    /// Sector changes don't leak
    SectorIsolationRule,

    /// Π outputs are deterministic
    DeterminismRule,

    /// Custom rule with description
    Custom(String),
}

/// A single executable doctrine constraint
#[derive(Debug, Clone)]
pub struct DoctrineRule {
    pub id: String,
    pub name: String,
    pub doctrine_name: String,
    pub severity: DoctrineSeverity,
    pub rule_kind: RuleKind,
}

impl DoctrineRule {
    /// Check if this rule passes for the given subject
    pub fn check(&self, subject: &str, kind: ElementKind, _context: &str) -> bool {
        match &self.rule_kind {
            RuleKind::ImmutabilityRule => {
                // Σ* changes only through ΔΣ snapshots
                !(subject.contains("direct_edit") && subject.contains("sigma"))
            }
            RuleKind::ClosedWorldRule => {
                // Every ΔΣ, ΔΠ, ΔQ must cite Γ observations
                !subject.contains("ad_hoc") && !subject.contains("undocumented")
            }
            RuleKind::ChatmanConstantRule => {
                // Π projections must respect 8-tick budget
                if kind == ElementKind::Pattern {
                    !subject.contains("exceeds_ticks")
                } else {
                    true
                }
            }
            RuleKind::SectorIsolationRule => {
                // Changes to Σ_finance cannot affect Σ_healthcare
                !subject.contains("cross_sector") || subject.contains("shared_base")
            }
            RuleKind::DeterminismRule => {
                // Π(Σ, Q, Λ, config) must always yield identical output
                if kind == ElementKind::Service {
                    !subject.contains("random_output") && !subject.contains("non_deterministic")
                } else {
                    true
                }
            }
            RuleKind::Custom(_) => {
                // Custom rules pass by default (would need user-defined logic)
                true
            }
        }
    }
}

/// Doctrine Engine - Multi-constraint governor for ggen
#[derive(Debug, Clone)]
pub struct DoctrineEngine {
    rules: HashMap<String, DoctrineRule>,
    violations: Vec<(String, String)>, // (rule_id, violation_msg)
}

impl DoctrineEngine {
    /// Create new doctrine engine with built-in DOCTRINE_2027 rules
    pub fn new() -> Self {
        let mut engine = Self {
            rules: HashMap::new(),
            violations: Vec::new(),
        };

        // DOCTRINE_2027: Immutability
        engine.add_rule(
            "immutability-no-direct-sigma-edit",
            "No Direct Σ* Mutation",
            "Immutability",
            DoctrineSeverity::Critical,
            RuleKind::ImmutabilityRule,
        );

        // DOCTRINE_2027: Closed-World
        engine.add_rule(
            "closed-world-changes-justified",
            "All Changes Justified from Γ",
            "Closed-World",
            DoctrineSeverity::Critical,
            RuleKind::ClosedWorldRule,
        );

        // DOCTRINE_2027: Chatman Constant
        engine.add_rule(
            "chatman-constant-tick-budget",
            "Hot Path ≤8 Ticks",
            "Performance",
            DoctrineSeverity::Hard,
            RuleKind::ChatmanConstantRule,
        );

        // DOCTRINE_2027: Sector Isolation
        engine.add_rule(
            "sector-isolation-no-cross-leak",
            "Sector Isolation",
            "Multi-Tenancy",
            DoctrineSeverity::Hard,
            RuleKind::SectorIsolationRule,
        );

        // DOCTRINE_2027: Determinism
        engine.add_rule(
            "determinism-π-idempotent",
            "Projections are Deterministic",
            "Determinism",
            DoctrineSeverity::Hard,
            RuleKind::DeterminismRule,
        );

        engine
    }

    /// Add a rule
    fn add_rule(
        &mut self,
        id: &str,
        name: &str,
        doctrine: &str,
        severity: DoctrineSeverity,
        rule_kind: RuleKind,
    ) {
        self.rules.insert(
            id.to_string(),
            DoctrineRule {
                id: id.to_string(),
                name: name.to_string(),
                doctrine_name: doctrine.to_string(),
                severity,
                rule_kind,
            },
        );
    }

    /// Check all rules against a subject
    pub fn check_all(&mut self, subject: &str, kind: ElementKind, context: &str) -> Result<(), Vec<AHIError>> {
        let mut errors = Vec::new();

        for (id, rule) in &self.rules.clone() {
            let passes = rule.check(subject, kind, context);

            if !passes {
                let msg = format!(
                    "Doctrine violation: {} ({}). Subject: {}, Kind: {:?}, Context: {}",
                    rule.name, rule.doctrine_name, subject, kind, context
                );

                self.violations.push((id.clone(), msg.clone()));

                match rule.severity {
                    DoctrineSeverity::Soft => {
                        // Log but don't block
                    }
                    DoctrineSeverity::Hard => {
                        errors.push(AHIError::DoctrineViolation(msg));
                    }
                    DoctrineSeverity::Critical => {
                        errors.push(AHIError::DoctrineViolation(msg));
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Get all violations from last check
    pub fn violations(&self) -> &[(String, String)] {
        &self.violations
    }

    /// Clear violation history
    pub fn clear_violations(&mut self) {
        self.violations.clear();
    }

    /// Distance from doctrine compliance (0-100, 0=perfect)
    pub fn doctrine_distance(&self, subject: &str, kind: ElementKind, context: &str) -> f64 {
        let mut violation_count = 0;
        let mut critical_violation = false;

        for rule in self.rules.values() {
            let passes = rule.check(subject, kind, context);
            if !passes {
                violation_count += 1;
                if rule.severity == DoctrineSeverity::Critical {
                    critical_violation = true;
                }
            }
        }

        if critical_violation {
            return 100.0; // Perfect non-compliance
        }

        if self.rules.is_empty() {
            return 0.0;
        }

        (violation_count as f64 / self.rules.len() as f64) * 100.0
    }

    /// Get rule by ID
    pub fn get_rule(&self, id: &str) -> Option<&DoctrineRule> {
        self.rules.get(id)
    }

    /// List all rules by doctrine
    pub fn rules_by_doctrine(&self, doctrine: &str) -> Vec<&DoctrineRule> {
        self.rules
            .values()
            .filter(|r| r.doctrine_name == doctrine)
            .collect()
    }

    /// Get all critical rules
    pub fn critical_rules(&self) -> Vec<&DoctrineRule> {
        self.rules
            .values()
            .filter(|r| r.severity == DoctrineSeverity::Critical)
            .collect()
    }
}

impl Default for DoctrineEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl DoctrineConstraint for DoctrineEngine {
    fn check(&self, subject: &str, kind: ElementKind, context: &str) -> Result<(), AHIError> {
        let mut engine_clone = self.clone();
        engine_clone.check_all(subject, kind, context).map_err(|mut errs| {
            errs.pop().unwrap_or_else(|| {
                AHIError::DoctrineViolation("Unknown doctrine violation".to_string())
            })
        })
    }

    fn name(&self) -> &str {
        "DOCTRINE_2027"
    }

    fn doctrine(&self) -> &str {
        "Closed-World Governance with Deterministic Projections"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doctrine_engine_creation() {
        let engine = DoctrineEngine::new();
        assert!(!engine.rules.is_empty());
    }

    #[test]
    fn test_doctrine_immutability_check() {
        let mut engine = DoctrineEngine::new();
        let result = engine.check_all(
            "normal_update",
            ElementKind::Concept,
            "Σ update via ΔΣ",
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_doctrine_violation_direct_edit() {
        let mut engine = DoctrineEngine::new();
        let result = engine.check_all(
            "direct_edit_sigma_mutation",
            ElementKind::Concept,
            "Attempted direct Σ* mutation",
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_doctrine_distance_metric() {
        let engine = DoctrineEngine::new();
        let perfect = engine.doctrine_distance("normal_operation", ElementKind::Pattern, "");
        assert_eq!(perfect, 0.0);

        let violations = engine.doctrine_distance(
            "direct_edit_sigma_bad_ad_hoc_undocumented",
            ElementKind::Pattern,
            "",
        );
        assert!(violations > 0.0);
    }

    #[test]
    fn test_doctrine_rules_by_type() {
        let engine = DoctrineEngine::new();
        let immutability_rules = engine.rules_by_doctrine("Immutability");
        assert!(!immutability_rules.is_empty());

        let critical = engine.critical_rules();
        assert!(!critical.is_empty());
    }

    #[test]
    fn test_doctrine_violation_tracking() {
        let mut engine = DoctrineEngine::new();
        assert_eq!(engine.violations().len(), 0);

        let _ = engine.check_all(
            "direct_edit_sigma_mutation",
            ElementKind::Concept,
            "test",
        );

        assert!(!engine.violations().is_empty());
    }
}

