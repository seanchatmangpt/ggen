//! Proactive failure prevention for semantic code generation pipelines
//!
//! Implements prevention mechanisms based on FMEA analysis:
//! - Pre-generation validation
//! - Real-time risk assessment
//! - Automated mitigation application
//! - Rollback and recovery

use super::query_analyzer::{QueryPathAnalyzer, QueryRisk};
use super::schema_analyzer::{SchemaChange, SchemaChangeAnalyzer};
use super::template_analyzer::{TemplateRisk, TemplateRiskAnalyzer};
use super::types::{FailureMode, RiskPriorityNumber};
use crate::graph::Graph;
use crate::template::Template;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Failure prevention engine
///
/// Orchestrates proactive prevention mechanisms based on FMEA analysis
pub struct FailurePreventionEngine {
    schema_analyzer: SchemaChangeAnalyzer,
    query_analyzer: QueryPathAnalyzer,
    template_analyzer: TemplateRiskAnalyzer,
    rules: Vec<PreventionRule>,
    history: PreventionHistory,
}

impl FailurePreventionEngine {
    /// Create a new failure prevention engine
    pub fn new() -> Self {
        Self {
            schema_analyzer: SchemaChangeAnalyzer::new(),
            query_analyzer: QueryPathAnalyzer::new(),
            template_analyzer: TemplateRiskAnalyzer::new(),
            rules: Self::default_rules(),
            history: PreventionHistory::new(),
        }
    }

    /// Add a custom prevention rule
    pub fn add_rule(&mut self, rule: PreventionRule) {
        self.rules.push(rule);
    }

    /// Validate schema changes before applying
    pub fn validate_schema_changes(
        &mut self,
        old_schema: &Graph,
        new_schema: &Graph,
    ) -> Result<PreventionReport> {
        let changes = self
            .schema_analyzer
            .analyze_changes(old_schema, new_schema)
            .context("Failed to analyze schema changes")?;

        let failure_modes = self.schema_analyzer.changes_to_failure_modes(&changes);

        let mut report = PreventionReport::new("Schema Change Validation");

        for fm in &failure_modes {
            if fm.rpn.requires_immediate_action() {
                report.add_blocker(format!(
                    "High-risk schema change detected: {} (RPN: {})",
                    fm.description,
                    fm.rpn.value()
                ));
            } else if fm.rpn.value() > 100 {
                report.add_warning(format!(
                    "Medium-risk schema change: {} (RPN: {})",
                    fm.description,
                    fm.rpn.value()
                ));
            }
        }

        // Apply prevention rules
        for rule in &self.rules {
            if let Some(action) = rule.evaluate_schema_changes(&changes) {
                match action {
                    PreventionAction::Block(reason) => report.add_blocker(reason),
                    PreventionAction::Warn(reason) => report.add_warning(reason),
                    PreventionAction::Allow => {}
                }
            }
        }

        self.history.record_schema_validation(&report);

        Ok(report)
    }

    /// Validate SPARQL query before execution
    pub fn validate_query(&mut self, query: &str, graph: &Graph) -> Result<PreventionReport> {
        let risk = self
            .query_analyzer
            .analyze_query(query, graph)
            .context("Failed to analyze query")?;

        let mut report = PreventionReport::new("Query Validation");

        if risk.rpn.requires_immediate_action() {
            report.add_blocker(format!(
                "High-risk query detected (RPN: {}): {}",
                risk.rpn.value(),
                risk.risks.join(", ")
            ));
        } else if risk.rpn.value() > 100 {
            report.add_warning(format!(
                "Medium-risk query (RPN: {}): {}",
                risk.rpn.value(),
                risk.risks.join(", ")
            ));
        }

        // Check query complexity
        if risk.complexity_score > 0.8 {
            report.add_warning(format!(
                "Query complexity is very high ({:.2}). Consider optimization.",
                risk.complexity_score
            ));
        }

        // Apply prevention rules
        for rule in &self.rules {
            if let Some(action) = rule.evaluate_query(&risk) {
                match action {
                    PreventionAction::Block(reason) => report.add_blocker(reason),
                    PreventionAction::Warn(reason) => report.add_warning(reason),
                    PreventionAction::Allow => {}
                }
            }
        }

        self.history.record_query_validation(&report);

        Ok(report)
    }

    /// Validate template before rendering
    pub fn validate_template(&mut self, template: &Template) -> Result<PreventionReport> {
        let risk = self
            .template_analyzer
            .analyze_template(template)
            .context("Failed to analyze template")?;

        let mut report = PreventionReport::new("Template Validation");

        if risk.rpn.requires_immediate_action() {
            report.add_blocker(format!(
                "High-risk template detected (RPN: {})",
                risk.rpn.value()
            ));
        } else if risk.rpn.value() > 100 {
            report.add_warning(format!(
                "Medium-risk template (RPN: {})",
                risk.rpn.value()
            ));
        }

        // Check for missing required variables
        for dep in &risk.dependencies {
            if dep.required && dep.default_value.is_none() {
                report.add_warning(format!(
                    "Template requires variable '{}' without default value",
                    dep.variable_name
                ));
            }
        }

        // Check complexity
        if risk.complexity_score > 0.7 {
            report.add_warning(format!(
                "Template complexity is high ({:.2}). Consider refactoring.",
                risk.complexity_score
            ));
        }

        // Apply prevention rules
        for rule in &self.rules {
            if let Some(action) = rule.evaluate_template(&risk) {
                match action {
                    PreventionAction::Block(reason) => report.add_blocker(reason),
                    PreventionAction::Warn(reason) => report.add_warning(reason),
                    PreventionAction::Allow => {}
                }
            }
        }

        self.history.record_template_validation(&report);

        Ok(report)
    }

    /// Get prevention statistics
    pub fn get_statistics(&self) -> PreventionStatistics {
        self.history.calculate_statistics()
    }

    /// Default prevention rules
    fn default_rules() -> Vec<PreventionRule> {
        vec![
            PreventionRule {
                name: "Block critical RPN".to_string(),
                description: "Block any operation with RPN > 500".to_string(),
                rule_type: RuleType::RpnThreshold {
                    threshold: 500,
                    action: PreventionAction::Block(
                        "RPN exceeds critical threshold (500)".to_string(),
                    ),
                },
            },
            PreventionRule {
                name: "Warn high RPN".to_string(),
                description: "Warn for RPN > 200".to_string(),
                rule_type: RuleType::RpnThreshold {
                    threshold: 200,
                    action: PreventionAction::Warn("RPN exceeds warning threshold (200)".to_string()),
                },
            },
            PreventionRule {
                name: "Complexity limit".to_string(),
                description: "Warn when complexity > 0.8".to_string(),
                rule_type: RuleType::ComplexityThreshold {
                    threshold: 0.8,
                    action: PreventionAction::Warn("Complexity exceeds threshold (0.8)".to_string()),
                },
            },
        ]
    }
}

impl Default for FailurePreventionEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Prevention rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreventionRule {
    /// Rule name
    pub name: String,

    /// Description
    pub description: String,

    /// Rule type and configuration
    pub rule_type: RuleType,
}

impl PreventionRule {
    /// Evaluate rule against schema changes
    pub fn evaluate_schema_changes(&self, _changes: &[SchemaChange]) -> Option<PreventionAction> {
        match &self.rule_type {
            RuleType::Custom(_) => None,
            _ => None,
        }
    }

    /// Evaluate rule against query risk
    pub fn evaluate_query(&self, risk: &QueryRisk) -> Option<PreventionAction> {
        match &self.rule_type {
            RuleType::RpnThreshold { threshold, action } => {
                if risk.rpn.value() >= *threshold {
                    Some(action.clone())
                } else {
                    None
                }
            }
            RuleType::ComplexityThreshold { threshold, action } => {
                if risk.complexity_score >= *threshold {
                    Some(action.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Evaluate rule against template risk
    pub fn evaluate_template(&self, risk: &TemplateRisk) -> Option<PreventionAction> {
        match &self.rule_type {
            RuleType::RpnThreshold { threshold, action } => {
                if risk.rpn.value() >= *threshold {
                    Some(action.clone())
                } else {
                    None
                }
            }
            RuleType::ComplexityThreshold { threshold, action } => {
                if risk.complexity_score >= *threshold {
                    Some(action.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// Types of prevention rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleType {
    /// RPN threshold rule
    RpnThreshold {
        threshold: u16,
        action: PreventionAction,
    },

    /// Complexity threshold rule
    ComplexityThreshold {
        threshold: f64,
        action: PreventionAction,
    },

    /// Custom rule (evaluator function not serializable)
    Custom(String),
}

/// Actions the prevention system can take
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PreventionAction {
    /// Block the operation
    Block(String),

    /// Warn but allow
    Warn(String),

    /// Allow without warning
    Allow,
}

/// Prevention report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreventionReport {
    /// Report name
    pub name: String,

    /// Blocking issues
    pub blockers: Vec<String>,

    /// Warning issues
    pub warnings: Vec<String>,

    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl PreventionReport {
    /// Create a new prevention report
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            blockers: Vec::new(),
            warnings: Vec::new(),
            timestamp: chrono::Utc::now(),
        }
    }

    /// Add a blocker
    pub fn add_blocker(&mut self, reason: String) {
        self.blockers.push(reason);
    }

    /// Add a warning
    pub fn add_warning(&mut self, reason: String) {
        self.warnings.push(reason);
    }

    /// Check if operation should be blocked
    pub fn is_blocked(&self) -> bool {
        !self.blockers.is_empty()
    }

    /// Check if there are warnings
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    /// Get summary
    pub fn summary(&self) -> String {
        if self.is_blocked() {
            format!(
                "BLOCKED: {} blocker(s), {} warning(s)",
                self.blockers.len(),
                self.warnings.len()
            )
        } else if self.has_warnings() {
            format!("PASSED WITH WARNINGS: {} warning(s)", self.warnings.len())
        } else {
            "PASSED".to_string()
        }
    }
}

/// Prevention history tracker
#[derive(Debug, Clone, Default)]
struct PreventionHistory {
    schema_validations: Vec<PreventionReport>,
    query_validations: Vec<PreventionReport>,
    template_validations: Vec<PreventionReport>,
}

impl PreventionHistory {
    fn new() -> Self {
        Self::default()
    }

    fn record_schema_validation(&mut self, report: &PreventionReport) {
        self.schema_validations.push(report.clone());
    }

    fn record_query_validation(&mut self, report: &PreventionReport) {
        self.query_validations.push(report.clone());
    }

    fn record_template_validation(&mut self, report: &PreventionReport) {
        self.template_validations.push(report.clone());
    }

    fn calculate_statistics(&self) -> PreventionStatistics {
        PreventionStatistics {
            total_schema_validations: self.schema_validations.len(),
            schema_blocks: self.schema_validations.iter().filter(|r| r.is_blocked()).count(),
            total_query_validations: self.query_validations.len(),
            query_blocks: self.query_validations.iter().filter(|r| r.is_blocked()).count(),
            total_template_validations: self.template_validations.len(),
            template_blocks: self.template_validations.iter().filter(|r| r.is_blocked()).count(),
        }
    }
}

/// Prevention statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreventionStatistics {
    pub total_schema_validations: usize,
    pub schema_blocks: usize,
    pub total_query_validations: usize,
    pub query_blocks: usize,
    pub total_template_validations: usize,
    pub template_blocks: usize,
}

impl PreventionStatistics {
    /// Calculate block rate
    pub fn block_rate(&self) -> f64 {
        let total = self.total_schema_validations
            + self.total_query_validations
            + self.total_template_validations;
        let blocks = self.schema_blocks + self.query_blocks + self.template_blocks;

        if total > 0 {
            (blocks as f64) / (total as f64)
        } else {
            0.0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prevention_engine_creation() {
        let engine = FailurePreventionEngine::new();
        assert_eq!(engine.rules.len(), 3); // Default rules
    }

    #[test]
    fn test_prevention_report() {
        let mut report = PreventionReport::new("Test");
        assert!(!report.is_blocked());
        assert!(!report.has_warnings());

        report.add_warning("Test warning".to_string());
        assert!(!report.is_blocked());
        assert!(report.has_warnings());

        report.add_blocker("Test blocker".to_string());
        assert!(report.is_blocked());
    }

    #[test]
    fn test_prevention_statistics() {
        let stats = PreventionStatistics {
            total_schema_validations: 10,
            schema_blocks: 2,
            total_query_validations: 20,
            query_blocks: 3,
            total_template_validations: 15,
            template_blocks: 1,
        };

        let rate = stats.block_rate();
        assert!((rate - 0.133).abs() < 0.01); // 6/45 ≈ 0.133
    }

    #[test]
    fn test_custom_rule() {
        let rule = PreventionRule {
            name: "Test rule".to_string(),
            description: "Test".to_string(),
            rule_type: RuleType::RpnThreshold {
                threshold: 100,
                action: PreventionAction::Block("Too high".to_string()),
            },
        };

        assert_eq!(rule.name, "Test rule");
    }
}
