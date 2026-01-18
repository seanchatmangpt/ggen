//! SPARQL validation rules execution for post-generation quality checks

use crate::graph::Graph;
use crate::validation::error::{Result, ValidationError};
use crate::validation::violation::{ConstraintType, Severity, ValidationResult, Violation};
use oxigraph::sparql::QueryResults;
use std::time::Instant;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleSeverity {
    Error,
    Warning,
    Info,
}

impl From<RuleSeverity> for Severity {
    fn from(severity: RuleSeverity) -> Self {
        match severity {
            RuleSeverity::Error => Severity::Violation,
            RuleSeverity::Warning => Severity::Warning,
            RuleSeverity::Info => Severity::Info,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValidationRule {
    pub id: String,
    pub query: String,
    pub severity: RuleSeverity,
    pub message: String,
}

impl ValidationRule {
    pub fn new(
        id: impl Into<String>, query: impl Into<String>, severity: RuleSeverity,
        message: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            query: query.into(),
            severity,
            message: message.into(),
        }
    }

    pub fn error(
        id: impl Into<String>, query: impl Into<String>, message: impl Into<String>,
    ) -> Self {
        Self::new(id, query, RuleSeverity::Error, message)
    }

    pub fn warning(
        id: impl Into<String>, query: impl Into<String>, message: impl Into<String>,
    ) -> Self {
        Self::new(id, query, RuleSeverity::Warning, message)
    }

    pub fn info(
        id: impl Into<String>, query: impl Into<String>, message: impl Into<String>,
    ) -> Self {
        Self::new(id, query, RuleSeverity::Info, message)
    }
}

#[derive(Debug, Clone)]
pub struct RuleExecutor {
    timeout_ms: u64,
}

impl RuleExecutor {
    pub fn new() -> Self {
        Self { timeout_ms: 30000 }
    }

    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    pub fn execute(&self, output: &Graph, rules: &[ValidationRule]) -> Result<ValidationResult> {
        let start = Instant::now();
        let mut violations = Vec::new();

        for rule in rules {
            if start.elapsed().as_millis() > self.timeout_ms as u128 {
                return Err(ValidationError::timeout(
                    "Validation rules execution",
                    self.timeout_ms,
                ));
            }

            match self.execute_rule(output, rule) {
                Ok(rule_violations) => {
                    let has_violations = !rule_violations.is_empty();
                    violations.extend(rule_violations);

                    if has_violations && rule.severity == RuleSeverity::Error {
                        let duration_ms = start.elapsed().as_millis() as u64;
                        return Ok(ValidationResult::fail(violations, duration_ms));
                    }
                }
                Err(e) => {
                    let violation = Violation::new(
                        "query-execution-error",
                        ConstraintType::Cardinality,
                        format!("Failed to execute rule {}: {}", rule.id, e),
                    )
                    .with_severity(rule.severity.into());
                    violations.push(violation);

                    if rule.severity == RuleSeverity::Error {
                        let duration_ms = start.elapsed().as_millis() as u64;
                        return Ok(ValidationResult::fail(violations, duration_ms));
                    }
                }
            }
        }

        let duration_ms = start.elapsed().as_millis() as u64;
        let passed = violations.iter().all(|v| v.severity != Severity::Violation);

        if passed {
            Ok(ValidationResult::pass(duration_ms))
        } else {
            Ok(ValidationResult::fail(violations, duration_ms))
        }
    }

    fn execute_rule(&self, output: &Graph, rule: &ValidationRule) -> Result<Vec<Violation>> {
        let query_str = rule.query.trim().to_uppercase();

        if query_str.starts_with("ASK") {
            self.execute_ask_rule(output, rule)
        } else if query_str.starts_with("SELECT") {
            self.execute_select_rule(output, rule)
        } else {
            Err(ValidationError::invalid_query(
                &rule.id,
                "Query must start with ASK or SELECT",
            ))
        }
    }

    fn execute_ask_rule(&self, output: &Graph, rule: &ValidationRule) -> Result<Vec<Violation>> {
        let results = output
            .query(&rule.query)
            .map_err(|e| ValidationError::query_execution(&rule.id, &e.to_string()))?;

        let ask_result = match results {
            QueryResults::Boolean(b) => b,
            _ => {
                return Err(ValidationError::invalid_query(
                    &rule.id,
                    "Expected ASK query to return boolean",
                ))
            }
        };

        if ask_result {
            Ok(Vec::new())
        } else {
            let violation = Violation::new(
                rule.id.clone(),
                ConstraintType::Cardinality,
                rule.message.clone(),
            )
            .with_severity(rule.severity.into());
            Ok(vec![violation])
        }
    }

    fn execute_select_rule(&self, output: &Graph, rule: &ValidationRule) -> Result<Vec<Violation>> {
        let results = output
            .query(&rule.query)
            .map_err(|e| ValidationError::query_execution(&rule.id, &e.to_string()))?;

        let mut violations = Vec::new();

        match results {
            QueryResults::Solutions(solutions) => {
                for solution_result in solutions {
                    let solution = solution_result
                        .map_err(|e| ValidationError::query_execution(&rule.id, &e.to_string()))?;

                    let focus_node = solution
                        .get("node")
                        .or_else(|| solution.get("s"))
                        .map(|term| term.to_string())
                        .unwrap_or_else(|| "unknown".to_string());

                    let value = solution
                        .get("value")
                        .or_else(|| solution.get("o"))
                        .map(|term| term.to_string());

                    let message = if let Some(val) = value {
                        format!("{} (value: {})", rule.message, val)
                    } else {
                        rule.message.clone()
                    };

                    let violation =
                        Violation::new(focus_node, ConstraintType::Cardinality, message)
                            .with_severity(rule.severity.into());

                    violations.push(violation);
                }
            }
            _ => {
                return Err(ValidationError::invalid_query(
                    &rule.id,
                    "Expected SELECT query to return solutions",
                ))
            }
        }

        Ok(violations)
    }
}

impl Default for RuleExecutor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rule_creation() {
        let rule = ValidationRule::error("test-rule", "ASK { ?s ?p ?o }", "Test violation");
        assert_eq!(rule.id, "test-rule");
        assert_eq!(rule.severity, RuleSeverity::Error);
        assert_eq!(rule.message, "Test violation");
    }

    #[test]
    fn test_executor_creation() {
        let executor = RuleExecutor::new();
        assert_eq!(executor.timeout_ms, 30000);

        let executor = RuleExecutor::new().with_timeout(5000);
        assert_eq!(executor.timeout_ms, 5000);
    }
}
