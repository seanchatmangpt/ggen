//! Performance budget enforcement and violation detection
//!
//! This module validates tests against strict performance budgets:
//! - Unit tests: ≤1s total (1,000ms)
//! - Integration tests: ≤10s total (10,000ms)
//! - Combined: ≤11s total (11,000ms)
//!
//! Violations are classified by severity and tracked for reporting.

use crate::types::{BudgetViolation, OptimizationResult, Severity, TestId, TestType};
use std::collections::HashMap;

/// Performance budgets (milliseconds)
#[derive(Debug, Clone, Copy)]
pub struct PerformanceBudgets {
    /// Unit test budget: 1,000ms (1s)
    pub unit_test_budget_ms: u64,
    /// Integration test budget: 10,000ms (10s)
    pub integration_test_budget_ms: u64,
    /// Combined budget: 11,000ms (11s)
    pub combined_budget_ms: u64,
}

impl Default for PerformanceBudgets {
    fn default() -> Self {
        Self {
            unit_test_budget_ms: 1_000,
            integration_test_budget_ms: 10_000,
            combined_budget_ms: 11_000,
        }
    }
}

/// Budget enforcement engine
#[derive(Debug)]
pub struct BudgetEnforcer {
    budgets: PerformanceBudgets,
}

impl BudgetEnforcer {
    /// Create new budget enforcer with default budgets
    pub fn new() -> Self {
        Self {
            budgets: PerformanceBudgets::default(),
        }
    }

    /// Create budget enforcer with custom budgets
    pub fn with_budgets(budgets: PerformanceBudgets) -> Self {
        Self { budgets }
    }

    /// Get budget for specific test type
    ///
    /// # Arguments
    /// * `test_type` - Type of test (Unit/Integration/Benchmark)
    #[must_use]
    pub fn budget_for_type(&self, test_type: TestType) -> u64 {
        match test_type {
            TestType::Unit => self.budgets.unit_test_budget_ms,
            TestType::Integration => self.budgets.integration_test_budget_ms,
            TestType::Benchmark => u64::MAX, // Benchmarks exempt from budgets
        }
    }

    /// Check if single test violates its budget
    ///
    /// # Arguments
    /// * `test_id` - Test identifier
    /// * `test_type` - Type of test
    /// * `exec_time_ms` - Actual execution time
    ///
    /// # Returns
    /// `Some(BudgetViolation)` if test exceeds budget, `None` otherwise
    #[must_use]
    pub fn check_test_budget(
        &self,
        test_id: TestId,
        test_type: TestType,
        exec_time_ms: u64,
    ) -> Option<BudgetViolation> {
        let budget_ms = self.budget_for_type(test_type);

        if exec_time_ms > budget_ms {
            Some(BudgetViolation::new(test_id, exec_time_ms, budget_ms))
        } else {
            None
        }
    }

    /// Validate all tests against budgets
    ///
    /// # Arguments
    /// * `tests` - Map of test_id to (test_type, exec_time_ms)
    ///
    /// # Returns
    /// List of all budget violations
    pub fn validate_all_budgets(
        &self,
        tests: &HashMap<TestId, (TestType, u64)>,
    ) -> Vec<BudgetViolation> {
        tests
            .iter()
            .filter_map(|(test_id, (test_type, exec_time_ms))| {
                self.check_test_budget(test_id.clone(), *test_type, *exec_time_ms)
            })
            .collect()
    }

    /// Calculate total execution time by test type
    ///
    /// # Arguments
    /// * `tests` - Map of test_id to (test_type, exec_time_ms)
    ///
    /// # Returns
    /// (unit_total_ms, integration_total_ms, combined_total_ms)
    #[must_use]
    pub fn calculate_totals(
        &self,
        tests: &HashMap<TestId, (TestType, u64)>,
    ) -> (u64, u64, u64) {
        let mut unit_total = 0u64;
        let mut integration_total = 0u64;

        for (_test_id, (test_type, exec_time_ms)) in tests {
            match test_type {
                TestType::Unit => unit_total += exec_time_ms,
                TestType::Integration => integration_total += exec_time_ms,
                TestType::Benchmark => {} // Benchmarks don't count toward budgets
            }
        }

        let combined_total = unit_total + integration_total;
        (unit_total, integration_total, combined_total)
    }

    /// Check if total execution time violates combined budget
    ///
    /// # Arguments
    /// * `tests` - Map of test_id to (test_type, exec_time_ms)
    ///
    /// # Returns
    /// `Ok(())` if within budget, `Err` with violation details otherwise
    pub fn validate_combined_budget(
        &self,
        tests: &HashMap<TestId, (TestType, u64)>,
    ) -> OptimizationResult<()> {
        let (unit_total, integration_total, combined_total) = self.calculate_totals(tests);

        // Check individual budgets first
        if unit_total > self.budgets.unit_test_budget_ms {
            return Err(crate::types::OptimizationError::BudgetExceeded(format!(
                "Unit tests exceed budget: {}ms > {}ms ({}ms over)",
                unit_total,
                self.budgets.unit_test_budget_ms,
                unit_total - self.budgets.unit_test_budget_ms
            )));
        }

        if integration_total > self.budgets.integration_test_budget_ms {
            return Err(crate::types::OptimizationError::BudgetExceeded(format!(
                "Integration tests exceed budget: {}ms > {}ms ({}ms over)",
                integration_total,
                self.budgets.integration_test_budget_ms,
                integration_total - self.budgets.integration_test_budget_ms
            )));
        }

        // Check combined budget
        if combined_total > self.budgets.combined_budget_ms {
            return Err(crate::types::OptimizationError::BudgetExceeded(format!(
                "Combined tests exceed budget: {}ms > {}ms ({}ms over)",
                combined_total,
                self.budgets.combined_budget_ms,
                combined_total - self.budgets.combined_budget_ms
            )));
        }

        Ok(())
    }

    /// Generate budget compliance report
    ///
    /// # Arguments
    /// * `tests` - Map of test_id to (test_type, exec_time_ms)
    ///
    /// # Returns
    /// Formatted budget report with totals, violations, and compliance status
    pub fn generate_budget_report(&self, tests: &HashMap<TestId, (TestType, u64)>) -> String {
        let (unit_total, integration_total, combined_total) = self.calculate_totals(tests);
        let violations = self.validate_all_budgets(tests);

        let unit_status = if unit_total <= self.budgets.unit_test_budget_ms {
            "✅ PASS"
        } else {
            "❌ FAIL"
        };

        let integration_status = if integration_total <= self.budgets.integration_test_budget_ms {
            "✅ PASS"
        } else {
            "❌ FAIL"
        };

        let combined_status = if combined_total <= self.budgets.combined_budget_ms {
            "✅ PASS"
        } else {
            "❌ FAIL"
        };

        let mut report = String::new();
        report.push_str("⏱️  Performance Budget Compliance Report\n\n");
        report.push_str(&format!(
            "Unit Tests:        {:>6}ms / {:>6}ms {}\n",
            unit_total, self.budgets.unit_test_budget_ms, unit_status
        ));
        report.push_str(&format!(
            "Integration Tests: {:>6}ms / {:>6}ms {}\n",
            integration_total, self.budgets.integration_test_budget_ms, integration_status
        ));
        report.push_str(&format!(
            "Combined:          {:>6}ms / {:>6}ms {}\n\n",
            combined_total, self.budgets.combined_budget_ms, combined_status
        ));

        if !violations.is_empty() {
            report.push_str(&format!("⚠️  Budget Violations: {}\n\n", violations.len()));

            // Group by severity
            let critical: Vec<_> = violations
                .iter()
                .filter(|v| matches!(v.severity, Severity::Critical))
                .collect();
            let errors: Vec<_> = violations
                .iter()
                .filter(|v| matches!(v.severity, Severity::Error))
                .collect();
            let warnings: Vec<_> = violations
                .iter()
                .filter(|v| matches!(v.severity, Severity::Warning))
                .collect();

            if !critical.is_empty() {
                report.push_str(&format!("🔴 Critical ({}):\n", critical.len()));
                for violation in &critical {
                    report.push_str(&format!(
                        "  - {}: {}ms ({}ms over budget)\n",
                        violation.test_id.as_str(),
                        violation.exec_time_ms,
                        violation.excess_ms
                    ));
                }
                report.push('\n');
            }

            if !errors.is_empty() {
                report.push_str(&format!("🟠 Errors ({}):\n", errors.len()));
                for violation in &errors {
                    report.push_str(&format!(
                        "  - {}: {}ms ({}ms over budget)\n",
                        violation.test_id.as_str(),
                        violation.exec_time_ms,
                        violation.excess_ms
                    ));
                }
                report.push('\n');
            }

            if !warnings.is_empty() {
                report.push_str(&format!("🟡 Warnings ({}):\n", warnings.len()));
                for violation in &warnings {
                    report.push_str(&format!(
                        "  - {}: {}ms ({}ms over budget)\n",
                        violation.test_id.as_str(),
                        violation.exec_time_ms,
                        violation.excess_ms
                    ));
                }
            }
        } else {
            report.push_str("✅ All tests within budget!\n");
        }

        report
    }
}

impl Default for BudgetEnforcer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enforcer_creation_with_default_budgets() {
        let enforcer = BudgetEnforcer::new();
        assert_eq!(enforcer.budgets.unit_test_budget_ms, 1_000);
        assert_eq!(enforcer.budgets.integration_test_budget_ms, 10_000);
        assert_eq!(enforcer.budgets.combined_budget_ms, 11_000);
    }

    #[test]
    fn test_budget_for_unit_test() {
        let enforcer = BudgetEnforcer::new();
        assert_eq!(enforcer.budget_for_type(TestType::Unit), 1_000);
    }

    #[test]
    fn test_budget_for_integration_test() {
        let enforcer = BudgetEnforcer::new();
        assert_eq!(enforcer.budget_for_type(TestType::Integration), 10_000);
    }

    #[test]
    fn test_check_test_budget_within_budget() {
        let enforcer = BudgetEnforcer::new();
        let test_id = TestId::new("fast_test").unwrap();
        let violation = enforcer.check_test_budget(test_id, TestType::Unit, 500);
        assert!(violation.is_none());
    }

    #[test]
    fn test_check_test_budget_violation() {
        let enforcer = BudgetEnforcer::new();
        let test_id = TestId::new("slow_test").unwrap();
        let violation = enforcer.check_test_budget(test_id.clone(), TestType::Unit, 1_500);
        assert!(violation.is_some());

        let v = violation.unwrap();
        assert_eq!(v.test_id, test_id);
        assert_eq!(v.exec_time_ms, 1_500);
        assert_eq!(v.budget_ms, 1_000);
        assert_eq!(v.excess_ms, 500);
    }

    #[test]
    fn test_calculate_totals() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("unit1").unwrap(),
            (TestType::Unit, 200),
        );
        tests.insert(
            TestId::new("unit2").unwrap(),
            (TestType::Unit, 300),
        );
        tests.insert(
            TestId::new("integration1").unwrap(),
            (TestType::Integration, 2_000),
        );

        let (unit_total, integration_total, combined_total) = enforcer.calculate_totals(&tests);
        assert_eq!(unit_total, 500);
        assert_eq!(integration_total, 2_000);
        assert_eq!(combined_total, 2_500);
    }

    #[test]
    fn test_validate_combined_budget_success() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("unit1").unwrap(),
            (TestType::Unit, 900),
        );
        tests.insert(
            TestId::new("integration1").unwrap(),
            (TestType::Integration, 9_000),
        );

        let result = enforcer.validate_combined_budget(&tests);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_combined_budget_unit_violation() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("slow_unit").unwrap(),
            (TestType::Unit, 1_500),
        );

        let result = enforcer.validate_combined_budget(&tests);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unit tests exceed budget"));
    }

    #[test]
    fn test_validate_combined_budget_integration_violation() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("slow_integration").unwrap(),
            (TestType::Integration, 11_000),
        );

        let result = enforcer.validate_combined_budget(&tests);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Integration tests exceed budget"));
    }

    #[test]
    fn test_validate_combined_budget_combined_violation() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("unit1").unwrap(),
            (TestType::Unit, 1_000),
        );
        tests.insert(
            TestId::new("integration1").unwrap(),
            (TestType::Integration, 10_500),
        );

        let result = enforcer.validate_combined_budget(&tests);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Combined tests exceed budget"));
    }

    #[test]
    fn test_generate_budget_report_passing() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("unit1").unwrap(),
            (TestType::Unit, 500),
        );
        tests.insert(
            TestId::new("integration1").unwrap(),
            (TestType::Integration, 5_000),
        );

        let report = enforcer.generate_budget_report(&tests);
        assert!(report.contains("✅ PASS"));
        assert!(report.contains("All tests within budget"));
    }

    #[test]
    fn test_generate_budget_report_with_violations() {
        let enforcer = BudgetEnforcer::new();
        let mut tests = HashMap::new();

        tests.insert(
            TestId::new("slow_unit").unwrap(),
            (TestType::Unit, 1_500),
        );

        let report = enforcer.generate_budget_report(&tests);
        assert!(report.contains("❌ FAIL"));
        assert!(report.contains("Budget Violations"));
    }
}
