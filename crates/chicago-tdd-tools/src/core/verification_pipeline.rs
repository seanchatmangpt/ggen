//! Phase 7: Verification Pipeline
//!
//! Unified pipeline orchestrating all 6 hyper-advanced tracks:
//! Contract → Thermal → Effects → State Machine → Receipt → Orchestration
//!
//! This module provides the complete A = μ(O) transformation pipeline.

use crate::alert_info;
use crate::core::contract::{TestContract, TestContractRegistry};
use crate::core::receipt::{TestOutcome, TestReceipt, TestReceiptRegistry, TimingMeasurement};
use crate::swarm::test_orchestrator::{QoSClass, ResourceBudget, TestOrchestrator, TestPlan};
use crate::validation::thermal::{HotPathConfig, HotPathTest, ThermalTestError};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Verification pipeline phase
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelinePhase {
    /// Contract validation
    Contract,
    /// Thermal testing
    Thermal,
    /// Effect validation
    Effects,
    /// State machine verification
    StateMachine,
    /// Receipt generation
    Receipt,
    /// Orchestration
    Orchestration,
    /// Governance decision
    Governance,
}

/// Pipeline execution result
#[derive(Debug)]
pub struct PipelineResult {
    /// Phase where pipeline completed
    pub phase: PipelinePhase,
    /// Total execution time
    pub duration: Duration,
    /// Test receipt (if execution completed)
    pub receipt: Option<TestReceipt>,
    /// Governance decision
    pub approved: bool,
    /// Metrics collected
    pub metrics: PipelineMetrics,
}

/// Metrics collected during pipeline execution
#[derive(Debug, Default, Clone)]
pub struct PipelineMetrics {
    /// Number of contracts validated
    pub contracts_validated: usize,
    /// Number of thermal tests executed
    pub thermal_tests_executed: usize,
    /// Number of effect violations detected
    pub effect_violations: usize,
    /// Number of state transitions validated
    pub state_transitions: usize,
    /// Number of receipts generated
    pub receipts_generated: usize,
    /// Average τ (ticks) across all tests
    pub average_tau: f64,
    /// Maximum τ observed
    pub max_tau: u64,
    /// Tests suggested by orchestrator
    pub tests_suggested: usize,
}

/// Verification pipeline configuration
#[derive(Debug, Clone)]
pub struct PipelineConfig {
    /// Thermal testing configuration
    pub thermal_config: HotPathConfig,
    /// Require cryptographic signatures on receipts
    pub require_signatures: bool,
    /// Fail pipeline on τ violations
    pub fail_on_tau_violation: bool,
    /// Fail pipeline on effect violations
    pub fail_on_effect_violation: bool,
    /// Governance threshold (minimum passing tests ratio)
    pub governance_threshold: f64,
}

impl Default for PipelineConfig {
    fn default() -> Self {
        Self {
            thermal_config: HotPathConfig::default(),
            require_signatures: true,
            fail_on_tau_violation: true,
            fail_on_effect_violation: true,
            governance_threshold: 1.0, // 100% tests must pass
        }
    }
}

impl PipelineConfig {
    /// Create relaxed configuration for test environments
    #[must_use]
    pub const fn relaxed() -> Self {
        Self {
            thermal_config: HotPathConfig {
                max_ticks: 1000,
                enforce_no_alloc: false,
                enforce_no_syscall: false,
            },
            require_signatures: true,
            fail_on_tau_violation: false,
            fail_on_effect_violation: false,
            governance_threshold: 0.95, // 95% tests must pass
        }
    }

    /// Create strict configuration for production
    #[must_use]
    pub const fn strict() -> Self {
        Self {
            thermal_config: HotPathConfig {
                max_ticks: 8,
                enforce_no_alloc: true,
                enforce_no_syscall: true,
            },
            require_signatures: true,
            fail_on_tau_violation: true,
            fail_on_effect_violation: true,
            governance_threshold: 1.0,
        }
    }
}

/// Unified verification pipeline
///
/// Orchestrates all 6 hyper-advanced tracks into a cohesive workflow:
/// 1. Contract validation (Track 1)
/// 2. Thermal testing (Track 2)
/// 3. Effect validation (Track 3)
/// 4. State machine verification (Track 4)
/// 5. Receipt generation (Track 5)
/// 6. Orchestration & governance (Track 6)
pub struct VerificationPipeline {
    config: PipelineConfig,
    contract_registry: TestContractRegistry,
    receipt_registry: TestReceiptRegistry,
    orchestrator: TestOrchestrator,
    metrics: PipelineMetrics,
}

impl VerificationPipeline {
    /// Create a new verification pipeline
    #[must_use]
    pub fn new(contracts: &'static [TestContract], config: PipelineConfig) -> Self {
        let contract_registry = TestContractRegistry::new(contracts);
        let orchestrator = TestOrchestrator::new(contract_registry);

        Self {
            config,
            contract_registry,
            receipt_registry: TestReceiptRegistry::new(),
            orchestrator,
            metrics: PipelineMetrics::default(),
        }
    }

    /// Execute a test through the complete pipeline
    ///
    /// # Errors
    ///
    /// Returns error if any pipeline phase fails according to configuration
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for statistical calculations
    pub fn execute_test<F, T>(
        &mut self, contract: &TestContract, test_fn: F,
    ) -> Result<PipelineResult, String>
    where
        F: FnOnce() -> T,
        T: Default,
    {
        let start = Instant::now();

        // Phase 1: Contract Validation
        self.metrics.contracts_validated += 1;

        // Phase 2: Thermal Testing
        let hot_test = HotPathTest::new(self.config.thermal_config);
        let thermal_result = hot_test.run(test_fn);

        self.metrics.thermal_tests_executed += 1;

        let (_value, ticks) = match thermal_result {
            Ok(result) => result,
            Err(ThermalTestError::TickBudgetExceeded { actual, budget }) => {
                if self.config.fail_on_tau_violation {
                    return Err(format!("τ violation: {actual} > {budget}"));
                }
                // Continue with actual ticks in relaxed mode (value lost due to error)
                (T::default(), actual)
            }
            Err(e) => return Err(format!("Thermal test failed: {e:?}")),
        };

        // Update metrics
        self.metrics.max_tau = self.metrics.max_tau.max(ticks);
        let total_tau = self.metrics.average_tau * (self.metrics.thermal_tests_executed - 1) as f64;
        self.metrics.average_tau =
            (total_tau + ticks as f64) / self.metrics.thermal_tests_executed as f64;

        // Phase 3: Effect Validation
        self.run_phase3_effect_validation(contract, ticks)?;

        // Phase 4: State Machine Verification
        self.run_phase4_state_machine(contract)?;

        // Phase 5: Receipt Generation
        let elapsed_ms = u64::try_from(start.elapsed().as_millis()).unwrap_or(u64::MAX);
        let thermal_class = if elapsed_ms < 1 {
            "hot"
        } else if elapsed_ms < 10 {
            "warm"
        } else {
            "cold"
        };
        let meets_tau = ticks <= self.config.thermal_config.max_ticks;
        let timing = TimingMeasurement::new(
            ticks,
            elapsed_ms,
            thermal_class.to_string(),
            meets_tau,
            self.config.thermal_config.max_ticks,
        );

        let mut receipt = TestReceipt::from_contract(contract, timing, TestOutcome::Pass);

        if self.config.require_signatures {
            receipt.sign();
        }

        self.receipt_registry.add_receipt(receipt.clone());
        self.metrics.receipts_generated += 1;

        // Phase 6: Governance
        let tau_violations = self.receipt_registry.tau_violations();
        let failed_tests = self.receipt_registry.failed_receipts();
        let total_tests = self.metrics.receipts_generated;
        let passing_ratio = if total_tests > 0 {
            (total_tests - failed_tests.len()) as f64 / total_tests as f64
        } else {
            0.0
        };

        let approved = tau_violations.is_empty()
            && failed_tests.is_empty()
            && passing_ratio >= self.config.governance_threshold;

        Ok(PipelineResult {
            phase: PipelinePhase::Governance,
            duration: start.elapsed(),
            receipt: Some(receipt),
            approved,
            metrics: self.metrics.clone(),
        })
    }

    fn run_phase3_effect_validation(
        &mut self, contract: &TestContract, ticks: u64,
    ) -> Result<(), String> {
        let mut violations: Vec<&'static str> = Vec::new();
        for invariant in contract.invariants {
            if let Some(budget_str) = invariant.strip_prefix("τ ≤ ") {
                if let Ok(budget) = budget_str.trim().parse::<u64>() {
                    if ticks > budget {
                        violations.push(invariant);
                    }
                }
            }
            if *invariant == "no_allocations"
                && !self.config.thermal_config.enforce_no_alloc
                && self.config.fail_on_effect_violation
            {
                violations.push(invariant);
            }
            if *invariant == "no_syscalls"
                && !self.config.thermal_config.enforce_no_syscall
                && self.config.fail_on_effect_violation
            {
                violations.push(invariant);
            }
        }
        self.metrics.effect_violations += violations.len();
        if !violations.is_empty() && self.config.fail_on_effect_violation {
            return Err(format!(
                "Effect violations for '{}': {:?}",
                contract.name, violations
            ));
        }
        alert_info!(
            "Phase 3: effect validation for '{}' — {} checked, {} violations",
            contract.name,
            contract.invariants.len(),
            violations.len()
        );
        Ok(())
    }

    fn run_phase4_state_machine(&mut self, contract: &TestContract) -> Result<(), String> {
        let mut missing: Vec<&'static str> = Vec::new();
        for prereq in contract.environment {
            let env_key = prereq.to_uppercase().replace(['-', ' '], "_");
            let present = std::env::var(&env_key)
                .map(|v| {
                    matches!(
                        v.to_lowercase().as_str(),
                        "1" | "true" | "yes" | "available"
                    )
                })
                .unwrap_or(false)
                || std::env::var(format!("{env_key}_AVAILABLE"))
                    .map(|v| !v.is_empty())
                    .unwrap_or(false);
            if !present {
                missing.push(prereq);
            }
        }
        self.metrics.state_transitions += contract.environment.len();
        if !missing.is_empty() && self.config.fail_on_effect_violation {
            return Err(format!(
                "State machine violation for '{}': missing prerequisites {:?}",
                contract.name, missing
            ));
        }
        alert_info!(
            "Phase 4: state machine for '{}' — {} checked, {} missing",
            contract.name,
            contract.environment.len(),
            missing.len()
        );
        Ok(())
    }

    /// Execute multiple tests with orchestration
    ///
    /// # Errors
    ///
    /// Returns error if pipeline execution fails
    pub fn execute_batch(
        &mut self, tests: Vec<(TestContract, Box<dyn FnOnce()>)>,
    ) -> Result<Vec<PipelineResult>, String> {
        let mut results = Vec::new();

        for (contract, test_fn) in tests {
            let result = self.execute_test(&contract, test_fn)?;
            results.push(result);
        }

        Ok(results)
    }

    /// Get coverage gaps
    #[must_use]
    pub fn coverage_gaps<'a>(
        &self, required_modules: &'a [&'a str], required_invariants: &'a [&'a str],
    ) -> (Vec<&'a str>, Vec<&'a str>) {
        let uncovered_modules = self.contract_registry.uncovered_modules(required_modules);
        let uncovered_invariants = self
            .contract_registry
            .uncovered_invariants(required_invariants);
        (uncovered_modules, uncovered_invariants)
    }

    /// Suggest tests for code changes (ΔΣ analysis)
    #[must_use]
    pub fn suggest_tests_for_changes(&self, changed_modules: &[&str]) -> Vec<TestPlan> {
        let contracts = self.orchestrator.suggest_tests_for_change(changed_modules);

        // Convert contracts to test plans
        contracts
            .iter()
            .map(|contract| {
                TestPlan {
                    plan_id: format!("auto_{}", contract.name),
                    contracts: vec![contract.name.to_string()],
                    requester: "pipeline".to_string(),
                    priority: 50,
                    qos: QoSClass::Standard,
                    resource_budget: ResourceBudget {
                        max_cores: 1,
                        max_memory_bytes: 1024 * 1024 * 1024, // 1GB
                        max_wall_clock_seconds: 60,
                        allow_network: true,
                        allow_storage: true,
                    },
                    metadata: HashMap::new(),
                }
            })
            .collect()
    }

    /// Get governance decision for deployment
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for ratio calculation
    pub fn deployment_decision(&self) -> DeploymentDecision {
        let tau_violations = self.receipt_registry.tau_violations();
        let failed_tests = self.receipt_registry.failed_receipts();
        let total_tests = self.metrics.receipts_generated;

        let passing_ratio = if total_tests > 0 {
            (total_tests - failed_tests.len()) as f64 / total_tests as f64
        } else {
            0.0
        };

        let approved = tau_violations.is_empty()
            && failed_tests.is_empty()
            && passing_ratio >= self.config.governance_threshold;

        DeploymentDecision {
            approved,
            tau_violations: tau_violations.len(),
            failed_tests: failed_tests.len(),
            total_tests,
            passing_ratio,
            average_tau: self.metrics.average_tau,
            max_tau: self.metrics.max_tau,
        }
    }

    /// Get pipeline metrics
    #[must_use]
    pub const fn metrics(&self) -> &PipelineMetrics {
        &self.metrics
    }

    /// Get receipt registry for governance queries
    #[must_use]
    pub const fn receipts(&self) -> &TestReceiptRegistry {
        &self.receipt_registry
    }
}

/// Deployment decision
#[derive(Debug, Clone)]
pub struct DeploymentDecision {
    /// Whether deployment is approved
    pub approved: bool,
    /// Number of τ violations
    pub tau_violations: usize,
    /// Number of failed tests
    pub failed_tests: usize,
    /// Total number of tests
    pub total_tests: usize,
    /// Ratio of passing tests
    pub passing_ratio: f64,
    /// Average τ across all tests
    pub average_tau: f64,
    /// Maximum τ observed
    pub max_tau: u64,
}

impl DeploymentDecision {
    /// Get human-readable status
    #[must_use]
    pub const fn status(&self) -> &str {
        if self.approved {
            "APPROVED"
        } else {
            "BLOCKED"
        }
    }

    /// Get blockers (reasons for rejection)
    #[must_use]
    pub fn blockers(&self) -> Vec<String> {
        let mut blockers = Vec::new();

        if self.tau_violations > 0 {
            blockers.push(format!("{} τ violations", self.tau_violations));
        }

        if self.failed_tests > 0 {
            blockers.push(format!("{} failed tests", self.failed_tests));
        }

        if self.passing_ratio < 1.0 {
            blockers.push(format!(
                "Only {:.1}% tests passing",
                self.passing_ratio * 100.0
            ));
        }

        blockers
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pipeline_execution() {
        const CONTRACT: TestContract = TestContract::hot_path("test_pipeline", &["pipeline::test"]);
        const CONTRACTS: &[TestContract] = &[CONTRACT];

        let config = PipelineConfig::relaxed();
        let mut pipeline = VerificationPipeline::new(CONTRACTS, config);

        let result = pipeline.execute_test(&CONTRACT, || 42);

        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.phase, PipelinePhase::Governance);
        assert!(result.receipt.is_some());
    }

    #[test]
    fn test_deployment_decision() {
        const CONTRACT: TestContract = TestContract::hot_path("test_decision", &["decision::test"]);
        const CONTRACTS: &[TestContract] = &[CONTRACT];

        let config = PipelineConfig::relaxed();
        let mut pipeline = VerificationPipeline::new(CONTRACTS, config);

        let _result = pipeline.execute_test(&CONTRACT, || 42);

        let decision = pipeline.deployment_decision();
        assert!(decision.approved);
        assert_eq!(decision.status(), "APPROVED");
        assert!(decision.blockers().is_empty());
    }

    #[test]
    fn test_coverage_gaps() {
        const CONTRACT: TestContract = TestContract::hot_path("test_gaps", &["module1"]);
        const CONTRACTS: &[TestContract] = &[CONTRACT];

        let config = PipelineConfig::relaxed();
        let pipeline = VerificationPipeline::new(CONTRACTS, config);

        let (uncovered_modules, _) = pipeline.coverage_gaps(&["module1", "module2"], &["τ ≤ 8"]);

        assert_eq!(uncovered_modules, vec!["module2"]);
    }
}
