//! Swarm-Native Test Orchestrator
//!
//! Provides agent-driven test scheduling and orchestration for trillions of agents:
//! - Accepts test plans from many agents
//! - Schedules executions based on tenant/org/priority/QoS
//! - Hardware-aware scheduling (μ-kernel nodes/cores)
//! - Machine-readable planning API
//! - Suggests minimal sufficient test sets for changes
//! - Suggests additional tests to improve coverage/mutation score
//!
//! # Architecture
//!
//! ```text
//! TestPlan (from agent)
//!     ↓
//! TestOrchestrator
//!     ↓
//! Schedule based on:
//!     - Contracts (coverage, Q, resources)
//!     - Priority/QoS
//!     - Hardware constraints
//!     ↓
//! Execute → Receipts → Governance/AHI
//! ```

use crate::core::contract::{TestContract, TestContractRegistry};
use crate::core::receipt::{TestOutcome, TestReceipt};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

/// Test plan: describes tests to execute
///
/// Agents submit test plans to the orchestrator.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestPlan {
    /// Plan ID
    pub plan_id: String,

    /// Test contracts to execute
    pub contracts: Vec<String>, // Contract names

    /// Requester (agent ID, org, tenant)
    pub requester: String,

    /// Priority (0-100, higher = more urgent)
    pub priority: u8,

    /// `QoS` class (Best Effort, Standard, Premium)
    pub qos: QoSClass,

    /// Resource constraints
    pub resource_budget: ResourceBudget,

    /// Metadata
    pub metadata: HashMap<String, String>,
}

/// `QoS` class for test execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum QoSClass {
    /// Best effort (lowest priority)
    BestEffort,
    /// Standard (medium priority)
    Standard,
    /// Premium (highest priority)
    Premium,
}

/// Resource budget for test execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceBudget {
    /// Maximum cores to use
    pub max_cores: usize,

    /// Maximum memory in bytes
    pub max_memory_bytes: u64,

    /// Maximum wall clock time in seconds
    pub max_wall_clock_seconds: u64,

    /// Allow network access
    pub allow_network: bool,

    /// Allow storage access
    pub allow_storage: bool,
}

impl ResourceBudget {
    /// Create a default resource budget
    #[must_use]
    pub const fn default_budget() -> Self {
        Self {
            max_cores: 1,
            max_memory_bytes: 1_073_741_824, // 1 GB
            max_wall_clock_seconds: 300,     // 5 minutes
            allow_network: false,
            allow_storage: false,
        }
    }

    /// Create an unlimited budget (for premium `QoS`)
    #[must_use]
    pub const fn unlimited() -> Self {
        Self {
            max_cores: usize::MAX,
            max_memory_bytes: u64::MAX,
            max_wall_clock_seconds: 3600, // 1 hour
            allow_network: true,
            allow_storage: true,
        }
    }
}

/// Test execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestExecutionResult {
    /// Plan ID
    pub plan_id: String,

    /// Test receipts
    pub receipts: Vec<TestReceipt>,

    /// Execution summary
    pub summary: ExecutionSummary,
}

/// Execution summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionSummary {
    /// Total tests executed
    pub total_tests: usize,

    /// Tests passed
    pub passed: usize,

    /// Tests failed
    pub failed: usize,

    /// Tests skipped
    pub skipped: usize,

    /// Total wall clock time (ms)
    pub total_wall_clock_ms: u64,

    /// Total ticks
    pub total_ticks: u64,
}

impl ExecutionSummary {
    /// Create a new execution summary
    #[must_use]
    pub const fn new() -> Self {
        Self {
            total_tests: 0,
            passed: 0,
            failed: 0,
            skipped: 0,
            total_wall_clock_ms: 0,
            total_ticks: 0,
        }
    }

    /// Add a receipt to the summary
    #[allow(clippy::missing_const_for_fn)]
    pub fn add_receipt(&mut self, receipt: &TestReceipt) {
        self.total_tests += 1;
        match receipt.result {
            TestOutcome::Pass => self.passed += 1,
            TestOutcome::Fail | TestOutcome::Error => self.failed += 1,
            TestOutcome::Skip => self.skipped += 1,
        }
        self.total_wall_clock_ms += receipt.timing.wall_clock_ms;
        self.total_ticks += receipt.timing.total_ticks;
    }

    /// Check if all tests passed
    #[must_use]
    pub const fn all_passed(&self) -> bool {
        self.failed == 0 && self.total_tests > 0
    }
}

impl Default for ExecutionSummary {
    fn default() -> Self {
        Self::new()
    }
}

/// Test orchestrator: schedules and executes test plans
pub struct TestOrchestrator {
    /// Test contract registry
    registry: TestContractRegistry,

    /// Pending test plans (priority queue)
    pending: VecDeque<TestPlan>,

    /// Executed plans (for audit trail)
    executed: Vec<TestExecutionResult>,
}

impl TestOrchestrator {
    /// Create a new test orchestrator
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(registry: TestContractRegistry) -> Self {
        Self { registry, pending: VecDeque::new(), executed: Vec::new() }
    }

    /// Submit a test plan
    pub fn submit_plan(&mut self, mut plan: TestPlan) {
        // Insert in priority order (higher priority first)
        let insert_idx = self
            .pending
            .iter()
            .position(|p| {
                p.priority < plan.priority || (p.priority == plan.priority && p.qos < plan.qos)
            })
            .unwrap_or(self.pending.len());

        // Assign plan ID if not set
        if plan.plan_id.is_empty() {
            plan.plan_id = format!("plan-{}", uuid::Uuid::new_v4());
        }

        self.pending.insert(insert_idx, plan);
    }

    /// Get next plan to execute
    #[must_use]
    pub fn next_plan(&mut self) -> Option<TestPlan> {
        self.pending.pop_front()
    }

    /// Execute a test plan
    ///
    /// In a real implementation, this would:
    /// - Schedule tests across workers
    /// - Execute tests in parallel (respecting resource budgets)
    /// - Collect receipts
    /// - Generate summary
    ///
    /// For now, this is a mock implementation.
    #[must_use]
    pub fn execute_plan(&mut self, plan: &TestPlan) -> TestExecutionResult {
        let summary = ExecutionSummary::new();
        let receipts = Vec::new(); // Would be populated by actual test execution

        let result = TestExecutionResult { plan_id: plan.plan_id.clone(), receipts, summary };

        self.executed.push(result.clone());
        result
    }

    /// Suggest minimal sufficient test set for a change
    ///
    /// Given a change (Δ Σ), suggests which tests must run.
    #[must_use]
    pub fn suggest_tests_for_change(&self, changed_modules: &[&str]) -> Vec<&'static TestContract> {
        let mut suggested = Vec::new();

        for contract in self.registry.all() {
            // Check if this test covers any changed module
            for module in changed_modules {
                if contract.covers_module_runtime(module) {
                    suggested.push(contract);
                    break;
                }
            }
        }

        suggested
    }

    /// Suggest additional tests to improve coverage
    ///
    /// Analyzes current coverage and suggests tests to fill gaps.
    #[must_use]
    pub fn suggest_tests_for_coverage(
        &self,
        required_invariants: &[&str],
        current_coverage: &[&str],
    ) -> Vec<&'static TestContract> {
        let mut suggested = Vec::new();

        // Find uncovered invariants
        let uncovered: Vec<&str> = required_invariants
            .iter()
            .filter(|&&inv| !current_coverage.contains(&inv))
            .copied()
            .collect();

        // Suggest tests that cover uncovered invariants
        for contract in self.registry.all() {
            for inv in &uncovered {
                if contract.verifies_invariant(inv) {
                    suggested.push(contract);
                    break;
                }
            }
        }

        suggested
    }

    /// Get pending plan count
    #[must_use]
    pub fn pending_count(&self) -> usize {
        self.pending.len()
    }

    /// Get executed plan count
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn executed_count(&self) -> usize {
        self.executed.len()
    }

    /// Get all executed results
    #[must_use]
    pub fn executed_results(&self) -> &[TestExecutionResult] {
        &self.executed
    }
}

/// Test planning API: helps agents decide what to test
pub struct TestPlanningAPI {
    registry: TestContractRegistry,
}

impl TestPlanningAPI {
    /// Create a new planning API
    #[must_use]
    pub const fn new(registry: TestContractRegistry) -> Self {
        Self { registry }
    }

    /// Get all available tests
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn available_tests(&self) -> &[TestContract] {
        self.registry.all()
    }

    /// Get tests for a specific thermal class
    #[must_use]
    pub fn tests_by_thermal_class(&self, class: &str) -> Vec<&'static TestContract> {
        self.registry
            .all()
            .iter()
            .filter(|c| c.thermal_class().to_string() == class)
            .collect()
    }

    /// Get tests requiring a specific environment
    #[must_use]
    pub fn tests_requiring_environment(&self, env: &str) -> Vec<&'static TestContract> {
        self.registry.tests_requiring_environment(env)
    }

    /// Analyze coverage gap
    ///
    /// Returns modules and invariants that lack coverage.
    #[must_use]
    pub fn coverage_gap<'a>(
        &self,
        required_modules: &[&'a str],
        required_invariants: &[&'a str],
    ) -> CoverageGap<'a> {
        CoverageGap {
            uncovered_modules: self.registry.uncovered_modules(required_modules),
            uncovered_invariants: self.registry.uncovered_invariants(required_invariants),
        }
    }
}

/// Coverage gap analysis
#[derive(Debug, Clone)]
pub struct CoverageGap<'a> {
    /// Modules without test coverage
    pub uncovered_modules: Vec<&'a str>,

    /// Invariants without test coverage
    pub uncovered_invariants: Vec<&'a str>,
}

impl CoverageGap<'_> {
    /// Check if there are any gaps
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn has_gaps(&self) -> bool {
        !self.uncovered_modules.is_empty() || !self.uncovered_invariants.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::contract::TestContract;

    #[test]
    fn test_qos_ordering() {
        assert!(QoSClass::Premium > QoSClass::Standard);
        assert!(QoSClass::Standard > QoSClass::BestEffort);
    }

    #[test]
    fn test_resource_budget() {
        let default_budget = ResourceBudget::default_budget();
        assert_eq!(default_budget.max_cores, 1);
        assert!(!default_budget.allow_network);

        let unlimited = ResourceBudget::unlimited();
        assert_eq!(unlimited.max_cores, usize::MAX);
        assert!(unlimited.allow_network);
    }

    #[test]
    fn test_execution_summary() {
        let summary = ExecutionSummary::new();
        assert_eq!(summary.total_tests, 0);
        assert!(!summary.all_passed()); // Empty set: no tests to pass
    }

    #[test]
    #[allow(clippy::unwrap_used)] // Test code: unwrap is acceptable
    fn test_orchestrator_submission() {
        const CONTRACTS: &[TestContract] = &[
            TestContract::hot_path("test1", &["module1"]),
            TestContract::warm_path("test2", &["module2"], &["inv1"]),
        ];

        let registry = TestContractRegistry::new(CONTRACTS);
        let mut orchestrator = TestOrchestrator::new(registry);

        let plan1 = TestPlan {
            plan_id: "plan1".to_string(),
            contracts: vec!["test1".to_string()],
            requester: "agent1".to_string(),
            priority: 50,
            qos: QoSClass::Standard,
            resource_budget: ResourceBudget::default_budget(),
            metadata: HashMap::new(),
        };

        let plan2 = TestPlan {
            plan_id: "plan2".to_string(),
            contracts: vec!["test2".to_string()],
            requester: "agent2".to_string(),
            priority: 80,
            qos: QoSClass::Premium,
            resource_budget: ResourceBudget::unlimited(),
            metadata: HashMap::new(),
        };

        orchestrator.submit_plan(plan1);
        orchestrator.submit_plan(plan2);

        assert_eq!(orchestrator.pending_count(), 2);

        // Should get plan2 first (higher priority)
        let next = orchestrator.next_plan();
        assert!(next.is_some());
        assert_eq!(next.unwrap().plan_id, "plan2");
    }

    #[test]
    fn test_suggest_tests_for_change() {
        const CONTRACTS: &[TestContract] = &[
            TestContract::hot_path("test1", &["module1"]),
            TestContract::warm_path("test2", &["module2"], &["inv1"]),
            TestContract::cold_path("test3", &["module1", "module3"], &[]),
        ];

        let registry = TestContractRegistry::new(CONTRACTS);
        let orchestrator = TestOrchestrator::new(registry);

        let suggested = orchestrator.suggest_tests_for_change(&["module1"]);
        assert_eq!(suggested.len(), 2); // test1 and test3 cover module1
    }

    #[test]
    fn test_planning_api() {
        const CONTRACTS: &[TestContract] = &[
            TestContract::hot_path("test1", &["module1"]),
            TestContract::warm_path("test2", &["module2"], &["inv1"]),
        ];

        let registry = TestContractRegistry::new(CONTRACTS);
        let api = TestPlanningAPI::new(registry);

        let all_tests = api.available_tests();
        assert_eq!(all_tests.len(), 2);

        let hot_tests = api.tests_by_thermal_class("hot");
        assert_eq!(hot_tests.len(), 1);
    }

    #[test]
    fn test_coverage_gap() {
        const CONTRACTS: &[TestContract] = &[TestContract::hot_path("test1", &["module1"])];

        let registry = TestContractRegistry::new(CONTRACTS);
        let api = TestPlanningAPI::new(registry);

        let gap = api.coverage_gap(
            &["module1", "module2"],      // module2 not covered
            &["τ ≤ 8", "error_recovery"], // error_recovery not covered (hot_path includes τ ≤ 8 and no_panics)
        );

        assert!(gap.has_gaps());
        assert_eq!(gap.uncovered_modules.len(), 1);
        assert_eq!(gap.uncovered_modules[0], "module2");
        assert_eq!(gap.uncovered_invariants.len(), 1);
        assert_eq!(gap.uncovered_invariants[0], "error_recovery");
    }
}
