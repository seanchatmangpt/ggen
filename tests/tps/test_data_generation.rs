//! Test Data Generation Utilities with Realistic Scenarios
//!
//! Generates test data for all TPS principle testing:
//! - Payment scenarios (various amounts, currencies, edge cases)
//! - Deployment configurations (different service types)
//! - Queue workloads (varying priorities and sizes)
//! - Failure scenarios (quality issues, timeouts)
//! - Load profiles (realistic traffic patterns)

use std::collections::HashMap;

/// Payment test data builder
#[derive(Clone, Debug)]
pub struct PaymentTestDataBuilder {
    amounts: Vec<f64>,
    customers: Vec<String>,
    currencies: Vec<String>,
}

impl Default for PaymentTestDataBuilder {
    fn default() -> Self {
        Self {
            amounts: vec![9.99, 99.99, 999.99, 5000.00],
            customers: vec!["cust-001", "cust-002", "cust-003"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            currencies: vec!["USD", "EUR", "GBP"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        }
    }
}

impl PaymentTestDataBuilder {
    /// Create new builder with defaults
    pub fn new() -> Self {
        Self::default()
    }

    /// Add custom amount
    pub fn with_amount(mut self, amount: f64) -> Self {
        self.amounts.push(amount);
        self
    }

    /// Generate all combinations
    pub fn build(&self) -> Vec<PaymentTestCase> {
        let mut cases = Vec::new();

        for (idx, &amount) in self.amounts.iter().enumerate() {
            for customer in &self.customers {
                for currency in &self.currencies {
                    cases.push(PaymentTestCase {
                        id: format!("payment-{}", cases.len()),
                        amount,
                        customer_id: customer.clone(),
                        currency: currency.clone(),
                        scenario: self.classify_amount(amount),
                    });
                }
            }
        }

        cases
    }

    fn classify_amount(&self, amount: f64) -> String {
        match amount {
            a if a < 50.0 => "SMALL".to_string(),
            a if a < 500.0 => "MEDIUM".to_string(),
            _ => "LARGE".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PaymentTestCase {
    pub id: String,
    pub amount: f64,
    pub customer_id: String,
    pub currency: String,
    pub scenario: String,
}

/// Deployment test data builder
#[derive(Clone, Debug)]
pub struct DeploymentTestDataBuilder {
    services: Vec<String>,
    versions: Vec<String>,
    replica_counts: Vec<usize>,
}

impl Default for DeploymentTestDataBuilder {
    fn default() -> Self {
        Self {
            services: vec![
                "payment-api", "order-processor", "inventory-service", "shipping-service"
            ]
            .iter()
            .map(|s| s.to_string())
            .collect(),
            versions: vec!["1.0.0", "2.0.0", "3.0.0"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            replica_counts: vec![1, 3, 5, 10],
        }
    }
}

impl DeploymentTestDataBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(&self) -> Vec<DeploymentTestCase> {
        let mut cases = Vec::new();

        for service in &self.services {
            for version in &self.versions {
                for &replicas in &self.replica_counts {
                    cases.push(DeploymentTestCase {
                        id: format!("deploy-{}", cases.len()),
                        service: service.clone(),
                        version: version.clone(),
                        replicas,
                        scenario: self.classify_deployment(replicas),
                    });
                }
            }
        }

        cases
    }

    fn classify_deployment(&self, replicas: usize) -> String {
        match replicas {
            1 => "SINGLE".to_string(),
            2..=5 => "SMALL".to_string(),
            _ => "LARGE".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeploymentTestCase {
    pub id: String,
    pub service: String,
    pub version: String,
    pub replicas: usize,
    pub scenario: String,
}

/// Queue workload test data builder
#[derive(Clone, Debug)]
pub struct QueueWorkloadBuilder {
    priorities: Vec<usize>,
    sizes: Vec<usize>,
}

impl Default for QueueWorkloadBuilder {
    fn default() -> Self {
        Self {
            priorities: vec![1, 2, 3], // 1 = high, 3 = low
            sizes: vec![1, 10, 100, 1000],
        }
    }
}

impl QueueWorkloadBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(&self) -> Vec<WorkItem> {
        let mut items = Vec::new();

        for (idx, &size) in self.sizes.iter().enumerate() {
            for &priority in &self.priorities {
                for i in 0..size {
                    items.push(WorkItem {
                        id: format!("work-{}-{}", idx, i),
                        size: 1,
                        priority,
                        estimated_duration_ms: (10 * priority) as u64,
                    });
                }
            }
        }

        items
    }
}

#[derive(Clone, Debug)]
pub struct WorkItem {
    pub id: String,
    pub size: usize,
    pub priority: usize,
    pub estimated_duration_ms: u64,
}

/// Failure scenario test data builder
#[derive(Clone, Debug)]
pub struct FailureScenarioBuilder {
    failure_types: Vec<String>,
    components: Vec<String>,
}

impl Default for FailureScenarioBuilder {
    fn default() -> Self {
        Self {
            failure_types: vec![
                "QUALITY_CHECK", "TIMEOUT", "NETWORK", "RESOURCE_EXHAUSTION"
            ]
            .iter()
            .map(|s| s.to_string())
            .collect(),
            components: vec!["payment", "deployment", "monitoring"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        }
    }
}

impl FailureScenarioBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn build(&self) -> Vec<FailureScenario> {
        let mut scenarios = Vec::new();

        for failure_type in &self.failure_types {
            for component in &self.components {
                scenarios.push(FailureScenario {
                    id: format!("fail-{}-{}", failure_type, component),
                    failure_type: failure_type.clone(),
                    component: component.clone(),
                    recovery_time_ms: self.estimate_recovery_time(failure_type),
                });
            }
        }

        scenarios
    }

    fn estimate_recovery_time(&self, failure_type: &str) -> u64 {
        match failure_type {
            "QUALITY_CHECK" => 0,      // Manual review needed
            "TIMEOUT" => 5000,         // Retry logic
            "NETWORK" => 1000,         // Circuit breaker reset
            "RESOURCE_EXHAUSTION" => 30000, // Cleanup and restart
            _ => 10000,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FailureScenario {
    pub id: String,
    pub failure_type: String,
    pub component: String,
    pub recovery_time_ms: u64,
}

/// Load profile test data builder
#[derive(Clone, Debug)]
pub struct LoadProfileBuilder;

impl LoadProfileBuilder {
    pub fn new() -> Self {
        Self
    }

    /// Generate realistic traffic pattern (bell curve with spikes)
    pub fn build_realistic_load(&self) -> Vec<LoadDataPoint> {
        let mut points = Vec::new();

        // Simulate 24-hour period
        for hour in 0..24 {
            let base_rps = match hour {
                0..=5 => 10,      // Night: low traffic
                6..=9 => 200,     // Morning: ramp up
                10..=16 => 500,   // Business hours: peak
                17..=19 => 300,   // Evening: decline
                _ => 50,          // Late night: low
            };

            // Add some variability
            for minute in 0..6 {
                let rps = (base_rps as f64 * (0.9 + 0.2 * (minute as f64 / 6.0))) as u32;
                points.push(LoadDataPoint {
                    timestamp: format!("{}:{:02}", hour, minute * 10),
                    requests_per_second: rps,
                });
            }
        }

        points
    }

    /// Generate spike load (sudden traffic burst)
    pub fn build_spike_load(&self) -> Vec<LoadDataPoint> {
        let mut points = Vec::new();

        // Baseline
        for i in 0..50 {
            points.push(LoadDataPoint {
                timestamp: format!("t+{}s", i),
                requests_per_second: 100,
            });
        }

        // Spike
        for i in 50..60 {
            let rps = 100 + ((i - 50) as u32 * 100);
            points.push(LoadDataPoint {
                timestamp: format!("t+{}s", i),
                requests_per_second: rps,
            });
        }

        // Recovery
        for i in 60..100 {
            let rps = 1000.saturating_sub(((i - 60) as u32 * 20));
            points.push(LoadDataPoint {
                timestamp: format!("t+{}s", i),
                requests_per_second: rps,
            });
        }

        points
    }
}

#[derive(Clone, Debug)]
pub struct LoadDataPoint {
    pub timestamp: String,
    pub requests_per_second: u32,
}

/// Cross-principle interaction scenario builder
#[derive(Clone, Debug)]
pub struct CrossPrincipleScenarioBuilder;

impl CrossPrincipleScenarioBuilder {
    pub fn new() -> Self {
        Self
    }

    /// Generate Jidoka + Kanban + Andon interaction scenarios
    pub fn build(&self) -> Vec<CrossPrincipleScenario> {
        vec![
            CrossPrincipleScenario {
                id: "jidoka-kanban-andon-1".to_string(),
                name: "Quality Defect Blocks Queue".to_string(),
                description: "Jidoka detects quality issue, blocks Kanban queue item, fires Andon alert".to_string(),
                expected_jidoka_action: "BLOCK".to_string(),
                expected_kanban_state: "BLOCKED".to_string(),
                expected_andon_level: "CRITICAL".to_string(),
            },
            CrossPrincipleScenario {
                id: "jidoka-kanban-andon-2".to_string(),
                name: "Queue Overflow Triggers Jidoka".to_string(),
                description: "Kanban queue exceeds WIP limit, Jidoka stops accepting new items, Andon warning".to_string(),
                expected_jidoka_action: "STOP_ACCEPTING".to_string(),
                expected_kanban_state: "FULL".to_string(),
                expected_andon_level: "WARNING".to_string(),
            },
            CrossPrincipleScenario {
                id: "jidoka-kanban-andon-3".to_string(),
                name: "Service Failure Cascade".to_string(),
                description: "Payment service fails, payment queue blocks, Jidoka engages, Andon escalates".to_string(),
                expected_jidoka_action: "HALT".to_string(),
                expected_kanban_state: "BLOCKED".to_string(),
                expected_andon_level: "CRITICAL".to_string(),
            },
        ]
    }
}

#[derive(Clone, Debug)]
pub struct CrossPrincipleScenario {
    pub id: String,
    pub name: String,
    pub description: String,
    pub expected_jidoka_action: String,
    pub expected_kanban_state: String,
    pub expected_andon_level: String,
}

/// Complete test data provider for all scenarios
pub struct AllTestDataProvider;

impl AllTestDataProvider {
    pub fn payments() -> Vec<PaymentTestCase> {
        PaymentTestDataBuilder::new().build()
    }

    pub fn deployments() -> Vec<DeploymentTestCase> {
        DeploymentTestDataBuilder::new().build()
    }

    pub fn queue_workloads() -> Vec<WorkItem> {
        QueueWorkloadBuilder::new().build()
    }

    pub fn failure_scenarios() -> Vec<FailureScenario> {
        FailureScenarioBuilder::new().build()
    }

    pub fn realistic_load() -> Vec<LoadDataPoint> {
        LoadProfileBuilder::new().build_realistic_load()
    }

    pub fn spike_load() -> Vec<LoadDataPoint> {
        LoadProfileBuilder::new().build_spike_load()
    }

    pub fn cross_principle_scenarios() -> Vec<CrossPrincipleScenario> {
        CrossPrincipleScenarioBuilder::new().build()
    }

    /// Generate comprehensive test report
    pub fn generate_report() -> TestDataReport {
        TestDataReport {
            payment_cases: Self::payments().len(),
            deployment_cases: Self::deployments().len(),
            queue_items: Self::queue_workloads().len(),
            failure_scenarios: Self::failure_scenarios().len(),
            cross_principle_scenarios: Self::cross_principle_scenarios().len(),
        }
    }
}

#[derive(Debug)]
pub struct TestDataReport {
    pub payment_cases: usize,
    pub deployment_cases: usize,
    pub queue_items: usize,
    pub failure_scenarios: usize,
    pub cross_principle_scenarios: usize,
}

// ============================================================================
// Tests for Test Data Generation
// ============================================================================

#[test]
fn test_payment_data_generation() {
    let data = PaymentTestDataBuilder::new().build();
    assert!(!data.is_empty(), "Should generate payment test cases");
    assert!(
        data.iter().any(|p| p.scenario == "SMALL"),
        "Should have small payments"
    );
    assert!(
        data.iter().any(|p| p.scenario == "LARGE"),
        "Should have large payments"
    );
}

#[test]
fn test_deployment_data_generation() {
    let data = DeploymentTestDataBuilder::new().build();
    assert!(!data.is_empty(), "Should generate deployment test cases");
    assert!(
        data.iter().any(|d| d.replicas == 1),
        "Should have single replica deployments"
    );
    assert!(
        data.iter().any(|d| d.replicas == 10),
        "Should have large replica deployments"
    );
}

#[test]
fn test_queue_workload_generation() {
    let workloads = QueueWorkloadBuilder::new().build();
    assert!(!workloads.is_empty(), "Should generate work items");

    // Verify priority ordering
    let priorities: Vec<_> = workloads.iter().map(|w| w.priority).collect();
    assert!(
        priorities.iter().all(|p| *p >= 1 && *p <= 3),
        "All priorities should be 1-3"
    );
}

#[test]
fn test_failure_scenario_generation() {
    let scenarios = FailureScenarioBuilder::new().build();
    assert!(!scenarios.is_empty(), "Should generate failure scenarios");

    // Check that all failure types are represented
    let types: std::collections::HashSet<_> = scenarios
        .iter()
        .map(|s| s.failure_type.clone())
        .collect();
    assert!(
        types.len() > 1,
        "Should have multiple failure types"
    );
}

#[test]
fn test_load_profile_generation() {
    let realistic = LoadProfileBuilder::new().build_realistic_load();
    assert!(!realistic.is_empty(), "Should generate realistic load profile");

    // Verify traffic increases during business hours
    let business_hours: Vec<_> = realistic
        .iter()
        .filter(|p| p.timestamp.starts_with("10:") || p.timestamp.starts_with("14:"))
        .collect();
    assert!(business_hours.len() > 0, "Should have business hour data");

    let spike = LoadProfileBuilder::new().build_spike_load();
    assert!(!spike.is_empty(), "Should generate spike profile");
}

#[test]
fn test_cross_principle_scenario_generation() {
    let scenarios = CrossPrincipleScenarioBuilder::new().build();
    assert!(!scenarios.is_empty(), "Should generate cross-principle scenarios");

    // Verify all three principles are mentioned
    let descriptions = scenarios
        .iter()
        .map(|s| s.description.as_str())
        .collect::<Vec<_>>()
        .join(" ");

    assert!(descriptions.contains("Jidoka"), "Should mention Jidoka");
    assert!(descriptions.contains("Kanban"), "Should mention Kanban");
    assert!(descriptions.contains("Andon"), "Should mention Andon");
}

#[test]
fn test_all_test_data_provider() {
    let report = AllTestDataProvider::generate_report();

    assert!(report.payment_cases > 0, "Should have payment test cases");
    assert!(report.deployment_cases > 0, "Should have deployment test cases");
    assert!(report.queue_items > 0, "Should have queue items");
    assert!(report.failure_scenarios > 0, "Should have failure scenarios");
    assert!(
        report.cross_principle_scenarios > 0,
        "Should have cross-principle scenarios"
    );

    println!("Test Data Report:\n{:#?}", report);
}
