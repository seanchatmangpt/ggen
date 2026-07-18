//! Performance Benchmarks - Validates system SLOs
//!
//! Measures and validates performance of:
//! - Agent creation and startup
//! - Message throughput
//! - Tool discovery and execution
//! - Plan generation
//! - Consensus operations
//! - Domain balance calculations
//! - Load testing for realistic scenarios

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub const VERSION: &str = "0.1.0";

/// SLO definitions (milliseconds) - OSIRIS Life Domains
pub mod osiris_slos {
    pub const AGENT_INIT_MS: u64 = 100;
    pub const GOAL_DISCOVERY_MS: u64 = 50;
    pub const CONSENSUS_VOTING_6_AGENT_MS: u64 = 200;
    pub const LEARNING_OUTCOME_RECORD_MS: u64 = 20;
    pub const METRIC_CALC_MS: u64 = 50;
}

/// SLO definitions (milliseconds) - A2A Tool Use Integration
pub mod a2a_tool_use_slos {
    pub const TOOL_DISCOVERY_20_TOOLS_MS: u64 = 100;
    pub const PLAN_GENERATION_5_STEP_MS: u64 = 200;
    pub const TOOL_EXECUTION_MS: u64 = 300;
    pub const RESULT_ANALYSIS_MS: u64 = 100;
}

/// SLO definitions (milliseconds) - A2A Agent Lifecycle
pub mod a2a_lifecycle_slos {
    pub const STATE_TRANSITION_MS: u64 = 5;
    pub const MESSAGE_ROUTING_MS: u64 = 10;
    pub const TASK_SCHEDULING_MS: u64 = 20;
    pub const AGENT_CREATION_MS: u64 = 50;
}

/// Legacy SLO definitions for backward compatibility
pub mod slos {
    pub const AGENT_CREATION_MS: u64 = 100;
    pub const AGENT_STARTUP_MS: u64 = 500;
    pub const MESSAGE_THROUGHPUT_MIN: u64 = 10000; // msgs/sec
    pub const TOOL_DISCOVERY_MS: u64 = 200;
    pub const PLAN_GENERATION_10_STEPS_MS: u64 = 1000;
    pub const TOOL_EXECUTION_MS: u64 = 100;
    pub const CONSENSUS_3_AGENT_MS: u64 = 2000;
    pub const DOMAIN_BALANCE_MS: u64 = 500;
}

/// Performance measurement result
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    /// Test name
    pub name: String,
    /// Time taken in milliseconds
    pub elapsed_ms: u64,
    /// SLO target in milliseconds
    pub slo_ms: u64,
    /// Whether SLO was met
    pub passed: bool,
    /// Optional message throughput (msgs/sec)
    pub throughput: Option<u64>,
}

impl BenchmarkResult {
    /// Create new benchmark result
    pub fn new(name: String, elapsed_ms: u64, slo_ms: u64) -> Self {
        let passed = elapsed_ms <= slo_ms;
        Self {
            name,
            elapsed_ms,
            slo_ms,
            passed,
            throughput: None,
        }
    }

    /// Set throughput for message tests
    pub fn with_throughput(mut self, throughput: u64) -> Self {
        self.throughput = Some(throughput);
        self
    }

    /// Get slack percentage (negative = SLO breach)
    pub fn slack_percentage(&self) -> f64 {
        ((self.slo_ms as f64 - self.elapsed_ms as f64) / self.slo_ms as f64) * 100.0
    }

    /// Status string
    pub fn status(&self) -> &str {
        if self.passed { "PASS" } else { "FAIL" }
    }
}

/// Load test result with statistical analysis
#[derive(Debug, Clone)]
pub struct LoadTestResult {
    /// Test name
    pub name: String,
    /// Total operations executed
    pub total_ops: u64,
    /// Total time in milliseconds
    pub total_ms: u64,
    /// Minimum operation time
    pub min_ms: u64,
    /// Maximum operation time
    pub max_ms: u64,
    /// Average operation time
    pub avg_ms: f64,
    /// p99 (99th percentile) latency
    pub p99_ms: f64,
    /// Peak memory usage in MB
    pub peak_memory_mb: u64,
    /// Throughput (ops/sec)
    pub throughput_ops_sec: f64,
    /// Whether test passed all SLOs
    pub passed: bool,
}

impl LoadTestResult {
    /// Calculate standard deviation
    pub fn calculate_stddev(&self, measurements: &[u64]) -> f64 {
        if measurements.is_empty() {
            return 0.0;
        }
        let mean = self.avg_ms;
        let variance = measurements
            .iter()
            .map(|&m| (m as f64 - mean).powi(2))
            .sum::<f64>()
            / measurements.len() as f64;
        variance.sqrt()
    }

    /// Get stress level (0.0 = no stress, 1.0 = at SLO limit)
    pub fn stress_level(&self, slo_ms: u64) -> f64 {
        (self.avg_ms / slo_ms as f64).min(1.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_benchmark_result_creation() {
        let result = BenchmarkResult::new("test".to_string(), 50, 100);
        assert!(result.passed);
    }

    #[test]
    fn test_benchmark_slack_calculation() {
        let result = BenchmarkResult::new("test".to_string(), 75, 100);
        assert_eq!(result.slack_percentage(), 25.0);
    }

    #[test]
    fn test_slo_constants() {
        assert_eq!(slos::AGENT_CREATION_MS, 100);
        assert_eq!(slos::AGENT_STARTUP_MS, 500);
        assert_eq!(slos::MESSAGE_THROUGHPUT_MIN, 10000);
    }

    #[test]
    fn test_osiris_slos_defined() {
        assert!(osiris_slos::AGENT_INIT_MS > 0);
        assert!(osiris_slos::GOAL_DISCOVERY_MS > 0);
        assert!(osiris_slos::CONSENSUS_VOTING_6_AGENT_MS > 0);
        assert!(osiris_slos::LEARNING_OUTCOME_RECORD_MS > 0);
        assert!(osiris_slos::METRIC_CALC_MS > 0);
    }

    #[test]
    fn test_a2a_tool_use_slos_defined() {
        assert!(a2a_tool_use_slos::TOOL_DISCOVERY_20_TOOLS_MS > 0);
        assert!(a2a_tool_use_slos::PLAN_GENERATION_5_STEP_MS > 0);
        assert!(a2a_tool_use_slos::TOOL_EXECUTION_MS > 0);
        assert!(a2a_tool_use_slos::RESULT_ANALYSIS_MS > 0);
    }

    #[test]
    fn test_a2a_lifecycle_slos_defined() {
        assert!(a2a_lifecycle_slos::STATE_TRANSITION_MS > 0);
        assert!(a2a_lifecycle_slos::MESSAGE_ROUTING_MS > 0);
        assert!(a2a_lifecycle_slos::TASK_SCHEDULING_MS > 0);
        assert!(a2a_lifecycle_slos::AGENT_CREATION_MS > 0);
    }

    #[test]
    fn test_load_test_result_stress_level() {
        let result = LoadTestResult {
            name: "test".to_string(),
            total_ops: 1000,
            total_ms: 1000,
            min_ms: 1,
            max_ms: 2,
            avg_ms: 1.5,
            p99_ms: 1.9,
            peak_memory_mb: 10,
            throughput_ops_sec: 1000.0,
            passed: true,
        };
        assert!(result.stress_level(100) < 1.0);
    }

    #[test]
    fn test_benchmark_result_negative_slack() {
        let result = BenchmarkResult::new("test".to_string(), 150, 100);
        assert!(!result.passed);
        assert!(result.slack_percentage() < 0.0);
    }
}
