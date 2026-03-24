//! Performance Benchmarks - Validates system SLOs
//!
//! Measures and validates performance of:
//! - Agent creation and startup
//! - Message throughput
//! - Tool discovery and execution
//! - Plan generation
//! - Consensus operations
//! - Domain balance calculations

#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub const VERSION: &str = "0.1.0";

/// SLO definitions (milliseconds)
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
}
