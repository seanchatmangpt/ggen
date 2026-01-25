#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

//! # tai-testing: Production Hardening & Resilience Testing Framework
//!
//! Comprehensive testing strategies for production systems including:
//! - **Chaos Engineering**: Deliberately inject failures to test system resilience
//! - **Property-Based Testing**: Verify invariants hold under arbitrary inputs
//! - **Compliance Validation**: Automated compliance checking (FISMA, FedRAMP, SOC 2, HIPAA, PCI-DSS)
//! - **Load Testing**: Determine system limits before production deployment
//!
//! ## Core Principles
//!
//! 1. **Test in Production-Like Environments**: Chaos experiments run in staging/production-like clusters
//! 2. **Observable Failures**: All failures are instrumented with metrics, logs, and traces
//! 3. **Automated Compliance**: Compliance is code, not spreadsheets
//! 4. **Deterministic Resilience**: Know exactly how systems behave under failure
//! 5. **Continuous Verification**: Tests run automatically, failures blocked from production
//!
//! ## Quick Start
//!
//! ### Chaos Engineering
//!
//! ```rust,no_run
//! use tai_testing::chaos::ChaosExperiment;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Create a pod kill experiment
//! let experiment = ChaosExperiment::pod_kill(
//!     "production-cluster",
//!     "payment-service",
//!     3, // kill 3 pods
//! );
//!
//! // Run the experiment and collect metrics
//! let metrics = experiment.execute().await?;
//! println!("Recovery time: {:?}", metrics.recovery_time);
//! # Ok(())
//! # }
//! ```
//!
//! ### Property-Based Testing
//!
//! ```rust,no_run
//! use tai_testing::property::StateInvariant;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Verify circuit breaker invariants
//! let invariant = StateInvariant::circuit_breaker_never_stuck_open();
//! invariant.verify().await?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Compliance Validation
//!
//! ```rust,no_run
//! use tai_testing::compliance::ComplianceFramework;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let framework = ComplianceFramework::fisma();
//! let results = framework.audit().await?;
//! println!("Compliant: {}", results.is_compliant());
//! # Ok(())
//! # }
//! ```
//!
//! ### Load Testing
//!
//! ```rust,no_run
//! use tai_testing::load::LoadTest;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let load_test = LoadTest::new("http://api.example.com")
//!     .with_ramp_up(100, 1000) // 100 to 1000 RPS over duration
//!     .with_duration(std::time::Duration::from_secs(300));
//!
//! let results = load_test.run().await?;
//! println!("P99 latency: {:?}", results.latency_p99);
//! # Ok(())
//! # }
//! ```

pub mod chaos;
pub mod compliance;
pub mod load;
pub mod property;

/// Re-export commonly used types
pub use chaos::{ChaosExperiment, ChaosExperimentType, ExperimentMetrics};
pub use compliance::{ComplianceFramework, ComplianceResult, ComplianceViolation};
pub use load::{LatencyPercentiles, LoadTest, LoadTestResult};
pub use property::{InvariantViolation, StateInvariant};

/// Common error types
#[derive(Debug, thiserror::Error)]
pub enum TestingError {
    /// Chaos experiment failed
    #[error("Chaos experiment failed: {0}")]
    ChaosFailure(String),

    /// Property violation detected
    #[error("Property violation: {0}")]
    PropertyViolation(String),

    /// Compliance check failed
    #[error("Compliance check failed: {0}")]
    ComplianceFailure(String),

    /// Load test failed
    #[error("Load test failed: {0}")]
    LoadTestFailure(String),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

/// Result type for testing operations
pub type Result<T> = std::result::Result<T, TestingError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        // Verify all public types are exported
        let _ = std::any::type_name::<ChaosExperiment>();
        let _ = std::any::type_name::<ComplianceFramework>();
        let _ = std::any::type_name::<LoadTest>();
        let _ = std::any::type_name::<StateInvariant>();
    }
}
