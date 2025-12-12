//! FMEA (Failure Mode and Effects Analysis) Mitigations
//!
//! Implements automatic detection and recovery for all identified failure modes.
//! Each failure mode has:
//! - Detection mechanism (how we know it failed)
//! - Recovery procedure (automated fix)
//! - Monitoring (tracking failure rates)

use crate::error::{Error, Result};
use crate::rdf::poka_yoke::PackageId;
use chrono::{DateTime, Utc};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{error, warn, info};

/// FMEA failure mode identifier
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum FailureMode {
    /// Signature verification failed
    SignatureVerificationFailed,
    /// Network timeout during download
    NetworkTimeout,
    /// Data corruption detected
    DataCorruption,
    /// RDF store connection failure
    RdfStoreConnectionFailure,
    /// SPARQL query timeout
    SparqlQueryTimeout,
    /// State transition violation
    StateTransitionViolation,
    /// Dependency resolution cycle
    DependencyResolutionCycle,
    /// Checksum mismatch
    ChecksumMismatch,
    /// Invalid RDF triple
    InvalidRdfTriple,
    /// Configuration file missing
    ConfigurationFileMissing,
}

/// Failure severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Low = 1,
    Medium = 5,
    High = 8,
    Critical = 10,
}

/// Failure occurrence probability
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Occurrence {
    Rare = 1,
    Unlikely = 3,
    Moderate = 5,
    Likely = 7,
    VeryLikely = 9,
}

/// Failure detection capability
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Detection {
    AlmostCertain = 1,
    High = 3,
    Moderate = 5,
    Low = 7,
    AlmostImpossible = 9,
}

/// Risk Priority Number (RPN = Severity × Occurrence × Detection)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RiskPriorityNumber(u32);

impl RiskPriorityNumber {
    pub fn calculate(severity: Severity, occurrence: Occurrence, detection: Detection) -> Self {
        Self((severity as u32) * (occurrence as u32) * (detection as u32))
    }

    pub fn value(&self) -> u32 {
        self.0
    }

    pub fn is_high_risk(&self) -> bool {
        self.0 >= 100
    }

    pub fn is_critical_risk(&self) -> bool {
        self.0 >= 200
    }
}

/// FMEA failure record
#[derive(Debug, Clone)]
pub struct FailureRecord {
    pub mode: FailureMode,
    pub occurred_at: DateTime<Utc>,
    pub context: String,
    pub severity: Severity,
    pub recovery_attempted: bool,
    pub recovery_successful: bool,
    pub rpn: RiskPriorityNumber,
}

/// FMEA mitigation engine
pub struct FmeaMitigations {
    /// Failure history for analysis
    failure_history: Arc<RwLock<Vec<FailureRecord>>>,
    /// Failure counters per mode
    failure_counts: Arc<RwLock<HashMap<FailureMode, usize>>>,
    /// RPN thresholds for alerting
    alert_threshold: u32,
}

impl FmeaMitigations {
    /// Create a new FMEA mitigation engine
    pub fn new() -> Self {
        Self {
            failure_history: Arc::new(RwLock::new(Vec::new())),
            failure_counts: Arc::new(RwLock::new(HashMap::new())),
            alert_threshold: 100, // Alert on high risk
        }
    }

    /// Set the RPN alert threshold
    pub fn with_alert_threshold(mut self, threshold: u32) -> Self {
        self.alert_threshold = threshold;
        self
    }

    // ========== Failure Detection ==========

    /// Detect and handle signature verification failure
    pub fn handle_signature_failure(
        &self,
        package_id: &PackageId,
        error: &Error,
    ) -> Result<RecoveryAction> {
        let mode = FailureMode::SignatureVerificationFailed;
        let rpn = RiskPriorityNumber::calculate(
            Severity::Critical,
            Occurrence::Unlikely,
            Detection::AlmostCertain,
        );

        info!(
            package_id = %package_id,
            "Signature verification failed, attempting recovery"
        );

        // Record failure
        self.record_failure(mode.clone(), "Signature verification failed", Severity::Critical, rpn);

        // Recovery: Try to fetch public key and retry
        let recovery = RecoveryAction::RetryWithFallback {
            primary_action: "re-fetch public key and verify".to_string(),
            fallback_action: "use cached version if available".to_string(),
            max_retries: 2,
        };

        Ok(recovery)
    }

    /// Detect and handle network timeout
    pub fn handle_network_timeout(&self, url: &str, timeout_ms: u64) -> Result<RecoveryAction> {
        let mode = FailureMode::NetworkTimeout;
        let rpn = RiskPriorityNumber::calculate(
            Severity::Medium,
            Occurrence::Moderate,
            Detection::AlmostCertain,
        );

        warn!(url = %url, timeout_ms = %timeout_ms, "Network timeout detected");

        self.record_failure(mode.clone(), &format!("Network timeout: {}", url), Severity::Medium, rpn);

        // Recovery: Exponential backoff retry
        let recovery = RecoveryAction::RetryWithBackoff {
            initial_delay_ms: 100,
            max_delay_ms: 5000,
            max_retries: 3,
        };

        Ok(recovery)
    }

    /// Detect and handle data corruption
    pub fn handle_data_corruption(
        &self,
        expected_checksum: &str,
        actual_checksum: &str,
    ) -> Result<RecoveryAction> {
        let mode = FailureMode::DataCorruption;
        let rpn = RiskPriorityNumber::calculate(
            Severity::High,
            Occurrence::Rare,
            Detection::AlmostCertain,
        );

        error!(
            expected = %expected_checksum,
            actual = %actual_checksum,
            "Data corruption detected"
        );

        self.record_failure(
            mode.clone(),
            "Checksum mismatch indicates data corruption",
            Severity::High,
            rpn,
        );

        // Recovery: Re-download and verify
        let recovery = RecoveryAction::RedownloadAndVerify {
            max_attempts: 3,
            use_alternative_source: true,
        };

        Ok(recovery)
    }

    /// Detect and handle RDF store connection failure
    pub fn handle_rdf_store_failure(&self, error: &Error) -> Result<RecoveryAction> {
        let mode = FailureMode::RdfStoreConnectionFailure;
        let rpn = RiskPriorityNumber::calculate(
            Severity::Critical,
            Occurrence::Unlikely,
            Detection::AlmostCertain,
        );

        error!("RDF store connection failure: {}", error);

        self.record_failure(
            mode.clone(),
            &format!("RDF store failure: {}", error),
            Severity::Critical,
            rpn,
        );

        // Recovery: Reconnect with exponential backoff
        let recovery = RecoveryAction::ReconnectStore {
            initial_delay_ms: 500,
            max_delay_ms: 10000,
            max_retries: 5,
        };

        Ok(recovery)
    }

    /// Detect and handle SPARQL query timeout
    pub fn handle_sparql_timeout(&self, query: &str, timeout_ms: u64) -> Result<RecoveryAction> {
        let mode = FailureMode::SparqlQueryTimeout;
        let rpn = RiskPriorityNumber::calculate(
            Severity::Medium,
            Occurrence::Moderate,
            Detection::AlmostCertain,
        );

        warn!(timeout_ms = %timeout_ms, "SPARQL query timeout");

        self.record_failure(
            mode.clone(),
            &format!("SPARQL timeout after {}ms", timeout_ms),
            Severity::Medium,
            rpn,
        );

        // Recovery: Simplify query or use cached result
        let recovery = RecoveryAction::OptimizeQuery {
            use_cache_if_available: true,
            simplify_query: true,
            increase_timeout: true,
        };

        Ok(recovery)
    }

    /// Detect and handle state transition violation
    pub fn handle_state_violation(
        &self,
        from_state: &str,
        to_state: &str,
    ) -> Result<RecoveryAction> {
        let mode = FailureMode::StateTransitionViolation;
        let rpn = RiskPriorityNumber::calculate(
            Severity::High,
            Occurrence::Rare,
            Detection::AlmostCertain,
        );

        error!(from = %from_state, to = %to_state, "Invalid state transition");

        self.record_failure(
            mode.clone(),
            &format!("Invalid transition: {} -> {}", from_state, to_state),
            Severity::High,
            rpn,
        );

        // Recovery: Reject transition and log
        let recovery = RecoveryAction::RejectAndLog {
            reason: format!("State transition {} -> {} is not allowed", from_state, to_state),
        };

        Ok(recovery)
    }

    /// Detect and handle dependency resolution cycle
    pub fn handle_dependency_cycle(&self, cycle: &[PackageId]) -> Result<RecoveryAction> {
        let mode = FailureMode::DependencyResolutionCycle;
        let rpn = RiskPriorityNumber::calculate(
            Severity::High,
            Occurrence::Rare,
            Detection::AlmostCertain,
        );

        error!("Dependency cycle detected: {:?}", cycle);

        self.record_failure(
            mode.clone(),
            &format!("Dependency cycle: {:?}", cycle),
            Severity::High,
            rpn,
        );

        // Recovery: Break cycle by removing optional dependencies
        let recovery = RecoveryAction::BreakDependencyCycle {
            remove_optional_deps: true,
            suggest_alternatives: true,
        };

        Ok(recovery)
    }

    // ========== Failure Recording and Monitoring ==========

    /// Record a failure for monitoring and analysis
    fn record_failure(
        &self,
        mode: FailureMode,
        context: &str,
        severity: Severity,
        rpn: RiskPriorityNumber,
    ) {
        let record = FailureRecord {
            mode: mode.clone(),
            occurred_at: Utc::now(),
            context: context.to_string(),
            severity,
            recovery_attempted: false,
            recovery_successful: false,
            rpn,
        };

        // Update counters
        {
            let mut counts = self.failure_counts.write();
            *counts.entry(mode.clone()).or_insert(0) += 1;
        }

        // Store record
        {
            let mut history = self.failure_history.write();
            history.push(record);

            // Keep only recent history (last 1000 failures)
            if history.len() > 1000 {
                history.drain(0..100); // Remove oldest 100
            }
        }

        // Check if we should alert
        if rpn.value() >= self.alert_threshold {
            warn!(
                rpn = %rpn.value(),
                mode = ?mode,
                "High-risk failure detected"
            );
        }
    }

    /// Get failure statistics
    pub fn get_statistics(&self) -> FailureStatistics {
        let history = self.failure_history.read();
        let counts = self.failure_counts.read();

        let total_failures = history.len();
        let unique_modes = counts.len();

        let high_risk_count = history
            .iter()
            .filter(|r| r.rpn.is_high_risk())
            .count();

        let critical_risk_count = history
            .iter()
            .filter(|r| r.rpn.is_critical_risk())
            .count();

        let mode_counts: HashMap<FailureMode, usize> = counts.clone();

        FailureStatistics {
            total_failures,
            unique_modes,
            high_risk_count,
            critical_risk_count,
            mode_counts,
        }
    }

    /// Get failure rate for a specific mode
    pub fn get_failure_rate(&self, mode: &FailureMode) -> f64 {
        let counts = self.failure_counts.read();
        let mode_count = counts.get(mode).copied().unwrap_or(0);

        let total = counts.values().sum::<usize>();
        if total == 0 {
            0.0
        } else {
            (mode_count as f64) / (total as f64)
        }
    }

    /// Check if failure rate exceeds threshold
    pub fn is_failure_rate_high(&self, mode: &FailureMode, threshold: f64) -> bool {
        self.get_failure_rate(mode) > threshold
    }
}

impl Default for FmeaMitigations {
    fn default() -> Self {
        Self::new()
    }
}

/// Recovery actions for different failure modes
#[derive(Debug, Clone)]
pub enum RecoveryAction {
    /// Retry with fallback strategy
    RetryWithFallback {
        primary_action: String,
        fallback_action: String,
        max_retries: u32,
    },
    /// Retry with exponential backoff
    RetryWithBackoff {
        initial_delay_ms: u64,
        max_delay_ms: u64,
        max_retries: u32,
    },
    /// Re-download and verify
    RedownloadAndVerify {
        max_attempts: u32,
        use_alternative_source: bool,
    },
    /// Reconnect to store
    ReconnectStore {
        initial_delay_ms: u64,
        max_delay_ms: u64,
        max_retries: u32,
    },
    /// Optimize query
    OptimizeQuery {
        use_cache_if_available: bool,
        simplify_query: bool,
        increase_timeout: bool,
    },
    /// Reject and log
    RejectAndLog {
        reason: String,
    },
    /// Break dependency cycle
    BreakDependencyCycle {
        remove_optional_deps: bool,
        suggest_alternatives: bool,
    },
}

/// Failure statistics
#[derive(Debug, Clone)]
pub struct FailureStatistics {
    pub total_failures: usize,
    pub unique_modes: usize,
    pub high_risk_count: usize,
    pub critical_risk_count: usize,
    pub mode_counts: HashMap<FailureMode, usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rpn_calculation() {
        let rpn = RiskPriorityNumber::calculate(
            Severity::Critical,
            Occurrence::Moderate,
            Detection::Low,
        );
        assert_eq!(rpn.value(), 10 * 5 * 7);
        assert!(rpn.is_high_risk());
        assert!(rpn.is_critical_risk());
    }

    #[test]
    fn test_failure_recording() {
        let fmea = FmeaMitigations::new();

        fmea.record_failure(
            FailureMode::SignatureVerificationFailed,
            "test context",
            Severity::Critical,
            RiskPriorityNumber(350),
        );

        let stats = fmea.get_statistics();
        assert_eq!(stats.total_failures, 1);
        assert_eq!(stats.unique_modes, 1);
        assert_eq!(stats.critical_risk_count, 1);
    }

    #[test]
    fn test_failure_rate_calculation() {
        let fmea = FmeaMitigations::new();

        // Record multiple failures
        for _ in 0..7 {
            fmea.record_failure(
                FailureMode::NetworkTimeout,
                "timeout",
                Severity::Medium,
                RiskPriorityNumber(75),
            );
        }

        for _ in 0..3 {
            fmea.record_failure(
                FailureMode::DataCorruption,
                "corruption",
                Severity::High,
                RiskPriorityNumber(80),
            );
        }

        let network_rate = fmea.get_failure_rate(&FailureMode::NetworkTimeout);
        assert_eq!(network_rate, 0.7); // 7 out of 10

        let corruption_rate = fmea.get_failure_rate(&FailureMode::DataCorruption);
        assert_eq!(corruption_rate, 0.3); // 3 out of 10
    }

    #[test]
    fn test_high_failure_rate_detection() {
        let fmea = FmeaMitigations::new();

        for _ in 0..8 {
            fmea.record_failure(
                FailureMode::SparqlQueryTimeout,
                "timeout",
                Severity::Medium,
                RiskPriorityNumber(75),
            );
        }

        for _ in 0..2 {
            fmea.record_failure(
                FailureMode::NetworkTimeout,
                "timeout",
                Severity::Medium,
                RiskPriorityNumber(75),
            );
        }

        assert!(fmea.is_failure_rate_high(&FailureMode::SparqlQueryTimeout, 0.5));
        assert!(!fmea.is_failure_rate_high(&FailureMode::NetworkTimeout, 0.5));
    }

    #[test]
    fn test_recovery_actions() -> Result<()> {
        let fmea = FmeaMitigations::new();

        // Test signature failure recovery
        let pkg_id = PackageId::new("test-pkg")?;
        let error = Error::SignatureVerificationFailed {
            reason: "invalid signature".to_string(),
        };
        let recovery = fmea.handle_signature_failure(&pkg_id, &error)?;
        match recovery {
            RecoveryAction::RetryWithFallback { .. } => {},
            _ => panic!("Expected RetryWithFallback"),
        }

        // Test network timeout recovery
        let recovery = fmea.handle_network_timeout("https://example.com", 5000)?;
        match recovery {
            RecoveryAction::RetryWithBackoff { .. } => {},
            _ => panic!("Expected RetryWithBackoff"),
        }

        // Test data corruption recovery
        let recovery = fmea.handle_data_corruption("abc123", "xyz789")?;
        match recovery {
            RecoveryAction::RedownloadAndVerify { .. } => {},
            _ => panic!("Expected RedownloadAndVerify"),
        }

        Ok(())
    }
}
