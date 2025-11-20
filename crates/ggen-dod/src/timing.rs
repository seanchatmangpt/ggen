//! Timing system: Enforces Chatman Constant (τ ≤ 8ms) guarantees
//!
//! All hot paths that declare τ constraints are verified to stay within bounds.
//! Violations are fatal and prevent execution.

use crate::error::DoDResult;
use serde::{Deserialize, Serialize};
// Removed unused imports: Duration, Instant

/// Timing measurement for a kernel operation
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TimingMeasurement {
    /// When measurement started
    start_time: Option<std::time::SystemTime>,
    /// When measurement ended
    end_time: Option<std::time::SystemTime>,
    /// Elapsed milliseconds
    elapsed_ms: u64,
}

impl TimingMeasurement {
    /// Create a new timing measurement (started now)
    pub fn new() -> Self {
        Self {
            start_time: Some(std::time::SystemTime::now()),
            end_time: None,
            elapsed_ms: 0,
        }
    }

    /// Mark measurement as finished with elapsed time
    pub fn finished(mut self, elapsed_ms: u64) -> Self {
        self.elapsed_ms = elapsed_ms;
        self.end_time = Some(std::time::SystemTime::now());
        self
    }

    /// Get elapsed time in milliseconds
    pub fn elapsed_ms(&self) -> u64 {
        self.elapsed_ms
    }

    /// Check if within timing constraint
    pub fn within_constraint(&self, max_ms: u64) -> bool {
        self.elapsed_ms <= max_ms
    }
}

impl Default for TimingMeasurement {
    fn default() -> Self {
        Self::new()
    }
}

/// A timing guarantee (τ ≤ X ms)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TimingGuarantee {
    /// Maximum time in milliseconds
    max_ms: u64,
    /// Whether this constraint is enforced strictly
    strict: bool,
}

impl TimingGuarantee {
    /// Create a new timing guarantee
    pub fn new(max_ms: u64) -> Self {
        Self {
            max_ms,
            strict: true,
        }
    }

    /// Create a soft constraint (warning, not error)
    pub fn soft(max_ms: u64) -> Self {
        Self {
            max_ms,
            strict: false,
        }
    }

    /// Get maximum time in milliseconds
    pub fn max_ms(&self) -> u64 {
        self.max_ms
    }

    /// Is this a strict constraint?
    pub fn is_strict(&self) -> bool {
        self.strict
    }

    /// Check if measurement violates this guarantee
    pub fn check(&self, measurement: &TimingMeasurement) -> DoDResult<()> {
        if measurement.elapsed_ms > self.max_ms {
            if self.strict {
                return Err(crate::error::DoDError::TimingViolation {
                    expected: self.max_ms,
                    actual: measurement.elapsed_ms,
                });
            }
        }
        Ok(())
    }
}

/// The Chatman Constant: τ ≤ 8 milliseconds
pub const CHATMAN_CONSTANT_MS: u64 = 8;

/// Kernel timing constraint (strict)
pub fn kernel_timing_constraint() -> TimingGuarantee {
    TimingGuarantee::new(CHATMAN_CONSTANT_MS)
}

/// Decision closure check constraint (tighter)
pub fn decision_closure_timing() -> TimingGuarantee {
    TimingGuarantee::new(2)
}

/// Schema validation timing (moderate)
pub fn schema_validation_timing() -> TimingGuarantee {
    TimingGuarantee::soft(5)
}

/// Timing enforcer for execution
#[derive(Clone)]
pub struct TimingEnforcer {
    /// Constraints being enforced
    constraints: Vec<(String, TimingGuarantee)>,
    /// Measurements collected
    measurements: Vec<(String, TimingMeasurement)>,
}

impl TimingEnforcer {
    /// Create a new timing enforcer
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            measurements: Vec::new(),
        }
    }

    /// Register a timing constraint
    pub fn with_constraint(mut self, name: impl Into<String>, constraint: TimingGuarantee) -> Self {
        self.constraints.push((name.into(), constraint));
        self
    }

    /// Register a measurement
    pub fn record_measurement(
        mut self, name: impl Into<String>, measurement: TimingMeasurement,
    ) -> Self {
        self.measurements.push((name.into(), measurement));
        self
    }

    /// Check all measurements against constraints
    pub fn verify(&self) -> DoDResult<()> {
        for (_name, measurement) in &self.measurements {
            for (_constraint_name, constraint) in &self.constraints {
                constraint.check(measurement)?;
            }
        }
        Ok(())
    }

    /// Get all measurements
    pub fn measurements(&self) -> &[(String, TimingMeasurement)] {
        &self.measurements
    }

    /// Compute statistics
    pub fn stats(&self) -> TimingStats {
        if self.measurements.is_empty() {
            return TimingStats::default();
        }

        let times: Vec<u64> = self
            .measurements
            .iter()
            .map(|(_, m)| m.elapsed_ms)
            .collect();
        let sum: u64 = times.iter().sum();
        let count = times.len() as u64;
        let mean = sum / count;

        let mut sorted = times.clone();
        sorted.sort_unstable();
        let median = sorted[sorted.len() / 2];
        let min = *sorted.first().unwrap_or(&0);
        let max = *sorted.last().unwrap_or(&0);

        // Calculate standard deviation
        let variance: u64 = times
            .iter()
            .map(|t| {
                let diff = if *t > mean { *t - mean } else { mean - *t };
                diff * diff
            })
            .sum::<u64>()
            / count;
        let stddev = (variance as f64).sqrt() as u64;

        TimingStats {
            count: count as usize,
            min,
            max,
            mean,
            median,
            stddev,
            p99: sorted[(sorted.len() * 99) / 100],
        }
    }
}

impl Default for TimingEnforcer {
    fn default() -> Self {
        Self::new()
    }
}

/// Timing statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TimingStats {
    /// Number of measurements
    pub count: usize,
    /// Minimum time in ms
    pub min: u64,
    /// Maximum time in ms
    pub max: u64,
    /// Mean time in ms
    pub mean: u64,
    /// Median time in ms
    pub median: u64,
    /// Standard deviation in ms
    pub stddev: u64,
    /// 99th percentile in ms
    pub p99: u64,
}

impl TimingStats {
    /// Is this within the Chatman Constant?
    pub fn within_chatman_constant(&self) -> bool {
        self.p99 <= CHATMAN_CONSTANT_MS
    }

    /// Confidence interval (95%)
    pub fn confidence_interval_95(&self) -> (u64, u64) {
        let margin = (1.96 * self.stddev as f64) as u64;
        let lower = self.mean.saturating_sub(margin);
        let upper = self.mean + margin;
        (lower, upper)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timing_measurement() {
        let measurement = TimingMeasurement::new().finished(5);
        assert_eq!(measurement.elapsed_ms(), 5);
    }

    #[test]
    fn test_timing_guarantee_check() -> DoDResult<()> {
        let guarantee = TimingGuarantee::new(10);
        let measurement = TimingMeasurement::new().finished(8);
        guarantee.check(&measurement)?;
        Ok(())
    }

    #[test]
    fn test_timing_violation() {
        let guarantee = TimingGuarantee::new(5);
        let measurement = TimingMeasurement::new().finished(10);
        assert!(guarantee.check(&measurement).is_err());
    }

    #[test]
    fn test_chatman_constant() {
        assert_eq!(CHATMAN_CONSTANT_MS, 8);
    }

    #[test]
    fn test_timing_enforcer() -> DoDResult<()> {
        let enforcer = TimingEnforcer::new()
            .with_constraint("kernel", kernel_timing_constraint())
            .record_measurement("test", TimingMeasurement::new().finished(5));

        enforcer.verify()?;
        Ok(())
    }

    #[test]
    fn test_timing_stats() {
        let enforcer = TimingEnforcer::new()
            .record_measurement("m1", TimingMeasurement::new().finished(5))
            .record_measurement("m2", TimingMeasurement::new().finished(10))
            .record_measurement("m3", TimingMeasurement::new().finished(7));

        let stats = enforcer.stats();
        assert_eq!(stats.count, 3);
        assert_eq!(stats.min, 5);
        assert_eq!(stats.max, 10);
    }
}
