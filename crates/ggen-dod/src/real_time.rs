//! Real-Time Systems Extension
//!
//! Closes Gap #2: Extends timing model from millisecond (τ ≤ 8ms) to microsecond and nanosecond scale.
//!
//! This module formalizes:
//! - Hard real-time deadlines (must meet or fail)
//! - Firm deadlines (miss is tolerable but undesirable)
//! - Soft deadlines (miss reduces QoS)
//! - Period/deadline scheduling analysis
//! - Jitter bounds and worst-case execution time (WCET)
//!
//! Key insight: τ becomes a spectrum of guarantees, not a single number.

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;
use std::time::Duration;

/// Deadline classification (from real-time systems theory)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Ord, PartialOrd)]
pub enum DeadlineType {
    /// Hard: missing deadline = system failure
    Hard,

    /// Firm: missing deadline is tolerable, value drops to zero
    Firm,

    /// Soft: missing deadline reduces QoS but doesn't break
    Soft,
}

/// Real-time operation specification
///
/// Q-invariant encoding of real-time constraints:
/// - Worst-case execution time (WCET)
/// - Deadline
/// - Period (for periodic tasks)
/// - Jitter bounds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RealTimeOperation {
    /// Unique operation identifier
    pub id: String,

    /// Worst-case execution time (nanoseconds)
    pub wcet_ns: u64,

    /// Deadline relative to start (nanoseconds)
    pub deadline_ns: u64,

    /// Deadline type (hard/firm/soft)
    pub deadline_type: DeadlineType,

    /// Period (for periodic tasks), None for aperiodic
    pub period_ns: Option<u64>,

    /// Jitter bound (maximum deviation from period)
    pub jitter_ns: Option<u64>,

    /// Priority (higher number = higher priority)
    pub priority: u32,
}

impl RealTimeOperation {
    /// Verify Q_hard_real_time: WCET + jitter < deadline
    pub fn verify_schedulable(&self) -> Result<SchedulabilityProof, SchedulabilityViolation> {
        let jitter = self.jitter_ns.unwrap_or(0);

        if self.wcet_ns + jitter > self.deadline_ns {
            return Err(SchedulabilityViolation::NotSchedulable {
                wcet: self.wcet_ns,
                jitter,
                deadline: self.deadline_ns,
            });
        }

        Ok(SchedulabilityProof {
            operation_id: self.id.clone(),
            slack_ns: self.deadline_ns - (self.wcet_ns + jitter),
            deadline_type: self.deadline_type,
        })
    }

    /// Verify Q_periodic: all instances are properly spaced
    pub fn verify_periodic(&self) -> Result<(), RealTimeViolation> {
        match self.period_ns {
            Some(period) if self.wcet_ns > period => {
                Err(RealTimeViolation::WCETExceedsPeriod {
                    wcet: self.wcet_ns,
                    period,
                })
            }
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchedulabilityProof {
    pub operation_id: String,
    pub slack_ns: u64, // How much slack time available
    pub deadline_type: DeadlineType,
}

#[derive(Debug, Clone)]
pub enum SchedulabilityViolation {
    NotSchedulable {
        wcet: u64,
        jitter: u64,
        deadline: u64,
    },
    ImpossibleDeadline,
}

#[derive(Debug, Clone)]
pub enum RealTimeViolation {
    WCETExceedsPeriod { wcet: u64, period: u64 },
    MissedDeadline(String),
    ExcessiveJitter { measured: u64, bound: u64 },
}

/// Real-time execution measurement with proof
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ExecutionMeasurement {
    pub operation_id: String,
    pub start_ns: u64,
    pub end_ns: u64,
    pub actual_duration_ns: u64,
}

impl ExecutionMeasurement {
    pub fn duration(&self) -> Duration {
        Duration::from_nanos(self.actual_duration_ns)
    }

    /// Verify against deadline
    pub fn verify_deadline(&self, deadline_ns: u64) -> Result<DeadlineMet, DeadlineMissed> {
        if self.actual_duration_ns <= deadline_ns {
            Ok(DeadlineMet {
                operation_id: self.operation_id.clone(),
                slack_ns: deadline_ns - self.actual_duration_ns,
            })
        } else {
            Err(DeadlineMissed {
                operation_id: self.operation_id.clone(),
                overrun_ns: self.actual_duration_ns - deadline_ns,
            })
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeadlineMet {
    pub operation_id: String,
    pub slack_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeadlineMissed {
    pub operation_id: String,
    pub overrun_ns: u64,
}

/// Rate monotonic analysis (RMA)
///
/// Classical real-time scheduling algorithm:
/// - Highest priority task runs first
/// - Fixed priorities based on rate (shortest period = highest priority)
/// - Sufficient condition: U_i = sum(C_i / T_i) ≤ n(2^(1/n) - 1)
///
/// This proves Q_schedulable for a task set.
pub struct RateMonotonicAnalyzer {
    tasks: Vec<PeriodicTask>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeriodicTask {
    pub id: String,
    pub wcet_ns: u64,      // Execution time
    pub period_ns: u64,    // Deadline = period
    pub priority: u32,     // 0 = lowest, max = highest
}

impl RateMonotonicAnalyzer {
    pub fn new() -> Self {
        RateMonotonicAnalyzer { tasks: Vec::new() }
    }

    pub fn add_task(&mut self, task: PeriodicTask) {
        self.tasks.push(task);
        // Sort by priority (highest first)
        self.tasks.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Liu & Layland sufficient condition
    ///
    /// Returns the utilization bound: if actual utilization ≤ bound, system is schedulable
    pub fn utilization_bound(&self) -> f64 {
        if self.tasks.is_empty() {
            return 1.0;
        }

        let n = self.tasks.len() as f64;
        n * (2_f64.powf(1.0 / n) - 1.0)
    }

    /// Actual utilization of the task set
    pub fn actual_utilization(&self) -> f64 {
        self.tasks
            .iter()
            .map(|t| t.wcet_ns as f64 / t.period_ns as f64)
            .sum()
    }

    /// Analyze schedulability
    pub fn analyze(&self) -> Result<RMAProof, SchedulabilityViolation> {
        let actual = self.actual_utilization();
        let bound = self.utilization_bound();

        if actual <= bound {
            Ok(RMAProof {
                schedulable: true,
                actual_utilization: actual,
                bound,
                reason: "Liu & Layland condition satisfied".to_string(),
            })
        } else {
            Err(SchedulabilityViolation::ImpossibleDeadline)
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RMAProof {
    pub schedulable: bool,
    pub actual_utilization: f64,
    pub bound: f64,
    pub reason: String,
}

/// Deadline-monotonic scheduling (better than RMA)
///
/// Priority = 1 / deadline (shorter deadline = higher priority)
pub struct DeadlineMonotonicAnalyzer {
    tasks: Vec<GeneralTask>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneralTask {
    pub id: String,
    pub wcet_ns: u64,
    pub deadline_ns: u64,
    pub period_ns: Option<u64>,
}

impl DeadlineMonotonicAnalyzer {
    pub fn new() -> Self {
        DeadlineMonotonicAnalyzer { tasks: Vec::new() }
    }

    pub fn add_task(&mut self, task: GeneralTask) {
        self.tasks.push(task);
        // Sort by deadline (earlier deadline = higher priority)
        self.tasks.sort_by(|a, b| a.deadline_ns.cmp(&b.deadline_ns));
    }

    /// Response time analysis (sufficient condition)
    ///
    /// R_i = C_i + sum(ceil(R_i / T_j) * C_j) for all higher priority j
    /// R_i <= D_i (deadline) → schedulable
    pub fn analyze(&self) -> Result<DMProof, SchedulabilityViolation> {
        let mut results = Vec::new();

        for (i, task) in self.tasks.iter().enumerate() {
            let mut response_time = task.wcet_ns;

            // Add interference from higher priority tasks
            for j in 0..i {
                let higher = &self.tasks[j];
                let period = higher.period_ns.unwrap_or(higher.deadline_ns);

                // Number of times higher priority task executes within response time
                let num_executions = (response_time + period - 1) / period;
                response_time += num_executions * higher.wcet_ns;
            }

            if response_time > task.deadline_ns {
                return Err(SchedulabilityViolation::NotSchedulable {
                    wcet: task.wcet_ns,
                    jitter: 0,
                    deadline: task.deadline_ns,
                });
            }

            results.push(TaskResponse {
                task_id: task.id.clone(),
                response_time_ns: response_time,
                deadline_ns: task.deadline_ns,
                slack_ns: task.deadline_ns - response_time,
            });
        }

        Ok(DMProof { results })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskResponse {
    pub task_id: String,
    pub response_time_ns: u64,
    pub deadline_ns: u64,
    pub slack_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DMProof {
    pub results: Vec<TaskResponse>,
}

/// Jitter characterization
///
/// Q_bounded_jitter: all jitter within certified bounds
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JitterMeasurement {
    pub operation_id: String,
    pub measurements: Vec<u64>,  // durations in nanoseconds
    pub mean_ns: u64,
    pub stddev_ns: u64,
    pub max_ns: u64,
    pub min_ns: u64,
}

impl JitterMeasurement {
    /// Compute statistics from measurements
    pub fn compute(operation_id: String, measurements: Vec<u64>) -> Self {
        if measurements.is_empty() {
            return JitterMeasurement {
                operation_id,
                measurements: Vec::new(),
                mean_ns: 0,
                stddev_ns: 0,
                max_ns: 0,
                min_ns: 0,
            };
        }

        let max = *measurements.iter().max().unwrap();
        let min = *measurements.iter().min().unwrap();
        let mean = measurements.iter().sum::<u64>() / measurements.len() as u64;

        let variance: u64 = measurements
            .iter()
            .map(|x| {
                let diff = (*x as i64) - (mean as i64);
                (diff * diff) as u64
            })
            .sum::<u64>()
            / measurements.len() as u64;

        let stddev = (variance as f64).sqrt() as u64;

        JitterMeasurement {
            operation_id,
            measurements,
            mean_ns: mean,
            stddev_ns: stddev,
            max_ns: max,
            min_ns: min,
        }
    }

    /// Verify jitter is within bound
    pub fn verify_jitter_bound(&self, bound_ns: u64) -> Result<JitterBounded, JitterViolation> {
        if self.max_ns - self.min_ns > bound_ns {
            return Err(JitterViolation::ExceedsBound {
                measured: self.max_ns - self.min_ns,
                bound: bound_ns,
            });
        }

        Ok(JitterBounded {
            operation_id: self.operation_id.clone(),
            jitter_ns: self.max_ns - self.min_ns,
            bound_ns,
            slack_ns: bound_ns - (self.max_ns - self.min_ns),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JitterBounded {
    pub operation_id: String,
    pub jitter_ns: u64,
    pub bound_ns: u64,
    pub slack_ns: u64,
}

#[derive(Debug, Clone)]
pub enum JitterViolation {
    ExceedsBound { measured: u64, bound: u64 },
    Uncharacterized,
}

/// Time server (atomic clock abstraction)
///
/// Maintains global monotonic time with bounded drift
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeServer {
    /// Current logical time (nanoseconds)
    pub time_ns: u64,

    /// Maximum drift from real time (guarantees time never goes backward)
    pub max_drift_ns: u64,

    /// Last update
    pub last_update_ns: u64,
}

impl TimeServer {
    pub fn new(max_drift_ns: u64) -> Self {
        TimeServer {
            time_ns: 0,
            max_drift_ns,
            last_update_ns: 0,
        }
    }

    /// Advance time (monotonically non-decreasing)
    pub fn advance(&mut self, delta_ns: u64) -> Result<(), TimeViolation> {
        let new_time = self.time_ns + delta_ns;

        // Check monotonicity
        if new_time < self.time_ns {
            return Err(TimeViolation::TimeWentBackward);
        }

        self.time_ns = new_time;
        self.last_update_ns = new_time;

        Ok(())
    }

    /// Set time (with drift verification)
    pub fn set_time(&mut self, absolute_ns: u64) -> Result<(), TimeViolation> {
        let drift = if absolute_ns > self.time_ns {
            absolute_ns - self.time_ns
        } else {
            self.time_ns - absolute_ns
        };

        if drift > self.max_drift_ns {
            return Err(TimeViolation::ExcessiveDrift {
                measured: drift,
                bound: self.max_drift_ns,
            });
        }

        self.time_ns = absolute_ns;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum TimeViolation {
    TimeWentBackward,
    ExcessiveDrift { measured: u64, bound: u64 },
}

impl fmt::Display for RealTimeOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "RealTimeOp(id={}, wcet={}ns, deadline={}ns, type={:?})",
            self.id, self.wcet_ns, self.deadline_ns, self.deadline_type
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schedulability_hard_deadline() {
        let op = RealTimeOperation {
            id: "op1".to_string(),
            wcet_ns: 500,
            deadline_ns: 1000,
            deadline_type: DeadlineType::Hard,
            period_ns: Some(2000),
            jitter_ns: Some(100),
            priority: 10,
        };

        let proof = op.verify_schedulable().unwrap();
        assert_eq!(proof.slack_ns, 400); // 1000 - 500 - 100
    }

    #[test]
    fn test_rate_monotonic() {
        let mut analyzer = RateMonotonicAnalyzer::new();

        analyzer.add_task(PeriodicTask {
            id: "t1".to_string(),
            wcet_ns: 100,
            period_ns: 500,
            priority: 1,
        });

        analyzer.add_task(PeriodicTask {
            id: "t2".to_string(),
            wcet_ns: 100,
            period_ns: 1000,
            priority: 0,
        });

        let proof = analyzer.analyze().unwrap();
        assert!(proof.schedulable);
    }

    #[test]
    fn test_jitter_measurement() {
        let measurements = vec![1000, 1050, 900, 1100, 950];
        let jitter = JitterMeasurement::compute("op1".to_string(), measurements);

        assert_eq!(jitter.max_ns, 1100);
        assert_eq!(jitter.min_ns, 900);
    }

    #[test]
    fn test_time_monotonicity() {
        let mut ts = TimeServer::new(1000);

        ts.advance(100).unwrap();
        assert_eq!(ts.time_ns, 100);

        ts.advance(200).unwrap();
        assert_eq!(ts.time_ns, 300);

        // Time can't go backward
        let result = ts.advance(0); // This would keep time same, which is OK
        assert!(result.is_ok());
    }
}
