//! Phase 6b: Analyze - Finding Generation from Observations
//!
//! Detects drift, SLO breaches, patterns, and opportunities.
//! Transforms Γ(O) into findings F through SPARQL rules and heuristics.

use super::types::{Finding, FindingKind, Observation};
use super::monitor::{MonitorEngine, MetricTrend};
use std::collections::HashMap;

/// Analyze engine: generates findings from observations
pub struct AnalyzeEngine {
    findings: Vec<Finding>,
    slo_config: SLOConfig,
}

/// SLO configuration
#[derive(Debug, Clone)]
pub struct SLOConfig {
    /// Max acceptable P99 ticks per pattern (Chatman constant)
    pub max_ticks_p99: f64,

    /// Max acceptable guard failure rate (%)
    pub max_guard_failure_rate: f64,

    /// Max acceptable average latency (ms)
    pub max_avg_latency_ms: f64,

    /// SLO breach threshold for metric increase (%)
    pub slo_increase_threshold: f64,
}

impl Default for SLOConfig {
    fn default() -> Self {
        Self {
            max_ticks_p99: 8.0,     // Chatman constant
            max_guard_failure_rate: 1.0,
            max_avg_latency_ms: 100.0,
            slo_increase_threshold: 10.0,
        }
    }
}

impl AnalyzeEngine {
    /// Create new analyze engine
    pub fn new(slo_config: SLOConfig) -> Self {
        Self {
            findings: Vec::new(),
            slo_config,
        }
    }

    /// Analyze observations from monitor engine
    pub fn analyze(&mut self, monitor: &MonitorEngine) -> Vec<Finding> {
        self.findings.clear();

        // 1. Detect tick budget violations
        self.detect_tick_budget_violations(monitor);

        // 2. Detect guard failure rates
        self.detect_guard_failures(monitor);

        // 3. Detect SLO breaches
        self.detect_slo_breaches(monitor);

        // 4. Detect increasing trends (drift)
        self.detect_drift(monitor);

        // 5. Detect optimization opportunities
        self.detect_optimization_opportunities(monitor);

        self.findings.clone()
    }

    /// Detect patterns exceeding tick budget
    fn detect_tick_budget_violations(&mut self, monitor: &MonitorEngine) {
        for (metric_name, agg) in monitor.aggregations() {
            if metric_name.contains("pattern") && metric_name.contains("ticks") {
                if agg.p99 > self.slo_config.max_ticks_p99 {
                    let pattern_name = metric_name
                        .strip_prefix("pattern.")
                        .and_then(|s| s.strip_suffix(".ticks"))
                        .unwrap_or(&metric_name)
                        .to_string();

                    let finding = Finding {
                        id: format!("finding-tick-{}", pattern_name),
                        kind: FindingKind::TickBudgetViolation,
                        severity: "High".to_string(),
                        description: format!(
                            "Pattern '{}' exceeds tick budget: P99 = {} ticks (max = {})",
                            pattern_name, agg.p99, self.slo_config.max_ticks_p99
                        ),
                        component: pattern_name,
                        evidence: vec![metric_name.clone()],
                        suggested_action: Some(
                            "Consider breaking pattern into smaller units or optimizing hot path"
                                .to_string(),
                        ),
                        timestamp: get_timestamp(),
                        metadata: HashMap::new(),
                    };

                    self.findings.push(finding);
                }
            }
        }
    }

    /// Detect guards with high failure rates
    fn detect_guard_failures(&mut self, monitor: &MonitorEngine) {
        for (metric_name, agg) in monitor.aggregations() {
            if metric_name.contains("guard") && metric_name.contains("failure_rate") {
                if agg.avg > self.slo_config.max_guard_failure_rate {
                    let guard_id = metric_name
                        .strip_prefix("guard.")
                        .and_then(|s| s.strip_suffix(".failure_rate"))
                        .unwrap_or(&metric_name)
                        .to_string();

                    let finding = Finding {
                        id: format!("finding-guard-{}", guard_id),
                        kind: FindingKind::GuardFailureRate,
                        severity: if agg.avg > 5.0 { "Critical" } else { "High" }.to_string(),
                        description: format!(
                            "Guard '{}' has high failure rate: {:.2}% (threshold = {}%)",
                            guard_id, agg.avg, self.slo_config.max_guard_failure_rate
                        ),
                        component: guard_id,
                        evidence: vec![metric_name.clone()],
                        suggested_action: Some(
                            "Analyze failures, consider relaxing guard or fixing root cause"
                                .to_string(),
                        ),
                        timestamp: get_timestamp(),
                        metadata: HashMap::new(),
                    };

                    self.findings.push(finding);
                }
            }
        }
    }

    /// Detect SLO breaches
    fn detect_slo_breaches(&mut self, monitor: &MonitorEngine) {
        for (metric_name, agg) in monitor.aggregations() {
            if metric_name.contains("latency") {
                if agg.avg > self.slo_config.max_avg_latency_ms {
                    let finding = Finding {
                        id: format!("finding-slo-{}", metric_name),
                        kind: FindingKind::SLOBreach,
                        severity: "High".to_string(),
                        description: format!(
                            "Latency SLO breach: {} (avg={:.1}ms, threshold={}ms)",
                            metric_name, agg.avg, self.slo_config.max_avg_latency_ms
                        ),
                        component: metric_name.clone(),
                        evidence: vec![metric_name.clone()],
                        suggested_action: Some(
                            "Investigate latency causes, may require query optimization or caching"
                                .to_string(),
                        ),
                        timestamp: get_timestamp(),
                        metadata: HashMap::new(),
                    };

                    self.findings.push(finding);
                }
            }
        }
    }

    /// Detect drift (increasing trends)
    fn detect_drift(&mut self, monitor: &MonitorEngine) {
        for (metric_name, agg) in monitor.aggregations() {
            if agg.trend == MetricTrend::Increasing {
                // Check if increase is significant
                if agg.p99 > agg.min * (1.0 + self.slo_config.slo_increase_threshold / 100.0) {
                    let finding = Finding {
                        id: format!("finding-drift-{}", metric_name),
                        kind: FindingKind::DriftDetected,
                        severity: "Medium".to_string(),
                        description: format!(
                            "Metric '{}' shows increasing trend: min={:.1}, p99={:.1}",
                            metric_name, agg.min, agg.p99
                        ),
                        component: metric_name.clone(),
                        evidence: vec![metric_name.clone()],
                        suggested_action: Some(
                            "Investigate why metric is increasing over time; may indicate resource leak or workload growth"
                                .to_string(),
                        ),
                        timestamp: get_timestamp(),
                        metadata: HashMap::new(),
                    };

                    self.findings.push(finding);
                }
            }
        }
    }

    /// Detect optimization opportunities
    fn detect_optimization_opportunities(&mut self, monitor: &MonitorEngine) {
        // Identify patterns with consistently low tick usage
        for (metric_name, agg) in monitor.aggregations() {
            if metric_name.contains("pattern") && metric_name.contains("ticks") {
                if agg.p99 < 2.0 && agg.count >= 10 {
                    let pattern_name = metric_name
                        .strip_prefix("pattern.")
                        .and_then(|s| s.strip_suffix(".ticks"))
                        .unwrap_or(&metric_name)
                        .to_string();

                    let finding = Finding {
                        id: format!("finding-opt-{}", pattern_name),
                        kind: FindingKind::OptimizationOpportunity,
                        severity: "Low".to_string(),
                        description: format!(
                            "Pattern '{}' consistently uses few ticks (P99={:.1}): opportunity to merge with hot path",
                            pattern_name, agg.p99
                        ),
                        component: pattern_name,
                        evidence: vec![metric_name.clone()],
                        suggested_action: Some(
                            "Consider inlining or merging this pattern into hot path"
                                .to_string(),
                        ),
                        timestamp: get_timestamp(),
                        metadata: HashMap::new(),
                    };

                    self.findings.push(finding);
                }
            }
        }
    }

    /// Get all findings
    pub fn findings(&self) -> &[Finding] {
        &self.findings
    }

    /// Get findings by kind
    pub fn findings_by_kind(&self, kind: FindingKind) -> Vec<&Finding> {
        self.findings.iter().filter(|f| f.kind == kind).collect()
    }

    /// Get critical or high severity findings
    pub fn critical_findings(&self) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.severity == "Critical" || f.severity == "High")
            .collect()
    }
}

/// Get current timestamp in milliseconds
fn get_timestamp() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mape_k::monitor::MonitorEngine;

    #[test]
    fn test_tick_budget_finding() {
        let mut monitor = MonitorEngine::new();

        // Add observation with high ticks
        let obs = Observation {
            id: "obs-1".to_string(),
            obs_type: super::super::types::ObservationType::Event,
            timestamp: 1000,
            data: serde_json::json!({
                "pattern": "expensive_pattern",
                "ticks": 12.0
            }),
            source: "test".to_string(),
        };
        monitor.ingest_observation(obs);
        monitor.run_aggregations();

        let mut analyzer = AnalyzeEngine::new(SLOConfig::default());
        let findings = analyzer.analyze(&monitor);

        let violations = findings
            .iter()
            .filter(|f| f.kind == FindingKind::TickBudgetViolation)
            .collect::<Vec<_>>();
        assert!(!violations.is_empty());
    }

    #[test]
    fn test_finding_severity() {
        let mut analyzer = AnalyzeEngine::new(SLOConfig::default());

        let finding = Finding {
            id: "test".to_string(),
            kind: FindingKind::SLOBreach,
            severity: "Critical".to_string(),
            description: "Test".to_string(),
            component: "test".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: 0,
            metadata: HashMap::new(),
        };

        analyzer.findings.push(finding);
        let critical = analyzer.critical_findings();
        assert_eq!(critical.len(), 1);
    }
}
