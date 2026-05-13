//! System Health and Validation Framework
//!
//! Real-time monitoring, health checks, and prevention mechanisms
//! integrating FMEA, POKA-YOKE, MURA, and MUDA principles

use super::quality_assurance::*;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// System health status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum HealthStatus {
    /// All checks passing
    Healthy,

    /// Minor issues detected (non-critical)
    Degraded,

    /// Significant issues (affecting performance)
    Warning,

    /// Critical issues (system unreliable)
    Critical,

    /// System failure
    Failed,
}

/// System health report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemHealthReport {
    /// Overall status
    pub status: HealthStatus,

    /// Timestamp
    pub checked_at: String,

    /// Individual health checks
    pub checks: BTreeMap<String, CheckResult>,

    /// Summary metrics
    pub metrics: HealthMetrics,

    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Result of a single health check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    /// Check name
    pub name: String,

    /// Pass or fail
    pub passed: bool,

    /// Status message
    pub message: String,

    /// Severity if failed
    pub severity: Option<String>,

    /// Details
    pub details: BTreeMap<String, String>,
}

/// Health metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthMetrics {
    /// Overall health score (0-100)
    pub health_score: u32,

    /// Checks passed
    pub checks_passed: u32,

    /// Checks failed
    pub checks_failed: u32,

    /// Critical issues
    pub critical_issues: u32,

    /// Warnings
    pub warnings: u32,
}

/// Prevention mechanism registry
#[derive(Debug, Clone)]
pub struct PreventionRegistry {
    /// FMEA analysis
    pub fmea: FMEA,

    /// POKA-YOKE system
    pub poka_yoke: PokaYoke,

    /// MURA standardization
    pub mura: MURA,

    /// MUDA waste elimination
    pub muda: MUDA,
}

/// Validation rule for input sanitization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRule {
    /// Rule identifier
    pub id: String,

    /// What it validates
    pub validates: String,

    /// Validation logic description
    pub logic: String,

    /// Reject invalid input
    pub enforce: bool,

    /// Warning on suspicious input
    pub warn: bool,

    /// Auto-correct if possible
    pub auto_correct: bool,
}

/// Real-time health monitor
pub struct HealthMonitor {
    /// Prevention registry
    pub registry: PreventionRegistry,

    /// Validation rules
    pub rules: Vec<ValidationRule>,

    /// Recent check results
    pub recent_results: Vec<SystemHealthReport>,

    /// Alert thresholds
    pub thresholds: AlertThresholds,
}

/// Alert configuration
#[derive(Debug, Clone)]
pub struct AlertThresholds {
    /// Critical issue threshold
    pub critical_threshold: u32,

    /// Warning threshold
    pub warning_threshold: u32,

    /// Health score degradation threshold
    pub health_score_threshold: u32,
}

impl SystemHealthReport {
    /// Create a new health report
    pub fn new() -> Self {
        Self {
            status: HealthStatus::Healthy,
            checked_at: chrono::Utc::now().to_rfc3339(),
            checks: BTreeMap::new(),
            metrics: HealthMetrics {
                health_score: 100,
                checks_passed: 0,
                checks_failed: 0,
                critical_issues: 0,
                warnings: 0,
            },
            recommendations: Vec::new(),
        }
    }

    /// Add a check result
    pub fn add_check(&mut self, check: CheckResult) {
        if check.passed {
            self.metrics.checks_passed += 1;
        } else {
            self.metrics.checks_failed += 1;

            // Track severity
            if let Some(ref severity) = check.severity {
                if severity.to_lowercase().contains("critical") {
                    self.metrics.critical_issues += 1;
                } else if severity.to_lowercase().contains("warning") {
                    self.metrics.warnings += 1;
                }
            }
        }

        self.checks.insert(check.name.clone(), check);
        self.recalculate_health();
    }

    /// Recalculate overall health status
    fn recalculate_health(&mut self) {
        let total_checks = self.metrics.checks_passed + self.metrics.checks_failed;
        if total_checks == 0 {
            self.metrics.health_score = 100;
        } else {
            self.metrics.health_score =
                ((self.metrics.checks_passed * 100) / total_checks).min(100);
        }

        // Determine status based on issues and health score
        self.status = if self.metrics.critical_issues > 0 {
            HealthStatus::Critical
        } else if self.metrics.warnings > 0 {
            HealthStatus::Warning
        } else if self.metrics.checks_failed > 0 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Healthy
        };
    }

    /// Add a recommendation
    pub fn add_recommendation(&mut self, recommendation: String) {
        self.recommendations.push(recommendation);
    }

    /// Is system healthy enough for operation
    pub fn is_operational(&self) -> bool {
        self.status != HealthStatus::Critical && self.status != HealthStatus::Failed
    }
}

impl Default for SystemHealthReport {
    fn default() -> Self {
        Self::new()
    }
}

impl PreventionRegistry {
    /// Create a new prevention registry
    pub fn new() -> Self {
        Self {
            fmea: FMEA::new("Ontology System".to_string(), 80),
            poka_yoke: PokaYoke::new(),
            mura: MURA::new(),
            muda: MUDA::new(),
        }
    }

    /// Get overall quality status
    pub fn quality_status(&self) -> QualityStatus {
        QualityStatus {
            fmea_high_risks: self.fmea.high_risk_failures().len(),
            poka_yoke_effectiveness: self.poka_yoke.prevention_effectiveness(),
            mura_consistency: self.mura.consistency_score(),
            muda_waste_items: self.muda.metrics.total_waste_items,
            quick_wins_available: self.muda.quick_wins().len(),
        }
    }
}

impl Default for PreventionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for HealthMonitor {
    fn default() -> Self {
        Self::new()
    }
}

impl HealthMonitor {
    /// Create a new health monitor
    pub fn new() -> Self {
        Self {
            registry: PreventionRegistry::new(),
            rules: vec![
                ValidationRule {
                    id: "version-constraint".to_string(),
                    validates: "Version constraints".to_string(),
                    logic: "Verify semver format".to_string(),
                    enforce: true,
                    warn: true,
                    auto_correct: false,
                },
                ValidationRule {
                    id: "namespace-uri".to_string(),
                    validates: "Namespace URIs".to_string(),
                    logic: "Validate URI format".to_string(),
                    enforce: true,
                    warn: true,
                    auto_correct: true,
                },
                ValidationRule {
                    id: "class-name".to_string(),
                    validates: "Class names".to_string(),
                    logic: "Enforce naming conventions".to_string(),
                    enforce: true,
                    warn: false,
                    auto_correct: true,
                },
                ValidationRule {
                    id: "circular-deps".to_string(),
                    validates: "Dependency cycles".to_string(),
                    logic: "Detect circular dependencies".to_string(),
                    enforce: true,
                    warn: true,
                    auto_correct: false,
                },
            ],
            recent_results: Vec::new(),
            thresholds: AlertThresholds {
                critical_threshold: 2,
                warning_threshold: 5,
                health_score_threshold: 80,
            },
        }
    }

    /// Run comprehensive health check
    pub fn comprehensive_health_check(&mut self) -> SystemHealthReport {
        let mut report = SystemHealthReport::new();

        // Check 1: FMEA - Failure mode analysis
        let high_risks = self.registry.fmea.high_risk_failures().len();
        report.add_check(CheckResult {
            name: "FMEA: High-Risk Failure Modes".to_string(),
            passed: high_risks == 0,
            message: format!("Found {} high-risk failure modes", high_risks),
            severity: if high_risks > 0 {
                Some("Critical".to_string())
            } else {
                None
            },
            details: {
                let mut details = BTreeMap::new();
                details.insert("threshold".to_string(), "80".to_string());
                details.insert("high_risk_count".to_string(), high_risks.to_string());
                details
            },
        });

        // Check 2: POKA-YOKE - Mistake prevention
        let prevention_effectiveness = self.registry.poka_yoke.prevention_effectiveness();
        report.add_check(CheckResult {
            name: "POKA-YOKE: Error Prevention".to_string(),
            passed: prevention_effectiveness >= 90.0,
            message: format!("Prevention effectiveness: {:.1}%", prevention_effectiveness),
            severity: if prevention_effectiveness < 80.0 {
                Some("Warning".to_string())
            } else {
                None
            },
            details: {
                let mut details = BTreeMap::new();
                details.insert(
                    "prevented".to_string(),
                    self.registry.poka_yoke.stats.errors_prevented.to_string(),
                );
                details.insert(
                    "detected".to_string(),
                    self.registry.poka_yoke.stats.errors_detected.to_string(),
                );
                details.insert(
                    "false_positives".to_string(),
                    self.registry.poka_yoke.stats.false_positives.to_string(),
                );
                details
            },
        });

        // Check 3: MURA - Consistency standards
        let consistency = self.registry.mura.consistency_score();
        let violations = self.registry.mura.critical_violations().len();

        report.add_check(CheckResult {
            name: "MURA: Process Standardization".to_string(),
            passed: consistency >= 95.0 && violations == 0,
            message: format!(
                "Consistency: {:.1}%, Critical violations: {}",
                consistency, violations
            ),
            severity: if violations > 0 {
                Some("Warning".to_string())
            } else {
                None
            },
            details: {
                let mut details = BTreeMap::new();
                details.insert(
                    "consistency_score".to_string(),
                    format!("{:.1}", consistency),
                );
                details.insert("violations".to_string(), violations.to_string());
                details.insert(
                    "standards".to_string(),
                    self.registry.mura.standards.len().to_string(),
                );
                details
            },
        });

        // Check 4: MUDA - Waste elimination
        let quick_wins = self.registry.muda.quick_wins().len();
        let waste_items = self.registry.muda.metrics.total_waste_items;

        report.add_check(CheckResult {
            name: "MUDA: Waste Elimination".to_string(),
            passed: waste_items == 0,
            message: format!(
                "Total waste items: {}, Quick wins: {}",
                waste_items, quick_wins
            ),
            severity: if waste_items > 5 {
                Some("Warning".to_string())
            } else {
                None
            },
            details: {
                let mut details = BTreeMap::new();
                details.insert("total_waste".to_string(), waste_items.to_string());
                details.insert("quick_wins".to_string(), quick_wins.to_string());
                details.insert(
                    "improvement_potential".to_string(),
                    format!("{:.1}%", self.registry.muda.metrics.improvement_potential),
                );
                details
            },
        });

        // Check 5: Validation rules
        let failed_validations = self.rules.iter().filter(|r| r.enforce).count();

        report.add_check(CheckResult {
            name: "Validation Rules".to_string(),
            passed: failed_validations == 0,
            message: format!("Validation rules enforced: {}", self.rules.len()),
            severity: None,
            details: {
                let mut details = BTreeMap::new();
                details.insert("rules_count".to_string(), self.rules.len().to_string());
                details.insert(
                    "enforced".to_string(),
                    self.rules.iter().filter(|r| r.enforce).count().to_string(),
                );
                details
            },
        });

        // Add recommendations
        if high_risks > 0 {
            report.add_recommendation(
                "Analyze high-risk failure modes and implement preventive measures".to_string(),
            );
        }

        if quick_wins > 0 {
            report.add_recommendation(format!(
                "Implement {} quick-win waste elimination opportunities",
                quick_wins
            ));
        }

        if violations > 0 {
            report.add_recommendation(
                "Address critical standardization violations to ensure consistency".to_string(),
            );
        }

        self.recent_results.push(report.clone());

        // Keep only last 100 results
        if self.recent_results.len() > 100 {
            self.recent_results.remove(0);
        }

        report
    }

    /// Get quality metrics
    pub fn quality_metrics(&self) -> QualityMetrics {
        QualityMetrics {
            prevention_registry: self.registry.quality_status(),
            last_health_check: self.recent_results.last().cloned(),
            trend: self.calculate_trend(),
        }
    }

    /// Calculate health trend
    fn calculate_trend(&self) -> HealthTrend {
        if self.recent_results.len() < 2 {
            return HealthTrend::Stable;
        }

        let recent = &self.recent_results[self.recent_results.len() - 1];
        let previous = &self.recent_results[self.recent_results.len() - 2];

        if recent.metrics.health_score > previous.metrics.health_score {
            HealthTrend::Improving
        } else if recent.metrics.health_score < previous.metrics.health_score {
            HealthTrend::Declining
        } else {
            HealthTrend::Stable
        }
    }

    /// Get health history
    pub fn health_history(&self, limit: usize) -> Vec<&SystemHealthReport> {
        self.recent_results.iter().rev().take(limit).collect()
    }
}

/// Quality status snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityStatus {
    pub fmea_high_risks: usize,
    pub poka_yoke_effectiveness: f32,
    pub mura_consistency: f32,
    pub muda_waste_items: u32,
    pub quick_wins_available: usize,
}

/// Overall quality metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    pub prevention_registry: QualityStatus,
    pub last_health_check: Option<SystemHealthReport>,
    pub trend: HealthTrend,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum HealthTrend {
    Improving,
    Stable,
    Declining,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_health_report_creation() {
        let mut report = SystemHealthReport::new();

        report.add_check(CheckResult {
            name: "test-check".to_string(),
            passed: true,
            message: "Test passed".to_string(),
            severity: None,
            details: BTreeMap::new(),
        });

        assert_eq!(report.metrics.checks_passed, 1);
        assert_eq!(report.status, HealthStatus::Healthy);
        assert!(report.is_operational());
    }

    #[test]
    fn test_health_monitor_comprehensive_check() {
        let mut monitor = HealthMonitor::new();
        let report = monitor.comprehensive_health_check();

        assert!(report.checks.contains_key("FMEA: High-Risk Failure Modes"));
        assert!(report.checks.contains_key("POKA-YOKE: Error Prevention"));
        assert!(report.checks.contains_key("MURA: Process Standardization"));
        assert!(report.checks.contains_key("MUDA: Waste Elimination"));
    }

    #[test]
    fn test_health_trend_calculation() {
        let mut monitor = HealthMonitor::new();

        let _report1 = monitor.comprehensive_health_check();
        let _report2 = monitor.comprehensive_health_check();

        // After two checks, trend should be available
        assert_eq!(monitor.recent_results.len(), 2);
    }
}
