//! CLI commands for Quality Assurance operations
//!
//! Provides command-line interfaces to:
//! - Run comprehensive health checks
//! - Analyze FMEA failure modes
//! - Conduct GEMBA walks
//! - Check ANDON alerts
//! - Export quality metrics
//! - Generate quality reports

use super::{Andon, GembaWalk, HealthMonitor};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Quality Assurance CLI Result Type
pub type QaCliResult<T> = Result<T, QaCliError>;

/// Quality Assurance CLI Errors
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QaCliError {
    /// Health check failed
    HealthCheckFailed(String),
    /// Invalid configuration
    InvalidConfiguration(String),
    /// File IO error
    FileIoError(String),
    /// Export error
    ExportError(String),
    /// Serialization error
    SerializationError(String),
}

impl std::fmt::Display for QaCliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HealthCheckFailed(msg) => write!(f, "Health check failed: {}", msg),
            Self::InvalidConfiguration(msg) => write!(f, "Invalid configuration: {}", msg),
            Self::FileIoError(msg) => write!(f, "File IO error: {}", msg),
            Self::ExportError(msg) => write!(f, "Export error: {}", msg),
            Self::SerializationError(msg) => write!(f, "Serialization error: {}", msg),
        }
    }
}

impl std::error::Error for QaCliError {}

/// Health check command output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckOutput {
    /// Overall health status
    pub status: String,
    /// Health score (0-100)
    pub score: u32,
    /// Individual check results
    pub checks: BTreeMap<String, CheckOutput>,
    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Individual health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckOutput {
    /// Check passed
    pub passed: bool,
    /// Status message
    pub message: String,
    /// Severity if failed
    pub severity: Option<String>,
}

/// FMEA analysis output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaAnalysisOutput {
    /// Total failure modes
    pub total_failures: usize,
    /// High-risk failures (RPN > threshold)
    pub high_risk_failures: Vec<HighRiskFailure>,
    /// Risk priority distribution
    pub rpn_distribution: BTreeMap<String, usize>,
    /// Recommendations
    pub recommendations: Vec<String>,
}

/// High-risk failure representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HighRiskFailure {
    /// Failure description
    pub description: String,
    /// Risk Priority Number
    pub rpn: u32,
    /// Severity (1-10)
    pub severity: u32,
    /// Occurrence (1-10)
    pub occurrence: u32,
    /// Detection difficulty (1-10)
    pub detection: u32,
    /// Preventive actions
    pub preventive_actions: Vec<String>,
}

/// ANDON alert status report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonStatusOutput {
    /// Total alerts (all time)
    pub total_alerts: u32,
    /// Currently active alerts
    pub active_alerts: u32,
    /// Alerts by severity
    pub alerts_by_severity: BTreeMap<String, u32>,
    /// Most common triggers
    pub top_triggers: Vec<(String, u32)>,
    /// Average resolution time (ms)
    pub avg_resolution_time_ms: u32,
    /// Critical/Emergency alert list (if any)
    pub critical_alerts: Vec<AlertSummary>,
}

/// Individual alert summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertSummary {
    /// Alert ID
    pub id: String,
    /// Alert severity
    pub severity: String,
    /// Alert trigger
    pub trigger: String,
    /// Status
    pub status: String,
}

/// GEMBA walk session output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaWalkOutput {
    /// Total walks conducted
    pub total_walks: u32,
    /// Total observations
    pub total_observations: u32,
    /// Problems identified
    pub problems_identified: u32,
    /// Problems resolved
    pub problems_resolved: u32,
    /// Improvements implemented
    pub improvements_implemented: u32,
    /// Average problems per walk
    pub avg_problems_per_walk: f32,
    /// Overall effectiveness (%)
    pub effectiveness: f32,
    /// Unresolved problems
    pub unresolved_problems: Vec<ProblemSummary>,
    /// Blocked improvements
    pub blocked_improvements: Vec<ImprovementSummary>,
}

/// Problem area summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProblemSummary {
    /// Problem ID
    pub id: String,
    /// Location
    pub location: String,
    /// Description
    pub description: String,
    /// Severity
    pub severity: String,
    /// Status
    pub status: String,
}

/// Improvement action summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementSummary {
    /// Action ID
    pub id: String,
    /// Improvement description
    pub improvement: String,
    /// Owner
    pub owner: String,
    /// Progress (%)
    pub progress: u32,
    /// Status
    pub status: String,
    /// Blockers
    pub blockers: Vec<String>,
}

/// Quality metrics export
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetricsExport {
    /// Generated timestamp
    pub generated_at: String,
    /// FMEA metrics
    pub fmea: FmeaMetrics,
    /// POKA-YOKE metrics
    pub poka_yoke: PokaYokeMetrics,
    /// MURA metrics
    pub mura_metrics: MuraMetrics,
    /// MUDA metrics
    pub muda_metrics: MudaMetrics,
    /// Overall system health
    pub health: SystemHealthMetrics,
}

/// FMEA metrics summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaMetrics {
    /// Total failure modes analyzed
    pub total_failure_modes: usize,
    /// High-risk count (RPN > threshold)
    pub high_risk_count: usize,
    /// Average RPN
    pub average_rpn: f32,
    /// Highest RPN (most critical)
    pub highest_rpn: u32,
}

/// POKA-YOKE metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PokaYokeMetrics {
    /// Total prevention rules
    pub total_rules: usize,
    /// Prevention effectiveness (%)
    pub effectiveness: f32,
    /// Errors prevented
    pub errors_prevented: u32,
    /// Errors detected
    pub errors_detected: u32,
    /// False positives
    pub false_positives: u32,
}

/// MURA metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MuraMetrics {
    /// Total standards defined
    pub total_standards: usize,
    /// Consistency score (0-100)
    pub consistency_score: f32,
    /// Critical violations
    pub critical_violations: usize,
    /// Total violations
    pub total_violations: usize,
}

/// MUDA metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MudaMetrics {
    /// Total waste items identified
    pub total_waste_items: u32,
    /// Quick-win opportunities
    pub quick_wins: usize,
    /// Improvement potential (%)
    pub improvement_potential: f32,
}

/// System health metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemHealthMetrics {
    /// Overall health status
    pub status: String,
    /// Health score (0-100)
    pub score: u32,
    /// Checks passed
    pub checks_passed: u32,
    /// Checks failed
    pub checks_failed: u32,
    /// Trend (Improving, Stable, Declining)
    pub trend: String,
}

/// Quality Assurance CLI interface
pub struct QaCliManager {
    /// Health monitor instance
    pub monitor: HealthMonitor,
    /// Alert system instance
    pub andon: Andon,
    /// Operational audit system
    pub gemba: GembaWalk,
}

impl QaCliManager {
    /// Create new QA CLI manager
    pub fn new() -> Self {
        Self {
            monitor: HealthMonitor::new(),
            andon: Andon::new(100),
            gemba: GembaWalk::new(),
        }
    }

    /// Execute health check command
    pub fn health_check(&mut self) -> QaCliResult<HealthCheckOutput> {
        let report = self.monitor.comprehensive_health_check();

        let mut checks = BTreeMap::new();
        for (name, result) in report.checks {
            checks.insert(
                name,
                CheckOutput {
                    passed: result.passed,
                    message: result.message,
                    severity: result.severity,
                },
            );
        }

        Ok(HealthCheckOutput {
            status: format!("{:?}", report.status),
            score: report.metrics.health_score,
            checks,
            recommendations: report.recommendations,
        })
    }

    /// Execute FMEA analysis command
    pub fn fmea_analyze(&self) -> QaCliResult<FmeaAnalysisOutput> {
        let high_risks = self.monitor.registry.fmea.high_risk_failures();

        let mut high_risk_failures = Vec::new();
        for failure in high_risks {
            high_risk_failures.push(HighRiskFailure {
                description: failure.name.clone(),
                rpn: failure.rpn,
                severity: failure.severity,
                occurrence: failure.occurrence,
                detection: failure.detection,
                preventive_actions: failure.preventive_actions.clone(),
            });
        }

        // Distribution: Low (RPN < 100), Medium (100-300), High (> 300)
        let mut distribution = BTreeMap::new();
        distribution.insert("Low (0-99)".to_string(), 0);
        distribution.insert("Medium (100-299)".to_string(), 0);
        distribution.insert("High (300+)".to_string(), 0);

        for failure in &self.monitor.registry.fmea.failure_modes {
            if failure.rpn < 100 {
                if let Some(count) = distribution.get_mut("Low (0-99)") {
                    *count += 1;
                }
            } else if failure.rpn < 300 {
                if let Some(count) = distribution.get_mut("Medium (100-299)") {
                    *count += 1;
                }
            } else {
                if let Some(count) = distribution.get_mut("High (300+)") {
                    *count += 1;
                }
            }
        }

        let mut recommendations = Vec::new();
        if !high_risk_failures.is_empty() {
            recommendations
                .push("Review and implement preventive actions for high-risk failures".to_string());
        }
        if self.monitor.registry.fmea.failure_modes.len() > 10 {
            recommendations
                .push("Consider prioritizing failures with highest RPN scores".to_string());
        }

        Ok(FmeaAnalysisOutput {
            total_failures: self.monitor.registry.fmea.failure_modes.len(),
            high_risk_failures,
            rpn_distribution: distribution,
            recommendations,
        })
    }

    /// Execute ANDON status command
    pub fn andon_status(&self) -> QaCliResult<AndonStatusOutput> {
        let critical_alerts = self.andon.critical_alerts();

        let mut critical_list = Vec::new();
        for alert in critical_alerts {
            critical_list.push(AlertSummary {
                id: alert.id.clone(),
                severity: format!("{:?}", alert.severity),
                trigger: alert.trigger.clone(),
                status: format!("{:?}", alert.status),
            });
        }

        Ok(AndonStatusOutput {
            total_alerts: self.andon.stats.total_alerts,
            active_alerts: self.andon.stats.active_alerts,
            alerts_by_severity: self.andon.stats.alerts_by_severity.clone(),
            top_triggers: self.andon.stats.top_triggers.clone(),
            avg_resolution_time_ms: self.andon.stats.avg_resolution_time_ms,
            critical_alerts: critical_list,
        })
    }

    /// Execute GEMBA walk status command
    pub fn gemba_status(&self) -> QaCliResult<GembaWalkOutput> {
        let unresolved = self.gemba.unresolved_problems();
        let blocked = self.gemba.blocked_improvements();

        let mut unresolved_list = Vec::new();
        for problem in unresolved {
            unresolved_list.push(ProblemSummary {
                id: problem.id.clone(),
                location: problem.location.clone(),
                description: problem.description.clone(),
                severity: format!("{:?}", problem.severity),
                status: format!("{:?}", problem.status),
            });
        }

        let mut blocked_list = Vec::new();
        for action in blocked {
            blocked_list.push(ImprovementSummary {
                id: action.id.clone(),
                improvement: action.improvement.clone(),
                owner: action.owner.clone(),
                progress: action.progress,
                status: format!("{:?}", action.status),
                blockers: action.blockers.clone(),
            });
        }

        Ok(GembaWalkOutput {
            total_walks: self.gemba.stats.total_walks,
            total_observations: self.gemba.stats.total_observations,
            problems_identified: self.gemba.stats.problems_identified,
            problems_resolved: self.gemba.stats.problems_resolved,
            improvements_implemented: self.gemba.stats.improvements_implemented,
            avg_problems_per_walk: self.gemba.stats.avg_problems_per_walk,
            effectiveness: self.gemba.stats.effectiveness,
            unresolved_problems: unresolved_list,
            blocked_improvements: blocked_list,
        })
    }

    /// Export comprehensive quality metrics
    pub fn export_metrics(&self) -> QaCliResult<QualityMetricsExport> {
        let _quality_status = self.monitor.registry.quality_status();

        // Calculate FMEA metrics
        let fmea_failures = &self.monitor.registry.fmea.failure_modes;
        let average_rpn = if !fmea_failures.is_empty() {
            fmea_failures.iter().map(|f| f.rpn as f32).sum::<f32>() / fmea_failures.len() as f32
        } else {
            0.0
        };

        let highest_rpn = fmea_failures.iter().map(|f| f.rpn).max().unwrap_or(0);

        let quality_metrics = self.monitor.quality_metrics();
        let trend_str = format!("{:?}", quality_metrics.trend);

        Ok(QualityMetricsExport {
            generated_at: chrono::Utc::now().to_rfc3339(),
            fmea: FmeaMetrics {
                total_failure_modes: fmea_failures.len(),
                high_risk_count: self.monitor.registry.fmea.high_risk_failures().len(),
                average_rpn,
                highest_rpn,
            },
            poka_yoke: PokaYokeMetrics {
                total_rules: self.monitor.registry.poka_yoke.prevention_rules.len(),
                effectiveness: self.monitor.registry.poka_yoke.prevention_effectiveness(),
                errors_prevented: self.monitor.registry.poka_yoke.stats.errors_prevented,
                errors_detected: self.monitor.registry.poka_yoke.stats.errors_detected,
                false_positives: self.monitor.registry.poka_yoke.stats.false_positives,
            },
            mura_metrics: MuraMetrics {
                total_standards: self.monitor.registry.mura.standards.len(),
                consistency_score: self.monitor.registry.mura.consistency_score(),
                critical_violations: self.monitor.registry.mura.critical_violations().len(),
                total_violations: self.monitor.registry.mura.violations.len(),
            },
            muda_metrics: MudaMetrics {
                total_waste_items: self.monitor.registry.muda.metrics.total_waste_items,
                quick_wins: self.monitor.registry.muda.quick_wins().len(),
                improvement_potential: self.monitor.registry.muda.metrics.improvement_potential,
            },
            health: SystemHealthMetrics {
                status: quality_metrics
                    .last_health_check
                    .as_ref()
                    .map(|r| format!("{:?}", r.status))
                    .unwrap_or_else(|| "Unknown".to_string()),
                score: quality_metrics
                    .last_health_check
                    .as_ref()
                    .map(|r| r.metrics.health_score)
                    .unwrap_or(0),
                checks_passed: quality_metrics
                    .last_health_check
                    .as_ref()
                    .map(|r| r.metrics.checks_passed)
                    .unwrap_or(0),
                checks_failed: quality_metrics
                    .last_health_check
                    .as_ref()
                    .map(|r| r.metrics.checks_failed)
                    .unwrap_or(0),
                trend: trend_str,
            },
        })
    }

    /// Generate formatted text report
    pub fn generate_report(&mut self) -> QaCliResult<String> {
        let health = self.health_check()?;
        let fmea = self.fmea_analyze()?;
        let andon = self.andon_status()?;
        let gemba = self.gemba_status()?;
        let _metrics = self.export_metrics()?;

        let mut report = String::new();
        report.push_str("╔═══════════════════════════════════════════════════════════╗\n");
        report.push_str("║  Quality Assurance Comprehensive Report                   ║\n");
        report.push_str("╠═══════════════════════════════════════════════════════════╣\n");

        // Health Check Section
        report.push_str(&format!("║ System Health Status: {:45} ║\n", health.status));
        report.push_str(&format!("║ Health Score: {:50}/100 ║\n", health.score));
        report.push('\n');

        // FMEA Section
        report.push_str("║ FMEA (Failure Mode Analysis):                            ║\n");
        report.push_str(&format!(
            "║   Total Failure Modes: {:42} ║\n",
            fmea.total_failures
        ));
        report.push_str(&format!(
            "║   High-Risk Failures (RPN > 80): {:34} ║\n",
            fmea.high_risk_failures.len()
        ));
        report.push('\n');

        // ANDON Section
        report.push_str("║ ANDON Alert System:                                      ║\n");
        report.push_str(&format!(
            "║   Active Alerts: {:47} ║\n",
            andon.active_alerts
        ));
        report.push_str(&format!(
            "║   Total Alerts (all time): {:39} ║\n",
            andon.total_alerts
        ));
        report.push('\n');

        // GEMBA Section
        report.push_str("║ GEMBA Walk (Operational Audit):                          ║\n");
        report.push_str(&format!("║   Total Walks: {:48} ║\n", gemba.total_walks));
        report.push_str(&format!(
            "║   Problems Identified: {:42} ║\n",
            gemba.problems_identified
        ));
        report.push_str(&format!(
            "║   Effectiveness: {:45}% ║\n",
            gemba.effectiveness as u32
        ));
        report.push('\n');

        // Recommendations
        if !health.recommendations.is_empty() {
            report.push_str("║ Recommendations:                                         ║\n");
            for rec in &health.recommendations {
                let truncated = if rec.len() > 53 {
                    format!("{}...", &rec[..50])
                } else {
                    rec.clone()
                };
                report.push_str(&format!("║   - {:52} ║\n", truncated));
            }
        }

        report.push_str("╚═══════════════════════════════════════════════════════════╝\n");

        Ok(report)
    }
}

impl Default for QaCliManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_qa_cli_manager_creation() {
        let manager = QaCliManager::new();
        assert_eq!(manager.andon.stats.total_alerts, 0);
        assert_eq!(manager.gemba.stats.total_walks, 0);
    }

    #[test]
    fn test_health_check_command() {
        let mut manager = QaCliManager::new();
        let result = manager.health_check();
        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(output.checks.len() >= 5);
        assert!(output.score <= 100);
    }

    #[test]
    fn test_fmea_analyze_command() {
        let manager = QaCliManager::new();
        let result = manager.fmea_analyze();
        assert!(result.is_ok());

        let output = result.unwrap();
        assert_eq!(output.total_failures, 0); // Empty system
        assert!(output.rpn_distribution.len() == 3); // Low, Medium, High
    }

    #[test]
    fn test_andon_status_command() {
        let manager = QaCliManager::new();
        let result = manager.andon_status();
        assert!(result.is_ok());

        let output = result.unwrap();
        assert_eq!(output.total_alerts, 0);
        assert_eq!(output.active_alerts, 0);
    }

    #[test]
    fn test_gemba_status_command() {
        let manager = QaCliManager::new();
        let result = manager.gemba_status();
        assert!(result.is_ok());

        let output = result.unwrap();
        assert_eq!(output.total_walks, 0);
        assert_eq!(output.problems_identified, 0);
    }

    #[test]
    fn test_metrics_export() {
        let manager = QaCliManager::new();
        let result = manager.export_metrics();
        assert!(result.is_ok());

        let metrics = result.unwrap();
        assert!(!metrics.generated_at.is_empty());
        assert_eq!(metrics.fmea.total_failure_modes, 0);
    }

    #[test]
    fn test_generate_report() {
        let mut manager = QaCliManager::new();
        let result = manager.generate_report();
        assert!(result.is_ok());

        let report = result.unwrap();
        assert!(report.contains("Quality Assurance Comprehensive Report"));
        assert!(report.contains("FMEA"));
        assert!(report.contains("ANDON"));
        assert!(report.contains("GEMBA"));
    }
}
