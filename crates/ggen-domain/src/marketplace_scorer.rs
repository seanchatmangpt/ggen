//! Marketplace Scorer - Package Ranking by Performance and Compliance
//!
//! Scores and ranks marketplace packages based on:
//! - SLO adherence (uptime, latency, throughput)
//! - Guard compliance (guard breach rate, failure patterns)
//! - Economic metrics (cost per operation, ROI)
//! - Adoption metrics (usage, growth rate)
//! - Risk metrics (incident rate, rollback frequency)
//!
//! Scores feed into promotion/retirement decisions in the marketplace.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Marketplace package identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PackageId {
    pub name: String,
    pub version: String,
}

impl PackageId {
    pub fn new(name: String, version: String) -> Self {
        Self { name, version }
    }
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.name, self.version)
    }
}

/// SLO metrics for a package
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SLOMetrics {
    /// Uptime percentage (0-100)
    pub uptime_percent: f64,

    /// P99 latency (ms)
    pub p99_latency_ms: f64,

    /// Target P99 latency (ms)
    pub target_p99_latency_ms: f64,

    /// Throughput (ops/sec)
    pub throughput_ops_per_sec: f64,

    /// Target throughput (ops/sec)
    pub target_throughput_ops_per_sec: f64,

    /// Error rate (percentage)
    pub error_rate_percent: f64,

    /// Target error rate (percentage)
    pub target_error_rate_percent: f64,
}

impl SLOMetrics {
    /// Compute SLO compliance score (0-100)
    pub fn compliance_score(&self) -> f64 {
        let uptime_score = (self.uptime_percent / 100.0) * 25.0; // 25 points

        let latency_score = if self.p99_latency_ms <= self.target_p99_latency_ms {
            25.0
        } else {
            let overage = self.p99_latency_ms / self.target_p99_latency_ms;
            (25.0 / overage).min(25.0)
        };

        let throughput_score = if self.throughput_ops_per_sec >= self.target_throughput_ops_per_sec {
            25.0
        } else {
            let shortfall = self.throughput_ops_per_sec / self.target_throughput_ops_per_sec;
            shortfall * 25.0
        };

        let error_score = if self.error_rate_percent <= self.target_error_rate_percent {
            25.0
        } else {
            let excess = self.error_rate_percent / self.target_error_rate_percent;
            (25.0 / excess).min(25.0)
        };

        (uptime_score + latency_score + throughput_score + error_score).min(100.0)
    }
}

/// Guard compliance metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GuardMetrics {
    /// Total guard checks performed
    pub total_checks: u64,

    /// Guard checks that passed
    pub passed_checks: u64,

    /// Guard checks that failed
    pub failed_checks: u64,

    /// Breach rate (0-100)
    pub breach_rate_percent: f64,

    /// Most common guard failure type
    pub primary_failure_type: Option<String>,

    /// Guard types affected by failures
    pub affected_guards: Vec<String>,
}

impl GuardMetrics {
    /// Compute guard compliance score (0-100)
    pub fn compliance_score(&self) -> f64 {
        if self.total_checks == 0 {
            return 50.0; // Unknown
        }

        let pass_rate = (self.passed_checks as f64 / self.total_checks as f64) * 100.0;
        pass_rate.min(100.0)
    }
}

/// Economic metrics for a package
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EconomicMetrics {
    /// Cost per operation (USD)
    pub cost_per_op_usd: f64,

    /// Monthly cost (USD)
    pub monthly_cost_usd: f64,

    /// Revenue or value per operation (USD)
    pub revenue_per_op_usd: f64,

    /// Return on investment (percentage)
    pub roi_percent: f64,

    /// Cost trend (0 = stable, >0 = increasing, <0 = decreasing)
    pub cost_trend: f64,
}

impl EconomicMetrics {
    /// Compute economic efficiency score (0-100)
    /// Higher = more cost-efficient and profitable
    pub fn efficiency_score(&self) -> f64 {
        if self.cost_per_op_usd <= 0.0 {
            return 50.0;
        }

        // ROI component (0-50 points)
        let roi_score = (self.roi_percent / 200.0).min(50.0).max(0.0);

        // Cost efficiency component (0-50 points)
        // Lower cost = higher score (logarithmic scale)
        let cost_score = if self.cost_per_op_usd > 0.0 {
            (50.0 - (self.cost_per_op_usd.log10() + 2.0) * 10.0).min(50.0).max(0.0)
        } else {
            25.0
        };

        roi_score + cost_score
    }
}

/// Adoption and usage metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AdoptionMetrics {
    /// Number of active tenants using this package
    pub active_tenants: u32,

    /// Total operations/invocations this period
    pub total_operations: u64,

    /// Operations trend (growth rate percentage)
    pub growth_rate_percent: f64,

    /// Sector distribution (sector → percentage of usage)
    pub sector_distribution: HashMap<String, f64>,
}

impl AdoptionMetrics {
    /// Compute adoption score (0-100)
    /// Considers growth, breadth of sector adoption
    pub fn adoption_score(&self) -> f64 {
        let tenant_score = ((self.active_tenants as f64) / 100.0).min(50.0); // 50 points

        let growth_score = ((self.growth_rate_percent / 200.0).min(1.0)) * 25.0; // 25 points

        let sector_score = (self.sector_distribution.len() as f64 / 5.0).min(1.0) * 25.0; // 25 points

        (tenant_score + growth_score + sector_score).min(100.0)
    }
}

/// Risk metrics for a package
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RiskMetrics {
    /// Incident count in period
    pub incident_count: u32,

    /// Rollback count in period
    pub rollback_count: u32,

    /// Severity of last incident (0-100)
    pub last_incident_severity: u32,

    /// Time since last rollback (seconds)
    pub seconds_since_last_rollback: u64,

    /// Breaking change risk (0-100)
    pub breaking_change_risk: u32,
}

impl RiskMetrics {
    /// Compute risk score (0-100, 0=safe, 100=dangerous)
    pub fn risk_score(&self) -> f64 {
        let incident_score = (self.incident_count as f64 * 10.0).min(40.0); // 0-40 points

        let rollback_score = (self.rollback_count as f64 * 15.0).min(30.0); // 0-30 points

        let breaking_score = self.breaking_change_risk as f64 * 0.3; // 0-30 points

        (incident_score + rollback_score + breaking_score).min(100.0)
    }
}

/// Comprehensive package score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageScore {
    pub package_id: PackageId,

    /// SLO compliance score (0-100)
    pub slo_score: f64,

    /// Guard compliance score (0-100)
    pub guard_score: f64,

    /// Economic efficiency score (0-100)
    pub economic_score: f64,

    /// Adoption and growth score (0-100)
    pub adoption_score: f64,

    /// Risk assessment (0-100, 0=safe, 100=dangerous)
    pub risk_score: f64,

    /// Composite score (0-100), weighted sum of above
    pub composite_score: f64,

    /// Recommendation (Active, Warning, Deprecated, Quarantined)
    pub recommendation: PackageRecommendation,

    /// Timestamp of scoring
    pub scored_at: u64,
}

/// Package recommendation status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PackageRecommendation {
    /// Actively promoted, high quality
    Promoted,

    /// Stable, meets baseline
    Active,

    /// Issues detected, investigate
    Warning,

    /// Not recommended for new projects
    Deprecated,

    /// Broken/unsafe, do not use
    Quarantined,
}

impl std::fmt::Display for PackageRecommendation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageRecommendation::Promoted => write!(f, "Promoted"),
            PackageRecommendation::Active => write!(f, "Active"),
            PackageRecommendation::Warning => write!(f, "Warning"),
            PackageRecommendation::Deprecated => write!(f, "Deprecated"),
            PackageRecommendation::Quarantined => write!(f, "Quarantined"),
        }
    }
}

/// Marketplace Scorer
#[derive(Debug, Clone)]
pub struct MarketplaceScorer {
    // Weights for composite score (must sum to 1.0)
    slo_weight: f64,
    guard_weight: f64,
    economic_weight: f64,
    adoption_weight: f64,
    risk_weight: f64,

    // Thresholds for recommendations
    promoted_threshold: f64,       // >= 85
    active_threshold: f64,         // >= 60
    deprecated_threshold: f64,     // >= 30
    quarantine_risk_threshold: f64, // risk >= 80

    scores: HashMap<PackageId, PackageScore>,
}

impl MarketplaceScorer {
    pub fn new() -> Self {
        Self {
            slo_weight: 0.25,
            guard_weight: 0.25,
            economic_weight: 0.20,
            adoption_weight: 0.15,
            risk_weight: 0.15,

            promoted_threshold: 85.0,
            active_threshold: 60.0,
            deprecated_threshold: 30.0,
            quarantine_risk_threshold: 80.0,

            scores: HashMap::new(),
        }
    }

    /// Score a package based on all metrics
    pub fn score_package(
        &mut self,
        package_id: PackageId,
        slo: &SLOMetrics,
        guards: &GuardMetrics,
        economic: &EconomicMetrics,
        adoption: &AdoptionMetrics,
        risk: &RiskMetrics,
    ) -> PackageScore {
        let slo_score = slo.compliance_score();
        let guard_score = guards.compliance_score();
        let economic_score = economic.efficiency_score();
        let adoption_score = adoption.adoption_score();
        let risk_score = risk.risk_score();

        // Composite score: weighted average
        let composite_score = (slo_score * self.slo_weight
            + guard_score * self.guard_weight
            + economic_score * self.economic_weight
            + adoption_score * self.adoption_weight
            + (100.0 - risk_score) * self.risk_weight) // Invert risk (lower risk = higher score)
            .min(100.0);

        // Determine recommendation
        let recommendation = if risk_score >= self.quarantine_risk_threshold {
            PackageRecommendation::Quarantined
        } else if composite_score >= self.promoted_threshold {
            PackageRecommendation::Promoted
        } else if composite_score >= self.active_threshold {
            PackageRecommendation::Active
        } else if composite_score >= self.deprecated_threshold {
            PackageRecommendation::Deprecated
        } else {
            PackageRecommendation::Quarantined
        };

        let score = PackageScore {
            package_id: package_id.clone(),
            slo_score,
            guard_score,
            economic_score,
            adoption_score,
            risk_score,
            composite_score,
            recommendation,
            scored_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_millis() as u64)
                .unwrap_or(0),
        };

        self.scores.insert(package_id, score.clone());
        score
    }

    /// Get all scores
    pub fn scores(&self) -> Vec<&PackageScore> {
        self.scores.values().collect()
    }

    /// Get score for a specific package
    pub fn get_score(&self, package_id: &PackageId) -> Option<&PackageScore> {
        self.scores.get(package_id)
    }

    /// Rank packages by composite score (descending)
    pub fn rank_by_composite(&self) -> Vec<&PackageScore> {
        let mut scores: Vec<_> = self.scores.values().collect();
        scores.sort_by(|a, b| b.composite_score.partial_cmp(&a.composite_score).unwrap());
        scores
    }

    /// Get packages to promote (score >= promoted_threshold)
    pub fn candidates_for_promotion(&self) -> Vec<&PackageScore> {
        self.scores
            .values()
            .filter(|s| s.composite_score >= self.promoted_threshold)
            .collect()
    }

    /// Get packages to deprecate (score < deprecated_threshold)
    pub fn candidates_for_deprecation(&self) -> Vec<&PackageScore> {
        self.scores
            .values()
            .filter(|s| s.recommendation == PackageRecommendation::Deprecated)
            .collect()
    }

    /// Get packages to quarantine (risk >= quarantine_threshold)
    pub fn candidates_for_quarantine(&self) -> Vec<&PackageScore> {
        self.scores
            .values()
            .filter(|s| s.recommendation == PackageRecommendation::Quarantined)
            .collect()
    }
}

impl Default for MarketplaceScorer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slo_metrics_compliance() {
        let slo = SLOMetrics {
            uptime_percent: 99.9,
            p99_latency_ms: 50.0,
            target_p99_latency_ms: 100.0,
            throughput_ops_per_sec: 10000.0,
            target_throughput_ops_per_sec: 5000.0,
            error_rate_percent: 0.1,
            target_error_rate_percent: 1.0,
        };

        let score = slo.compliance_score();
        assert!(score > 90.0);
    }

    #[test]
    fn test_guard_metrics_compliance() {
        let guards = GuardMetrics {
            total_checks: 1000,
            passed_checks: 950,
            failed_checks: 50,
            breach_rate_percent: 5.0,
            primary_failure_type: Some("timeout".to_string()),
            affected_guards: vec!["rate_limit".to_string()],
        };

        let score = guards.compliance_score();
        assert!(score > 90.0);
    }

    #[test]
    fn test_economic_efficiency() {
        let econ = EconomicMetrics {
            cost_per_op_usd: 0.01,
            monthly_cost_usd: 1000.0,
            revenue_per_op_usd: 0.05,
            roi_percent: 400.0,
            cost_trend: -0.05,
        };

        let score = econ.efficiency_score();
        assert!(score > 50.0);
    }

    #[test]
    fn test_marketplace_scorer() {
        let mut scorer = MarketplaceScorer::new();
        let pkg = PackageId::new("test-package".to_string(), "1.0.0".to_string());

        let slo = SLOMetrics {
            uptime_percent: 99.9,
            p99_latency_ms: 50.0,
            target_p99_latency_ms: 100.0,
            throughput_ops_per_sec: 10000.0,
            target_throughput_ops_per_sec: 5000.0,
            error_rate_percent: 0.1,
            target_error_rate_percent: 1.0,
        };

        let guards = GuardMetrics {
            total_checks: 1000,
            passed_checks: 950,
            failed_checks: 50,
            breach_rate_percent: 5.0,
            primary_failure_type: None,
            affected_guards: vec![],
        };

        let econ = EconomicMetrics {
            cost_per_op_usd: 0.01,
            monthly_cost_usd: 1000.0,
            revenue_per_op_usd: 0.05,
            roi_percent: 400.0,
            cost_trend: 0.0,
        };

        let adoption = AdoptionMetrics {
            active_tenants: 50,
            total_operations: 1000000,
            growth_rate_percent: 20.0,
            sector_distribution: HashMap::new(),
        };

        let risk = RiskMetrics {
            incident_count: 0,
            rollback_count: 0,
            last_incident_severity: 0,
            seconds_since_last_rollback: 86400,
            breaking_change_risk: 10,
        };

        let score = scorer.score_package(pkg, &slo, &guards, &econ, &adoption, &risk);
        assert!(score.composite_score > 80.0);
        assert_eq!(score.recommendation, PackageRecommendation::Promoted);
    }

    #[test]
    fn test_package_ranking() {
        let mut scorer = MarketplaceScorer::new();

        // Score package 1 (high quality)
        let pkg1 = PackageId::new("pkg1".to_string(), "1.0.0".to_string());
        let _score1 = scorer.score_package(
            pkg1,
            &SLOMetrics {
                uptime_percent: 99.9,
                p99_latency_ms: 50.0,
                target_p99_latency_ms: 100.0,
                throughput_ops_per_sec: 10000.0,
                target_throughput_ops_per_sec: 5000.0,
                error_rate_percent: 0.1,
                target_error_rate_percent: 1.0,
            },
            &GuardMetrics {
                total_checks: 1000,
                passed_checks: 950,
                failed_checks: 50,
                breach_rate_percent: 5.0,
                primary_failure_type: None,
                affected_guards: vec![],
            },
            &EconomicMetrics {
                cost_per_op_usd: 0.01,
                monthly_cost_usd: 1000.0,
                revenue_per_op_usd: 0.05,
                roi_percent: 400.0,
                cost_trend: 0.0,
            },
            &AdoptionMetrics {
                active_tenants: 50,
                total_operations: 1000000,
                growth_rate_percent: 20.0,
                sector_distribution: HashMap::new(),
            },
            &RiskMetrics {
                incident_count: 0,
                rollback_count: 0,
                last_incident_severity: 0,
                seconds_since_last_rollback: 86400,
                breaking_change_risk: 10,
            },
        );

        let ranked = scorer.rank_by_composite();
        assert_eq!(ranked.len(), 1);
        assert_eq!(ranked[0].package_id.name, "pkg1");
    }
}
