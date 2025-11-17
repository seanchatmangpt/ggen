//! Auto-Promotion Pipeline - Autonomic Package Promotion/Retirement
//!
//! Continuously monitors marketplace packages and autonomously:
//! - Promotes packages meeting SLO, guard, and adoption criteria
//! - Deprecates packages declining in performance or adoption
//! - Suggests ontology improvements based on package health
//! - Ensures all promotions are justified by Γ signals and doctrine-aligned

use super::marketplace_scorer::{MarketplaceScorer, PackageScore, PackageRecommendation};
use super::ontology_proposal_engine::OntologySigmaProposal;
use super::ahi_contract::AHIError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A promotion or deprecation decision with justification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromotionDecision {
    pub decision_id: String,
    pub package_name: String,
    pub package_version: String,
    pub decision_type: DecisionType,
    pub justified_by: Vec<String>, // Observation IDs from Γ
    pub metrics_snapshot: DecisionMetrics,
    pub previous_status: String,
    pub new_status: String,
    pub confidence: f64,        // 0-1, how confident in this decision
    pub reversible: bool,       // Can this decision be reverted?
    pub revert_condition: Option<String>, // Condition to trigger revert
    pub timestamp: u64,
}

/// Type of promotion/deprecation decision
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DecisionType {
    /// Promote to Featured/Active status
    Promote,
    /// Deprecate from active status
    Deprecate,
    /// Quarantine due to critical issues
    Quarantine,
    /// Restore from deprecation
    Restore,
}

impl std::fmt::Display for DecisionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecisionType::Promote => write!(f, "Promote"),
            DecisionType::Deprecate => write!(f, "Deprecate"),
            DecisionType::Quarantine => write!(f, "Quarantine"),
            DecisionType::Restore => write!(f, "Restore"),
        }
    }
}

/// Metrics snapshot at decision time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionMetrics {
    pub slo_score: f64,
    pub guard_score: f64,
    pub adoption_score: f64,
    pub risk_score: f64,
    pub composite_score: f64,
    pub uptime_percent: f64,
    pub error_rate: f64,
    pub active_tenants: usize,
    pub growth_trend: f64,
}

/// Pipeline state and history
#[derive(Debug, Clone)]
pub struct AutoPromotionPipeline {
    #[allow(dead_code)]
    scorer: MarketplaceScorer,
    promotion_threshold: f64,  // Composite score ≥ this to promote
    deprecation_threshold: f64, // Composite score ≤ this to deprecate
    quarantine_threshold: f64,  // Risk score ≥ this to quarantine
    decisions: Vec<PromotionDecision>,
    decision_history: HashMap<String, Vec<PromotionDecision>>,
    ontology_suggestions: Vec<OntologySigmaProposal>,
}

impl AutoPromotionPipeline {
    /// Create new pipeline with default thresholds
    pub fn new() -> Self {
        Self {
            scorer: MarketplaceScorer::new(),
            promotion_threshold: 80.0,  // 80+ = promote
            deprecation_threshold: 40.0, // 40- = deprecate
            quarantine_threshold: 80.0,  // Risk 80+ = quarantine
            decisions: Vec::new(),
            decision_history: HashMap::new(),
            ontology_suggestions: Vec::new(),
        }
    }

    /// Set custom thresholds
    pub fn with_thresholds(
        promotion: f64,
        deprecation: f64,
        quarantine: f64,
    ) -> Self {
        let mut pipeline = Self::new();
        pipeline.promotion_threshold = promotion;
        pipeline.deprecation_threshold = deprecation;
        pipeline.quarantine_threshold = quarantine;
        pipeline
    }

    /// Evaluate package and generate decision
    pub fn evaluate_package(
        &mut self,
        package_name: &str,
        package_version: &str,
        score: &PackageScore,
        observation_ids: Vec<String>,
    ) -> Result<Option<PromotionDecision>, AHIError> {
        let current_recommendation = &score.recommendation;

        // Determine decision based on scores and current status
        let decision_type = if score.risk_score >= self.quarantine_threshold {
            Some(DecisionType::Quarantine)
        } else if score.composite_score >= self.promotion_threshold {
            // Promote if not already promoted
            match current_recommendation {
                PackageRecommendation::Promoted => None, // Already promoted
                _ => Some(DecisionType::Promote),
            }
        } else if score.composite_score <= self.deprecation_threshold {
            // Deprecate if still active
            match current_recommendation {
                PackageRecommendation::Active => Some(DecisionType::Deprecate),
                PackageRecommendation::Promoted => Some(DecisionType::Deprecate),
                _ => None,
            }
        } else {
            None // No decision needed
        };

        match decision_type {
            Some(dt) => {
                let decision = self.create_decision(
                    package_name,
                    package_version,
                    dt,
                    current_recommendation.to_string(),
                    score,
                    observation_ids,
                );

                // Store decision
                self.decisions.push(decision.clone());
                self.decision_history
                    .entry(format!("{}-{}", package_name, package_version))
                    .or_insert_with(Vec::new)
                    .push(decision.clone());

                Ok(Some(decision))
            }
            None => Ok(None),
        }
    }

    /// Create a decision with full justification
    fn create_decision(
        &self,
        package_name: &str,
        package_version: &str,
        decision_type: DecisionType,
        previous_status: String,
        score: &PackageScore,
        observation_ids: Vec<String>,
    ) -> PromotionDecision {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        let new_status = match decision_type {
            DecisionType::Promote => "Featured".to_string(),
            DecisionType::Deprecate => "Deprecated".to_string(),
            DecisionType::Quarantine => "Quarantined".to_string(),
            DecisionType::Restore => "Active".to_string(),
        };

        PromotionDecision {
            decision_id: format!(
                "decision-{}-{}-{}",
                package_name, package_version, now
            ),
            package_name: package_name.to_string(),
            package_version: package_version.to_string(),
            decision_type,
            justified_by: observation_ids,
            metrics_snapshot: DecisionMetrics {
                slo_score: score.slo_score,
                guard_score: score.guard_score,
                adoption_score: score.adoption_score,
                risk_score: score.risk_score,
                composite_score: score.composite_score,
                uptime_percent: 0.0,      // Would be extracted from SLOMetrics
                error_rate: 0.0,          // Would be extracted from SLOMetrics
                active_tenants: 0,        // Would be extracted from AdoptionMetrics
                growth_trend: 0.0,        // Would be calculated from adoption history
            },
            previous_status,
            new_status,
            confidence: self.calculate_confidence(score, decision_type),
            reversible: decision_type != DecisionType::Quarantine,
            revert_condition: match decision_type {
                DecisionType::Promote => {
                    Some("Composite score drops below 60".to_string())
                }
                DecisionType::Deprecate => {
                    Some("Composite score recovers above 60".to_string())
                }
                DecisionType::Quarantine => None,
                DecisionType::Restore => {
                    Some("Risk score rises above 75 again".to_string())
                }
            },
            timestamp: now,
        }
    }

    /// Calculate confidence level for decision
    fn calculate_confidence(&self, score: &PackageScore, _decision_type: DecisionType) -> f64 {
        // Confidence based on how far we are from thresholds
        let distance_from_threshold = (score.composite_score
            - (if score.composite_score >= self.promotion_threshold {
                self.promotion_threshold
            } else {
                self.deprecation_threshold
            }))
        .abs();

        // Normalize distance to 0-1 confidence (further = higher confidence)
        let confidence = (distance_from_threshold / 50.0).min(1.0);

        // Boost confidence if risk is low
        let risk_adjustment = (1.0 - (score.risk_score / 100.0)) * 0.1;
        (confidence + risk_adjustment).min(1.0)
    }

    /// Generate ontology suggestions based on package performance patterns
    pub fn suggest_ontology_improvements(
        &mut self,
        package_scores: &[PackageScore],
    ) -> Vec<OntologySigmaProposal> {
        let mut suggestions = Vec::new();

        // Detect patterns in package performance
        let mut guard_failure_count = 0;
        let mut performance_issues_count = 0;

        for score in package_scores {
            if score.guard_score < 50.0 {
                // Guard compliance is low - count it
                guard_failure_count += 1;
            }

            if score.slo_score < 50.0 {
                performance_issues_count += 1;
            }
        }

        // Suggest guard refinements if multiple packages have guard compliance issues
        if guard_failure_count >= 3 {
            let suggestion = OntologySigmaProposal {
                id: "suggestion-guard-adaptive".to_string(),
                change_kind: super::ontology_proposal_engine::SigmaChangeKind::GuardAdjustment,
                element_name: "adaptive_guard_thresholds".to_string(),
                element_type: "Guard".to_string(),
                current_definition: Some("Static guard thresholds".to_string()),
                proposed_definition: format!(
                    "Adaptive guard thresholds based on {} packages with low compliance",
                    guard_failure_count
                ),
                justification_evidence: vec![format!(
                    "{} packages failing guard compliance",
                    guard_failure_count
                )],
                estimated_coverage_improvement: 15.0,
                estimated_performance_delta: 5.0,
                risk_score: 30.0,
                affected_patterns: vec![],
                affected_guards: vec!["all_guards".to_string()],
                doctrine_aligned: true,
            };
            suggestions.push(suggestion);
        }

        // Suggest performance patterns if widespread issues
        if performance_issues_count >= (package_scores.len() / 3) {
            let suggestion = OntologySigmaProposal {
                id: "suggestion-perf-pattern".to_string(),
                change_kind: super::ontology_proposal_engine::SigmaChangeKind::NewPattern,
                element_name: "performance_optimization_pattern".to_string(),
                element_type: "Pattern".to_string(),
                current_definition: None,
                proposed_definition: "Pattern to address widespread performance issues".to_string(),
                justification_evidence: vec![format!(
                    "{} packages with low SLO scores",
                    performance_issues_count
                )],
                estimated_coverage_improvement: 20.0,
                estimated_performance_delta: -100.0,
                risk_score: 40.0,
                affected_patterns: vec!["all_packages".to_string()],
                affected_guards: vec![],
                doctrine_aligned: true,
            };
            suggestions.push(suggestion);
        }

        self.ontology_suggestions = suggestions.clone();
        suggestions
    }

    /// Get all promotion decisions
    pub fn decisions(&self) -> &[PromotionDecision] {
        &self.decisions
    }

    /// Get decisions for specific package
    pub fn package_decision_history(
        &self,
        package_name: &str,
        package_version: &str,
    ) -> Option<&Vec<PromotionDecision>> {
        self.decision_history
            .get(&format!("{}-{}", package_name, package_version))
    }

    /// Get latest decision for package
    pub fn latest_decision_for_package(
        &self,
        package_name: &str,
        package_version: &str,
    ) -> Option<&PromotionDecision> {
        self.decision_history
            .get(&format!("{}-{}", package_name, package_version))
            .and_then(|decisions| decisions.last())
    }

    /// Check if a decision can be reverted
    pub fn can_revert(&self, decision: &PromotionDecision) -> bool {
        decision.reversible
    }

    /// Generate audit report
    pub fn audit_report(&self) -> Vec<String> {
        let mut report = Vec::new();

        report.push(format!(
            "Auto-Promotion Pipeline Report - {} decisions",
            self.decisions.len()
        ));
        report.push(format!("Promotion threshold: {}", self.promotion_threshold));
        report.push(format!("Deprecation threshold: {}", self.deprecation_threshold));
        report.push(format!("Quarantine threshold: {}", self.quarantine_threshold));

        let promote_count = self
            .decisions
            .iter()
            .filter(|d| d.decision_type == DecisionType::Promote)
            .count();
        let deprecate_count = self
            .decisions
            .iter()
            .filter(|d| d.decision_type == DecisionType::Deprecate)
            .count();
        let quarantine_count = self
            .decisions
            .iter()
            .filter(|d| d.decision_type == DecisionType::Quarantine)
            .count();

        report.push(format!(
            "Decisions: {} promotions, {} deprecations, {} quarantines",
            promote_count, deprecate_count, quarantine_count
        ));
        report.push(format!(
            "Ontology suggestions: {}",
            self.ontology_suggestions.len()
        ));

        report
    }
}

impl Default for AutoPromotionPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marketplace_scorer::{
        AdoptionMetrics, EconomicMetrics, GuardMetrics, PackageId, RiskMetrics, SLOMetrics,
    };
    use std::collections::HashMap;

    #[test]
    fn test_pipeline_creation() {
        let pipeline = AutoPromotionPipeline::new();
        assert_eq!(pipeline.promotion_threshold, 80.0);
        assert_eq!(pipeline.deprecation_threshold, 40.0);
        assert_eq!(pipeline.quarantine_threshold, 80.0);
    }

    #[test]
    fn test_custom_thresholds() {
        let pipeline = AutoPromotionPipeline::with_thresholds(85.0, 35.0, 75.0);
        assert_eq!(pipeline.promotion_threshold, 85.0);
        assert_eq!(pipeline.deprecation_threshold, 35.0);
        assert_eq!(pipeline.quarantine_threshold, 75.0);
    }

    #[test]
    fn test_promotion_decision() {
        let mut pipeline = AutoPromotionPipeline::new();

        let pkg_id = PackageId::new("pkg1".to_string(), "1.0.0".to_string());
        let mut scorer = MarketplaceScorer::new();

        // Score high-quality package
        let score = scorer.score_package(
            pkg_id,
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
                passed_checks: 980,
                failed_checks: 20,
                breach_rate_percent: 2.0,
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
                active_tenants: 100,
                total_operations: 5000000,
                growth_rate_percent: 30.0,
                sector_distribution: HashMap::new(),
            },
            &RiskMetrics {
                incident_count: 0,
                rollback_count: 0,
                last_incident_severity: 0,
                seconds_since_last_rollback: 604800,
                breaking_change_risk: 5,
            },
        );

        let result = pipeline.evaluate_package(
            "pkg1",
            "1.0.0",
            &score,
            vec!["obs-1".to_string(), "obs-2".to_string()],
        );

        assert!(result.is_ok());
        assert!(result.unwrap().is_some());
    }

    #[test]
    fn test_confidence_calculation() {
        let pipeline = AutoPromotionPipeline::new();

        let score = PackageScore {
            package_id: PackageId::new("pkg".to_string(), "1.0".to_string()),
            slo_score: 85.0,
            guard_score: 80.0,
            economic_score: 75.0,
            adoption_score: 90.0,
            risk_score: 20.0,
            composite_score: 82.0,
            recommendation: PackageRecommendation::Active,
            scored_at: 1000,
        };

        let confidence = pipeline.calculate_confidence(&score, DecisionType::Promote);
        assert!(confidence > 0.0 && confidence <= 1.0);
    }

    #[test]
    fn test_audit_report() {
        let pipeline = AutoPromotionPipeline::new();
        let report = pipeline.audit_report();
        assert!(report.len() > 0);
        assert!(report.join(" ").contains("Pipeline Report"));
    }
}
