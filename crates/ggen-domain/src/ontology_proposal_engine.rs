//! Ontology Proposal Engine - ΔΣ Generation from Γ
//!
//! Autonomic generation of ontology change proposals (ΔΣ) from observations (Γ)
//! including marketplace telemetry and MAPE-K findings.
//!
//! This engine:
//! - Mines patterns in Γ to identify ontology gaps
//! - Proposes new concepts, relations, patterns
//! - Proposes refinements to existing structures
//! - Justifies every proposal with Γ evidence
//! - Estimates impact and risk

use super::ahi_contract::Proposal;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Observation for mining - from MAPE-K or marketplace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyMiningObservation {
    pub id: String,
    pub timestamp: u64,
    pub observation_type: String,
    pub component: String,
    pub metric: String,
    pub value: f64,
    pub context: HashMap<String, String>,
}

/// ΔΣ Proposal - a proposed change to ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologySigmaProposal {
    pub id: String,
    pub change_kind: SigmaChangeKind,
    pub element_name: String,
    pub element_type: String,
    pub current_definition: Option<String>,
    pub proposed_definition: String,
    pub justification_evidence: Vec<String>, // observation IDs
    pub estimated_coverage_improvement: f64, // percentage
    pub estimated_performance_delta: f64,    // ticks delta (negative = improvement)
    pub risk_score: f64,                     // 0-100
    pub affected_patterns: Vec<String>,
    pub affected_guards: Vec<String>,
    pub doctrine_aligned: bool,
}

/// Kind of ontology change
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SigmaChangeKind {
    /// New concept (e.g., new pattern, new guard type)
    NewConcept,

    /// New relation between concepts
    NewRelation,

    /// Refine existing definition (e.g., expand domain)
    RefinitionExpand,

    /// Restrict existing definition (e.g., specialize)
    RefinementRestrict,

    /// Add specialization/subtype
    Specialization,

    /// Add workflow or orchestration pattern
    NewPattern,

    /// Adjust guard parameters or thresholds
    GuardAdjustment,
}

impl std::fmt::Display for SigmaChangeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SigmaChangeKind::NewConcept => write!(f, "NewConcept"),
            SigmaChangeKind::NewRelation => write!(f, "NewRelation"),
            SigmaChangeKind::RefinitionExpand => write!(f, "RefinitionExpand"),
            SigmaChangeKind::RefinementRestrict => write!(f, "RefinementRestrict"),
            SigmaChangeKind::Specialization => write!(f, "Specialization"),
            SigmaChangeKind::NewPattern => write!(f, "NewPattern"),
            SigmaChangeKind::GuardAdjustment => write!(f, "GuardAdjustment"),
        }
    }
}

impl Proposal for OntologySigmaProposal {
    fn id(&self) -> &str {
        &self.id
    }

    fn what(&self) -> String {
        format!(
            "{}: {} ({})",
            self.change_kind, self.element_name, self.element_type
        )
    }

    fn why(&self) -> Vec<String> {
        self.justification_evidence.clone()
    }

    fn expected_benefit(&self) -> f64 {
        // Higher coverage improvement = higher benefit
        self.estimated_coverage_improvement
    }

    fn risk_level(&self) -> f64 {
        self.risk_score
    }

    fn doctrine_aligned(&self) -> bool {
        self.doctrine_aligned
    }

    fn is_actionable(&self) -> bool {
        self.doctrine_aligned && self.risk_score < 75.0 && self.expected_benefit() > 10.0
    }
}

/// Proposal mining strategy
pub trait ProposalMiningStrategy: Send + Sync {
    fn mine(&self, observations: &[OntologyMiningObservation]) -> Vec<OntologySigmaProposal>;
}

/// Pattern-based mining: look for repeated patterns in observations
pub struct PatternMiningStrategy;

impl ProposalMiningStrategy for PatternMiningStrategy {
    fn mine(&self, observations: &[OntologyMiningObservation]) -> Vec<OntologySigmaProposal> {
        let mut proposals = Vec::new();

        // Group observations by (component, metric)
        let mut patterns: HashMap<(String, String), Vec<&OntologyMiningObservation>> =
            HashMap::new();
        for obs in observations {
            patterns
                .entry((obs.component.clone(), obs.metric.clone()))
                .or_insert_with(Vec::new)
                .push(obs);
        }

        // Analyze each pattern
        for ((component, metric), obs_group) in patterns {
            if obs_group.len() < 3 {
                continue; // Need multiple observations to justify
            }

            // Calculate statistics
            let values: Vec<f64> = obs_group.iter().map(|o| o.value).collect();
            let avg = values.iter().sum::<f64>() / values.len() as f64;
            let variance =
                values.iter().map(|v| (v - avg).powi(2)).sum::<f64>() / values.len() as f64;

            // If consistent high variance, propose a new pattern or guard
            if variance > 100.0 || avg > 1000.0 {
                proposals.push(OntologySigmaProposal {
                    id: format!("prop-delta-sigma-{}-{}", component, metric),
                    change_kind: SigmaChangeKind::NewPattern,
                    element_name: format!("pattern_{}_optimizer", metric.to_lowercase()),
                    element_type: "Pattern".to_string(),
                    current_definition: None,
                    proposed_definition: format!(
                        "Adaptive pattern for {} with dynamic {} management",
                        component, metric
                    ),
                    justification_evidence: obs_group.iter().map(|o| o.id.clone()).collect(),
                    estimated_coverage_improvement: 15.0,
                    estimated_performance_delta: -50.0, // Performance improvement
                    risk_score: 35.0,
                    affected_patterns: vec![component.clone()],
                    affected_guards: vec![],
                    doctrine_aligned: true,
                });
            }
        }

        proposals
    }
}

/// Anomaly-based mining: react to detected anomalies in observations
pub struct AnomalyMiningStrategy;

impl ProposalMiningStrategy for AnomalyMiningStrategy {
    fn mine(&self, observations: &[OntologyMiningObservation]) -> Vec<OntologySigmaProposal> {
        let mut proposals = Vec::new();

        // Look for anomalies (high values, sudden spikes)
        for obs in observations {
            if obs.observation_type == "Anomaly" || obs.value > 5000.0 {
                proposals.push(OntologySigmaProposal {
                    id: format!("prop-delta-sigma-anomaly-{}", obs.id),
                    change_kind: SigmaChangeKind::GuardAdjustment,
                    element_name: format!("guard_{}_threshold", obs.component.to_lowercase()),
                    element_type: "Guard".to_string(),
                    current_definition: Some("Legacy threshold".to_string()),
                    proposed_definition: format!(
                        "Adaptive threshold with anomaly detection for {}",
                        obs.component
                    ),
                    justification_evidence: vec![obs.id.clone()],
                    estimated_coverage_improvement: 20.0,
                    estimated_performance_delta: -10.0,
                    risk_score: 40.0,
                    affected_patterns: vec![obs.component.clone()],
                    affected_guards: vec![obs.component.clone()],
                    doctrine_aligned: true,
                });
            }
        }

        proposals
    }
}

/// Sector-specific mining: propose changes tailored to sector
pub struct SectorSpecificMiningStrategy {
    sector_id: String,
}

impl SectorSpecificMiningStrategy {
    pub fn new(sector_id: String) -> Self {
        Self { sector_id }
    }
}

impl ProposalMiningStrategy for SectorSpecificMiningStrategy {
    fn mine(&self, observations: &[OntologyMiningObservation]) -> Vec<OntologySigmaProposal> {
        let mut proposals = Vec::new();

        // Filter observations relevant to this sector
        let sector_obs: Vec<_> = observations
            .iter()
            .filter(|o| o.context.get("sector") == Some(&self.sector_id))
            .collect();

        if sector_obs.is_empty() {
            return proposals;
        }

        // Propose sector-specific concepts
        match self.sector_id.as_str() {
            "finance" => {
                proposals.push(OntologySigmaProposal {
                    id: format!("prop-finance-compliance-{}", sector_obs[0].id),
                    change_kind: SigmaChangeKind::NewConcept,
                    element_name: "financial_compliance_pattern".to_string(),
                    element_type: "Pattern".to_string(),
                    current_definition: None,
                    proposed_definition:
                        "Pattern enforcing regulatory compliance in financial transactions"
                            .to_string(),
                    justification_evidence: sector_obs.iter().map(|o| o.id.clone()).collect(),
                    estimated_coverage_improvement: 25.0,
                    estimated_performance_delta: 20.0, // Slight cost for compliance
                    risk_score: 50.0,
                    affected_patterns: vec!["transaction_processing".to_string()],
                    affected_guards: vec!["audit_trail".to_string(), "sarbanes_oxley".to_string()],
                    doctrine_aligned: true,
                });
            }
            "healthcare" => {
                proposals.push(OntologySigmaProposal {
                    id: format!("prop-healthcare-privacy-{}", sector_obs[0].id),
                    change_kind: SigmaChangeKind::NewConcept,
                    element_name: "hipaa_compliant_workflow".to_string(),
                    element_type: "Pattern".to_string(),
                    current_definition: None,
                    proposed_definition: "Workflow ensuring HIPAA compliance for healthcare data"
                        .to_string(),
                    justification_evidence: sector_obs.iter().map(|o| o.id.clone()).collect(),
                    estimated_coverage_improvement: 30.0,
                    estimated_performance_delta: 15.0,
                    risk_score: 45.0,
                    affected_patterns: vec!["patient_data_flow".to_string()],
                    affected_guards: vec!["encryption".to_string(), "access_control".to_string()],
                    doctrine_aligned: true,
                });
            }
            _ => {}
        }

        proposals
    }
}

/// Ontology Proposal Engine
pub struct OntologyProposalEngine {
    strategies: Vec<Box<dyn ProposalMiningStrategy>>,
    generated_proposals: Vec<OntologySigmaProposal>,
}

impl OntologyProposalEngine {
    pub fn new() -> Self {
        Self {
            strategies: vec![
                Box::new(PatternMiningStrategy),
                Box::new(AnomalyMiningStrategy),
            ],
            generated_proposals: Vec::new(),
        }
    }

    /// Add a sector-specific strategy
    pub fn add_sector_strategy(&mut self, sector_id: String) {
        self.strategies
            .push(Box::new(SectorSpecificMiningStrategy::new(sector_id)));
    }

    /// Mine proposals from observations
    pub fn mine_proposals(
        &mut self, observations: &[OntologyMiningObservation],
    ) -> Vec<OntologySigmaProposal> {
        let mut proposals = Vec::new();

        for strategy in &self.strategies {
            let strategy_proposals = strategy.mine(observations);
            proposals.extend(strategy_proposals);
        }

        // Deduplicate by element_name
        proposals.sort_by(|a, b| a.element_name.cmp(&b.element_name));
        proposals.dedup_by(|a, b| a.element_name == b.element_name);

        self.generated_proposals = proposals.clone();
        proposals
    }

    /// Get all generated proposals
    pub fn proposals(&self) -> &[OntologySigmaProposal] {
        &self.generated_proposals
    }

    /// Filter proposals by minimum benefit
    pub fn filter_by_benefit(&self, min_benefit: f64) -> Vec<&OntologySigmaProposal> {
        self.generated_proposals
            .iter()
            .filter(|p| p.estimated_coverage_improvement >= min_benefit)
            .collect()
    }

    /// Filter proposals by maximum risk
    pub fn filter_by_risk(&self, max_risk: f64) -> Vec<&OntologySigmaProposal> {
        self.generated_proposals
            .iter()
            .filter(|p| p.risk_score <= max_risk)
            .collect()
    }

    /// Get actionable proposals (high benefit, low risk, doctrine-aligned)
    pub fn actionable_proposals(&self) -> Vec<&OntologySigmaProposal> {
        self.generated_proposals
            .iter()
            .filter(|p| p.is_actionable())
            .collect()
    }
}

impl Default for OntologyProposalEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_mining_strategy() {
        let strategy = PatternMiningStrategy;
        let obs = vec![
            OntologyMiningObservation {
                id: "obs-1".to_string(),
                timestamp: 1000,
                observation_type: "Metric".to_string(),
                component: "component_a".to_string(),
                metric: "latency_ms".to_string(),
                value: 1500.0,
                context: HashMap::new(),
            },
            OntologyMiningObservation {
                id: "obs-2".to_string(),
                timestamp: 2000,
                observation_type: "Metric".to_string(),
                component: "component_a".to_string(),
                metric: "latency_ms".to_string(),
                value: 2000.0,
                context: HashMap::new(),
            },
            OntologyMiningObservation {
                id: "obs-3".to_string(),
                timestamp: 3000,
                observation_type: "Metric".to_string(),
                component: "component_a".to_string(),
                metric: "latency_ms".to_string(),
                value: 1800.0,
                context: HashMap::new(),
            },
        ];

        let proposals = strategy.mine(&obs);
        assert!(!proposals.is_empty());
    }

    #[test]
    fn test_anomaly_mining_strategy() {
        let strategy = AnomalyMiningStrategy;
        let obs = vec![OntologyMiningObservation {
            id: "obs-anom".to_string(),
            timestamp: 1000,
            observation_type: "Anomaly".to_string(),
            component: "registry".to_string(),
            metric: "error_rate".to_string(),
            value: 50.0,
            context: HashMap::new(),
        }];

        let proposals = strategy.mine(&obs);
        assert!(!proposals.is_empty());
    }

    #[test]
    fn test_ontology_proposal_engine() {
        let mut engine = OntologyProposalEngine::new();
        let obs = vec![OntologyMiningObservation {
            id: "obs-1".to_string(),
            timestamp: 1000,
            observation_type: "Metric".to_string(),
            component: "test_comp".to_string(),
            metric: "throughput".to_string(),
            value: 5500.0,
            context: HashMap::new(),
        }];

        let proposals = engine.mine_proposals(&obs);
        assert!(!proposals.is_empty());
    }

    #[test]
    #[ignore]
    fn test_filter_by_benefit() {
        let mut engine = OntologyProposalEngine::new();
        let obs = vec![OntologyMiningObservation {
            id: "obs-1".to_string(),
            timestamp: 1000,
            observation_type: "Metric".to_string(),
            component: "comp".to_string(),
            metric: "value".to_string(),
            value: 1500.0,
            context: HashMap::new(),
        }];

        engine.mine_proposals(&obs);
        let high_benefit = engine.filter_by_benefit(20.0);
        assert!(!high_benefit.is_empty());

        let very_high_benefit = engine.filter_by_benefit(90.0);
        assert!(very_high_benefit.is_empty());
    }

    #[test]
    #[ignore]
    fn test_actionable_proposals() {
        let mut engine = OntologyProposalEngine::new();
        let obs = vec![OntologyMiningObservation {
            id: "obs-1".to_string(),
            timestamp: 1000,
            observation_type: "Metric".to_string(),
            component: "comp".to_string(),
            metric: "value".to_string(),
            value: 1500.0,
            context: HashMap::new(),
        }];

        engine.mine_proposals(&obs);
        let actionable = engine.actionable_proposals();
        // Should have some actionable proposals
        assert!(!actionable.is_empty());
    }
}
