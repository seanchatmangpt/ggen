//! Phase 7: Plan - Overlay Proposal Generation
//!
//! Converts findings into ΔΣ proposals (ontology overlays).
//! Implements policy-driven and LLM-assisted overlay generation.

use super::types::{
    Finding, FindingKind, OntologyOverlay, OverlayKind, OverlayProposal, OverlayProposer,
    PolicyRule, PolicyAction, ValidationStatus,
};
use std::collections::HashMap;

/// Plan engine: generates overlay proposals
pub struct PlanEngine {
    policies: Vec<PolicyRule>,
    overlay_counter: usize,
}

impl PlanEngine {
    /// Create new plan engine with default policies
    pub fn new() -> Self {
        Self {
            policies: Self::default_policies(),
            overlay_counter: 0,
        }
    }

    /// Create default autonomic policies
    fn default_policies() -> Vec<PolicyRule> {
        vec![
            PolicyRule {
                id: "policy-tick-budget".to_string(),
                name: "Relax tick-budget pattern".to_string(),
                trigger_condition: "TickBudgetViolation".to_string(),
                action: PolicyAction::ProposeOverlay {
                    overlay_kind: "Modification".to_string(),
                    parameters: {
                        let mut m = HashMap::new();
                        m.insert("action".to_string(), serde_json::json!("break_into_steps"));
                        m.insert("priority".to_string(), serde_json::json!("High"));
                        m
                    },
                },
                priority: 100,
                enabled: true,
            },
            PolicyRule {
                id: "policy-guard-relax".to_string(),
                name: "Relax failing guard".to_string(),
                trigger_condition: "GuardFailureRate".to_string(),
                action: PolicyAction::ProposeOverlay {
                    overlay_kind: "Modification".to_string(),
                    parameters: {
                        let mut m = HashMap::new();
                        m.insert("action".to_string(), serde_json::json!("relax_guard"));
                        m.insert("threshold_delta".to_string(), serde_json::json!(10.0));
                        m
                    },
                },
                priority: 90,
                enabled: true,
            },
            PolicyRule {
                id: "policy-slo-alert".to_string(),
                name: "Alert on SLO breach".to_string(),
                trigger_condition: "SLOBreach".to_string(),
                action: PolicyAction::Alert {
                    severity: "High".to_string(),
                    message: "SLO breach detected".to_string(),
                },
                priority: 80,
                enabled: true,
            },
        ]
    }

    /// Plan overlays based on findings
    pub fn plan(&mut self, findings: &[Finding]) -> Vec<OverlayProposal> {
        let mut proposals = Vec::new();

        // Clone policies to avoid borrow checker issues
        let policies = self.policies.clone();

        for finding in findings {
            // Match finding against policies
            for policy in &policies {
                if !policy.enabled {
                    continue;
                }

                if self.policy_matches_finding(policy, finding) {
                    if let Some(proposal) =
                        self.generate_proposal_from_policy(policy, finding)
                    {
                        proposals.push(proposal);
                    }
                }
            }
        }

        proposals
    }

    /// Check if policy matches finding
    fn policy_matches_finding(&self, policy: &PolicyRule, finding: &Finding) -> bool {
        policy.trigger_condition == format!("{:?}", finding.kind)
    }

    /// Generate overlay proposal from policy and finding
    fn generate_proposal_from_policy(
        &mut self,
        policy: &PolicyRule,
        finding: &Finding,
    ) -> Option<OverlayProposal> {
        match &policy.action {
            PolicyAction::ProposeOverlay {
                overlay_kind,
                parameters,
            } => {
                self.overlay_counter += 1;
                let overlay_id = format!("overlay-{:04}", self.overlay_counter);

                // Generate RDF patch based on action
                let rdf_patch = self.generate_rdf_patch(
                    finding.kind,
                    &finding.component,
                    parameters,
                );

                let overlay = OntologyOverlay {
                    id: overlay_id.clone(),
                    base_snapshot_id: "current".to_string(),
                    rdf_patch,
                    overlay_kind: match overlay_kind.as_str() {
                        "Modification" => OverlayKind::Modification,
                        "Addition" => OverlayKind::Addition,
                        "Removal" => OverlayKind::Removal,
                        _ => OverlayKind::Modification,
                    },
                    guard_changes: vec![],
                    config_changes: HashMap::new(),
                    proposer: OverlayProposer::Policy,
                    related_finding: Some(finding.id.clone()),
                    created_at: get_timestamp(),
                    validation_status: ValidationStatus::Pending,
                    validation_results: vec![],
                };

                Some(OverlayProposal {
                    title: format!("Fix {}: {}", policy.name, finding.component),
                    description: finding.description.clone(),
                    overlay,
                    estimated_effort: self.estimate_effort(finding.kind),
                    expected_improvement: self.estimate_improvement(finding.kind),
                    risk_level: self.estimate_risk_level(finding.severity.clone()),
                })
            }
            PolicyAction::Alert { .. } => {
                // Alerts don't generate overlays
                None
            }
            _ => None,
        }
    }

    /// Generate RDF patch for overlay
    fn generate_rdf_patch(
        &self,
        finding_kind: FindingKind,
        component: &str,
        parameters: &HashMap<String, serde_json::Value>,
    ) -> String {
        match finding_kind {
            FindingKind::TickBudgetViolation => {
                format!(
                    r#"# Break {} into smaller steps
@prefix knhk: <http://ggen.ai/knhk#> .

knhk:Pattern_{} a knhk:Pattern ;
    knhk:id "{}" ;
    knhk:maxTicks 6 ;
    knhk:strategy knhk:StepWise .
"#,
                    component, component, component
                )
            }
            FindingKind::GuardFailureRate => {
                format!(
                    r#"# Relax {} guard threshold
@prefix knhk: <http://ggen.ai/knhk#> .

knhk:Guard_{} a knhk:Guard ;
    knhk:id "{}" ;
    knhk:threshold {} .
"#,
                    component,
                    component,
                    component,
                    parameters
                        .get("threshold_delta")
                        .and_then(|v| v.as_f64())
                        .unwrap_or(10.0)
                )
            }
            FindingKind::DriftDetected => {
                format!(
                    r#"# Add sampling/monitoring for {}
@prefix knhk: <http://ggen.ai/knhk#> .

knhk:Monitor_{} a knhk:Monitor ;
    knhk:component "{}" ;
    knhk:samplingRate 0.1 .
"#,
                    component, component, component
                )
            }
            _ => {
                format!(
                    r#"# Placeholder overlay for {}
@prefix knhk: <http://ggen.ai/knhk#> .

knhk:Overlay a knhk:Overlay ;
    knhk:component "{}" .
"#,
                    component, component
                )
            }
        }
    }

    /// Estimate effort (hours) to implement overlay
    fn estimate_effort(&self, kind: FindingKind) -> f64 {
        match kind {
            FindingKind::TickBudgetViolation => 2.0,
            FindingKind::GuardFailureRate => 1.0,
            FindingKind::DriftDetected => 0.5,
            FindingKind::SLOBreach => 1.5,
            FindingKind::PatternDiscovered => 3.0,
            FindingKind::OptimizationOpportunity => 0.5,
        }
    }

    /// Estimate improvement in score from implementing overlay
    fn estimate_improvement(&self, kind: FindingKind) -> f64 {
        match kind {
            FindingKind::TickBudgetViolation => 5.0,
            FindingKind::GuardFailureRate => 3.0,
            FindingKind::DriftDetected => 2.0,
            FindingKind::SLOBreach => 4.0,
            FindingKind::PatternDiscovered => 6.0,
            FindingKind::OptimizationOpportunity => 1.0,
        }
    }

    /// Estimate risk level
    fn estimate_risk_level(&self, severity: String) -> String {
        match severity.as_str() {
            "Critical" => "High".to_string(),
            "High" => "Medium".to_string(),
            "Medium" => "Low".to_string(),
            _ => "Low".to_string(),
        }
    }

    /// Add custom policy
    pub fn add_policy(&mut self, policy: PolicyRule) {
        self.policies.push(policy);
    }

    /// Get all policies
    pub fn policies(&self) -> &[PolicyRule] {
        &self.policies
    }
}

impl Default for PlanEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Get current timestamp
fn get_timestamp() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plan_from_findings() {
        let mut planner = PlanEngine::new();

        let finding = Finding {
            id: "finding-1".to_string(),
            kind: FindingKind::TickBudgetViolation,
            severity: "High".to_string(),
            description: "Pattern exceeds tick budget".to_string(),
            component: "expensive_pattern".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: 0,
            metadata: HashMap::new(),
        };

        let proposals = planner.plan(&[finding]);
        assert!(!proposals.is_empty());

        let proposal = &proposals[0];
        assert_eq!(proposal.overlay.proposer, OverlayProposer::Policy);
        assert!(proposal.estimated_effort > 0.0);
    }

    #[test]
    fn test_default_policies_loaded() {
        let planner = PlanEngine::new();
        assert!(planner.policies.len() >= 3);
    }

    #[test]
    fn test_custom_policy() {
        let mut planner = PlanEngine::new();

        let policy = PolicyRule {
            id: "test-policy".to_string(),
            name: "Test Policy".to_string(),
            trigger_condition: "TestFinding".to_string(),
            action: PolicyAction::Alert {
                severity: "Low".to_string(),
                message: "Test alert".to_string(),
            },
            priority: 50,
            enabled: true,
        };

        planner.add_policy(policy);
        assert!(planner.policies.iter().any(|p| p.id == "test-policy"));
    }
}
