//! # PDCA (Plan-Do-Check-Act) Cycle Implementation
//!
//! Implements automated continuous improvement cycles for ontology refinement.
//! Each cycle follows the Deming/Shewhart PDCA methodology:
//!
//! - **Plan**: Analyze current state and plan improvements
//! - **Do**: Execute planned changes in a controlled manner
//! - **Check**: Measure results and validate improvements
//! - **Act**: Standardize successful changes or adjust approach

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::{info, warn};

use crate::{KaizenError, Result};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CyclePhase {
    /// Planning phase: Identify improvement opportunities
    Plan,
    /// Execution phase: Implement changes
    Do,
    /// Validation phase: Measure results
    Check,
    /// Standardization phase: Apply or rollback changes
    Act,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PDCACycle {
    pub id: String,
    pub phase: CyclePhase,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub target: ImprovementTarget,
    pub metrics: CycleMetrics,
    pub actions: Vec<Action>,
    pub status: CycleStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ImprovementTarget {
    /// Ontology structure improvements
    OntologyStructure {
        ontology_uri: String,
        focus_area: String,
    },
    /// Namespace organization
    NamespaceOrganization {
        namespace: String,
    },
    /// Template quality improvements
    TemplateQuality {
        template_id: String,
    },
    /// Generation performance
    GenerationPerformance {
        component: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CycleMetrics {
    pub baseline: HashMap<String, f64>,
    pub current: HashMap<String, f64>,
    pub target: HashMap<String, f64>,
    pub improvement_percentage: Option<f64>,
}

impl CycleMetrics {
    /// Calculate improvement percentage for a specific metric
    pub fn calculate_improvement(&self, metric_name: &str) -> Option<f64> {
        let baseline = self.baseline.get(metric_name)?;
        let current = self.current.get(metric_name)?;

        if *baseline == 0.0 {
            return None;
        }

        Some(((current - baseline) / baseline) * 100.0)
    }

    /// Check if target metrics have been achieved
    pub fn targets_achieved(&self) -> bool {
        for (metric, target_value) in &self.target {
            if let Some(current_value) = self.current.get(metric) {
                if current_value < target_value {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Action {
    pub id: String,
    pub description: String,
    pub action_type: ActionType,
    pub executed_at: Option<DateTime<Utc>>,
    pub result: Option<ActionResult>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ActionType {
    /// Refactor ontology structure
    RefactorOntology { query: String },
    /// Reorganize namespace
    ReorganizeNamespace { from: String, to: String },
    /// Update template
    UpdateTemplate { template_id: String, changes: String },
    /// Optimize query
    OptimizeQuery { original: String, optimized: String },
    /// Update documentation
    UpdateDocumentation { section: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionResult {
    pub success: bool,
    pub message: String,
    pub metrics_before: HashMap<String, f64>,
    pub metrics_after: HashMap<String, f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CycleStatus {
    /// Cycle is actively running
    Active,
    /// Cycle completed successfully
    Completed,
    /// Cycle failed and was rolled back
    Failed,
    /// Cycle partially succeeded (some actions applied)
    Partial,
}

impl PDCACycle {
    /// Create a new PDCA cycle
    pub fn new(target: ImprovementTarget) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            phase: CyclePhase::Plan,
            start_time: Utc::now(),
            end_time: None,
            target,
            metrics: CycleMetrics::default(),
            actions: Vec::new(),
            status: CycleStatus::Active,
        }
    }

    /// Transition to the next phase of the cycle
    pub fn advance_phase(&mut self) -> Result<()> {
        self.phase = match self.phase {
            CyclePhase::Plan => CyclePhase::Do,
            CyclePhase::Do => CyclePhase::Check,
            CyclePhase::Check => CyclePhase::Act,
            CyclePhase::Act => {
                self.end_time = Some(Utc::now());
                return Ok(());
            }
        };
        Ok(())
    }

    /// Add an action to the cycle
    pub fn add_action(&mut self, action: Action) {
        info!(
            cycle_id = %self.id,
            action_id = %action.id,
            "Adding action to PDCA cycle"
        );
        self.actions.push(action);
    }

    /// Execute all planned actions
    pub async fn execute_actions(&mut self) -> Result<()> {
        if self.phase != CyclePhase::Do {
            return Err(KaizenError::PDCAError(
                "Actions can only be executed in Do phase".to_string(),
            ));
        }

        for action in &mut self.actions {
            // In a real implementation, this would execute the actual changes
            info!(
                action_id = %action.id,
                action_type = ?action.action_type,
                "Executing action"
            );
            action.executed_at = Some(Utc::now());
        }

        Ok(())
    }

    /// Validate the results of executed actions
    pub fn validate_results(&mut self) -> Result<bool> {
        if self.phase != CyclePhase::Check {
            return Err(KaizenError::PDCAError(
                "Results can only be validated in Check phase".to_string(),
            ));
        }

        let targets_met = self.metrics.targets_achieved();

        if targets_met {
            info!(
                cycle_id = %self.id,
                improvement = ?self.metrics.improvement_percentage,
                "Cycle validation successful"
            );
        } else {
            warn!(
                cycle_id = %self.id,
                "Cycle validation failed - targets not met"
            );
        }

        Ok(targets_met)
    }

    /// Complete the cycle by standardizing or rolling back
    pub fn complete(&mut self, success: bool) -> Result<()> {
        if self.phase != CyclePhase::Act {
            return Err(KaizenError::PDCAError(
                "Cycle can only be completed in Act phase".to_string(),
            ));
        }

        self.status = if success {
            CycleStatus::Completed
        } else {
            CycleStatus::Failed
        };

        self.end_time = Some(Utc::now());

        info!(
            cycle_id = %self.id,
            status = ?self.status,
            duration_secs = (Utc::now() - self.start_time).num_seconds(),
            "PDCA cycle completed"
        );

        Ok(())
    }

    /// Calculate the total improvement achieved
    pub fn total_improvement(&self) -> Option<f64> {
        let mut total = 0.0;
        let mut count = 0;

        for metric in self.metrics.baseline.keys() {
            if let Some(improvement) = self.metrics.calculate_improvement(metric) {
                total += improvement;
                count += 1;
            }
        }

        if count > 0 {
            Some(total / count as f64)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cycle_creation() {
        let target = ImprovementTarget::OntologyStructure {
            ontology_uri: "http://example.org/ontology".to_string(),
            focus_area: "class hierarchy".to_string(),
        };

        let cycle = PDCACycle::new(target);
        assert_eq!(cycle.phase, CyclePhase::Plan);
        assert_eq!(cycle.status, CycleStatus::Active);
    }

    #[test]
    fn test_phase_advancement() {
        let target = ImprovementTarget::NamespaceOrganization {
            namespace: "http://example.org/ns/".to_string(),
        };

        let mut cycle = PDCACycle::new(target);

        assert_eq!(cycle.phase, CyclePhase::Plan);
        cycle.advance_phase().unwrap();
        assert_eq!(cycle.phase, CyclePhase::Do);
        cycle.advance_phase().unwrap();
        assert_eq!(cycle.phase, CyclePhase::Check);
        cycle.advance_phase().unwrap();
        assert_eq!(cycle.phase, CyclePhase::Act);
    }

    #[test]
    fn test_metrics_improvement_calculation() {
        let mut metrics = CycleMetrics::default();
        metrics.baseline.insert("query_time_ms".to_string(), 100.0);
        metrics.current.insert("query_time_ms".to_string(), 80.0);

        let improvement = metrics.calculate_improvement("query_time_ms");
        assert_eq!(improvement, Some(-20.0)); // 20% improvement (reduction)
    }

    #[test]
    fn test_targets_achieved() {
        let mut metrics = CycleMetrics::default();
        metrics.baseline.insert("accuracy".to_string(), 0.8);
        metrics.current.insert("accuracy".to_string(), 0.95);
        metrics.target.insert("accuracy".to_string(), 0.9);

        assert!(metrics.targets_achieved());

        metrics.current.insert("accuracy".to_string(), 0.85);
        assert!(!metrics.targets_achieved());
    }
}
