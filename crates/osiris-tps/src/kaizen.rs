//! TPS Kaizen Implementation
//!
//! Implements Kaizen - continuous improvement cycles

use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

use crate::signals::{TPSLevel, TPSSignal};

/// Kaizen improvement stages
#[derive(Debug, Clone, PartialEq)]
pub enum KaizenStage {
    /// Plan - identify improvement opportunity
    Plan,
    /// Do - implement the improvement
    Do,
    /// Check - verify results
    Check,
    /// Act - standardize and improve further
    Act,
}

/// Kaizen improvement proposal
#[derive(Debug, Clone)]
pub struct KaizenImprovement {
    pub id: String,
    pub title: String,
    pub description: String,
    pub stage: KaizenStage,
    pub suggested_by: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub implemented_at: Option<chrono::DateTime<chrono::Utc>>,
    pub results: Option<KaizenResults>,
    pub status: ImprovementStatus,
}

/// Kaizen improvement results
#[derive(Debug, Clone)]
pub struct KaizenResults {
    pub before_metrics: HashMap<String, f64>,
    pub after_metrics: HashMap<String, f64>,
    pub improvement_percent: f64,
    pub benefits: Vec<String>,
    pub lessons_learned: Vec<String>,
}

/// Improvement status
#[derive(Debug, Clone, PartialEq)]
pub enum ImprovementStatus {
    /// Suggested but not implemented
    Suggested,
    /// In progress
    InProgress,
    /// Implemented successfully
    Implemented,
    /// Rejected or not feasible
    Rejected,
    /// On hold
    OnHold,
}

/// Kaizen cycle manager
pub struct KaizenCycle {
    improvements: Arc<RwLock<HashMap<String, KaizenImprovement>>>,
    active_cycle: Arc<RwLock<Option<OngoingCycle>>>,
    cycle_history: Arc<RwLock<Vec<CycleHistory>>>,
    improvement_suggestions: Arc<RwLock<Vec<ImprovementSuggestion>>>,
}

/// Ongoing Kaizen cycle
#[derive(Debug, Clone)]
pub struct OngoingCycle {
    pub cycle_id: String,
    pub start_time: chrono::DateTime<chrono::Utc>,
    pub current_stage: KaizenStage,
    pub improvements: Vec<String>,
    pub metrics_baseline: HashMap<String, f64>,
}

/// Cycle history
#[derive(Debug, Clone)]
pub struct CycleHistory {
    pub cycle_id: String,
    pub start_time: chrono::DateTime<chrono::Utc>,
    pub end_time: chrono::DateTime<chrono::Utc>,
    pub improvements_count: usize,
    pub average_improvement: f64,
    pub successful_improvements: usize,
}

/// Improvement suggestion
#[derive(Debug, Clone)]
pub struct ImprovementSuggestion {
    pub id: String,
    pub area: String,
    pub description: String,
    pub priority: f64, // 0.0 to 1.0
    pub suggested_by: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl KaizenCycle {
    /// Create a new Kaizen cycle manager
    pub fn new() -> Self {
        Self {
            improvements: Arc::new(RwLock::new(HashMap::new())),
            active_cycle: Arc::new(RwLock::new(None)),
            cycle_history: Arc::new(RwLock::new(Vec::new())),
            improvement_suggestions: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Start a new Kaizen cycle
    pub async fn start_cycle(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!("Starting new Kaizen cycle");

        let cycle_id = format!("cycle_{}", uuid::Uuid::new_v4());
        let cycle = OngoingCycle {
            cycle_id: cycle_id.clone(),
            start_time: chrono::Utc::now(),
            current_stage: KaizenStage::Plan,
            improvements: Vec::new(),
            metrics_baseline: self.capture_baseline_metrics().await?,
        };

        let mut active_cycle = self.active_cycle.write().await;
        *active_cycle = Some(cycle);

        Ok(())
    }

    /// Suggest an improvement
    pub async fn suggest_improvement(
        &self, message: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        info!("Kaizen improvement suggestion: {}", message);

        let suggestion = ImprovementSuggestion {
            id: format!("suggestion_{}", uuid::Uuid::new_v4()),
            area: "general".to_string(), // Would be parsed from message in real implementation
            description: message.clone(),
            priority: 0.5, // Default priority
            suggested_by: "system".to_string(),
            timestamp: chrono::Utc::now(),
        };

        let mut suggestions = self.improvement_suggestions.write().await;
        suggestions.push(suggestion);

        // Create formal improvement
        let improvement = KaizenImprovement {
            id: format!("improvement_{}", uuid::Uuid::new_v4()),
            title: "Suggested Improvement".to_string(),
            description: message,
            stage: KaizenStage::Plan,
            suggested_by: "system".to_string(),
            created_at: chrono::Utc::now(),
            implemented_at: None,
            results: None,
            status: ImprovementStatus::Suggested,
        };

        let mut improvements = self.improvements.write().await;
        improvements.insert(improvement.id.clone(), improvement);

        Ok(())
    }

    /// Record an observation
    pub async fn record_observation(
        &self, message: String,
    ) -> Result<(), Box<dyn std::error::Error>> {
        debug!("Kaizen observation: {}", message);

        // Observations can be converted to suggestions
        self.suggest_improvement(format!("Observation: {}", message))
            .await?;

        Ok(())
    }

    /// Move to the next Kaizen stage
    pub async fn next_stage(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut active_cycle = self.active_cycle.write().await;

        if let Some(cycle) = active_cycle.as_mut() {
            let next_stage = match cycle.current_stage {
                KaizenStage::Plan => KaizenStage::Do,
                KaizenStage::Do => KaizenStage::Check,
                KaizenStage::Check => KaizenStage::Act,
                KaizenStage::Act => {
                    // Cycle complete - start new one
                    self.complete_cycle(&cycle.cycle_id).await?;
                    return self.start_cycle().await;
                }
            };

            info!(
                "Moving Kaizen cycle from {:?} to {:?}",
                cycle.current_stage, next_stage
            );
            cycle.current_stage = next_stage;
        } else {
            return Err("No active Kaizen cycle".into());
        }

        Ok(())
    }

    /// Implement an improvement
    pub async fn improve_process(
        &self, parameters: Value,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        let improvement_id = parameters
            .get("improvement_id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| "Improvement ID not specified")?;

        let mut improvements = self.improvements.write().await;

        if let Some(improvement) = improvements.get_mut(improvement_id) {
            improvement.status = ImprovementStatus::InProgress;
            improvement.stage = KaizenStage::Do;

            // Simulate implementation
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;

            improvement.status = ImprovementStatus::Implemented;
            improvement.implemented_at = Some(chrono::Utc::now());

            // Capture results
            let results = KaizenResults {
                before_metrics: self.capture_baseline_metrics().await?,
                after_metrics: self.capture_improved_metrics().await?,
                improvement_percent: self.calculate_improvement().await?,
                benefits: vec![
                    "Increased efficiency".to_string(),
                    "Reduced waste".to_string(),
                    "Improved quality".to_string(),
                ],
                lessons_learned: vec![
                    "Small improvements add up".to_string(),
                    "Employee input is valuable".to_string(),
                ],
            };

            improvement.results = Some(results);

            // Move to next stage
            self.next_stage().await?;

            Ok(json!({
                "status": "success",
                "improvement_id": improvement_id,
                "message": "Improvement implemented successfully"
            }))
        } else {
            Err(format!("Improvement {} not found", improvement_id).into())
        }
    }

    /// Capture baseline metrics
    async fn capture_baseline_metrics(
        &self,
    ) -> Result<HashMap<String, f64>, Box<dyn std::error::Error>> {
        // In real implementation, this would capture actual metrics
        // For now, return mock data
        Ok(HashMap::from([
            ("efficiency".to_string(), 0.75),
            ("quality".to_string(), 0.85),
            ("waste".to_string(), 0.25),
            ("cycle_time".to_string(), 60.0),
        ]))
    }

    /// Capture improved metrics
    async fn capture_improved_metrics(
        &self,
    ) -> Result<HashMap<String, f64>, Box<dyn std::error::Error>> {
        // In real implementation, this would capture actual metrics after improvement
        // For now, return mock data with improvements
        Ok(HashMap::from([
            ("efficiency".to_string(), 0.85),
            ("quality".to_string(), 0.95),
            ("waste".to_string(), 0.15),
            ("cycle_time".to_string(), 45.0),
        ]))
    }

    /// Calculate improvement percentage
    async fn calculate_improvement(&self) -> Result<f64, Box<dyn std::error::Error>> {
        let baseline = self.capture_baseline_metrics().await?;
        let improved = self.capture_improved_metrics().await?;

        let efficiency_improvement =
            improved.get("efficiency").unwrap() - baseline.get("efficiency").unwrap();
        let overall_improvement =
            (efficiency_improvement / baseline.get("efficiency").unwrap()) * 100.0;

        Ok(overall_improvement)
    }

    /// Complete the current Kaizen cycle
    async fn complete_cycle(&self, cycle_id: &str) -> Result<(), Box<dyn std::error::Error>> {
        let active_cycle = self.active_cycle.read().await;
        let cycle = active_cycle.as_ref().ok_or("No active cycle to complete")?;

        let cycle_history = CycleHistory {
            cycle_id: cycle_id.to_string(),
            start_time: cycle.start_time,
            end_time: chrono::Utc::now(),
            improvements_count: cycle.improvements.len(),
            average_improvement: self.calculate_improvement().await?,
            successful_improvements: cycle.improvements.len(), // Assume all are successful
        };

        let mut history = self.cycle_history.write().await;
        history.push(cycle_history);

        info!("Kaizen cycle {} completed", cycle_id);

        // Clear active cycle
        let mut active_cycle = self.active_cycle.write().await;
        *active_cycle = None;

        Ok(())
    }

    /// Get active cycle information
    pub async fn get_active_cycle(&self) -> Option<Value> {
        let active_cycle = self.active_cycle.read().await;

        if let Some(cycle) = active_cycle.as_ref() {
            Some(json!({
                "cycle_id": cycle.cycle_id,
                "current_stage": format!("{:?}", cycle.current_stage),
                "start_time": cycle.start_time.to_rfc3339(),
                "duration_ms": chrono::Utc::now().timestamp_millis() - cycle.start_time.timestamp_millis(),
                "improvements_count": cycle.improvements.len()
            }))
        } else {
            None
        }
    }

    /// Get all improvements
    pub async fn get_improvements(
        &self, status_filter: Option<ImprovementStatus>,
    ) -> Vec<KaizenImprovement> {
        let improvements = self.improvements.read().await;
        let improvements_vec = improvements.values().cloned().collect::<Vec<_>>();

        match status_filter {
            Some(filter) => improvements_vec
                .into_iter()
                .filter(|i| i.status == filter)
                .collect(),
            None => improvements_vec,
        }
    }

    /// Get improvement suggestions
    pub async fn get_improvement_suggestions(&self) -> Vec<ImprovementSuggestion> {
        let suggestions = self.improvement_suggestions.read().await;
        suggestions.iter().cloned().collect()
    }

    /// Get cycle history
    pub async fn get_cycle_history(&self, limit: Option<usize>) -> Vec<CycleHistory> {
        let history = self.cycle_history.read().await;
        let history_vec = history.iter().cloned().collect::<Vec<_>>();

        match limit {
            Some(l) => history_vec.into_iter().take(l).collect(),
            None => history_vec,
        }
    }

    /// Get system status
    pub async fn get_status(&self) -> Value {
        let improvements = self.improvements.read().await;
        let suggestions = self.improvement_suggestions.read().await;
        let history = self.cycle_history.read().await;
        let active_cycle = self.get_active_cycle().await;

        let total_improvements = improvements.len();
        let implemented_improvements = improvements
            .values()
            .filter(|i| i.status == ImprovementStatus::Implemented)
            .count();
        let suggested_improvements = improvements
            .values()
            .filter(|i| i.status == ImprovementStatus::Suggested)
            .count();
        let total_suggestions = suggestions.len();

        let average_improvement = if !history.is_empty() {
            let total_improvement: f64 = history.iter().map(|h| h.average_improvement).sum();
            Some(total_improvement / history.len() as f64)
        } else {
            None
        };

        json!({
            "active_cycle": active_cycle.is_some(),
            "current_stage": if let Some(cycle) = &*self.active_cycle.read().await {
                Some(format!("{:?}", cycle.current_stage))
            } else {
                None
            },
            "improvements": {
                "total": total_improvements,
                "implemented": implemented_improvements,
                "suggested": suggested_improvements
            },
            "suggestions": total_suggestions,
            "cycle_history": history.len(),
            "average_improvement_percent": average_improvement,
            "timestamp": chrono::Utc::now().to_rfc3339()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kaizen_cycle_creation() {
        let kaizen = KaizenCycle::new();
        assert_eq!(kaizen.improvements.try_read().unwrap().len(), 0);
        assert_eq!(kaizen.improvement_suggestions.try_read().unwrap().len(), 0);
    }

    #[tokio::test]
    async fn test_cycle_start() {
        let kaizen = KaizenCycle::new();
        let result = kaizen.start_cycle().await;
        assert!(result.is_ok());

        let active_cycle = kaizen.get_active_cycle().await;
        assert!(active_cycle.is_some());
    }

    #[tokio::test]
    async fn test_improvement_suggestion() {
        let kaizen = KaizenCycle::new();
        kaizen.start_cycle().await.unwrap();

        let result = kaizen
            .suggest_improvement("Test improvement suggestion".to_string())
            .await;
        assert!(result.is_ok());

        let improvements = kaizen.get_improvements(None).await;
        assert_eq!(improvements.len(), 1);
        assert_eq!(improvements[0].status, ImprovementStatus::Suggested);

        let suggestions = kaizen.get_improvement_suggestions().await;
        assert_eq!(suggestions.len(), 1);
    }

    #[tokio::test]
    async fn test_observation_recording() {
        let kaizen = KaizenCycle::new();
        kaizen.start_cycle().await.unwrap();

        let result = kaizen
            .record_observation("Test observation".to_string())
            .await;
        assert!(result.is_ok());

        let improvements = kaizen.get_improvements(None).await;
        assert!(!improvements.is_empty());
    }

    #[tokio::test]
    async fn test_stage_progression() {
        let kaizen = KaizenCycle::new();
        kaizen.start_cycle().await.unwrap();

        // Initially in Plan stage
        let active_cycle = kaizen.get_active_cycle().await.unwrap();
        assert_eq!(active_cycle["current_stage"], "Plan");

        // Move to Do stage
        kaizen.next_stage().await.unwrap();
        let active_cycle = kaizen.get_active_cycle().await.unwrap();
        assert_eq!(active_cycle["current_stage"], "Do");

        // Move to Check stage
        kaizen.next_stage().await.unwrap();
        let active_cycle = kaizen.get_active_cycle().await.unwrap();
        assert_eq!(active_cycle["current_stage"], "Check");

        // Move to Act stage
        kaizen.next_stage().await.unwrap();
        let active_cycle = kaizen.get_active_cycle().await.unwrap();
        assert_eq!(active_cycle["current_stage"], "Act");
    }

    #[tokio::test]
    async fn test_improvement_implementation() {
        let kaizen = KaizenCycle::new();
        kaizen.start_cycle().await.unwrap();

        // First suggest an improvement
        kaizen
            .suggest_improvement("Test improvement".to_string())
            .await
            .unwrap();

        // Get the improvement ID
        let improvements = kaizen.get_improvements(None).await;
        let improvement_id = &improvements[0].id;

        // Implement the improvement
        let params = json!({
            "improvement_id": improvement_id
        });

        let result = kaizen.improve_process(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_cycle_completion() {
        let kaizen = KaizenCycle::new();
        kaizen.start_cycle().await.unwrap();

        // Move through all stages to complete the cycle
        kaizen.next_stage().await.unwrap(); // Plan -> Do
        kaizen.next_stage().await.unwrap(); // Do -> Check
        kaizen.next_stage().await.unwrap(); // Check -> Act -> new cycle

        // After completion, there should be a new cycle
        let active_cycle = kaizen.get_active_cycle().await;
        assert!(active_cycle.is_some());

        let history = kaizen.get_cycle_history(None).await;
        assert_eq!(history.len(), 1);
    }

    #[tokio::test]
    async fn test_kaizen_status() {
        let kaizen = KaizenCycle::new();
        let status = kaizen.get_status().await;
        assert!(status.is_object());
        assert_eq!(status["active_cycle"], false);
        assert!(status["improvements"]["total"] == 0);
    }
}
