//! Self-Improvement Mechanisms
//!
//! Tracks:
//! - Action outcomes
//! - Action effectiveness learning
//! - Agent learning sharing

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

/// Action outcome record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionOutcome {
    pub domain_id: String,
    pub action: String,
    pub result: String,
    pub success: bool,
    pub timestamp: String,
}

/// Improvement tracker for learning mechanisms
#[derive(Clone)]
pub struct ImprovementTracker {
    outcomes: Arc<RwLock<Vec<ActionOutcome>>>,
    learning_patterns: Arc<RwLock<HashMap<String, f64>>>,
}

impl ImprovementTracker {
    /// Create new improvement tracker
    pub fn new() -> Self {
        Self {
            outcomes: Arc::new(RwLock::new(Vec::new())),
            learning_patterns: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Record an action and its outcome
    pub async fn record_action(
        &self,
        domain_id: &str,
        action: String,
        result: String,
    ) -> anyhow::Result<()> {
        let success = !result.contains("failed") && !result.contains("error");
        
        let outcome = ActionOutcome {
            domain_id: domain_id.to_string(),
            action: action.clone(),
            result,
            success,
            timestamp: chrono::Utc::now().to_rfc3339(),
        };
        
        let mut outcomes = self.outcomes.write().await;
        outcomes.push(outcome);
        
        // Update learning pattern
        let action_key = format!("{}_{}", domain_id, action);
        let mut patterns = self.learning_patterns.write().await;
        let count = patterns.entry(action_key).or_insert(0.0);
        if success {
            *count += 1.0;
        } else {
            *count -= 0.5;
        }
        
        info!("Recorded outcome for action: {} (success: {})", action, success);
        Ok(())
    }

    /// Get effectiveness of an action
    pub async fn get_action_effectiveness(&self, domain_id: &str, action: &str) -> f64 {
        let patterns = self.learning_patterns.read().await;
        let key = format!("{}_{}", domain_id, action);
        patterns.get(&key).copied().unwrap_or(0.0)
    }

    /// Get top performing actions for a domain
    pub async fn get_top_actions(&self, domain_id: &str, limit: usize) -> Vec<(String, f64)> {
        let patterns = self.learning_patterns.read().await;
        let mut actions: Vec<_> = patterns
            .iter()
            .filter(|(k, _)| k.starts_with(&format!("{}_", domain_id)))
            .map(|(k, v)| {
                let action = k.strip_prefix(&format!("{}_", domain_id))
                    .unwrap_or(k)
                    .to_string();
                (action, *v)
            })
            .collect();
        
        actions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        actions.into_iter().take(limit).collect()
    }

    /// Share learning between agents
    pub async fn share_learning(&self, source_domain: &str, dest_domain: &str) -> anyhow::Result<()> {
        info!("Sharing learning from {} to {}", source_domain, dest_domain);

        let source_patterns: Vec<_> = {
            let patterns = self.learning_patterns.read().await;
            patterns
                .iter()
                .filter(|(k, _)| k.starts_with(&format!("{}_", source_domain)))
                .map(|(k, v)| (k.clone(), *v))
                .collect()
        };

        let mut new_patterns = self.learning_patterns.write().await;
        for (k, v) in source_patterns {
            if let Some(action) = k.strip_prefix(&format!("{}_", source_domain)) {
                let dest_key = format!("{}_{}", dest_domain, action);
                let current = new_patterns.entry(dest_key).or_insert(0.0);
                *current = (*current + v) / 2.0; // Average with existing
            }
        }

        Ok(())
    }

    /// Get all recorded outcomes
    pub async fn get_outcomes(&self) -> Vec<ActionOutcome> {
        self.outcomes.read().await.clone()
    }
}

impl Default for ImprovementTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_record_action() {
        let tracker = ImprovementTracker::new();
        let result = tracker
            .record_action("health", "workout".to_string(), "success".to_string())
            .await;
        assert!(result.is_ok());
        
        let outcomes = tracker.get_outcomes().await;
        assert_eq!(outcomes.len(), 1);
    }

    #[tokio::test]
    async fn test_action_effectiveness() {
        let tracker = ImprovementTracker::new();
        tracker
            .record_action("health", "workout".to_string(), "success".to_string())
            .await
            .unwrap();
        
        let effectiveness = tracker.get_action_effectiveness("health", "workout").await;
        assert!(effectiveness > 0.0);
    }

    #[tokio::test]
    async fn test_share_learning() {
        let tracker = ImprovementTracker::new();
        tracker
            .record_action("health", "meditate".to_string(), "success".to_string())
            .await
            .unwrap();
        
        let result = tracker.share_learning("health", "spirituality").await;
        assert!(result.is_ok());
    }
}
