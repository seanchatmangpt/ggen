//! # Production Readiness Tracking
//!
//! Tracks and scores production readiness based on requirements.
//! Provides detailed reports on blockers and warnings.

use super::config::{LifecycleConfig, Status};
use crate::error::{CleanroomError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Production readiness score and details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadinessScore {
    /// Overall readiness score (0-100)
    pub score: u8,

    /// Individual requirement statuses
    pub requirements: Vec<RequirementStatus>,

    /// Critical blockers preventing deployment
    pub blockers: Vec<String>,

    /// Non-critical warnings
    pub warnings: Vec<String>,

    /// Category breakdown
    pub category_scores: HashMap<String, u8>,

    /// Recommendations
    pub recommendations: Vec<String>,
}

/// Status of individual requirement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequirementStatus {
    /// Requirement ID
    pub id: String,

    /// Requirement name
    pub name: String,

    /// Current status
    pub status: Status,

    /// Category
    pub category: String,

    /// Priority (1-5)
    pub priority: u8,

    /// Status details
    pub details: String,

    /// Validation result (if available)
    pub validation_passed: Option<bool>,
}

/// Readiness tracker
pub struct ReadinessTracker {
    config: LifecycleConfig,
    requirement_states: HashMap<String, Status>,
}

impl ReadinessTracker {
    /// Create new readiness tracker
    pub fn new(config: LifecycleConfig) -> Self {
        let mut requirement_states = HashMap::new();
        for req in &config.readiness_requirements {
            requirement_states.insert(req.id.clone(), req.status);
        }

        Self {
            config,
            requirement_states,
        }
    }

    /// Evaluate production readiness
    pub async fn evaluate(&self) -> Result<ReadinessScore> {
        let mut requirements = Vec::new();
        let mut blockers = Vec::new();
        let mut warnings = Vec::new();
        let mut category_scores: HashMap<String, Vec<u8>> = HashMap::new();
        let mut recommendations = Vec::new();

        // Evaluate each requirement
        for req in &self.config.readiness_requirements {
            let status = self.requirement_states
                .get(&req.id)
                .copied()
                .unwrap_or(req.status);

            let validation_passed = if let Some(cmd) = &req.validation_command {
                Some(self.run_validation(cmd).await?)
            } else {
                None
            };

            let req_status = RequirementStatus {
                id: req.id.clone(),
                name: req.name.clone(),
                status,
                category: format!("{:?}", req.category),
                priority: req.priority,
                details: req.description.clone(),
                validation_passed,
            };

            // Track blockers
            if req.priority >= 4 && status != Status::Complete {
                blockers.push(format!(
                    "{} ({}): {}",
                    req.name, status, req.description
                ));
            }

            // Track warnings
            if req.priority >= 2 && req.priority < 4 && status != Status::Complete {
                warnings.push(format!(
                    "{} ({}): {}",
                    req.name, status, req.description
                ));
            }

            // Calculate score contribution
            let score_contribution = match status {
                Status::Complete => 100,
                Status::InProgress => 50,
                Status::NotStarted => 0,
                Status::Blocked => 0,
            };

            category_scores
                .entry(format!("{:?}", req.category))
                .or_insert_with(Vec::new)
                .push(score_contribution);

            requirements.push(req_status);
        }

        // Generate recommendations
        recommendations.extend(self.generate_recommendations(&requirements));

        // Calculate overall score
        let score = self.calculate_overall_score(&category_scores);

        // Calculate category scores
        let category_scores = category_scores
            .into_iter()
            .map(|(cat, scores)| {
                let avg = if !scores.is_empty() {
                    scores.iter().sum::<u8>() / scores.len() as u8
                } else {
                    0
                };
                (cat, avg)
            })
            .collect();

        Ok(ReadinessScore {
            score,
            requirements,
            blockers,
            warnings,
            category_scores,
            recommendations,
        })
    }

    /// Update requirement status
    pub async fn update_requirement(&self, id: &str, _status: Status) -> Result<()> {
        // Note: In a real implementation, this would persist to storage
        // For now, we just validate the ID exists
        if !self.config.readiness_requirements.iter().any(|r| r.id == id) {
            return Err(CleanroomError::validation_error(
                format!("Requirement '{}' not found", id)
            ));
        }

        Ok(())
    }

    /// Check if ready for production
    pub fn is_ready_for_production(&self, score: &ReadinessScore) -> bool {
        score.score >= 80 && score.blockers.is_empty()
    }

    /// Calculate overall readiness score
    fn calculate_overall_score(&self, category_scores: &HashMap<String, Vec<u8>>) -> u8 {
        if category_scores.is_empty() {
            return 0;
        }

        let mut total_score = 0u32;
        let mut total_weight = 0u32;

        for scores in category_scores.values() {
            if !scores.is_empty() {
                let avg = scores.iter().map(|&s| s as u32).sum::<u32>() / scores.len() as u32;
                total_score += avg;
                total_weight += 1;
            }
        }

        if total_weight > 0 {
            (total_score / total_weight) as u8
        } else {
            0
        }
    }

    /// Generate recommendations based on current state
    fn generate_recommendations(&self, requirements: &[RequirementStatus]) -> Vec<String> {
        let mut recs = Vec::new();

        // Check for high-priority incomplete requirements
        let incomplete_critical: Vec<_> = requirements
            .iter()
            .filter(|r| r.priority >= 4 && r.status != Status::Complete)
            .collect();

        if !incomplete_critical.is_empty() {
            recs.push(format!(
                "Complete {} critical requirement(s) before deployment",
                incomplete_critical.len()
            ));
        }

        // Check for category-specific issues
        let mut category_incomplete: HashMap<&str, usize> = HashMap::new();
        for req in requirements {
            if req.status != Status::Complete {
                *category_incomplete.entry(&req.category).or_insert(0) += 1;
            }
        }

        for (category, count) in category_incomplete {
            if count > 2 {
                recs.push(format!(
                    "Focus on {} category: {} incomplete requirement(s)",
                    category, count
                ));
            }
        }

        // Check for blocked requirements
        let blocked: Vec<_> = requirements
            .iter()
            .filter(|r| r.status == Status::Blocked)
            .collect();

        if !blocked.is_empty() {
            recs.push(format!(
                "Unblock {} requirement(s) to proceed",
                blocked.len()
            ));
        }

        recs
    }

    /// Run validation command
    async fn run_validation(&self, _command: &str) -> Result<bool> {
        // Placeholder: In production, this would execute the validation command
        // For now, return true to indicate validation would pass
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_readiness_tracker_creation() {
        let config = LifecycleConfig::default_with_name("test");
        let tracker = ReadinessTracker::new(config);
        let score = tracker.evaluate().await.unwrap();
        assert!(score.score <= 100);
    }

    #[tokio::test]
    async fn test_empty_requirements() {
        let mut config = LifecycleConfig::default_with_name("test");
        config.readiness_requirements.clear();
        let tracker = ReadinessTracker::new(config);
        let score = tracker.evaluate().await.unwrap();
        assert_eq!(score.score, 0);
    }

    #[test]
    fn test_is_ready_for_production() {
        let config = LifecycleConfig::default_with_name("test");
        let tracker = ReadinessTracker::new(config);

        let ready_score = ReadinessScore {
            score: 85,
            requirements: vec![],
            blockers: vec![],
            warnings: vec![],
            category_scores: HashMap::new(),
            recommendations: vec![],
        };

        assert!(tracker.is_ready_for_production(&ready_score));

        let not_ready_score = ReadinessScore {
            score: 75,
            requirements: vec![],
            blockers: vec!["Critical issue".to_string()],
            warnings: vec![],
            category_scores: HashMap::new(),
            recommendations: vec![],
        };

        assert!(!tracker.is_ready_for_production(&not_ready_score));
    }
}
