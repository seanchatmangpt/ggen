//! Life domain management with OSIRIS integration
//!
//! Manages life domains (health, work, relationships, etc.) and goal tracking.

use crate::error::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Life domains
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LifeDomain {
    Health,
    Work,
    Relationships,
    Finances,
    Learning,
    Creativity,
    Spirituality,
    Recreation,
}

impl std::fmt::Display for LifeDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Health => write!(f, "Health"),
            Self::Work => write!(f, "Work"),
            Self::Relationships => write!(f, "Relationships"),
            Self::Finances => write!(f, "Finances"),
            Self::Learning => write!(f, "Learning"),
            Self::Creativity => write!(f, "Creativity"),
            Self::Spirituality => write!(f, "Spirituality"),
            Self::Recreation => write!(f, "Recreation"),
        }
    }
}

/// Domain goal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Goal {
    pub id: Uuid,
    pub domain: LifeDomain,
    pub description: String,
    pub target: f64,
    pub current: f64,
    pub priority: u8,
    pub created_at: DateTime<Utc>,
    pub target_date: DateTime<Utc>,
}

impl Goal {
    /// Calculate progress percentage
    pub fn progress_percentage(&self) -> f64 {
        if self.target <= 0.0 {
            0.0
        } else {
            (self.current / self.target * 100.0).min(100.0)
        }
    }
}

/// Domain balance assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainBalance {
    pub health: f64,
    pub work: f64,
    pub relationships: f64,
    pub finances: f64,
    pub learning: f64,
    pub creativity: f64,
    pub spirituality: f64,
    pub recreation: f64,
    pub overall_balance: f64,
}

impl DomainBalance {
    /// Calculate overall balance (1.0 = perfectly balanced)
    pub fn calculate_overall(&self) -> f64 {
        let sum = self.health
            + self.work
            + self.relationships
            + self.finances
            + self.learning
            + self.creativity
            + self.spirituality
            + self.recreation;
        let avg = sum / 8.0;

        // Variance penalty for imbalance
        let domains = vec![
            self.health,
            self.work,
            self.relationships,
            self.finances,
            self.learning,
            self.creativity,
            self.spirituality,
            self.recreation,
        ];

        let variance = domains
            .iter()
            .map(|&x| (x - avg).powi(2))
            .sum::<f64>()
            / 8.0;

        (avg - variance.sqrt()).max(0.0).min(1.0)
    }
}

/// Domain manager
pub struct DomainManager {
    goals: HashMap<Uuid, Goal>,
    balance: DomainBalance,
}

impl DomainManager {
    /// Create a new domain manager
    pub fn new() -> Self {
        Self {
            goals: HashMap::new(),
            balance: DomainBalance {
                health: 0.5,
                work: 0.5,
                relationships: 0.5,
                finances: 0.5,
                learning: 0.5,
                creativity: 0.5,
                spirituality: 0.5,
                recreation: 0.5,
                overall_balance: 0.5,
            },
        }
    }

    /// Create a goal for a domain
    pub fn create_goal(
        &mut self,
        domain: LifeDomain,
        description: String,
        target: f64,
        priority: u8,
    ) -> Goal {
        let goal = Goal {
            id: Uuid::new_v4(),
            domain,
            description,
            target,
            current: 0.0,
            priority,
            created_at: Utc::now(),
            target_date: Utc::now() + chrono::Duration::days(30),
        };

        self.goals.insert(goal.id, goal.clone());
        goal
    }

    /// Update goal progress
    pub fn update_goal_progress(&mut self, goal_id: Uuid, progress: f64) -> Result<()> {
        let goal = self.goals.get_mut(&goal_id)
            .ok_or_else(|| crate::WorkflowError::DomainError("Goal not found".to_string()))?;

        goal.current = goal.current + progress;
        if goal.current > goal.target {
            goal.current = goal.target;
        }

        Ok(())
    }

    /// Get goal by ID
    pub fn get_goal(&self, goal_id: Uuid) -> Result<&Goal> {
        self.goals.get(&goal_id)
            .ok_or_else(|| crate::WorkflowError::DomainError("Goal not found".to_string()))
    }

    /// Get goals for a domain
    pub fn goals_for_domain(&self, domain: LifeDomain) -> Vec<Goal> {
        self.goals
            .values()
            .filter(|g| g.domain == domain)
            .cloned()
            .collect()
    }

    /// Calculate domain balance
    pub fn calculate_balance(&mut self) -> DomainBalance {
        let domains = vec![
            (LifeDomain::Health, 0.2),
            (LifeDomain::Work, 0.2),
            (LifeDomain::Relationships, 0.15),
            (LifeDomain::Finances, 0.15),
            (LifeDomain::Learning, 0.1),
            (LifeDomain::Creativity, 0.1),
            (LifeDomain::Spirituality, 0.05),
            (LifeDomain::Recreation, 0.05),
        ];

        let mut balance = DomainBalance {
            health: 0.0,
            work: 0.0,
            relationships: 0.0,
            finances: 0.0,
            learning: 0.0,
            creativity: 0.0,
            spirituality: 0.0,
            recreation: 0.0,
            overall_balance: 0.0,
        };

        for (domain, _weight) in domains {
            let goals = self.goals_for_domain(domain);
            let avg_progress = if goals.is_empty() {
                0.5 // Default to 50% if no goals
            } else {
                goals.iter().map(|g| g.progress_percentage() / 100.0).sum::<f64>() / goals.len() as f64
            };

            match domain {
                LifeDomain::Health => balance.health = avg_progress,
                LifeDomain::Work => balance.work = avg_progress,
                LifeDomain::Relationships => balance.relationships = avg_progress,
                LifeDomain::Finances => balance.finances = avg_progress,
                LifeDomain::Learning => balance.learning = avg_progress,
                LifeDomain::Creativity => balance.creativity = avg_progress,
                LifeDomain::Spirituality => balance.spirituality = avg_progress,
                LifeDomain::Recreation => balance.recreation = avg_progress,
            }
        }

        balance.overall_balance = balance.calculate_overall();
        self.balance = balance.clone();
        balance
    }

    /// Get current balance
    pub fn get_balance(&self) -> &DomainBalance {
        &self.balance
    }
}

impl Default for DomainManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_goal() {
        let mut manager = DomainManager::new();
        let goal = manager.create_goal(
            LifeDomain::Health,
            "Lose 10 pounds".to_string(),
            10.0,
            5,
        );

        assert_eq!(goal.domain, LifeDomain::Health);
        assert_eq!(goal.target, 10.0);
        assert_eq!(goal.current, 0.0);
    }

    #[test]
    fn test_goal_progress() {
        let mut manager = DomainManager::new();
        let goal = manager.create_goal(
            LifeDomain::Learning,
            "Complete Rust course".to_string(),
            10.0,
            5,
        );

        assert!(manager.update_goal_progress(goal.id, 3.0).is_ok());
        let updated = manager.get_goal(goal.id).unwrap();
        assert_eq!(updated.current, 3.0);
        assert!((updated.progress_percentage() - 30.0).abs() < 0.01);
    }

    #[test]
    fn test_domain_balance() {
        let mut manager = DomainManager::new();

        // Create goals in different domains
        for domain in &[
            LifeDomain::Health,
            LifeDomain::Work,
            LifeDomain::Relationships,
        ] {
            manager.create_goal(*domain, "Test goal".to_string(), 10.0, 5);
        }

        let balance = manager.calculate_balance();
        assert!(balance.health > 0.0);
        assert!(balance.work > 0.0);
        assert!(balance.relationships > 0.0);
    }
}
