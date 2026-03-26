//! Life domain management for goal setting and tracking

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A life domain managed by agents
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LifeDomain {
    /// Physical health and fitness
    Health,
    /// Mental wellbeing and stress
    Mental,
    /// Financial management
    Financial,
    /// Social connections
    Social,
    /// Career and professional growth
    Career,
    /// Personal growth and learning
    Learning,
}

impl LifeDomain {
    /// Get domain name
    pub fn name(&self) -> &'static str {
        match self {
            LifeDomain::Health => "Health",
            LifeDomain::Mental => "Mental",
            LifeDomain::Financial => "Financial",
            LifeDomain::Social => "Social",
            LifeDomain::Career => "Career",
            LifeDomain::Learning => "Learning",
        }
    }
}

/// Status of a domain
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DomainStatus {
    /// Domain is thriving
    Thriving,
    /// Domain is balanced
    Balanced,
    /// Domain needs attention
    Needs,
    /// Domain is failing
    Critical,
}

impl DomainStatus {
    pub fn code(&self) -> &'static str {
        match self {
            DomainStatus::Thriving => "THRIVING",
            DomainStatus::Balanced => "BALANCED",
            DomainStatus::Needs => "NEEDS",
            DomainStatus::Critical => "CRITICAL",
        }
    }
}

/// A goal within a domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainGoal {
    /// Goal ID
    pub id: Uuid,
    /// Goal description
    pub description: String,
    /// Target domain
    pub domain: String,
    /// Priority (0-10)
    pub priority: u8,
    /// Current progress (0-100)
    pub progress: u8,
    /// Is goal completed
    pub completed: bool,
}

impl DomainGoal {
    /// Create new goal
    pub fn new(description: String, domain: LifeDomain, priority: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            description,
            domain: domain.name().to_string(),
            priority,
            progress: 0,
            completed: false,
        }
    }

    /// Update progress
    pub fn set_progress(&mut self, progress: u8) {
        self.progress = progress.min(100);
        if self.progress == 100 {
            self.completed = true;
        }
    }
}

/// Manager for domain goals and balancing
pub struct DomainManager {
    goals: Vec<DomainGoal>,
}

impl DomainManager {
    /// Create new domain manager
    pub fn new() -> Self {
        Self { goals: Vec::new() }
    }

    /// Add a goal
    pub fn add_goal(&mut self, goal: DomainGoal) -> Uuid {
        let id = goal.id;
        self.goals.push(goal);
        id
    }

    /// Get all goals
    pub fn all_goals(&self) -> &[DomainGoal] {
        &self.goals
    }

    /// Get goals for domain
    pub fn goals_for_domain(&self, domain: &str) -> Vec<&DomainGoal> {
        self.goals.iter().filter(|g| g.domain == domain).collect()
    }

    /// Update goal progress
    pub fn update_progress(&mut self, goal_id: Uuid, progress: u8) -> bool {
        if let Some(goal) = self.goals.iter_mut().find(|g| g.id == goal_id) {
            goal.set_progress(progress);
            return true;
        }
        false
    }

    /// Get domain status based on goals
    pub fn domain_status(&self, domain: &str) -> DomainStatus {
        let domain_goals: Vec<_> = self
            .goals
            .iter()
            .filter(|g| g.domain == domain)
            .collect();

        if domain_goals.is_empty() {
            return DomainStatus::Balanced;
        }

        let completed = domain_goals.iter().filter(|g| g.completed).count();
        let total = domain_goals.len();
        let avg_progress: u32 = domain_goals.iter().map(|g| g.progress as u32).sum::<u32>()
            / total as u32;

        if completed == total {
            DomainStatus::Thriving
        } else if avg_progress >= 70 {
            DomainStatus::Balanced
        } else if avg_progress >= 40 {
            DomainStatus::Needs
        } else {
            DomainStatus::Critical
        }
    }

    /// Calculate domain balance score (0-100)
    pub fn calculate_balance_score(&self) -> f64 {
        let domains = ["Health", "Mental", "Financial", "Social", "Career", "Learning"];
        let statuses: Vec<_> = domains.iter().map(|d| self.domain_status(d)).collect();

        let score: f64 = statuses
            .iter()
            .map(|s| match s {
                DomainStatus::Thriving => 100.0,
                DomainStatus::Balanced => 75.0,
                DomainStatus::Needs => 40.0,
                DomainStatus::Critical => 0.0,
            })
            .sum::<f64>()
            / statuses.len() as f64;

        score
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
    fn test_domain_names() {
        assert_eq!(LifeDomain::Health.name(), "Health");
        assert_eq!(LifeDomain::Career.name(), "Career");
    }

    #[test]
    fn test_domain_status_codes() {
        assert_eq!(DomainStatus::Thriving.code(), "THRIVING");
        assert_eq!(DomainStatus::Critical.code(), "CRITICAL");
    }

    #[test]
    fn test_goal_creation() {
        let goal = DomainGoal::new(
            "Run a 5K".to_string(),
            LifeDomain::Health,
            9,
        );
        assert_eq!(goal.priority, 9);
        assert_eq!(goal.progress, 0);
        assert!(!goal.completed);
    }

    #[test]
    fn test_goal_progress() {
        let mut goal = DomainGoal::new(
            "Run a 5K".to_string(),
            LifeDomain::Health,
            9,
        );
        goal.set_progress(100);
        assert!(goal.completed);
    }

    #[test]
    fn test_domain_manager() {
        let mut manager = DomainManager::new();
        let goal = DomainGoal::new(
            "Get fit".to_string(),
            LifeDomain::Health,
            10,
        );
        manager.add_goal(goal);
        assert_eq!(manager.all_goals().len(), 1);
    }

    #[test]
    fn test_balance_score() {
        let manager = DomainManager::new();
        let score = manager.calculate_balance_score();
        assert!(score >= 0.0 && score <= 100.0);
    }
}
