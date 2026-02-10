//! Life Domain Management
//!
//! Manages different domains of life in the OSIRIS system

use serde_json::Value;
use std::collections::HashMap;
use chrono::{DateTime, Utc};

/// Life domain status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DomainStatus {
    /// Domain is active
    Active,
    /// Domain is being developed
    Developing,
    /// Domain is under review
    Reviewing,
    /// Domain is paused
    Paused,
    /// Domain is complete
    Complete,
    /// Domain is archived
    Archived,
}

/// Represents a life domain in the OSIRIS system
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LifeDomain {
    /// Domain identifier
    pub id: String,
    /// Domain name
    pub name: String,
    /// Domain description
    pub description: String,
    /// Domain status
    pub status: DomainStatus,
    /// Domain metrics
    pub metrics: HashMap<String, Value>,
    /// Domain goals
    pub goals: Vec<Goal>,
    /// Domain created at
    pub created_at: DateTime<Utc>,
    /// Domain last updated
    pub updated_at: DateTime<Utc>,
}

/// Life goal
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Goal {
    /// Goal ID
    pub id: String,
    /// Goal title
    pub title: String,
    /// Goal description
    pub description: String,
    /// Goal status
    pub status: GoalStatus,
    /// Target date
    pub target_date: Option<DateTime<Utc>>,
    /// Priority (0-1)
    pub priority: f64,
    /// Progress (0-1)
    pub progress: f64,
    /// Created at
    pub created_at: DateTime<Utc>,
    /// Completed at
    pub completed_at: Option<DateTime<Utc>>,
}

/// Goal status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GoalStatus {
    /// Goal is planned
    Planned,
    /// Goal is in progress
    InProgress,
    /// Goal is completed
    Completed,
    /// Goal is on hold
    OnHold,
    /// Goal is cancelled
    Cancelled,
}

impl LifeDomain {
    /// Create a new life domain
    pub fn new(id: String, name: String, description: String) -> Self {
        Self {
            id,
            name,
            description,
            status: DomainStatus::Active,
            metrics: HashMap::new(),
            goals: Vec::new(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    /// Create a life domain with metrics
    pub fn with_metrics(id: String, name: String, description: String, metrics: HashMap<String, Value>) -> Self {
        let mut domain = Self::new(id, name, description);
        domain.metrics = metrics;
        domain
    }

    /// Update domain status
    pub fn update_status(&mut self, status: DomainStatus) {
        self.status = status;
        self.updated_at = Utc::now();
    }

    /// Add a goal to the domain
    pub fn add_goal(&mut self, goal: Goal) {
        self.goals.push(goal);
        self.updated_at = Utc::now();
    }

    /// Update a goal's progress
    pub fn update_goal_progress(&mut self, goal_id: &str, progress: f64) -> Result<(), String> {
        for goal in &mut self.goals {
            if goal.id == goal_id {
                goal.progress = progress.clamp(0.0, 1.0);
                self.updated_at = Utc::now();

                // Check if goal is completed
                if progress >= 1.0 {
                    goal.status = GoalStatus::Completed;
                    goal.completed_at = Some(Utc::now());
                }

                return Ok(());
            }
        }
        Err(format!("Goal {} not found", goal_id))
    }

    /// Update domain metrics
    pub fn update_metrics(&mut self, metrics: HashMap<String, Value>) {
        self.metrics = metrics;
        self.updated_at = Utc::now();
    }

    /// Calculate domain health score (0-1)
    pub fn calculate_health_score(&self) -> f64 {
        let goals_completed: f64 = self.goals
            .iter()
            .filter(|g| g.status == GoalStatus::Completed)
            .count() as f64;

        let total_goals = self.goals.len() as f64;

        if total_goals == 0.0 {
            return 0.5; // Default score for domains without goals
        }

        goals_completed / total_goals
    }

    /// Check if domain is active
    pub fn is_active(&self) -> bool {
        matches!(self.status, DomainStatus::Active)
    }

    /// Get domain goals by status
    pub fn get_goals_by_status(&self, status: GoalStatus) -> Vec<&Goal> {
        self.goals
            .iter()
            .filter(|g| g.status == status)
            .collect()
    }

    /// Get active goals
    pub fn get_active_goals(&self) -> Vec<&Goal> {
        self.get_goals_by_status(GoalStatus::InProgress)
    }

    /// Get completed goals
    pub fn get_completed_goals(&self) -> Vec<&Goal> {
        self.get_goals_by_status(GoalStatus::Completed)
    }

    /// Calculate domain progress
    pub fn calculate_progress(&self) -> f64 {
        let total_progress: f64 = self.goals.iter().map(|g| g.progress).sum();
        let total_goals = self.goals.len() as f64;

        if total_goals == 0.0 {
            return 0.0;
        }

        total_progress / total_goals
    }
}

impl Goal {
    /// Create a new goal
    pub fn new(id: String, title: String, description: String) -> Self {
        Self {
            id,
            title,
            description,
            status: GoalStatus::Planned,
            target_date: None,
            priority: 0.5,
            progress: 0.0,
            created_at: Utc::now(),
            completed_at: None,
        }
    }

    /// Create a goal with target date
    pub fn with_target_date(id: String, title: String, description: String, target_date: DateTime<Utc>) -> Self {
        let mut goal = Self::new(id, title, description);
        goal.target_date = Some(target_date);
        goal
    }

    /// Create a goal with priority
    pub fn with_priority(id: String, title: String, description: String, priority: f64) -> Self {
        let mut goal = Self::new(id, title, description);
        goal.priority = priority.clamp(0.0, 1.0);
        goal
    }

    /// Start working on the goal
    pub fn start(&mut self) {
        self.status = GoalStatus::InProgress;
    }

    /// Update progress
    pub fn update_progress(&mut self, progress: f64) {
        self.progress = progress.clamp(0.0, 1.0);

        if self.progress >= 1.0 {
            self.complete();
        }
    }

    /// Complete the goal
    pub fn complete(&mut self) {
        self.status = GoalStatus::Completed;
        self.progress = 1.0;
        self.completed_at = Some(Utc::now());
    }

    /// Put goal on hold
    pub fn put_on_hold(&mut self) {
        self.status = GoalStatus::OnHold;
    }

    /// Cancel the goal
    pub fn cancel(&mut self) {
        self.status = GoalStatus::Cancelled;
    }

    /// Check if goal is overdue
    pub fn is_overdue(&self) -> bool {
        match (self.target_date, self.status) {
            (Some(date), GoalStatus::InProgress | GoalStatus::Planned) => date < Utc::now(),
            _ => false,
        }
    }

    /// Check if goal is due soon (within 7 days)
    pub fn is_due_soon(&self) -> bool {
        match (self.target_date, self.status) {
            (Some(date), GoalStatus::InProgress | GoalStatus::Planned) => {
                let now = Utc::now();
                let days_until_due = (date - now).num_days();
                days_until_due <= 7 && days_until_due >= 0
            }
            _ => false,
        }
    }

    /// Get time remaining until target date
    pub fn time_remaining(&self) -> Option<chrono::Duration> {
        match self.target_date {
            Some(date) if self.status == GoalStatus::InProgress || self.status == GoalStatus::Planned => {
                Some(date - Utc::now())
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_life_domain_creation() {
        let domain = LifeDomain::new(
            "health".to_string(),
            "Health".to_string(),
            "Health and wellness domain".to_string(),
        );

        assert_eq!(domain.id, "health");
        assert_eq!(domain.name, "Health");
        assert_eq!(domain.status, DomainStatus::Active);
        assert!(domain.metrics.is_empty());
        assert!(domain.goals.is_empty());
    }

    #[test]
    fn test_life_domain_with_metrics() {
        let mut metrics = HashMap::new();
        metrics.insert("score".to_string(), serde_json::json!(0.8));
        metrics.insert("activities".to_string(), serde_json::json!(5));

        let domain = LifeDomain::with_metrics(
            "fitness".to_string(),
            "Fitness".to_string(),
            "Physical fitness domain".to_string(),
            metrics,
        );

        assert_eq!(domain.metrics["score"], 0.8);
        assert_eq!(domain.metrics["activities"], 5);
    }

    #[test]
    fn test_goal_creation() {
        let goal = Goal::new(
            "weight_loss".to_string(),
            "Lose Weight".to_string(),
            "Lose 10 pounds".to_string(),
        );

        assert_eq!(goal.id, "weight_loss");
        assert_eq!(goal.title, "Lose Weight");
        assert_eq!(goal.status, GoalStatus::Planned);
        assert_eq!(goal.progress, 0.0);
        assert!(goal.target_date.is_none());
    }

    #[test]
    fn test_goal_with_target_date() {
        let target_date = Utc::now() + chrono::Duration::days(30);
        let goal = Goal::with_target_date(
            "goal1".to_string(),
            "Test Goal".to_string(),
            "Test description".to_string(),
            target_date,
        );

        assert!(goal.target_date.is_some());
        assert_eq!(goal.target_date.unwrap(), target_date);
    }

    #[test]
    fn test_goal_priority() {
        let goal = Goal::with_priority(
            "goal1".to_string(),
            "High Priority Goal".to_string(),
            "Description".to_string(),
            0.9,
        );

        assert_eq!(goal.priority, 0.9);
    }

    #[test]
    fn test_goal_progress_updates() {
        let mut goal = Goal::new("goal1".to_string(), "Test".to_string(), "Test");

        // Start goal
        goal.start();
        assert_eq!(goal.status, GoalStatus::InProgress);

        // Update progress
        goal.update_progress(0.5);
        assert_eq!(goal.progress, 0.5);

        // Complete goal
        goal.update_progress(1.0);
        assert_eq!(goal.status, GoalStatus::Completed);
        assert_eq!(goal.progress, 1.0);
    }

    #[test]
    fn test_goal_overdue_check() {
        let mut goal = Goal::new("goal1".to_string(), "Test".to_string(), "Test");

        // Set target date in the past
        goal.target_date = Some(Utc::now() - chrono::Duration::days(1));
        goal.start();

        assert!(goal.is_overdue());
        assert!(!goal.is_due_soon());
    }

    #[test]
    fn test_goal_due_soon_check() {
        let mut goal = Goal::new("goal1".to_string(), "Test".to_string(), "Test");

        // Set target date in 3 days
        goal.target_date = Some(Utc::now() + chrono::Duration::days(3));
        goal.start();

        assert!(!goal.is_overdue());
        assert!(goal.is_due_soon());
    }

    #[test]
    fn test_domain_health_score() {
        let mut domain = LifeDomain::new("domain1".to_string(), "Domain".to_string(), "Test");

        // No goals should return 0.5
        assert_eq!(domain.calculate_health_score(), 0.5);

        // Add completed goal
        domain.add_goal(Goal::new("goal1".to_string(), "Goal".to_string(), "Completed"));
        let mut goal = domain.goals.first_mut().unwrap();
        goal.complete();
        assert_eq!(domain.calculate_health_score(), 1.0);
    }

    #[test]
    fn test_domain_progress_calculation() {
        let mut domain = LifeDomain::new("domain1".to_string(), "Domain".to_string(), "Test");

        // No goals should return 0.0
        assert_eq!(domain.calculate_progress(), 0.0);

        // Add goals with different progress
        domain.add_goal(Goal::with_progress("goal1".to_string(), "Goal 1".to_string(), "0.5"));
        domain.add_goal(Goal::with_progress("goal2".to_string(), "Goal 2".to_string(), "0.8"));

        // Average should be (0.5 + 0.8) / 2 = 0.65
        assert!((domain.calculate_progress() - 0.65).abs() < 0.001);
    }

    // Helper function for testing
    impl Goal {
        fn with_progress(id: String, title: String, progress_str: &str) -> Self {
            let mut goal = Goal::new(id, title, "Test");
            goal.update_progress(progress_str.parse().unwrap());
            goal
        }
    }
}