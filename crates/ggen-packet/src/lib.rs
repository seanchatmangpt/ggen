//! Packet discipline and work order types for ggen
//!
//! This crate provides strongly-typed work order packets with:
//! - Validation at boundaries
//! - Routing strategies
//! - Parsing from typed formats
//! - Reversibility policies
//! - Acceptance criteria

/// Parser module for work order packets
pub mod parser;
/// Router module for packet routing strategies
pub mod router;
/// Validation module for packet boundary checks
pub mod validation;

use chrono::DateTime;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use uuid::Uuid;
use validator::Validate;

/// Work order packet with strong typing and validation
#[derive(Debug, Clone, Serialize, Deserialize, Validate, PartialEq, Eq)]
pub struct WorkOrder {
    /// Objective of the work order (1-1000 chars)
    #[validate(length(min = 1, max = 1000))]
    pub objective: String,

    /// Constraints on work order execution (max 100)
    #[validate(length(min = 0, max = 100))]
    pub constraints: Vec<Constraint>,

    /// Acceptance test criteria
    pub acceptance_test: AcceptanceTest,

    /// Reversibility policy
    pub reversibility: ReversibilityPolicy,

    /// Dependencies on other work orders (max 50)
    #[validate(length(min = 0, max = 50))]
    pub dependencies: Vec<String>,

    /// Owner of the work order (1-100 chars)
    #[validate(length(min = 1, max = 100))]
    pub owner: String,

    /// Unique identifier
    pub id: WorkOrderId,

    /// Creation timestamp
    pub created_at: DateTime<chrono::Utc>,

    /// Current status
    pub status: WorkOrderStatus,

    /// Priority level
    pub priority: Priority,

    /// Tags for categorization (max 20)
    #[validate(length(min = 0, max = 20))]
    pub tags: HashSet<String>,
}

/// Unique work order identifier
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct WorkOrderId(Uuid);

impl WorkOrderId {
    /// Generate a new unique work order ID
    #[must_use]
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    /// Create a work order ID from an existing UUID
    #[must_use]
    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }

    /// Get the underlying UUID
    #[must_use]
    pub const fn as_uuid(&self) -> &Uuid {
        &self.0
    }
}

impl Default for WorkOrderId {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for WorkOrderId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Constraint on work order execution
#[derive(Debug, Clone, Serialize, Deserialize, Validate, PartialEq, Eq, Hash)]
pub struct Constraint {
    /// Description of the constraint (1-200 chars)
    #[validate(length(min = 1, max = 200))]
    pub description: String,

    /// Type of constraint
    pub constraint_type: ConstraintType,

    /// Whether the constraint is enforced
    pub enforced: bool,
}

/// Types of constraints
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConstraintType {
    /// Time-based constraint
    Time,
    /// Resource constraint
    Resource,
    /// Quality constraint
    Quality,
    /// Safety constraint
    Safety,
    /// Compliance constraint
    Compliance,
    /// Budget constraint
    Budget,
    /// Dependency constraint
    Dependency,
}

/// Acceptance criteria for work completion
#[derive(Debug, Clone, Serialize, Deserialize, Validate, PartialEq, Eq)]
pub struct AcceptanceTest {
    /// Description of the acceptance test (1-500 chars)
    #[validate(length(min = 1, max = 500))]
    pub description: String,

    /// List of criteria (1-50 items)
    #[validate(length(min = 1, max = 50))]
    pub criteria: Vec<AcceptanceCriterion>,

    /// Type of test
    pub test_type: TestType,
}

/// Individual acceptance criterion
#[derive(Debug, Clone, Serialize, Deserialize, Validate, PartialEq, Eq)]
pub struct AcceptanceCriterion {
    /// Criterion description (1-200 chars)
    #[validate(length(min = 1, max = 200))]
    pub criterion: String,

    /// Whether this criterion is required
    pub required: bool,

    /// Whether this criterion is measurable
    pub measurable: bool,
}

/// Types of acceptance tests
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum TestType {
    /// Manual testing
    Manual,
    /// Automated testing
    Automated,
    /// Hybrid manual and automated
    Hybrid,
}

/// Reversibility policy for work orders
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReversibilityPolicy {
    /// Whether the work order is reversible
    pub reversible: bool,

    /// Steps to rollback if needed
    pub rollback_steps: Vec<String>,

    /// Whether backup is required before execution
    pub backup_required: bool,

    /// Whether verification is required after execution
    pub verification_required: bool,
}

/// Work order status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum WorkOrderStatus {
    /// Pending validation
    Pending,
    /// Validated and ready
    Validated,
    /// Currently in progress
    InProgress,
    /// Blocked by dependencies or issues
    Blocked,
    /// Successfully completed
    Completed,
    /// Cancelled before completion
    Cancelled,
    /// Failed during execution
    Failed,
}

/// Work order priority
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Priority {
    /// Critical priority (highest)
    Critical = 4,
    /// High priority
    High = 3,
    /// Normal priority
    Normal = 2,
    /// Low priority
    Low = 1,
}

impl WorkOrder {
    /// Create a new work order
    pub fn new(
        objective: String,
        owner: String,
    ) -> Result<Self, validation::ValidationError> {
        let work_order = Self {
            objective,
            constraints: Vec::new(),
            acceptance_test: AcceptanceTest {
                description: String::from("Default acceptance test"),
                criteria: vec![AcceptanceCriterion {
                    criterion: String::from("Work completed successfully"),
                    required: true,
                    measurable: true,
                }],
                test_type: TestType::Manual,
            },
            reversibility: ReversibilityPolicy {
                reversible: false,
                rollback_steps: Vec::new(),
                backup_required: false,
                verification_required: false,
            },
            dependencies: Vec::new(),
            owner,
            id: WorkOrderId::new(),
            created_at: chrono::Utc::now(),
            status: WorkOrderStatus::Pending,
            priority: Priority::Normal,
            tags: HashSet::new(),
        };

        validation::validate_work_order(&work_order)?;
        Ok(work_order)
    }

    /// Add a constraint to the work order
    pub fn with_constraint(mut self, constraint: Constraint) -> Result<Self, validation::ValidationError> {
        self.constraints.push(constraint);
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Set acceptance test
    pub fn with_acceptance_test(mut self, test: AcceptanceTest) -> Result<Self, validation::ValidationError> {
        self.acceptance_test = test;
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Set reversibility policy
    pub fn with_reversibility(mut self, policy: ReversibilityPolicy) -> Result<Self, validation::ValidationError> {
        self.reversibility = policy;
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Add dependencies
    pub fn with_dependencies(mut self, deps: Vec<String>) -> Result<Self, validation::ValidationError> {
        self.dependencies = deps;
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Set priority
    pub fn with_priority(mut self, priority: Priority) -> Result<Self, validation::ValidationError> {
        self.priority = priority;
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Add tags
    pub fn with_tags(mut self, tags: HashSet<String>) -> Result<Self, validation::ValidationError> {
        self.tags = tags;
        validation::validate_work_order(&self)?;
        Ok(self)
    }

    /// Transition to a new status
    pub fn transition_to(&mut self, status: WorkOrderStatus) -> Result<(), validation::ValidationError> {
        self.status = status;
        validation::validate_work_order(self)?;
        Ok(())
    }

    /// Check if work order is in a terminal state
    #[must_use]
    pub const fn is_terminal(&self) -> bool {
        matches!(
            self.status,
            WorkOrderStatus::Completed | WorkOrderStatus::Cancelled | WorkOrderStatus::Failed
        )
    }

    /// Check if work order is active
    #[must_use]
    pub const fn is_active(&self) -> bool {
        matches!(self.status, WorkOrderStatus::InProgress)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_work_order_creation() {
        let result = WorkOrder::new(
            String::from("Implement feature X"),
            String::from("team@example.com"),
        );
        assert!(result.is_ok());

        let wo = result.ok().unwrap();
        assert_eq!(wo.objective, "Implement feature X");
        assert_eq!(wo.owner, "team@example.com");
        assert_eq!(wo.status, WorkOrderStatus::Pending);
        assert_eq!(wo.priority, Priority::Normal);
    }

    #[test]
    fn test_work_order_validation_empty_objective() {
        let result = WorkOrder::new(String::new(), String::from("owner"));
        assert!(result.is_err());
    }

    #[test]
    fn test_work_order_validation_empty_owner() {
        let result = WorkOrder::new(String::from("objective"), String::new());
        assert!(result.is_err());
    }

    #[test]
    fn test_work_order_with_constraint() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let constraint = Constraint {
            description: String::from("Must complete in 2 hours"),
            constraint_type: ConstraintType::Time,
            enforced: true,
        };

        let result = wo.with_constraint(constraint);
        assert!(result.is_ok());
        assert_eq!(result.ok().unwrap().constraints.len(), 1);
    }

    #[test]
    fn test_work_order_state_transitions() {
        let mut wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        assert!(wo.transition_to(WorkOrderStatus::Validated).is_ok());
        assert_eq!(wo.status, WorkOrderStatus::Validated);

        assert!(wo.transition_to(WorkOrderStatus::InProgress).is_ok());
        assert!(wo.is_active());

        assert!(wo.transition_to(WorkOrderStatus::Completed).is_ok());
        assert!(wo.is_terminal());
    }

    #[test]
    fn test_priority_ordering() {
        assert!(Priority::Critical > Priority::High);
        assert!(Priority::High > Priority::Normal);
        assert!(Priority::Normal > Priority::Low);
    }

    #[test]
    fn test_work_order_id_uniqueness() {
        let id1 = WorkOrderId::new();
        let id2 = WorkOrderId::new();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_reversibility_policy() {
        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let policy = ReversibilityPolicy {
            reversible: true,
            rollback_steps: vec![
                String::from("Step 1: Backup data"),
                String::from("Step 2: Restore previous state"),
            ],
            backup_required: true,
            verification_required: true,
        };

        let result = wo.with_reversibility(policy);
        assert!(result.is_ok());

        let wo = result.ok().unwrap();
        assert!(wo.reversibility.reversible);
        assert_eq!(wo.reversibility.rollback_steps.len(), 2);
    }

    #[test]
    fn test_acceptance_test() {
        let test = AcceptanceTest {
            description: String::from("Integration test suite"),
            criteria: vec![
                AcceptanceCriterion {
                    criterion: String::from("All unit tests pass"),
                    required: true,
                    measurable: true,
                },
                AcceptanceCriterion {
                    criterion: String::from("Code coverage > 80%"),
                    required: true,
                    measurable: true,
                },
            ],
            test_type: TestType::Automated,
        };

        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap()
        .with_acceptance_test(test);

        assert!(wo.is_ok());
        let wo = wo.ok().unwrap();
        assert_eq!(wo.acceptance_test.criteria.len(), 2);
        assert_eq!(wo.acceptance_test.test_type, TestType::Automated);
    }

    #[test]
    fn test_work_order_with_tags() {
        let mut tags = HashSet::new();
        tags.insert(String::from("urgent"));
        tags.insert(String::from("backend"));

        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap()
        .with_tags(tags);

        assert!(wo.is_ok());
        let wo = wo.ok().unwrap();
        assert_eq!(wo.tags.len(), 2);
        assert!(wo.tags.contains("urgent"));
    }
}
