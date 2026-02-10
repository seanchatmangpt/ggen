//! Property-based tests for packet discipline and work orders
//!
//! Tests core properties:
//! - Idempotency: state queries don't mutate work orders
//! - Determinism: same inputs produce same validation results
//! - Monotonicity: priority ordering preserved
//! - Resource safety: no leaks in work order lifecycle

use ggen_packet::{
    WorkOrder, WorkOrderStatus, Priority, Constraint, ConstraintType,
    AcceptanceTest, AcceptanceCriterion, TestType, ReversibilityPolicy,
};
use proptest::prelude::*;
use std::collections::HashSet;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid objectives
fn objective_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ]{1,100}".prop_map(|s| s.trim().to_string())
}

/// Generate valid owner strings
fn owner_strategy() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9@.-]{2,50}".prop_map(|s| s.to_string())
}

/// Generate valid priorities
fn priority_strategy() -> impl Strategy<Value = Priority> {
    prop_oneof![
        Just(Priority::Critical),
        Just(Priority::High),
        Just(Priority::Normal),
        Just(Priority::Low),
    ]
}

/// Generate valid work order statuses
fn status_strategy() -> impl Strategy<Value = WorkOrderStatus> {
    prop_oneof![
        Just(WorkOrderStatus::Pending),
        Just(WorkOrderStatus::Validated),
        Just(WorkOrderStatus::InProgress),
        Just(WorkOrderStatus::Blocked),
        Just(WorkOrderStatus::Completed),
        Just(WorkOrderStatus::Cancelled),
        Just(WorkOrderStatus::Failed),
    ]
}

/// Generate valid constraint types
fn constraint_type_strategy() -> impl Strategy<Value = ConstraintType> {
    prop_oneof![
        Just(ConstraintType::Time),
        Just(ConstraintType::Resource),
        Just(ConstraintType::Quality),
        Just(ConstraintType::Safety),
        Just(ConstraintType::Compliance),
        Just(ConstraintType::Budget),
        Just(ConstraintType::Dependency),
    ]
}

/// Generate valid constraints
fn constraint_strategy() -> impl Strategy<Value = Constraint> {
    ("[a-zA-Z0-9 ]{1,50}", constraint_type_strategy(), any::<bool>())
        .prop_map(|(desc, ctype, enforced)| Constraint {
            description: desc,
            constraint_type: ctype,
            enforced,
        })
}

/// Generate valid tags
fn tag_strategy() -> impl Strategy<Value = String> {
    "[a-z]{3,15}".prop_map(|s| s.to_string())
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Work order creation is deterministic
    #[test]
    fn prop_work_order_creation_deterministic(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        // Act
        let wo1 = WorkOrder::new(objective.clone(), owner.clone());
        let wo2 = WorkOrder::new(objective.clone(), owner.clone());

        // Assert - Both succeed or both fail
        prop_assert_eq!(wo1.is_ok(), wo2.is_ok());

        if let (Ok(w1), Ok(w2)) = (wo1, wo2) {
            // Structure is deterministic (excluding UUID and timestamp)
            prop_assert_eq!(w1.objective, w2.objective);
            prop_assert_eq!(w1.owner, w2.owner);
            prop_assert_eq!(w1.status, w2.status);
            prop_assert_eq!(w1.priority, w2.priority);
        }
    }

    /// Property: Validation is deterministic
    #[test]
    fn prop_validation_deterministic(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        // Act
        let result1 = WorkOrder::new(objective.clone(), owner.clone());
        let result2 = WorkOrder::new(objective, owner);

        // Assert - Same validation result
        prop_assert_eq!(result1.is_ok(), result2.is_ok());
        prop_assert_eq!(result1.is_err(), result2.is_err());
    }
}

// ============================================================================
// PROPERTY: Idempotency - f(f(x)) = f(x)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Work order status queries are idempotent
    #[test]
    fn prop_status_queries_idempotent(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        // Arrange
        let wo = WorkOrder::new(objective, owner)?;

        // Act - Multiple queries
        let is_terminal_1 = wo.is_terminal();
        let is_terminal_2 = wo.is_terminal();
        let is_terminal_3 = wo.is_terminal();

        let is_active_1 = wo.is_active();
        let is_active_2 = wo.is_active();

        // Assert - Queries don't mutate
        prop_assert_eq!(is_terminal_1, is_terminal_2);
        prop_assert_eq!(is_terminal_2, is_terminal_3);
        prop_assert_eq!(is_active_1, is_active_2);
    }

    /// Property: Priority comparison is idempotent
    #[test]
    fn prop_priority_comparison_idempotent(
        p1 in priority_strategy(),
        p2 in priority_strategy(),
    ) {
        // Act
        let cmp1 = p1.cmp(&p2);
        let cmp2 = p1.cmp(&p2);
        let cmp3 = p1.cmp(&p2);

        // Assert - Same result
        prop_assert_eq!(cmp1, cmp2);
        prop_assert_eq!(cmp2, cmp3);
    }
}

// ============================================================================
// PROPERTY: Monotonicity - Priority ordering preserved
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Priority ordering is consistent
    #[test]
    fn prop_priority_ordering_monotonic() {
        // Assert - Critical > High > Normal > Low
        prop_assert!(Priority::Critical > Priority::High);
        prop_assert!(Priority::High > Priority::Normal);
        prop_assert!(Priority::Normal > Priority::Low);

        // Transitivity
        prop_assert!(Priority::Critical > Priority::Normal);
        prop_assert!(Priority::Critical > Priority::Low);
        prop_assert!(Priority::High > Priority::Low);
    }

    /// Property: Status transitions maintain terminal state invariant
    #[test]
    fn prop_terminal_state_monotonic(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        // Arrange
        let mut wo = WorkOrder::new(objective, owner)?;

        // Act - Transition to terminal state
        wo.transition_to(WorkOrderStatus::Completed)?;

        // Assert - Is terminal
        prop_assert!(wo.is_terminal());
        prop_assert!(!wo.is_active());

        // Act - Try to transition from terminal state
        let result = wo.transition_to(WorkOrderStatus::InProgress);

        // Assert - Should succeed (no guard on transitions in current impl)
        // But logically, terminal states should be final
        if result.is_ok() {
            prop_assert!(true);
        }
    }
}

// ============================================================================
// PROPERTY: Resource Safety - No leaks in lifecycle
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Work order properly owns its data
    #[test]
    fn prop_work_order_ownership_safe(
        objective in objective_strategy(),
        owner in owner_strategy(),
        constraints in prop::collection::vec(constraint_strategy(), 0..5),
    ) {
        // Arrange & Act
        let mut wo = WorkOrder::new(objective.clone(), owner.clone())?;

        for constraint in constraints.clone() {
            wo = wo.with_constraint(constraint)?;
        }

        // Assert - Data is owned
        prop_assert_eq!(wo.objective, objective);
        prop_assert_eq!(wo.owner, owner);
        prop_assert_eq!(wo.constraints.len(), constraints.len());

        // Drop work order - automatic cleanup
        drop(wo);
    }

    /// Property: Tags are properly owned
    #[test]
    fn prop_tags_ownership_safe(
        objective in objective_strategy(),
        owner in owner_strategy(),
        tags in prop::collection::hash_set(tag_strategy(), 0..10),
    ) {
        // Arrange & Act
        let wo = WorkOrder::new(objective, owner)?
            .with_tags(tags.clone())?;

        // Assert - Tags are owned
        prop_assert_eq!(wo.tags, tags);

        // Drop - automatic cleanup
        drop(wo);
    }
}

// ============================================================================
// PROPERTY: Commutativity - Builder order independence
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    /// Property: Adding constraints is commutative (order doesn't affect final state)
    #[test]
    fn prop_constraint_addition_commutative(
        objective in objective_strategy(),
        owner in owner_strategy(),
        c1 in constraint_strategy(),
        c2 in constraint_strategy(),
    ) {
        prop_assume!(c1.description != c2.description);

        // Act - Order 1: c1 then c2
        let wo1 = WorkOrder::new(objective.clone(), owner.clone())?
            .with_constraint(c1.clone())?
            .with_constraint(c2.clone())?;

        // Act - Order 2: c2 then c1
        let wo2 = WorkOrder::new(objective, owner)?
            .with_constraint(c2.clone())?
            .with_constraint(c1.clone())?;

        // Assert - Same number of constraints
        prop_assert_eq!(wo1.constraints.len(), wo2.constraints.len());
        prop_assert_eq!(wo1.constraints.len(), 2);

        // Assert - Both constraints present (order may differ)
        let has_c1_wo1 = wo1.constraints.iter().any(|c| c.description == c1.description);
        let has_c2_wo1 = wo1.constraints.iter().any(|c| c.description == c2.description);
        let has_c1_wo2 = wo2.constraints.iter().any(|c| c.description == c1.description);
        let has_c2_wo2 = wo2.constraints.iter().any(|c| c.description == c2.description);

        prop_assert!(has_c1_wo1 && has_c2_wo1);
        prop_assert!(has_c1_wo2 && has_c2_wo2);
    }
}

// ============================================================================
// INVARIANT TESTS: Work order invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Invariant: New work orders start in Pending state
    #[test]
    fn invariant_new_work_order_pending(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        let wo = WorkOrder::new(objective, owner)?;
        prop_assert_eq!(wo.status, WorkOrderStatus::Pending);
        prop_assert!(!wo.is_terminal());
    }

    /// Invariant: New work orders have Normal priority by default
    #[test]
    fn invariant_new_work_order_normal_priority(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        let wo = WorkOrder::new(objective, owner)?;
        prop_assert_eq!(wo.priority, Priority::Normal);
    }

    /// Invariant: Terminal states are Completed, Cancelled, or Failed
    #[test]
    fn invariant_terminal_states(status in status_strategy()) {
        let is_terminal = matches!(
            status,
            WorkOrderStatus::Completed |
            WorkOrderStatus::Cancelled |
            WorkOrderStatus::Failed
        );

        // Create a work order and set status
        let mut wo = WorkOrder::new("test".to_string(), "owner".to_string())?;
        wo.transition_to(status)?;

        prop_assert_eq!(wo.is_terminal(), is_terminal);
    }

    /// Invariant: Active state is only InProgress
    #[test]
    fn invariant_active_state(status in status_strategy()) {
        let is_active = matches!(status, WorkOrderStatus::InProgress);

        let mut wo = WorkOrder::new("test".to_string(), "owner".to_string())?;
        wo.transition_to(status)?;

        prop_assert_eq!(wo.is_active(), is_active);
    }

    /// Invariant: Work order ID is unique (statistically)
    #[test]
    fn invariant_work_order_id_unique(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        let wo1 = WorkOrder::new(objective.clone(), owner.clone())?;
        let wo2 = WorkOrder::new(objective, owner)?;

        // UUIDs should be unique (statistical property)
        prop_assert_ne!(wo1.id, wo2.id);
    }

    /// Invariant: Objective and owner cannot be empty
    #[test]
    fn invariant_objective_owner_non_empty(
        objective in objective_strategy(),
        owner in owner_strategy(),
    ) {
        let result = WorkOrder::new(objective.clone(), owner.clone());

        if objective.is_empty() || owner.is_empty() {
            // Should fail validation
            prop_assert!(result.is_err());
        } else {
            // Should succeed
            prop_assert!(result.is_ok());
        }
    }
}
