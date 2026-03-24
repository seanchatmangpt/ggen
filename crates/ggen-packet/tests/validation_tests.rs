//! Validation tests for ggen-packet crate

use ggen_packet::*;
use ggen_packet::router::RoutingStrategy;
use std::collections::HashSet;

#[test]
fn test_reject_invalid_packet_at_boundary() {
    // Empty objective should be rejected
    let result = WorkOrder::new(String::new(), String::from("owner"));
    assert!(result.is_err());

    // Empty owner should be rejected
    let result = WorkOrder::new(String::from("objective"), String::new());
    assert!(result.is_err());

    // Whitespace-only should be rejected
    let result = WorkOrder::new(String::from("   "), String::from("owner"));
    assert!(result.is_err());
}

#[test]
fn test_accept_valid_packet_at_boundary() {
    let result = WorkOrder::new(
        String::from("Implement authentication"),
        String::from("security-team@example.com"),
    );
    assert!(result.is_ok());

    let wo = result.ok().unwrap();
    assert_eq!(wo.objective, "Implement authentication");
    assert_eq!(wo.owner, "security-team@example.com");
    assert_eq!(wo.status, WorkOrderStatus::Pending);
}

#[test]
fn test_constraint_validation() {
    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap();

    // Valid constraint
    let constraint = Constraint {
        description: String::from("Must complete in 4 hours"),
        constraint_type: ConstraintType::Time,
        enforced: true,
    };
    assert!(wo.clone().with_constraint(constraint).is_ok());

    // Invalid constraint (empty description)
    let constraint = Constraint {
        description: String::from("   "),
        constraint_type: ConstraintType::Time,
        enforced: true,
    };
    assert!(wo.with_constraint(constraint).is_err());
}

#[test]
fn test_reversibility_validation() {
    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap();

    // Valid reversibility policy
    let policy = ReversibilityPolicy {
        reversible: true,
        rollback_steps: vec![String::from("Step 1"), String::from("Step 2")],
        backup_required: true,
        verification_required: true,
    };
    assert!(wo.clone().with_reversibility(policy).is_ok());

    // Invalid: reversible but no rollback steps
    let policy = ReversibilityPolicy {
        reversible: true,
        rollback_steps: Vec::new(),
        backup_required: false,
        verification_required: false,
    };
    assert!(wo.clone().with_reversibility(policy).is_err());

    // Invalid: backup required but not reversible
    let policy = ReversibilityPolicy {
        reversible: false,
        rollback_steps: Vec::new(),
        backup_required: true,
        verification_required: false,
    };
    assert!(wo.with_reversibility(policy).is_err());
}

#[test]
fn test_acceptance_test_validation() {
    // Valid acceptance test
    let test = AcceptanceTest {
        description: String::from("Integration test suite"),
        criteria: vec![
            AcceptanceCriterion {
                criterion: String::from("All tests pass"),
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

    // Invalid: no criteria - should fail when creating work order
    let test = AcceptanceTest {
        description: String::from("Test"),
        criteria: Vec::new(),
        test_type: TestType::Manual,
    };

    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap()
    .with_acceptance_test(test);
    assert!(wo.is_err());

    // Invalid: no required criteria
    let test = AcceptanceTest {
        description: String::from("Test"),
        criteria: vec![
            AcceptanceCriterion {
                criterion: String::from("Optional check"),
                required: false,
                measurable: true,
            },
        ],
        test_type: TestType::Manual,
    };

    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap()
    .with_acceptance_test(test);
    assert!(wo.is_err());
}

#[test]
fn test_dependency_validation() {
    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap();

    // Valid dependencies
    let deps = vec![String::from("dep1"), String::from("dep2")];
    assert!(wo.clone().with_dependencies(deps).is_ok());

    // Invalid: duplicate dependencies
    let deps = vec![
        String::from("dep1"),
        String::from("dep2"),
        String::from("dep1"),
    ];
    assert!(wo.clone().with_dependencies(deps).is_err());

    // Invalid: empty dependency
    let deps = vec![String::from("dep1"), String::new()];
    assert!(wo.with_dependencies(deps).is_err());
}

#[test]
fn test_work_order_lifecycle() {
    let mut wo = WorkOrder::new(
        String::from("Deploy service"),
        String::from("devops@example.com"),
    )
    .ok()
    .unwrap();

    // Initial state
    assert_eq!(wo.status, WorkOrderStatus::Pending);
    assert!(!wo.is_active());
    assert!(!wo.is_terminal());

    // Validate
    assert!(wo.transition_to(WorkOrderStatus::Validated).is_ok());
    assert!(!wo.is_active());

    // Start work
    assert!(wo.transition_to(WorkOrderStatus::InProgress).is_ok());
    assert!(wo.is_active());
    assert!(!wo.is_terminal());

    // Complete
    assert!(wo.transition_to(WorkOrderStatus::Completed).is_ok());
    assert!(!wo.is_active());
    assert!(wo.is_terminal());
}

#[test]
fn test_priority_based_routing() {
    let router = router::PriorityRouter::new("default".into())
        .with_mapping(Priority::Critical, "critical-channel".into())
        .with_mapping(Priority::High, "high-channel".into());

    let wo_critical = WorkOrder::new(
        String::from("Critical task"),
        String::from("owner"),
    )
    .ok()
    .unwrap()
    .with_priority(Priority::Critical)
    .ok()
    .unwrap();

    let channel = router.route(&wo_critical).ok().unwrap();
    assert_eq!(channel.as_str(), "critical-channel");

    let wo_normal = WorkOrder::new(
        String::from("Normal task"),
        String::from("owner"),
    )
    .ok()
    .unwrap();

    let channel = router.route(&wo_normal).ok().unwrap();
    assert_eq!(channel.as_str(), "default");
}

#[test]
fn test_tag_based_routing() {
    let router = router::TagRouter::new("default".into())
        .with_mapping(String::from("backend"), "backend-channel".into())
        .with_mapping(String::from("database"), "db-channel".into());

    let mut tags = HashSet::new();
    tags.insert(String::from("backend"));
    tags.insert(String::from("api"));

    let wo = WorkOrder::new(
        String::from("Task"),
        String::from("owner"),
    )
    .ok()
    .unwrap()
    .with_tags(tags)
    .ok()
    .unwrap();

    let channel = router.route(&wo).ok().unwrap();
    assert_eq!(channel.as_str(), "backend-channel");
}

#[test]
fn test_parser_json_roundtrip() {
    let original = WorkOrder::new(
        String::from("Test task"),
        String::from("test@example.com"),
    )
    .ok()
    .unwrap()
    .with_priority(Priority::High)
    .ok()
    .unwrap();

    // Serialize to JSON
    let json = serde_json::to_string(&original).ok().unwrap();

    // Parse back
    let parsed = parser::parse_work_order(&json).ok().unwrap();

    assert_eq!(original.objective, parsed.objective);
    assert_eq!(original.owner, parsed.owner);
    assert_eq!(original.priority, parsed.priority);
}

#[test]
fn test_full_packet_workflow() {
    // Create work order
    let constraint = Constraint {
        description: String::from("Complete within SLA"),
        constraint_type: ConstraintType::Time,
        enforced: true,
    };

    let mut tags = HashSet::new();
    tags.insert(String::from("production"));
    tags.insert(String::from("backend"));

    let mut wo = WorkOrder::new(
        String::from("Deploy microservice update"),
        String::from("platform-team@example.com"),
    )
    .ok()
    .unwrap()
    .with_constraint(constraint)
    .ok()
    .unwrap()
    .with_priority(Priority::High)
    .ok()
    .unwrap()
    .with_tags(tags)
    .ok()
    .unwrap();

    // Validate at boundary
    assert!(validation::validate_work_order(&wo).is_ok());

    // Route packet
    let router = router::PriorityRouter::new("default".into())
        .with_mapping(Priority::High, "high-priority-queue".into());

    let channel = router.route(&wo).ok().unwrap();
    assert_eq!(channel.as_str(), "high-priority-queue");

    // Execute workflow
    assert!(wo.transition_to(WorkOrderStatus::Validated).is_ok());
    assert!(wo.transition_to(WorkOrderStatus::InProgress).is_ok());
    assert!(wo.is_active());

    // Complete
    assert!(wo.transition_to(WorkOrderStatus::Completed).is_ok());
    assert!(wo.is_terminal());
}
