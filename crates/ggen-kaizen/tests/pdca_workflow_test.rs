//! Integration tests for PDCA workflow.

use ggen_kaizen::{pdca::*, Category, Improvement, Priority};

fn create_test_improvement() -> Improvement {
    Improvement::new(
        "IMP-TEST-001".to_string(),
        "Reduce build time".to_string(),
        "Optimize compilation process to reduce build time from 15s to 10s".to_string(),
        Category::Performance,
        Priority::High,
        "build-team@example.com".to_string(),
    )
}

#[test]
fn test_complete_pdca_workflow() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Plan phase
    let plan_result = cycle.start_phase(
        PdcaState::Do,
        "Identified bottlenecks and created optimization plan".to_string(),
    );
    assert!(plan_result.is_ok());
    assert_eq!(cycle.current_state, PdcaState::Do);

    // Add objectives to Plan phase
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Profile compilation process".to_string());
        phase.add_objective("Identify slow dependencies".to_string());
        phase.add_outcome("Created detailed optimization plan".to_string());
    }

    // Act - Do phase
    let do_result = cycle.start_phase(
        PdcaState::Check,
        "Implemented caching and parallel compilation".to_string(),
    );
    assert!(do_result.is_ok());
    assert_eq!(cycle.current_state, PdcaState::Check);

    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Implement incremental compilation".to_string());
        phase.add_outcome("Reduced build time to 12s".to_string());
    }

    // Act - Check phase
    let check_result = cycle.start_phase(
        PdcaState::Act,
        "Measured results and validated improvements".to_string(),
    );
    assert!(check_result.is_ok());
    assert_eq!(cycle.current_state, PdcaState::Act);

    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Validate build time improvements".to_string());
        phase.add_outcome("Confirmed 20% build time reduction".to_string());
    }

    // Assert - Complete cycle
    let complete_result = cycle.complete();
    assert!(complete_result.is_ok());
    assert!(cycle.is_completed());
    assert_eq!(cycle.current_state, PdcaState::Completed);
    assert_eq!(cycle.phases.len(), 3);
}

#[test]
fn test_pdca_iteration_cycle() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Complete first cycle
    cycle.advance("Do phase".to_string()).ok();
    cycle.advance("Check phase".to_string()).ok();
    cycle.advance("Act phase".to_string()).ok();

    // Act - Iterate instead of completing
    let iterate_result = cycle.iterate("Starting second iteration with refined approach".to_string());

    // Assert
    assert!(iterate_result.is_ok());
    assert_eq!(cycle.current_state, PdcaState::Plan);
    assert_eq!(cycle.phases.len(), 4); // Plan, Do, Check, Act from first iteration + new Plan
    assert!(!cycle.is_completed());
}

#[test]
fn test_pdca_invalid_state_transitions() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act & Assert - Try to skip from Plan to Check
    let result = cycle.start_phase(PdcaState::Check, "Invalid transition".to_string());
    assert!(result.is_err());
    assert_eq!(cycle.current_state, PdcaState::Plan);

    // Act & Assert - Try to complete without being in Act state
    let complete_result = cycle.complete();
    assert!(complete_result.is_err());
}

#[test]
fn test_pdca_phase_duration_tracking() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Progress through phases
    cycle.advance("Do phase".to_string()).ok();

    // Complete the Do phase
    if let Some(phase) = cycle.phases.last_mut() {
        phase.complete();
    }

    cycle.advance("Check phase".to_string()).ok();

    // Assert - Check phase duration
    let do_duration = cycle.state_duration(PdcaState::Do);
    assert!(do_duration >= chrono::Duration::zero());
}

#[test]
fn test_pdca_multiple_cycles() {
    // Arrange
    let improvement1 = create_test_improvement();
    let improvement2 = Improvement::new(
        "IMP-TEST-002".to_string(),
        "Improve test coverage".to_string(),
        "Increase coverage from 80% to 90%".to_string(),
        Category::Quality,
        Priority::Medium,
        "qa-team@example.com".to_string(),
    );

    // Act
    let mut cycle1 = PdcaCycle::new(improvement1, 1);
    let mut cycle2 = PdcaCycle::new(improvement2, 1);

    cycle1.advance("Do".to_string()).ok();
    cycle2.advance("Do".to_string()).ok();

    // Assert
    assert_eq!(cycle1.current_state, PdcaState::Do);
    assert_eq!(cycle2.current_state, PdcaState::Do);
    assert_eq!(cycle1.improvement.id, "IMP-TEST-001");
    assert_eq!(cycle2.improvement.id, "IMP-TEST-002");
}

#[test]
fn test_pdca_cycle_with_outcomes() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Progress and add outcomes at each phase
    cycle.advance("Do phase".to_string()).ok();
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_outcome("Implemented caching mechanism".to_string());
        phase.add_outcome("Enabled parallel compilation".to_string());
    }

    cycle.advance("Check phase".to_string()).ok();
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_outcome("Build time reduced to 12s".to_string());
        phase.add_outcome("No regression in functionality".to_string());
    }

    // Assert
    assert_eq!(cycle.phases[0].outcomes.len(), 2);
    assert_eq!(cycle.phases[1].outcomes.len(), 2);
}

#[test]
fn test_pdca_cycle_objectives_tracking() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Add objectives
    cycle.advance("Do phase".to_string()).ok();
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Profile compilation".to_string());
        phase.add_objective("Identify bottlenecks".to_string());
        phase.add_objective("Implement optimizations".to_string());
    }

    // Assert
    let phase = cycle.current_phase();
    assert!(phase.is_some());
    assert_eq!(phase.unwrap().objectives.len(), 3);
}

#[test]
fn test_pdca_advance_convenience_method() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Use advance instead of start_phase
    let result1 = cycle.advance("Moving to Do".to_string());
    let result2 = cycle.advance("Moving to Check".to_string());
    let result3 = cycle.advance("Moving to Act".to_string());

    // Assert
    assert!(result1.is_ok());
    assert!(result2.is_ok());
    assert!(result3.is_ok());
    assert_eq!(cycle.current_state, PdcaState::Act);
    assert_eq!(cycle.phases.len(), 3);
}

#[test]
fn test_pdca_cannot_advance_after_completion() {
    // Arrange
    let improvement = create_test_improvement();
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Complete the cycle
    cycle.advance("Do".to_string()).ok();
    cycle.advance("Check".to_string()).ok();
    cycle.advance("Act".to_string()).ok();
    cycle.complete().ok();

    // Act & Assert - Try to advance after completion
    let result = cycle.advance("Invalid".to_string());
    assert!(result.is_err());
}
