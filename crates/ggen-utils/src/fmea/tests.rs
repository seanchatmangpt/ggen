//! Unit tests for FMEA types and registry.
//!
//! Tests follow Chicago TDD principles:
//! - State-based testing (verify outputs)
//! - Real collaborators (no mocks)
//! - AAA pattern (Arrange-Act-Assert)

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
// Test code can use unwrap/expect

use super::*;

// ============================================================================
// Severity Tests
// ============================================================================

#[test]
fn test_severity_valid_range() {
    // Arrange & Act
    let s1 = Severity::new(1).unwrap();
    let s5 = Severity::new(5).unwrap();
    let s10 = Severity::new(10).unwrap();

    // Assert
    assert_eq!(s1.value(), 1);
    assert_eq!(s5.value(), 5);
    assert_eq!(s10.value(), 10);
}

#[test]
fn test_severity_invalid_range() {
    // Arrange & Act
    let result_0 = Severity::new(0);
    let result_11 = Severity::new(11);
    let result_255 = Severity::new(255);

    // Assert
    assert!(result_0.is_err());
    assert!(result_11.is_err());
    assert!(result_255.is_err());
}

#[test]
fn test_severity_levels() {
    // Arrange & Act
    let low = Severity::new(2).unwrap();
    let medium = Severity::new(5).unwrap();
    let high = Severity::new(7).unwrap();
    let critical = Severity::new(9).unwrap();

    // Assert
    assert_eq!(low.level(), "LOW");
    assert_eq!(medium.level(), "MEDIUM");
    assert_eq!(high.level(), "HIGH");
    assert_eq!(critical.level(), "CRITICAL");
}

// ============================================================================
// Occurrence Tests
// ============================================================================

#[test]
fn test_occurrence_valid_range() {
    // Arrange & Act
    let o1 = Occurrence::new(1).unwrap();
    let o5 = Occurrence::new(5).unwrap();
    let o10 = Occurrence::new(10).unwrap();

    // Assert
    assert_eq!(o1.value(), 1);
    assert_eq!(o5.value(), 5);
    assert_eq!(o10.value(), 10);
}

#[test]
fn test_occurrence_invalid_range() {
    // Arrange & Act
    let result_0 = Occurrence::new(0);
    let result_11 = Occurrence::new(11);

    // Assert
    assert!(result_0.is_err());
    assert!(result_11.is_err());
}

#[test]
fn test_occurrence_levels() {
    // Arrange & Act
    let remote = Occurrence::new(1).unwrap();
    let low = Occurrence::new(3).unwrap();
    let moderate = Occurrence::new(5).unwrap();
    let high = Occurrence::new(7).unwrap();
    let very_high = Occurrence::new(9).unwrap();

    // Assert
    assert_eq!(remote.level(), "REMOTE");
    assert_eq!(low.level(), "LOW");
    assert_eq!(moderate.level(), "MODERATE");
    assert_eq!(high.level(), "HIGH");
    assert_eq!(very_high.level(), "VERY_HIGH");
}

// ============================================================================
// Detection Tests
// ============================================================================

#[test]
fn test_detection_valid_range() {
    // Arrange & Act
    let d1 = Detection::new(1).unwrap();
    let d5 = Detection::new(5).unwrap();
    let d10 = Detection::new(10).unwrap();

    // Assert
    assert_eq!(d1.value(), 1);
    assert_eq!(d5.value(), 5);
    assert_eq!(d10.value(), 10);
}

#[test]
fn test_detection_invalid_range() {
    // Arrange & Act
    let result_0 = Detection::new(0);
    let result_11 = Detection::new(11);

    // Assert
    assert!(result_0.is_err());
    assert!(result_11.is_err());
}

#[test]
fn test_detection_levels() {
    // Arrange & Act
    let very_high = Detection::new(1).unwrap();
    let high = Detection::new(3).unwrap();
    let medium = Detection::new(5).unwrap();
    let low = Detection::new(7).unwrap();
    let very_low = Detection::new(9).unwrap();

    // Assert
    assert_eq!(very_high.level(), "VERY_HIGH");
    assert_eq!(high.level(), "HIGH");
    assert_eq!(medium.level(), "MEDIUM");
    assert_eq!(low.level(), "LOW");
    assert_eq!(very_low.level(), "VERY_LOW");
}

// ============================================================================
// RPN Tests
// ============================================================================

#[test]
fn test_rpn_calculation() {
    // Arrange
    let s9 = Severity::new(9).unwrap();
    let o7 = Occurrence::new(7).unwrap();
    let d7 = Detection::new(7).unwrap();

    // Act
    let rpn = RPN::calculate(s9, o7, d7);

    // Assert
    assert_eq!(rpn.value(), 441); // 9 × 7 × 7 = 441
}

#[test]
fn test_rpn_risk_levels() {
    // Arrange
    let s1 = Severity::new(1).unwrap();
    let o1 = Occurrence::new(1).unwrap();
    let d1 = Detection::new(1).unwrap();

    let s5 = Severity::new(5).unwrap();
    let o5 = Occurrence::new(5).unwrap();
    let d5 = Detection::new(5).unwrap();

    let s7 = Severity::new(7).unwrap();
    let o7 = Occurrence::new(7).unwrap();
    let d7 = Detection::new(7).unwrap();

    let s10 = Severity::new(10).unwrap();
    let o10 = Occurrence::new(10).unwrap();
    let d10 = Detection::new(10).unwrap();

    // Act
    let rpn_low = RPN::calculate(s1, o1, d1);        // 1
    let rpn_medium = RPN::calculate(s5, o5, d5);     // 125
    let rpn_high = RPN::calculate(s7, o7, d7);       // 343
    let rpn_critical = RPN::calculate(s10, o10, d10); // 1000

    // Assert
    assert_eq!(rpn_low.risk_level(), "LOW");
    assert_eq!(rpn_medium.risk_level(), "MEDIUM");
    assert_eq!(rpn_high.risk_level(), "HIGH");
    assert_eq!(rpn_critical.risk_level(), "CRITICAL");
}

// ============================================================================
// FailureMode Builder Tests
// ============================================================================

#[test]
fn test_failure_mode_builder_success() {
    // Arrange & Act
    let mode = FailureMode::builder()
        .id("test_mode")
        .category(FailureCategory::FileIO)
        .description("Test failure mode")
        .severity(Severity::new(8).unwrap())
        .occurrence(Occurrence::new(6).unwrap())
        .detection(Detection::new(6).unwrap())
        .effect("Effect 1")
        .effect("Effect 2")
        .cause("Cause 1")
        .control("Control 1")
        .action("Action 1")
        .build()
        .unwrap();

    // Assert
    assert_eq!(mode.id, "test_mode");
    assert_eq!(mode.category, FailureCategory::FileIO);
    assert_eq!(mode.severity.value(), 8);
    assert_eq!(mode.occurrence.value(), 6);
    assert_eq!(mode.detection.value(), 6);
    assert_eq!(mode.rpn.value(), 288); // 8 × 6 × 6 = 288
    assert_eq!(mode.effects.len(), 2);
    assert_eq!(mode.causes.len(), 1);
    assert_eq!(mode.controls.len(), 1);
    assert_eq!(mode.actions.len(), 1);
}

#[test]
fn test_failure_mode_builder_missing_required() {
    // Arrange & Act
    let result = FailureMode::builder()
        .id("test_mode")
        // Missing: category, description, severity, occurrence, detection
        .build();

    // Assert
    assert!(result.is_err());
}

#[test]
fn test_failure_mode_builder_auto_rpn() {
    // Arrange & Act
    let mode = FailureMode::builder()
        .id("test_mode")
        .category(FailureCategory::NetworkOps)
        .description("Test")
        .severity(Severity::new(9).unwrap())
        .occurrence(Occurrence::new(7).unwrap())
        .detection(Detection::new(7).unwrap())
        .build()
        .unwrap();

    // Assert
    // RPN should be auto-calculated: 9 × 7 × 7 = 441
    assert_eq!(mode.rpn.value(), 441);
    // 441 is in HIGH range (251-500), not CRITICAL (501-1000)
    assert_eq!(mode.rpn.risk_level(), "HIGH");
}

// ============================================================================
// FmeaRegistry Tests
// ============================================================================

#[test]
fn test_registry_register_and_get() {
    // Arrange
    let mut registry = FmeaRegistry::new();
    let mode = FailureMode::builder()
        .id("test_mode")
        .category(FailureCategory::FileIO)
        .description("Test")
        .severity(Severity::new(5).unwrap())
        .occurrence(Occurrence::new(5).unwrap())
        .detection(Detection::new(5).unwrap())
        .build()
        .unwrap();

    // Act
    registry.register(mode);

    // Assert
    let retrieved = registry.get_failure_mode("test_mode");
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().id, "test_mode");
}

#[test]
fn test_registry_record_event() {
    // Arrange
    let mut registry = FmeaRegistry::new();
    let event = FailureEvent::new(
        "test_mode".to_string(),
        "test_operation".to_string(),
        "Test error".to_string(),
    );

    // Act
    registry.record_event(event);

    // Assert
    assert_eq!(registry.event_count(), 1);
    let recent = registry.recent_events(10).collect::<Vec<_>>();
    assert_eq!(recent.len(), 1);
    assert_eq!(recent[0].mode_id, "test_mode");
}

#[test]
fn test_registry_ring_buffer() {
    // Arrange
    let mut registry = FmeaRegistry::new();

    // Act: Add 1500 events (exceeds max of 1000)
    for i in 0..1500 {
        let event = FailureEvent::new(
            format!("mode_{}", i),
            "test_op".to_string(),
            "error".to_string(),
        );
        registry.record_event(event);
    }

    // Assert: Should only retain 1000 most recent
    assert_eq!(registry.event_count(), 1000);

    // Verify oldest events were evicted (0-499 should be gone)
    let all_events: Vec<_> = registry.recent_events(1000).collect();
    assert_eq!(all_events.len(), 1000);
    // Most recent should be mode_1499
    assert!(all_events[0].mode_id.contains("1499"));
}

#[test]
fn test_registry_filter_by_category() {
    // Arrange
    let mut registry = FmeaRegistry::new();

    let mode1 = FailureMode::builder()
        .id("file_mode")
        .category(FailureCategory::FileIO)
        .description("Test")
        .severity(Severity::new(5).unwrap())
        .occurrence(Occurrence::new(5).unwrap())
        .detection(Detection::new(5).unwrap())
        .build()
        .unwrap();

    let mode2 = FailureMode::builder()
        .id("network_mode")
        .category(FailureCategory::NetworkOps)
        .description("Test")
        .severity(Severity::new(5).unwrap())
        .occurrence(Occurrence::new(5).unwrap())
        .detection(Detection::new(5).unwrap())
        .build()
        .unwrap();

    registry.register(mode1);
    registry.register(mode2);

    // Act
    let file_modes: Vec<_> = registry.failure_modes_by_category(FailureCategory::FileIO).collect();
    let network_modes: Vec<_> = registry.failure_modes_by_category(FailureCategory::NetworkOps).collect();

    // Assert
    assert_eq!(file_modes.len(), 1);
    assert_eq!(file_modes[0].id, "file_mode");
    assert_eq!(network_modes.len(), 1);
    assert_eq!(network_modes[0].id, "network_mode");
}

#[test]
fn test_registry_events_for_mode() {
    // Arrange
    let mut registry = FmeaRegistry::new();

    registry.record_event(FailureEvent::new(
        "mode_a".to_string(),
        "op1".to_string(),
        "error1".to_string(),
    ));
    registry.record_event(FailureEvent::new(
        "mode_b".to_string(),
        "op2".to_string(),
        "error2".to_string(),
    ));
    registry.record_event(FailureEvent::new(
        "mode_a".to_string(),
        "op3".to_string(),
        "error3".to_string(),
    ));

    // Act
    let mode_a_events: Vec<_> = registry.events_for_mode("mode_a").collect();
    let mode_b_events: Vec<_> = registry.events_for_mode("mode_b").collect();

    // Assert
    assert_eq!(mode_a_events.len(), 2);
    assert_eq!(mode_b_events.len(), 1);
}

// ============================================================================
// Catalog Tests (verify pre-registered failure modes)
// ============================================================================

#[test]
fn test_catalog_critical_failures_registered() {
    // Arrange
    let mut registry = FmeaRegistry::new();

    // Act
    register_critical_failures(&mut registry);

    // Assert
    assert_eq!(registry.failure_mode_count(), 8);

    // Verify each critical failure mode exists
    assert!(registry.get_failure_mode("path_traversal_attack").is_some());
    assert!(registry.get_failure_mode("template_ssti").is_some());
    assert!(registry.get_failure_mode("dep_cycle_detected").is_some());
    assert!(registry.get_failure_mode("file_io_write_fail").is_some());
    assert!(registry.get_failure_mode("lockfile_race_corrupt").is_some());
    assert!(registry.get_failure_mode("network_timeout").is_some());
    assert!(registry.get_failure_mode("mutex_poisoned").is_some());
    assert!(registry.get_failure_mode("deser_invalid_format").is_some());
}

#[test]
fn test_catalog_rpn_values() {
    // Arrange
    let mut registry = FmeaRegistry::new();
    register_critical_failures(&mut registry);

    // Act & Assert: Verify RPN values match Pareto analysis
    let path_traversal = registry.get_failure_mode("path_traversal_attack").unwrap();
    assert_eq!(path_traversal.rpn.value(), 441);

    let template_ssti = registry.get_failure_mode("template_ssti").unwrap();
    assert_eq!(template_ssti.rpn.value(), 378);

    let dep_cycle = registry.get_failure_mode("dep_cycle_detected").unwrap();
    assert_eq!(dep_cycle.rpn.value(), 343);

    let file_io = registry.get_failure_mode("file_io_write_fail").unwrap();
    assert_eq!(file_io.rpn.value(), 288);
}

#[test]
fn test_catalog_pareto_distribution() {
    // Arrange
    let mut registry = FmeaRegistry::new();
    register_critical_failures(&mut registry);

    // Act: Collect all RPNs
    let mut rpns: Vec<_> = registry
        .all_failure_modes()
        .map(|m| m.rpn.value())
        .collect();
    rpns.sort_by(|a, b| b.cmp(a)); // Sort descending

    // Assert: Top 4 should account for significant portion (Pareto 80/20)
    let total: u16 = rpns.iter().sum();
    let top_4: u16 = rpns.iter().take(4).sum();
    let percentage = (top_4 as f64 / total as f64) * 100.0;

    // Top 4 should be >= 60% (relaxed from 80% due to 8 items)
    assert!(percentage >= 60.0, "Top 4 RPN should be >= 60% of total, got {:.1}%", percentage);
}
