//! Workflow Structure Verification Tests
//!
//! Tests for verifying discovered process models match expected workflow structures.
//! Covers sequential, parallel, loop, and complex workflow patterns.

use ggen_process_mining::petri_net::Arc as PetriArc;
use ggen_process_mining::{
    Event, EventLog, Marking, PetriNet, Place, ProcessMiner, Trace, Transition,
};
use std::collections::HashSet;

#[cfg(test)]
mod workflow_structure_tests {
    use super::super::helpers::*;
    use super::*;

    /// Test sequential workflow structure detection.
    #[test]
    fn test_detect_sequential_workflow() {
        // Arrange - Create clearly sequential log
        let log = EventLog::new("Sequential Process")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Step1", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Step2", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "Step3", "2024-01-01T12:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e4", "Step1", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e5", "Step2", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e6", "Step3", "2024-01-02T12:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Verify sequential structure
        verify_workflow_pattern(&net, "sequential");

        // Additional verification: each transition should have clear predecessor/successor
        for transition in &net.transitions {
            let inputs = net.input_places_for(&transition.id);
            let outputs = net.output_places_for(&transition.id);

            // In a proper sequential flow, most transitions should have clear flow
            assert!(
                !inputs.is_empty() || !outputs.is_empty(),
                "Transition '{}' should have connections",
                transition.id
            );
        }
    }

    /// Test parallel workflow structure detection.
    #[test]
    fn test_detect_parallel_workflow() {
        // Arrange - Create log with parallel activities
        // Pattern: Start -> (A, B concurrent) -> End
        let log = EventLog::new("Parallel Process")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Start", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "TaskA", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "TaskB", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e4", "End", "2024-01-01T12:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e5", "Start", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e6", "TaskB", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e7", "TaskA", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e8", "End", "2024-01-02T12:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Verify parallel structure
        verify_workflow_pattern(&net, "parallel");

        // Check for places with multiple output transitions (parallel split)
        let has_parallel_split = net
            .places
            .iter()
            .any(|p| net.output_transitions_for(&p.id).len() > 1);
        assert!(
            has_parallel_split,
            "Parallel workflow should have parallel splits"
        );
    }

    /// Test loop workflow structure detection.
    #[test]
    fn test_detect_loop_workflow() {
        // Arrange - Create log with looping pattern
        // Pattern: A -> B -> A (possible loop)
        let log = EventLog::new("Loop Process")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Start", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "Rework", "2024-01-01T12:00:00Z").unwrap())
                    .with_event(Event::new("e4", "Review", "2024-01-01T13:00:00Z").unwrap())
                    .with_event(Event::new("e5", "Finish", "2024-01-01T14:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e6", "Start", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e7", "Review", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e8", "Finish", "2024-01-02T12:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert - Should discover a model
        assert!(result.is_ok());
        let net = result.unwrap();

        // Verify the net is valid
        assert!(net.validate().is_ok());
    }

    /// Test choice (XOR) workflow structure detection.
    #[test]
    fn test_detect_choice_workflow() {
        // Arrange - Create log with exclusive choices
        // Pattern: A -> (B OR C) -> D
        let log = EventLog::new("Choice Process")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Start", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "PathA", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "End", "2024-01-01T12:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e4", "Start", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e5", "PathB", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e6", "End", "2024-01-02T12:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - All activities discovered
        let activities: HashSet<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        assert!(activities.contains("Start"));
        assert!(activities.contains("PathA") || activities.contains("PathB"));
        assert!(activities.contains("End"));
    }

    /// Test complex workflow with multiple patterns.
    #[test]
    fn test_detect_complex_workflow() {
        // Arrange - Create complex log with sequential, parallel, and choice
        let log = EventLog::new("Complex Process")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "AutoApprove", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "Complete", "2024-01-01T12:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e4", "Submit", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e5", "ManualReview", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e6", "ManagerApprove", "2024-01-02T12:00:00Z").unwrap())
                    .with_event(Event::new("e7", "Complete", "2024-01-02T13:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Should discover multiple paths
        let activities: Vec<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        assert!(
            activities.len() >= 3,
            "Should discover at least 3 unique activities"
        );
        assert!(net.validate().is_ok());
    }

    /// Test workflow structure preservation through conversion.
    #[test]
    fn test_workflow_structure_preserved_through_conversion() {
        // Arrange
        let original_net = create_parallel_petri_net();
        let original_activities: HashSet<String> = original_net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        // Act - Convert to YAWL and back
        use ggen_process_mining::YawlBridge;

        let bridge = YawlBridge::new();
        let yawl = bridge.petri_net_to_yawl(&original_net).unwrap();
        let recovered_net = bridge.yawl_to_petri_net(&yawl).unwrap();

        let recovered_activities: HashSet<String> = recovered_net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        // Assert - Activities should be preserved
        for activity in &original_activities {
            assert!(
                recovered_activities.contains(activity),
                "Activity '{}' should be preserved after conversion",
                activity
            );
        }
    }

    /// Test discovered model has valid start and end markings.
    #[test]
    fn test_discovered_model_has_valid_markings() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert
        assert!(
            !net.initial_marking.tokens.is_empty(),
            "Discovered model must have initial marking"
        );

        assert!(
            !net.final_marking.tokens.is_empty(),
            "Discovered model must have final marking"
        );

        // Initial and final markings should be different
        let initial_places: HashSet<_> = net.initial_marking.tokens.keys().collect();
        let final_places: HashSet<_> = net.final_marking.tokens.keys().collect();

        assert!(
            initial_places != final_places
                || (initial_places.is_empty() && final_places.is_empty()),
            "Initial and final markings should typically be different"
        );
    }

    /// Test workflow structure metrics.
    #[test]
    fn test_workflow_structure_metrics() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Act - Calculate structure metrics
        let place_count = net.places.len();
        let transition_count = net.transitions.len();
        let arc_count = net.arcs.len();
        let density = if place_count + transition_count > 0 {
            arc_count as f64 / (place_count + transition_count) as f64
        } else {
            0.0
        };

        // Assert - Reasonable structural properties
        assert!(place_count > 0, "Should have at least one place");
        assert!(transition_count > 0, "Should have at least one transition");
        assert!(arc_count > 0, "Should have at least one arc");
        assert!(density > 0.0, "Network should have some connectivity");
        assert!(density < 10.0, "Network should not be excessively dense");
    }

    /// Test workflow handles all activities from log.
    #[test]
    fn test_workflow_covers_all_log_activities() {
        // Arrange
        let log = create_simple_log();
        let log_activities: HashSet<String> = log.unique_activities().into_iter().collect();

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();
        let model_activities: HashSet<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        // Assert - All log activities should be in model
        for activity in &log_activities {
            assert!(
                model_activities.contains(activity),
                "Log activity '{}' should be in discovered model",
                activity
            );
        }
    }

    /// Test workflow connectivity.
    #[test]
    fn test_workflow_connectivity() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Act - Build connectivity map
        let mut reachable_from_start = HashSet::new();
        let mut reachable_from_end = HashSet::new();

        for (initial_place, _) in &net.initial_marking.tokens {
            let mut visited = HashSet::new();
            let mut to_visit = vec![initial_place.clone()];

            while let Some(current) = to_visit.pop() {
                if visited.contains(&current) {
                    continue;
                }
                visited.insert(current.clone());

                // Follow transitions
                for trans_id in net.output_transitions_for(&current) {
                    for place in net.output_places_for(&trans_id.id) {
                        to_visit.push(place.id.clone());
                    }
                }
            }

            reachable_from_start = visited;
        }

        for (final_place, _) in &net.final_marking.tokens {
            let mut visited = HashSet::new();
            let mut to_visit = vec![final_place.clone()];

            while let Some(current) = to_visit.pop() {
                if visited.contains(&current) {
                    continue;
                }
                visited.insert(current.clone());

                // Follow transitions backward
                for trans_id in net.input_transitions_for(&current) {
                    for place in net.input_places_for(&trans_id.id) {
                        to_visit.push(place.id.clone());
                    }
                }
            }

            reachable_from_end = visited;
        }

        // Assert - Start should reach end
        for final_place in net.final_marking.tokens.keys() {
            assert!(
                reachable_from_start.contains(final_place),
                "Final place '{}' should be reachable from start",
                final_place
            );
        }

        for initial_place in net.initial_marking.tokens.keys() {
            assert!(
                reachable_from_end.contains(initial_place),
                "Initial place '{}' should reach end",
                initial_place
            );
        }
    }

    /// Test workflow produces valid YAWL for execution.
    #[test]
    fn test_workflow_produces_executable_yawl() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();
        let yawl = miner.to_yawl(net).unwrap();

        // Assert - YAWL should be well-formed
        assert!(yawl.contains("<?xml version"));
        assert!(yawl.contains("<specification"));
        assert!(yawl.contains("</specification>"));

        // Should have tasks (transitions)
        assert!(yawl.contains("<task"));

        // Should have conditions (places)
        assert!(yawl.contains("<condition"));

        // Should have flows (arcs)
        assert!(yawl.contains("<flow"));
    }

    /// Test real-world workflow patterns.
    #[test]
    fn test_real_world_approval_workflow() {
        // Arrange - Simulate real approval process
        let log = EventLog::new("Approval Workflow")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Request", "2024-01-01T09:00:00Z").unwrap())
                    .with_event(Event::new("e2", "ManagerReview", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e3", "Approve", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e4", "Process", "2024-01-01T12:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e5", "Request", "2024-01-02T09:00:00Z").unwrap())
                    .with_event(Event::new("e6", "ManagerReview", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e7", "Reject", "2024-01-02T11:00:00Z").unwrap())
                    .with_event(Event::new("e8", "Notify", "2024-01-02T12:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Should discover approval workflow structure
        let activities: Vec<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        assert!(
            activities.len() >= 4,
            "Should discover at least 4 activities"
        );
        assert!(activities.contains(&"Request".to_string()));
        assert!(activities.contains(&"ManagerReview".to_string()));
        assert!(net.validate().is_ok());
    }

    /// Test workflow handles large event logs.
    #[test]
    fn test_workflow_from_large_log() {
        // Arrange - Create log with many traces
        let mut log = EventLog::new("Large Log");

        for i in 0..50 {
            let case_id = format!("case{}", i);
            let trace = Trace::new(&case_id)
                .with_event(
                    Event::new(
                        format!("e{}_1", i),
                        "Submit",
                        &format!("2024-01-01T10:00:00Z"),
                    )
                    .unwrap(),
                )
                .with_event(
                    Event::new(
                        format!("e{}_2", i),
                        "Review",
                        &format!("2024-01-01T11:00:00Z"),
                    )
                    .unwrap(),
                )
                .with_event(
                    Event::new(
                        format!("e{}_3", i),
                        "Approve",
                        &format!("2024-01-01T12:00:00Z"),
                    )
                    .unwrap(),
                );

            // Safety: We need to construct the log properly
            let log_name = log.name.clone();
            let mut traces = log.traces;
            traces.push(trace);
            log = EventLog {
                name: log_name,
                traces,
                extensions: log.extensions.clone(),
            };
        }

        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();
        assert!(net.validate().is_ok());
    }
}
