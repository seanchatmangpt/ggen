//! Alpha++ Process Discovery Tests
//!
//! Tests for the Alpha++ algorithm discovering process models from event logs.
//! Validates that discovered models match expected workflow structures.

use ggen_process_mining::{AlphaPlusPlus, EventLog, PetriNet, ProcessMiner};
use std::collections::HashSet;

#[cfg(test)]
mod discovery_tests {
    use super::super::helpers::*;
    use super::*;

    /// Test Alpha++ discovery on simple sequential log.
    #[test]
    fn test_alpha_plusplus_simple_sequential_discovery() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new().with_validation(true);

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert
        assert!(
            result.is_ok(),
            "Alpha++ discovery should succeed on simple log"
        );

        let net = result.unwrap();
        assert_eq!(net.name, Some("Simple Sequential Log".to_string()));
        assert!(
            !net.transitions.is_empty(),
            "Discovered net should have transitions"
        );

        // Verify sequential structure
        verify_workflow_pattern(&net, "sequential");
    }

    /// Test Alpha++ discovery on parallel process log.
    #[test]
    fn test_alpha_plusplus_parallel_discovery() {
        // Arrange
        let log = create_parallel_log();
        let miner = ProcessMiner::new().with_min_support(0.1);

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert
        assert!(result.is_ok());

        let net = result.unwrap();
        verify_petri_net_structure(&net, 3, 4, 8);
    }

    /// Test Alpha++ handles insufficient log size.
    #[test]
    fn test_alpha_plusplus_insufficient_log() {
        // Arrange - Create a log with only one trace (insufficient)
        use ggen_process_mining::{Event, Trace};

        let tiny_log = EventLog::new("Tiny Log").with_trace(
            Trace::new("only_case")
                .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap()),
        );

        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&tiny_log);

        // Assert
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(
                e.to_string().contains("Insufficient event log")
                    || e.to_string().contains("need at least")
            );
        }
    }

    /// Test Alpha++ direct algorithm usage.
    #[test]
    fn test_alpha_plusplus_algorithm_direct() {
        // Arrange
        let log = create_simple_log();
        let algorithm = AlphaPlusPlus::new()
            .with_min_support(0.1)
            .with_validation(true);

        // Act
        let result = algorithm.discover(&log);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();
        assert!(net.validate().is_ok());
    }

    /// Test footprint relation extraction.
    #[test]
    fn test_footprint_extraction_sequential() {
        // Arrange
        let log = create_simple_log();

        // Build expected causal relations for sequential process
        let mut expected_causal = HashSet::new();
        expected_causal.insert(("Submit".to_string(), "Review".to_string()));
        expected_causal.insert(("Review".to_string(), "Approve".to_string()));

        // Act - Discover model and verify causal relations
        let miner = ProcessMiner::new().with_validation(false);
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Verify discovered model has sequential structure
        let activities: Vec<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        assert!(
            activities.len() >= 3,
            "Should discover at least 3 activities"
        );

        // Check for sequential ordering in the network structure
        let has_sequential_flow = net.arcs.iter().any(|arc| {
            let from_label = net
                .transitions
                .iter()
                .find(|t| t.id == arc.source)
                .and_then(|t| t.label.clone());
            let to_label = net
                .transitions
                .iter()
                .find(|t| t.id == arc.source)
                .and_then(|t| t.label.clone());

            match (from_label, to_label) {
                (Some(from), Some(to)) => {
                    matches!(
                        (from.as_str(), to.as_str()),
                        ("Submit", "Review") | ("Review", "Approve")
                    )
                }
                _ => false,
            }
        });

        assert!(
            has_sequential_flow,
            "Discovered model should capture sequential flow"
        );
    }

    /// Test Alpha++ with varying minimum support thresholds.
    #[test]
    fn test_alpha_plusplus_varying_support() {
        // Arrange
        let log = create_simple_log();

        // Test with low support threshold
        let low_support_miner = ProcessMiner::new().with_min_support(0.0);
        let low_result = low_support_miner.discover_alpha_plusplus(&log);
        assert!(low_result.is_ok());

        // Test with high support threshold (might reduce discovered relations)
        let high_support_miner = ProcessMiner::new().with_min_support(0.9);
        let high_result = high_support_miner.discover_alpha_plusplus(&log);
        assert!(high_result.is_ok());

        // Both should produce valid nets
        let low_net = low_result.unwrap();
        let high_net = high_result.unwrap();

        assert!(low_net.validate().is_ok());
        assert!(high_net.validate().is_ok());
    }

    /// Test discovery on log with duplicate traces.
    #[test]
    fn test_alpha_plusplus_duplicate_traces() {
        // Arrange
        use ggen_process_mining::{Event, Trace};

        let trace1 = Trace::new("case1")
            .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "B", "2024-01-01T11:00:00Z").unwrap());

        let trace2 = Trace::new("case2")
            .with_event(Event::new("e3", "A", "2024-01-02T10:00:00Z").unwrap())
            .with_event(Event::new("e4", "B", "2024-01-02T11:00:00Z").unwrap());

        let log = EventLog::new("Duplicate Traces Log")
            .with_trace(trace1)
            .with_trace(trace2);

        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();

        // Should discover the same A->B pattern
        let activities: Vec<_> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.as_ref())
            .collect();

        assert!(activities.contains(&&"A".to_string()));
        assert!(activities.contains(&&"B".to_string()));
    }

    /// Test discovery produces workflow matching source log.
    #[test]
    fn test_discovered_workflow_matches_log() {
        // Arrange
        let log = create_simple_log();
        let log_activities = log.unique_activities();

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - All activities from log should be in discovered model
        let discovered_activities: HashSet<String> = net
            .transitions
            .iter()
            .filter_map(|t| t.label.clone())
            .collect();

        for activity in &log_activities {
            assert!(
                discovered_activities.contains(activity),
                "Activity '{}' from log should be in discovered model",
                activity
            );
        }

        // Assert - Discovered model should be valid
        assert!(net.validate().is_ok());
    }

    /// Test Alpha++ with validation enabled vs disabled.
    #[test]
    fn test_alpha_plusplus_validation_flag() {
        // Arrange
        let log = create_simple_log();

        let validating_miner = ProcessMiner::new().with_validation(true);
        let non_validating_miner = ProcessMiner::new().with_validation(false);

        // Act
        let result_validated = validating_miner.discover_alpha_plusplus(&log);
        let result_non_validated = non_validating_miner.discover_alpha_plusplus(&log);

        // Assert - Both should succeed
        assert!(result_validated.is_ok());
        assert!(result_non_validated.is_ok());

        // Both should produce structurally valid nets
        let net_validated = result_validated.unwrap();
        let net_non_validated = result_non_validated.unwrap();

        assert_eq!(
            net_validated.transitions.len(),
            net_non_validated.transitions.len()
        );
    }

    /// Test inductive miner as alternative discovery method.
    #[test]
    fn test_inductive_miner_discovery() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_inductive(&log);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();

        // Inductive miner should produce a valid net
        assert!(net.validate().is_ok());
        assert!(!net.transitions.is_empty());
    }

    /// Test discovery preserves process name.
    #[test]
    fn test_discovery_preserves_log_name() {
        // Arrange
        let log = EventLog::new("Purchase Order Process")
            .with_trace(ggen_process_mining::Trace::new("case1").with_event(
                ggen_process_mining::Event::new("e1", "Create", "2024-01-01T10:00:00Z").unwrap(),
            ))
            .with_trace(ggen_process_mining::Trace::new("case2").with_event(
                ggen_process_mining::Event::new("e2", "Approve", "2024-01-02T10:00:00Z").unwrap(),
            ));

        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert
        assert_eq!(net.name, Some("Purchase Order Process".to_string()));
    }

    /// Test Alpha++ on empty log (should fail gracefully).
    #[test]
    fn test_alpha_plusplus_empty_log() {
        // Arrange
        let empty_log = EventLog::new("Empty Log");
        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&empty_log);

        // Assert
        assert!(result.is_err());
    }

    /// Test Alpha++ handles single-activity traces.
    #[test]
    fn test_alpha_plusplus_single_activity_traces() {
        // Arrange
        use ggen_process_mining::{Event, Trace};

        let log = EventLog::new("Single Activity Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap()),
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e2", "A", "2024-01-02T10:00:00Z").unwrap()),
            );

        let miner = ProcessMiner::new();

        // Act
        let result = miner.discover_alpha_plusplus(&log);

        // Assert - Should fail due to insufficient traces
        assert!(result.is_err());
    }

    /// Test discovery creates consistent markings.
    #[test]
    fn test_discovery_creates_consistent_markings() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();

        // Assert - Should have valid initial and final markings
        assert!(
            !net.initial_marking.tokens.is_empty(),
            "Discovered net should have initial marking"
        );
        assert!(
            !net.final_marking.tokens.is_empty(),
            "Discovered net should have final marking"
        );
    }

    /// Test conversion of discovered model to YAWL.
    #[test]
    fn test_discovered_model_to_yawl_conversion() {
        // Arrange
        let log = create_simple_log();
        let miner = ProcessMiner::new();

        // Act
        let net = miner.discover_alpha_plusplus(&log).unwrap();
        let yawl_result = miner.to_yawl(net);

        // Assert
        assert!(yawl_result.is_ok());
        let yawl = yawl_result.unwrap();

        // Verify YAWL contains discovered activities
        assert!(yawl.contains("<specification"));
        assert!(yawl.contains("Simple Sequential Log"));
    }
}
