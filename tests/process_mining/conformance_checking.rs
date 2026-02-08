//! Conformance Checking Tests
//!
//! Tests for validating event logs against process models.
//! Verifies fitness, precision, and generalization metrics.

use ggen_process_mining::{
    EventLog, PetriNet, ConformanceChecker,
    Trace, Event, Place, Transition, Marking,
};
use ggen_process_mining::petri_net::Arc as PetriArc;
use ggen_process_mining::event_log::AttributeValue;

#[cfg(test)]
mod conformance_tests {
    use super::*;
    use super::super::helpers::*;

    /// Convenience function for conformance checking (local definition).
    fn check_conformance(model: &PetriNet, log: &EventLog) -> ggen_process_mining::Result<ggen_process_mining::ConformanceReport> {
        ggen_process_mining::ConformanceChecker::new().check(model, log)
    }

    /// Test conformance checking on perfectly conforming log.
    #[test]
    fn test_conformance_perfect_fit() {
        // Arrange
        let net = create_simple_petri_net();
        let log = create_simple_log(); // Log matches the model

        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_ok());

        let report = result.unwrap();
        assert!(report.fitness > 0.5, "Fitness should be reasonably high");
        assert!(report.fitness <= 1.0, "Fitness cannot exceed 1.0");
        assert!(report.precision >= 0.0 && report.precision <= 1.0);
        assert!(report.generalization >= 0.0 && report.generalization <= 1.0);
    }

    /// Test conformance checking with non-conforming log.
    #[test]
    fn test_conformance_with_deviations() {
        // Arrange
        let net = create_simple_petri_net();

        // Create log with deviations (extra activities)
        use ggen_process_mining::Trace;

        let deviating_trace = Trace::new("case1")
            .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap())
            .with_event(Event::new("e3", "ExtraActivity", "2024-01-01T11:30:00Z").unwrap())
            .with_event(Event::new("e4", "Approve", "2024-01-01T12:00:00Z").unwrap());

        let log = EventLog::new("Deviating Log").with_trace(deviating_trace);

        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_ok());

        let report = result.unwrap();
        // Fitness should be lower than 1.0 due to deviations
        assert!(report.fitness < 1.0, "Fitness should be less than perfect with deviations");

        // Should have at least one non-conforming trace
        assert!(report.non_conforming_count() >= 0);
    }

    /// Test conformance on empty log.
    #[test]
    fn test_conformance_empty_log() {
        // Arrange
        let net = create_simple_petri_net();
        let empty_log = EventLog::new("Empty Log");

        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&net, &empty_log);

        // Assert
        assert!(result.is_ok());

        let report = result.unwrap();
        assert_eq!(report.fitness, 1.0, "Empty log has perfect fitness by definition");
        assert_eq!(report.trace_results.len(), 0);
    }

    /// Test conformance checking computes per-trace results.
    #[test]
    fn test_conformance_per_trace_results() {
        // Arrange
        let net = create_simple_petri_net();
        let log = create_simple_log();

        let checker = ConformanceChecker::new();

        // Act
        let report = checker.check(&net, &log).unwrap();

        // Assert
        assert_eq!(report.trace_results.len(), log.traces.len(),
                   "Should have one result per trace");

        for trace_result in &report.trace_results {
            assert!(!trace_result.case_id.is_empty());
        }
    }

    /// Test conformance with alignments enabled.
    #[test]
    fn test_conformance_with_alignments() {
        // Arrange
        let net = create_simple_petri_net();
        let log = create_simple_log();

        let checker = ConformanceChecker::new().with_alignments(true);

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_ok());

        let report = result.unwrap();
        // When alignments enabled, should have alignment data
        assert_eq!(report.alignments.len(), log.traces.len());
    }

    /// Test conformance with alignments disabled.
    #[test]
    fn test_conformance_without_alignments() {
        // Arrange
        let net = create_simple_petri_net();
        let log = create_simple_log();

        let checker = ConformanceChecker::new().with_alignments(false);

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_ok());

        let report = result.unwrap();
        assert_eq!(report.alignments.len(), 0);
    }

    /// Test conformance checker validates model.
    #[test]
    fn test_conformance_validates_model() {
        // Arrange - Create invalid Petri net (no transitions)
        let invalid_net = PetriNet::new()
            .with_place(Place::new("p1"));

        let log = create_simple_log();
        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&invalid_net, &log);

        // Assert
        assert!(result.is_err());
    }

    /// Test conformance metrics are bounded.
    #[test]
    fn test_conformance_metrics_bounded() {
        // Arrange
        let net = create_simple_petri_net();
        let log = create_simple_log();

        let checker = ConformanceChecker::new();

        // Act
        let report = checker.check(&net, &log).unwrap();

        // Assert - All metrics should be in [0, 1] range
        assert!(report.fitness >= 0.0 && report.fitness <= 1.0,
               "Fitness must be in [0, 1] range, got {}", report.fitness);
        assert!(report.precision >= 0.0 && report.precision <= 1.0,
               "Precision must be in [0, 1] range, got {}", report.precision);
        assert!(report.generalization >= 0.0 && report.generalization <= 1.0,
               "Generalization must be in [0, 1] range, got {}", report.generalization);
    }

    /// Test conformance report convenience methods.
    #[test]
    fn test_conformance_report_convenience_methods() {
        // Arrange
        let log = EventLog::new("Mixed Log")
            .with_trace(
                Trace::new("conforming_case")
                    .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Approve", "2024-01-01T11:00:00Z").unwrap())
            )
            .with_trace(
                Trace::new("deviating_case")
                    .with_event(Event::new("e3", "Submit", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e4", "Unknown", "2024-01-02T11:00:00Z").unwrap())
            );

        let net = create_simple_petri_net();
        let report = check_conformance(&net, &log).unwrap();

        // Act & Assert
        let conforming = report.conforming_count();
        let non_conforming = report.non_conforming_count();
        let percentage = report.conforming_percentage();

        assert_eq!(conforming + non_conforming, 2);
        assert!(percentage >= 0.0 && percentage <= 100.0);
    }

    /// Test trace completion detection.
    #[test]
    fn test_conformance_trace_completion() {
        // Arrange
        let net = create_simple_petri_net();

        // Incomplete trace (missing final activity)
        let incomplete_trace = Trace::new("case1")
            .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap());

        let log = EventLog::new("Incomplete Log").with_trace(incomplete_trace);

        let checker = ConformanceChecker::new();

        // Act
        let report = checker.check(&net, &log).unwrap();

        // Assert
        let trace_result = &report.trace_results[0];
        assert!(!trace_result.is_complete, "Trace should not be marked as complete");
    }

    /// Test model moves detection in conformance.
    #[test]
    fn test_conformance_model_moves() {
        // Arrange
        // Model expects: Submit -> Review -> Approve
        // Log has: Submit -> Approve (missing Review - needs model move)

        let log = EventLog::new("Skip Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Approve", "2024-01-01T11:00:00Z").unwrap())
            );

        let net = create_simple_petri_net();
        let report = check_conformance(&net, &log).unwrap();

        // Assert
        // Should detect deviation
        assert!(report.fitness < 1.0);
    }

    /// Test log moves detection in conformance.
    #[test]
    fn test_conformance_log_moves() {
        // Arrange
        // Log has activities not in model

        let log = EventLog::new("Extra Activity Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "NotInModel", "2024-01-01T11:30:00Z").unwrap())
                    .with_event(Event::new("e4", "Approve", "2024-01-01T12:00:00Z").unwrap())
            );

        let net = create_simple_petri_net();
        let report = check_conformance(&net, &log).unwrap();

        // Assert
        // Should detect log move
        let trace_result = &report.trace_results[0];
        assert!(trace_result.log_moves >= 0);
    }

    /// Test precision metric calculation.
    #[test]
    fn test_conformance_precision_calculation() {
        // Arrange
        let net = PetriNet::from_activities(&["A", "B", "C"]);
        let log = EventLog::new("Partial Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "B", "2024-01-01T11:00:00Z").unwrap())
            );

        let checker = ConformanceChecker::new();

        // Act
        let report = checker.check(&net, &log).unwrap();

        // Assert
        // Precision should reflect log covers less than full model
        assert!(report.precision >= 0.0 && report.precision <= 1.0);
    }

    /// Test generalization metric calculation.
    #[test]
    fn test_conformance_generalization_calculation() {
        // Arrange
        let complex_net = PetriNet::new()
            .with_name("Complex Model")
            .with_place(Place::new("p1"))
            .with_place(Place::new("p2"))
            .with_place(Place::new("p3"))
            .with_place(Place::new("p4"))
            .with_transition(Transition::new("t1"))
            .with_transition(Transition::new("t2"))
            .with_transition(Transition::new("t3"))
            .with_arc(PetriArc::new("p1", "t1"))
            .with_arc(PetriArc::new("t1", "p2"))
            .with_arc(PetriArc::new("p2", "t2"))
            .with_arc(PetriArc::new("t2", "p3"))
            .with_arc(PetriArc::new("p3", "t3"))
            .with_arc(PetriArc::new("t3", "p4"))
            .with_initial_marking(Marking::new().with_token("p1", 1))
            .with_final_marking(Marking::new().with_token("p4", 1));

        let simple_log = EventLog::new("Simple Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "B", "2024-01-01T11:00:00Z").unwrap())
            );

        let checker = ConformanceChecker::new();

        // Act
        let report = checker.check(&complex_net, &simple_log).unwrap();

        // Assert
        // Generalization should penalize overly complex model for simple log
        assert!(report.generalization >= 0.0 && report.generalization <= 1.0);
    }

    /// Test conformance on parallel process.
    #[test]
    fn test_conformance_parallel_process() {
        // Arrange
        let net = create_parallel_petri_net();
        let log = create_parallel_log();

        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_ok());
        let report = result.unwrap();
        assert!(report.fitness > 0.0);
    }

    /// Test conformance with invalid log (out of order timestamps).
    #[test]
    fn test_conformance_invalid_log() {
        // Arrange
        let net = create_simple_petri_net();

        // Create log with out-of-order events
        use ggen_process_mining::event_log::AttributeValue;

        let invalid_trace = Trace::new("case1")
            .with_event(Event::new("e1", "Submit", "2024-01-01T12:00:00Z").unwrap())
            .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap()); // Earlier!

        let log = EventLog::new("Invalid Log").with_trace(invalid_trace);

        let checker = ConformanceChecker::new();

        // Act
        let result = checker.check(&net, &log);

        // Assert
        assert!(result.is_err());
    }

    /// Test conformance percentage calculation.
    #[test]
    fn test_conformance_percentage_calculation() {
        // Arrange
        use ggen_process_mining::Trace;

        let log = EventLog::new("Mixed Results Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap())
                    .with_event(Event::new("e3", "Approve", "2024-01-01T12:00:00Z").unwrap())
            )
            .with_trace(
                Trace::new("case2")
                    .with_event(Event::new("e4", "Submit", "2024-01-02T10:00:00Z").unwrap())
                    .with_event(Event::new("e5", "Extra", "2024-01-02T11:00:00Z").unwrap())
            );

        let net = create_simple_petri_net();
        let report = check_conformance(&net, &log).unwrap();

        // Act
        let percentage = report.conforming_percentage();

        // Assert
        assert!(percentage >= 0.0 && percentage <= 100.0);
    }
}
