//! Conformance checking for process models.
//!
//! This module provides functions for validating event logs against process
//! models and computing fitness metrics.

use crate::error::Result;
use crate::event_log::{Event, EventLog, Trace};
use crate::petri_net::PetriNet;

/// Conformance checker for validating event logs against process models.
#[derive(Debug, Clone)]
pub struct ConformanceChecker {
    /// Whether to compute detailed alignments.
    compute_alignments: bool,
}

impl Default for ConformanceChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl ConformanceChecker {
    /// Create a new conformance checker.
    #[must_use]
    pub fn new() -> Self {
        Self {
            compute_alignments: true,
        }
    }

    /// Set whether to compute detailed alignments.
    #[must_use]
    pub fn with_alignments(mut self, compute: bool) -> Self {
        self.compute_alignments = compute;
        self
    }

    /// Check conformance of an event log against a Petri net model.
    ///
    /// This computes fitness, precision, and generalization metrics.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Petri net is invalid
    /// - The event log contains invalid traces
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::{ConformanceChecker, PetriNet, EventLog};
    ///
    /// let checker = ConformanceChecker::new();
    /// let net = PetriNet::from_activities(&["A", "B", "C"]);
    /// let log = EventLog::from_xes_file("log.xes")?;
    /// let report = checker.check(&net, &log)?;
    /// ```
    pub fn check(&self, model: &PetriNet, log: &EventLog) -> Result<ConformanceReport> {
        // Validate inputs
        model.validate()?;
        log.validate()?;

        let mut trace_results = Vec::new();

        for trace in &log.traces {
            let result = self.check_trace(model, trace)?;
            trace_results.push(result);
        }

        // Compute aggregate metrics
        let fitness = self.compute_fitness(&trace_results);
        let precision = self.compute_precision(model, log)?;
        let generalization = self.compute_generalization(model, log);

        let mut alignments = Vec::new();
        if self.compute_alignments {
            for (trace, _result) in log.traces.iter().zip(&trace_results) {
                let alignment = self.compute_alignment(model, trace)?;
                alignments.push(alignment);
            }
        }

        Ok(ConformanceReport {
            fitness,
            precision,
            generalization,
            trace_results,
            alignments,
        })
    }

    /// Check conformance of a single trace against the model.
    fn check_trace(&self, model: &PetriNet, trace: &Trace) -> Result<TraceConformance> {
        let mut is_conforming = true;
        let mut missing_moves = 0;
        let mut model_moves = 0;
        let mut log_moves = 0;

        // Simulate trace execution on the Petri net
        let mut current_marking = model.initial_marking.clone();
        let mut enabled_transitions = self.get_enabled_transitions(model, &current_marking);

        for event in &trace.events {
            // Find matching transition
            let matching_transition = model
                .transitions
                .iter()
                .find(|t| t.label.as_ref() == Some(&event.activity));

            match matching_transition {
                Some(trans) if enabled_transitions.contains(&trans.id) => {
                    // Fire the transition
                    current_marking = self.fire_transition(model, &current_marking, &trans.id)?;
                    enabled_transitions = self.get_enabled_transitions(model, &current_marking);
                }
                Some(_) => {
                    // Transition exists but not enabled - model move needed
                    model_moves += 1;
                    is_conforming = false;
                }
                None => {
                    // No matching transition - log move needed
                    log_moves += 1;
                    is_conforming = false;
                }
            }
        }

        // Check if we reached final marking
        let is_complete = current_marking.tokens == model.final_marking.tokens;

        if !is_complete {
            is_conforming = false;
            missing_moves = model.final_marking.tokens.len()
                - current_marking
                    .tokens
                    .iter()
                    .filter(|(k, v)| model.final_marking.tokens.get(*k) == Some(v))
                    .count();
        }

        Ok(TraceConformance {
            case_id: trace.case_id.clone(),
            is_conforming,
            is_complete,
            missing_moves,
            model_moves,
            log_moves,
        })
    }

    /// Get enabled transitions for a marking.
    fn get_enabled_transitions(
        &self, model: &PetriNet, marking: &crate::petri_net::Marking,
    ) -> Vec<String> {
        let mut enabled = Vec::new();

        for transition in &model.transitions {
            let input_places: Vec<_> = model
                .arcs
                .iter()
                .filter(|a| a.target == transition.id)
                .map(|a| &a.source)
                .collect();

            let all_input_marked = input_places.iter().all(|p| marking.is_marked(p));

            if all_input_marked {
                enabled.push(transition.id.clone());
            }
        }

        enabled
    }

    /// Fire a transition and return the new marking.
    fn fire_transition(
        &self, model: &PetriNet, marking: &crate::petri_net::Marking, transition_id: &str,
    ) -> Result<crate::petri_net::Marking> {
        let mut new_tokens = marking.tokens.clone();

        // Consume tokens from input places
        for arc in &model.arcs {
            if arc.target == transition_id {
                let current = new_tokens.get(&arc.source).copied().unwrap_or(0);
                let count = current.saturating_sub(arc.weight);
                new_tokens.insert(arc.source.clone(), count);
            }
        }

        // Produce tokens to output places
        for arc in &model.arcs {
            if arc.source == transition_id {
                let current = new_tokens.get(&arc.target).copied().unwrap_or(0);
                let count = current + arc.weight;
                new_tokens.insert(arc.target.clone(), count);
            }
        }

        Ok(crate::petri_net::Marking { tokens: new_tokens })
    }

    /// Compute overall fitness score.
    fn compute_fitness(&self, trace_results: &[TraceConformance]) -> f64 {
        if trace_results.is_empty() {
            return 1.0;
        }

        let total_moves: usize = trace_results
            .iter()
            .map(|r| r.missing_moves + r.model_moves + r.log_moves)
            .sum();

        let executed_moves: usize = trace_results
            .iter()
            .map(|r| r.model_moves + r.log_moves)
            .sum();

        if total_moves == 0 {
            return 1.0;
        }

        1.0 - (executed_moves as f64 / total_moves as f64)
    }

    /// Compute precision score.
    fn compute_precision(&self, model: &PetriNet, log: &EventLog) -> Result<f64> {
        // Simplified precision: measure how much of the model behavior is seen in the log
        let model_activities = model.transitions.len();

        if model_activities == 0 {
            return Ok(1.0);
        }

        let log_activities = log.unique_activities().len();

        // Avoid division by zero
        if model_activities >= log_activities {
            return Ok(log_activities as f64 / model_activities as f64);
        }

        // Model has fewer activities than log - potential precision issue
        Ok(model_activities as f64 / log_activities as f64)
    }

    /// Compute generalization score.
    fn compute_generalization(&self, model: &PetriNet, log: &EventLog) -> f64 {
        // Simplified generalization: based on model complexity vs log size
        let model_complexity = model.places.len() + model.transitions.len();
        let log_size = log.total_events();

        if log_size == 0 {
            return 1.0;
        }

        // Penalize overly complex models for simple logs
        let ratio = model_complexity as f64 / log_size as f64;
        (1.0 / (1.0 + ratio)).min(1.0)
    }

    /// Compute alignment between trace and model.
    fn compute_alignment(&self, _model: &PetriNet, _trace: &Trace) -> Result<Alignment> {
        // Simplified alignment - in production this would use A* or Dijkstra
        Ok(Alignment {
            cost: 0,
            steps: Vec::new(),
        })
    }
}

/// Conformance report for an entire event log.
#[derive(Debug, Clone)]
pub struct ConformanceReport {
    /// Overall fitness score (0-1, where 1 is perfect fitness).
    pub fitness: f64,

    /// Overall precision score (0-1).
    pub precision: f64,

    /// Overall generalization score (0-1).
    pub generalization: f64,

    /// Per-trace conformance results.
    pub trace_results: Vec<TraceConformance>,

    /// Detailed alignments (if computed).
    pub alignments: Vec<Alignment>,
}

impl ConformanceReport {
    /// Get the number of conforming traces.
    #[must_use]
    pub fn conforming_count(&self) -> usize {
        self.trace_results
            .iter()
            .filter(|r| r.is_conforming)
            .count()
    }

    /// Get the number of non-conforming traces.
    #[must_use]
    pub fn non_conforming_count(&self) -> usize {
        self.trace_results
            .iter()
            .filter(|r| !r.is_conforming)
            .count()
    }

    /// Get the percentage of conforming traces.
    #[must_use]
    pub fn conforming_percentage(&self) -> f64 {
        if self.trace_results.is_empty() {
            return 0.0;
        }
        (self.conforming_count() as f64 / self.trace_results.len() as f64) * 100.0
    }
}

/// Conformance result for a single trace.
#[derive(Debug, Clone)]
pub struct TraceConformance {
    /// Case identifier.
    pub case_id: String,

    /// Whether the trace conforms to the model.
    pub is_conforming: bool,

    /// Whether the trace reached the final marking.
    pub is_complete: bool,

    /// Number of missing moves (model elements not executed).
    pub missing_moves: usize,

    /// Number of model moves (silent transitions).
    pub model_moves: usize,

    /// Number of log moves (events not in model).
    pub log_moves: usize,
}

/// Alignment between a trace and model.
#[derive(Debug, Clone)]
pub struct Alignment {
    /// Alignment cost (lower is better).
    pub cost: usize,

    /// Alignment steps.
    pub steps: Vec<AlignmentStep>,
}

/// A single step in an alignment.
#[derive(Debug, Clone)]
pub enum AlignmentStep {
    /// Model move (silent transition).
    ModelMove(String),

    /// Log move (event not in model).
    LogMove(Event),

    /// Synchronous move (event matches model transition).
    SyncMove(String, Event),
}

/// Convenience function for conformance checking.
///
/// # Examples
///
/// ```rust,ignore
/// use ggen_process_mining::{check_conformance, PetriNet, EventLog};
///
/// let net = PetriNet::from_activities(&["A", "B", "C"]);
/// let log = EventLog::from_xes_file("log.xes")?;
/// let report = check_conformance(&net, &log)?;
/// ```
pub fn check_conformance(model: &PetriNet, log: &EventLog) -> Result<ConformanceReport> {
    ConformanceChecker::new().check(model, log)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_log::Event;

    fn make_test_trace() -> Trace {
        Trace::new("case1")
            .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "B", "2024-01-01T11:00:00Z").unwrap())
            .with_event(Event::new("e3", "C", "2024-01-01T12:00:00Z").unwrap())
    }

    fn make_test_log() -> EventLog {
        EventLog::new("Test Log").with_trace(make_test_trace())
    }

    #[test]
    fn test_conformance_checker_creation() {
        let checker = ConformanceChecker::new();
        assert!(checker.compute_alignments);
    }

    #[test]
    fn test_conformance_checker_with_config() {
        let checker = ConformanceChecker::new().with_alignments(false);
        assert!(!checker.compute_alignments);
    }

    #[test]
    fn test_check_conformance_simple() {
        let net = PetriNet::from_activities(&["A", "B", "C"]);
        let log = make_test_log();

        let checker = ConformanceChecker::new().with_alignments(false);
        let result = checker.check(&net, &log);

        assert!(result.is_ok());

        let report = result.unwrap();
        assert!(report.fitness > 0.0);
        assert!(!report.trace_results.is_empty());
    }

    #[test]
    fn test_conformance_report_metrics() {
        let net = PetriNet::from_activities(&["A", "B"]);
        let log = make_test_log();

        let checker = ConformanceChecker::new();
        let report = checker.check(&net, &log).unwrap();

        assert!(report.fitness >= 0.0 && report.fitness <= 1.0);
        assert!(report.precision >= 0.0 && report.precision <= 1.0);
        assert!(report.generalization >= 0.0 && report.generalization <= 1.0);
    }

    #[test]
    fn test_conformance_report_counts() {
        let report = ConformanceReport {
            fitness: 0.9,
            precision: 0.8,
            generalization: 0.85,
            trace_results: vec![
                TraceConformance {
                    case_id: "case1".to_string(),
                    is_conforming: true,
                    is_complete: true,
                    missing_moves: 0,
                    model_moves: 0,
                    log_moves: 0,
                },
                TraceConformance {
                    case_id: "case2".to_string(),
                    is_conforming: false,
                    is_complete: false,
                    missing_moves: 1,
                    model_moves: 2,
                    log_moves: 0,
                },
            ],
            alignments: Vec::new(),
        };

        assert_eq!(report.conforming_count(), 1);
        assert_eq!(report.non_conforming_count(), 1);
        assert_eq!(report.conforming_percentage(), 50.0);
    }

    #[test]
    fn test_convenience_function() {
        let net = PetriNet::from_activities(&["A", "B", "C"]);
        let log = make_test_log();

        let result = check_conformance(&net, &log);
        assert!(result.is_ok());
    }

    #[test]
    fn test_empty_log_conformance() {
        let net = PetriNet::from_activities(&["A", "B"]);
        let log = EventLog::new("Empty Log");

        let checker = ConformanceChecker::new();
        let report = checker.check(&net, &log).unwrap();

        assert_eq!(report.fitness, 1.0); // No violations in empty log
        assert_eq!(report.trace_results.len(), 0);
    }
}
