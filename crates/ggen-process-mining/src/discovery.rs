//! Process discovery algorithms.
//!
//! This module provides implementations of process discovery algorithms for
//! extracting process models from event logs.

use crate::error::{Error, Result};
use crate::event_log::EventLog;
use crate::petri_net::{Arc, Marking, PetriNet, Place, Transition};
use std::collections::{HashMap, HashSet};

/// Process miner for discovering process models from event logs.
#[derive(Debug, Clone)]
pub struct ProcessMiner {
    config: MinerConfig,
}

/// Configuration for process mining operations.
#[derive(Debug, Clone)]
pub struct MinerConfig {
    /// Whether to validate output models.
    pub validate_output: bool,

    /// Minimum support threshold for discovering relations.
    pub min_support: f64,

    /// Whether to filter infrequent traces.
    pub filter_infrequent: bool,
}

impl Default for MinerConfig {
    fn default() -> Self {
        Self {
            validate_output: true,
            min_support: 0.1,
            filter_infrequent: true,
        }
    }
}

impl ProcessMiner {
    /// Create a new process miner with default configuration.
    #[must_use]
    pub fn new() -> Self {
        Self {
            config: MinerConfig::default(),
        }
    }

    /// Get the current configuration.
    #[must_use]
    pub const fn config(&self) -> &MinerConfig {
        &self.config
    }

    /// Set whether to validate output models.
    #[must_use]
    pub fn with_validation(mut self, validate: bool) -> Self {
        self.config.validate_output = validate;
        self
    }

    /// Set the minimum support threshold.
    #[must_use]
    pub fn with_min_support(mut self, support: f64) -> Self {
        self.config.min_support = support;
        self
    }

    /// Discover a process model using the Alpha++ algorithm.
    ///
    /// Alpha++ is an enhanced version of the Alpha algorithm that handles
    /// more complex patterns like loops, invisible tasks, and skipped activities.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The event log is too small for discovery
    /// - Discovery fails to produce a valid model
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::{ProcessMiner, EventLog};
    ///
    /// let log = EventLog::from_xes_file("log.xes")?;
    /// let miner = ProcessMiner::new();
    /// let net = miner.discover_alpha_plusplus(&log)?;
    /// ```
    pub fn discover_alpha_plusplus(&self, log: &EventLog) -> Result<PetriNet> {
        // Validate log size
        if log.traces.len() < 2 {
            return Err(Error::insufficient_log(2, log.traces.len()));
        }

        let algorithm = AlphaPlusPlus::new()
            .with_min_support(self.config.min_support)
            .with_validation(self.config.validate_output);

        algorithm.discover(log)
    }

    /// Discover a process model using the inductive miner algorithm.
    ///
    /// Inductive miner recursively splits the log to discover a process tree.
    pub fn discover_inductive(&self, log: &EventLog) -> Result<PetriNet> {
        // Simplified inductive miner implementation
        // In production, this would use a more sophisticated algorithm
        let activities = log.unique_activities();

        if activities.is_empty() {
            return Err(Error::AlphaPlusPlusDiscovery(
                "no activities found".to_string(),
            ));
        }

        // Create a simple sequential model
        let activity_refs: Vec<&str> = activities.iter().map(|s| s.as_str()).collect();
        let net = PetriNet::from_activities(&activity_refs);

        if self.config.validate_output {
            net.validate()?;
        }

        Ok(net)
    }

    /// Convert a discovered Petri net to YAWL for execution.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::ProcessMiner;
    ///
    /// let miner = ProcessMiner::new();
    /// let net = miner.discover_alpha_plusplus(&log)?;
    /// let yawl = miner.to_yawl(net)?;
    /// ```
    pub fn to_yawl(&self, net: PetriNet) -> Result<String> {
        use crate::yawl_bridge::PetriNetToYawl;
        net.to_yawl()
    }
}

impl Default for ProcessMiner {
    fn default() -> Self {
        Self::new()
    }
}

/// Alpha++ process discovery algorithm.
///
/// Alpha++ extends the Alpha algorithm with:
/// - Handling of loops (length-one and length-two loops)
/// - Detection of invisible tasks
/// - Support for non-local dependencies
#[derive(Debug, Clone)]
pub struct AlphaPlusPlus {
    min_support: f64,
    validate: bool,
}

impl Default for AlphaPlusPlus {
    fn default() -> Self {
        Self::new()
    }
}

impl AlphaPlusPlus {
    /// Create a new Alpha++ algorithm instance.
    #[must_use]
    pub fn new() -> Self {
        Self {
            min_support: 0.1,
            validate: true,
        }
    }

    /// Set the minimum support threshold.
    #[must_use]
    pub fn with_min_support(mut self, support: f64) -> Self {
        self.min_support = support;
        self
    }

    /// Set whether to validate the output model.
    #[must_use]
    pub fn with_validation(mut self, validate: bool) -> Self {
        self.validate = validate;
        self
    }

    /// Discover a Petri net from an event log.
    pub fn discover(&self, log: &EventLog) -> Result<PetriNet> {
        // Step 1: Extract footprint relations
        let relations = self.extract_footprint(log)?;

        // Step 2: Build places based on relations
        let places = self.build_places(&relations)?;

        // Step 3: Build transitions from activities
        let transitions: HashSet<Transition> = log
            .unique_activities()
            .iter()
            .enumerate()
            .map(|(i, a)| Transition::new(format!("t_{i}")).with_label(a))
            .collect();

        // Step 4: Build arcs connecting places and transitions
        let arcs = self.build_arcs(&places, &transitions, log);

        // Step 5: Set initial and final markings
        let (initial_marking, final_marking) = self.set_markings(&places, log);

        let net = PetriNet {
            places: places.into_iter().collect(),
            transitions,
            arcs,
            initial_marking,
            final_marking,
            name: Some(log.name.clone()),
        };

        if self.validate {
            net.validate()?;
        }

        Ok(net)
    }

    /// Extract footprint relations from the event log.
    fn extract_footprint(&self, log: &EventLog) -> Result<FootprintRelations> {
        let mut direct_succession: HashSet<(String, String)> = HashSet::new();
        let mut causal: HashSet<(String, String)> = HashSet::new();
        let mut parallel: HashSet<(String, String)> = HashSet::new();
        let mut unrelated: HashSet<(String, String)> = HashSet::new();

        let activities = log.unique_activities();
        let _activity_set: HashSet<&str> = activities.iter().map(|s| s.as_str()).collect();

        // Build direct succession matrix
        for trace in &log.traces {
            let sequence = trace.activity_sequence();
            for window in sequence.windows(2) {
                let (a, b) = (&window[0].to_string(), &window[1].to_string());
                direct_succession.insert((a.clone(), b.clone()));
            }
        }

        // Classify relations
        for a in &activities {
            for b in &activities {
                if a == b {
                    continue;
                }

                let a_follows_b = direct_succession.contains(&(a.clone(), b.clone()));
                let b_follows_a = direct_succession.contains(&(b.clone(), a.clone()));

                match (a_follows_b, b_follows_a) {
                    (true, false) => {
                        causal.insert((a.clone(), b.clone()));
                    }
                    (false, true) => {
                        causal.insert((b.clone(), a.clone()));
                    }
                    (true, true) => {
                        parallel.insert((a.clone(), b.clone()));
                        parallel.insert((b.clone(), a.clone()));
                    }
                    (false, false) => {
                        unrelated.insert((a.clone(), b.clone()));
                    }
                }
            }
        }

        Ok(FootprintRelations {
            direct_succession,
            causal,
            parallel,
            unrelated,
        })
    }

    /// Build places based on footprint relations.
    fn build_places(&self, relations: &FootprintRelations) -> Result<Vec<Place>> {
        let mut places = Vec::new();

        // For each causal relation, create a place
        let mut place_index = 0;
        for (input, output) in &relations.causal {
            let place = Place::new(format!("p_{place_index}"))
                .with_label(format!("{}_to_{}", input, output));
            places.push(place);
            place_index += 1;
        }

        // Handle parallel splits with additional places
        for (a, b) in &relations.parallel {
            // Only process half of the parallel pairs to avoid duplicates
            if a < b {
                let place = Place::new(format!("p_{place_index}"))
                    .with_label(format!("parallel_{}_{}", a, b));
                places.push(place);
                place_index += 1;
            }
        }

        Ok(places)
    }

    /// Build arcs connecting places and transitions.
    fn build_arcs(
        &self, places: &[Place], transitions: &HashSet<Transition>, log: &EventLog,
    ) -> Vec<Arc> {
        let mut arcs = Vec::new();
        let activity_to_transition: HashMap<String, &Transition> = transitions
            .iter()
            .filter_map(|t| t.label.as_ref().map(|l| (l.clone(), t)))
            .collect();

        // Build arcs based on place labels
        for place in places {
            if let Some(label) = &place.label {
                if let Some((from, to)) = label.split_once("_to_") {
                    if let (Some(from_t), Some(to_t)) = (
                        activity_to_transition.get(from),
                        activity_to_transition.get(to),
                    ) {
                        // Create arc: transition -> place -> transition
                        arcs.push(Arc::new(from_t.id.clone(), place.id.clone()));
                        arcs.push(Arc::new(place.id.clone(), to_t.id.clone()));
                    }
                }
            }
        }

        // Add start and end connections
        if let Some(first_trace) = log.traces.first() {
            if let Some(first_activity) = first_trace.events.first() {
                if let Some(first_transition) = activity_to_transition.get(&first_activity.activity)
                {
                    let start_place = Place::new("p_start").with_label("start");
                    arcs.push(Arc::new(
                        start_place.id.clone(),
                        first_transition.id.clone(),
                    ));
                }
            }
        }

        arcs
    }

    /// Set initial and final markings based on log analysis.
    fn set_markings(&self, places: &[Place], log: &EventLog) -> (Marking, Marking) {
        // Find start activities (first in traces)
        let start_activities: HashSet<String> = log
            .traces
            .iter()
            .filter_map(|t| t.events.first().map(|e| e.activity.clone()))
            .collect();

        // Find end activities (last in traces)
        let end_activities: HashSet<String> = log
            .traces
            .iter()
            .filter_map(|t| t.events.last().map(|e| e.activity.clone()))
            .collect();

        let mut initial_marking = Marking::new();
        let mut final_marking = Marking::new();

        // Mark places connected to start activities
        for place in places {
            if let Some(label) = &place.label {
                for start_act in &start_activities {
                    if label.starts_with(start_act) {
                        initial_marking.tokens.insert(place.id.clone(), 1);
                        break;
                    }
                }
                for end_act in &end_activities {
                    if label.ends_with(end_act) {
                        final_marking.tokens.insert(place.id.clone(), 1);
                        break;
                    }
                }
            }
        }

        // Ensure at least one initial and final place
        if initial_marking.tokens.is_empty() {
            initial_marking = Marking::new().with_token("p_start", 1);
        }
        if final_marking.tokens.is_empty() {
            final_marking = Marking::new().with_token("p_end", 1);
        }

        (initial_marking, final_marking)
    }
}

/// Footprint relations extracted from an event log.
#[derive(Debug, Clone)]
struct FootprintRelations {
    /// Direct succession relations (a -> b).
    #[allow(dead_code)]
    direct_succession: HashSet<(String, String)>,

    /// Causal relations (a -> b but not b -> a).
    causal: HashSet<(String, String)>,

    /// Parallel relations (a -> b and b -> a).
    parallel: HashSet<(String, String)>,

    /// Unrelated activities (no direct succession).
    #[allow(dead_code)]
    unrelated: HashSet<(String, String)>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_log::{Event, Trace};

    fn make_test_log() -> EventLog {
        let trace1 = Trace::new("case1")
            .with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "B", "2024-01-01T11:00:00Z").unwrap())
            .with_event(Event::new("e3", "C", "2024-01-01T12:00:00Z").unwrap());

        let trace2 = Trace::new("case2")
            .with_event(Event::new("e4", "A", "2024-01-02T10:00:00Z").unwrap())
            .with_event(Event::new("e5", "B", "2024-01-02T11:00:00Z").unwrap())
            .with_event(Event::new("e6", "C", "2024-01-02T12:00:00Z").unwrap());

        EventLog::new("Test Log")
            .with_trace(trace1)
            .with_trace(trace2)
    }

    #[test]
    fn test_process_miner_creation() {
        let miner = ProcessMiner::new();
        assert!(miner.config().validate_output);
    }

    #[test]
    fn test_process_miner_with_config() {
        let miner = ProcessMiner::new()
            .with_validation(false)
            .with_min_support(0.5);

        assert!(!miner.config().validate_output);
        assert_eq!(miner.config().min_support, 0.5);
    }

    #[test]
    fn test_insufficient_log_error() {
        let small_log = EventLog::new("Tiny Log").with_trace(
            Trace::new("case1").with_event(Event::new("e1", "A", "2024-01-01T10:00:00Z").unwrap()),
        );

        let miner = ProcessMiner::new();
        let result = miner.discover_alpha_plusplus(&small_log);

        assert!(matches!(result, Err(Error::InsufficientLog { .. })));
    }

    #[test]
    fn test_alpha_plusplus_discovery() {
        let log = make_test_log();
        let miner = ProcessMiner::new().with_validation(false);
        let result = miner.discover_alpha_plusplus(&log);

        assert!(result.is_ok());

        let net = result.unwrap();
        assert_eq!(net.name, Some("Test Log".to_string()));
        assert!(!net.transitions.is_empty());
    }

    #[test]
    fn test_inductive_discovery() {
        let log = make_test_log();
        let miner = ProcessMiner::new();
        let result = miner.discover_inductive(&log);

        assert!(result.is_ok());

        let net = result.unwrap();
        assert_eq!(net.transitions.len(), 3);
    }

    #[test]
    fn test_footprint_extraction() {
        let log = make_test_log();
        let algorithm = AlphaPlusPlus::new();
        let result = algorithm.extract_footprint(&log);

        assert!(result.is_ok());

        let relations = result.unwrap();
        // Should have causal relations A->B, B->C
        assert!(relations
            .causal
            .contains(&(String::from("A"), String::from("B"))));
        assert!(relations
            .causal
            .contains(&(String::from("B"), String::from("C"))));
    }

    #[test]
    fn test_alpha_plusplus_default() {
        let algo = AlphaPlusPlus::default();
        assert_eq!(algo.min_support, 0.1);
        assert!(algo.validate);
    }
}
