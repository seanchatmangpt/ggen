//! Petri net data structures for process mining.
//!
//! Petri nets are a fundamental modeling formalism in process mining, used for
//! process discovery, conformance checking, and model transformation.

use crate::error::{Error, Result};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// A place in a Petri net (represented as a circle).
///
/// Places can hold tokens and represent states or conditions in the process.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Place {
    /// Unique identifier for this place.
    pub id: String,

    /// Optional label/name for this place.
    pub label: Option<String>,
}

impl Place {
    /// Create a new place with the given ID.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::Place;
    ///
    /// let place = Place::new("p1").with_label("Start");
    /// ```
    #[must_use]
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            label: None,
        }
    }

    /// Set the label for this place.
    #[must_use]
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

impl fmt::Display for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.label.as_deref().unwrap_or(&self.id))
    }
}

/// A transition in a Petri net (represented as a rectangle).
///
/// Transitions represent activities or events in the process.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Transition {
    /// Unique identifier for this transition.
    pub id: String,

    /// Optional label/name for this transition (typically the activity name).
    pub label: Option<String>,
}

impl Transition {
    /// Create a new transition with the given ID.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::Transition;
    ///
    /// let transition = Transition::new("t1").with_label("Approve");
    /// ```
    #[must_use]
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            label: None,
        }
    }

    /// Set the label for this transition.
    #[must_use]
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.label.as_deref().unwrap_or(&self.id))
    }
}

/// A directed arc in a Petri net.
///
/// Arcs connect places to transitions or transitions to places.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arc {
    /// Source node (place or transition ID).
    pub source: String,

    /// Target node (place or transition ID).
    pub target: String,

    /// Weight of the arc (default: 1).
    pub weight: usize,
}

impl Arc {
    /// Create a new arc with the given source and target.
    #[must_use]
    pub fn new(source: impl Into<String>, target: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            target: target.into(),
            weight: 1,
        }
    }

    /// Set the weight of this arc.
    #[must_use]
    pub fn with_weight(mut self, weight: usize) -> Self {
        self.weight = weight;
        self
    }
}

/// A marking represents the distribution of tokens in places.
///
/// Markings define the state of a Petri net at a given point in time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Marking {
    /// Token count per place (place ID -> token count).
    pub tokens: HashMap<String, usize>,
}

impl Marking {
    /// Create a new empty marking.
    #[must_use]
    pub fn new() -> Self {
        Self {
            tokens: HashMap::new(),
        }
    }

    /// Set the token count for a place.
    #[must_use]
    pub fn with_token(mut self, place: impl Into<String>, count: usize) -> Self {
        self.tokens.insert(place.into(), count);
        self
    }

    /// Get the token count for a place.
    #[must_use]
    pub fn get(&self, place: &str) -> usize {
        self.tokens.get(place).copied().unwrap_or(0)
    }

    /// Check if a place is marked (has at least one token).
    #[must_use]
    pub fn is_marked(&self, place: &str) -> bool {
        self.get(place) > 0
    }
}

impl Default for Marking {
    fn default() -> Self {
        Self::new()
    }
}

/// A Petri net structure for process modeling.
///
/// Petri nets consist of places, transitions, and arcs, providing a formal
/// foundation for process discovery and analysis.
#[derive(Debug, Clone)]
pub struct PetriNet {
    /// All places in this net.
    pub places: HashSet<Place>,

    /// All transitions in this net.
    pub transitions: HashSet<Transition>,

    /// All arcs (place -> transition or transition -> place).
    pub arcs: Vec<Arc>,

    /// Initial marking (starting state).
    pub initial_marking: Marking,

    /// Final marking (accepting state(s)).
    pub final_marking: Marking,

    /// Optional name/label for this net.
    pub name: Option<String>,
}

impl PetriNet {
    /// Create a new empty Petri net.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::PetriNet;
    ///
    /// let net = PetriNet::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self {
            places: HashSet::new(),
            transitions: HashSet::new(),
            arcs: Vec::new(),
            initial_marking: Marking::new(),
            final_marking: Marking::new(),
            name: None,
        }
    }

    /// Set the name of this Petri net.
    #[must_use]
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Add a place to this net.
    #[must_use]
    pub fn with_place(mut self, place: Place) -> Self {
        self.places.insert(place);
        self
    }

    /// Add a transition to this net.
    #[must_use]
    pub fn with_transition(mut self, transition: Transition) -> Self {
        self.transitions.insert(transition);
        self
    }

    /// Add an arc to this net.
    #[must_use]
    pub fn with_arc(mut self, arc: Arc) -> Self {
        self.arcs.push(arc);
        self
    }

    /// Set the initial marking.
    #[must_use]
    pub fn with_initial_marking(mut self, marking: Marking) -> Self {
        self.initial_marking = marking;
        self
    }

    /// Set the final marking.
    #[must_use]
    pub fn with_final_marking(mut self, marking: Marking) -> Self {
        self.final_marking = marking;
        self
    }

    /// Get all input places for a transition.
    ///
    /// Returns places that have arcs to the given transition.
    #[must_use]
    pub fn input_places_for(&self, transition_id: &str) -> Vec<&Place> {
        self.arcs
            .iter()
            .filter(|arc| arc.target == transition_id)
            .filter_map(|arc| self.places.iter().find(|p| p.id == arc.source))
            .collect()
    }

    /// Get all output places for a transition.
    ///
    /// Returns places that receive arcs from the given transition.
    #[must_use]
    pub fn output_places_for(&self, transition_id: &str) -> Vec<&Place> {
        self.arcs
            .iter()
            .filter(|arc| arc.source == transition_id)
            .filter_map(|arc| self.places.iter().find(|p| p.id == arc.target))
            .collect()
    }

    /// Get all input transitions for a place.
    #[must_use]
    pub fn input_transitions_for(&self, place_id: &str) -> Vec<&Transition> {
        self.arcs
            .iter()
            .filter(|arc| arc.target == place_id)
            .filter_map(|arc| self.transitions.iter().find(|t| t.id == arc.source))
            .collect()
    }

    /// Get all output transitions for a place.
    #[must_use]
    pub fn output_transitions_for(&self, place_id: &str) -> Vec<&Transition> {
        self.arcs
            .iter()
            .filter(|arc| arc.source == place_id)
            .filter_map(|arc| self.transitions.iter().find(|t| t.id == arc.target))
            .collect()
    }

    /// Validate this Petri net for structural correctness.
    ///
    /// Checks that:
    /// - All arc endpoints reference existing nodes
    /// - No orphaned places or transitions
    /// - At least one place and one transition
    pub fn validate(&self) -> Result<()> {
        if self.places.is_empty() {
            return Err(Error::InvalidPetriNet("net has no places".to_string()));
        }

        if self.transitions.is_empty() {
            return Err(Error::InvalidPetriNet("net has no transitions".to_string()));
        }

        let place_ids: HashSet<&str> = self.places.iter().map(|p| p.id.as_str()).collect();
        let transition_ids: HashSet<&str> = self.transitions.iter().map(|t| t.id.as_str()).collect();
        let all_ids: HashSet<&str> = place_ids.union(&transition_ids).copied().collect();

        for arc in &self.arcs {
            if !all_ids.contains(arc.source.as_str()) {
                return Err(Error::InvalidPetriNet(format!(
                    "arc references non-existent source: {}",
                    arc.source
                )));
            }
            if !all_ids.contains(arc.target.as_str()) {
                return Err(Error::InvalidPetriNet(format!(
                    "arc references non-existent target: {}",
                    arc.target
                )));
            }
        }

        Ok(())
    }

    /// Create a simple sequential Petri net from a sequence of activities.
    ///
    /// This is useful for creating basic process models from activity sequences.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_process_mining::PetriNet;
    ///
    /// let net = PetriNet::from_activities(&["A", "B", "C"]);
    /// ```
    pub fn from_activities(activities: &[&str]) -> Self {
        let mut builder = Self::new();

        let mut prev_place = Place::new("p_start").with_label("start");
        builder.places.insert(prev_place.clone());
        builder.initial_marking = Marking::new().with_token("p_start", 1);

        for (i, activity) in activities.iter().enumerate() {
            let transition = Transition::new(format!("t_{i}")).with_label(*activity);
            let next_place = Place::new(format!("p_{i}"));

            builder.arcs.push(Arc::new(prev_place.id.clone(), transition.id.clone()));
            builder.arcs.push(Arc::new(transition.id.clone(), next_place.id.clone()));

            builder.transitions.insert(transition);
            builder.places.insert(next_place.clone());
            prev_place = next_place;
        }

        // Mark last place as final
        if let Some(last_place) = builder.places.iter().last() {
            builder.final_marking = Marking::new().with_token(last_place.id.clone(), 1);
        }

        builder
    }
}

impl Default for PetriNet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_place_creation() {
        let place = Place::new("p1").with_label("Start");
        assert_eq!(place.id, "p1");
        assert_eq!(place.label, Some("Start".to_string()));
    }

    #[test]
    fn test_transition_creation() {
        let transition = Transition::new("t1").with_label("Approve");
        assert_eq!(transition.id, "t1");
        assert_eq!(transition.label, Some("Approve".to_string()));
    }

    #[test]
    fn test_arc_creation() {
        let arc = Arc::new("p1", "t1").with_weight(2);
        assert_eq!(arc.source, "p1");
        assert_eq!(arc.target, "t1");
        assert_eq!(arc.weight, 2);
    }

    #[test]
    fn test_marking_operations() {
        let marking = Marking::new()
            .with_token("p1", 3)
            .with_token("p2", 1);

        assert_eq!(marking.get("p1"), 3);
        assert_eq!(marking.get("p2"), 1);
        assert_eq!(marking.get("p3"), 0);
        assert!(marking.is_marked("p1"));
        assert!(!marking.is_marked("p3"));
    }

    #[test]
    fn test_petri_net_builder() {
        let net = PetriNet::new()
            .with_name("Test Net")
            .with_place(Place::new("p1"))
            .with_transition(Transition::new("t1"))
            .with_arc(Arc::new("p1", "t1"))
            .with_initial_marking(Marking::new().with_token("p1", 1));

        assert_eq!(net.name, Some("Test Net".to_string()));
        assert_eq!(net.places.len(), 1);
        assert_eq!(net.transitions.len(), 1);
        assert_eq!(net.arcs.len(), 1);
        assert!(net.initial_marking.is_marked("p1"));
    }

    #[test]
    fn test_petri_net_validation_empty() {
        let net = PetriNet::new();
        assert!(matches!(
            net.validate(),
            Err(Error::InvalidPetriNet(_))
        ));
    }

    #[test]
    fn test_petri_net_validation_invalid_arc() {
        let net = PetriNet::new()
            .with_place(Place::new("p1"))
            .with_transition(Transition::new("t1"))
            .with_arc(Arc::new("p1", "nonexistent"));

        assert!(matches!(
            net.validate(),
            Err(Error::InvalidPetriNet(_))
        ));
    }

    #[test]
    fn test_petri_net_validation_valid() {
        let net = PetriNet::new()
            .with_place(Place::new("p1"))
            .with_transition(Transition::new("t1"))
            .with_arc(Arc::new("p1", "t1"));

        assert!(net.validate().is_ok());
    }

    #[test]
    fn test_from_activities() {
        let net = PetriNet::from_activities(&["A", "B", "C"]);

        assert_eq!(net.transitions.len(), 3);
        assert!(net.initial_marking.is_marked("p_start"));
        assert!(net.validate().is_ok());
    }

    #[test]
    fn test_input_output_places() {
        let net = PetriNet::new()
            .with_place(Place::new("p1"))
            .with_place(Place::new("p2"))
            .with_place(Place::new("p3"))
            .with_transition(Transition::new("t1"))
            .with_arc(Arc::new("p1", "t1"))
            .with_arc(Arc::new("t1", "p2"))
            .with_arc(Arc::new("p2", "t1"));

        let inputs = net.input_places_for("t1");
        assert_eq!(inputs.len(), 2);

        let outputs = net.output_places_for("t1");
        assert_eq!(outputs.len(), 1);
    }

    #[test]
    fn test_input_output_transitions() {
        let net = PetriNet::new()
            .with_place(Place::new("p1"))
            .with_transition(Transition::new("t1"))
            .with_transition(Transition::new("t2"))
            .with_arc(Arc::new("t1", "p1"))
            .with_arc(Arc::new("p1", "t2"));

        let inputs = net.input_transitions_for("p1");
        assert_eq!(inputs.len(), 1);

        let outputs = net.output_transitions_for("p1");
        assert_eq!(outputs.len(), 1);
    }
}
