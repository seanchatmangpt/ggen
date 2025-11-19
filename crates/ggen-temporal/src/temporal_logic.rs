//! Temporal logic operators and reasoning engine
//!
//! This module implements temporal logic for reasoning about properties over time.
//! It provides operators from Linear Temporal Logic (LTL) and Computation Tree Logic (CTL).
//!
//! ## Temporal Operators
//!
//! ### Linear Temporal Logic (LTL)
//! - **Always (□)**: Property holds at all future times
//! - **Eventually (◇)**: Property holds at some future time
//! - **Next (○)**: Property holds at the next time step
//! - **Until (U)**: Property A holds until B becomes true
//! - **Release (R)**: Property A releases property B
//!
//! ### Example
//!
//! ```rust
//! use ggen_temporal::temporal_logic::*;
//!
//! // "Always eventually the system stabilizes"
//! let formula = TemporalFormula::Always(Box::new(
//!     TemporalFormula::Eventually(Box::new(
//!         TemporalFormula::Predicate("stabilized".to_string())
//!     ))
//! ));
//!
//! // "Tests pass until deployment"
//! let formula2 = TemporalFormula::Until(
//!     Box::new(TemporalFormula::Predicate("tests_pass".to_string())),
//!     Box::new(TemporalFormula::Predicate("deployed".to_string()))
//! );
//! ```

use crate::event_sourcing::{Event, EventStream};
use crate::vector_clock::VectorTime;
use crate::{Result, TemporalError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Truth value in temporal logic
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TruthValue {
    True,
    False,
    Unknown,
}

impl TruthValue {
    #[must_use]
    pub const fn is_true(&self) -> bool {
        matches!(self, Self::True)
    }

    #[must_use]
    pub const fn is_false(&self) -> bool {
        matches!(self, Self::False)
    }

    #[must_use]
    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

impl std::ops::Not for TruthValue {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Unknown => Self::Unknown,
        }
    }
}

/// Temporal logic operators
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemporalOperator {
    /// Always (□) - property holds at all future times
    Always,
    /// Eventually (◇) - property holds at some future time
    Eventually,
    /// Next (○) - property holds at the next time step
    Next,
    /// Until (U) - property A holds until B becomes true
    Until,
    /// Release (R) - property B holds until A releases it
    Release,
    /// Weak Until - like Until but B doesn't have to become true
    WeakUntil,
}

/// Temporal logic formula (syntax tree)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TemporalFormula {
    /// Atomic predicate (base case)
    Predicate(String),

    /// Boolean true
    True,

    /// Boolean false
    False,

    /// Logical negation
    Not(Box<TemporalFormula>),

    /// Logical conjunction (AND)
    And(Box<TemporalFormula>, Box<TemporalFormula>),

    /// Logical disjunction (OR)
    Or(Box<TemporalFormula>, Box<TemporalFormula>),

    /// Logical implication
    Implies(Box<TemporalFormula>, Box<TemporalFormula>),

    /// Always (□)
    Always(Box<TemporalFormula>),

    /// Eventually (◇)
    Eventually(Box<TemporalFormula>),

    /// Next (○)
    Next(Box<TemporalFormula>),

    /// Until (U)
    Until(Box<TemporalFormula>, Box<TemporalFormula>),

    /// Release (R)
    Release(Box<TemporalFormula>, Box<TemporalFormula>),
}

impl TemporalFormula {
    /// Create a predicate formula
    #[must_use]
    pub fn predicate(name: impl Into<String>) -> Self {
        Self::Predicate(name.into())
    }

    /// Create an Always formula
    #[must_use]
    pub fn always(formula: Self) -> Self {
        Self::Always(Box::new(formula))
    }

    /// Create an Eventually formula
    #[must_use]
    pub fn eventually(formula: Self) -> Self {
        Self::Eventually(Box::new(formula))
    }

    /// Create a Next formula
    #[must_use]
    pub fn next(formula: Self) -> Self {
        Self::Next(Box::new(formula))
    }

    /// Create an Until formula
    #[must_use]
    pub fn until(left: Self, right: Self) -> Self {
        Self::Until(Box::new(left), Box::new(right))
    }

    /// Negate a formula
    #[must_use]
    pub fn not(formula: Self) -> Self {
        Self::Not(Box::new(formula))
    }

    /// Conjunction of two formulas
    #[must_use]
    pub fn and(left: Self, right: Self) -> Self {
        Self::And(Box::new(left), Box::new(right))
    }

    /// Disjunction of two formulas
    #[must_use]
    pub fn or(left: Self, right: Self) -> Self {
        Self::Or(Box::new(left), Box::new(right))
    }

    /// Implication
    #[must_use]
    pub fn implies(left: Self, right: Self) -> Self {
        Self::Implies(Box::new(left), Box::new(right))
    }
}

impl std::fmt::Display for TemporalFormula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Predicate(name) => write!(f, "{name}"),
            Self::True => write!(f, "⊤"),
            Self::False => write!(f, "⊥"),
            Self::Not(inner) => write!(f, "¬({inner})"),
            Self::And(left, right) => write!(f, "({left} ∧ {right})"),
            Self::Or(left, right) => write!(f, "({left} ∨ {right})"),
            Self::Implies(left, right) => write!(f, "({left} → {right})"),
            Self::Always(inner) => write!(f, "□({inner})"),
            Self::Eventually(inner) => write!(f, "◇({inner})"),
            Self::Next(inner) => write!(f, "○({inner})"),
            Self::Until(left, right) => write!(f, "({left} U {right})"),
            Self::Release(left, right) => write!(f, "({left} R {right})"),
        }
    }
}

/// State at a point in time (used for evaluation)
#[derive(Debug, Clone)]
pub struct TemporalState {
    /// Vector time for this state
    pub vector_time: VectorTime,
    /// Predicates that are true in this state
    pub predicates: HashMap<String, TruthValue>,
}

impl TemporalState {
    #[must_use]
    pub fn new(vector_time: VectorTime) -> Self {
        Self {
            vector_time,
            predicates: HashMap::new(),
        }
    }

    /// Set a predicate value
    pub fn set_predicate(&mut self, name: String, value: TruthValue) {
        self.predicates.insert(name, value);
    }

    /// Get a predicate value
    #[must_use]
    pub fn get_predicate(&self, name: &str) -> TruthValue {
        self.predicates.get(name).copied().unwrap_or(TruthValue::Unknown)
    }
}

/// Temporal reasoning engine
pub struct TemporalReasoner {
    /// Predicate evaluator function
    evaluator: Box<dyn Fn(&Event) -> HashMap<String, TruthValue> + Send + Sync>,
}

impl TemporalReasoner {
    /// Create a new temporal reasoner with a predicate evaluator
    pub fn new<F>(evaluator: F) -> Self
    where
        F: Fn(&Event) -> HashMap<String, TruthValue> + Send + Sync + 'static,
    {
        Self {
            evaluator: Box::new(evaluator),
        }
    }

    /// Create a default reasoner (all predicates unknown)
    #[must_use]
    pub fn default_evaluator() -> Self {
        Self::new(|_event| HashMap::new())
    }

    /// Evaluate a temporal formula over an event stream
    pub fn evaluate(&self, formula: &TemporalFormula, stream: &EventStream) -> Result<TruthValue> {
        let states = self.build_states(stream);

        if states.is_empty() {
            return Ok(TruthValue::Unknown);
        }

        self.evaluate_at_position(formula, &states, 0)
    }

    /// Build temporal states from event stream
    fn build_states(&self, stream: &EventStream) -> Vec<TemporalState> {
        stream
            .events()
            .iter()
            .map(|event| {
                let predicates = (self.evaluator)(event);
                TemporalState {
                    vector_time: event.vector_time.clone(),
                    predicates,
                }
            })
            .collect()
    }

    /// Evaluate formula at a specific position in the state sequence
    fn evaluate_at_position(
        &self,
        formula: &TemporalFormula,
        states: &[TemporalState],
        pos: usize,
    ) -> Result<TruthValue> {
        if pos >= states.len() {
            return Ok(TruthValue::Unknown);
        }

        let current_state = &states[pos];

        match formula {
            TemporalFormula::True => Ok(TruthValue::True),
            TemporalFormula::False => Ok(TruthValue::False),

            TemporalFormula::Predicate(name) => Ok(current_state.get_predicate(name)),

            TemporalFormula::Not(inner) => {
                let result = self.evaluate_at_position(inner, states, pos)?;
                Ok(!result)
            }

            TemporalFormula::And(left, right) => {
                let left_result = self.evaluate_at_position(left, states, pos)?;
                let right_result = self.evaluate_at_position(right, states, pos)?;

                Ok(match (left_result, right_result) {
                    (TruthValue::True, TruthValue::True) => TruthValue::True,
                    (TruthValue::False, _) | (_, TruthValue::False) => TruthValue::False,
                    _ => TruthValue::Unknown,
                })
            }

            TemporalFormula::Or(left, right) => {
                let left_result = self.evaluate_at_position(left, states, pos)?;
                let right_result = self.evaluate_at_position(right, states, pos)?;

                Ok(match (left_result, right_result) {
                    (TruthValue::True, _) | (_, TruthValue::True) => TruthValue::True,
                    (TruthValue::False, TruthValue::False) => TruthValue::False,
                    _ => TruthValue::Unknown,
                })
            }

            TemporalFormula::Implies(left, right) => {
                let left_result = self.evaluate_at_position(left, states, pos)?;
                let right_result = self.evaluate_at_position(right, states, pos)?;

                Ok(match (left_result, right_result) {
                    (TruthValue::False, _) => TruthValue::True,
                    (TruthValue::True, TruthValue::True) => TruthValue::True,
                    (TruthValue::True, TruthValue::False) => TruthValue::False,
                    _ => TruthValue::Unknown,
                })
            }

            TemporalFormula::Next(inner) => {
                if pos + 1 < states.len() {
                    self.evaluate_at_position(inner, states, pos + 1)
                } else {
                    Ok(TruthValue::Unknown)
                }
            }

            TemporalFormula::Always(inner) => {
                // Check if formula holds at all future positions
                for i in pos..states.len() {
                    let result = self.evaluate_at_position(inner, states, i)?;
                    if result.is_false() {
                        return Ok(TruthValue::False);
                    }
                    if result.is_unknown() {
                        return Ok(TruthValue::Unknown);
                    }
                }
                Ok(TruthValue::True)
            }

            TemporalFormula::Eventually(inner) => {
                // Check if formula holds at any future position
                for i in pos..states.len() {
                    let result = self.evaluate_at_position(inner, states, i)?;
                    if result.is_true() {
                        return Ok(TruthValue::True);
                    }
                }
                Ok(TruthValue::False)
            }

            TemporalFormula::Until(left, right) => {
                // left U right: left holds until right becomes true
                for i in pos..states.len() {
                    let right_result = self.evaluate_at_position(right, states, i)?;
                    if right_result.is_true() {
                        return Ok(TruthValue::True);
                    }

                    let left_result = self.evaluate_at_position(left, states, i)?;
                    if left_result.is_false() {
                        return Ok(TruthValue::False);
                    }
                }
                Ok(TruthValue::False)
            }

            TemporalFormula::Release(left, right) => {
                // left R right: right holds until left releases it
                for i in pos..states.len() {
                    let left_result = self.evaluate_at_position(left, states, i)?;
                    if left_result.is_true() {
                        return Ok(TruthValue::True);
                    }

                    let right_result = self.evaluate_at_position(right, states, i)?;
                    if right_result.is_false() {
                        return Ok(TruthValue::False);
                    }
                }
                Ok(TruthValue::True)
            }
        }
    }

    /// Check if a property holds throughout the entire stream
    pub fn check_invariant(&self, predicate: &str, stream: &EventStream) -> Result<bool> {
        let formula = TemporalFormula::always(TemporalFormula::predicate(predicate));
        let result = self.evaluate(&formula, stream)?;
        Ok(result.is_true())
    }

    /// Check if a property eventually holds
    pub fn check_liveness(&self, predicate: &str, stream: &EventStream) -> Result<bool> {
        let formula = TemporalFormula::eventually(TemporalFormula::predicate(predicate));
        let result = self.evaluate(&formula, stream)?;
        Ok(result.is_true())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_sourcing::{Event, EventData, EventType};

    fn create_test_event(id: u32, predicates: Vec<(&str, bool)>) -> Event {
        let mut event = Event::new(
            format!("entity-{id}"),
            EventType::Updated,
            EventData::Custom {
                data: serde_json::json!({}),
            },
            VectorTime::new(),
        );

        // Store predicates in custom data for testing
        let pred_map: HashMap<String, bool> = predicates
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect();

        event.data = EventData::Custom {
            data: serde_json::json!({"predicates": pred_map}),
        };

        event
    }

    fn test_evaluator(event: &Event) -> HashMap<String, TruthValue> {
        let mut result = HashMap::new();

        if let EventData::Custom { data } = &event.data {
            if let Some(predicates) = data.get("predicates") {
                if let Some(obj) = predicates.as_object() {
                    for (key, value) in obj {
                        if let Some(bool_val) = value.as_bool() {
                            result.insert(
                                key.clone(),
                                if bool_val {
                                    TruthValue::True
                                } else {
                                    TruthValue::False
                                },
                            );
                        }
                    }
                }
            }
        }

        result
    }

    #[test]
    fn test_truth_value_operations() {
        assert!(TruthValue::True.is_true());
        assert!(TruthValue::False.is_false());
        assert!(TruthValue::Unknown.is_unknown());

        assert_eq!(!TruthValue::True, TruthValue::False);
        assert_eq!(!TruthValue::False, TruthValue::True);
    }

    #[test]
    fn test_temporal_formula_display() {
        let formula = TemporalFormula::always(TemporalFormula::predicate("stable"));
        assert_eq!(formula.to_string(), "□(stable)");

        let formula2 = TemporalFormula::eventually(TemporalFormula::predicate("done"));
        assert_eq!(formula2.to_string(), "◇(done)");
    }

    #[test]
    fn test_always_operator() {
        let events = vec![
            create_test_event(1, vec![("stable", true)]),
            create_test_event(2, vec![("stable", true)]),
            create_test_event(3, vec![("stable", true)]),
        ];

        let stream = EventStream::new(events);
        let reasoner = TemporalReasoner::new(test_evaluator);

        let formula = TemporalFormula::always(TemporalFormula::predicate("stable"));
        let result = reasoner.evaluate(&formula, &stream).unwrap();

        assert_eq!(result, TruthValue::True);
    }

    #[test]
    fn test_eventually_operator() {
        let events = vec![
            create_test_event(1, vec![("done", false)]),
            create_test_event(2, vec![("done", false)]),
            create_test_event(3, vec![("done", true)]),
        ];

        let stream = EventStream::new(events);
        let reasoner = TemporalReasoner::new(test_evaluator);

        let formula = TemporalFormula::eventually(TemporalFormula::predicate("done"));
        let result = reasoner.evaluate(&formula, &stream).unwrap();

        assert_eq!(result, TruthValue::True);
    }

    #[test]
    fn test_until_operator() {
        let events = vec![
            create_test_event(1, vec![("testing", true), ("deployed", false)]),
            create_test_event(2, vec![("testing", true), ("deployed", false)]),
            create_test_event(3, vec![("testing", true), ("deployed", true)]),
        ];

        let stream = EventStream::new(events);
        let reasoner = TemporalReasoner::new(test_evaluator);

        let formula = TemporalFormula::until(
            TemporalFormula::predicate("testing"),
            TemporalFormula::predicate("deployed"),
        );

        let result = reasoner.evaluate(&formula, &stream).unwrap();
        assert_eq!(result, TruthValue::True);
    }

    #[test]
    fn test_check_invariant() {
        let events = vec![
            create_test_event(1, vec![("safe", true)]),
            create_test_event(2, vec![("safe", true)]),
            create_test_event(3, vec![("safe", true)]),
        ];

        let stream = EventStream::new(events);
        let reasoner = TemporalReasoner::new(test_evaluator);

        assert!(reasoner.check_invariant("safe", &stream).unwrap());
    }

    #[test]
    fn test_complex_formula() {
        let events = vec![
            create_test_event(1, vec![("a", true), ("b", false)]),
            create_test_event(2, vec![("a", true), ("b", true)]),
        ];

        let stream = EventStream::new(events);
        let reasoner = TemporalReasoner::new(test_evaluator);

        // (a AND eventually b)
        let formula = TemporalFormula::and(
            TemporalFormula::predicate("a"),
            TemporalFormula::eventually(TemporalFormula::predicate("b")),
        );

        let result = reasoner.evaluate(&formula, &stream).unwrap();
        assert_eq!(result, TruthValue::True);
    }
}
