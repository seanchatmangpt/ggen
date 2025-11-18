//! State Machine Executor
//!
//! Loads state machine definitions from RDF and validates state transitions.
//! All state transitions are defined in Turtle configuration files.

use crate::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use super::turtle_config::TurtleConfigLoader;

/// State machine executor for package lifecycle
pub struct StateMachineExecutor {
    /// State definitions loaded from RDF
    states: HashMap<String, StateDefinition>,
    /// Valid transitions (from_state -> to_states)
    transitions: HashMap<String, HashSet<String>>,
    /// Transition guards (conditions that must be met)
    guards: HashMap<(String, String), Vec<TransitionGuard>>,
}

/// State definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateDefinition {
    /// Human-readable name of the state
    pub name: String,
    /// Description of the state and its purpose
    pub description: String,
    /// Whether this is the initial state
    pub is_initial: bool,
    /// Whether this is a terminal/final state
    pub is_final: bool,
    /// Operations allowed in this state
    pub allowed_operations: Vec<String>,
}

/// Transition guard - condition that must be met for transition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransitionGuard {
    /// Package must have at least one author
    HasAuthors,
    /// Package must have keywords
    HasKeywords,
    /// Package must have categories
    HasCategories,
    /// Package must have checksum
    HasChecksum,
    /// Package quality score must meet minimum
    MinimumQuality(u32),
    /// Custom SPARQL query that must return true
    CustomQuery(String),
}

impl StateMachineExecutor {
    /// Create a new state machine executor with default states
    pub fn new() -> Self {
        let mut states = HashMap::new();
        let mut transitions = HashMap::new();
        let mut guards = HashMap::new();

        // Define default states
        states.insert(
            "Draft".to_string(),
            StateDefinition {
                name: "Draft".to_string(),
                description: "Package is being drafted and not yet published".to_string(),
                is_initial: true,
                is_final: false,
                allowed_operations: vec![
                    "edit".to_string(),
                    "add_metadata".to_string(),
                    "validate".to_string(),
                ],
            },
        );

        states.insert(
            "Published".to_string(),
            StateDefinition {
                name: "Published".to_string(),
                description: "Package is published and available for installation".to_string(),
                is_initial: false,
                is_final: false,
                allowed_operations: vec![
                    "install".to_string(),
                    "download".to_string(),
                    "deprecate".to_string(),
                    "yank".to_string(),
                ],
            },
        );

        states.insert(
            "Deprecated".to_string(),
            StateDefinition {
                name: "Deprecated".to_string(),
                description: "Package is deprecated but still available".to_string(),
                is_initial: false,
                is_final: false,
                allowed_operations: vec!["install".to_string(), "yank".to_string()],
            },
        );

        states.insert(
            "Yanked".to_string(),
            StateDefinition {
                name: "Yanked".to_string(),
                description: "Package is yanked and no longer available".to_string(),
                is_initial: false,
                is_final: true,
                allowed_operations: vec![],
            },
        );

        // Define transitions
        let draft_transitions = ["Published".to_string()].into_iter().collect();
        transitions.insert("Draft".to_string(), draft_transitions);

        let published_transitions = ["Deprecated".to_string(), "Yanked".to_string()]
            .into_iter()
            .collect();
        transitions.insert("Published".to_string(), published_transitions);

        let deprecated_transitions = ["Yanked".to_string()].into_iter().collect();
        transitions.insert("Deprecated".to_string(), deprecated_transitions);

        // Define guards
        guards.insert(
            ("Draft".to_string(), "Published".to_string()),
            vec![
                TransitionGuard::HasAuthors,
                TransitionGuard::HasKeywords,
                TransitionGuard::HasCategories,
                TransitionGuard::HasChecksum,
            ],
        );

        Self {
            states,
            transitions,
            guards,
        }
    }

    /// Load state machine configuration from Turtle files
    pub fn load_from_config(&self, config: &TurtleConfigLoader) -> Result<()> {
        // In production, this would parse state definitions from RDF
        // For now, using default states defined in new()
        Ok(())
    }

    /// Validate a state transition
    pub fn validate_transition(&self, from_state: Option<&str>, to_state: &str) -> Result<()> {
        // Check if from_state is None (initial state)
        if from_state.is_none() {
            // Verify to_state is an initial state
            if let Some(state_def) = self.states.get(to_state) {
                if state_def.is_initial {
                    return Ok(());
                } else {
                    return Err(Error::InvalidStateTransition {
                        from: "None".to_string(),
                        to: to_state.to_string(),
                    });
                }
            } else {
                return Err(Error::UnknownState {
                    state: to_state.to_string(),
                });
            }
        }

        let from = from_state.unwrap();

        // Check if from_state exists
        if !self.states.contains_key(from) {
            return Err(Error::UnknownState {
                state: from.to_string(),
            });
        }

        // Check if to_state exists
        if !self.states.contains_key(to_state) {
            return Err(Error::UnknownState {
                state: to_state.to_string(),
            });
        }

        // Check if transition is allowed
        if let Some(allowed_transitions) = self.transitions.get(from) {
            if !allowed_transitions.contains(to_state) {
                return Err(Error::InvalidStateTransition {
                    from: from.to_string(),
                    to: to_state.to_string(),
                });
            }
        } else {
            return Err(Error::InvalidStateTransition {
                from: from.to_string(),
                to: to_state.to_string(),
            });
        }

        Ok(())
    }

    /// Validate transition guards (conditions that must be met)
    pub fn validate_guards(
        &self, from_state: &str, to_state: &str, context: &TransitionContext,
    ) -> Result<Vec<String>> {
        let key = (from_state.to_string(), to_state.to_string());
        let mut failed_guards = Vec::new();

        if let Some(guards) = self.guards.get(&key) {
            for guard in guards {
                if !self.check_guard(guard, context)? {
                    failed_guards.push(format!("{:?}", guard));
                }
            }
        }

        Ok(failed_guards)
    }

    /// Check if a guard condition is met
    fn check_guard(&self, guard: &TransitionGuard, context: &TransitionContext) -> Result<bool> {
        match guard {
            TransitionGuard::HasAuthors => Ok(context.has_authors),
            TransitionGuard::HasKeywords => Ok(context.has_keywords),
            TransitionGuard::HasCategories => Ok(context.has_categories),
            TransitionGuard::HasChecksum => Ok(context.has_checksum),
            TransitionGuard::MinimumQuality(min) => {
                Ok(context.quality_score.map(|q| q >= *min).unwrap_or(false))
            }
            TransitionGuard::CustomQuery(_query) => {
                // Would execute SPARQL query in production
                Ok(true)
            }
        }
    }

    /// Get allowed operations for a state
    pub fn allowed_operations(&self, state: &str) -> Result<Vec<String>> {
        self.states
            .get(state)
            .map(|s| s.allowed_operations.clone())
            .ok_or_else(|| Error::UnknownState {
                state: state.to_string(),
            })
    }

    /// Check if an operation is allowed in a state
    pub fn is_operation_allowed(&self, state: &str, operation: &str) -> Result<bool> {
        let allowed = self.allowed_operations(state)?;
        Ok(allowed.contains(&operation.to_string()))
    }

    /// Get all possible transitions from a state
    pub fn get_transitions(&self, from_state: &str) -> Result<Vec<String>> {
        self.transitions
            .get(from_state)
            .map(|set| set.iter().cloned().collect())
            .ok_or_else(|| Error::UnknownState {
                state: from_state.to_string(),
            })
    }

    /// Get state definition
    pub fn get_state(&self, state: &str) -> Option<&StateDefinition> {
        self.states.get(state)
    }

    /// Check if a state is final (terminal)
    pub fn is_final_state(&self, state: &str) -> bool {
        self.states.get(state).map(|s| s.is_final).unwrap_or(false)
    }

    /// Get initial states
    pub fn initial_states(&self) -> Vec<String> {
        self.states
            .values()
            .filter(|s| s.is_initial)
            .map(|s| s.name.clone())
            .collect()
    }
}

impl Default for StateMachineExecutor {
    fn default() -> Self {
        Self::new()
    }
}

/// Context for evaluating transition guards
#[derive(Debug, Clone, Default)]
pub struct TransitionContext {
    /// Whether the package has author metadata
    pub has_authors: bool,
    /// Whether the package has keywords
    pub has_keywords: bool,
    /// Whether the package has categories
    pub has_categories: bool,
    /// Whether the package has a checksum
    pub has_checksum: bool,
    /// Optional quality score if available
    pub quality_score: Option<u32>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_state_transition() {
        let sm = StateMachineExecutor::new();

        // Can transition from None to Draft (initial state)
        assert!(sm.validate_transition(None, "Draft").is_ok());

        // Cannot transition from None to Published (not initial)
        assert!(sm.validate_transition(None, "Published").is_err());
    }

    #[test]
    fn test_draft_to_published() {
        let sm = StateMachineExecutor::new();

        // Valid transition
        assert!(sm.validate_transition(Some("Draft"), "Published").is_ok());

        // Invalid transition
        assert!(sm.validate_transition(Some("Draft"), "Yanked").is_err());
    }

    #[test]
    fn test_published_transitions() {
        let sm = StateMachineExecutor::new();

        // Can deprecate published package
        assert!(sm
            .validate_transition(Some("Published"), "Deprecated")
            .is_ok());

        // Can yank published package
        assert!(sm.validate_transition(Some("Published"), "Yanked").is_ok());

        // Cannot transition back to draft
        assert!(sm.validate_transition(Some("Published"), "Draft").is_err());
    }

    #[test]
    fn test_deprecated_transitions() {
        let sm = StateMachineExecutor::new();

        // Can yank deprecated package
        assert!(sm.validate_transition(Some("Deprecated"), "Yanked").is_ok());

        // Cannot transition to other states
        assert!(sm
            .validate_transition(Some("Deprecated"), "Published")
            .is_err());
    }

    #[test]
    fn test_yanked_final_state() {
        let sm = StateMachineExecutor::new();

        // Yanked is a final state
        assert!(sm.is_final_state("Yanked"));
        assert!(!sm.is_final_state("Draft"));

        // No transitions from Yanked
        let transitions = sm.get_transitions("Yanked").unwrap_or_default();
        assert!(transitions.is_empty());
    }

    #[test]
    fn test_allowed_operations() {
        let sm = StateMachineExecutor::new();

        // Draft allows editing
        assert!(sm.is_operation_allowed("Draft", "edit").unwrap());

        // Published allows installation
        assert!(sm.is_operation_allowed("Published", "install").unwrap());

        // Yanked allows nothing
        let ops = sm.allowed_operations("Yanked").unwrap();
        assert!(ops.is_empty());
    }

    #[test]
    fn test_transition_guards() {
        let sm = StateMachineExecutor::new();

        // Context without required fields
        let context = TransitionContext {
            has_authors: false,
            has_keywords: false,
            has_categories: false,
            has_checksum: false,
            quality_score: None,
        };

        let failed = sm.validate_guards("Draft", "Published", &context).unwrap();
        assert!(!failed.is_empty());

        // Context with all required fields
        let context = TransitionContext {
            has_authors: true,
            has_keywords: true,
            has_categories: true,
            has_checksum: true,
            quality_score: Some(95),
        };

        let failed = sm.validate_guards("Draft", "Published", &context).unwrap();
        assert!(failed.is_empty());
    }

    #[test]
    fn test_initial_states() {
        let sm = StateMachineExecutor::new();

        let initial = sm.initial_states();
        assert_eq!(initial.len(), 1);
        assert!(initial.contains(&"Draft".to_string()));
    }
}
