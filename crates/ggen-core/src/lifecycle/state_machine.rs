//! Type-level lifecycle state machine for poka-yoke error prevention
//!
//! This module provides a type-level state machine that prevents invalid lifecycle
//! phase transitions at compile time using PhantomData markers.
//!
//! # Poka-Yoke Design
//!
//! **Error Prevention**: Makes invalid phase transitions impossible at compile time.
//! For example, you cannot call `deploy()` on a state that hasn't completed `build()`.
//!
//! # Example
//!
//! ```rust
//! use ggen_core::lifecycle::state_machine::*;
//!
//! // Start with initial state
//! let lifecycle = LifecycleStateMachine::<Initial>::new();
//!
//! // Valid transitions
//! let lifecycle = lifecycle.init()?;
//! let lifecycle = lifecycle.setup()?;
//! let lifecycle = lifecycle.build()?;
//! let lifecycle = lifecycle.test()?;
//! let lifecycle = lifecycle.deploy()?;
//!
//! // Invalid transitions fail at compile time:
//! // lifecycle.deploy() // Compile error: method doesn't exist on Built state
//! ```

use super::error::{LifecycleError, Result};
use super::state::LifecycleState;
use std::marker::PhantomData;

/// Type-level markers for lifecycle states
///
/// These marker types encode the current state of the lifecycle in the type system,
/// preventing invalid transitions at compile time.
/// Initial state - lifecycle not started
pub struct Initial;

/// Initialized state - `init` phase completed
pub struct Initialized;

/// Setup state - `setup` phase completed
pub struct Setup;

/// Built state - `build` phase completed
pub struct Built;

/// Tested state - `test` phase completed
pub struct Tested;

/// Deployed state - `deploy` phase completed
pub struct Deployed;

/// Type-level lifecycle state machine
///
/// The `State` type parameter encodes the current lifecycle state, preventing
/// invalid transitions at compile time.
///
/// **Poka-yoke**: Invalid state transitions are impossible - the compiler prevents them.
#[derive(Debug, Clone)]
pub struct LifecycleStateMachine<State> {
    /// Current lifecycle state
    pub(crate) state: LifecycleState,
    /// Type-level state marker (compile-time only, zero runtime cost)
    pub(crate) _marker: PhantomData<State>,
}

impl LifecycleStateMachine<Initial> {
    /// Create a new lifecycle state machine in the initial state
    pub fn new() -> Self {
        Self {
            state: LifecycleState::default(),
            _marker: PhantomData,
        }
    }

    /// Transition from Initial to Initialized (run `init` phase)
    ///
    /// **Poka-yoke**: Can only be called on `Initial` state - compiler enforces this.
    pub fn init(self) -> Result<LifecycleStateMachine<Initialized>> {
        // Note: init() doesn't validate prerequisites since it's the entry point
        // The phase execution (in exec.rs) will record the phase run

        Ok(LifecycleStateMachine {
            state: self.state,
            _marker: PhantomData,
        })
    }
}

impl LifecycleStateMachine<Initialized> {
    /// Transition from Initialized to Setup (run `setup` phase)
    ///
    /// **Poka-yoke**: Can only be called on `Initialized` state - compiler enforces this.
    pub fn setup(self) -> Result<LifecycleStateMachine<Setup>> {
        // Validate that init has been run
        if !self.state.has_completed_phase("init") {
            return Err(LifecycleError::Other(
                "Cannot run setup: init phase not completed".into(),
            ));
        }

        Ok(LifecycleStateMachine {
            state: self.state,
            _marker: PhantomData,
        })
    }
}

impl LifecycleStateMachine<Setup> {
    /// Transition from Setup to Built (run `build` phase)
    ///
    /// **Poka-yoke**: Can only be called on `Setup` state - compiler enforces this.
    pub fn build(self) -> Result<LifecycleStateMachine<Built>> {
        // Validate that setup has been run
        if !self.state.has_completed_phase("setup") {
            return Err(LifecycleError::Other(
                "Cannot run build: setup phase not completed".into(),
            ));
        }

        Ok(LifecycleStateMachine {
            state: self.state,
            _marker: PhantomData,
        })
    }
}

impl LifecycleStateMachine<Built> {
    /// Transition from Built to Tested (run `test` phase)
    ///
    /// **Poka-yoke**: Can only be called on `Built` state - compiler enforces this.
    pub fn test(self) -> Result<LifecycleStateMachine<Tested>> {
        // Validate that build has been run
        if !self.state.has_completed_phase("build") {
            return Err(LifecycleError::Other(
                "Cannot run test: build phase not completed".into(),
            ));
        }

        Ok(LifecycleStateMachine {
            state: self.state,
            _marker: PhantomData,
        })
    }
}

impl LifecycleStateMachine<Tested> {
    /// Transition from Tested to Deployed (run `deploy` phase)
    ///
    /// **Poka-yoke**: Can only be called on `Tested` state - compiler enforces this.
    pub fn deploy(self) -> Result<LifecycleStateMachine<Deployed>> {
        // Validate that test has been run
        if !self.state.has_completed_phase("test") {
            return Err(LifecycleError::Other(
                "Cannot run deploy: test phase not completed".into(),
            ));
        }

        Ok(LifecycleStateMachine {
            state: self.state,
            _marker: PhantomData,
        })
    }
}

// Common implementations for all states

impl<State> LifecycleStateMachine<State> {
    /// Get the underlying lifecycle state
    ///
    /// This allows accessing state information regardless of the current state marker.
    pub fn state(&self) -> &LifecycleState {
        &self.state
    }

    /// Get mutable access to the underlying lifecycle state
    ///
    /// **Warning**: Modifying state directly can break type-level invariants.
    /// Prefer using transition methods when possible.
    pub fn state_mut(&mut self) -> &mut LifecycleState {
        &mut self.state
    }

    /// Check if a phase has been completed
    pub fn has_completed_phase(&self, phase: &str) -> bool {
        self.state.phase_history.iter().any(|r| r.phase == phase)
    }

    /// Get the last completed phase
    pub fn last_phase(&self) -> Option<&str> {
        self.state.last_phase.as_deref()
    }
}

impl Default for LifecycleStateMachine<Initial> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_state_transitions() {
        // State machine validates transitions but doesn't record phases
        // Phases are recorded in exec.rs during actual execution
        let mut lifecycle = LifecycleStateMachine::<Initial>::new();
        lifecycle
            .state_mut()
            .record_run("init".to_string(), 0, 100, true);
        let lifecycle = lifecycle.init().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Initialized> {
            state: lifecycle.state().clone(),
            _marker: PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("setup".to_string(), 100, 200, true);
        let lifecycle = lifecycle.setup().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Setup> {
            state: lifecycle.state().clone(),
            _marker: PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("build".to_string(), 200, 300, true);
        let lifecycle = lifecycle.build().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Built> {
            state: lifecycle.state().clone(),
            _marker: PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("test".to_string(), 300, 400, true);
        let lifecycle = lifecycle.test().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Tested> {
            state: lifecycle.state().clone(),
            _marker: PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("deploy".to_string(), 400, 500, true);
        let lifecycle = lifecycle.deploy().unwrap();

        assert!(lifecycle.has_completed_phase("init"));
        assert!(lifecycle.has_completed_phase("setup"));
        assert!(lifecycle.has_completed_phase("build"));
        assert!(lifecycle.has_completed_phase("test"));
        assert!(lifecycle.has_completed_phase("deploy"));
    }

    #[test]
    fn test_state_access() {
        let lifecycle = LifecycleStateMachine::<Initial>::new();
        assert_eq!(lifecycle.last_phase(), None);
        assert!(!lifecycle.has_completed_phase("init"));
    }

    #[test]
    fn test_invalid_transition_validation() {
        let lifecycle = LifecycleStateMachine::<Initial>::new();
        // Cannot call setup() on Initial state - compile error!
        // let lifecycle = lifecycle.setup().unwrap(); // This would fail to compile

        // But we can transition through init first (after recording the phase)
        let mut lifecycle = lifecycle;
        lifecycle
            .state_mut()
            .record_run("init".to_string(), 0, 100, true);
        let lifecycle = lifecycle.init().unwrap();
        let mut lifecycle = LifecycleStateMachine::<Initialized> {
            state: lifecycle.state().clone(),
            _marker: PhantomData,
        };
        let _lifecycle = lifecycle.setup().unwrap(); // Now this works
    }
}
