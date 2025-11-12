//! Compile-time tests for poka-yoke design
//!
//! These tests verify that invalid operations fail to compile, demonstrating
//! that the type system prevents entire classes of errors.

#[cfg(test)]
mod compile_time_tests {
    use super::super::{
        model::PhaseBuilder, state::LifecycleState, state_machine::*,
        state_validation::ValidatedLifecycleState,
    };

    /// Test that phase builder requires at least one command
    #[test]
    fn test_phase_builder_requires_command() {
        let builder = PhaseBuilder::new("test");
        // This should fail at compile time if we try to use it without commands
        // But since build() returns Result, we test runtime validation
        let result = builder.build();
        assert!(result.is_err());
    }

    /// Test that validated phase has commands
    #[test]
    fn test_validated_phase_has_commands() {
        let phase = PhaseBuilder::new("build")
            .command("cargo build")
            .build()
            .unwrap();

        let commands = phase.commands();
        assert!(!commands.is_empty());
        assert_eq!(commands[0], "cargo build");
    }

    /// Test state machine valid transitions
    #[test]
    fn test_state_machine_valid_transitions() {
        let mut lifecycle = LifecycleStateMachine::<Initial>::new();
        // Record phases as they would be executed
        lifecycle
            .state_mut()
            .record_run("init".to_string(), 0, 100, true);
        let lifecycle = lifecycle.init().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Initialized> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("setup".to_string(), 100, 200, true);
        let lifecycle = lifecycle.setup().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Setup> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("build".to_string(), 200, 300, true);
        let lifecycle = lifecycle.build().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Built> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("test".to_string(), 300, 400, true);
        let lifecycle = lifecycle.test().unwrap();

        let mut lifecycle = LifecycleStateMachine::<Tested> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("deploy".to_string(), 400, 500, true);
        let lifecycle = lifecycle.deploy().unwrap();

        // All transitions succeeded
        assert!(lifecycle.has_completed_phase("init"));
        assert!(lifecycle.has_completed_phase("deploy"));
    }

    /// Test that state validation catches invalid state
    #[test]
    fn test_state_validation_catches_invalid_state() {
        let mut state = LifecycleState::default();
        // Deploy without test should fail (critical safety check)
        state.record_run("deploy".to_string(), 0, 100, true);

        let result = ValidatedLifecycleState::new(state);
        assert!(result.is_err());
    }

    /// Test that validated state can be created from valid state
    #[test]
    fn test_validated_state_from_valid_state() {
        let mut state = LifecycleState::default();
        state.record_run("init".to_string(), 0, 100, true);
        state.record_run("setup".to_string(), 100, 200, true);

        let validated = ValidatedLifecycleState::new(state).unwrap();
        assert!(validated.state().has_completed_phase("init"));
        assert!(validated.state().has_completed_phase("setup"));
    }
}

// Note: True compile-time tests would be in a separate file that's expected to fail compilation.
// For example:
//
// ```rust
// // This file should NOT compile
// fn test_invalid_transition() {
//     let lifecycle = LifecycleStateMachine::<Initial>::new();
//     lifecycle.deploy(); // Compile error: method doesn't exist
// }
// ```
//
// These would be tested using `trybuild` crate or similar compile-fail test framework.
