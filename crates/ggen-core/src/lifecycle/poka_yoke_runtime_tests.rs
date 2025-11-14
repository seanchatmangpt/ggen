//! Runtime tests for poka-yoke design
//!
//! These tests verify that valid operations compile and work correctly,
//! and that invalid operations are caught at runtime when compile-time
//! prevention isn't possible.

#[cfg(test)]
mod runtime_tests {
    use super::super::{
        hooks::validate_hooks,
        model::{Hooks, Make, PhaseBuilder, Project},
        state::LifecycleState,
        state_machine::*,
        state_validation::ValidatedLifecycleState,
    };
    use std::collections::BTreeMap;

    /// Test complete lifecycle flow with state machine
    test!(test_complete_lifecycle_flow, {
        // Test state machine with proper phase recording
        // In real usage, exec.rs would record phases during execution
        let mut lifecycle = LifecycleStateMachine::<Initial>::new();

        // Record init phase and transition
        lifecycle
            .state_mut()
            .record_run("init".to_string(), 0, 100, true);
        let lifecycle = lifecycle.init().unwrap();

        // Record setup phase and transition
        let mut lifecycle = LifecycleStateMachine::<Initialized> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("setup".to_string(), 100, 200, true);
        let lifecycle = lifecycle.setup().unwrap();

        // Record build phase and transition
        let mut lifecycle = LifecycleStateMachine::<Setup> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("build".to_string(), 200, 300, true);
        let lifecycle = lifecycle.build().unwrap();

        // Record test phase and transition
        let mut lifecycle = LifecycleStateMachine::<Built> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("test".to_string(), 300, 400, true);
        let lifecycle = lifecycle.test().unwrap();

        // Record deploy phase and transition
        let mut lifecycle = LifecycleStateMachine::<Tested> {
            state: lifecycle.state().clone(),
            _marker: std::marker::PhantomData,
        };
        lifecycle
            .state_mut()
            .record_run("deploy".to_string(), 400, 500, true);
        let lifecycle = lifecycle.deploy().unwrap();

        // Verify all phases completed
        assert!(lifecycle.has_completed_phase("init"));
        assert!(lifecycle.has_completed_phase("setup"));
        assert!(lifecycle.has_completed_phase("build"));
        assert!(lifecycle.has_completed_phase("test"));
        assert!(lifecycle.has_completed_phase("deploy"));
    });

    /// Test phase builder with multiple commands
    test!(test_phase_builder_multiple_commands, {
        let phase = PhaseBuilder::new("build")
            .add_command("cargo clean")
            .add_command("cargo build --release")
            .build()
            .unwrap();

        let commands = phase.commands();
        assert_eq!(commands.len(), 2);
        assert_eq!(commands[0], "cargo clean");
        assert_eq!(commands[1], "cargo build --release");
    });

    /// Test hook validation with valid hooks
    test!(test_hook_validation_valid, {
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["validate".to_string()]);
        hooks.after_build = Some(vec!["notify".to_string()]);

        let mut lifecycle = BTreeMap::new();
        lifecycle.insert(
            "validate".to_string(),
            PhaseBuilder::new("validate")
                .command("echo validate")
                .build()
                .unwrap()
                .as_ref()
                .clone(),
        );
        lifecycle.insert(
            "build".to_string(),
            PhaseBuilder::new("build")
                .command("cargo build")
                .build()
                .unwrap()
                .as_ref()
                .clone(),
        );
        lifecycle.insert(
            "notify".to_string(),
            PhaseBuilder::new("notify")
                .command("echo notify")
                .build()
                .unwrap()
                .as_ref()
                .clone(),
        );

        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle,
            hooks: Some(hooks),
        };

        let validated = validate_hooks(&make).unwrap();
        assert!(validated.hooks().before_build.is_some());
    });

    /// Test hook validation catches invalid phase reference
    test!(test_hook_validation_invalid_phase, {
        let mut hooks = Hooks::default();
        hooks.before_build = Some(vec!["nonexistent".to_string()]);

        let mut lifecycle = BTreeMap::new();
        lifecycle.insert(
            "build".to_string(),
            PhaseBuilder::new("build")
                .command("cargo build")
                .build()
                .unwrap()
                .as_ref()
                .clone(),
        );

        let make = Make {
            project: Project {
                name: "test".to_string(),
                project_type: None,
                version: None,
                description: None,
            },
            workspace: None,
            lifecycle,
            hooks: Some(hooks),
        };

        let result = validate_hooks(&make);
        assert!(result.is_err());
    });

    /// Test state validation with inconsistent history
    test!(test_state_validation_inconsistent_history, {
        let mut state = LifecycleState::default();
        // Run deploy before build (invalid)
        state.record_run("deploy".to_string(), 0, 100, true);
        state.record_run("build".to_string(), 100, 200, true);

        let result = ValidatedLifecycleState::new(state);
        assert!(result.is_err());
    });

    /// Test state validation with valid history
    test!(test_state_validation_valid_history, {
        let mut state = LifecycleState::default();
        state.record_run("init".to_string(), 0, 100, true);
        state.record_run("setup".to_string(), 100, 200, true);
        state.record_run("build".to_string(), 200, 300, true);
        state.record_run("test".to_string(), 300, 400, true);

        let validated = ValidatedLifecycleState::new(state).unwrap();
        assert!(validated.state().has_completed_phase("init"));
        assert!(validated.state().has_completed_phase("test"));
    });
}
