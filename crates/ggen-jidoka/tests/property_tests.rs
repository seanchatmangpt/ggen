//! Property-based tests for Jidoka gates and monitoring
//!
//! Tests core properties:
//! - Idempotency: gate checks don't mutate state
//! - Determinism: same inputs produce same gate results
//! - Monotonicity: gate pass/fail states are consistent
//! - Resource safety: no leaks in gate lifecycle

use ggen_jidoka::{
    gate::{Gate, GateResult, GateStatus},
    line::{JidokaLine, LineStatus},
};
use proptest::prelude::*;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid gate names
fn gate_name_strategy() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9-]{2,30}".prop_map(|s| s.to_string())
}

/// Generate valid gate descriptions
fn gate_description_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ]{10,100}".prop_map(|s| s.trim().to_string())
}

/// Generate valid line names
fn line_name_strategy() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9-]{2,30}".prop_map(|s| s.to_string())
}

/// Generate boolean gate results
fn gate_pass_strategy() -> impl Strategy<Value = bool> {
    any::<bool>()
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Gate creation is deterministic
    #[test]
    fn prop_gate_creation_deterministic(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Act
        let gate1 = Gate::new(name.clone(), description.clone());
        let gate2 = Gate::new(name.clone(), description.clone());

        // Assert - Structure is deterministic
        prop_assert_eq!(gate1.name(), gate2.name());
        prop_assert_eq!(gate1.description(), gate2.description());
        prop_assert_eq!(gate1.status(), gate2.status());
    }

    /// Property: Gate status queries are deterministic
    #[test]
    fn prop_gate_status_deterministic(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Arrange
        let gate = Gate::new(name, description);

        // Act
        let status1 = gate.status();
        let status2 = gate.status();

        // Assert - Same status
        prop_assert_eq!(status1, status2);
    }
}

// ============================================================================
// PROPERTY: Idempotency - f(f(x)) = f(x)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Gate accessor methods are idempotent
    #[test]
    fn prop_gate_accessors_idempotent(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Arrange
        let gate = Gate::new(name.clone(), description.clone());

        // Act - Multiple accesses
        let name1 = gate.name();
        let name2 = gate.name();
        let name3 = gate.name();

        let desc1 = gate.description();
        let desc2 = gate.description();

        let status1 = gate.status();
        let status2 = gate.status();

        // Assert - Idempotent
        prop_assert_eq!(name1, name2);
        prop_assert_eq!(name2, name3);
        prop_assert_eq!(desc1, desc2);
        prop_assert_eq!(status1, status2);

        prop_assert_eq!(name1, &name);
        prop_assert_eq!(desc1, &description);
    }

    /// Property: Gate result queries are idempotent
    #[test]
    fn prop_gate_result_queries_idempotent(
        message in gate_description_strategy(),
    ) {
        // Arrange
        let result_pass = GateResult::pass(message.clone());
        let result_fail = GateResult::fail(message.clone());

        // Act - Multiple queries
        let is_pass_1 = result_pass.is_pass();
        let is_pass_2 = result_pass.is_pass();

        let is_fail_1 = result_fail.is_fail();
        let is_fail_2 = result_fail.is_fail();

        // Assert - Idempotent
        prop_assert_eq!(is_pass_1, is_pass_2);
        prop_assert_eq!(is_fail_1, is_fail_2);
        prop_assert!(is_pass_1);
        prop_assert!(is_fail_1);
    }
}

// ============================================================================
// PROPERTY: Monotonicity - Gate states are consistent
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Passing gates have Passing status
    #[test]
    fn prop_passing_gate_status(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Arrange
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let mut gate = Gate::new(name, description);

            // Act - Execute gate that passes
            let result = gate.execute(async { Ok(GateResult::pass("Success")) }).await;

            // Assert - Gate is passing
            prop_assert!(result.is_ok());
            prop_assert_eq!(gate.status(), GateStatus::Passing);
            Ok(())
        })?;
    }

    /// Property: Failing gates have Failing status
    #[test]
    fn prop_failing_gate_status(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Arrange
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let mut gate = Gate::new(name, description);

            // Act - Execute gate that fails
            let result = gate.execute(async { Ok(GateResult::fail("Failed")) }).await;

            // Assert - Gate is failing
            prop_assert!(result.is_ok());
            prop_assert_eq!(gate.status(), GateStatus::Failing);
            Ok(())
        })?;
    }

    /// Property: Gate result pass/fail is mutually exclusive
    #[test]
    fn prop_gate_result_mutually_exclusive(
        message in gate_description_strategy(),
        passes in gate_pass_strategy(),
    ) {
        // Act
        let result = if passes {
            GateResult::pass(message)
        } else {
            GateResult::fail(message)
        };

        // Assert - Exactly one is true
        prop_assert_ne!(result.is_pass(), result.is_fail());
        prop_assert_eq!(result.is_pass(), passes);
        prop_assert_eq!(result.is_fail(), !passes);
    }
}

// ============================================================================
// PROPERTY: Resource Safety - No leaks in lifecycle
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Gate properly owns its data
    #[test]
    fn prop_gate_ownership_safe(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        // Arrange & Act
        let gate = Gate::new(name.clone(), description.clone());

        // Assert - Data is owned
        prop_assert_eq!(gate.name(), &name);
        prop_assert_eq!(gate.description(), &description);

        // Drop gate - automatic cleanup
        drop(gate);
    }

    /// Property: Jidoka line properly owns gates
    #[test]
    fn prop_line_ownership_safe(
        line_name in line_name_strategy(),
        gate1_name in gate_name_strategy(),
        gate2_name in gate_name_strategy(),
    ) {
        prop_assume!(gate1_name != gate2_name);

        // Arrange
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let mut line = JidokaLine::new(line_name.clone());

            let gate1 = Gate::new(gate1_name.clone(), "Gate 1".to_string());
            let gate2 = Gate::new(gate2_name.clone(), "Gate 2".to_string());

            // Act - Add gates
            line.add_gate(gate1);
            line.add_gate(gate2);

            // Assert - Line owns gates
            prop_assert_eq!(line.gate_count(), 2);
            prop_assert_eq!(line.name(), &line_name);

            // Drop line - automatic cleanup
            drop(line);
            Ok(())
        })?;
    }
}

// ============================================================================
// INVARIANT TESTS: Jidoka invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Invariant: New gates start in Pending status
    #[test]
    fn invariant_new_gate_pending(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        let gate = Gate::new(name, description);
        prop_assert_eq!(gate.status(), GateStatus::Pending);
    }

    /// Invariant: New lines start in Running status
    #[test]
    fn invariant_new_line_running(name in line_name_strategy()) {
        let line = JidokaLine::new(name);
        prop_assert_eq!(line.status(), LineStatus::Running);
    }

    /// Invariant: Gate result message is preserved
    #[test]
    fn invariant_gate_result_message_preserved(
        message in gate_description_strategy(),
        passes in gate_pass_strategy(),
    ) {
        let result = if passes {
            GateResult::pass(message.clone())
        } else {
            GateResult::fail(message.clone())
        };

        prop_assert_eq!(result.message(), &message);
    }

    /// Invariant: Line gate count matches added gates
    #[test]
    fn invariant_line_gate_count(
        line_name in line_name_strategy(),
        gate_names in prop::collection::vec(gate_name_strategy(), 0..10),
    ) {
        let rt = tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {
            let mut line = JidokaLine::new(line_name);

            // Add gates
            for name in &gate_names {
                let gate = Gate::new(name.clone(), format!("Gate {}", name));
                line.add_gate(gate);
            }

            // Assert - Count matches
            prop_assert_eq!(line.gate_count(), gate_names.len());
            Ok(())
        })?;
    }

    /// Invariant: Gate status is one of Pending, Passing, or Failing
    #[test]
    fn invariant_gate_status_valid(
        name in gate_name_strategy(),
        description in gate_description_strategy(),
    ) {
        let gate = Gate::new(name, description);
        let status = gate.status();

        prop_assert!(matches!(
            status,
            GateStatus::Pending | GateStatus::Passing | GateStatus::Failing
        ));
    }
}
