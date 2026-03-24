//! Tests demonstrating that invalid operations are impossible.
//!
//! These tests document operations that CANNOT compile, proving
//! the type system prevents errors at compile time.
//!
//! Note: To run true compile-fail tests, add `trybuild` dependency
//! and create separate `.rs` files in `tests/compile_fail/`.

// ============================================================================
// IMPOSSIBLE OPERATIONS - These would fail to compile
// ============================================================================

// IMPOSSIBLE: Cannot call start() on uninitialized state machine
// ```
// let machine = StateMachine::<Uninitialized>::new();
// let running = machine.start(); // ERROR: method not found
// ```

// IMPOSSIBLE: Cannot call stop() on initialized state machine
// ```
// let machine = StateMachine::<Uninitialized>::new();
// let machine = machine.initialize("config");
// let stopped = machine.stop(); // ERROR: method not found
// ```

// IMPOSSIBLE: Cannot call restart() on stopped state machine
// ```
// let machine = StateMachine::<Uninitialized>::new();
// let machine = machine.initialize("config");
// let machine = machine.start();
// let machine = machine.stop();
// let restarted = machine.restart(); // ERROR: method not found
// ```

// IMPOSSIBLE: Cannot build Config without all required fields
// ```
// let config = ConfigBuilder::new()
//     .with_name("app")
//     .build(); // ERROR: method not found (missing version and port)
// ```

// IMPOSSIBLE: Cannot build Config with only name and version
// ```
// let config = ConfigBuilder::new()
//     .with_name("app")
//     .with_version("1.0")
//     .build(); // ERROR: method not found (missing port)
// ```

// IMPOSSIBLE: Cannot build DbConnection without host
// ```
// let conn = DbConnectionBuilder::new()
//     .with_database("db")
//     .with_user("user")
//     .build(); // ERROR: method not found (missing host)
// ```

// IMPOSSIBLE: Cannot create empty NonEmpty vector directly
// ```
// let vec = NonEmpty::<i32> { head: ???, tail: ??? }; // ERROR: private fields
// ```

// IMPOSSIBLE: Cannot set the same builder field twice (type changes)
// ```
// let builder = ConfigBuilder::new().with_name("first");
// let builder = builder.with_name("second"); // ERROR: method not found
// ```

// ============================================================================
// POSSIBLE OPERATIONS - These demonstrate the API works correctly
// ============================================================================

#[test]
fn test_correct_state_machine_usage() {
    use ggen_poka_yoke::type_guards::{StateMachine, Uninitialized};

    // This is the ONLY valid sequence
    let machine = StateMachine::<Uninitialized>::new();
    let machine = machine.initialize("config");
    let machine = machine.start();
    let _machine = machine.stop();
}

#[test]
fn test_correct_config_builder_usage() {
    use ggen_poka_yoke::builders::ConfigBuilder;

    // This is the ONLY way to build a Config
    let _config = ConfigBuilder::new()
        .with_name("app")
        .with_version("1.0.0")
        .with_port(8080)
        .build();
}

#[test]
fn test_correct_db_connection_builder_usage() {
    use ggen_poka_yoke::builders::DbConnectionBuilder;

    // This is the ONLY way to build a DbConnection
    let _conn = DbConnectionBuilder::new()
        .with_host("localhost")
        .with_database("db")
        .with_user("user")
        .build();
}

#[test]
fn test_non_empty_construction() {
    use ggen_poka_yoke::type_guards::NonEmpty;

    // Can only construct NonEmpty with at least one element
    let _vec = NonEmpty::new(42);
    let _vec = NonEmpty::with_tail(1, vec![2, 3]);

    // Cannot create empty NonEmpty - type system prevents it
}

#[test]
fn test_positive_construction() {
    use ggen_poka_yoke::type_guards::Positive;

    // Can only construct Positive with non-zero values
    let _pos = Positive::<u32>::new(1);

    // Zero returns None
    assert!(Positive::<u32>::new(0).is_none());
}

#[test]
fn test_bounded_construction() {
    use ggen_poka_yoke::type_guards::Bounded;

    type Percentage = Bounded<0, 100>;

    // Can only construct Bounded within range
    let _pct = Percentage::new(50);

    // Out of range returns None
    assert!(Percentage::new(-1).is_none());
    assert!(Percentage::new(101).is_none());
}

// ============================================================================
// DOCUMENTATION: What makes these guarantees possible
// ============================================================================

/// The type system enforces the following guarantees:
///
/// 1. **State Machine Transitions**
///    - Each state is a distinct type (phantom data marker)
///    - Methods are only available for specific states
///    - Invalid transitions cannot be expressed
///
/// 2. **Builder Patterns**
///    - Each builder stage is a distinct type
///    - `build()` is only available when all required fields are set
///    - Missing fields prevent compilation
///
/// 3. **Non-Empty Collections**
///    - Constructor requires at least one element
///    - Private fields prevent direct construction
///    - `is_empty()` always returns `false` by construction
///
/// 4. **Bounded Values**
///    - Construction validates bounds
///    - Constructor returns `Option<Self>`
///    - Invalid values produce `None`, not panic
///
/// 5. **Positive Numbers**
///    - Construction validates > 0
///    - Zero and negative values produce `None`
///    - Type guarantees valid range
///
/// This is **poka-yoke** (error proofing): making mistakes impossible
/// through type-level design rather than runtime checks.
#[test]
fn test_type_system_guarantees() {
    use ggen_poka_yoke::type_guards::{NonEmpty, Positive, StateMachine, Uninitialized};

    // All these operations are SAFE by construction
    let machine = StateMachine::<Uninitialized>::new()
        .initialize("config")
        .start();

    assert!(machine.is_running()); // Always true for Running state

    let vec = NonEmpty::new(42);
    assert!(!vec.is_empty()); // Always false for NonEmpty

    let pos = Positive::<u32>::new(10).unwrap();
    assert!(pos.get() > 0); // Always true for Positive
}
