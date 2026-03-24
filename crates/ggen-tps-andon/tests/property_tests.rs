//! Property-based tests for TPS Andon system
//!
//! Tests core properties:
//! - Idempotency: signal queries don't mutate state
//! - Determinism: same inputs produce same signals
//! - Monotonicity: signal severity ordering preserved
//! - Resource safety: no leaks in signal lifecycle

use ggen_tps_andon::{AndonSignal, SignalColor};
use proptest::prelude::*;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid signal messages
fn signal_message_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ]{1,200}".prop_map(|s| s.trim().to_string())
}

/// Generate valid component names
fn component_strategy() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9-]{0,30}".prop_map(|s| s.to_string())
}

/// Generate valid signal colors
fn signal_color_strategy() -> impl Strategy<Value = SignalColor> {
    prop_oneof![
        Just(SignalColor::Red),
        Just(SignalColor::Yellow),
        Just(SignalColor::Green),
    ]
}

/// Generate valid trace IDs
fn trace_id_strategy() -> impl Strategy<Value = String> {
    "trace-[a-f0-9]{32}".prop_map(|s| s.to_string())
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Signal color properties are deterministic
    #[test]
    fn prop_signal_color_properties_deterministic(color in signal_color_strategy()) {
        // Act
        let name1 = color.name();
        let name2 = color.name();
        let code1 = color.code();
        let code2 = color.code();
        let requires_action1 = color.requires_action();
        let requires_action2 = color.requires_action();
        let is_critical1 = color.is_critical();
        let is_critical2 = color.is_critical();

        // Assert - Properties are deterministic
        prop_assert_eq!(name1, name2);
        prop_assert_eq!(code1, code2);
        prop_assert_eq!(requires_action1, requires_action2);
        prop_assert_eq!(is_critical1, is_critical2);
    }

    /// Property: Signal creation is deterministic in structure
    #[test]
    fn prop_signal_creation_deterministic(
        color in signal_color_strategy(),
        message in signal_message_strategy(),
    ) {
        // Act
        let signal1 = AndonSignal::with_color(color, message.clone());
        let signal2 = AndonSignal::with_color(color, message.clone());

        // Assert - Structure is deterministic (excluding UUID and timestamp)
        prop_assert_eq!(signal1.color, signal2.color);
        prop_assert_eq!(signal1.message, signal2.message);
        prop_assert_eq!(signal1.occurrence_count, signal2.occurrence_count);
        prop_assert_eq!(signal1.occurrence_count, 1);
    }
}

// ============================================================================
// PROPERTY: Idempotency - f(f(x)) = f(x)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Signal color queries are idempotent
    #[test]
    fn prop_signal_color_queries_idempotent(color in signal_color_strategy()) {
        // Act - Multiple queries
        let name1 = color.name();
        let name2 = color.name();
        let name3 = color.name();

        let requires_action1 = color.requires_action();
        let requires_action2 = color.requires_action();

        // Assert - Queries don't mutate
        prop_assert_eq!(name1, name2);
        prop_assert_eq!(name2, name3);
        prop_assert_eq!(requires_action1, requires_action2);
    }

    /// Property: Signal JSON serialization is idempotent
    #[test]
    fn prop_signal_json_serialization_idempotent(
        message in signal_message_strategy(),
        component in component_strategy(),
    ) {
        // Arrange
        let signal = AndonSignal::red(message)
            .with_component(component);

        // Act - Multiple serializations
        let json1 = signal.as_json();
        let json2 = signal.as_json();
        let json3 = signal.as_json();

        // Assert - Same result each time
        prop_assert_eq!(json1, json2);
        prop_assert_eq!(json2, json3);
    }
}

// ============================================================================
// PROPERTY: Monotonicity - Signal severity ordering preserved
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Critical signals always require action
    #[test]
    fn prop_critical_signals_require_action(message in signal_message_strategy()) {
        // Arrange & Act
        let signal = AndonSignal::red(message);

        // Assert - Red is critical and requires action
        prop_assert!(signal.color.is_critical());
        prop_assert!(signal.color.requires_action());
        prop_assert_eq!(signal.color, SignalColor::Red);
    }

    /// Property: Green signals never require action
    #[test]
    fn prop_green_signals_no_action(message in signal_message_strategy()) {
        // Arrange & Act
        let signal = AndonSignal::green(message);

        // Assert - Green is not critical and doesn't require action
        prop_assert!(!signal.color.is_critical());
        prop_assert!(!signal.color.requires_action());
        prop_assert_eq!(signal.color, SignalColor::Green);
    }

    /// Property: Yellow signals require action but not critical
    #[test]
    fn prop_yellow_signals_warning(message in signal_message_strategy()) {
        // Arrange & Act
        let signal = AndonSignal::yellow(message);

        // Assert - Yellow requires action but is not critical
        prop_assert!(!signal.color.is_critical());
        prop_assert!(signal.color.requires_action());
        prop_assert_eq!(signal.color, SignalColor::Yellow);
    }

    /// Property: Signal severity is monotonic (Red > Yellow > Green)
    #[test]
    fn prop_signal_severity_monotonic() {
        // Assert - Severity ordering
        prop_assert!(SignalColor::Red.is_critical());
        prop_assert!(!SignalColor::Yellow.is_critical());
        prop_assert!(!SignalColor::Green.is_critical());

        prop_assert!(SignalColor::Red.requires_action());
        prop_assert!(SignalColor::Yellow.requires_action());
        prop_assert!(!SignalColor::Green.requires_action());
    }
}

// ============================================================================
// PROPERTY: Resource Safety - No leaks in lifecycle
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Signal builder methods properly own data
    #[test]
    fn prop_signal_builder_ownership_safe(
        message in signal_message_strategy(),
        component in component_strategy(),
        trace_id in trace_id_strategy(),
    ) {
        // Arrange & Act
        let signal = AndonSignal::red(message.clone())
            .with_component(component.clone())
            .with_trace_id(trace_id.clone());

        // Assert - Data is owned
        prop_assert_eq!(signal.message, message);
        prop_assert_eq!(signal.component, component);
        prop_assert_eq!(signal.trace_id, Some(trace_id));

        // Drop signal - automatic cleanup
        drop(signal);
    }

    /// Property: Signal with details properly owns JSON value
    #[test]
    fn prop_signal_details_ownership_safe(
        message in signal_message_strategy(),
    ) {
        // Arrange
        let details = serde_json::json!({
            "key1": "value1",
            "key2": 42,
            "nested": {
                "field": "value"
            }
        });

        // Act
        let signal = AndonSignal::yellow(message)
            .with_details(details.clone());

        // Assert - Details are owned
        prop_assert_eq!(signal.details, details);

        // Drop signal - automatic cleanup
        drop(signal);
    }
}

// ============================================================================
// PROPERTY: Commutativity - Builder order independence where applicable
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Signal builder methods are commutative
    #[test]
    fn prop_signal_builder_commutative(
        message in signal_message_strategy(),
        component in component_strategy(),
        trace_id in trace_id_strategy(),
    ) {
        // Arrange & Act - Order 1: component then trace_id
        let signal1 = AndonSignal::red(message.clone())
            .with_component(component.clone())
            .with_trace_id(trace_id.clone());

        // Act - Order 2: trace_id then component
        let signal2 = AndonSignal::red(message)
            .with_trace_id(trace_id.clone())
            .with_component(component.clone());

        // Assert - Results are equivalent (ignoring ID and timestamp)
        prop_assert_eq!(signal1.color, signal2.color);
        prop_assert_eq!(signal1.message, signal2.message);
        prop_assert_eq!(signal1.component, signal2.component);
        prop_assert_eq!(signal1.trace_id, signal2.trace_id);
    }
}

// ============================================================================
// INVARIANT TESTS: Signal invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Invariant: Occurrence count starts at 1
    #[test]
    fn invariant_occurrence_count_starts_at_one(
        color in signal_color_strategy(),
        message in signal_message_strategy(),
    ) {
        let signal = AndonSignal::with_color(color, message);
        prop_assert_eq!(signal.occurrence_count, 1);
    }

    /// Invariant: Occurrence count is monotonically increasing
    #[test]
    fn invariant_occurrence_count_monotonic(message in signal_message_strategy()) {
        let mut signal = AndonSignal::red(message);
        let initial = signal.occurrence_count;

        // Act - Increment multiple times
        signal.increment_occurrence();
        let after_first = signal.occurrence_count;

        signal.increment_occurrence();
        let after_second = signal.occurrence_count;

        signal.increment_occurrence();
        let after_third = signal.occurrence_count;

        // Assert - Monotonically increasing
        prop_assert!(after_first > initial);
        prop_assert!(after_second > after_first);
        prop_assert!(after_third > after_second);
        prop_assert_eq!(initial, 1);
        prop_assert_eq!(after_first, 2);
        prop_assert_eq!(after_second, 3);
        prop_assert_eq!(after_third, 4);
    }

    /// Invariant: Signal ID is always populated
    #[test]
    fn invariant_signal_id_populated(
        color in signal_color_strategy(),
        message in signal_message_strategy(),
    ) {
        let signal = AndonSignal::with_color(color, message);
        prop_assert!(!signal.id.is_empty());
    }

    /// Invariant: Signal timestamp is set
    #[test]
    fn invariant_signal_timestamp_set(
        color in signal_color_strategy(),
        message in signal_message_strategy(),
    ) {
        let signal = AndonSignal::with_color(color, message);
        // Timestamp should be within reasonable bounds (last hour to future second)
        let now = chrono::Utc::now();
        let diff = now.signed_duration_since(signal.timestamp);
        prop_assert!(diff.num_seconds().abs() < 3600); // Within 1 hour
    }
}
