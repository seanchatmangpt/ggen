//! Property-based tests for A2A protocol
//!
//! Tests core properties:
//! - Idempotency: state transitions are idempotent where applicable
//! - Determinism: same inputs produce same outputs
//! - Monotonicity: task state transitions preserve ordering
//! - Resource safety: no leaks in task lifecycle

use ggen_a2a::{Task, TaskState, TaskStateMachine, StateTransition, Artifact, ArtifactType, ArtifactContent};
use proptest::prelude::*;
use std::collections::HashMap;

// ============================================================================
// STRATEGIES: Input generation for property tests
// ============================================================================

/// Generate valid task titles
fn task_title_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ]{1,100}".prop_map(|s| s.trim().to_string())
}

/// Generate valid agent IDs
fn agent_id_strategy() -> impl Strategy<Value = String> {
    "agent-[a-z0-9]{1,20}".prop_map(|s| s.to_string())
}

/// Generate valid task states
fn task_state_strategy() -> impl Strategy<Value = TaskState> {
    prop_oneof![
        Just(TaskState::Created),
        Just(TaskState::Running),
        Just(TaskState::Blocked),
        Just(TaskState::Completed),
        Just(TaskState::Failed),
    ]
}

/// Generate valid non-terminal task states
fn active_state_strategy() -> impl Strategy<Value = TaskState> {
    prop_oneof![
        Just(TaskState::Created),
        Just(TaskState::Running),
        Just(TaskState::Blocked),
    ]
}

/// Generate valid artifact types
fn artifact_type_strategy() -> impl Strategy<Value = ArtifactType> {
    prop_oneof![
        Just(ArtifactType::Code),
        Just(ArtifactType::Documentation),
        Just(ArtifactType::Configuration),
        Just(ArtifactType::Data),
        Just(ArtifactType::Binary),
    ]
}

/// Generate valid artifact content
fn artifact_content_strategy() -> impl Strategy<Value = ArtifactContent> {
    prop_oneof![
        "[a-zA-Z0-9 ]{1,1000}".prop_map(|s| ArtifactContent::Text(s)),
        proptest::collection::vec(any::<u8>(), 0..1000).prop_map(ArtifactContent::Binary),
    ]
}

// ============================================================================
// PROPERTY: Determinism - Same input produces same output
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Task creation is deterministic (given same timestamp)
    /// Same title and agent produce same structure
    #[test]
    fn prop_task_creation_deterministic(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
    ) {
        // Arrange & Act
        let task1 = Task::new(title.clone(), agent.clone());
        let task2 = Task::new(title.clone(), agent.clone());

        // Assert - Structure is deterministic (excluding UUID and timestamps)
        prop_assert_eq!(task1.title, task2.title);
        prop_assert_eq!(task1.created_by, task2.created_by);
        prop_assert_eq!(task1.state, task2.state);
        prop_assert_eq!(task1.state, TaskState::Created);
    }

    /// Property: State transition validation is deterministic
    #[test]
    fn prop_state_transition_validation_deterministic(
        from in task_state_strategy(),
        to in task_state_strategy(),
    ) {
        // Arrange & Act
        let result1 = TaskStateMachine::is_valid_transition(from, to);
        let result2 = TaskStateMachine::is_valid_transition(from, to);

        // Assert - Same inputs produce same validation result
        prop_assert_eq!(result1, result2);
    }
}

// ============================================================================
// PROPERTY: Idempotency - f(f(x)) = f(x)
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Terminal state check is idempotent
    #[test]
    fn prop_terminal_state_check_idempotent(state in task_state_strategy()) {
        // Arrange
        let is_terminal_1 = state.is_terminal();
        let is_terminal_2 = state.is_terminal();

        // Assert - Multiple checks produce same result
        prop_assert_eq!(is_terminal_1, is_terminal_2);
    }

    /// Property: Task state queries are idempotent
    #[test]
    fn prop_task_state_queries_idempotent(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
    ) {
        // Arrange
        let task = Task::new(title, agent);

        // Act
        let is_active_1 = task.is_active();
        let is_active_2 = task.is_active();
        let is_terminal_1 = task.is_terminal();
        let is_terminal_2 = task.is_terminal();

        // Assert - Queries don't mutate state
        prop_assert_eq!(is_active_1, is_active_2);
        prop_assert_eq!(is_terminal_1, is_terminal_2);
    }
}

// ============================================================================
// PROPERTY: Monotonicity - State transitions preserve ordering
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Once terminal, always terminal (monotonic progression)
    #[test]
    fn prop_terminal_state_monotonic(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
    ) {
        // Arrange
        let mut task = Task::new(title, agent.clone())
            .with_assignment(agent.clone());

        // Act - Transition to terminal state
        let transition1 = StateTransition::new(TaskState::Running, agent.clone());
        TaskStateMachine::transition(&mut task, transition1)?;

        let transition2 = StateTransition::new(TaskState::Completed, agent.clone());
        TaskStateMachine::transition(&mut task, transition2)?;

        // Assert - Cannot transition from terminal state
        prop_assert!(task.is_terminal());

        let transition3 = StateTransition::new(TaskState::Running, agent);
        let result = TaskStateMachine::transition(&mut task, transition3);
        prop_assert!(result.is_err());
    }

    /// Property: Valid transitions maintain state ordering invariants
    #[test]
    fn prop_valid_transitions_preserve_invariants(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
    ) {
        // Arrange
        let mut task = Task::new(title, agent.clone())
            .with_assignment(agent.clone());

        // Act - Created → Running
        let transition = StateTransition::new(TaskState::Running, agent.clone());
        TaskStateMachine::transition(&mut task, transition)?;

        // Assert - Task is in Running state
        prop_assert_eq!(task.state, TaskState::Running);
        prop_assert!(task.is_active());
        prop_assert!(!task.is_terminal());

        // Act - Running → Completed
        let transition = StateTransition::new(TaskState::Completed, agent);
        TaskStateMachine::transition(&mut task, transition)?;

        // Assert - Task is in Completed state and terminal
        prop_assert_eq!(task.state, TaskState::Completed);
        prop_assert!(!task.is_active());
        prop_assert!(task.is_terminal());
        prop_assert!(task.completed_at.is_some());
    }
}

// ============================================================================
// PROPERTY: Resource Safety - No leaks in lifecycle
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Task artifacts are properly owned (no leaks)
    #[test]
    fn prop_artifact_ownership_safe(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
        artifact_name in "[a-z]{1,20}",
        artifact_type in artifact_type_strategy(),
        content in artifact_content_strategy(),
    ) {
        // Arrange
        let artifact = Artifact::new(artifact_name.clone(), artifact_type, content);

        // Act
        let task = Task::new(title, agent)
            .with_artifact(artifact_name.clone(), artifact);

        // Assert - Artifact is owned by task
        prop_assert_eq!(task.artifacts.len(), 1);
        prop_assert!(task.artifacts.contains_key(&artifact_name));

        // Act - Task goes out of scope (automatic cleanup)
        drop(task);
        // No memory leaks - Rust ownership ensures cleanup
    }

    /// Property: Task metadata is properly owned
    #[test]
    fn prop_metadata_ownership_safe(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
        key in "[a-z]{1,20}",
        value in "[a-zA-Z0-9 ]{1,100}",
    ) {
        // Arrange & Act
        let task = Task::new(title, agent)
            .with_metadata(key.clone(), value.clone());

        // Assert - Metadata is owned
        prop_assert_eq!(task.metadata.len(), 1);
        prop_assert_eq!(task.metadata.get(&key), Some(&value));

        // Drop task - automatic cleanup
        drop(task);
    }
}

// ============================================================================
// PROPERTY: Commutativity - Order independence where applicable
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Property: Adding metadata is commutative (order doesn't matter)
    #[test]
    fn prop_metadata_addition_commutative(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
        key1 in "[a-z]{1,10}1",
        value1 in "[a-zA-Z0-9 ]{1,50}",
        key2 in "[a-z]{1,10}2",
        value2 in "[a-zA-Z0-9 ]{1,50}",
    ) {
        prop_assume!(key1 != key2);

        // Arrange & Act - Order 1: key1 then key2
        let task1 = Task::new(title.clone(), agent.clone())
            .with_metadata(key1.clone(), value1.clone())
            .with_metadata(key2.clone(), value2.clone());

        // Act - Order 2: key2 then key1
        let task2 = Task::new(title, agent)
            .with_metadata(key2.clone(), value2.clone())
            .with_metadata(key1.clone(), value1.clone());

        // Assert - Results are equivalent
        prop_assert_eq!(task1.metadata.get(&key1), task2.metadata.get(&key1));
        prop_assert_eq!(task1.metadata.get(&key2), task2.metadata.get(&key2));
        prop_assert_eq!(task1.metadata.len(), task2.metadata.len());
    }

    /// Property: Possible transitions are deterministic and complete
    #[test]
    fn prop_possible_transitions_deterministic(state in active_state_strategy()) {
        // Act
        let transitions1 = TaskStateMachine::possible_transitions(state);
        let transitions2 = TaskStateMachine::possible_transitions(state);

        // Assert - Same state produces same possible transitions
        prop_assert_eq!(transitions1, transitions2);

        // Assert - All returned transitions are valid
        for next_state in &transitions1 {
            prop_assert!(TaskStateMachine::is_valid_transition(state, *next_state));
        }
    }
}

// ============================================================================
// INVARIANT TESTS: State machine invariants
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    /// Invariant: Terminal states have no valid transitions
    #[test]
    fn invariant_terminal_states_no_transitions(state in task_state_strategy()) {
        if state.is_terminal() {
            let transitions = TaskStateMachine::possible_transitions(state);
            prop_assert_eq!(transitions.len(), 0);
        }
    }

    /// Invariant: Active states always have at least one valid transition
    #[test]
    fn invariant_active_states_have_transitions(state in active_state_strategy()) {
        let transitions = TaskStateMachine::possible_transitions(state);
        prop_assert!(transitions.len() > 0);
    }

    /// Invariant: Task duration is non-negative
    #[test]
    fn invariant_task_duration_non_negative(
        title in task_title_strategy(),
        agent in agent_id_strategy(),
    ) {
        // Arrange
        let mut task = Task::new(title, agent.clone())
            .with_assignment(agent.clone());

        // Act - Complete task
        let transition1 = StateTransition::new(TaskState::Running, agent.clone());
        TaskStateMachine::transition(&mut task, transition1)?;

        let transition2 = StateTransition::new(TaskState::Completed, agent);
        TaskStateMachine::transition(&mut task, transition2)?;

        // Assert - Duration is non-negative
        if let Some(duration) = task.duration() {
            prop_assert!(duration.num_milliseconds() >= 0);
        }
    }
}
