//! Chicago TDD Test Examples
//!
//! Demonstrates Chicago-style testing with AAA pattern (Arrange-Act-Assert).
//! Key principles:
//! - State-based verification
//! - Real collaborators (no mocks)
//! - Observable outcomes
//! - Test real behavior

use ggen_testing::{
    assertions::*,
    fixtures::*,
    property::*,
    snapshot::*,
    TestHarness, TestResult, StateVerifier,
};
use std::collections::BTreeMap;

// ============================================================================
// Example 1: File System Operations (Real Collaborator)
// ============================================================================

#[test]
fn test_file_creation_changes_state() -> TestResult<()> {
    // Arrange: Set up real file system fixture
    let fixture = TempFsFixture::new()?;
    let mut state = StateVerifier::new(fixture.exists("test.txt"));

    // Act: Perform real file operation
    fixture.create_file("test.txt", "content")?;

    // Assert: Verify observable state changed
    state.capture(fixture.exists("test.txt"));
    assert!(state.changed(), "File creation should change existence state");
    assert_eq!(fixture.read_file("test.txt")?, "content");

    Ok(())
}

#[test]
fn test_directory_creation_is_observable() -> TestResult<()> {
    // Arrange
    let fixture = TempFsFixture::new()?;
    let file_path = "subdir/nested/file.txt";

    // Act
    fixture.create_file(file_path, "nested content")?;

    // Assert: Verify directory structure exists
    assert!(fixture.exists("subdir"));
    assert!(fixture.exists("subdir/nested"));
    assert!(fixture.exists(file_path));
    assert_eq!(fixture.read_file(file_path)?, "nested content");

    Ok(())
}

// ============================================================================
// Example 2: In-Memory Store (Real State Verification)
// ============================================================================

#[test]
fn test_store_insert_changes_state() {
    // Arrange: Create real store with initial state
    let mut store = InMemoryStoreFixture::new();
    let mut state = StateVerifier::new(store.len());

    // Act: Perform real operation
    store.insert("key1".to_string(), "value1".to_string());

    // Assert: Verify state changed
    state.capture(store.len());
    assert!(state.changed(), "Insert should change store size");
    assert_eq!(store.len(), 1);
    assert_eq!(store.get("key1"), Some(&"value1".to_string()));
}

#[test]
fn test_store_operations_maintain_consistency() {
    // Arrange
    let mut store = InMemoryStoreFixture::new();

    // Act: Multiple operations
    store.insert("a".to_string(), "1".to_string());
    store.insert("b".to_string(), "2".to_string());
    store.insert("c".to_string(), "3".to_string());

    // Assert: Verify final state
    assert_length(&store.keys(), 3);
    assert_contains(&store.keys(), &&"a".to_string());
    assert_contains(&store.keys(), &&"b".to_string());
    assert_contains(&store.keys(), &&"c".to_string());
}

#[test]
fn test_store_remove_changes_state() {
    // Arrange
    let mut store = InMemoryStoreFixture::new();
    store.insert("key".to_string(), "value".to_string());
    let before_size = store.len();

    // Act
    let removed = store.remove("key");

    // Assert
    assert_eq!(removed, Some("value".to_string()));
    assert!(store.len() < before_size, "Remove should decrease size");
    assert_none(&store.get("key"));
}

// ============================================================================
// Example 3: Event Log (Observable Side Effects)
// ============================================================================

#[test]
fn test_event_log_captures_operations() {
    // Arrange
    let mut log = EventLogFixture::new();
    let initial_count = log.count();

    // Act: Perform operations that should be logged
    log.log("operation_start".to_string(), BTreeMap::new());
    log.log("data_processed".to_string(), {
        let mut data = BTreeMap::new();
        data.insert("count".to_string(), "42".to_string());
        data
    });
    log.log("operation_complete".to_string(), BTreeMap::new());

    // Assert: Verify events are observable
    assert_greater_than(&log.count(), &initial_count);
    assert_eq!(log.count(), 3);
    assert!(log.contains("operation_start"));
    assert!(log.contains("data_processed"));
    assert!(log.contains("operation_complete"));
}

#[test]
fn test_event_log_preserves_order() {
    // Arrange
    let mut log = EventLogFixture::new();

    // Act
    log.log("first".to_string(), BTreeMap::new());
    log.log("second".to_string(), BTreeMap::new());
    log.log("third".to_string(), BTreeMap::new());

    // Assert: Verify order is preserved
    let events = log.events();
    assert_length(events, 3);
    assert_eq!(events[0].name, "first");
    assert_eq!(events[1].name, "second");
    assert_eq!(events[2].name, "third");
}

// ============================================================================
// Example 4: Test Harness Integration
// ============================================================================

#[tokio::test]
async fn test_harness_captures_execution_metadata() -> TestResult<()> {
    // Arrange
    let harness = TestHarness::new("metadata_test");

    // Act
    let result = harness
        .execute(|| async {
            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            Ok::<_, anyhow::Error>(42)
        })
        .await?;

    // Assert
    assert_eq!(result, 42);
    let metadata = harness.metadata();
    assert_not_empty(&metadata);
    assert_eq!(metadata[0].test_name, "metadata_test");
    assert!(metadata[0].success);
    assert!(metadata[0].duration.as_millis() >= 10);

    Ok(())
}

#[test]
fn test_harness_sync_execution() -> TestResult<()> {
    // Arrange
    let harness = TestHarness::new("sync_test");

    // Act
    let result = harness.execute_sync(|| Ok::<_, anyhow::Error>("success"))?;

    // Assert
    assert_eq!(result, "success");
    assert_not_empty(&harness.metadata());

    Ok(())
}

// ============================================================================
// Example 5: State Verifier with Complex State
// ============================================================================

#[test]
fn test_state_verifier_with_collections() {
    // Arrange
    let initial = vec![1, 2, 3];
    let mut verifier = StateVerifier::new(initial.clone());

    // Act
    let mut modified = initial;
    modified.push(4);
    verifier.capture(modified);

    // Assert
    assert!(verifier.changed());
    if let Some((before, after)) = verifier.diff() {
        assert_length(&before, 3);
        assert_length(&after, 4);
        assert_contains(&after, &4);
    }
}

// ============================================================================
// Example 6: Property-Based Testing with State
// ============================================================================

proptest! {
    fn test_store_size_property(
        keys in prop::collection::vec(StrategyBuilder::identifier(), 1..10)
    ) {
        // Arrange
        let mut store = InMemoryStoreFixture::new();

        // Act: Insert all keys
        for key in &keys {
            store.insert(key.clone(), "value".to_string());
        }

        // Assert: Store size matches number of unique keys
        let unique_keys: std::collections::HashSet<_> = keys.iter().collect();
        prop_assert_eq!(store.len(), unique_keys.len());
    }

    fn test_file_content_roundtrip(
        content in StrategyBuilder::non_empty_string()
    ) {
        // Arrange
        let fixture = TempFsFixture::new().unwrap();

        // Act
        fixture.create_file("test.txt", &content).unwrap();
        let read_content = fixture.read_file("test.txt").unwrap();

        // Assert: Content survives roundtrip
        prop_assert_eq!(content, read_content);
    }
}

// ============================================================================
// Example 7: Snapshot Testing for State Verification
// ============================================================================

#[test]
fn test_snapshot_captures_state() {
    // Arrange
    let mut store = InMemoryStoreFixture::new();
    store.insert("key1".to_string(), "value1".to_string());
    store.insert("key2".to_string(), "value2".to_string());

    // Act & Assert: Snapshot the observable state
    let snapshot = SnapshotTest::new("store_state");
    snapshot.assert_state(&store);
}

#[test]
fn test_snapshot_manager_tracks_multiple_states() {
    // Arrange
    let mut manager = SnapshotManager::new("state_progression");
    let mut store = InMemoryStoreFixture::new();

    // Act & Assert: Capture state at each step
    manager.snapshot(&store); // Empty state

    store.insert("first".to_string(), "1".to_string());
    manager.snapshot(&store); // After first insert

    store.insert("second".to_string(), "2".to_string());
    manager.snapshot(&store); // After second insert

    assert_eq!(manager.counter(), 3);
}

// ============================================================================
// Example 8: Complex Workflow with Real Collaborators
// ============================================================================

#[test]
fn test_complex_workflow_with_multiple_collaborators() -> TestResult<()> {
    // Arrange: Set up real collaborators
    let mut store = InMemoryStoreFixture::new();
    let mut log = EventLogFixture::new();
    let fixture = TempFsFixture::new()?;

    // Act: Execute workflow with real operations
    log.log("workflow_start".to_string(), BTreeMap::new());

    fixture.create_file("data.txt", "initial")?;
    store.insert("file_path".to_string(), "data.txt".to_string());
    log.log("file_created".to_string(), BTreeMap::new());

    let content = fixture.read_file("data.txt")?;
    store.insert("content".to_string(), content);
    log.log("content_stored".to_string(), BTreeMap::new());

    log.log("workflow_complete".to_string(), BTreeMap::new());

    // Assert: Verify observable state across all collaborators
    assert_eq!(fixture.read_file("data.txt")?, "initial");
    assert_eq!(store.get("file_path"), Some(&"data.txt".to_string()));
    assert_eq!(store.get("content"), Some(&"initial".to_string()));
    assert_eq!(log.count_by_name("workflow_start"), 1);
    assert_eq!(log.count_by_name("file_created"), 1);
    assert_eq!(log.count_by_name("content_stored"), 1);
    assert_eq!(log.count_by_name("workflow_complete"), 1);

    Ok(())
}

// ============================================================================
// Example 9: Error State Verification
// ============================================================================

#[test]
fn test_error_states_are_observable() -> TestResult<()> {
    // Arrange
    let fixture = TempFsFixture::new()?;
    fixture.create_file("readonly.txt", "content")?;

    // Act: Attempt to read non-existent file
    let result = fixture.read_file("nonexistent.txt");

    // Assert: Error state is observable
    assert_err(&result);

    Ok(())
}

// ============================================================================
// Example 10: State Transitions with Assertions
// ============================================================================

#[test]
fn test_state_transitions() {
    // Arrange: Initial state
    let mut store = InMemoryStoreFixture::new();
    assert!(store.is_empty());

    // Act & Assert: Transition 1 - Add items
    store.insert("a".to_string(), "1".to_string());
    store.insert("b".to_string(), "2".to_string());
    assert!(!store.is_empty());
    assert_eq!(store.len(), 2);

    // Act & Assert: Transition 2 - Remove item
    store.remove("a");
    assert_eq!(store.len(), 1);
    assert_none(&store.get("a"));
    assert_some(&store.get("b"));

    // Act & Assert: Transition 3 - Clear all
    store.clear();
    assert!(store.is_empty());
    assert_eq!(store.len(), 0);
}
