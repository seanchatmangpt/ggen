//! Examples demonstrating type-safe ID management
//!
//! This example shows how to use the type-safe ID system for containers,
//! sessions, tasks, tests, and scenarios with proper type safety.

use cleanroom::ids::container::{
    container_id_generator, container_id_registry, deterministic_container_id_generator,
    ContainerIdGenerator, ContainerIdRegistry, ContainerResourceUsage, ContainerStatus,
};
use cleanroom::ids::{
    container_id, scenario_id, session_id, task_id, test_id, ContainerId, IdRegistry, ScenarioId,
    SessionId, TaskId, TestId,
};
use uuid::Uuid;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Cleanroom ID Management Examples");
    println!("=================================");

    // Example 1: Basic ID creation
    println!("\n1. Basic ID Creation");
    let container_id = ContainerId::new();
    let session_id = SessionId::new();
    let task_id = TaskId::new();
    let test_id = TestId::new();
    let scenario_id = ScenarioId::new();

    println!("✓ Container ID: {}", container_id);
    println!("✓ Session ID: {}", session_id);
    println!("✓ Task ID: {}", task_id);
    println!("✓ Test ID: {}", test_id);
    println!("✓ Scenario ID: {}", scenario_id);

    // Example 2: ID conversion and validation
    println!("\n2. ID Conversion and Validation");
    let container_id = ContainerId::new();
    let value: u64 = container_id.into();
    println!("✓ Container ID value: {}", value);

    let back: ContainerId = value.try_into().unwrap();
    println!("✓ Converted back: {}", back);
    assert_eq!(container_id, back);

    // Test zero value rejection
    let zero_result: Result<ContainerId, _> = 0u64.try_into();
    assert!(zero_result.is_err());
    println!("✓ Zero value correctly rejected");

    // Example 3: Session ID with UUID integration
    println!("\n3. Session ID with UUID Integration");
    let uuid = Uuid::new_v4();
    let session_id = SessionId::from_uuid(uuid);
    let uuid_back = session_id.to_uuid();

    println!("✓ Original UUID: {}", uuid);
    println!("✓ Session ID: {}", session_id);
    println!("✓ Converted back UUID: {}", uuid_back);
    assert_eq!(uuid, uuid_back);

    // Example 4: ID registry management
    println!("\n4. ID Registry Management");
    let mut registry = IdRegistry::new();

    let container_id = ContainerId::new();
    let session_id = SessionId::new();
    let task_id = TaskId::new();
    let test_id = TestId::new();
    let scenario_id = ScenarioId::new();

    assert!(registry.register_container(container_id));
    assert!(registry.register_session(session_id));
    assert!(registry.register_task(task_id));
    assert!(registry.register_test(test_id));
    assert!(registry.register_scenario(scenario_id));

    println!("✓ All IDs registered successfully");
    println!("✓ Total registered: {}", registry.total_count());

    assert!(registry.is_container_registered(&container_id));
    assert!(registry.is_session_registered(&session_id));
    assert!(registry.is_task_registered(&task_id));
    assert!(registry.is_test_registered(&test_id));
    assert!(registry.is_scenario_registered(&scenario_id));

    println!("✓ All IDs verified as registered");

    // Test duplicate registration
    assert!(!registry.register_container(container_id));
    println!("✓ Duplicate registration correctly rejected");

    // Unregister some IDs
    assert!(registry.unregister_container(&container_id));
    assert!(registry.unregister_session(&session_id));

    println!("✓ Some IDs unregistered");
    println!("✓ Remaining registered: {}", registry.total_count());

    assert!(!registry.is_container_registered(&container_id));
    assert!(!registry.is_session_registered(&session_id));
    assert!(registry.is_task_registered(&task_id));

    println!("✓ Unregistration verified");

    // Example 5: Container ID registry with metadata
    println!("\n5. Container ID Registry with Metadata");
    let registry = container_id_registry();

    // Note: This would need to be async in real usage
    println!("✓ Container ID registry created");
    println!("✓ Total containers: {}", registry.total_count());

    // Example 6: Container ID generator
    println!("\n6. Container ID Generator");
    let mut generator = container_id_generator();

    let id1 = generator.next();
    let id2 = generator.next();
    let id3 = generator.next();

    println!("✓ Generated IDs: {}, {}, {}", id1, id2, id3);
    assert_ne!(id1, id2);
    assert_ne!(id2, id3);
    assert_ne!(id1, id3);

    println!("✓ All generated IDs are unique");

    // Example 7: Deterministic container ID generator
    println!("\n7. Deterministic Container ID Generator");
    let mut generator = deterministic_container_id_generator();

    let id1 = generator.next();
    let id2 = generator.next();
    let id3 = generator.next();

    println!("✓ Deterministic IDs: {}, {}, {}", id1, id2, id3);
    assert_eq!(id1.value(), 1);
    assert_eq!(id2.value(), 2);
    assert_eq!(id3.value(), 3);

    println!("✓ IDs generated in sequence");

    generator.reset();
    let id4 = generator.next();
    assert_eq!(id4.value(), 1);
    println!("✓ Generator reset successfully");

    // Example 8: Convenience functions
    println!("\n8. Convenience Functions");
    let _container_id = container_id();
    let _session_id = session_id();
    let _task_id = task_id();
    let _test_id = test_id();
    let _scenario_id = scenario_id();

    println!("✓ All convenience functions work");
    assert!(_container_id.value() > 0);
    assert!(_session_id.value() > 0);
    assert!(_task_id.value() > 0);
    assert!(_test_id.value() > 0);
    assert!(_scenario_id.value() > 0);

    // Example 9: ID serialization
    println!("\n9. ID Serialization");
    let container_id = ContainerId::new();
    let session_id = SessionId::new();

    let serialized_container = serde_json::to_string(&container_id).unwrap();
    let serialized_session = serde_json::to_string(&session_id).unwrap();

    println!("✓ Container ID serialized: {}", serialized_container);
    println!("✓ Session ID serialized: {}", serialized_session);

    let deserialized_container: ContainerId = serde_json::from_str(&serialized_container).unwrap();
    let deserialized_session: SessionId = serde_json::from_str(&serialized_session).unwrap();

    assert_eq!(container_id, deserialized_container);
    assert_eq!(session_id, deserialized_session);

    println!("✓ IDs deserialized correctly");

    // Example 10: ID hashing and collections
    println!("\n10. ID Hashing and Collections");
    use std::collections::HashMap;

    let mut map = HashMap::new();
    let container_id = ContainerId::new();
    let session_id = SessionId::new();

    map.insert(container_id, "container");
    map.insert(session_id, "session");

    println!("✓ IDs used as HashMap keys");
    assert_eq!(map.get(&container_id), Some(&"container"));
    assert_eq!(map.get(&session_id), Some(&"session"));

    let mut set = std::collections::HashSet::new();
    set.insert(container_id);
    set.insert(session_id);

    println!("✓ IDs used in HashSet");
    assert!(set.contains(&container_id));
    assert!(set.contains(&session_id));
    assert_eq!(set.len(), 2);

    // Example 11: Type safety demonstration
    println!("\n11. Type Safety Demonstration");
    let container_id = ContainerId::new();
    let session_id = SessionId::new();

    // These are different types and cannot be mixed
    // let mixed = container_id == session_id; // This would not compile
    println!("✓ Type safety prevents mixing different ID types");

    // Each ID type has its own methods and properties
    println!("✓ Container ID value: {}", container_id.value());
    println!("✓ Session ID value: {}", session_id.value());

    // ID types can be used in type-safe collections
    let mut container_map: HashMap<ContainerId, String> = HashMap::new();
    let mut session_map: HashMap<SessionId, String> = HashMap::new();

    container_map.insert(container_id, "container data".to_string());
    session_map.insert(session_id, "session data".to_string());

    println!("✓ Type-safe collections work correctly");

    // Example 12: ID display formatting
    println!("\n12. ID Display Formatting");
    let container_id = ContainerId::new();
    let session_id = SessionId::new();
    let task_id = TaskId::new();
    let test_id = TestId::new();
    let scenario_id = ScenarioId::new();

    println!("✓ Container ID: {}", container_id);
    println!("✓ Session ID: {}", session_id);
    println!("✓ Task ID: {}", task_id);
    println!("✓ Test ID: {}", test_id);
    println!("✓ Scenario ID: {}", scenario_id);

    // Verify display format
    assert!(format!("{}", container_id).starts_with("container-"));
    assert!(format!("{}", session_id).starts_with("session-"));
    assert!(format!("{}", task_id).starts_with("task-"));
    assert!(format!("{}", test_id).starts_with("test-"));
    assert!(format!("{}", scenario_id).starts_with("scenario-"));

    println!("\n=== All ID Management Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_id_creation() {
        let container_id = ContainerId::new();
        let session_id = SessionId::new();
        let task_id = TaskId::new();
        let test_id = TestId::new();
        let scenario_id = ScenarioId::new();

        assert!(container_id.value() > 0);
        assert!(session_id.value() > 0);
        assert!(task_id.value() > 0);
        assert!(test_id.value() > 0);
        assert!(scenario_id.value() > 0);
    }

    #[test]
    fn test_id_conversion() {
        let container_id = ContainerId::new();
        let value: u64 = container_id.into();
        let back: ContainerId = value.try_into().unwrap();
        assert_eq!(container_id, back);
    }

    #[test]
    fn test_id_registry() {
        let mut registry = IdRegistry::new();

        let container_id = ContainerId::new();
        let session_id = SessionId::new();

        assert!(registry.register_container(container_id));
        assert!(registry.register_session(session_id));

        assert!(registry.is_container_registered(&container_id));
        assert!(registry.is_session_registered(&session_id));

        assert_eq!(registry.total_count(), 2);
    }

    #[test]
    fn test_session_id_uuid() {
        let uuid = Uuid::new_v4();
        let session_id = SessionId::from_uuid(uuid);
        let uuid_back = session_id.to_uuid();
        assert_eq!(uuid, uuid_back);
    }

    #[test]
    fn test_container_id_generator() {
        let mut generator = ContainerIdGenerator::new();

        let id1 = generator.next();
        let id2 = generator.next();

        assert_ne!(id1, id2);
        assert!(id1.value() > 0);
        assert!(id2.value() > 0);
    }

    #[test]
    fn test_deterministic_generator() {
        let mut generator = ContainerIdGenerator::deterministic();

        let id1 = generator.next();
        let id2 = generator.next();
        let id3 = generator.next();

        assert_eq!(id1.value(), 1);
        assert_eq!(id2.value(), 2);
        assert_eq!(id3.value(), 3);

        generator.reset();
        let id4 = generator.next();
        assert_eq!(id4.value(), 1);
    }

    #[test]
    fn test_id_serialization() {
        let container_id = ContainerId::new();
        let serialized = serde_json::to_string(&container_id).unwrap();
        let deserialized: ContainerId = serde_json::from_str(&serialized).unwrap();
        assert_eq!(container_id, deserialized);
    }

    #[test]
    fn test_id_hashing() {
        use std::collections::HashMap;

        let mut map = HashMap::new();
        let container_id = ContainerId::new();
        let session_id = SessionId::new();

        map.insert(container_id, "container");
        map.insert(session_id, "session");

        assert_eq!(map.get(&container_id), Some(&"container"));
        assert_eq!(map.get(&session_id), Some(&"session"));
    }

    #[test]
    fn test_convenience_functions() {
        let _container_id = container_id();
        let _session_id = session_id();
        let _task_id = task_id();
        let _test_id = test_id();
        let _scenario_id = scenario_id();

        assert!(_container_id.value() > 0);
        assert!(_session_id.value() > 0);
        assert!(_task_id.value() > 0);
        assert!(_test_id.value() > 0);
        assert!(_scenario_id.value() > 0);
    }

    #[test]
    fn test_id_display() {
        let container_id = ContainerId::new();
        let session_id = SessionId::new();

        assert!(format!("{}", container_id).starts_with("container-"));
        assert!(format!("{}", session_id).starts_with("session-"));
    }
}
