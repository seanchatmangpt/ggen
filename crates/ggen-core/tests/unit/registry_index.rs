//! Unit tests for RegistryIndex operations

use ggen_core::registry::RegistryIndex;
use super::mock_impls::{create_mock_registry_index, create_mock_pack};
use std::collections::HashMap;
use chrono::Utc;

#[test]
fn test_registry_index_creation() {
    let index = create_mock_registry_index(3);
    assert_eq!(index.packs.len(), 3);
    assert!(index.updated <= Utc::now());
}

#[test]
fn test_registry_index_empty() {
    let index = RegistryIndex {
        updated: Utc::now(),
        packs: HashMap::new(),
    };

    assert!(index.packs.is_empty());
}

#[test]
fn test_registry_index_lookup() {
    let mut index = create_mock_registry_index(0);
    let pack = create_mock_pack("lookup-test", "Lookup Test", "1.0.0");

    index.packs.insert("lookup-test".to_string(), pack);

    assert!(index.packs.contains_key("lookup-test"));
    let retrieved = index.packs.get("lookup-test").unwrap();
    assert_eq!(retrieved.id, "lookup-test");
}

#[test]
fn test_registry_index_iteration() {
    let index = create_mock_registry_index(5);

    let mut count = 0;
    for (id, pack) in &index.packs {
        assert_eq!(id, &pack.id);
        count += 1;
    }

    assert_eq!(count, 5);
}

#[test]
fn test_registry_index_serialization_roundtrip() {
    let index = create_mock_registry_index(3);

    // Serialize to JSON
    let json = serde_json::to_string(&index).unwrap();

    // Deserialize back
    let parsed: RegistryIndex = serde_json::from_str(&json).unwrap();

    // Should be equivalent
    assert_eq!(index.packs.len(), parsed.packs.len());

    for (id, pack) in &index.packs {
        let parsed_pack = parsed.packs.get(id).unwrap();
        assert_eq!(pack.id, parsed_pack.id);
        assert_eq!(pack.name, parsed_pack.name);
        assert_eq!(pack.latest_version, parsed_pack.latest_version);
    }
}

#[test]
fn test_registry_index_pretty_print() {
    let index = create_mock_registry_index(2);

    // Should be able to pretty-print
    let json = serde_json::to_string_pretty(&index).unwrap();

    assert!(json.contains("mock-pack-0"));
    assert!(json.contains("mock-pack-1"));
    assert!(json.contains("updated"));
}

#[test]
fn test_registry_index_large_scale() {
    // Test with a larger number of packs
    let index = create_mock_registry_index(100);

    assert_eq!(index.packs.len(), 100);

    // Verify all packs are accessible
    for i in 0..100 {
        let id = format!("mock-pack-{}", i);
        assert!(index.packs.contains_key(&id));
    }
}

#[test]
fn test_registry_index_pack_names_unique() {
    let index = create_mock_registry_index(10);

    let mut names = std::collections::HashSet::new();
    for pack in index.packs.values() {
        assert!(names.insert(pack.name.clone()), "Duplicate pack name: {}", pack.name);
    }
}

#[test]
fn test_registry_index_all_packs_valid() {
    let index = create_mock_registry_index(10);

    for (id, pack) in &index.packs {
        // Verify ID consistency
        assert_eq!(id, &pack.id);

        // Verify latest version exists in versions map
        assert!(pack.versions.contains_key(&pack.latest_version),
            "Pack {} missing latest version {} in versions map",
            pack.id, pack.latest_version);

        // Verify required fields
        assert!(!pack.name.is_empty());
        assert!(!pack.description.is_empty());
    }
}
