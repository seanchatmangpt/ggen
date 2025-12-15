//! Unit tests for RegistryIndex operations

use super::mock_impls::{create_mock_pack, create_mock_registry_index};
use chicago_tdd_tools::prelude::*;
use chrono::Utc;
use ggen_core::registry::RegistryIndex;
use std::collections::HashMap;

test!(test_registry_index_creation, {
    // Arrange
    let index = create_mock_registry_index(3);

    // Act & Assert
    assert_eq!(index.packs.len(), 3);
    assert!(index.updated <= Utc::now());
});

test!(test_registry_index_empty, {
    // Arrange
    let index = RegistryIndex {
        updated: Utc::now(),
        packs: HashMap::new(),
    };

    // Assert
    assert!(index.packs.is_empty());
});

test!(test_registry_index_lookup, {
    // Arrange
    let mut index = create_mock_registry_index(0);
    let pack = create_mock_pack("lookup-test", "Lookup Test", "1.0.0");

    // Act
    index.packs.insert("lookup-test".to_string(), pack);

    // Assert
    assert!(index.packs.contains_key("lookup-test"));
    let retrieved = index.packs.get("lookup-test").unwrap();
    assert_eq!(retrieved.id, "lookup-test");
});

test!(test_registry_index_iteration, {
    // Arrange
    let index = create_mock_registry_index(5);

    // Act
    let mut count = 0;
    for (id, pack) in &index.packs {
        assert_eq!(id, &pack.id);
        count += 1;
    }

    // Assert
    assert_eq!(count, 5);
});

test!(test_registry_index_serialization_roundtrip, {
    // Arrange
    let index = create_mock_registry_index(3);

    // Act
    let json = serde_json::to_string(&index).unwrap();
    let parsed: RegistryIndex = serde_json::from_str(&json).unwrap();

    // Assert
    assert_eq!(index.packs.len(), parsed.packs.len());
    for (id, pack) in &index.packs {
        let parsed_pack = parsed.packs.get(id).unwrap();
        assert_eq!(pack.id, parsed_pack.id);
        assert_eq!(pack.name, parsed_pack.name);
        assert_eq!(pack.latest_version, parsed_pack.latest_version);
    }
});

test!(test_registry_index_pretty_print, {
    // Arrange
    let index = create_mock_registry_index(2);

    // Act
    let json = serde_json::to_string_pretty(&index).unwrap();

    // Assert
    assert!(json.contains("mock-pack-0"));
    assert!(json.contains("mock-pack-1"));
    assert!(json.contains("updated"));
});

test!(test_registry_index_large_scale, {
    // Arrange
    let index = create_mock_registry_index(100);

    // Assert
    assert_eq!(index.packs.len(), 100);
    for i in 0..100 {
        let id = format!("mock-pack-{}", i);
        assert!(index.packs.contains_key(&id));
    }
});

test!(test_registry_index_pack_names_unique, {
    // Arrange
    let index = create_mock_registry_index(10);

    // Act & Assert
    let mut names = std::collections::HashSet::new();
    for pack in index.packs.values() {
        assert!(
            names.insert(pack.name.clone()),
            "Duplicate pack name: {}",
            pack.name
        );
    }
});

test!(test_registry_index_all_packs_valid, {
    // Arrange
    let index = create_mock_registry_index(10);

    // Assert
    for (id, pack) in &index.packs {
        assert_eq!(id, &pack.id);
        assert!(
            pack.versions.contains_key(&pack.latest_version),
            "Pack {} missing latest version {} in versions map",
            pack.id,
            pack.latest_version
        );
        assert!(!pack.name.is_empty());
        assert!(!pack.description.is_empty());
    }
});
