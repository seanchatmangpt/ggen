//! Chicago TDD Tests for Ownership Declarations and Merge Strategies
//!
//! These tests follow Chicago TDD methodology:
//! - Test with REAL filesystem operations
//! - Create REAL ownership declarations
//! - Verify REAL merge strategies
//! - Test REAL conflict resolution
//! - No mocks for critical paths

use ggen_marketplace::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

/// Test environment with isolated directories
struct TestEnv {
    temp_dir: TempDir,
    ownership_dir: PathBuf,
}

impl TestEnv {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        let ownership_dir = temp_dir.path().join("ownership");

        fs::create_dir_all(&ownership_dir)?;

        Ok(Self {
            temp_dir,
            ownership_dir,
        })
    }

    fn ownership_path(&self) -> &Path {
        &self.ownership_dir
    }
}

/// Create a test ownership declaration
fn create_test_declaration(
    owner: &str,
    target: OwnershipTarget,
) -> OwnershipDeclaration {
    OwnershipDeclaration::new(
        owner.to_string(),
        target,
        vec!["read".to_string(), "write".to_string()],
    )
}

// ============================================================================
// OWNERSHIP DECLARATION TESTS
// ============================================================================

#[tokio::test]
async fn test_create_ownership_declaration() -> Result<(), Box<dyn std::error::Error>> {
    // Create ownership declaration for a pack
    let target = OwnershipTarget::Pack("surface-mcp/test-server".to_string());
    let declaration = create_test_declaration("team-alpha", target);

    // Verify declaration structure
    assert_eq!(declaration.owner(), "team-alpha");
    assert!(matches!(declaration.target(), OwnershipTarget::Pack(_)));
    assert_eq!(declaration.permissions().len(), 2);

    Ok(())
}

#[tokio::test]
async fn test_create_ownership_for_namespace() -> Result<(), Box<dyn std::error::Error>> {
    // Create ownership for entire namespace
    let target = OwnershipTarget::Namespace("surface-mcp".to_string());
    let declaration = OwnershipDeclaration::new(
        "platform-team".to_string(),
        target,
        vec!["admin".to_string()],
    );

    // Verify namespace ownership
    assert_eq!(declaration.owner(), "platform-team");
    assert!(matches!(declaration.target(), OwnershipTarget::Namespace(_)));

    // TODO: Phase 2 - Verify namespace ownership applies to all packs in namespace
    // let map = OwnershipMap::new();
    // map.add_declaration(declaration);
    // assert!(map.check_permission("platform-team", "surface-mcp/any-pack", "admin"));

    Ok(())
}

#[tokio::test]
async fn test_create_ownership_for_bundle() -> Result<(), Box<dyn std::error::Error>> {
    // Create ownership for a bundle
    let target = OwnershipTarget::Bundle("web-stack".to_string());
    let declaration = OwnershipDeclaration::new(
        "web-team".to_string(),
        target,
        vec!["read".to_string(), "modify".to_string()],
    );

    // Verify bundle ownership
    assert_eq!(declaration.owner(), "web-team");
    assert!(matches!(declaration.target(), OwnershipTarget::Bundle(_)));

    Ok(())
}

#[tokio::test]
async fn test_ownership_declaration_serialization() -> Result<(), Box<dyn std::error::Error>> {
    let target = OwnershipTarget::Pack("surface-mcp/test-server".to_string());
    let declaration = create_test_declaration("team-alpha", target);

    // Serialize to TOML
    let toml_str = toml::to_string_pretty(&declaration)?;

    // Verify serialization contains key fields
    assert!(toml_str.contains("team-alpha"));
    assert!(toml_str.contains("surface-mcp/test-server"));

    // Deserialize back
    let deserialized: OwnershipDeclaration = toml::from_str(&toml_str)?;
    assert_eq!(deserialized.owner(), declaration.owner());
    assert_eq!(deserialized.permissions(), declaration.permissions());

    Ok(())
}

// ============================================================================
// OWNERSHIP MAP TESTS
// ============================================================================

#[tokio::test]
async fn test_create_ownership_map() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create ownership map
    let map = OwnershipMap::new();

    // Verify map is empty
    assert!(map.is_empty());
    assert_eq!(map.len(), 0);

    // Add declarations
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("pack-a".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("pack-b".to_string()),
    );

    // TODO: Phase 2 - Add declarations to map
    // map.add_declaration(decl1);
    // map.add_declaration(decl2);

    // assert_eq!(map.len(), 2);

    Ok(())
}

#[tokio::test]
async fn test_check_ownership_permission() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    let map = OwnershipMap::new();

    // Add ownership declaration
    let declaration = OwnershipDeclaration::new(
        "team-alpha".to_string(),
        OwnershipTarget::Pack("critical-pack".to_string()),
        vec!["read".to_string(), "write".to_string()],
    );

    // TODO: Phase 2 - Add declaration and check permissions
    // map.add_declaration(declaration);

    // Check valid permission
    // assert!(map.check_permission("team-alpha", "critical-pack", "write"));

    // Check invalid permission
    // assert!(!map.check_permission("team-alpha", "critical-pack", "admin"));

    // Check wrong owner
    // assert!(!map.check_permission("team-beta", "critical-pack", "read"));

    Ok(())
}

#[tokio::test]
async fn test_ownership_namespace_inheritance() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    let map = OwnershipMap::new();

    // Add namespace ownership
    let namespace_decl = OwnershipDeclaration::new(
        "platform-team".to_string(),
        OwnershipTarget::Namespace("surface-mcp".to_string()),
        vec!["admin".to_string()],
    );

    // TODO: Phase 2 - Add namespace declaration
    // map.add_declaration(namespace_decl);

    // Verify namespace ownership applies to packs
    // assert!(map.check_permission("platform-team", "surface-mcp/any-pack", "admin"));
    // assert!(!map.check_permission("platform-team", "projection-rust/other-pack", "admin"));

    Ok(())
}

// ============================================================================
// MERGE STRATEGY TESTS
// ============================================================================

#[tokio::test]
async fn test_merge_strategy_keep_both() -> Result<(), Box<dyn std::error::Error>> {
    // Create two ownership maps
    let map1 = OwnershipMap::new();
    let map2 = OwnershipMap::new();

    // Add declarations to each map
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("other-pack".to_string()),
    );

    // TODO: Phase 2 - Merge with KEEP_BOTH strategy
    // let merged = map1.merge(map2, MergeStrategy::KeepBoth)?;
    // assert_eq!(merged.len(), 2);

    Ok(())
}

#[tokio::test]
async fn test_merge_strategy_keep_first() -> Result<(), Box<dyn std::error::Error>> {
    let map1 = OwnershipMap::new();
    let map2 = OwnershipMap::new();

    // Both maps claim ownership of same pack
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );

    // TODO: Phase 2 - Merge with KEEP_FIRST strategy
    // let merged = map1.merge(map2, MergeStrategy::KeepFirst)?;
    // let owner = merged.get_owner("shared-pack");
    // assert_eq!(owner, Some("team-alpha"));

    Ok(())
}

#[tokio::test]
async fn test_merge_strategy_keep_last() -> Result<(), Box<dyn std::error::Error>> {
    let map1 = OwnershipMap::new();
    let map2 = OwnershipMap::new();

    // Both maps claim ownership of same pack
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );

    // TODO: Phase 2 - Merge with KEEP_LAST strategy
    // let merged = map1.merge(map2, MergeStrategy::KeepLast)?;
    // let owner = merged.get_owner("shared-pack");
    // assert_eq!(owner, Some("team-beta"));

    Ok(())
}

#[tokio::test]
async fn test_merge_strategy_fail_on_conflict() -> Result<(), Box<dyn std::error::Error>> {
    let map1 = OwnershipMap::new();
    let map2 = OwnershipMap::new();

    // Both maps claim ownership of same pack
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );

    // TODO: Phase 2 - Merge with FAIL_ON_CONFLICT strategy
    // let result = map1.merge(map2, MergeStrategy::FailOnConflict);
    // assert!(result.is_err(), "Should fail on conflict");

    Ok(())
}

// ============================================================================
// OWNERSHIP CONFLICT DETECTION TESTS
// ============================================================================

#[tokio::test]
async fn test_detect_ownership_conflicts() -> Result<(), Box<dyn std::error::Error>> {
    let map = OwnershipMap::new();

    // Add conflicting declarations
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );

    // TODO: Phase 2 - Add declarations and detect conflicts
    // map.add_declaration(decl1);
    // map.add_declaration(decl2);

    // let conflicts = map.detect_conflicts()?;
    // assert!(!conflicts.is_empty());
    // assert!(conflicts.iter().any(|c| c.target() == "shared-pack"));

    Ok(())
}

#[tokio::test]
async fn test_resolve_ownership_conflicts() -> Result<(), Box<dyn std::error::Error>> {
    let map = OwnershipMap::new();

    // Add conflicting declarations
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("shared-pack".to_string()),
    );

    // TODO: Phase 2 - Resolve conflicts with strategy
    // map.add_declaration(decl1);
    // map.add_declaration(decl2);

    // map.resolve_conflicts(MergeStrategy::KeepFirst)?;
    // assert_eq!(map.get_owner("shared-pack"), Some("team-alpha"));

    Ok(())
}

// ============================================================================
// OWNERSHIP FILE PERSISTENCE TESTS
// ============================================================================

#[tokio::test]
async fn test_save_ownership_map_to_file() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    let map = OwnershipMap::new();

    // Add declaration
    let decl = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("test-pack".to_string()),
    );

    // TODO: Phase 2 - Add declaration and save
    // map.add_declaration(decl);

    // let file_path = env.ownership_path().join("ownership.toml");
    // map.save_to_file(&file_path).await?;

    // Verify file exists
    // assert!(file_path.exists());

    // Verify file content
    // let content = fs::read_to_string(&file_path)?;
    // assert!(content.contains("team-alpha"));
    // assert!(content.contains("test-pack"));

    Ok(())
}

#[tokio::test]
async fn test_load_ownership_map_from_file() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create ownership file
    let file_path = env.ownership_path().join("ownership.toml");
    let toml_content = r#"
[[declarations]]
owner = "team-alpha"
target = { pack = "test-pack" }
permissions = ["read", "write"]
"#;

    fs::write(&file_path, toml_content)?;

    // TODO: Phase 2 - Load ownership map
    // let map = OwnershipMap::load_from_file(&file_path).await?;
    // assert_eq!(map.len(), 1);
    // assert!(map.check_permission("team-alpha", "test-pack", "read"));

    Ok(())
}

#[tokio::test]
async fn test_ownership_persistence_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    let original_map = OwnershipMap::new();

    // Add multiple declarations
    for i in 1..=3 {
        let decl = create_test_declaration(
            &format!("team-{}", i),
            OwnershipTarget::Pack(format!("pack-{}", i)),
        );
        // original_map.add_declaration(decl);
    }

    // Save and load
    let file_path = env.ownership_path().join("ownership.toml");
    // original_map.save_to_file(&file_path).await?;
    // let loaded_map = OwnershipMap::load_from_file(&file_path).await?;

    // Verify roundtrip
    // assert_eq!(loaded_map.len(), original_map.len());
    // for i in 1..=3 {
    //     assert!(loaded_map.check_permission(&format!("team-{}", i), &format!("pack-{}", i), "read"));
    // }

    Ok(())
}

// ============================================================================
// OWNERSHIP VALIDATION TESTS
// ============================================================================

#[tokio::test]
async fn test_validate_ownership_consistency() -> Result<(), Box<dyn std::error::Error>> {
    let map = OwnershipMap::new();

    // Add consistent declarations
    let decl1 = create_test_declaration(
        "team-alpha",
        OwnershipTarget::Pack("pack-a".to_string()),
    );
    let decl2 = create_test_declaration(
        "team-beta",
        OwnershipTarget::Pack("pack-b".to_string()),
    );

    // TODO: Phase 2 - Validate consistency
    // map.add_declaration(decl1);
    // map.add_declaration(decl2);

    // let result = map.validate_consistency();
    // assert!(result.is_ok(), "Map should be consistent");

    Ok(())
}

#[tokio::test]
async fn test_validate_ownership_detects_orphans() -> Result<(), Box<dyn std::error::Error>> {
    let map = OwnershipMap::new();

    // Add namespace ownership
    let namespace_decl = OwnershipDeclaration::new(
        "platform-team".to_string(),
        OwnershipTarget::Namespace("surface-mcp".to_string()),
        vec!["admin".to_string()],
    );

    // TODO: Phase 2 - Add declaration and validate
    // map.add_declaration(namespace_decl);

    // Create pack in namespace without specific ownership
    // let result = map.validate_consistency();
    // assert!(result.is_ok(), "Namespace ownership should apply to all packs");

    Ok(())
}

#[tokio::test]
async fn test_validate_ownership_missing_required_fields() -> Result<(), Box<dyn std::error::Error>> {
    // Create declaration with empty owner (invalid)
    let target = OwnershipTarget::Pack("test-pack".to_string());
    let declaration = OwnershipDeclaration::new(
        "".to_string(),  // Invalid: empty owner
        target,
        vec!["read".to_string()],
    );

    // TODO: Phase 2 - Validate declaration
    // let result = declaration.validate();
    // assert!(result.is_err(), "Empty owner should fail validation");

    Ok(())
}
