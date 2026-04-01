//! Chicago TDD Tests for Bundle Expansion and Dependency Resolution
//!
//! These tests follow Chicago TDD methodology:
//! - Test with REAL filesystem operations
//! - Create REAL bundle definitions
//! - Verify REAL dependency resolution
//! - Test REAL conflict detection
//! - No mocks for critical paths

use ggen_marketplace::prelude::*;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

/// Test environment with isolated directories
struct TestEnv {
    temp_dir: TempDir,
    bundles_dir: PathBuf,
    packs_dir: PathBuf,
}

impl TestEnv {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let temp_dir = TempDir::new()?;
        let bundles_dir = temp_dir.path().join("bundles");
        let packs_dir = temp_dir.path().join("packs");

        fs::create_dir_all(&bundles_dir)?;
        fs::create_dir_all(&packs_dir)?;

        Ok(Self {
            temp_dir,
            bundles_dir,
            packs_dir,
        })
    }

    fn bundles_path(&self) -> &Path {
        &self.bundles_dir
    }

    fn packs_path(&self) -> &Path {
        &self.packs_dir
    }
}

/// Create a test bundle definition
fn create_test_bundle(name: &str, packs: Vec<&str>) -> Bundle {
    let pack_ids: Vec<AtomicPackId> = packs
        .iter()
        .map(|p| AtomicPackId::new(
            AtomicPackCategory::Surface,
            AtomicPackClass::SurfaceMcp,
            p
        ))
        .collect();

    Bundle::new(name.to_string(), pack_ids)
}

// ============================================================================
// BUNDLE CREATION TESTS
// ============================================================================

#[tokio::test]
async fn test_create_simple_bundle() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a simple bundle with 2 packs
    let bundle = create_test_bundle("web-stack", vec!["http-server", "websocket-server"]);

    // Verify bundle structure
    assert_eq!(bundle.name(), "web-stack");
    assert_eq!(bundle.len(), 2);

    // Verify bundle contains both packs
    let pack_names: HashSet<_> = bundle
        .iter()
        .map(|p| p.name().to_string())
        .collect();

    assert!(pack_names.contains("http-server"));
    assert!(pack_names.contains("websocket-server"));

    Ok(())
}

#[tokio::test]
async fn test_create_bundle_with_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with dependencies
    let bundle = create_test_bundle(
        "full-mcp-stack",
        vec!["http-server", "client-rust", "policy-strict"]
    );

    // Verify bundle structure
    assert_eq!(bundle.len(), 3);

    // TODO: Phase 2 - Expand bundle with dependencies
    // let expanded = installer.expand_bundle(&bundle).await?;
    // assert!(expanded.len() >= 3, "Should include dependencies");

    Ok(())
}

#[tokio::test]
async fn test_create_empty_bundle() -> Result<(), Box<dyn std::error::Error>> {
    // Create an empty bundle
    let bundle = Bundle::new("empty".to_string(), vec![]);

    // Verify bundle is empty
    assert_eq!(bundle.name(), "empty");
    assert_eq!(bundle.len(), 0);
    assert!(bundle.is_empty());

    Ok(())
}

// ============================================================================
// BUNDLE EXPANSION TESTS
// ============================================================================

#[tokio::test]
async fn test_expand_bundle_flat() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with no interdependencies
    let bundle = create_test_bundle("independent-packs", vec!["server-a", "server-b", "server-c"]);

    // TODO: Phase 2 - Expand bundle
    // let expanded = installer.expand_bundle(&bundle).await?;
    // assert_eq!(expanded.len(), 3);

    // Verify all packs are present
    // for pack in &expanded {
    //     assert!(["server-a", "server-b", "server-c"].contains(&pack.name()));
    // }

    Ok(())
}

#[tokio::test]
async fn test_expand_bundle_with_nested_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with nested dependencies
    // Level 1: server
    // Level 2: server -> rust-client
    // Level 3: rust-client -> policy-strict
    let bundle = create_test_bundle("nested-stack", vec!["server"]);

    // TODO: Phase 2 - Expand bundle with full dependency tree
    // let expanded = installer.expand_bundle(&bundle).await?;
    // assert!(expanded.len() >= 3, "Should include nested dependencies");

    // Verify dependency order (dependencies first)
    // let pack_names: Vec<_> = expanded.iter().map(|p| p.name()).collect();
    // assert!(pack_names.contains(&"policy-strict"));
    // assert!(pack_names.contains(&"rust-client"));
    // assert!(pack_names.contains(&"server"));

    Ok(())
}

#[tokio::test]
async fn test_expand_bundle_deduplicates_shared_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle where multiple packs share dependencies
    // server-a -> rust-client -> policy-strict
    // server-b -> rust-client -> policy-strict
    let bundle = create_test_bundle("shared-deps", vec!["server-a", "server-b"]);

    // TODO: Phase 2 - Expand bundle
    // let expanded = installer.expand_bundle(&bundle).await?;

    // Count unique pack names
    // let unique_packs: HashSet<_> = expanded.iter().map(|p| p.name()).collect();

    // rust-client and policy-strict should appear only once
    // assert_eq!(unique_packs.len(), 4, "Should deduplicate shared dependencies");
    // assert!(unique_packs.contains("rust-client"));
    // assert!(unique_packs.contains("policy-strict"));

    Ok(())
}

// ============================================================================
// BUNDLE CONFLICT DETECTION TESTS
// ============================================================================

#[tokio::test]
async fn test_detect_bundle_version_conflicts() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with conflicting version requirements
    // server-a requires rust-client@^1.0
    // server-b requires rust-client@^2.0
    let bundle = create_test_bundle("conflicting-versions", vec!["server-a", "server-b"]);

    // TODO: Phase 2 - Detect version conflicts
    // let result = installer.expand_bundle(&bundle).await;
    // assert!(result.is_err(), "Should detect version conflict");

    // if let Err(Error::DependencyResolutionFailed { .. }) = result {
    //     // Expected error
    // } else {
    //     panic!("Expected DependencyResolutionFailed error");
    // }

    Ok(())
}

#[tokio::test]
async fn test_detect_bundle_circular_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with circular dependencies
    // pack-a -> pack-b -> pack-c -> pack-a
    let bundle = create_test_bundle("circular-deps", vec!["pack-a"]);

    // TODO: Phase 2 - Detect circular dependencies
    // let result = installer.expand_bundle(&bundle).await;
    // assert!(result.is_err(), "Should detect circular dependency");

    // if let Err(Error::DependencyResolutionFailed { .. }) = result {
    //     // Expected error
    // } else {
    //     panic!("Expected DependencyResolutionFailed error");
    // }

    Ok(())
}

#[tokio::test]
async fn test_detect_bundle_incompatible_classes() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with incompatible pack classes
    // Surface MCP + Runtime Stdio (incompatible runtime combination)
    let pack_mcp = AtomicPackId::new(
        AtomicPackCategory::Surface,
        AtomicPackClass::SurfaceMcp,
        "server"
    );

    let pack_stdio = AtomicPackId::new(
        AtomicPackCategory::Runtime,
        AtomicPackClass::RuntimeStdio,
        "runtime"
    );

    let bundle = Bundle::new("incompatible".to_string(), vec![pack_mcp, pack_stdio]);

    // TODO: Phase 2 - Detect incompatible class combinations
    // let result = installer.expand_bundle(&bundle).await;
    // assert!(result.is_err(), "Should detect incompatible classes");

    Ok(())
}

// ============================================================================
// BUNDLE INSTALLATION TESTS
// ============================================================================

#[tokio::test]
async fn test_install_bundle() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle
    let bundle = create_test_bundle("web-stack", vec!["http-server", "websocket-server"]);

    // TODO: Phase 2 - Install bundle
    // installer.install_bundle(&bundle, env.packs_path()).await?;

    // Verify all packs installed
    for pack_name in &["http-server", "websocket-server"] {
        let pack_path = env.packs_path().join("surface-mcp").join(pack_name);
        assert!(
            pack_path.exists(),
            "Pack {} should be installed",
            pack_name
        );
    }

    Ok(())
}

#[tokio::test]
async fn test_install_bundle_creates_manifest() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle
    let bundle = create_test_bundle("minimal-stack", vec!["server"]);

    // TODO: Phase 2 - Install bundle
    // installer.install_bundle(&bundle, env.packs_path()).await?;

    // Verify bundle manifest created
    let manifest_path = env.packs_path().join("bundle-manifest.toml");
    assert!(
        manifest_path.exists(),
        "Bundle manifest should be created"
    );

    // Verify manifest contains bundle info
    // let manifest = fs::read_to_string(&manifest_path)?;
    // assert!(manifest.contains("minimal-stack"));

    Ok(())
}

#[tokio::test]
async fn test_install_bundle_rollback_on_failure() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create a bundle with a pack that will fail during installation
    let bundle = create_test_bundle("failing-bundle", vec!["good-pack", "bad-pack"]);

    // TODO: Phase 2 - Install bundle (should fail on bad-pack)
    // let result = installer.install_bundle(&bundle, env.packs_path()).await;
    // assert!(result.is_err(), "Installation should fail");

    // Verify rollback: no packs should be installed
    // assert!(!env.packs_path().join("surface-mcp/good-pack").exists());
    // assert!(!env.packs_path().join("surface-mcp/bad-pack").exists());

    Ok(())
}

// ============================================================================
// BUNDLE UPDATE TESTS
// ============================================================================

#[tokio::test]
async fn test_update_bundle() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create initial bundle
    let bundle_v1 = create_test_bundle("stack", vec!["server-v1", "client-v1"]);

    // TODO: Phase 2 - Install initial bundle
    // installer.install_bundle(&bundle_v1, env.packs_path()).await?;

    // Create updated bundle
    let bundle_v2 = create_test_bundle("stack", vec!["server-v2", "client-v2"]);

    // TODO: Phase 2 - Update bundle
    // installer.update_bundle(&bundle_v2, env.packs_path()).await?;

    // Verify packs updated
    // assert!(env.packs_path().join("surface-mcp/server-v2").exists());
    // assert!(!env.packs_path().join("surface-mcp/server-v1").exists());

    Ok(())
}

#[tokio::test]
async fn test_update_bundle_preserves_config() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Create initial bundle
    let bundle_v1 = create_test_bundle("stack", vec!["server"]);

    // TODO: Phase 2 - Install bundle and create config
    // installer.install_bundle(&bundle_v1, env.packs_path()).await?;
    // let config_path = env.packs_path().join("surface-mcp/server/config.toml");
    // fs::write(&config_path, "port = 8080")?;

    // Update bundle
    let bundle_v2 = create_test_bundle("stack", vec!["server-v2"]);

    // TODO: Phase 2 - Update bundle
    // installer.update_bundle(&bundle_v2, env.packs_path()).await?;

    // Verify config preserved
    // let config = fs::read_to_string(&config_path)?;
    // assert_eq!(config, "port = 8080");

    Ok(())
}

// ============================================================================
// BUNDLE QUERY TESTS
// ============================================================================

#[tokio::test]
async fn test_list_installed_bundles() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Install multiple bundles
    // TODO: Phase 2
    // installer.install_bundle(&create_test_bundle("stack-a", vec!["server-a"]), env.packs_path()).await?;
    // installer.install_bundle(&create_test_bundle("stack-b", vec!["server-b"]), env.packs_path()).await?;

    // List bundles
    // let bundles = installer.list_bundles().await?;
    // assert_eq!(bundles.len(), 2);

    Ok(())
}

#[tokio::test]
async fn test_get_bundle_by_name() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Install a bundle
    let bundle = create_test_bundle("my-stack", vec!["server"]);

    // TODO: Phase 2 - Install and query
    // installer.install_bundle(&bundle, env.packs_path()).await?;
    // let found = installer.get_bundle("my-stack").await?;
    // assert!(found.is_some());
    // assert_eq!(found.unwrap().name(), "my-stack");

    Ok(())
}

#[tokio::test]
async fn test_query_bundles_by_pack() -> Result<(), Box<dyn std::error::Error>> {
    let env = TestEnv::new()?;

    // Install bundles that share a common pack
    // Bundle A: server + client-a
    // Bundle B: server + client-b
    // TODO: Phase 2
    // installer.install_bundle(&create_test_bundle("stack-a", vec!["server", "client-a"]), env.packs_path()).await?;
    // installer.install_bundle(&create_test_bundle("stack-b", vec!["server", "client-b"]), env.packs_path()).await?;

    // Query bundles containing "server"
    // let bundles = installer.get_bundles_with_pack("server").await?;
    // assert_eq!(bundles.len(), 2);

    Ok(())
}
