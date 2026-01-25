//! Dependency Resolution and Consolidation Tests
//!
//! Tests verify that dependency deduplication strategy is working correctly,
//! workspace.dependencies are properly inherited, and transitive dependencies don't bloat the build.

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use toml::Value;

/// State: Dependency metadata
#[derive(Debug, Clone)]
struct DependencyInfo {
    name: String,
    version: String,
    path: Option<String>,
}

/// State: Workspace dependencies analysis
#[derive(Debug, Clone)]
struct WorkspaceDependencies {
    workspace_deps: HashMap<String, String>,
    root_dependencies: HashMap<String, String>,
}

impl WorkspaceDependencies {
    fn load_from_workspace_root() -> anyhow::Result<Self> {
        let cargo_toml = std::env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .ok_or_else(|| anyhow::anyhow!("Cannot find workspace root"))?
            .join("Cargo.toml");

        let content = fs::read_to_string(&cargo_toml)?;
        let toml: Value = toml::from_str(&content)?;

        // Extract workspace dependencies
        let workspace_deps = toml
            .get("workspace")
            .and_then(|w| w.get("dependencies"))
            .and_then(|d| d.as_table())
            .cloned()
            .unwrap_or_default();

        let mut workspace_map = HashMap::new();
        for (name, value) in workspace_deps.iter() {
            let version = if let Some(table) = value.as_table() {
                table
                    .get("version")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "workspace".to_string())
            } else if let Some(version_str) = value.as_str() {
                version_str.to_string()
            } else {
                "unknown".to_string()
            };
            workspace_map.insert(name.clone(), version);
        }

        // Extract root dependencies
        let root_deps = toml
            .get("dependencies")
            .and_then(|d| d.as_table())
            .cloned()
            .unwrap_or_default();

        let mut root_map = HashMap::new();
        for (name, value) in root_deps.iter() {
            let version = if let Some(table) = value.as_table() {
                table
                    .get("version")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "workspace".to_string())
            } else if let Some(version_str) = value.as_str() {
                version_str.to_string()
            } else {
                "unknown".to_string()
            };
            root_map.insert(name.clone(), version);
        }

        Ok(Self {
            workspace_deps: workspace_map,
            root_dependencies: root_map,
        })
    }
}

#[test]
fn test_critical_crates_consolidated_to_single_versions() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check versions of critical deduplication crates
    let axum_version = deps.workspace_deps.get("axum").cloned();
    let tonic_version = deps.workspace_deps.get("tonic").cloned();
    let derive_more_version = deps.workspace_deps.get("derive_more").cloned();
    let darling_version = deps.workspace_deps.get("darling").cloned();

    // Assert: These critical crates should be defined in workspace.dependencies
    assert!(
        axum_version.is_some(),
        "axum must be in workspace.dependencies"
    );
    assert!(
        tonic_version.is_some(),
        "tonic must be in workspace.dependencies"
    );
    assert!(
        derive_more_version.is_some(),
        "derive_more must be in workspace.dependencies"
    );
    assert!(
        darling_version.is_some(),
        "darling must be in workspace.dependencies"
    );
}

#[test]
fn test_axum_version_matches_specification() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check axum version
    let axum_version = deps.workspace_deps.get("axum").cloned();

    // Assert: Axum should be at v0.8 per DEPENDENCY_DEDUPLICATION_PLAN
    assert_eq!(
        axum_version,
        Some("0.8".to_string()),
        "axum must be consolidated to v0.8"
    );
}

#[test]
fn test_tonic_version_matches_specification() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check tonic version
    let tonic_version = deps.workspace_deps.get("tonic").cloned();

    // Assert: Tonic should be at v0.14 per DEPENDENCY_DEDUPLICATION_PLAN
    assert_eq!(
        tonic_version,
        Some("0.14".to_string()),
        "tonic must be consolidated to v0.14"
    );
}

#[test]
fn test_derive_more_version_matches_specification() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check derive_more version
    let derive_more_version = deps.workspace_deps.get("derive_more").cloned();

    // Assert: derive_more should be at v1.0 per DEPENDENCY_DEDUPLICATION_PLAN
    assert_eq!(
        derive_more_version,
        Some("1.0".to_string()),
        "derive_more must be consolidated to v1.0"
    );
}

#[test]
fn test_darling_version_matches_specification() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check darling version
    let darling_version = deps.workspace_deps.get("darling").cloned();

    // Assert: darling should be at v0.21 per DEPENDENCY_DEDUPLICATION_PLAN
    assert_eq!(
        darling_version,
        Some("0.21".to_string()),
        "darling must be consolidated to v0.21"
    );
}

#[test]
fn test_tokio_workspace_dependency_defined() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check tokio in workspace.dependencies
    let tokio = deps.workspace_deps.get("tokio").cloned();

    // Assert: Tokio should be in workspace.dependencies
    assert!(tokio.is_some(), "tokio must be in workspace.dependencies");
}

#[test]
fn test_serde_ecosystem_consolidated() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check serde crates are in workspace.dependencies
    let serde = deps.workspace_deps.get("serde").cloned();
    let serde_json = deps.workspace_deps.get("serde_json").cloned();

    // Assert: Serde ecosystem should be consolidated
    assert!(
        serde.is_some(),
        "serde must be in workspace.dependencies"
    );
    assert!(
        serde_json.is_some(),
        "serde_json must be in workspace.dependencies"
    );
}

#[test]
fn test_base64_version_pinned_to_resolve_conflicts() {
    // Arrange: Load workspace dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Check base64 version
    let base64_version = deps.workspace_deps.get("base64").cloned();

    // Assert: base64 should be pinned to v0.22 to resolve duplicate dependency issue
    assert_eq!(
        base64_version,
        Some("0.22".to_string()),
        "base64 must be pinned to 0.22 per CARGO_OPTIMIZATION_PLAN"
    );
}

#[test]
fn test_config_default_features_false() {
    // Arrange: Load root dependencies to check config crate
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");
    let cargo_toml = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("Cargo.toml"))
        .expect("Cannot find workspace root");

    let content = fs::read_to_string(cargo_toml).expect("Failed to read Cargo.toml");
    let toml: Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check config crate configuration
    let config = toml
        .get("workspace")
        .and_then(|w| w.get("dependencies"))
        .and_then(|d| d.get("config"))
        .and_then(|c| c.as_table());

    // Assert: config should have default-features=false
    if let Some(cfg_table) = config {
        let default_features = cfg_table
            .get("default-features")
            .and_then(|v| v.as_bool());
        assert_eq!(
            default_features,
            Some(false),
            "config crate must have default-features=false"
        );
    }
}

#[test]
fn test_multiple_crate_versions_allowed_for_proc_macros() {
    // Arrange: Load workspace lints configuration
    let cargo_toml = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("Cargo.toml"))
        .expect("Cannot find workspace root");

    let content = fs::read_to_string(cargo_toml).expect("Failed to read Cargo.toml");
    let toml: Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check if multiple_crate_versions is allowed
    let clippy_lints = toml
        .get("workspace")
        .and_then(|w| w.get("lints"))
        .and_then(|l| l.get("clippy"));

    // Assert: multiple_crate_versions should be "allow" for unavoidable proc-macro conflicts
    if let Some(lints) = clippy_lints {
        if let Some(level) = lints.get("multiple_crate_versions").and_then(|v| v.as_str()) {
            assert_eq!(
                level, "allow",
                "multiple_crate_versions must be allowed for unavoidable proc-macro conflicts"
            );
        }
    }
}

#[test]
fn test_workspace_resolver_v2_enabled() {
    // Arrange: Load workspace configuration
    let cargo_toml = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("Cargo.toml"))
        .expect("Cannot find workspace root");

    let content = fs::read_to_string(cargo_toml).expect("Failed to read Cargo.toml");
    let toml: Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check resolver version
    let resolver = toml
        .get("workspace")
        .and_then(|w| w.get("resolver"))
        .and_then(|r| r.as_str());

    // Assert: Workspace should use resolver v2 for better dependency resolution
    assert_eq!(
        resolver,
        Some("2"),
        "Workspace must use resolver = \"2\" for better dependency resolution"
    );
}

#[test]
fn test_core_local_dependencies_version_match() {
    // Arrange: Load workspace and root dependencies
    let deps = WorkspaceDependencies::load_from_workspace_root()
        .expect("Failed to load workspace Cargo.toml");

    // Act: Compare workspace dependency versions
    let core_version = deps.workspace_deps.get("ggen-core").cloned();
    let utils_version = deps.workspace_deps.get("ggen-utils").cloned();

    // Assert: All core crates should use v0.2.0
    assert_eq!(
        core_version,
        Some("0.2.0".to_string()),
        "ggen-core must be v0.2.0"
    );
    assert_eq!(
        utils_version,
        Some("0.2.0".to_string()),
        "ggen-utils must be v0.2.0"
    );
}
