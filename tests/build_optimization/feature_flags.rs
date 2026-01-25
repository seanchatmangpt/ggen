//! Feature Flag Configuration Tests
//!
//! Tests verify that feature flags are correctly configured, feature combinations work correctly,
//! and feature gating properly controls optional dependencies.

use std::fs;
use std::path::PathBuf;
use toml::Value;

/// State: Parsed features configuration
#[derive(Debug, Clone)]
struct FeaturesConfig {
    default: Vec<String>,
    features: std::collections::HashMap<String, Vec<String>>,
}

impl FeaturesConfig {
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

        let features_table = toml
            .get("features")
            .ok_or_else(|| anyhow::anyhow!("No features table found"))?
            .as_table()
            .ok_or_else(|| anyhow::anyhow!("Features is not a table"))?;

        let default = features_table
            .get("default")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default();

        let mut features = std::collections::HashMap::new();
        for (name, value) in features_table.iter() {
            if name != "default" {
                if let Some(arr) = value.as_array() {
                    features.insert(
                        name.clone(),
                        arr.iter()
                            .filter_map(|v| v.as_str().map(|s| s.to_string()))
                            .collect(),
                    );
                }
            }
        }

        Ok(Self { default, features })
    }
}

#[test]
fn test_default_feature_is_core() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect default feature set
    let default_features = config.default.clone();

    // Assert: Default should only include "core" for minimal build
    assert_eq!(
        default_features,
        vec!["core".to_string()],
        "Default features must only include 'core' for fast builds"
    );
}

#[test]
fn test_core_feature_is_empty() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect core feature definition
    let core_deps = config.features.get("core").cloned().unwrap_or_default();

    // Assert: Core feature should be empty (no additional dependencies)
    assert_eq!(
        core_deps, vec![],
        "Core feature must be empty (no optional dependencies)"
    );
}

#[test]
fn test_ai_feature_includes_required_crates() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect AI feature definition
    let ai_deps = config.features.get("ai").cloned().unwrap_or_default();

    // Assert: AI feature should include ggen-ai and genai
    assert!(
        ai_deps.contains(&"ggen-ai".to_string()),
        "AI feature must include ggen-ai"
    );
    assert!(
        ai_deps.contains(&"genai".to_string()),
        "AI feature must include genai"
    );
}

#[test]
fn test_otel_feature_includes_otel_crate() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect OTEL feature definition
    let otel_deps = config.features.get("otel").cloned().unwrap_or_default();

    // Assert: OTEL feature should include ggen-core/otel
    assert!(
        otel_deps.contains(&"ggen-core/otel".to_string()),
        "OTEL feature must include ggen-core/otel"
    );
}

#[test]
fn test_convenience_bundles_include_core() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect convenience bundle definitions
    let prod_features = config.features.get("prod").cloned().unwrap_or_default();
    let dev_features = config.features.get("dev").cloned().unwrap_or_default();
    let full_features = config.features.get("full").cloned().unwrap_or_default();

    // Assert: All bundles should include "core"
    assert!(
        prod_features.contains(&"core".to_string()),
        "prod bundle must include core"
    );
    assert!(
        dev_features.contains(&"core".to_string()),
        "dev bundle must include core"
    );
    assert!(
        full_features.contains(&"core".to_string()),
        "full bundle must include core"
    );
}

#[test]
fn test_dev_bundle_includes_ai() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect dev bundle
    let dev_features = config.features.get("dev").cloned().unwrap_or_default();

    // Assert: Dev bundle should include both core and ai for local development
    assert!(
        dev_features.contains(&"core".to_string()),
        "dev bundle must include core"
    );
    assert!(
        dev_features.contains(&"ai".to_string()),
        "dev bundle must include ai for development"
    );
}

#[test]
fn test_full_bundle_includes_all_features() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect full bundle
    let full_features = config.features.get("full").cloned().unwrap_or_default();

    // Assert: Full bundle should include core, ai, and otel
    assert!(
        full_features.contains(&"core".to_string()),
        "full bundle must include core"
    );
    assert!(
        full_features.contains(&"ai".to_string()),
        "full bundle must include ai"
    );
    assert!(
        full_features.contains(&"otel".to_string()),
        "full bundle must include otel"
    );
}

#[test]
fn test_prod_bundle_minimal() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect prod bundle
    let prod_features = config.features.get("prod").cloned().unwrap_or_default();

    // Assert: Prod bundle should only include core for minimal build
    assert_eq!(
        prod_features,
        vec!["core".to_string()],
        "prod bundle must be minimal (core only)"
    );
}

#[test]
fn test_backward_compatibility_features_exist() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Check for backward compatibility features
    let all_features = config.features.keys().collect::<Vec<_>>();

    // Assert: Backward compatibility features should be defined
    assert!(
        all_features.iter().any(|f| f == &"nightly"),
        "nightly backward compatibility feature must be defined"
    );
    assert!(
        all_features.iter().any(|f| f == &"termlog"),
        "termlog backward compatibility feature must be defined"
    );
    assert!(
        all_features.iter().any(|f| f == &"journald"),
        "journald backward compatibility feature must be defined"
    );
}

#[test]
fn test_feature_combinations_are_logically_consistent() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Verify feature dependencies are consistent
    let ai_features = config.features.get("ai").cloned().unwrap_or_default();
    let full_features = config.features.get("full").cloned().unwrap_or_default();

    // Assert: Full should include all features from ai
    for ai_feature in ai_features.iter() {
        assert!(
            full_features.contains(ai_feature),
            "full bundle must include all ai features: {}",
            ai_feature
        );
    }
}

#[test]
fn test_optional_feature_reduces_dependencies() {
    // Arrange: Load features configuration
    let config = FeaturesConfig::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Count features
    let core_features = config.features.get("core").cloned().unwrap_or_default();
    let ai_features = config.features.get("ai").cloned().unwrap_or_default();

    // Assert: Optional features should add dependencies (ai should have more than core)
    assert!(
        ai_features.len() > core_features.len(),
        "ai feature should add dependencies beyond core"
    );
}
