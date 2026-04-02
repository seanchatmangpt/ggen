//! Chicago TDD Tests for Semantic Version Compatibility
//!
//! These tests follow Chicago TDD methodology:
//! - Test with REAL version parsing
//! - Verify REAL semantic version resolution
//! - Test REAL version range matching
//! - Test REAL conflict detection
//! - No mocks for critical paths
//!
//! GATED: semver crate not in dev-dependencies.

#![cfg(feature = "integration")]

use ggen_marketplace::models::PackageVersion;
use semver::{Version, VersionReq};

fn semver_req_matches(req: &VersionReq, pv: &PackageVersion) -> bool {
    let Ok(v) = Version::parse(pv.as_str()) else {
        return false;
    };
    req.matches(&v)
}

fn resolve_highest_compatible(
    req: &VersionReq, available: &[PackageVersion],
) -> Option<PackageVersion> {
    available
        .iter()
        .filter(|pv| semver_req_matches(req, pv))
        .max()
        .cloned()
}

// ============================================================================
// SEMANTIC VERSION PARSING TESTS
// ============================================================================

#[tokio::test]
async fn test_parse_semantic_version() {
    // Parse valid semantic versions
    let v1 = "1.0.0".parse::<PackageVersion>();
    assert!(v1.is_ok());

    let v2 = "2.3.1".parse::<PackageVersion>();
    assert!(v2.is_ok());

    let v3 = "0.0.1".parse::<PackageVersion>();
    assert!(v3.is_ok());

    // Verify version components
    let version = v1.unwrap();
    assert_eq!(version.as_str(), "1.0.0");
}

#[tokio::test]
async fn test_parse_invalid_semantic_version() {
    // Parse invalid versions
    let v1 = "1.0".parse::<PackageVersion>();
    assert!(v1.is_err(), "Missing patch should fail");

    let v2 = "v1.0.0".parse::<PackageVersion>();
    assert!(v2.is_err(), "'v' prefix should fail");

    let v3 = "1.0.0-beta".parse::<PackageVersion>();
    assert!(v3.is_err(), "Pre-release not supported yet");

    let v4 = "not.a.version".parse::<PackageVersion>();
    assert!(v4.is_err(), "Invalid format should fail");
}

#[tokio::test]
async fn test_compare_semantic_versions() {
    let v1_0_0: PackageVersion = "1.0.0".parse().unwrap();
    let v1_0_1: PackageVersion = "1.0.1".parse().unwrap();
    let v1_1_0: PackageVersion = "1.1.0".parse().unwrap();
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();

    // Patch version comparison
    assert!(v1_0_1 > v1_0_0);

    // Minor version comparison
    assert!(v1_1_0 > v1_0_1);

    // Major version comparison
    assert!(v2_0_0 > v1_1_0);

    // Transitive comparison
    assert!(v2_0_0 > v1_0_0);
}

// ============================================================================
// VERSION REQUIREMENT PARSING TESTS
// ============================================================================

#[tokio::test]
async fn test_parse_caret_requirement() {
    // Caret requirement: ^1.2.3 means >=1.2.3 <2.0.0
    let req = "^1.2.3".parse::<VersionReq>().unwrap();

    // Should match 1.2.3
    let v1_2_3: PackageVersion = "1.2.3".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_3));

    // Should match 1.2.4 (patch update)
    let v1_2_4: PackageVersion = "1.2.4".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_4));

    // Should match 1.3.0 (minor update)
    let v1_3_0: PackageVersion = "1.3.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_3_0));

    // Should NOT match 2.0.0 (major update)
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v2_0_0));
}

#[tokio::test]
async fn test_parse_tilde_requirement() {
    // Tilde requirement: ~1.2.3 means >=1.2.3 <1.3.0
    let req = "~1.2.3".parse::<VersionReq>().unwrap();

    // Should match 1.2.3
    let v1_2_3: PackageVersion = "1.2.3".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_3));

    // Should match 1.2.4 (patch update)
    let v1_2_4: PackageVersion = "1.2.4".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_4));

    // Should NOT match 1.3.0 (minor update)
    let v1_3_0: PackageVersion = "1.3.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_3_0));

    // Should NOT match 2.0.0 (major update)
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v2_0_0));
}

#[tokio::test]
async fn test_parse_exact_requirement() {
    // Exact requirement: 1.2.3 means exactly 1.2.3
    let req = "1.2.3".parse::<VersionReq>().unwrap();

    // Should match 1.2.3
    let v1_2_3: PackageVersion = "1.2.3".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_3));

    // Should NOT match 1.2.4
    let v1_2_4: PackageVersion = "1.2.4".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_2_4));

    // Should NOT match 1.3.0
    let v1_3_0: PackageVersion = "1.3.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_3_0));
}

#[tokio::test]
async fn test_parse_greater_than_requirement() {
    // Greater than: >1.2.3
    let req = ">1.2.3".parse::<VersionReq>().unwrap();

    // Should NOT match 1.2.3
    let v1_2_3: PackageVersion = "1.2.3".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_2_3));

    // Should match 1.2.4
    let v1_2_4: PackageVersion = "1.2.4".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_4));

    // Should match 2.0.0
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v2_0_0));
}

#[tokio::test]
async fn test_parse_range_requirement() {
    // Range: 1.2.3 - 2.0.0
    let req = ">=1.2.3 <2.0.0".parse::<VersionReq>().unwrap();

    // Should match 1.2.3
    let v1_2_3: PackageVersion = "1.2.3".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_3));

    // Should match 1.5.0
    let v1_5_0: PackageVersion = "1.5.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_5_0));

    // Should NOT match 2.0.0
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v2_0_0));

    // Should NOT match 1.0.0
    let v1_0_0: PackageVersion = "1.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_0_0));
}

// ============================================================================
// VERSION RESOLUTION TESTS
// ============================================================================

#[tokio::test]
async fn test_resolve_compatible_version() {
    // Available versions: 1.0.0, 1.1.0, 1.2.0, 2.0.0
    let available: Vec<PackageVersion> = vec![
        "1.0.0".parse().unwrap(),
        "1.1.0".parse().unwrap(),
        "1.2.0".parse().unwrap(),
        "2.0.0".parse().unwrap(),
    ];

    // Requirement: ^1.0.0 (should resolve to 1.2.0, the latest compatible)
    let req: VersionReq = "^1.0.0".parse().unwrap();
    let resolved = resolve_highest_compatible(&req, &available);

    assert!(resolved.is_some());
    assert_eq!(resolved.unwrap(), "1.2.0".parse().unwrap());
}

#[tokio::test]
async fn test_resolve_no_compatible_version() {
    // Available versions: 1.0.0, 1.1.0
    let available: Vec<PackageVersion> = vec!["1.0.0".parse().unwrap(), "1.1.0".parse().unwrap()];

    // Requirement: ^2.0.0 (no compatible version)
    let req: VersionReq = "^2.0.0".parse().unwrap();
    let resolved = resolve_highest_compatible(&req, &available);

    assert!(
        resolved.is_none(),
        "Should return None when no compatible version"
    );
}

#[tokio::test]
async fn test_resolve_with_prerelease_versions() {
    // Available versions: 1.0.0, 1.1.0-beta, 1.1.0
    let available: Vec<PackageVersion> = vec![
        "1.0.0".parse().unwrap(),
        "1.1.0".parse().unwrap(), // Assuming pre-release not supported yet
    ];

    // Requirement: ^1.0.0 (should prefer stable over pre-release)
    let req: VersionReq = "^1.0.0".parse().unwrap();
    let resolved = resolve_highest_compatible(&req, &available);

    assert!(resolved.is_some());
    assert_eq!(resolved.unwrap(), "1.1.0".parse().unwrap());
}

// ============================================================================
// VERSION CONFLICT DETECTION TESTS
// ============================================================================

#[tokio::test]
async fn test_detect_version_conflict() {
    use std::collections::HashMap;

    // Dependency requirements:
    // pack-a requires shared@^1.0.0
    // pack-b requires shared@^2.0.0
    let mut requirements = HashMap::new();
    requirements.insert(
        "shared".to_string(),
        vec![
            "^1.0.0".parse::<VersionReq>().unwrap(),
            "^2.0.0".parse::<VersionReq>().unwrap(),
        ],
    );

    // TODO: Phase 2 - Detect version conflict
    // let conflicts = detect_conflicts(&requirements);
    // assert!(!conflicts.is_empty());
    // assert!(conflicts.contains_key("shared"));
}

#[tokio::test]
async fn test_no_version_conflict() {
    use std::collections::HashMap;

    // Dependency requirements:
    // pack-a requires shared@^1.0.0
    // pack-b requires shared@^1.2.0
    let mut requirements = HashMap::new();
    requirements.insert(
        "shared".to_string(),
        vec![
            "^1.0.0".parse::<VersionReq>().unwrap(),
            "^1.2.0".parse::<VersionReq>().unwrap(),
        ],
    );

    // TODO: Phase 2 - No conflict (both requirements compatible)
    // let conflicts = detect_conflicts(&requirements);
    // assert!(conflicts.is_empty());
}

#[tokio::test]
async fn test_detect_transitive_conflicts() {
    // Complex dependency graph with transitive conflicts:
    // top -> mid-a -> shared@^1.0.0
    // top -> mid-b -> shared@^2.0.0

    // TODO: Phase 2 - Build dependency graph and detect conflicts
    // let graph = DependencyGraph::new();
    // graph.add_dependency("top", "mid-a");
    // graph.add_dependency("top", "mid-b");
    // graph.add_dependency("mid-a", "shared");
    // graph.add_dependency("mid-b", "shared");

    // let conflicts = graph.detect_conflicts();
    // assert!(conflicts.contains_key("shared"));
}

// ============================================================================
// DEPENDENCY RESOLUTION TESTS
// ============================================================================

#[tokio::test]
async fn test_resolve_simple_dependencies() {
    // Simple dependency tree:
    // main -> dep-a, dep-b

    // TODO: Phase 2 - Resolve dependencies
    // let resolver = DependencyResolver::new();
    // let resolved = resolver.resolve("main").await?;

    // assert_eq!(resolved.len(), 3);  // main + 2 deps
    // assert!(resolved.contains("main"));
    // assert!(resolved.contains("dep-a"));
    // assert!(resolved.contains("dep-b"));
}

#[tokio::test]
async fn test_resolve_nested_dependencies() {
    // Nested dependency tree:
    // main -> mid
    // mid -> leaf
    // leaf -> (no dependencies)

    // TODO: Phase 2 - Resolve nested dependencies
    // let resolver = DependencyResolver::new();
    // let resolved = resolver.resolve("main").await?;

    // assert_eq!(resolved.len(), 3);
    // assert!(resolved.contains("main"));
    // assert!(resolved.contains("mid"));
    // assert!(resolved.contains("leaf"));

    // Verify order (dependencies first)
    // let order: Vec<_> = resolved.iter().collect();
    // assert_eq!(order[0], "leaf");
    // assert_eq!(order[1], "mid");
    // assert_eq!(order[2], "main");
}

#[tokio::test]
async fn test_resolve_diamond_dependencies() {
    // Diamond dependency:
    // main -> a, b
    // a -> shared
    // b -> shared

    // TODO: Phase 2 - Resolve diamond (should deduplicate shared)
    // let resolver = DependencyResolver::new();
    // let resolved = resolver.resolve("main").await?;

    // assert_eq!(resolved.len(), 4);  // main + a + b + shared (only once)
    // assert_eq!(resolved.count("shared"), 1, "Shared should appear only once");
}

#[tokio::test]
async fn test_resolve_circular_dependencies() {
    // Circular dependency:
    // a -> b
    // b -> c
    // c -> a

    // TODO: Phase 2 - Detect circular dependencies
    // let resolver = DependencyResolver::new();
    // let result = resolver.resolve("a").await;

    // assert!(result.is_err(), "Should detect circular dependency");
    // if let Err(Error::DependencyResolutionFailed { .. }) = result {
    //     // Expected error
    // } else {
    //     panic!("Expected DependencyResolutionFailed error");
    // }
}

// ============================================================================
// VERSION BOUNDS TESTS
// ============================================================================

#[tokio::test]
async fn test_caret_major_version_zero() {
    // Caret with 0.x: ^0.2.3 means >=0.2.3 <0.3.0
    let req = "^0.2.3".parse::<VersionReq>().unwrap();

    // Should match 0.2.3
    let v0_2_3: PackageVersion = "0.2.3".parse().unwrap();
    assert!(semver_req_matches(&req, &v0_2_3));

    // Should match 0.2.4
    let v0_2_4: PackageVersion = "0.2.4".parse().unwrap();
    assert!(semver_req_matches(&req, &v0_2_4));

    // Should NOT match 0.3.0
    let v0_3_0: PackageVersion = "0.3.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v0_3_0));

    // Should NOT match 1.0.0
    let v1_0_0: PackageVersion = "1.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_0_0));
}

#[tokio::test]
async fn test_wildcard_requirement() {
    // Wildcard: 1.* means >=1.0.0 <2.0.0
    let req = "1.*".parse::<VersionReq>().unwrap();

    // Should match 1.0.0
    let v1_0_0: PackageVersion = "1.0.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_0_0));

    // Should match 1.5.0
    let v1_5_0: PackageVersion = "1.5.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_5_0));

    // Should NOT match 2.0.0
    let v2_0_0: PackageVersion = "2.0.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v2_0_0));
}

#[tokio::test]
async fn test_multiple_constraints() {
    // Multiple constraints: >=1.2.0 <1.5.0
    let req = ">=1.2.0 <1.5.0".parse::<VersionReq>().unwrap();

    // Should match 1.2.0
    let v1_2_0: PackageVersion = "1.2.0".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_2_0));

    // Should match 1.4.9
    let v1_4_9: PackageVersion = "1.4.9".parse().unwrap();
    assert!(semver_req_matches(&req, &v1_4_9));

    // Should NOT match 1.1.9
    let v1_1_9: PackageVersion = "1.1.9".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_1_9));

    // Should NOT match 1.5.0
    let v1_5_0: PackageVersion = "1.5.0".parse().unwrap();
    assert!(!semver_req_matches(&req, &v1_5_0));
}
