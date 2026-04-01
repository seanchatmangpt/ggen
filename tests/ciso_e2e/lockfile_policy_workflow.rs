//! Lockfile and Policy E2E Workflow Tests
//!
//! Tests multi-command workflows for lockfile management and policy enforcement.
//! All tests invoke the REAL ggen binary and verify actual CLI output.

use super::helpers::*;

// Known pack IDs from the real marketplace output
const PACK_DEVOPS: &str = "devops-automation";
const PACK_WEB_FULLSTACK: &str = "web-fullstack";
const PACK_DATA_SCIENCE: &str = "data-science-toolkit";

// ==============================================================================
// Packs Install Tests
// ==============================================================================

#[test]
fn test_packs_install_populates_state() {
    let (json, _output, success) = run_ggen_json(&[
        "packs",
        "install",
        "--pack_id",
        PACK_DEVOPS,
    ]).unwrap();

    println!("packs install output JSON: {}", serde_json::to_string_pretty(&json).unwrap());

    // Command should succeed
    assert!(success, "packs install should succeed");

    // Response must contain pack_id
    assert_nonempty_string(&json, "pack_id");
    assert_eq!(json["pack_id"].as_str().unwrap(), PACK_DEVOPS);

    // Response must contain status
    assert_nonempty_string(&json, "status");
    assert_eq!(json["status"].as_str().unwrap(), "installed");

    // Response must reference lockfile or digest
    let message = json["message"].as_str().unwrap_or("");
    assert!(
        message.contains("lockfile") || message.contains("digest"),
        "Install message should reference lockfile or digest, got: {}",
        message
    );

}

#[test]
fn test_packs_install_then_list_workflow() {
    // Step 1: Install a pack
    let (install_json, _, install_ok) = run_ggen_json(&[
        "packs",
        "install",
        "--pack_id",
        PACK_WEB_FULLSTACK,
    ]).unwrap();
    assert!(install_ok, "packs install should succeed");
    println!("Installed pack: {}", install_json["pack_id"]);

    // Step 2: List all packs
    let (list_json, _, list_ok) = run_ggen_json(&["packs", "list"]).unwrap();
    assert!(list_ok, "packs list should succeed");

    // Step 3: Verify the installed pack appears in the list
    let packs = list_json["packs"].as_array().expect("packs should be an array");
    let found = packs.iter().any(|p| {
        p["id"].as_str() == Some(PACK_WEB_FULLSTACK)
    });
    assert!(found, "Installed pack '{}' should appear in packs list", PACK_WEB_FULLSTACK);

    // Step 4: Verify total count is positive
    assert_positive_number(&list_json, "total");
}

// ==============================================================================
// Policy Tests
// ==============================================================================

#[test]
fn test_policy_list_shows_trust_requirements() {
    let (json, _, success) = run_ggen_json(&["policy", "list"]).unwrap();
    assert!(success, "policy list should succeed");

    let profiles = json["profiles"]
        .as_array()
        .expect("profiles should be an array");

    assert!(
        !profiles.is_empty(),
        "policy list should return at least one profile"
    );

    // Each profile must have a trust_requirement field
    for profile in profiles {
        assert_nonempty_string(profile, "id");
        assert_nonempty_string(profile, "trust_requirement");

        println!(
            "Profile '{}' has trust_requirement='{}'",
            profile["id"].as_str().unwrap(),
            profile["trust_requirement"].as_str().unwrap()
        );
    }
}

#[test]
fn test_policy_enterprise_strict_stricter_than_development() {
    let (json, _, success) = run_ggen_json(&["policy", "list"]).unwrap();
    assert!(success, "policy list should succeed");

    let profiles = json["profiles"].as_array().expect("profiles should be an array");

    // Find enterprise-strict and development profiles
    let enterprise = profiles
        .iter()
        .find(|p| p["id"].as_str() == Some("enterprise-strict"))
        .expect("enterprise-strict profile should exist");

    let development = profiles
        .iter()
        .find(|p| p["id"].as_str() == Some("development"))
        .expect("development profile should exist");

    // Enterprise-strict should have more policies than development
    let ent_policy_count = enterprise["policy_count"].as_u64().unwrap_or(0);
    let dev_policy_count = development["policy_count"].as_u64().unwrap_or(0);

    println!(
        "Enterprise policy_count: {}, Development policy_count: {}",
        ent_policy_count, dev_policy_count
    );

    assert!(
        ent_policy_count > dev_policy_count,
        "enterprise-strict ({} policies) should have more policies than development ({} policies)",
        ent_policy_count,
        dev_policy_count
    );

    // Enterprise-strict should have different (higher) trust requirement
    let ent_trust = enterprise["trust_requirement"].as_str().unwrap_or("");
    let dev_trust = development["trust_requirement"].as_str().unwrap_or("");

    assert_ne!(
        ent_trust, dev_trust,
        "enterprise-strict and development should have different trust_requirement"
    );
}

// ==============================================================================
// Capability Tests
// ==============================================================================

#[test]
fn test_capability_enable_with_enterprise_profile() {
    let (json, output, success) = run_ggen_json(&[
        "capability",
        "enable",
        "--surface",
        "mcp",
        "--projection",
        "rust",
        "--profile",
        "enterprise-strict",
    ]).unwrap();

    println!("capability enable output:\n{}", output);

    // Command should succeed (enables with receipt)
    assert!(success, "capability enable should succeed");

    // Response must reference the enterprise-strict profile
    let profile = json["profile"].as_str().unwrap_or("");
    assert_eq!(
        profile, "enterprise-strict",
        "Response should reference enterprise-strict profile, got: '{}'",
        profile
    );

    // Response should indicate enabled status
    let status = json["status"].as_str().unwrap_or("");
    assert_eq!(status, "enabled", "Status should be 'enabled', got: '{}'", status);

    // Response should list atomic packs
    let atomic_packs = json["atomic_packs"]
        .as_array()
        .expect("atomic_packs should be an array");
    assert!(
        !atomic_packs.is_empty(),
        "Should resolve at least one atomic pack"
    );

    println!("Resolved {} atomic packs for enterprise-strict MCP", atomic_packs.len());
}

// ==============================================================================
// Packs Compose Tests
// ==============================================================================

#[test]
fn test_packs_compose_shows_compatibility() {
    let (json, output, success) = run_ggen_json(&[
        "packs",
        "compose",
        "--pack_ids",
        &format!("{},{}", PACK_DEVOPS, PACK_DATA_SCIENCE),
    ]).unwrap();

    println!("packs compose output:\n{}", output);

    // Compose should succeed for known packs
    assert!(success, "packs compose should succeed for known pack IDs");

    // Response must have compatible field
    let compatible = json["compatible"].as_bool().unwrap_or(false);
    assert!(compatible, "Known packs should be compatible");

    // Response must list pack_ids
    let pack_ids = json["pack_ids"]
        .as_array()
        .expect("pack_ids should be an array");
    assert_eq!(pack_ids.len(), 2, "Should compose exactly 2 packs");

    // Response must have atomic_packs
    let atomic = json["atomic_packs"]
        .as_array()
        .expect("atomic_packs should be an array");
    assert!(!atomic.is_empty(), "Should have at least one atomic pack");
}

// ==============================================================================
// Packs Validate Tests
// ==============================================================================

#[test]
fn test_packs_validate_checks_pack_structure() {
    let (json, output, success) = run_ggen_json(&[
        "packs",
        "validate",
        "--pack_id",
        PACK_DEVOPS,
    ]).unwrap();

    println!("packs validate output:\n{}", output);

    // Validate should succeed for a known pack
    assert!(success, "packs validate should succeed for known pack");

    // Response must have is_valid field
    let is_valid = json["is_valid"].as_bool().unwrap_or(false);
    assert!(is_valid, "Known pack '{}' should be valid", PACK_DEVOPS);

    // Response must reference the pack_id
    assert_nonempty_string(&json, "pack_id");
    assert_eq!(json["pack_id"].as_str().unwrap(), PACK_DEVOPS);

    // Errors should be empty for a valid pack
    let errors = json["errors"].as_array().expect("errors should be an array");
    assert!(
        errors.is_empty(),
        "Valid pack should have no errors, got: {:?}",
        errors
    );
}
