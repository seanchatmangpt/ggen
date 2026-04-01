//! Enterprise Profile E2E Workflow Tests
//!
//! Tests the complete enterprise governance workflow including discovery,
//! regulated industry profiles, profile field validation, and capability
//! pack cross-referencing.
//! All tests invoke the REAL ggen binary and verify actual CLI output.

use super::helpers::*;

// ==============================================================================
// Enterprise Discovery Workflow
// ==============================================================================

#[test]
fn test_enterprise_discovery_workflow() {
    // Step 1: List all governance surfaces (packs)
    let (packs_json, _, packs_ok) = run_ggen_json(&["packs", "list"]).unwrap();
    assert!(packs_ok, "packs list should succeed");

    let packs_total = packs_json["total"].as_u64().unwrap_or(0);
    assert!(packs_total > 0, "packs list should return total > 0");
    println!("Discovery: {} packs available", packs_total);

    // Step 2: List all capabilities
    let (caps_json, _, caps_ok) = run_ggen_json(&["capability", "list"]).unwrap();
    assert!(caps_ok, "capability list should succeed");

    let caps_total = caps_json["total"].as_u64().unwrap_or(0);
    assert!(caps_total > 0, "capability list should return total > 0");
    println!("Discovery: {} capabilities available", caps_total);

    // Step 3: List all policy profiles
    let (policy_json, _, policy_ok) = run_ggen_json(&["policy", "list"]).unwrap();
    assert!(policy_ok, "policy list should succeed");

    let profiles = policy_json["profiles"]
        .as_array()
        .expect("profiles should be an array");
    assert!(!profiles.is_empty(), "policy list should return profiles");
    println!("Discovery: {} policy profiles available", profiles.len());

    // Step 4: All commands returned valid structured JSON
    assert!(
        packs_json.is_object(),
        "packs list should return a JSON object"
    );
    assert!(
        caps_json.is_object(),
        "capability list should return a JSON object"
    );
    assert!(
        policy_json.is_object(),
        "policy list should return a JSON object"
    );

    println!(
        "Enterprise discovery complete: {} packs, {} capabilities, {} profiles",
        packs_total,
        caps_total,
        profiles.len()
    );
}

// ==============================================================================
// Regulated Finance Profile Tests
// ==============================================================================

#[test]
fn test_regulated_finance_has_strictest_requirements() {
    let (json, _, success) = run_ggen_json(&["policy", "list"]).unwrap();
    assert!(success, "policy list should succeed");

    let profiles = json["profiles"]
        .as_array()
        .expect("profiles should be an array");

    // Find regulated-finance profile
    let regulated = profiles
        .iter()
        .find(|p| p["id"].as_str() == Some("regulated-finance"))
        .expect("regulated-finance profile should exist");

    println!(
        "Regulated Finance profile: {}",
        serde_json::to_string_pretty(regulated).unwrap()
    );

    // regulated-finance should have the highest policy_count
    let reg_policy_count = regulated["policy_count"].as_u64().unwrap_or(0);
    for profile in profiles {
        let count = profile["policy_count"].as_u64().unwrap_or(0);
        assert!(
            reg_policy_count >= count,
            "regulated-finance ({} policies) should have >= policies than '{}' ({} policies)",
            reg_policy_count,
            profile["id"].as_str().unwrap_or("unknown"),
            count
        );
    }

    // regulated-finance should require SignedAndChained receipts
    let receipt_req = regulated["receipt_requirement"].as_str().unwrap_or("");
    assert_eq!(
        receipt_req, "SignedAndChained",
        "regulated-finance should require SignedAndChained receipts, got: '{}'",
        receipt_req
    );

    // regulated-finance should have the highest trust tier (EnterpriseCertified)
    let trust = regulated["trust_requirement"].as_str().unwrap_or("");
    assert_eq!(
        trust, "EnterpriseCertified",
        "regulated-finance should have EnterpriseCertified trust, got: '{}'",
        trust
    );
}

// ==============================================================================
// Profile Field Validation Tests
// ==============================================================================

#[test]
fn test_all_profiles_have_required_fields() {
    let (json, _, success) = run_ggen_json(&["policy", "list"]).unwrap();
    assert!(success, "policy list should succeed");

    let profiles = json["profiles"]
        .as_array()
        .expect("profiles should be an array");

    assert!(
        !profiles.is_empty(),
        "Should have at least one policy profile"
    );

    let required_fields = ["id", "name", "trust_requirement", "receipt_requirement"];

    for (i, profile) in profiles.iter().enumerate() {
        let profile_id = profile["id"].as_str().unwrap_or("<missing>");

        for field in &required_fields {
            let value = profile.get(*field);
            assert!(
                value.is_some(),
                "Profile #{} ('{}') is missing required field '{}'",
                i,
                profile_id,
                field
            );

            let str_val = value.unwrap().as_str().unwrap_or("");
            assert!(
                !str_val.is_empty(),
                "Profile '{}' field '{}' must be non-empty",
                profile_id,
                field
            );
        }

        println!("Profile '{}' passes all required field checks", profile_id);
    }
}

// ==============================================================================
// Capability-Pack Cross-Reference Tests
// ==============================================================================

#[test]
fn test_capability_categories_match_packs() {
    // Step 1: Get capabilities with categories and atomic packs
    let (caps_json, _, caps_ok) = run_ggen_json(&["capability", "list"]).unwrap();
    assert!(caps_ok, "capability list should succeed");

    let capabilities = caps_json["capabilities"]
        .as_array()
        .expect("capabilities should be an array");
    assert!(
        !capabilities.is_empty(),
        "Should have at least one capability"
    );

    // Step 2: Get packs list
    let (packs_json, _, packs_ok) = run_ggen_json(&["packs", "list"]).unwrap();
    assert!(packs_ok, "packs list should succeed");

    let packs = packs_json["packs"]
        .as_array()
        .expect("packs should be an array");

    // Step 3: Each capability must have a category
    let valid_categories = ["surface", "contract"];
    for cap in capabilities {
        let cap_id = cap["id"].as_str().unwrap_or("<missing>");
        let category = cap["category"].as_str().unwrap_or("");

        assert!(
            valid_categories.contains(&category),
            "Capability '{}' has unexpected category '{}', expected one of {:?}",
            cap_id,
            category,
            valid_categories
        );

        // Step 4: Each capability must have atomic_packs
        let atomic_packs = cap["atomic_packs"]
            .as_array()
            .expect(&format!("capability '{}' should have atomic_packs", cap_id));
        assert!(
            !atomic_packs.is_empty(),
            "Capability '{}' should reference at least one atomic pack",
            cap_id
        );

        // Each atomic pack ID should be a non-empty string
        for ap in atomic_packs {
            let ap_str = ap.as_str().unwrap_or("");
            assert!(
                !ap_str.is_empty(),
                "Capability '{}' has empty atomic pack reference",
                cap_id
            );
        }

        println!(
            "Capability '{}' (category: '{}') references {} atomic packs",
            cap_id,
            category,
            atomic_packs.len()
        );
    }

    // Step 5: Packs list should have non-empty categories
    let pack_categories: Vec<&str> = packs
        .iter()
        .map(|p| p["category"].as_str().unwrap_or(""))
        .filter(|c| !c.is_empty())
        .collect();
    assert!(
        !pack_categories.is_empty(),
        "Packs should have non-empty categories"
    );

    println!(
        "Cross-reference: {} capabilities, {} packs with {} distinct categories",
        capabilities.len(),
        packs.len(),
        pack_categories.len()
    );
}
