//! Policy Validation Workflow E2E Tests
//!
//! Tests policy profile listing, showing, and validation.
//! Each test calls 2+ real CLI commands in sequence.

use super::helpers::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a minimal `.ggen/packs.lock` in the given workspace directory.
///
/// The lockfile contains a single entry so that `ggen policy validate` can
/// load pack contexts without immediately erroring with "No project found".
/// The referenced pack does not need to exist in the cache -- `load_pack_metadata`
/// returns defaults when no `package.toml` / `metadata.json` is found.
fn setup_minimal_lockfile(workspace: &std::path::Path) {
    let ggen_dir = workspace.join(".ggen");
    std::fs::create_dir_all(&ggen_dir).expect("failed to create .ggen dir");

    let lockfile_content = r#"
updated_at = "2026-04-01T00:00:00Z"
ggen_version = "6.0.1"

[packs.e2e-test-pack]
version = "1.0.0"
installed_at = "2026-04-01T00:00:00Z"

[packs.e2e-test-pack.source]
type = "Local"
path = "/tmp/nonexistent-pack"
"#;

    std::fs::write(ggen_dir.join("packs.lock"), lockfile_content.trim_start())
        .expect("failed to write packs.lock");
}

#[test]
fn test_policy_list_returns_all_profiles() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen policy list`
    println!("[CISO] Running: ggen policy list");
    let (stdout, exit_code) = run_ggen(&["policy", "list"], workspace_path(&temp)).unwrap();
    println!("[CISO] policy list exit code: {}", exit_code);

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert profiles structure
    let profiles = json["profiles"]
        .as_array()
        .expect("policy list should have profiles array");
    let total = json["total"]
        .as_u64()
        .expect("policy list should have total");
    assert_eq!(profiles.len(), total as usize);
    assert!(
        total >= 3,
        "Expected at least 3 policy profiles, got {}",
        total
    );

    // Step 4: Collect profile IDs
    let profile_ids: Vec<&str> = profiles.iter().filter_map(|p| p["id"].as_str()).collect();
    println!("[CISO] Available profiles: {:?}", profile_ids);

    // Step 5: Assert expected profiles exist
    assert!(
        profile_ids.contains(&"enterprise-strict"),
        "Should have 'enterprise-strict' profile"
    );
    assert!(
        profile_ids.contains(&"regulated-finance"),
        "Should have 'regulated-finance' profile"
    );
    assert!(
        profile_ids.contains(&"development"),
        "Should have 'development' profile"
    );

    // Step 6: Assert each profile has required fields
    for profile in profiles {
        assert!(profile["id"].is_string(), "profile should have id");
        assert!(profile["name"].is_string(), "profile should have name");
        assert!(
            profile["description"].is_string(),
            "profile should have description"
        );
        assert!(
            profile["policy_count"].is_number(),
            "profile should have policy_count"
        );
        assert!(
            profile["receipt_requirement"].is_string(),
            "profile should have receipt_requirement"
        );
        assert!(
            profile["trust_requirement"].is_string(),
            "profile should have trust_requirement"
        );
    }

    println!("[CISO] PASS: policy list returned {} profiles", total);
}

#[test]
fn test_policy_show_enterprise_strict() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen policy list` to verify enterprise-strict exists
    println!("[CISO] Step 1: ggen policy list");
    let (list_output, _) = run_ggen(&["policy", "list"], workspace_path(&temp)).unwrap();
    let list_json = extract_json_from_output(&list_output).unwrap();
    let profiles = list_json["profiles"].as_array().unwrap();
    let enterprise = profiles
        .iter()
        .find(|p| p["id"].as_str() == Some("enterprise-strict"));
    assert!(
        enterprise.is_some(),
        "enterprise-strict profile should exist"
    );

    // Step 2: Run `ggen policy show --profile_id enterprise-strict`
    println!("[CISO] Step 2: ggen policy show --profile_id enterprise-strict");
    let (show_output, _) = run_ggen(
        &["policy", "show", "--profile_id", "enterprise-strict"],
        workspace_path(&temp),
    )
    .unwrap();
    let show_json = extract_json_from_output(&show_output).unwrap();

    // Step 3: Assert detailed profile structure
    assert_eq!(
        show_json["profile_id"].as_str(),
        Some("enterprise-strict"),
        "profile_id should match"
    );
    assert_eq!(
        show_json["receipt_requirement"].as_str(),
        Some("Signed"),
        "enterprise-strict should require Signed receipts"
    );
    assert_eq!(
        show_json["trust_requirement"].as_str(),
        Some("EnterpriseApproved"),
        "enterprise-strict should require EnterpriseApproved trust"
    );

    // Step 4: Assert policies array exists with correct count
    let policies = show_json["policies"]
        .as_array()
        .expect("show should have policies");
    let list_policy_count = enterprise.unwrap()["policy_count"].as_u64().unwrap();
    assert_eq!(
        policies.len(),
        list_policy_count as usize,
        "show policy count should match list policy_count"
    );
    assert!(
        list_policy_count > 0,
        "enterprise-strict should have at least 1 policy"
    );

    // Step 5: Assert each policy has id, name, description
    for policy in policies {
        assert!(policy["id"].is_string(), "policy should have id");
        assert!(policy["name"].is_string(), "policy should have name");
        assert!(
            policy["description"].is_string(),
            "policy should have description"
        );
    }

    println!(
        "[CISO] PASS: policy show enterprise-strict has {} policies",
        policies.len()
    );
}

#[test]
fn test_policy_validate_enterprise_strict() {
    let temp = create_temp_workspace().unwrap();

    // Step 0: Create minimal .ggen/packs.lock so validate can load pack contexts
    setup_minimal_lockfile(workspace_path(&temp));

    // Step 1: Run `ggen policy validate --profile enterprise-strict`
    println!("[CISO] Running: ggen policy validate --profile enterprise-strict");
    let (stdout, exit_code) = run_ggen(
        &["policy", "validate", "--profile", "enterprise-strict"],
        workspace_path(&temp),
    )
    .unwrap();
    println!(
        "[CISO] policy validate enterprise-strict exit code: {}",
        exit_code
    );

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert validation result structure
    assert!(
        json["profile_id"].is_string(),
        "validation should have profile_id"
    );
    assert!(
        json["passed"].is_boolean(),
        "validation should have passed boolean"
    );
    assert!(
        json["policies_checked"].is_number(),
        "validation should have policies_checked count"
    );
    assert!(
        json["violation_count"].is_number(),
        "validation should have violation_count"
    );
    assert!(
        json["violations"].is_array(),
        "validation should have violations array"
    );

    // Step 4: Enterprise-strict is expected to have violations (unless all packs comply)
    // We just verify the structure is correct, not the specific pass/fail outcome.
    let passed = json["passed"].as_bool().unwrap();
    let violation_count = json["violation_count"].as_u64().unwrap();
    let policies_checked = json["policies_checked"].as_u64().unwrap();
    let violations = json["violations"].as_array().unwrap();

    assert_eq!(
        violations.len(),
        violation_count as usize,
        "violations array length should match violation_count"
    );

    if passed {
        assert_eq!(violation_count, 0, "passed=true should mean 0 violations");
    }

    println!(
        "[CISO] PASS: policy validate enterprise-strict: passed={}, policies_checked={}, violations={}",
        passed, policies_checked, violation_count
    );
}

#[test]
fn test_policy_validate_development_allows_experimental() {
    let temp = create_temp_workspace().unwrap();

    // Step 0: Create minimal .ggen/packs.lock so validate can load pack contexts
    setup_minimal_lockfile(workspace_path(&temp));

    // Step 1: Run `ggen policy validate --profile development`
    println!("[CISO] Running: ggen policy validate --profile development");
    let (stdout, exit_code) = run_ggen(
        &["policy", "validate", "--profile", "development"],
        workspace_path(&temp),
    )
    .unwrap();
    println!(
        "[CISO] policy validate development exit code: {}",
        exit_code
    );

    // Step 2: Parse JSON output
    let json = extract_json_from_output(&stdout).unwrap();

    // Step 3: Assert validation result structure
    assert_eq!(
        json["profile_id"].as_str(),
        Some("development"),
        "profile_id should be 'development'"
    );
    assert!(
        json["passed"].is_boolean(),
        "validation should have passed boolean"
    );
    assert!(
        json["policies_checked"].is_number(),
        "validation should have policies_checked"
    );

    // Step 4: Development profile should be more permissive (0 policies checked)
    let policies_checked = json["policies_checked"].as_u64().unwrap();
    assert_eq!(
        policies_checked, 0,
        "development profile should check 0 policies (most permissive)"
    );

    // Step 5: Development should pass with 0 violations
    let passed = json["passed"].as_bool().unwrap();
    let violation_count = json["violation_count"].as_u64().unwrap();
    assert!(passed, "development profile validation should pass");
    assert_eq!(
        violation_count, 0,
        "development profile should have 0 violations"
    );

    println!(
        "[CISO] PASS: policy validate development: passed={}, policies_checked={} (permissive mode)",
        passed, policies_checked
    );
}
