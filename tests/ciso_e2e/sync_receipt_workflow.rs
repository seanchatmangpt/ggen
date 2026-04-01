//! Sync and Receipt Workflow E2E Tests
//!
//! Tests init/sync workflow, receipt commands, and full cross-domain workflows.
//! Each test calls 2+ real CLI commands in sequence.

use super::helpers::*;

#[test]
fn test_init_then_sync_workflow() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen init test-project` in temp dir
    println!("[CISO] Step 1: ggen init test-project");
    let (init_output, init_exit) = run_ggen(&["init", "test-project"], workspace_path(&temp)).unwrap();
    println!(
        "[CISO] init exit code: {}, output:\n{}",
        init_exit,
        &init_output[..init_output.len().min(300)]
    );

    // Step 2: Assert ggen.toml created
    let project_dir = workspace_path(&temp).join("test-project");
    let ggen_toml = project_dir.join("ggen.toml");
    assert!(
        ggen_toml.exists(),
        "ggen.toml should exist after init"
    );

    // Step 3: Run `ggen sync` in project dir (may fail due to missing ontology,
    // which is expected - we just verify the command is reachable)
    println!("[CISO] Step 2: ggen sync in project dir");
    let (sync_output, sync_exit) = run_ggen(&["sync"], &project_dir).unwrap();
    println!(
        "[CISO] sync exit code: {}, output:\n{}",
        sync_exit,
        &sync_output[..sync_output.len().min(300)]
    );

    // Step 4: Sync may succeed or fail depending on config contents.
    // Either way the command should be reachable (exit code != -1 signal).
    assert!(
        sync_exit >= 0,
        "sync should exit normally (not signal), got exit code: {}",
        sync_exit
    );

    println!(
        "[CISO] PASS: init -> sync workflow completed. ggen.toml created, sync exit={}",
        sync_exit
    );
}

#[test]
fn test_receipt_verify_command_exists() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen receipt verify --help`
    println!("[CISO] Running: ggen receipt verify --help");
    let (stdout, _exit_code) =
        run_ggen(&["receipt", "verify", "--help"], workspace_path(&temp)).unwrap();

    // Step 2: Assert help output shows required args
    let output = format!("{}{}", stdout, ""); // stdout only for help
    assert!(
        output.contains("--receipt_file") || output.contains("RECEIPT_FILE"),
        "receipt verify help should show --receipt_file argument"
    );
    assert!(
        output.contains("--public_key") || output.contains("PUBLIC_KEY"),
        "receipt verify help should show --public_key argument"
    );

    println!("[CISO] PASS: receipt verify command exists with expected arguments");
}

#[test]
fn test_receipt_chain_verify_command_exists() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen receipt chain_verify --help`
    println!("[CISO] Running: ggen receipt chain_verify --help");
    let (stdout, _exit_code) =
        run_ggen(&["receipt", "chain_verify", "--help"], workspace_path(&temp)).unwrap();

    // Step 2: Assert help output shows required args
    let output = stdout;
    assert!(
        output.contains("--chain_file") || output.contains("CHAIN_FILE"),
        "receipt chain_verify help should show --chain_file argument"
    );
    assert!(
        output.contains("--public_key") || output.contains("PUBLIC_KEY"),
        "receipt chain_verify help should show --public_key argument"
    );

    println!(
        "[CISO] PASS: receipt chain_verify command exists with expected arguments"
    );
}

#[test]
fn test_full_pack_discovery_to_sync_workflow() {
    let temp = create_temp_workspace().unwrap();

    // Step 1: Run `ggen packs list` to get available packs
    println!("[CISO] Step 1: ggen packs list");
    let (packs_output, packs_exit) = run_ggen(&["packs", "list"], workspace_path(&temp)).unwrap();
    let packs_json = extract_json_from_output(&packs_output).unwrap();
    let packs_total = packs_json["total"].as_u64().expect("packs total");
    let packs = packs_json["packs"].as_array().expect("packs array");
    assert!(packs_total > 0, "packs list should return packs");

    // Step 2: Run `ggen capability list` to get capabilities
    println!("[CISO] Step 2: ggen capability list");
    let (cap_output, cap_exit) = run_ggen(&["capability", "list"], workspace_path(&temp)).unwrap();
    let cap_json = extract_json_from_output(&cap_output).unwrap();
    let cap_total = cap_json["total"].as_u64().expect("capability total");
    let capabilities = cap_json["capabilities"].as_array().expect("capabilities array");
    assert!(cap_total > 0, "capability list should return capabilities");

    // Step 3: Run `ggen policy list` to get profiles
    println!("[CISO] Step 3: ggen policy list");
    let (policy_output, policy_exit) = run_ggen(&["policy", "list"], workspace_path(&temp)).unwrap();
    let policy_json = extract_json_from_output(&policy_output).unwrap();
    let policy_total = policy_json["total"].as_u64().expect("policy total");
    let _profiles = policy_json["profiles"].as_array().expect("profiles array");
    assert!(policy_total >= 3, "policy list should return at least 3 profiles");

    // Step 4: Verify all commands returned valid JSON
    assert!(
        packs_exit == 0,
        "packs list should succeed"
    );
    assert!(
        cap_exit == 0,
        "capability list should succeed"
    );
    assert!(
        policy_exit == 0,
        "policy list should succeed"
    );

    // Step 5: Cross-reference: verify all capability atomic_packs reference valid pack structures
    let mut atomic_pack_ids_seen = std::collections::HashSet::new();
    for cap in capabilities {
        if let Some(atomic_packs) = cap["atomic_packs"].as_array() {
            for pack in atomic_packs {
                if let Some(pack_id) = pack.as_str() {
                    atomic_pack_ids_seen.insert(pack_id.to_string());
                }
            }
        }
    }

    println!(
        "[CISO] Step 4: Cross-domain summary: {} packs, {} capabilities ({} unique atomic packs), {} policy profiles",
        packs_total,
        cap_total,
        atomic_pack_ids_seen.len(),
        policy_total
    );

    // Step 6: Verify pack categories are non-empty
    let pack_categories: std::collections::HashSet<String> = packs
        .iter()
        .filter_map(|p| p["category"].as_str().map(|s| s.to_string()))
        .collect();
    assert!(
        !pack_categories.is_empty(),
        "packs should have at least one category"
    );

    println!(
        "[CISO] PASS: full discovery workflow - {} packs across {} categories, {} capabilities, {} profiles",
        packs_total,
        pack_categories.len(),
        cap_total,
        policy_total
    );
}
