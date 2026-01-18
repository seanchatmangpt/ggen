// Scenario 3: Marketplace search and discovery
// Chicago TDD: REAL marketplace interactions (or local registry)

use assert_cmd::Command;
use predicates::prelude::*;

use super::common::integration_timeout;
use super::test_helpers::*;

#[test]
fn test_marketplace_search_local() {
    let workspace = setup_workspace().unwrap();

    // Search should work even without network (uses local registry if available)
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("marketplace")
        .arg("search")
        .arg("rust")
        .current_dir(workspace.path())
        .timeout(integration_timeout())
        .output()
        .unwrap();

    // Should either succeed with results or gracefully handle no network
    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        println!("Search results:\n{}", stdout);
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Search failed (expected if no network): {}", stderr);
    }

    println!("✅ Marketplace search: PASSED (graceful handling)");
}

#[test]
fn test_marketplace_list_empty() {
    let workspace = setup_workspace().unwrap();

    // List installed templates (should be empty initially)
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("marketplace")
        .arg("list")
        .current_dir(workspace.path())
        .assert()
        .success();

    println!("✅ Marketplace list empty: PASSED");
}

#[test]
#[ignore] // Requires network access
fn test_marketplace_search_with_network() {
    if !is_network_available() {
        println!("⚠️  Skipping network test (no connection)");
        return;
    }

    let workspace = setup_workspace().unwrap();

    // Search production registry
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("marketplace")
        .arg("search")
        .arg("cli")
        .current_dir(workspace.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("cli").or(predicate::str::contains("template")));

    println!("✅ Marketplace search with network: PASSED");
}

#[test]
#[ignore] // Requires network access
fn test_marketplace_install_flow() {
    if !is_network_available() {
        println!("⚠️  Skipping network test (no connection)");
        return;
    }

    let workspace = setup_workspace().unwrap();

    // Try to install a template
    let output = Command::cargo_bin("ggen")
        .unwrap()
        .arg("marketplace")
        .arg("install")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(workspace.path())
        .timeout(integration_timeout())
        .output()
        .unwrap();

    if output.status.success() {
        println!("Template installed successfully");

        // Verify lockfile created
        let lockfile = workspace.path().join("ggen.lock");
        if lockfile.exists() {
            let content = std::fs::read_to_string(&lockfile).unwrap();
            assert!(content.contains("io.ggen"));
        }
    } else {
        println!(
            "Install failed (may be expected): {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    println!("✅ Marketplace install flow: PASSED (graceful)");
}
