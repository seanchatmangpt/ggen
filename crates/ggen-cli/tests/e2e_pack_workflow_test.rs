#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! End-to-End CLI Workflow Tests for Pack Management
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution (via assert_cmd)
//! - REAL file system operations (via tempfile)
//! - State-based verification (files created, receipts generated, lockfiles updated)
//! - NO mocking of CLI commands or file system
//!
//! **Test Coverage**:
//! 1. Pack installation workflow (install → lockfile → receipt)
//! 2. Capability enable workflow (enable → lockfile → atomic packs)
//! 3. Lockfile creation and persistence
//! 4. Receipt generation and verification
//! 5. Policy validation workflow
//! 6. End-to-end integration (install → enable → validate → receipt)
//!
//! **Verification Methods**:
//! - CLI exit codes (success/failure)
//! - JSON output validation
//! - File system state (directories, files, content)
//! - Receipt signature verification
//! - Lockfile consistency
//!
//! Fixture note (2026-08, real-CLI verification pass): the original fixtures installed a
//! placeholder pack id ("surface-mcp"/"projection-rust") against whatever pack registry the
//! *running machine* happens to have at `~/.ggen/packs/*.toml` -- on this repo's real machine
//! that registry currently has exactly 2 entries (`framework-lsp`, `tower-lsp-max`), neither of
//! which is `surface-mcp`/`projection-rust`. `ggen pack add <unregistered-id>` does not fail
//! (exit 0, `status: "not_found"`, no lockfile/receipt written -- confirmed live) -- so every
//! test that then asserted on lockfile/receipt state was really asserting on a no-op. Fixed via
//! `PackFixture` below (hermetic `$HOME` + `GGEN_PACKS_DIR`, same pattern as
//! `crates/ggen-cli/tests/proof_pack_test.rs::World`) so `pack add` installs a REAL fixture pack
//! regardless of what is or isn't registered on the machine running the suite. A few tests also
//! called CLI shapes that no longer exist (`receipt info`, `receipt verify --receipt_file`,
//! `pack show --pack_id`) or asserted `.success()` on commands that correctly fail closed
//! (`policy validate` with violations, `policy validate`/`pack show`/`receipt verify` on missing
//! input) -- see each test's own comment for the specific live-verified fix.

#![cfg(feature = "integration")]

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Utilities
// ============================================================================

/// Create a ggen CLI command pointing to the cargo binary
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

/// Create a minimal test pack metadata structure
fn create_test_pack_metadata(pack_dir: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let metadata = r#"{
        "id": "test-pack",
        "name": "Test Pack",
        "version": "1.0.0",
        "description": "A test pack for E2E testing",
        "category": "test",
        "packages": [],
        "templates": [],
        "trust_tier": "trusted",
        "signature": null
    }"#;

    fs::create_dir_all(pack_dir)?;
    let metadata_path = pack_dir.join("metadata.json");
    let mut file = File::create(metadata_path)?;
    file.write_all(metadata.as_bytes())?;
    Ok(())
}

/// Create a test lockfile with a sample pack
fn create_test_lockfile(lockfile_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let lockfile_content = r#"{
        "version": "6.0.1",
        "packs": {
            "surface-mcp": {
                "version": "1.0.0",
                "source": {
                    "type": "Registry",
                    "url": "https://registry.ggen.io"
                },
                "integrity": null,
                "installed_at": "2024-01-01T00:00:00Z",
                "dependencies": []
            }
        },
        "updated_at": "2024-01-01T00:00:00Z",
        "ggen_version": "6.0.1"
    }"#;

    if let Some(parent) = lockfile_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut file = File::create(lockfile_path)?;
    file.write_all(lockfile_content.as_bytes())?;
    Ok(())
}

/// Create a test receipt for verification
fn create_test_receipt(receipt_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let receipt_content = r#"{
        "operation_id": "test-op-123",
        "operation_type": "pack_install",
        "timestamp": "2024-01-01T00:00:00Z",
        "input_hashes": ["abc123"],
        "output_hashes": ["def456"],
        "signature": "test_signature"
    }"#;

    if let Some(parent) = receipt_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut file = File::create(receipt_path)?;
    file.write_all(receipt_content.as_bytes())?;
    Ok(())
}

/// Parse JSON output from CLI commands
fn parse_json(output: &str) -> Result<Value, Box<dyn std::error::Error>> {
    Ok(serde_json::from_str(output)?)
}

/// Verify lockfile exists and has valid structure
fn verify_lockfile_structure(lockfile_path: &Path) -> Result<bool, Box<dyn std::error::Error>> {
    if !lockfile_path.exists() {
        return Ok(false);
    }

    let content = fs::read_to_string(lockfile_path)?;
    let json: Value = serde_json::from_str(&content)?;

    // Verify required fields
    Ok(json.get("packs").is_some()
        && json.get("updated_at").is_some()
        && json.get("ggen_version").is_some())
}

/// Count packs in lockfile
fn count_lockfile_packs(lockfile_path: &Path) -> Result<usize, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(lockfile_path)?;
    let json: Value = serde_json::from_str(&content)?;

    if let Some(packs) = json.get("packs") {
        if let Some(obj) = packs.as_object() {
            return Ok(obj.len());
        }
    }

    Ok(0)
}

/// A hermetic pack-registry + install-catalog root for `pack add`/`pack list`/`pack show`.
///
/// `install_pack_by_id` (`crates/ggen-marketplace/src/marketplace/install.rs`) installs into
/// `$HOME/.ggen/packs/<id>` -- a durable, machine-global catalog by design (see that file's own
/// doc comments distinguishing it from the transient `GGEN_PACK_CACHE_DIR` download cache) -- and
/// refuses a second install at the same path unless `force` is set. Without a per-test `$HOME`
/// override, any test calling `pack add <id>` collides with whatever the developer's real
/// `~/.ggen/packs/<id>` already contains from a prior run. `GGEN_PACKS_DIR` similarly overrides
/// which pack TOML files the registry lookup (`ggen_marketplace::packs_registry::metadata`) sees,
/// so tests do not depend on which packs happen to be registered on the machine running them.
/// Mirrors the `World` pattern already established in `crates/ggen-cli/tests/proof_pack_test.rs`.
struct PackFixture {
    home: TempDir,
    registry: TempDir,
}

impl PackFixture {
    fn new() -> Self {
        PackFixture {
            home: TempDir::new().expect("home tempdir"),
            registry: TempDir::new().expect("registry tempdir"),
        }
    }

    /// Write a real, valid pack TOML into the registry so `pack add <id>` (and `list`/`show`)
    /// find it. Field shape mirrors `crates/ggen-cli/tests/proof_pack_test.rs::World::write_pack`
    /// and the real `~/.ggen/packs/framework-lsp.toml` on disk.
    fn write_pack(&self, id: &str, version: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "{id}"
version = "{version}"
description = "E2E fixture pack for e2e_pack_workflow_test"
category = "test"
author = "e2e-test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
"#
        );
        fs::write(self.registry.path().join(format!("{id}.toml")), toml)
            .expect("write registry pack toml");
    }

    /// `ggen pack ...` wired to this fixture's hermetic `$HOME` + registry (see struct doc).
    fn ggen(&self) -> Command {
        let mut cmd = ggen();
        cmd.env("HOME", self.home.path())
            .env("GGEN_PACKS_DIR", self.registry.path())
            .env(
                "GGEN_PACK_CACHE_DIR",
                self.home.path().join(".ggen").join("packs"),
            );
        cmd
    }
}

/// Write a compliant pack-metadata cache entry so `ggen policy validate` sees a signed receipt
/// and an explicit runtime declaration for `id`@`version` -- the two real requirements
/// `enterprise-strict` enforces in practice (live-verified: its other two policies,
/// `ForbidTemplateDefaults`/`ForbidInferredCapabilities`, already pass on `PackContext::new`'s
/// defaults). `ggen-cli/src/cmds/policy.rs::load_pack_contexts_from_project` reads
/// `$GGEN_PACK_CACHE_DIR/<id>/<version>/package.toml` for the signature
/// (`ggen_marketplace::marketplace::metadata::load_pack_metadata`) and `.../pack.toml` for the
/// runtime (`load_pack_config_from_cache`). Without this, every pack in a lockfile defaults to
/// unsigned + no runtime (confirmed live: "No metadata file found ... using defaults"), which
/// `enterprise-strict` always rejects.
fn write_compliant_pack_cache(cache_root: &Path, id: &str, version: &str) {
    let dir = cache_root.join(id).join(version);
    fs::create_dir_all(&dir).expect("create pack cache dir");
    fs::write(
        dir.join("package.toml"),
        format!(
            r#"[package]
name = "{id}"
version = "{version}"

[security]
signature = "test-signature-{id}"
trust_tier = "productionready"
"#
        ),
    )
    .expect("write package.toml");
    fs::write(dir.join("pack.toml"), "[pack]\nruntime = \"axum\"\n").expect("write pack.toml");
}

// ============================================================================
// Test Suite 1: Pack Installation Workflow
// ============================================================================

#[test]
fn test_pack_install_creates_lockfile() {
    println!("🔍 E2E Test: Pack installation creates lockfile");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install a pack
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile was created
    assert!(
        lockfile_path.exists(),
        "Lockfile should be created at {}",
        lockfile_path.display()
    );

    // Assert: Lockfile has valid structure
    assert!(
        verify_lockfile_structure(&lockfile_path).unwrap(),
        "Lockfile should have valid structure"
    );

    println!("✅ Test PASSED: Lockfile created successfully");
}

#[test]
fn test_pack_install_tracks_packs() {
    println!("🔍 E2E Test: Pack installation tracks packs in lockfile");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install a pack
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile contains the installed pack
    let pack_count = count_lockfile_packs(&lockfile_path).unwrap();
    assert_eq!(
        pack_count, 1,
        "Lockfile should contain exactly 1 pack, found {}",
        pack_count
    );

    // Assert: Verify pack details
    let content = fs::read_to_string(&lockfile_path).unwrap();
    let json: Value = serde_json::from_str(&content).unwrap();

    assert!(
        json["packs"].get("surface-mcp").is_some(),
        "Lockfile should contain surface-mcp pack"
    );

    println!("✅ Test PASSED: Pack tracked in lockfile");
}

#[test]
fn test_pack_install_returns_valid_json() {
    println!("🔍 E2E Test: Pack installation returns valid JSON");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Install a pack and capture output
    let result = ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains expected fields
    assert!(json.get("pack_id").is_some(), "Should have pack_id field");
    assert!(json.get("status").is_some(), "Should have status field");
    assert_eq!(
        json["pack_id"], "surface-mcp",
        "Should report correct pack_id"
    );

    println!("✅ Test PASSED: Valid JSON output");
}

#[test]
fn test_pack_install_fails_on_unknown_pack() {
    println!("🔍 E2E Test: Pack installation fails gracefully for unknown pack");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Try to install unknown pack
    let result = ggen()
        .arg("pack")
        .arg("add")
        .arg("unknown-pack-xyz")
        .current_dir(&temp_dir)
        .assert();

    // Assert: Command fails (non-zero exit code)
    // Note: The actual behavior depends on implementation - it might succeed with an error message
    result.success(); // For now, it succeeds with an error message

    println!("✅ Test PASSED: Graceful error handling");
}

#[test]
fn test_pack_list_shows_installed_packs() {
    println!("🔍 E2E Test: Pack list shows available packs");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: List packs
    let result = ggen()
        .arg("pack")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains packs array
    assert!(json.get("packs").is_some(), "Should have packs field");
    assert!(json.get("total").is_some(), "Should have total field");
    assert!(json["packs"].is_array(), "packs should be an array");

    println!("✅ Test PASSED: Pack list works");
}

#[test]
#[ignore = "Live `pack` noun has NO `validate` verb (only `policy validate` exists, crates/ggen-cli/src/cmds/policy.rs). Intent (validate a pack by id) is impossible on the current CLI; noun migrated packs->pack but verb has no live target."]
fn test_pack_validate_checks_pack() {
    println!("🔍 E2E Test: Pack validation works");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Validate a pack
    let result = ggen()
        .arg("pack")
        .arg("validate")
        .arg("--pack_id")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains validation result
    assert_eq!(
        json["pack_id"], "surface-mcp",
        "Should validate correct pack"
    );
    assert!(
        json.get("is_valid").is_some(),
        "Should have validation status"
    );

    println!("✅ Test PASSED: Pack validation works");
}

// ============================================================================
// Test Suite 2: Capability Enable Workflow
// ============================================================================

#[test]
#[ignore = "No `capability` noun exists on the live CLI (not registered in crates/ggen-cli/src/cmds/mod.rs). Intent (enable capability -> atomic packs) is impossible on the current CLI."]
fn test_capability_enable_expands_to_atomic_packs() {
    println!("🔍 E2E Test: Capability enable expands to atomic packs");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Enable a capability
    let result = ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains atomic_packs list
    assert_eq!(json["capability"], "mcp", "Should enable mcp capability");
    assert!(
        json.get("atomic_packs").is_some(),
        "Should list atomic packs"
    );
    assert!(
        json["atomic_packs"].is_array(),
        "atomic_packs should be array"
    );

    // Assert: Atomic packs are not empty
    let atomic_packs = json["atomic_packs"].as_array().unwrap();
    assert!(
        !atomic_packs.is_empty(),
        "atomic_packs should contain at least one pack"
    );

    println!("✅ Test PASSED: Capability expanded to atomic packs");
}

#[test]
#[ignore = "No `capability` noun exists on the live CLI (crates/ggen-cli/src/cmds/mod.rs). Intent (capability enable --projection) is impossible on the current CLI."]
fn test_capability_enable_with_projection() {
    println!("🔍 E2E Test: Capability enable with projection parameter");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Enable capability with projection
    let result = ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .arg("--projection")
        .arg("rust")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Projection parameter is reflected
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    assert_eq!(
        json["projection"],
        serde_json::json!("rust"),
        "Should have projection set to rust"
    );

    // Assert: Atomic packs include projection
    let atomic_packs = json["atomic_packs"].as_array().unwrap();
    let has_projection_pack = atomic_packs
        .iter()
        .any(|pack| pack.as_str().unwrap().contains("projection"));
    assert!(
        has_projection_pack,
        "atomic_packs should include projection pack"
    );

    println!("✅ Test PASSED: Projection parameter works");
}

#[test]
#[ignore = "No `capability` noun exists on the live CLI (crates/ggen-cli/src/cmds/mod.rs). Intent (capability enable updates lockfile) is impossible on the current CLI."]
fn test_capability_enable_updates_lockfile() {
    println!("🔍 E2E Test: Capability enable updates lockfile");

    // Arrange: Create temporary directory with initial lockfile
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    create_test_lockfile(&lockfile_path).unwrap();

    // Act: Enable capability
    ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile still exists
    assert!(
        lockfile_path.exists(),
        "Lockfile should exist after capability enable"
    );

    // Assert: Lockfile is valid
    assert!(
        verify_lockfile_structure(&lockfile_path).unwrap(),
        "Lockfile should remain valid"
    );

    println!("✅ Test PASSED: Lockfile updated after capability enable");
}

#[test]
#[ignore = "No `capability` noun exists on the live CLI (crates/ggen-cli/src/cmds/mod.rs). Intent (capability list) is impossible on the current CLI."]
fn test_capability_list_shows_capabilities() {
    println!("🔍 E2E Test: Capability list works");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: List capabilities
    let result = ggen()
        .arg("capability")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains capabilities
    assert!(
        json.get("capabilities").is_some(),
        "Should have capabilities"
    );
    assert!(json.get("total").is_some(), "Should have total");
    assert!(
        json["capabilities"].is_array(),
        "capabilities should be array"
    );

    println!("✅ Test PASSED: Capability list works");
}

#[test]
#[ignore = "No `capability` noun exists on the live CLI (crates/ggen-cli/src/cmds/mod.rs). Intent (capability inspect) is impossible on the current CLI."]
fn test_capability_inspect_shows_details() {
    println!("🔍 E2E Test: Capability inspect shows details");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Inspect a capability
    let result = ggen()
        .arg("capability")
        .arg("inspect")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains capability details
    assert_eq!(
        json["capability"], "mcp",
        "Should inspect correct capability"
    );
    assert!(
        json.get("atomic_packs").is_some(),
        "Should list atomic packs"
    );

    println!("✅ Test PASSED: Capability inspect works");
}

// ============================================================================
// Test Suite 3: Lockfile Creation and Persistence
// ============================================================================

#[test]
fn test_lockfile_created_after_pack_install() {
    println!("🔍 E2E Test: Lockfile created after pack install");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install pack
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile exists
    assert!(
        lockfile_path.exists(),
        "Lockfile should be created at {}",
        lockfile_path.display()
    );

    // Assert: .ggen directory exists
    assert!(
        temp_dir.path().join(".ggen").exists(),
        ".ggen directory should exist"
    );

    println!("✅ Test PASSED: Lockfile created");
}

#[test]
#[ignore = "Depends on the non-existent `capability list` noun (crates/ggen-cli/src/cmds/mod.rs). `packs list` was migrated to `pack list`, but the capability step cannot run on the current CLI."]
fn test_lockfile_persists_across_commands() {
    println!("🔍 E2E Test: Lockfile persists across commands");

    // Arrange: Create temporary directory with lockfile
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    create_test_lockfile(&lockfile_path).unwrap();

    // Act: Run multiple commands that read lockfile
    ggen()
        .arg("pack")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    ggen()
        .arg("capability")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile still exists
    assert!(
        lockfile_path.exists(),
        "Lockfile should persist across commands"
    );

    println!("✅ Test PASSED: Lockfile persists");
}

#[test]
fn test_lockfile_format_is_valid() {
    println!("🔍 E2E Test: Lockfile format is valid JSON");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install pack to create lockfile
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile is valid JSON
    let content = fs::read_to_string(&lockfile_path).expect("Failed to read lockfile");
    let json: Value = parse_json(&content).expect("Lockfile should be valid JSON");

    // Assert: Contains required fields. Real schema (`ggen_marketplace::packs::lockfile::
    // PackLockfile`) has `packs`/`updated_at`/`ggen_version` (+ an optional `profile`) --
    // no top-level `version` field, unlike this file's synthetic `create_test_lockfile`
    // fixture used elsewhere, which does not reflect the real writer's shape.
    assert!(json.get("packs").is_some(), "Should have packs");
    assert!(json["packs"].is_object(), "packs should be object");
    assert!(json.get("updated_at").is_some(), "Should have updated_at");
    assert!(
        json.get("ggen_version").is_some(),
        "Should have ggen_version"
    );

    println!("✅ Test PASSED: Lockfile format valid");
}

#[test]
fn test_lockfile_tracks_multiple_packs() {
    println!("🔍 E2E Test: Lockfile tracks multiple packs");

    // Arrange: Create temporary directory + a hermetic registry containing REAL
    // "surface-mcp"/"projection-rust" packs (see `PackFixture` doc comment).
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");
    fixture.write_pack("projection-rust", "1.0.0");

    // Act: Install multiple packs
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("projection-rust")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile contains both packs
    let pack_count = count_lockfile_packs(&lockfile_path).unwrap();
    assert_eq!(
        pack_count, 2,
        "Lockfile should contain 2 packs, found {}",
        pack_count
    );

    println!("✅ Test PASSED: Multiple packs tracked");
}

#[test]
fn test_lockfile_reproducibility() {
    println!("🔍 E2E Test: Lockfile ensures reproducibility");

    // Arrange: Create two temporary directories, each with its own hermetic
    // registry+$HOME (separate `PackFixture`s -- installing the SAME pack id
    // twice under one shared $HOME would hit `install_pack_by_id`'s "Pack
    // already installed" refusal on the second call; see `PackFixture` doc).
    let temp_dir1 = TempDir::new().unwrap();
    let temp_dir2 = TempDir::new().unwrap();
    let fixture1 = PackFixture::new();
    let fixture2 = PackFixture::new();
    fixture1.write_pack("surface-mcp", "1.0.0");
    fixture2.write_pack("surface-mcp", "1.0.0");

    // Act: Install same pack in both directories
    fixture1
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir1)
        .assert()
        .success();

    fixture2
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir2)
        .assert()
        .success();

    // Assert: Both lockfiles exist
    let lockfile1 = temp_dir1.path().join(".ggen/packs.lock");
    let lockfile2 = temp_dir2.path().join(".ggen/packs.lock");
    assert!(lockfile1.exists() && lockfile2.exists());

    // Assert: Both have same pack ID
    let content1 = fs::read_to_string(&lockfile1).unwrap();
    let content2 = fs::read_to_string(&lockfile2).unwrap();
    let json1: Value = parse_json(&content1).unwrap();
    let json2: Value = parse_json(&content2).unwrap();

    assert!(
        json1["packs"].get("surface-mcp").is_some(),
        "First lockfile should contain surface-mcp"
    );
    assert!(
        json2["packs"].get("surface-mcp").is_some(),
        "Second lockfile should contain surface-mcp"
    );

    println!("✅ Test PASSED: Lockfile ensures reproducibility");
}

// ============================================================================
// Test Suite 4: Receipt Generation and Verification
// ============================================================================

#[test]
fn test_receipt_generated_after_pack_install() {
    println!("🔍 E2E Test: Receipt generated after pack install");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install pack (should generate receipt)
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Receipts directory created
    assert!(
        receipts_dir.exists(),
        "Receipts directory should exist at {}",
        receipts_dir.display()
    );

    // Assert: At least one receipt file exists
    let receipt_files: Vec<_> = fs::read_dir(&receipts_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or(false)
        })
        .collect();

    assert!(
        !receipt_files.is_empty(),
        "At least one receipt file should exist"
    );

    println!("✅ Test PASSED: Receipt generated");
}

#[test]
#[ignore = "Live `receipt verify` (crates/ggen-engine/src/verbs/receipt.rs) takes ZERO arguments \
            and always targets .ggen-v2/receipt.json (the SYNC receipt chain) under the \
            resolved project root -- confirmed live: `ggen receipt verify --receipt_file X` \
            exits 1 with `error: unexpected argument '--receipt_file' found`, no such flag \
            exists. Pack-install receipts (.ggen/receipts/pack-*.json, generated by \
            crate::cmds::packs_receipt::generate_pack_install_receipt) are a completely \
            separate mechanism from the sync-receipt chain and have no CLI-level verify/inspect \
            command at all -- only the generation side exists. Intent (verify an arbitrary \
            receipt file passed as a CLI argument) is impossible on the current CLI. Real \
            current coverage of the actual `ggen receipt verify` (zero-arg, sync-receipt-chain) \
            command exists in crates/ggen-engine/tests/receipt_chain_e2e.rs."]
fn test_receipt_verify_works() {
    println!("🔍 E2E Test: Receipt verification works");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();
    let receipt_path = temp_dir.path().join("receipt.json");

    // Create test receipt
    create_test_receipt(&receipt_path).unwrap();

    // Act: Verify receipt (without public key - should show need for key)
    ggen()
        .arg("receipt")
        .arg("verify")
        .arg("--receipt_file")
        .arg(receipt_path.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    println!("✅ Test PASSED: Receipt verify command works");
}

#[test]
#[ignore = "No `receipt info` subcommand exists on the live CLI (crates/ggen-engine/src/verbs/\
            receipt.rs registers only `history`/`verify`; confirmed live: `ggen receipt info \
            --receipt_file X` exits 1 with `error: unrecognized subcommand 'info'`). \
            Pack-install receipts (.ggen/receipts/pack-*.json) have no CLI-level inspection \
            command at all -- only crate::cmds::packs_receipt::generate_pack_install_receipt \
            (write-only) exists. Intent (inspect an arbitrary receipt file via the CLI) is \
            impossible on the current CLI."]
fn test_receipt_info_shows_details() {
    println!("🔍 E2E Test: Receipt info shows details");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();
    let receipt_path = temp_dir.path().join("receipt.json");

    // Create test receipt
    create_test_receipt(&receipt_path).unwrap();

    // Act: Get receipt info
    let result = ggen()
        .arg("receipt")
        .arg("info")
        .arg("--receipt_file")
        .arg(receipt_path.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains receipt details
    assert!(
        json.get("operation_id").is_some(),
        "Should have operation_id"
    );
    assert!(json.get("timestamp").is_some(), "Should have timestamp");

    println!("✅ Test PASSED: Receipt info works");
}

#[test]
fn test_receipt_format_is_valid() {
    println!("🔍 E2E Test: Receipt format is valid JSON");

    // Arrange: Create temporary directory + a hermetic registry containing a REAL
    // "surface-mcp" pack (see `PackFixture` doc comment for why this is required).
    let temp_dir = TempDir::new().unwrap();
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");

    // Act: Install pack to generate receipt
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Find receipt file
    let receipt_files: Vec<_> = fs::read_dir(&receipts_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or(false)
        })
        .collect();

    assert!(!receipt_files.is_empty(), "Should have receipt files");

    // Assert: Receipt is valid JSON
    let receipt_path = receipt_files[0].path();
    let content = fs::read_to_string(&receipt_path).expect("Failed to read receipt");
    let json: Value = parse_json(&content).expect("Receipt should be valid JSON");

    // Assert: Contains required fields
    assert!(
        json.get("operation_id").is_some(),
        "Should have operation_id"
    );
    assert!(json.get("timestamp").is_some(), "Should have timestamp");
    assert!(
        json.get("input_hashes").is_some(),
        "Should have input_hashes"
    );
    assert!(
        json.get("output_hashes").is_some(),
        "Should have output_hashes"
    );

    println!("✅ Test PASSED: Receipt format valid");
}

#[test]
fn test_receipt_chain_verification() {
    println!("🔍 E2E Test: Receipt chain verification works");

    // Arrange: Create temporary directory + a hermetic registry containing REAL
    // "surface-mcp"/"projection-rust" packs (see `PackFixture` doc comment).
    let temp_dir = TempDir::new().unwrap();
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");
    fixture.write_pack("projection-rust", "1.0.0");

    // Act: Install multiple packs to create receipt chain
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("projection-rust")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Multiple receipts exist
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    let receipt_count = fs::read_dir(&receipts_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or(false)
        })
        .count();

    assert!(
        receipt_count >= 2,
        "Should have at least 2 receipts, found {}",
        receipt_count
    );

    println!("✅ Test PASSED: Receipt chain created");
}

// ============================================================================
// Test Suite 5: Policy Validation Workflow
// ============================================================================

#[test]
fn test_policy_validate_checks_lockfile() {
    println!("🔍 E2E Test: Policy validation checks lockfile");

    // Arrange: Create temporary directory with lockfile. `policy validate`
    // (crates/ggen-cli/src/cmds/policy.rs::run_policy_enforcement) is fail-closed by
    // design: a REAL violation returns Err (nonzero exit, no JSON), it does not emit a
    // soft `{"passed": false}` response -- confirmed live. So a "checks lockfile and
    // returns a validation result" test needs a lockfile pack that is ACTUALLY compliant
    // with `enterprise-strict`'s two live-enforced policies (signed receipts + explicit
    // runtime; see `write_compliant_pack_cache` doc comment), not just present.
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    create_test_lockfile(&lockfile_path).unwrap();
    let cache_root = TempDir::new().unwrap();
    write_compliant_pack_cache(cache_root.path(), "surface-mcp", "1.0.0");

    // Act: Validate against policy
    let result = ggen()
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .env("GGEN_PACK_CACHE_DIR", cache_root.path())
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains validation result
    assert!(json.get("profile_id").is_some(), "Should have profile_id");
    assert!(json.get("passed").is_some(), "Should have passed status");

    println!("✅ Test PASSED: Policy validation works");
}

#[test]
fn test_policy_list_shows_profiles() {
    println!("🔍 E2E Test: Policy list shows available profiles");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: List policy profiles
    let result = ggen()
        .arg("policy")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains profiles
    assert!(json.get("profiles").is_some(), "Should have profiles");
    assert!(json.get("total").is_some(), "Should have total");
    assert!(json["profiles"].is_array(), "profiles should be array");

    println!("✅ Test PASSED: Policy list works");
}

#[test]
fn test_policy_show_displays_profile_details() {
    println!("🔍 E2E Test: Policy show displays profile details");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Show profile details
    let result = ggen()
        .arg("policy")
        .arg("show")
        .arg("--profile_id")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Output is valid JSON
    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).expect("Output should be valid JSON");

    // Assert: Contains profile details
    assert_eq!(
        json["profile_id"], "enterprise-strict",
        "Should show correct profile"
    );
    assert!(json.get("policies").is_some(), "Should have policies");

    println!("✅ Test PASSED: Policy show works");
}

#[test]
fn test_policy_validation_without_lockfile_fails_gracefully() {
    println!("🔍 E2E Test: Policy validation handles missing lockfile gracefully");

    // Arrange: Create temporary directory (no lockfile)
    let temp_dir = TempDir::new().unwrap();

    // Act: Try to validate without lockfile
    let result = ggen()
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .assert();

    // Assert: "Gracefully" for a missing lockfile means a clear, non-panicking, nonzero
    // exit (`crates/ggen-cli/src/cmds/policy.rs::load_pack_contexts_from_project` returns
    // a typed `ArgumentError`, not a fake success) -- confirmed live: exit 1, stderr
    // "No project found. Please install packs first with 'ggen packs install <pack-id>'".
    // A silent `.success()` here would mask a real fail-open regression.
    result
        .failure()
        .stderr(predicate::str::contains("No project found"));

    println!("✅ Test PASSED: Graceful error handling");
}

#[test]
fn test_policy_enforces_trust_requirements() {
    println!("🔍 E2E Test: Policy enforces trust requirements");

    // Arrange: Create temporary directory with lockfile. `create_test_lockfile`'s
    // "surface-mcp" entry carries `"integrity": null` -- no signed-receipt/runtime
    // metadata -- so it does NOT meet `enterprise-strict`'s requirements. Point
    // GGEN_PACK_CACHE_DIR at a fresh EMPTY temp dir (rather than leaving it unset,
    // which would fall back to the real machine's `~/Library/Caches/ggen/packs`) so
    // "no metadata -> non-compliant -> rejected" is deterministic regardless of what
    // the host machine happens to have cached, not an accident of this one machine's
    // current state.
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    create_test_lockfile(&lockfile_path).unwrap();
    let empty_cache = TempDir::new().unwrap();

    // Act: Validate against strict profile
    let result = ggen()
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .env("GGEN_PACK_CACHE_DIR", empty_cache.path())
        .assert();

    // Assert: `policy validate` is fail-closed by design
    // (crates/ggen-cli/src/cmds/policy.rs::run_policy_enforcement) -- a real policy
    // violation returns Err (nonzero exit, no JSON), not a soft `{"passed": false}`.
    // Trust requirements ARE enforced: the non-compliant pack is REJECTED, loudly,
    // naming the profile and the violated policies -- confirmed live.
    result
        .failure()
        .stderr(predicate::str::contains("policy violation"))
        .stderr(predicate::str::contains("enterprise-strict"))
        .stderr(predicate::str::contains("surface-mcp"));

    println!("✅ Test PASSED: Trust requirements enforced");
}

// ============================================================================
// Test Suite 6: End-to-End Integration Workflows
// ============================================================================

#[test]
#[ignore = "Depends on the removed `packs validate` verb (no live `pack validate`; only `policy validate`). `packs install`->`pack add` and `packs list`->`pack list` were migrated, but the validate step cannot pass on the current CLI."]
fn test_full_workflow_install_to_receipt() {
    println!("🔍 E2E Test: Full workflow from install to receipt");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Step 1: Install pack
    ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 2: Verify .ggen directory exists
    assert!(
        temp_dir.path().join(".ggen").exists(),
        ".ggen directory should exist"
    );

    // Step 3: Verify lockfile exists
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    assert!(lockfile_path.exists(), "Lockfile should exist");

    // Step 4: Verify receipt exists
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    assert!(receipts_dir.exists(), "Receipts directory should exist");

    // Step 5: List packs (verifies installation)
    ggen()
        .arg("pack")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 6: Validate pack
    ggen()
        .arg("pack")
        .arg("validate")
        .arg("--pack_id")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    println!("✅ Test PASSED: Full workflow completed");
}

#[test]
#[ignore = "Depends on the non-existent `capability enable` noun (crates/ggen-cli/src/cmds/mod.rs). The policy step is live, but the capability step cannot run on the current CLI."]
fn test_full_workflow_capability_to_policy() {
    println!("🔍 E2E Test: Capability enable → policy validate workflow");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");

    // Step 1: Create initial lockfile
    create_test_lockfile(&lockfile_path).unwrap();

    // Step 2: Enable capability
    ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 3: Validate against policy
    ggen()
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 4: Verify lockfile still exists
    assert!(
        lockfile_path.exists(),
        "Lockfile should persist through workflow"
    );

    println!("✅ Test PASSED: Capability to policy workflow");
}

#[test]
#[ignore = "Depends on the nonexistent `receipt info` subcommand (crates/ggen-engine/src/verbs/\
            receipt.rs registers only `history`/`verify`; confirmed live: exit 1, \
            'unrecognized subcommand 'info''). Pack-install receipts (.ggen/receipts/*.json) \
            have no CLI-level inspection command at all. See test_receipt_info_shows_details' \
            comment for the fuller investigation; the pack-install step earlier in this test \
            would need `PackFixture` (see that struct's doc comment) but the workflow cannot \
            complete regardless since the receipt-info step has no live target."]
fn test_full_workflow_with_receipt_verification() {
    println!("🔍 E2E Test: Workflow with receipt verification");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Step 1: Install pack (generates receipt)
    ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 2: Find receipt file
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    let receipt_files: Vec<_> = fs::read_dir(&receipts_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or(false)
        })
        .collect();

    assert!(!receipt_files.is_empty(), "Should have receipt files");

    // Step 3: Get receipt info
    let receipt_path = receipt_files[0].path();
    ggen()
        .arg("receipt")
        .arg("info")
        .arg("--receipt_file")
        .arg(receipt_path.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();

    println!("✅ Test PASSED: Receipt verification workflow");
}

#[test]
#[ignore = "Depends on the non-existent `capability enable` noun (crates/ggen-cli/src/cmds/mod.rs). `packs list`->`pack list` was migrated, but the capability step cannot run on the current CLI."]
fn test_concurrent_operations_with_lockfile() {
    println!("🔍 E2E Test: Lockfile handles multiple operations");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");

    // Step 1: Create initial lockfile
    create_test_lockfile(&lockfile_path).unwrap();

    // Step 2: List packs (reads lockfile)
    ggen()
        .arg("pack")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 3: Enable capability (updates lockfile)
    ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 4: Validate policy (reads lockfile)
    ggen()
        .arg("policy")
        .arg("validate")
        .arg("--profile")
        .arg("enterprise-strict")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Assert: Lockfile exists after all operations
    assert!(
        lockfile_path.exists(),
        "Lockfile should persist through multiple operations"
    );

    println!("✅ Test PASSED: Concurrent operations handled");
}

#[test]
fn test_workflow_error_handling() {
    println!("🔍 E2E Test: Graceful error handling");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();

    // Test: Invalid pack ID returns helpful error. `pack show` takes the pack id as a
    // POSITIONAL argument, not `--pack_id` (confirmed live via `ggen pack show --help`:
    // `Usage: ggen pack show [OPTIONS] <PACK_ID>`; `--pack_id` itself is rejected by
    // clap as an unrecognized argument). And "gracefully" for an unknown pack means a
    // clear, non-panicking, NONZERO exit (`ggen_marketplace::packs_registry::metadata::
    // show_pack` returns a real Err) -- confirmed live: exit 1, "Pack 'nonexistent-pack-xyz'
    // not found ...". A silent `.success()` here would mask a real fail-open regression.
    ggen()
        .arg("pack")
        .arg("show")
        .arg("nonexistent-pack-xyz")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("not found"));

    // Test: Invalid receipt path handled gracefully. Live `receipt verify`
    // (crates/ggen-engine/src/verbs/receipt.rs) takes ZERO arguments and always
    // targets .ggen-v2/receipt.json under the resolved project root -- there is no
    // `--receipt_file` flag to point at an arbitrary path (confirmed live: passing it
    // is itself a clap argument-parsing error). With no sync ever run in this temp_dir,
    // .ggen-v2/receipt.json does not exist, so the real current equivalent of "invalid
    // receipt path" is simply running `receipt verify` here -- which fails loudly and
    // clearly (exit 1, "receipt ... unreadable: No such file or directory"), not a panic.
    ggen()
        .arg("receipt")
        .arg("verify")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("unreadable"));

    println!("✅ Test PASSED: Graceful error handling");
}

#[test]
fn test_full_workflow_multiple_packs() {
    println!("🔍 E2E Test: Install multiple packs with full workflow");

    // Arrange: Create temporary directory + a hermetic registry containing REAL
    // "surface-mcp"/"projection-rust" packs (see `PackFixture` doc comment).
    let temp_dir = TempDir::new().unwrap();
    let fixture = PackFixture::new();
    fixture.write_pack("surface-mcp", "1.0.0");
    fixture.write_pack("projection-rust", "1.0.0");

    // Step 1: Install multiple packs
    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    fixture
        .ggen()
        .arg("pack")
        .arg("add")
        .arg("projection-rust")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 2: Verify lockfile contains both packs
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
    let pack_count = count_lockfile_packs(&lockfile_path).unwrap();
    assert_eq!(pack_count, 2, "Should have 2 packs in lockfile");

    // Step 3: Verify receipts for both operations
    let receipts_dir = temp_dir.path().join(".ggen/receipts");
    let receipt_count = fs::read_dir(&receipts_dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "json")
                .unwrap_or(false)
        })
        .count();

    assert!(
        receipt_count >= 2,
        "Should have at least 2 receipts, found {}",
        receipt_count
    );

    // Step 4: List packs shows both (routed through the same fixture registry as the
    // installs above, so this reflects the 2 packs this test actually wrote/installed
    // rather than whatever happens to be registered on the machine running the suite).
    let result = fixture
        .ggen()
        .arg("pack")
        .arg("list")
        .current_dir(&temp_dir)
        .assert()
        .success();

    let output = String::from_utf8_lossy(&result.get_output().stdout);
    let json = parse_json(&output).unwrap();
    assert!(json["packs"].as_array().unwrap().len() >= 2);

    println!("✅ Test PASSED: Multiple packs workflow");
}

#[test]
#[ignore = "Depends on the non-existent `capability enable` noun (crates/ggen-cli/src/cmds/mod.rs). `packs install`->`pack add` was migrated, but the capability step cannot run on the current CLI."]
fn test_workflow_state_consistency() {
    println!("🔍 E2E Test: State consistency across workflow");

    // Arrange: Create temporary directory
    let temp_dir = TempDir::new().unwrap();
    let lockfile_path = temp_dir.path().join(".ggen/packs.lock");

    // Step 1: Install pack
    ggen()
        .arg("pack")
        .arg("add")
        .arg("surface-mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 2: Read initial lockfile state
    let content1 = fs::read_to_string(&lockfile_path).unwrap();
    let json1: Value = parse_json(&content1).unwrap();
    let pack_count1 = json1["packs"].as_object().unwrap().len();

    // Step 3: Enable capability (should update lockfile)
    ggen()
        .arg("capability")
        .arg("enable")
        .arg("mcp")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Step 4: Read updated lockfile state
    let content2 = fs::read_to_string(&lockfile_path).unwrap();
    let json2: Value = parse_json(&content2).unwrap();
    let pack_count2 = json2["packs"].as_object().unwrap().len();

    // Assert: Lockfile was updated (pack count should change or stay same)
    // The exact behavior depends on implementation
    assert!(pack_count2 >= pack_count1, "Lockfile should be updated");

    println!("✅ Test PASSED: State consistency maintained");
}
