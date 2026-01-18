//! Integration tests for drift detection
//!
//! Tests the full drift detection workflow including:
//! - First sync (creates baseline)
//! - No changes (no drift)
//! - Ontology changed (drift)
//! - Manifest changed (drift)
//! - Performance verification
//! - SHA256 tracking

use ggen_core::drift::{DriftDetector, DriftStatus, ChangeType};
use ggen_utils::error::Result;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::time::Instant;
use tempfile::TempDir;

/// Create a test file with content
fn create_test_file(dir: &Path, name: &str, content: &str) -> Result<std::path::PathBuf> {
    let path = dir.join(name);
    let mut file = fs::File::create(&path)?;
    file.write_all(content.as_bytes())?;
    file.sync_all()?;
    Ok(path)
}

#[test]
fn test_01_first_sync_creates_baseline() -> Result<()> {
    println!("\n[Test 1] First sync creates baseline");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // First check - no state exists yet
    let status = detector.check_drift(&ontology_path, &manifest_path)?;
    assert!(status.is_drifted(), "First run should report drift (no state)");

    if let DriftStatus::Drifted { ref changes, .. } = status {
        assert_eq!(changes.len(), 1, "Should have exactly one change (no state)");
        assert!(
            matches!(changes[0].change_type, ChangeType::NoState),
            "Change type should be NoState"
        );
        println!("  ✓ NoState change detected on first run");
    }

    // Save state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Baseline state saved");

    // Verify state file created
    assert!(detector.has_state(), "State file should exist");
    assert!(detector.state_file_path().exists(), "State file path should exist");
    println!("  ✓ State file created at: {}", detector.state_file_path().display());

    Ok(())
}

#[test]
fn test_02_no_changes_no_drift() -> Result<()> {
    println!("\n[Test 2] No changes - no drift");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save initial state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Initial state saved");

    // Check drift (should be clean)
    let status = detector.check_drift(&ontology_path, &manifest_path)?;
    assert!(!status.is_drifted(), "Should not detect drift when files unchanged");

    if let DriftStatus::Clean = status {
        println!("  ✓ Clean status confirmed");
    } else {
        panic!("Expected Clean status, got drifted");
    }

    // Verify no warning message
    assert!(status.warning_message().is_none(), "Should not have warning message");
    println!("  ✓ No warning message generated");

    Ok(())
}

#[test]
fn test_03_ontology_changed_drift_detected() -> Result<()> {
    println!("\n[Test 3] Ontology changed - drift detected");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save initial state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Initial state saved");

    // Modify ontology
    fs::write(&ontology_path, "@prefix ex: <http://example.org/> .\nex:NewClass a rdfs:Class .")?;
    println!("  ✓ Ontology modified");

    // Check drift (should detect change)
    let status = detector.check_drift(&ontology_path, &manifest_path)?;
    assert!(status.is_drifted(), "Should detect drift when ontology changed");

    if let DriftStatus::Drifted { ref changes, days_since_sync } = status {
        assert_eq!(changes.len(), 1, "Should have exactly one change");
        assert!(
            matches!(changes[0].change_type, ChangeType::Ontology),
            "Change type should be Ontology"
        );
        assert_eq!(days_since_sync, 0, "Days since sync should be 0 (just created)");

        // Verify hashes are different
        assert!(changes[0].old_hash.is_some(), "Should have old hash");
        assert!(changes[0].new_hash.is_some(), "Should have new hash");
        assert_ne!(
            changes[0].old_hash, changes[0].new_hash,
            "Old and new hashes should differ"
        );

        println!("  ✓ Ontology change detected");
        println!("  ✓ Hash changed: {:?} -> {:?}",
            &changes[0].old_hash.as_ref().unwrap()[..8],
            &changes[0].new_hash.as_ref().unwrap()[..8]
        );
    } else {
        panic!("Expected Drifted status, got Clean");
    }

    // Verify warning message
    let warning = status.warning_message();
    assert!(warning.is_some(), "Should have warning message");
    assert!(
        warning.unwrap().contains("Ontology"),
        "Warning should mention ontology"
    );
    println!("  ✓ Warning message generated");

    Ok(())
}

#[test]
fn test_04_manifest_changed_drift_detected() -> Result<()> {
    println!("\n[Test 4] Manifest changed - drift detected");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save initial state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Initial state saved");

    // Modify manifest
    fs::write(&manifest_path, "[project]\nname = \"test\"\nversion = \"1.0.0\"")?;
    println!("  ✓ Manifest modified");

    // Check drift (should detect change)
    let status = detector.check_drift(&ontology_path, &manifest_path)?;
    assert!(status.is_drifted(), "Should detect drift when manifest changed");

    if let DriftStatus::Drifted { ref changes, .. } = status {
        assert_eq!(changes.len(), 1, "Should have exactly one change");
        assert!(
            matches!(changes[0].change_type, ChangeType::Manifest),
            "Change type should be Manifest"
        );
        println!("  ✓ Manifest change detected");
    } else {
        panic!("Expected Drifted status, got Clean");
    }

    Ok(())
}

#[test]
fn test_05_performance_under_100ms() -> Result<()> {
    println!("\n[Test 5] Performance verification (<100ms overhead)");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save initial state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Initial state saved");

    // Measure drift check time (5 runs)
    let mut total_duration_ms = 0u128;
    let runs = 5;

    for i in 1..=runs {
        let start = Instant::now();
        let _ = detector.check_drift(&ontology_path, &manifest_path)?;
        let duration = start.elapsed();

        let duration_ms = duration.as_millis();
        total_duration_ms += duration_ms;
        println!("  ✓ Run {}: {}ms", i, duration_ms);
    }

    let avg_duration_ms = total_duration_ms / runs;
    println!("  ✓ Average drift check time: {}ms", avg_duration_ms);

    // Note: 100ms is a generous target. Typical checks are <10ms
    assert!(
        avg_duration_ms < 100,
        "Average drift check should be under 100ms, got {}ms",
        avg_duration_ms
    );

    println!("  ✓ Performance target met (<100ms)");

    Ok(())
}

#[test]
fn test_06_sha256_tracking() -> Result<()> {
    println!("\n[Test 6] SHA256 tracking verification");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ State saved");

    // Load and verify state file
    let state_file = detector.state_file_path();
    let state_content = fs::read_to_string(state_file)?;
    let state: serde_json::Value = serde_json::from_str(&state_content)?;

    // Verify ontology hash
    let ont_hash = state["ontology"]["hash"].as_str().expect("Ontology hash missing");
    assert_eq!(ont_hash.len(), 64, "SHA256 hash should be 64 characters");
    assert!(
        ont_hash.chars().all(|c| c.is_ascii_hexdigit()),
        "Hash should be hexadecimal"
    );
    println!("  ✓ Ontology hash valid: {}...", &ont_hash[..16]);

    // Verify manifest hash
    let man_hash = state["manifest"]["hash"].as_str().expect("Manifest hash missing");
    assert_eq!(man_hash.len(), 64, "SHA256 hash should be 64 characters");
    assert!(
        man_hash.chars().all(|c| c.is_ascii_hexdigit()),
        "Hash should be hexadecimal"
    );
    println!("  ✓ Manifest hash valid: {}...", &man_hash[..16]);

    // Verify hashes are different (different files)
    assert_ne!(ont_hash, man_hash, "Different files should have different hashes");
    println!("  ✓ Hashes are distinct");

    Ok(())
}

#[test]
fn test_07_ggen_directory_structure() -> Result<()> {
    println!("\n[Test 7] .ggen directory structure");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Verify state directory exists
    assert!(state_dir.exists(), ".ggen directory should exist");
    assert!(state_dir.is_dir(), ".ggen should be a directory");
    println!("  ✓ .ggen directory exists");

    // Save state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;

    // Verify state file location
    let state_file = detector.state_file_path();
    assert!(state_file.exists(), "State file should exist");
    assert_eq!(
        state_file.parent().unwrap(),
        &state_dir,
        "State file should be in .ggen directory"
    );
    assert_eq!(
        state_file.file_name().unwrap(),
        "sync-state.json",
        "State file should be named sync-state.json"
    );
    println!("  ✓ State file at correct location: {}", state_file.display());

    // Verify file is readable
    let content = fs::read_to_string(state_file)?;
    assert!(!content.is_empty(), "State file should not be empty");
    println!("  ✓ State file is readable");

    // Verify valid JSON
    let _: serde_json::Value = serde_json::from_str(&content)?;
    println!("  ✓ State file contains valid JSON");

    Ok(())
}

#[test]
fn test_08_non_blocking_execution() -> Result<()> {
    println!("\n[Test 8] Non-blocking execution");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Check without state (should not fail)
    let status = detector.check_drift(&ontology_path, &manifest_path);
    assert!(status.is_ok(), "Drift check should not fail without state");
    println!("  ✓ Drift check succeeds without state");

    // Create invalid state file
    let state_file = detector.state_file_path();
    fs::write(&state_file, "INVALID JSON")?;
    println!("  ✓ Created corrupted state file");

    // Check with corrupted state (should not fail, treat as no state)
    let status = detector.check_drift(&ontology_path, &manifest_path);
    assert!(status.is_ok(), "Drift check should not fail with corrupted state");

    if let Ok(DriftStatus::Drifted { ref changes, .. }) = status {
        assert!(
            matches!(changes[0].change_type, ChangeType::NoState),
            "Corrupted state should be treated as no state"
        );
        println!("  ✓ Corrupted state handled gracefully (treated as NoState)");
    }

    Ok(())
}

#[test]
fn test_09_clear_warning_messages() -> Result<()> {
    println!("\n[Test 9] Clear warning messages");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;

    // Modify ontology
    fs::write(&ontology_path, "@prefix ex: <http://example.org/> .\nex:Modified a rdfs:Class .")?;

    // Check drift
    let status = detector.check_drift(&ontology_path, &manifest_path)?;

    // Verify warning message
    let warning = status.warning_message();
    assert!(warning.is_some(), "Should have warning message");

    let warning_text = warning.unwrap();

    // Check for key elements
    assert!(warning_text.contains("⚠"), "Warning should contain warning symbol");
    assert!(
        warning_text.contains("changed") || warning_text.contains("Ontology"),
        "Warning should mention change"
    );
    assert!(
        warning_text.contains("sync") || warning_text.contains("update"),
        "Warning should suggest action"
    );
    assert!(
        warning_text.contains("days") || warning_text.contains("ago"),
        "Warning should mention time"
    );

    println!("  ✓ Warning message contains:");
    println!("    - Warning symbol (⚠)");
    println!("    - Change description");
    println!("    - Suggested action");
    println!("    - Time information");
    println!("\n  Warning message:\n{}", warning_text);

    Ok(())
}

#[test]
fn test_10_no_false_positives() -> Result<()> {
    println!("\n[Test 10] No false positives (mtime changes)");

    let temp_dir = TempDir::new()?;
    let ontology_path = create_test_file(temp_dir.path(), "ontology.ttl", "@prefix ex: <http://example.org/> .")?;
    let manifest_path = create_test_file(temp_dir.path(), "ggen.toml", "[project]\nname = \"test\"")?;
    let state_dir = temp_dir.path().join(".ggen");
    fs::create_dir(&state_dir)?;

    let detector = DriftDetector::new(&state_dir)?;

    // Save state
    detector.save_state(&ontology_path, &manifest_path, 5, 1000)?;
    println!("  ✓ Initial state saved");

    // Re-write same content (changes mtime but not content)
    fs::write(&ontology_path, "@prefix ex: <http://example.org/> .")?;
    println!("  ✓ File re-written with same content (mtime changed)");

    // Check drift (should be clean - SHA256 hasn't changed)
    let status = detector.check_drift(&ontology_path, &manifest_path)?;
    assert!(
        !status.is_drifted(),
        "Should not detect drift when content unchanged (only mtime changed)"
    );

    println!("  ✓ No false positive from mtime change");
    println!("  ✓ SHA256-based detection prevents false positives");

    Ok(())
}
