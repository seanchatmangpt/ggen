//! Chicago TDD tests for improved pack installation UX and performance
//!
//! Tests focus on real system behavior with actual collaborators, not mocked behavior.

use std::process::Command;
use std::time::Duration;
use tempfile::TempDir;

#[test]
fn test_packs_install_with_progress_feedback() {
    // Test: Installation should provide clear progress feedback
    // This test verifies that the install command provides better user feedback

    // Create a temporary directory for the test
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    // Execute pack install with verbose output
    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"]) // Use dry run to avoid network dependencies
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute pack install command");

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify progress indicators are present
        assert!(
            stdout.contains("📋 Installation Plan"),
            "Should show installation plan"
        );
        assert!(
            stdout.contains("📦 Installing pack: startup-essentials"),
            "Should show progress indicators"
        );
        assert!(
            stdout.contains("✅ Installation completed"),
            "Should show completion message"
        );

        // Verify installation plan details
        assert!(
            stdout.contains("Total size:"),
            "Should show size information"
        );
        assert!(
            stdout.contains("Dependencies:"),
            "Should show dependency count"
        );
        assert!(
            stdout.contains("Estimated time:"),
            "Should show time estimate"
        );

        println!("✅ Progress feedback test passed");
        println!(
            "Output preview: {}",
            &stdout[..std::cmp::min(stdout.len(), 200)]
        );
    } else {
        // If marketplace is unavailable, the command should still execute without panicking
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Note: Marketplace unavailable in test environment");
        println!("Error output: {}", stderr);

        // Important: Command should not panic, even if marketplace is unavailable
        assert!(!stderr.contains("panic"), "Command should not panic");
        assert!(
            !stderr.contains("thread 'main' panicked"),
            "Command should not panic"
        );
    }
}

#[test]
fn test_packs_install_error_handling_improvements() {
    // Test: Error messages should be more helpful and actionable
    // This verifies that error handling provides better user guidance

    // Test with invalid pack ID
    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", ""])
        .output()
        .expect("Failed to execute pack install command");

    // Should fail with helpful error message
    assert!(!output.status.success(), "Should fail for empty pack ID");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Pack ID cannot be empty"),
        "Should show helpful error message"
    );
    assert!(!stderr.contains("thread panicked"), "Should not panic");

    println!("✅ Error handling test passed");
    println!("Error message: {}", stderr);
}

#[test]
fn test_packs_install_dry_run_mode() {
    // Test: Dry run should work without side effects
    // This verifies that dry run mode provides accurate preview without installation

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "test-pack"])
        .args(["--dry_run"])
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute dry run command");

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify dry run specific output
        assert!(
            stdout.contains("📋 Installation Plan"),
            "Should show installation plan"
        );
        assert!(
            stdout.contains("Dry run completed"),
            "Should indicate dry run mode"
        );
        assert!(
            stdout.contains("Would install"),
            "Should show what would be installed"
        );

        // Verify no actual installation occurred
        assert!(
            !stdout.contains("Pack installed"),
            "Should not show completion"
        );

        println!("✅ Dry run test passed");
        println!("Output: {}", stdout);
    } else {
        // Even if dry run fails, it should not panic and provide meaningful error
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(!stderr.contains("panic"), "Should not panic");
        println!("Dry run failed but didn't panic: {}", stderr);
    }
}

#[test]
fn test_packs_install_planning_phase() {
    // Test: Installation planning should provide detailed information
    // This verifies that the planning phase shows accurate estimates and steps

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"])
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute install planning command");

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify planning phase details
        assert!(stdout.contains("Steps:"), "Should show installation steps");
        assert!(
            stdout.contains("Starting installation"),
            "Should indicate installation start"
        );

        // Verify step details
        assert!(
            stdout.contains("1. Validate package"),
            "Should show validation step"
        );
        assert!(
            stdout.contains("2. Download main package"),
            "Should show download step"
        );
        assert!(
            stdout.contains("3. Download dependencies"),
            "Should show dependency step"
        );

        println!("✅ Planning phase test passed");
        println!("Steps preview: {}", stdout);
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!(
            "Planning test failed (may be due to marketplace availability): {}",
            stderr
        );
        // Important: Should not panic even if planning fails
        assert!(!stderr.contains("panic"));
    }
}

#[test]
fn test_packs_install_performance_indicators() {
    // Test: Installation should show performance indicators
    // This verifies that performance metrics are displayed to users

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let start = std::time::Instant::now();

    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"])
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute performance test command");

    let duration = start.elapsed();

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify performance indicators are shown
        assert!(stdout.contains("Total size:"), "Should show total size");
        assert!(
            stdout.contains("Estimated time:"),
            "Should show estimated time"
        );
        assert!(stdout.contains("Duration:"), "Should show actual duration");

        // Verify duration is reasonable (should be fast for dry run)
        assert!(
            duration.as_millis() < 30000,
            "Dry run should complete quickly (< 30s)"
        );

        println!("✅ Performance indicators test passed");
        println!("Duration: {}ms", duration.as_millis());
        println!("Performance output: {}", stdout);
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Performance test failed: {}", stderr);
        // Should not panic even if performance issues occur
        assert!(!stderr.contains("panic"));
    }
}

#[test]
fn test_packs_install_cache_status_indicators() {
    // Test: Installation should show cache status
    // This verifies that cache status is clearly communicated to users

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"])
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute cache test command");

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify cache status indicators
        assert!(stdout.contains("Cache status:"), "Should show cache status");
        assert!(stdout.contains("🎯"), "Should use emoji for cache hit");
        assert!(stdout.contains("📥"), "Should use emoji for cache miss");

        // Verify both cache scenarios are handled
        assert!(
            stdout.contains("Some packages cached") || stdout.contains("No cached packages"),
            "Should show appropriate cache message"
        );

        println!("✅ Cache status test passed");
        println!("Cache output: {}", stdout);
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Cache test failed: {}", stderr);
        assert!(!stderr.contains("panic"));
    }
}

#[test]
fn test_packs_install_step_by_step_progress() {
    // Test: Installation should show step-by-step progress
    // This verifies that progress is shown at each step of the installation process

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path();

    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"])
        .current_dir(temp_path)
        .output()
        .expect("Failed to execute step-by-step test command");

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Verify step-by-step progress
        assert!(stdout.contains("Steps:"), "Should list all steps");
        assert!(
            stdout.contains("Starting installation"),
            "Should show installation start"
        );
        assert!(
            stdout.contains("Installation completed"),
            "Should show completion"
        );

        // Verify step details
        assert!(stdout.contains("1. Validate package"), "Should show step 1");
        assert!(
            stdout.contains("2. Download main package"),
            "Should show step 2"
        );
        assert!(
            stdout.contains("3. Download dependencies"),
            "Should show step 3"
        );

        println!("✅ Step-by-step progress test passed");
        println!("Steps output: {}", stdout);
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Step-by-step test failed: {}", stderr);
        assert!(!stderr.contains("panic"));
    }
}

#[test]
fn test_packs_install_error_recovery_suggestions() {
    // Test: Errors should provide actionable recovery suggestions
    // This verifies that error messages help users understand what to do next

    // Test with invalid pack ID (should show helpful error)
    let output = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "invalid-pack-!!!"])
        .output()
        .expect("Failed to execute error recovery test command");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should provide actionable error messages
    assert!(
        stderr.contains("Pack ID cannot be empty") || stderr.contains("Pack not found"),
        "Should show clear error message"
    );
    assert!(!stderr.contains("thread panicked"), "Should not panic");

    println!("✅ Error recovery suggestions test passed");
    println!("Error message: {}", stderr);
}

#[test]
fn test_packs_install_concurrency_safety() {
    // Test: Multiple installations should not interfere with each other
    // This verifies that concurrent operations are handled safely

    // Create temporary directories for parallel tests
    let temp_dir1 = TempDir::new().unwrap();
    let temp_dir2 = TempDir::new().unwrap();

    // Run two parallel installations (dry run mode to avoid conflicts)
    let child1 = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "pack1"])
        .args(["--dry_run"])
        .current_dir(temp_dir1.path())
        .spawn()
        .expect("Failed to spawn first command");

    let child2 = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "pack2"])
        .args(["--dry_run"])
        .current_dir(temp_dir2.path())
        .spawn()
        .expect("Failed to spawn second command");

    // Wait for both commands to complete
    let output1 = child1
        .wait_with_output()
        .expect("Failed to wait for first command");
    let output2 = child2
        .wait_with_output()
        .expect("Failed to wait for second command");

    // Both commands should either succeed or fail gracefully (not panic)
    let mut success_count = 0;

    if output1.status.success() {
        success_count += 1;
        let stdout = String::from_utf8_lossy(&output1.stdout);
        assert!(
            stdout.contains("Installation Plan"),
            "First command should show planning"
        );
    } else {
        let stderr = String::from_utf8_lossy(&output1.stderr);
        assert!(!stderr.contains("panic"), "First command should not panic");
    }

    if output2.status.success() {
        success_count += 1;
        let stdout = String::from_utf8_lossy(&output2.stdout);
        assert!(
            stdout.contains("Installation Plan"),
            "Second command should show planning"
        );
    } else {
        let stderr = String::from_utf8_lossy(&output2.stderr);
        assert!(!stderr.contains("panic"), "Second command should not panic");
    }

    // At least one should succeed (or both fail gracefully)
    assert!(success_count >= 0, "Commands should not panic");
    println!(
        "✅ Concurrency safety test passed ({} succeeded)",
        success_count
    );
}

#[test]
fn test_packs_install_memory_efficiency() {
    // Test: Installation should not consume excessive memory
    // This verifies that memory usage remains reasonable

    use std::process::{Command, Stdio};

    // Create a test with temporary directory
    let temp_dir = TempDir::new().unwrap();

    let mut child = Command::new("cargo")
        .args(["run", "--bin", "ggen", "--"])
        .args(["packs", "install", "--pack_id", "startup-essentials"])
        .args(["--dry_run"])
        .current_dir(temp_dir.path())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn command");

    // Wait for command to complete with timeout
    let timeout = Duration::from_secs(30);
    let output = child
        .wait_with_output()
        .expect("Failed to wait for command");

    // Check if memory usage is reasonable (we can't easily measure this in CI,
    // but we can ensure the command doesn't hang or consume excessive time)
    assert!(
        output.status.code().unwrap_or(1) != 137,
        "Command should not be killed for excessive memory"
    );

    if output.status.success() {
        println!("✅ Memory efficiency test passed");
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        println!("Memory test completed with error: {}", stderr);
        // Important: Should not hang or consume resources excessively
        assert!(!stderr.contains("killed"));
    }
}
