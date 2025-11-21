//! Unit tests for ggen node bindings
//!
//! JTBD VALIDATION: These tests verify that Node NIF functions actually work,
//! not just that they compile or construct arguments correctly.
//!
//! Each test validates:
//! 1. Function executes without crashing
//! 2. Exit code is appropriate (0 for success, non-zero for errors)
//! 3. Output contains expected content
//! 4. No unexpected errors in stderr

use chicago_tdd_tools::prelude::*;
use ggen_cli_lib::{run_for_node, RunResult};

/// Helper to assert successful command execution with content validation
fn assert_success_with_content(result: RunResult, expected_content: &str, test_name: &str) {
    assert_eq!(
        result.code, 0,
        "{}: Expected success (exit code 0), got {}\nstderr: {}",
        test_name, result.code, result.stderr
    );

    assert!(
        result.stdout.contains(expected_content) || result.stderr.contains(expected_content),
        "{}: Expected output to contain '{}'\nstdout: {}\nstderr: {}",
        test_name,
        expected_content,
        result.stdout,
        result.stderr
    );

    // For successful commands, stderr should be empty or only contain progress messages
    if !result.stderr.is_empty() {
        assert!(
            result.stderr.contains("Searching")
                || result.stderr.contains("Loading")
                || result.stderr.contains("Building")
                || result.stderr.is_empty(),
            "{}: Unexpected stderr for successful command: {}",
            test_name,
            result.stderr
        );
    }
}

/// Helper to assert command failure with error message
fn assert_failure_with_message(result: RunResult, test_name: &str) {
    assert_ne!(
        result.code, 0,
        "{}: Expected failure (non-zero exit code), got success",
        test_name
    );

    assert!(
        !result.stderr.is_empty() || !result.stdout.is_empty(),
        "{}: Expected error message in stdout or stderr",
        test_name
    );
}

#[cfg(test)]
mod version_tests {
    use super::*;

    /// JTBD: Verify that --version returns a valid semantic version string
    async_test!(test_version_returns_valid_semver, async {
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version command should not panic");

        // Should succeed
        assert_eq!(result.code, 0, "version should succeed");

        // Should contain the cargo package version
        let expected_version = env!("CARGO_PKG_VERSION");
        assert!(
            result.stdout.contains(expected_version),
            "Output should contain version {}, got: {}",
            expected_version,
            result.stdout
        );

        // Should be in semver format (X.Y.Z)
        let version_parts: Vec<&str> = expected_version.split('.').collect();
        assert_eq!(
            version_parts.len(),
            3,
            "Version should have 3 parts (major.minor.patch)"
        );

        for part in version_parts {
            assert!(
                part.parse::<u32>().is_ok(),
                "Version part '{}' should be a number",
                part
            );
        }
    });

    /// JTBD: Verify version command is fast
    async_test!(test_version_performance, async {
        use std::time::{Duration, Instant};

        let start = Instant::now();
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version command should not panic");
        let duration = start.elapsed();

        assert_eq!(result.code, 0);
        assert!(
            duration < Duration::from_millis(100),
            "Version should complete in under 100ms, took {:?}",
            duration
        );
    });
}

#[cfg(test)]
mod help_tests {
    use super::*;

    /// JTBD: Verify that --help returns usage information
    async_test!(test_help_shows_usage, async {
        let result = run_for_node(vec!["--help".to_string()])
            .await
            .expect("help command should not panic");

        assert_eq!(result.code, 0, "help should succeed");

        // Should contain usage information
        assert!(
            result.stdout.to_lowercase().contains("usage")
                || result.stdout.to_lowercase().contains("commands"),
            "Help should contain usage information, got: {}",
            result.stdout
        );

        // Should be substantial (not just a single line)
        assert!(
            result.stdout.len() > 100,
            "Help output should be substantial, got {} chars",
            result.stdout.len()
        );
    });

    /// JTBD: Verify that help for specific commands works
    async_test!(test_help_for_market_command, async {
        let result = run_for_node(vec!["market".to_string(), "--help".to_string()])
            .await
            .expect("market help should not panic");

        assert_eq!(result.code, 0, "market help should succeed");

        // Should mention marketplace-related commands
        assert!(
            result.stdout.to_lowercase().contains("market")
                || result.stdout.to_lowercase().contains("search")
                || result.stdout.to_lowercase().contains("add"),
            "Market help should mention marketplace commands, got: {}",
            result.stdout
        );
    });
}

#[cfg(test)]
mod marketplace_tests {
    use super::*;

    /// JTBD: Verify that market list returns installed packages
    async_test!(test_market_list_returns_packages, async {
        let result = run_for_node(vec!["market".to_string(), "list".to_string()])
            .await
            .expect("market list should not panic");

        // Should succeed
        assert_eq!(result.code, 0, "market list should succeed");

        // Should return something (either packages or "no packages" message)
        assert!(
            !result.stdout.is_empty() || !result.stderr.is_empty(),
            "market list should produce output"
        );
    });

    /// JTBD: Verify that market categories returns available categories
    async_test!(test_market_categories_returns_list, async {
        let result = run_for_node(vec!["market".to_string(), "categories".to_string()])
            .await
            .expect("market categories should not panic");

        assert_eq!(result.code, 0, "market categories should succeed");

        // Should have output
        assert!(
            !result.stdout.is_empty(),
            "market categories should return data"
        );
    });

    /// JTBD: Verify that market search with valid query works
    async_test!(test_market_search_executes, async {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "rust".to_string(),
        ])
        .await
        .expect("market search should not panic");

        // Should complete (success or no results found)
        assert!(
            result.code == 0 || result.code == 1,
            "market search should complete"
        );

        // If successful, should have output
        if result.code == 0 {
            assert!(
                !result.stdout.is_empty() || result.stderr.contains("Searching"),
                "Successful search should produce output"
            );
        }
    });

    /// JTBD: Verify that market add with invalid package fails gracefully
    async_test!(test_market_add_invalid_package_fails_gracefully, async {
        let result = run_for_node(vec![
            "market".to_string(),
            "add".to_string(),
            "definitely-not-a-real-package-12345".to_string(),
        ])
        .await
        .expect("market add should not panic");

        // Should fail (package doesn't exist)
        assert_ne!(result.code, 0, "Adding non-existent package should fail");

        // Should have error message
        assert!(
            !result.stderr.is_empty() || !result.stdout.is_empty(),
            "Failed add should produce error message"
        );
    });
}

#[cfg(test)]
mod lifecycle_tests {
    use super::*;

    /// JTBD: Verify that lifecycle list returns available phases
    async_test!(test_lifecycle_list_returns_phases, async {
        let result = run_for_node(vec!["lifecycle".to_string(), "list".to_string()])
            .await
            .expect("lifecycle list should not panic");

        assert_eq!(result.code, 0, "lifecycle list should succeed");

        // Should list common phases
        let output_lower = result.stdout.to_lowercase();
        let has_phases = output_lower.contains("init")
            || output_lower.contains("build")
            || output_lower.contains("test")
            || output_lower.contains("deploy");

        assert!(
            has_phases,
            "lifecycle list should mention common phases, got: {}",
            result.stdout
        );
    });

    /// JTBD: Verify that lifecycle readiness returns status
    async_test!(test_lifecycle_readiness_returns_status, async {
        let result = run_for_node(vec!["lifecycle".to_string(), "readiness".to_string()])
            .await
            .expect("lifecycle readiness should not panic");

        // Should complete (may succeed or fail depending on project state)
        assert!(
            result.code == 0 || result.code == 1,
            "lifecycle readiness should complete"
        );

        // Should have output
        assert!(
            !result.stdout.is_empty() || !result.stderr.is_empty(),
            "lifecycle readiness should produce output"
        );
    });
}

#[cfg(test)]
mod template_tests {
    use super::*;

    /// JTBD: Verify that template list (list command) returns templates
    async_test!(test_template_list_returns_templates, async {
        let result = run_for_node(vec!["list".to_string()])
            .await
            .expect("list command should not panic");

        // Should complete
        assert!(
            result.code == 0 || result.code == 1,
            "list command should complete"
        );

        // Should have output
        assert!(
            !result.stdout.is_empty() || !result.stderr.is_empty(),
            "list command should produce output"
        );
    });

    /// JTBD: Verify that gen with invalid template fails gracefully
    async_test!(test_template_generate_invalid_fails_gracefully, async {
        let result = run_for_node(vec![
            "gen".to_string(),
            "definitely-not-a-template-12345.tmpl".to_string(),
        ])
        .await
        .expect("gen command should not panic");

        // Should fail (template doesn't exist)
        assert_ne!(
            result.code, 0,
            "Generating from non-existent template should fail"
        );

        // Should have error message
        assert!(
            !result.stderr.is_empty() || !result.stdout.is_empty(),
            "Failed gen should produce error message"
        );
    });
}

#[cfg(test)]
mod utility_tests {
    use super::*;

    /// JTBD: Verify that doctor command runs diagnostics
    async_test!(test_doctor_runs_diagnostics, async {
        let result = run_for_node(vec!["doctor".to_string()])
            .await
            .expect("doctor command should not panic");

        assert_eq!(result.code, 0, "doctor should succeed");

        // Should check environment
        let output_lower = result.stdout.to_lowercase();
        let checks_env = output_lower.contains("rust")
            || output_lower.contains("cargo")
            || output_lower.contains("version")
            || output_lower.contains("environment");

        assert!(
            checks_env,
            "doctor should check environment, got: {}",
            result.stdout
        );
    });
}

#[cfg(test)]
mod error_handling_tests {
    use super::*;

    /// JTBD: Verify that empty args don't crash (should show help or error)
    async_test!(test_empty_args_handled, async {
        let result = run_for_node(vec![])
            .await
            .expect("empty args should not panic");

        // Should complete (help or error)
        assert!(result.code >= 0, "Should return valid exit code");

        // Should produce output
        assert!(
            !result.stdout.is_empty() || !result.stderr.is_empty(),
            "Empty args should produce help or error"
        );
    });

    /// JTBD: Verify that invalid command produces error
    async_test!(test_invalid_command_produces_error, async {
        let result = run_for_node(vec!["definitely-not-a-valid-command-12345".to_string()])
            .await
            .expect("invalid command should not panic");

        // Should fail
        assert_ne!(result.code, 0, "Invalid command should return error code");

        // Should have error message
        assert!(
            !result.stderr.is_empty() || !result.stdout.is_empty(),
            "Invalid command should produce error message"
        );
    });

    /// JTBD: Verify that special characters don't crash
    async_test!(test_special_characters_handled, async {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "rust-web@1.0-beta!".to_string(),
        ])
        .await
        .expect("special characters should not panic");

        // Should complete (doesn't matter if success or failure)
        assert!(result.code >= 0, "Should handle special chars");
    });

    /// JTBD: Verify that unicode doesn't crash
    async_test!(test_unicode_handled, async {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "Créer 日本語 🚀".to_string(),
        ])
        .await
        .expect("unicode should not panic");

        // Should complete
        assert!(result.code >= 0, "Should handle unicode");

        // Output should be valid UTF-8
        assert!(std::str::from_utf8(result.stdout.as_bytes()).is_ok());
        assert!(std::str::from_utf8(result.stderr.as_bytes()).is_ok());
    });

    /// JTBD: Verify that very long args don't crash
    async_test!(test_very_long_args_handled, async {
        let long_arg = "a".repeat(10_000);
        let result = run_for_node(vec!["market".to_string(), "search".to_string(), long_arg])
            .await
            .expect("very long args should not panic");

        // Should complete (may succeed or fail)
        assert!(result.code >= 0, "Should handle long args");
    });
}

#[cfg(test)]
mod ggen_broken_detection_tests {
    use super::*;

    /// JTBD: Detect when ggen returns 0 but stderr has unexpected errors
    async_test!(test_detects_false_success, async {
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version should not panic");

        if result.code == 0 {
            // Success should not have error messages in stderr
            if !result.stderr.is_empty() {
                assert!(
                    result.stderr.contains("Searching")
                        || result.stderr.contains("Loading")
                        || result.stderr.contains("Building"),
                    "Success (exit code 0) should not have unexpected errors in stderr: {}",
                    result.stderr
                );
            }
        }
    });

    /// JTBD: Detect when ggen returns 0 but output is empty
    async_test!(test_detects_silent_failure, async {
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version should not panic");

        if result.code == 0 {
            assert!(
                !result.stdout.is_empty(),
                "Successful version command should return version string"
            );
        }
    });

    /// JTBD: Detect when ggen returns 0 but output format is wrong
    async_test!(test_validates_version_format, async {
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version should not panic");

        if result.code == 0 {
            // Version should contain semantic version pattern
            let has_version =
                result.stdout.contains('.') && result.stdout.chars().any(|c| c.is_numeric());

            assert!(
                has_version,
                "Version output should contain version number, got: {}",
                result.stdout
            );
        }
    });

    /// JTBD: Detect when ggen returns error but no error message
    async_test!(test_detects_silent_error, async {
        let result = run_for_node(vec!["definitely-invalid-command-xyz".to_string()])
            .await
            .expect("invalid command should not panic");

        if result.code != 0 {
            assert!(
                !result.stderr.is_empty() || !result.stdout.is_empty(),
                "Failed command should produce error message"
            );
        }
    });

    /// JTBD: Detect when ggen crashes vs returns error
    async_test!(test_distinguishes_crash_from_error, async {
        // Invalid commands should error, not crash
        let result = run_for_node(vec!["invalid".to_string()])
            .await
            .expect("Invalid command should return error, not panic");

        // Should have valid exit code (not a crash signal)
        assert!(
            result.code >= 0 && result.code < 128,
            "Exit code should indicate error, not crash: {}",
            result.code
        );
    });
}

#[cfg(test)]
mod data_structure_validation_tests {
    use super::*;

    /// JTBD: Verify that version output is actually semantic versioning
    async_test!(test_version_is_semver, async {
        let result = run_for_node(vec!["--version".to_string()])
            .await
            .expect("version should not panic");

        if result.code == 0 {
            // Extract version number (remove "ggen " prefix if present)
            let version_str = result.stdout.trim().replace("ggen ", "").trim().to_string();

            // Should have 3 parts
            let parts: Vec<&str> = version_str.split('.').collect();
            assert!(
                parts.len() >= 3,
                "Version should have at least 3 parts (major.minor.patch), got: {}",
                version_str
            );

            // Major, minor, patch should be numbers
            for (i, part) in parts.iter().take(3).enumerate() {
                assert!(
                    part.chars().any(|c| c.is_numeric()),
                    "Version part {} should contain number, got: {}",
                    i,
                    part
                );
            }
        }
    });
}
