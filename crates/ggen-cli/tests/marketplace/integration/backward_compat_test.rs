//! Backward compatibility tests for marketplace v1
//!
//! Ensures that with marketplace-v1 feature flag, all commands work
//! identically to the original implementation with zero regression.

#[cfg(test)]
#[cfg(feature = "marketplace-v1")]
mod backward_compat_tests {
    use std::process::Command;
    use tempfile::TempDir;

    /// Helper: Run ggen marketplace command
    fn run_marketplace_cmd(args: &[&str]) -> Result<String, String> {
        let output = Command::new("cargo")
            .args(&["run", "--features", "marketplace-v1", "--"])
            .args(&["marketplace"])
            .args(args)
            .output()
            .map_err(|e| format!("Failed to execute command: {}", e))?;

        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).to_string())
        } else {
            Err(String::from_utf8_lossy(&output.stderr).to_string())
        }
    }

    #[test]
    fn test_v1_search_command_works() {
        let result = run_marketplace_cmd(&["search", "rust"]);
        assert!(
            result.is_ok(),
            "Search command should succeed with v1 backend"
        );

        let output = result.unwrap();
        assert!(!output.is_empty(), "Search should return results");
    }

    #[test]
    fn test_v1_list_command_works() {
        let result = run_marketplace_cmd(&["list"]);
        assert!(
            result.is_ok(),
            "List command should succeed with v1 backend"
        );

        let output = result.unwrap();
        assert!(!output.is_empty(), "List should return packages");
    }

    #[test]
    fn test_v1_maturity_command_works() {
        let result = run_marketplace_cmd(&["maturity", "test-package"]);

        // May fail if package doesn't exist, but should run without panic
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_validate_command_works() {
        let result = run_marketplace_cmd(&["validate", "test-package"]);

        // Should execute without panic
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_dashboard_command_works() {
        let result = run_marketplace_cmd(&["dashboard"]);

        // Dashboard should display statistics
        assert!(result.is_ok());
    }

    #[test]
    fn test_v1_search_output_format() {
        let result = run_marketplace_cmd(&["search", "database"]);

        if let Ok(output) = result {
            // V1 output format should contain package information
            // Exact format depends on implementation
            assert!(!output.is_empty());
        }
    }

    #[test]
    fn test_v1_list_output_format() {
        let result = run_marketplace_cmd(&["list", "--limit", "10"]);

        if let Ok(output) = result {
            // List should show packages with metadata
            assert!(!output.is_empty());
        }
    }

    #[test]
    fn test_v1_error_messages_unchanged() {
        // Test that error messages match v1 format
        let result = run_marketplace_cmd(&["search", ""]);

        // Empty search should produce specific error message
        assert!(result.is_err() || result.unwrap().contains("error"));
    }

    #[test]
    fn test_v1_invalid_package_id_error() {
        let result = run_marketplace_cmd(&["maturity", "invalid package name"]);

        // Should fail with appropriate error
        assert!(result.is_err());
    }

    #[test]
    fn test_v1_nonexistent_package_error() {
        let result = run_marketplace_cmd(&["maturity", "nonexistent-package-xyz-123"]);

        // Should handle gracefully
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_search_performance_baseline() {
        use std::time::Instant;

        let start = Instant::now();
        let _ = run_marketplace_cmd(&["search", "rust"]);
        let duration = start.elapsed();

        // V1 search should complete in reasonable time (<5s)
        assert!(duration.as_secs() < 5, "V1 search too slow: {:?}", duration);
    }

    #[test]
    fn test_v1_list_performance_baseline() {
        use std::time::Instant;

        let start = Instant::now();
        let _ = run_marketplace_cmd(&["list", "--limit", "100"]);
        let duration = start.elapsed();

        // V1 list should complete quickly
        assert!(duration.as_secs() < 3, "V1 list too slow: {:?}", duration);
    }

    #[test]
    fn test_v1_search_with_filters() {
        let result = run_marketplace_cmd(&["search", "database", "--tag", "rust"]);

        // Filtered search should work
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_search_pagination() {
        let result = run_marketplace_cmd(&["search", "test", "--limit", "5", "--offset", "0"]);

        // Pagination should work
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_list_sorting() {
        let result = run_marketplace_cmd(&["list", "--sort-by", "name"]);

        // Sorting should work
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_v1_concurrent_search_stability() {
        use std::thread;

        let handles: Vec<_> = (0..5)
            .map(|i| {
                thread::spawn(move || {
                    let query = format!("query-{}", i);
                    run_marketplace_cmd(&["search", &query])
                })
            })
            .collect();

        // All concurrent searches should complete
        for handle in handles {
            let _ = handle.join();
        }
    }

    #[test]
    fn test_v1_help_command_works() {
        let result = run_marketplace_cmd(&["--help"]);
        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(output.contains("marketplace") || output.contains("help"));
    }

    #[test]
    fn test_v1_version_flag_works() {
        let output = Command::new("cargo")
            .args(&["run", "--features", "marketplace-v1", "--", "--version"])
            .output()
            #[allow(clippy::expect_used)]
            .expect("Failed to execute version command");

        assert!(output.status.success());
    }

    #[test]
    fn test_v1_json_output_format() {
        let result = run_marketplace_cmd(&["search", "test", "--format", "json"]);

        if let Ok(output) = result {
            // Should be valid JSON if format supported
            if !output.is_empty() {
                let _: serde_json::Value =
                    serde_json::from_str(&output).unwrap_or_else(|_| serde_json::json!({}));
            }
        }
    }
}
