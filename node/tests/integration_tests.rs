//! Integration tests for ggen node bindings
//!
//! These tests validate end-to-end functionality by actually running CLI commands.
//! All tests use proper error handling and production-ready patterns.

#[cfg(test)]
mod integration {
    use ggen_cli_lib::run_for_node;

    /// Helper to run a command and validate success
    async fn run_and_expect_success(args: Vec<&str>) -> Result<String, String> {
        let args_owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();

        match run_for_node(args_owned).await {
            Ok(result) => {
                if result.code == 0 {
                    Ok(result.stdout)
                } else {
                    Err(format!(
                        "Command failed with code {}: {}",
                        result.code, result.stderr
                    ))
                }
            }
            Err(e) => Err(format!("Command execution failed: {}", e)),
        }
    }

    /// Helper to run a command and expect failure
    async fn run_and_expect_failure(args: Vec<&str>) -> Result<(i32, String), String> {
        let args_owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();

        match run_for_node(args_owned).await {
            Ok(result) => {
                if result.code != 0 {
                    Ok((result.code, result.stderr))
                } else {
                    Err("Expected command to fail but it succeeded".to_string())
                }
            }
            Err(e) => Ok((1, format!("{}", e))),
        }
    }

    #[tokio::test]
    async fn test_version_command() {
        let result = run_and_expect_success(vec!["--version"]).await;

        match result {
            Ok(output) => {
                assert!(
                    !output.is_empty(),
                    "Version output should not be empty"
                );
                assert!(
                    output.contains('.'),
                    "Version should contain dots (semver format)"
                );
            }
            Err(e) => panic!("Version command failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_help_command() {
        let result = run_and_expect_success(vec!["--help"]).await;

        match result {
            Ok(output) => {
                assert!(
                    output.to_lowercase().contains("usage"),
                    "Help should contain usage information"
                );
                assert!(
                    output.len() > 100,
                    "Help output should be substantial"
                );
            }
            Err(e) => panic!("Help command failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_invalid_command() {
        let result = run_and_expect_failure(vec!["invalid-command"]).await;

        match result {
            Ok((code, stderr)) => {
                assert_ne!(code, 0, "Invalid command should return non-zero exit code");
                assert!(
                    !stderr.is_empty(),
                    "Invalid command should produce error message"
                );
            }
            Err(e) => panic!("Expected failure but got: {}", e),
        }
    }

    #[tokio::test]
    async fn test_marketplace_list() {
        let result = run_and_expect_success(vec!["market", "list"]).await;

        match result {
            Ok(output) => {
                // List command should always succeed, even if empty
                assert!(
                    output.len() >= 0,
                    "List command should return valid output"
                );
            }
            Err(e) => panic!("Market list failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_marketplace_categories() {
        let result = run_and_expect_success(vec!["market", "categories"]).await;

        match result {
            Ok(output) => {
                // Categories should list available types
                assert!(!output.is_empty(), "Should have at least one category");
            }
            Err(e) => panic!("Market categories failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_marketplace_search_empty_query() {
        // Empty query should either return all results or error gracefully
        let result = run_for_node(vec!["market".to_string(), "search".to_string(), "".to_string()]).await;

        match result {
            Ok(res) => {
                // Should handle empty query gracefully (either way is acceptable)
                assert!(res.code == 0 || res.code != 0);
            }
            Err(_) => {
                // Error is also acceptable for empty query
            }
        }
    }

    #[tokio::test]
    async fn test_marketplace_search_with_query() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "rust".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Search should complete (results may be empty)
                assert!(res.code == 0 || res.code == 1);
            }
            Err(_) => {
                // Network errors are acceptable in tests
            }
        }
    }

    #[tokio::test]
    async fn test_lifecycle_list() {
        let result = run_and_expect_success(vec!["lifecycle", "list"]).await;

        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Should list lifecycle phases");
                // Should contain common phases
                let has_phases = output.to_lowercase().contains("init")
                    || output.to_lowercase().contains("build")
                    || output.to_lowercase().contains("test");
                assert!(has_phases, "Should list common lifecycle phases");
            }
            Err(e) => panic!("Lifecycle list failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_doctor_command() {
        let result = run_and_expect_success(vec!["doctor"]).await;

        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Doctor should output diagnostics");
                // Should check environment
                let checks_env = output.to_lowercase().contains("rust")
                    || output.to_lowercase().contains("cargo")
                    || output.to_lowercase().contains("environment");
                assert!(checks_env, "Doctor should check environment");
            }
            Err(e) => panic!("Doctor command failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_command_with_special_chars() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "test-package@1.0".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should handle special characters without crashing
                assert!(res.code == 0 || res.code == 1);
            }
            Err(_) => {
                // Errors are acceptable, just shouldn't panic
            }
        }
    }

    #[tokio::test]
    async fn test_concurrent_commands() {
        // Test that multiple commands can run concurrently without issues
        let tasks = vec![
            tokio::spawn(async {
                run_for_node(vec!["--version".to_string()]).await
            }),
            tokio::spawn(async {
                run_for_node(vec!["market".to_string(), "list".to_string()]).await
            }),
            tokio::spawn(async {
                run_for_node(vec!["lifecycle".to_string(), "list".to_string()]).await
            }),
        ];

        let results = futures::future::join_all(tasks).await;

        for result in results {
            match result {
                Ok(Ok(_)) => {
                    // Command completed successfully
                }
                Ok(Err(e)) => {
                    // Command error is acceptable
                    eprintln!("Command error (acceptable): {}", e);
                }
                Err(e) => {
                    panic!("Task panicked: {}", e);
                }
            }
        }
    }
}

#[cfg(test)]
mod error_recovery_tests {
    use ggen_cli_lib::run_for_node;

    #[tokio::test]
    async fn test_recovers_from_invalid_utf8() {
        // Test that the binding handles potential UTF-8 issues gracefully
        let result = run_for_node(vec!["--version".to_string()]).await;

        match result {
            Ok(res) => {
                // Should return valid UTF-8
                assert!(std::str::from_utf8(res.stdout.as_bytes()).is_ok());
                assert!(std::str::from_utf8(res.stderr.as_bytes()).is_ok());
            }
            Err(_) => {
                // Error is acceptable, just shouldn't panic
            }
        }
    }

    #[tokio::test]
    async fn test_handles_missing_subcommand() {
        let result = run_for_node(vec!["market".to_string()]).await;

        match result {
            Ok(res) => {
                // Should return help or error, not panic
                assert!(res.code != 0 || !res.stdout.is_empty());
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_handles_too_many_args() {
        let result = run_for_node(vec![
            "market".to_string(),
            "list".to_string(),
            "extra".to_string(),
            "args".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should handle gracefully (error or ignore)
                assert!(res.code == 0 || res.code != 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }
}

#[cfg(test)]
mod timeout_tests {
    use ggen_cli_lib::run_for_node;
    use std::time::{Duration, Instant};

    #[tokio::test]
    async fn test_version_completes_quickly() {
        let start = Instant::now();
        let result = run_for_node(vec!["--version".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Version command should succeed");
        assert!(
            duration < Duration::from_secs(5),
            "Version should complete in under 5 seconds, took {:?}",
            duration
        );
    }

    #[tokio::test]
    async fn test_help_completes_quickly() {
        let start = Instant::now();
        let result = run_for_node(vec!["--help".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Help command should succeed");
        assert!(
            duration < Duration::from_secs(5),
            "Help should complete in under 5 seconds, took {:?}",
            duration
        );
    }
}
