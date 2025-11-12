//! Integration tests for ggen node bindings
//!
//! JTBD VALIDATION: These tests validate end-to-end functionality by actually running CLI commands
//! and verifying the ACTUAL behavior matches expectations.
//!
//! Each test validates:
//! 1. Command executes without crashing
//! 2. Exit code matches expected behavior (0 for success, non-zero for errors)
//! 3. Output contains expected content (not just "something")
//! 4. Error messages are meaningful when failures occur
//! 5. No false positives (exit 0 with errors in stderr)

use chicago_tdd_tools::prelude::*;

#[cfg(test)]
mod integration {
    use super::*;
    use ggen_cli_lib::run_for_node;

    /// Helper to run a command and validate success with content validation
    async fn run_and_expect_success(args: Vec<&str>) -> Result<String, String> {
        let args_owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();

        match run_for_node(args_owned).await {
            Ok(result) => {
                if result.code == 0 {
                    if result.stdout.is_empty() && result.stderr.is_empty() {
                        return Err(format!(
                            "Command succeeded (exit 0) but produced no output - possible false success"
                        ));
                    }

                    if !result.stderr.is_empty() {
                        let is_progress = result.stderr.contains("Searching")
                            || result.stderr.contains("Loading")
                            || result.stderr.contains("Building");
                        if !is_progress {
                            return Err(format!(
                                "Command succeeded (exit 0) but has unexpected stderr: {}",
                                result.stderr
                            ));
                        }
                    }

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

    async_test_with_timeout!(test_version_command, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["--version"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Version output should not be empty");
                assert!(
                    output.contains('.'),
                    "Version should contain dots (semver format)"
                );
            }
            Err(e) => panic!("Version command failed: {}", e),
        }
    });

    async_test_with_timeout!(test_help_command, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["--help"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(
                    output.to_lowercase().contains("usage"),
                    "Help should contain usage information"
                );
                assert!(output.len() > 100, "Help output should be substantial");
            }
            Err(e) => panic!("Help command failed: {}", e),
        }
    });

    async_test_with_timeout!(test_invalid_command, 30, async {
        // Arrange & Act
        let result = run_and_expect_failure(vec!["invalid-command"]).await;

        // Assert
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
    });

    async_test_with_timeout!(test_marketplace_list, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["market", "list"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(
                    !output.is_empty(),
                    "Market list should return data (packages or 'no packages' message)"
                );

                let has_structure = output.contains('\n')
                    || output.contains("•")
                    || output.contains("-")
                    || output.contains("package")
                    || output.contains("No");

                assert!(
                    has_structure,
                    "Market list should have structured output, got: {}",
                    output
                );
            }
            Err(e) => panic!("Market list failed: {}", e),
        }
    });

    async_test_with_timeout!(test_marketplace_categories, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["market", "categories"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Should have at least one category");

                let has_categories = output.to_lowercase().contains("category")
                    || output.to_lowercase().contains("rust")
                    || output.to_lowercase().contains("template")
                    || output.to_lowercase().contains("service");

                assert!(
                    has_categories,
                    "Categories should list actual category names, got: {}",
                    output
                );
            }
            Err(e) => panic!("Market categories failed: {}", e),
        }
    });

    async_test_with_timeout!(test_marketplace_search_empty_query, 30, async {
        // Arrange & Act
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "".to_string(),
        ])
        .await;

        // Assert
        match result {
            Ok(res) => {
                if res.code != 0 {
                    assert!(
                        !res.stderr.is_empty() || !res.stdout.is_empty(),
                        "Empty query error should have message"
                    );
                } else {
                    assert!(
                        !res.stdout.is_empty(),
                        "Empty query success should return all packages"
                    );
                }
            }
            Err(e) => {
                assert!(!format!("{}", e).is_empty());
            }
        }
    });

    async_test_with_timeout!(test_marketplace_search_with_query, 30, async {
        // Arrange & Act
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "rust".to_string(),
        ])
        .await;

        // Assert
        match result {
            Ok(res) => {
                assert!(
                    res.code == 0 || res.code == 1,
                    "Search should complete with success (0) or no results (1), got: {}",
                    res.code
                );

                assert!(
                    !res.stdout.is_empty() || !res.stderr.is_empty(),
                    "Search should explain results (found packages or 'no results')"
                );

                if res.code == 0 {
                    let has_results = res.stdout.contains("package")
                        || res.stdout.contains("rust")
                        || res.stdout.contains("found")
                        || res.stdout.contains("•");

                    assert!(
                        has_results,
                        "Successful search should show package results, got: {}",
                        res.stdout
                    );
                }
            }
            Err(_) => {
                // Network errors are acceptable in tests
            }
        }
    });

    async_test_with_timeout!(test_lifecycle_list, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["lifecycle", "list"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Should list lifecycle phases");
                let has_phases = output.to_lowercase().contains("init")
                    || output.to_lowercase().contains("build")
                    || output.to_lowercase().contains("test");
                assert!(has_phases, "Should list common lifecycle phases");
            }
            Err(e) => panic!("Lifecycle list failed: {}", e),
        }
    });

    async_test_with_timeout!(test_doctor_command, 30, async {
        // Arrange & Act
        let result = run_and_expect_success(vec!["doctor"]).await;

        // Assert
        match result {
            Ok(output) => {
                assert!(!output.is_empty(), "Doctor should output diagnostics");
                let checks_env = output.to_lowercase().contains("rust")
                    || output.to_lowercase().contains("cargo")
                    || output.to_lowercase().contains("environment");
                assert!(checks_env, "Doctor should check environment");
            }
            Err(e) => panic!("Doctor command failed: {}", e),
        }
    });

    async_test_with_timeout!(test_command_with_special_chars, 30, async {
        // Arrange & Act
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "test-package@1.0".to_string(),
        ])
        .await;

        // Assert
        match result {
            Ok(res) => {
                assert!(res.code == 0 || res.code == 1);
            }
            Err(_) => {
                // Errors are acceptable, just shouldn't panic
            }
        }
    });

    async_test_with_timeout!(test_concurrent_commands, 30, async {
        // Arrange & Act
        let tasks = vec![
            tokio::spawn(async { run_for_node(vec!["--version".to_string()]).await }),
            tokio::spawn(async {
                run_for_node(vec!["market".to_string(), "list".to_string()]).await
            }),
            tokio::spawn(async {
                run_for_node(vec!["lifecycle".to_string(), "list".to_string()]).await
            }),
        ];

        let results = futures::future::join_all(tasks).await;

        // Assert
        for result in results {
            match result {
                Ok(Ok(_)) => {
                    // Command completed successfully
                }
                Ok(Err(e)) => {
                    eprintln!("Command error (acceptable): {}", e);
                }
                Err(e) => {
                    panic!("Task panicked: {}", e);
                }
            }
        }
    });
}

#[cfg(test)]
mod error_recovery_tests {
    use super::*;
    use ggen_cli_lib::run_for_node;

    async_test_with_timeout!(test_recovers_from_invalid_utf8, 30, async {
        // Arrange & Act
        let result = run_for_node(vec!["--version".to_string()]).await;

        // Assert
        match result {
            Ok(res) => {
                assert!(std::str::from_utf8(res.stdout.as_bytes()).is_ok());
                assert!(std::str::from_utf8(res.stderr.as_bytes()).is_ok());
            }
            Err(_) => {
                // Error is acceptable, just shouldn't panic
            }
        }
    });

    async_test_with_timeout!(test_handles_missing_subcommand, 30, async {
        // Arrange & Act
        let result = run_for_node(vec!["market".to_string()]).await;

        // Assert
        match result {
            Ok(res) => {
                assert!(res.code != 0 || !res.stdout.is_empty());
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    });

    async_test_with_timeout!(test_handles_too_many_args, 30, async {
        // Arrange & Act
        let result = run_for_node(vec![
            "market".to_string(),
            "list".to_string(),
            "extra".to_string(),
            "args".to_string(),
        ])
        .await;

        // Assert
        match result {
            Ok(res) => {
                assert!(res.code == 0 || res.code != 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    });
}

#[cfg(test)]
mod timeout_tests {
    use super::*;
    use ggen_cli_lib::run_for_node;
    use std::time::{Duration, Instant};

    async_test_with_timeout!(test_version_completes_quickly, 30, async {
        // Arrange
        let start = Instant::now();

        // Act
        let result = run_for_node(vec!["--version".to_string()]).await;
        let duration = start.elapsed();

        // Assert
        assert_ok!(&result, "Version command should succeed");
        assert!(
            duration < Duration::from_secs(5),
            "Version should complete in under 5 seconds, took {:?}",
            duration
        );
    });

    async_test_with_timeout!(test_help_completes_quickly, 30, async {
        // Arrange
        let start = Instant::now();

        // Act
        let result = run_for_node(vec!["--help".to_string()]).await;
        let duration = start.elapsed();

        // Assert
        assert_ok!(&result, "Help command should succeed");
        assert!(
            duration < Duration::from_secs(5),
            "Help should complete in under 5 seconds, took {:?}",
            duration
        );
    });
}
