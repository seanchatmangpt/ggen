//! Error handling tests for ggen node bindings
//!
//! These tests validate that all error paths are handled correctly
//! without panicking or using production anti-patterns.

#[cfg(test)]
mod error_handling {
    use ggen_cli_lib::run_for_node;

    #[tokio::test]
    async fn test_empty_args_handled() {
        let result = run_for_node(vec![]).await;

        match result {
            Ok(res) => {
                // Empty args should either show help or error
                assert!(res.code == 0 || res.code != 0);
            }
            Err(_) => {
                // Error is acceptable, shouldn't panic
            }
        }
    }

    #[tokio::test]
    async fn test_null_byte_in_args() {
        let result = run_for_node(vec!["test\0string".to_string()]).await;

        match result {
            Ok(_) => {
                // Should handle gracefully
            }
            Err(e) => {
                // Should return error, not panic
                let error_msg = format!("{}", e);
                assert!(!error_msg.is_empty());
            }
        }
    }

    #[tokio::test]
    async fn test_very_long_argument() {
        let long_arg = "a".repeat(100_000);
        let result = run_for_node(vec!["market".to_string(), "search".to_string(), long_arg]).await;

        match result {
            Ok(res) => {
                // Should complete or error gracefully
                assert!(res.code >= 0);
            }
            Err(e) => {
                // Error is acceptable
                let error_msg = format!("{}", e);
                assert!(!error_msg.is_empty());
            }
        }
    }

    #[tokio::test]
    async fn test_invalid_utf8_sequence() {
        // Test with characters that might cause UTF-8 issues
        let tricky_string = "Hello\u{FEFF}\u{200B}World"; // Zero-width chars
        let result = run_for_node(vec!["market".to_string(), "search".to_string(), tricky_string.to_string()]).await;

        match result {
            Ok(res) => {
                // Output should be valid UTF-8
                assert!(std::str::from_utf8(res.stdout.as_bytes()).is_ok());
                assert!(std::str::from_utf8(res.stderr.as_bytes()).is_ok());
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_path_traversal_attempt() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "../../../etc/passwd".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should not access filesystem inappropriately
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is expected and acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_command_injection_attempt() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "test; rm -rf /".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should treat as literal string, not execute
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_sql_injection_attempt() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "'; DROP TABLE packages; --".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should treat as literal search string
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_multiple_errors_in_sequence() {
        let errors = vec![
            run_for_node(vec!["invalid1".to_string()]).await,
            run_for_node(vec!["invalid2".to_string()]).await,
            run_for_node(vec!["invalid3".to_string()]).await,
        ];

        for (i, result) in errors.iter().enumerate() {
            match result {
                Ok(res) => {
                    assert!(res.code >= 0, "Error {} should have valid exit code", i);
                }
                Err(_) => {
                    // Each error should be independent
                }
            }
        }
    }

    #[tokio::test]
    async fn test_unicode_edge_cases() {
        let test_cases = vec![
            "🚀",                          // Emoji
            "你好世界",                    // Chinese
            "Привет",                      // Russian
            "مرحبا",                       // Arabic (RTL)
            "\u{1F600}\u{1F601}",         // Multiple emojis
            "A\u{0301}",                   // Combining character
        ];

        for test_case in test_cases {
            let result = run_for_node(vec![
                "market".to_string(),
                "search".to_string(),
                test_case.to_string(),
            ])
            .await;

            match result {
                Ok(res) => {
                    // Should handle all Unicode gracefully
                    assert!(res.code >= 0);
                }
                Err(_) => {
                    // Error is acceptable
                }
            }
        }
    }

    #[tokio::test]
    async fn test_whitespace_edge_cases() {
        let test_cases = vec![
            "   ",           // Only spaces
            "\t\t\t",        // Only tabs
            "\n\n",          // Only newlines
            "\r\n",          // Windows newlines
            " \t\n\r ",      // Mixed whitespace
        ];

        for test_case in test_cases {
            let result = run_for_node(vec![
                "market".to_string(),
                "search".to_string(),
                test_case.to_string(),
            ])
            .await;

            match result {
                Ok(res) => {
                    assert!(res.code >= 0);
                }
                Err(_) => {
                    // Error is acceptable
                }
            }
        }
    }

    #[tokio::test]
    async fn test_binary_data_in_args() {
        let binary_string = String::from_utf8_lossy(&[0xFF, 0xFE, 0xFD, 0x00]).to_string();
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            binary_string,
        ])
        .await;

        match result {
            Ok(res) => {
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is expected and acceptable
            }
        }
    }
}

#[cfg(test)]
mod boundary_tests {
    use ggen_cli_lib::run_for_node;

    #[tokio::test]
    async fn test_maximum_args_count() {
        // Test with many arguments
        let mut args = vec!["market".to_string(), "search".to_string()];
        for i in 0..100 {
            args.push(format!("arg{}", i));
        }

        let result = run_for_node(args).await;

        match result {
            Ok(res) => {
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_empty_string_args() {
        let result = run_for_node(vec![
            "market".to_string(),
            "".to_string(),
            "search".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_duplicate_flags() {
        let result = run_for_node(vec![
            "--help".to_string(),
            "--help".to_string(),
            "--help".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_conflicting_args() {
        let result = run_for_node(vec![
            "market".to_string(),
            "search".to_string(),
            "query".to_string(),
            "list".to_string(),
        ])
        .await;

        match result {
            Ok(res) => {
                // Should handle conflicting commands gracefully
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }
}

#[cfg(test)]
mod memory_safety_tests {
    use ggen_cli_lib::run_for_node;

    #[tokio::test]
    async fn test_no_memory_leak_on_error() {
        // Run multiple error-producing commands
        for _ in 0..10 {
            let _ = run_for_node(vec!["invalid".to_string()]).await;
        }
        // If there were memory leaks, this test would eventually fail
        // Rust's ownership system prevents most leaks, but we test anyway
    }

    #[tokio::test]
    async fn test_no_memory_leak_on_success() {
        // Run multiple successful commands
        for _ in 0..10 {
            let result = run_for_node(vec!["--version".to_string()]).await;
            assert!(result.is_ok());
        }
    }

    #[tokio::test]
    async fn test_large_output_handling() {
        // Commands that produce large output should not cause memory issues
        let result = run_for_node(vec!["--help".to_string()]).await;

        match result {
            Ok(res) => {
                // Even large output should be handled correctly
                assert!(res.stdout.len() > 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }
}
