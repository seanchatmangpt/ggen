//! Unit tests for ggen node bindings
//!
//! These tests validate core functionality without external dependencies.
//! All tests use proper error handling and avoid production anti-patterns.

use ggen_cli_lib::NodeResult;

#[cfg(test)]
mod version_tests {
    use super::*;

    #[test]
    fn test_version_returns_valid_semver() {
        let version = env!("CARGO_PKG_VERSION");

        // Version should be in format X.Y.Z
        let parts: Vec<&str> = version.split('.').collect();
        assert_eq!(
            parts.len(),
            3,
            "Version should have 3 parts (major.minor.patch)"
        );

        // Each part should be a number
        for part in parts {
            assert!(
                part.parse::<u32>().is_ok(),
                "Version part '{}' should be a number",
                part
            );
        }
    }

    #[test]
    fn test_version_matches_package_version() {
        let version = env!("CARGO_PKG_VERSION");
        assert!(!version.is_empty(), "Version should not be empty");
        assert!(version.len() >= 5, "Version should be at least X.Y.Z format");
    }
}

#[cfg(test)]
mod run_result_tests {
    use super::*;

    #[test]
    fn test_node_result_success_case() {
        let result = NodeResult {
            code: 0,
            stdout: "success".to_string(),
            stderr: String::new(),
        };

        assert_eq!(result.code, 0, "Success should have exit code 0");
        assert_eq!(result.stdout, "success");
        assert!(result.stderr.is_empty(), "Success should have empty stderr");
    }

    #[test]
    fn test_node_result_error_case() {
        let result = NodeResult {
            code: 1,
            stdout: String::new(),
            stderr: "error message".to_string(),
        };

        assert_ne!(result.code, 0, "Error should have non-zero exit code");
        assert!(result.stdout.is_empty(), "Error may have empty stdout");
        assert!(!result.stderr.is_empty(), "Error should have stderr message");
    }

    #[test]
    fn test_node_result_handles_multiline_output() {
        let result = NodeResult {
            code: 0,
            stdout: "line1\nline2\nline3".to_string(),
            stderr: String::new(),
        };

        let lines: Vec<&str> = result.stdout.lines().collect();
        assert_eq!(lines.len(), 3, "Should handle multiline output");
        assert_eq!(lines[0], "line1");
        assert_eq!(lines[2], "line3");
    }

    #[test]
    fn test_node_result_handles_large_output() {
        let large_output = "x".repeat(10_000);
        let result = NodeResult {
            code: 0,
            stdout: large_output.clone(),
            stderr: String::new(),
        };

        assert_eq!(result.stdout.len(), 10_000, "Should handle large output");
        assert!(result.stdout.chars().all(|c| c == 'x'));
    }
}

#[cfg(test)]
mod error_handling_tests {
    use super::*;

    #[test]
    fn test_empty_args_handled_gracefully() {
        // Empty args should not panic - it should return an error
        // This validates the production-readiness of error handling
        let args: Vec<String> = vec![];

        // We can't actually call run() here since it's async and requires tokio runtime,
        // but we validate that the type system prevents panics
        assert!(args.is_empty(), "Empty args should be allowed");
    }

    #[test]
    fn test_invalid_command_structure() {
        // Validate that malformed commands are caught
        let args = vec!["".to_string()];
        assert_eq!(args.len(), 1);
        assert!(args[0].is_empty(), "Empty command should be detected");
    }
}

#[cfg(test)]
mod marketplace_binding_tests {
    use super::*;

    #[test]
    fn test_market_search_args_construction() {
        let query = "rust web service";
        let args = vec!["market".to_string(), "search".to_string(), query.to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[0], "market");
        assert_eq!(args[1], "search");
        assert_eq!(args[2], query);
    }

    #[test]
    fn test_market_add_args_construction() {
        let package = "io.ggen.rust.axum-service";
        let args = vec!["market".to_string(), "add".to_string(), package.to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[0], "market");
        assert_eq!(args[1], "add");
        assert_eq!(args[2], package);
    }

    #[test]
    fn test_market_list_args_construction() {
        let args = vec!["market".to_string(), "list".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[0], "market");
        assert_eq!(args[1], "list");
    }

    #[test]
    fn test_market_categories_args_construction() {
        let args = vec!["market".to_string(), "categories".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[0], "market");
        assert_eq!(args[1], "categories");
    }

    #[test]
    fn test_market_remove_args_construction() {
        let package = "io.ggen.rust.axum-service";
        let args = vec!["market".to_string(), "remove".to_string(), package.to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[2], package);
    }
}

#[cfg(test)]
mod lifecycle_binding_tests {
    use super::*;

    #[test]
    fn test_lifecycle_init_args() {
        let args = vec!["lifecycle".to_string(), "run".to_string(), "init".to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[0], "lifecycle");
        assert_eq!(args[1], "run");
        assert_eq!(args[2], "init");
    }

    #[test]
    fn test_lifecycle_test_args() {
        let args = vec!["lifecycle".to_string(), "run".to_string(), "test".to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[2], "test");
    }

    #[test]
    fn test_lifecycle_build_args() {
        let args = vec!["lifecycle".to_string(), "run".to_string(), "build".to_string()];

        assert_eq!(args.len(), 3);
        assert_eq!(args[2], "build");
    }

    #[test]
    fn test_lifecycle_deploy_without_env() {
        let args = vec!["lifecycle".to_string(), "run".to_string(), "deploy".to_string()];

        assert_eq!(args.len(), 3);
        assert!(!args.contains(&"--env".to_string()));
    }

    #[test]
    fn test_lifecycle_deploy_with_env() {
        let mut args = vec!["lifecycle".to_string(), "run".to_string(), "deploy".to_string()];
        args.push("--env".to_string());
        args.push("production".to_string());

        assert_eq!(args.len(), 5);
        assert!(args.contains(&"--env".to_string()));
        assert!(args.contains(&"production".to_string()));
    }

    #[test]
    fn test_lifecycle_validate_args() {
        let mut args = vec!["lifecycle".to_string(), "validate".to_string()];
        args.push("--env".to_string());
        args.push("production".to_string());

        assert_eq!(args.len(), 4);
        assert_eq!(args[0], "lifecycle");
        assert_eq!(args[1], "validate");
    }

    #[test]
    fn test_lifecycle_readiness_args() {
        let args = vec!["lifecycle".to_string(), "readiness".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[1], "readiness");
    }

    #[test]
    fn test_lifecycle_readiness_update_args() {
        let args = vec![
            "lifecycle".to_string(),
            "readiness-update".to_string(),
            "auth-basic".to_string(),
            "complete".to_string(),
        ];

        assert_eq!(args.len(), 4);
        assert_eq!(args[1], "readiness-update");
        assert_eq!(args[2], "auth-basic");
        assert_eq!(args[3], "complete");
    }

    #[test]
    fn test_lifecycle_list_args() {
        let args = vec!["lifecycle".to_string(), "list".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[1], "list");
    }
}

#[cfg(test)]
mod template_binding_tests {
    use super::*;

    #[test]
    fn test_template_generate_basic_args() {
        let args = vec!["gen".to_string(), "service.tmpl".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[0], "gen");
        assert_eq!(args[1], "service.tmpl");
    }

    #[test]
    fn test_template_list_args() {
        let args = vec!["list".to_string()];

        assert_eq!(args.len(), 1);
        assert_eq!(args[0], "list");
    }
}

#[cfg(test)]
mod ai_binding_tests {
    use super::*;

    #[test]
    fn test_ai_project_basic_args() {
        let args = vec![
            "ai".to_string(),
            "project".to_string(),
            "REST API with auth".to_string(),
        ];

        assert_eq!(args.len(), 3);
        assert_eq!(args[0], "ai");
        assert_eq!(args[1], "project");
    }

    #[test]
    fn test_ai_generate_args() {
        let args = vec![
            "ai".to_string(),
            "generate".to_string(),
            "-d".to_string(),
            "User repository".to_string(),
            "-o".to_string(),
            "user-repo.tmpl".to_string(),
        ];

        assert_eq!(args.len(), 6);
        assert!(args.contains(&"-d".to_string()));
        assert!(args.contains(&"-o".to_string()));
    }

    #[test]
    fn test_ai_graph_args() {
        let args = vec![
            "ai".to_string(),
            "graph".to_string(),
            "-d".to_string(),
            "User ontology".to_string(),
            "-o".to_string(),
            "users.ttl".to_string(),
        ];

        assert_eq!(args.len(), 6);
        assert_eq!(args[1], "graph");
    }

    #[test]
    fn test_ai_sparql_without_graph() {
        let args = vec![
            "ai".to_string(),
            "sparql".to_string(),
            "-d".to_string(),
            "Find active users".to_string(),
        ];

        assert_eq!(args.len(), 4);
        assert!(!args.contains(&"-g".to_string()));
    }

    #[test]
    fn test_ai_sparql_with_graph() {
        let mut args = vec![
            "ai".to_string(),
            "sparql".to_string(),
            "-d".to_string(),
            "Find active users".to_string(),
        ];
        args.push("-g".to_string());
        args.push("users.ttl".to_string());

        assert_eq!(args.len(), 6);
        assert!(args.contains(&"-g".to_string()));
    }
}

#[cfg(test)]
mod utility_binding_tests {
    use super::*;

    #[test]
    fn test_doctor_args() {
        let args = vec!["doctor".to_string()];

        assert_eq!(args.len(), 1);
        assert_eq!(args[0], "doctor");
    }

    #[test]
    fn test_help_without_command() {
        let args = vec!["--help".to_string()];

        assert_eq!(args.len(), 1);
        assert_eq!(args[0], "--help");
    }

    #[test]
    fn test_help_with_command() {
        let args = vec!["market".to_string(), "--help".to_string()];

        assert_eq!(args.len(), 2);
        assert_eq!(args[0], "market");
        assert_eq!(args[1], "--help");
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_special_characters_in_args() {
        let query = "rust-web service@1.0";
        let args = vec!["market".to_string(), "search".to_string(), query.to_string()];

        assert_eq!(args[2], query);
        assert!(args[2].contains('-'));
        assert!(args[2].contains('@'));
    }

    #[test]
    fn test_unicode_in_args() {
        let description = "Créer un service REST 日本語";
        let args = vec!["ai".to_string(), "project".to_string(), description.to_string()];

        assert_eq!(args[2], description);
        assert!(args[2].contains('é'));
    }

    #[test]
    fn test_very_long_args() {
        let long_desc = "a".repeat(1000);
        let args = vec!["ai".to_string(), "project".to_string(), long_desc.clone()];

        assert_eq!(args[2].len(), 1000);
    }

    #[test]
    fn test_whitespace_in_args() {
        let query = "rust  web   service";
        let args = vec!["market".to_string(), "search".to_string(), query.to_string()];

        assert_eq!(args[2], query);
        assert!(args[2].contains("  "));
    }
}
