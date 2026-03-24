//! Sync Command Integration Tests - Chicago TDD
//!
//! Tests for sync operation: the core ggen pipeline command
//!
//! Chicago TDD Cycle:
//! 1. RED: Write failing test
//! 2. GREEN: Make test pass with REAL implementation
//! 3. REFACTOR: Improve code while maintaining green
//!
//! NO MOCKS - Tests against REAL ggen_core::codegen implementations

use std::path::PathBuf;

// ============================================================================
// Core Layer Imports (REAL types, NO mocks)
// ============================================================================

use ggen_core::codegen::{
    OutputFormat, SyncExecutor, SyncOptions, SyncResult, SyncedFileInfo, ValidationCheck,
};

// ============================================================================
// SyncOptions Tests
// ============================================================================

#[cfg(test)]
mod sync_options_tests {
    use super::*;

    /// Test: SyncOptions with default values
    #[test]
    fn test_sync_options_default() {
        let options = SyncOptions::default();

        assert_eq!(options.manifest_path, PathBuf::from("ggen.toml"));
        assert!(options.output_dir.is_none());
        assert!(options.cache_dir.is_none());
        assert!(!options.verbose);
        assert!(matches!(options.output_format, OutputFormat::Text));
        assert!(!options.validate_only);
        assert!(!options.dry_run);
        assert!(!options.watch);
        assert!(options.selected_rules.is_none());
        assert!(options.use_cache); // default is true
        assert!(!options.force);
        assert!(!options.audit);
        assert!(options.a2a_stage.is_none());
        assert!(options.ontology_path.is_none());
    }

    /// Test: SyncOptions::new() creates same as default
    #[test]
    fn test_sync_options_new() {
        let options = SyncOptions::new();
        let default = SyncOptions::default();

        assert_eq!(options.manifest_path, default.manifest_path);
        assert_eq!(options.output_format, default.output_format);
        assert_eq!(options.use_cache, default.use_cache);
    }

    /// Test: SyncOptions with custom manifest path
    #[test]
    fn test_sync_options_custom_manifest() {
        let mut options = SyncOptions::new();
        options.manifest_path = PathBuf::from("custom/ggen.toml");

        assert_eq!(options.manifest_path, PathBuf::from("custom/ggen.toml"));
        assert_eq!(options.manifest_path.to_str().unwrap(), "custom/ggen.toml");
    }

    /// Test: SyncOptions with output directory
    #[test]
    fn test_sync_options_with_output_dir() {
        let mut options = SyncOptions::new();
        options.output_dir = Some(PathBuf::from("generated"));

        assert_eq!(options.output_dir, Some(PathBuf::from("generated")));
    }

    /// Test: SyncOptions with cache directory
    #[test]
    fn test_sync_options_with_cache_dir() {
        let mut options = SyncOptions::new();
        options.cache_dir = Some(PathBuf::from(".ggen/cache"));

        assert_eq!(options.cache_dir, Some(PathBuf::from(".ggen/cache")));
    }

    /// Test: SyncOptions with verbose enabled
    #[test]
    fn test_sync_options_verbose() {
        let mut options = SyncOptions::new();
        options.verbose = true;

        assert!(options.verbose);
    }

    /// Test: SyncOptions with validate_only mode
    #[test]
    fn test_sync_options_validate_only() {
        let mut options = SyncOptions::new();
        options.validate_only = true;

        assert!(options.validate_only);
    }

    /// Test: SyncOptions with dry_run mode
    #[test]
    fn test_sync_options_dry_run() {
        let mut options = SyncOptions::new();
        options.dry_run = true;

        assert!(options.dry_run);
    }

    /// Test: SyncOptions with watch mode
    #[test]
    fn test_sync_options_watch() {
        let mut options = SyncOptions::new();
        options.watch = true;

        assert!(options.watch);
    }

    /// Test: SyncOptions with selected rules
    #[test]
    fn test_sync_options_selected_rules() {
        let mut options = SyncOptions::new();
        options.selected_rules = Some(vec![
            "structs".to_string(),
            "impls".to_string(),
            "traits".to_string(),
        ]);

        assert_eq!(
            options.selected_rules,
            Some(vec![
                "structs".to_string(),
                "impls".to_string(),
                "traits".to_string()
            ])
        );
    }

    /// Test: SyncOptions with use_cache disabled
    #[test]
    fn test_sync_options_no_cache() {
        let mut options = SyncOptions::new();
        options.use_cache = false;

        assert!(!options.use_cache);
    }

    /// Test: SyncOptions with force enabled
    #[test]
    fn test_sync_options_force() {
        let mut options = SyncOptions::new();
        options.force = true;

        assert!(options.force);
    }

    /// Test: SyncOptions with audit enabled
    #[test]
    fn test_sync_options_audit() {
        let mut options = SyncOptions::new();
        options.audit = true;

        assert!(options.audit);
    }

    /// Test: SyncOptions with A2A stage
    #[test]
    fn test_sync_options_a2a_stage() {
        let stages = ["μ₁", "μ₂", "μ₃", "μ₄", "μ₅"];

        for stage in stages {
            let mut options = SyncOptions::new();
            options.a2a_stage = Some(stage.to_string());

            assert_eq!(options.a2a_stage, Some(stage.to_string()));
        }
    }

    /// Test: SyncOptions with ontology path
    #[test]
    fn test_sync_options_ontology_path() {
        let mut options = SyncOptions::new();
        options.ontology_path = Some(PathBuf::from(
            ".specify/specs/014-a2a-integration/a2a-ontology.ttl",
        ));

        assert_eq!(
            options.ontology_path,
            Some(PathBuf::from(
                ".specify/specs/014-a2a-integration/a2a-ontology.ttl"
            ))
        );
    }

    /// Test: SyncOptions with Json output format
    #[test]
    fn test_sync_options_json_format() {
        let mut options = SyncOptions::new();
        options.output_format = OutputFormat::Json;

        assert!(matches!(options.output_format, OutputFormat::Json));
    }
}

// ============================================================================
// OutputFormat Tests
// ============================================================================

#[cfg(test)]
mod output_format_tests {
    use super::*;

    /// Test: OutputFormat::Text variant
    #[test]
    fn test_output_format_text() {
        let format = OutputFormat::Text;
        assert!(matches!(format, OutputFormat::Text));
    }

    /// Test: OutputFormat::Json variant
    #[test]
    fn test_output_format_json() {
        let format = OutputFormat::Json;
        assert!(matches!(format, OutputFormat::Json));
    }

    /// Test: OutputFormat default is Text
    #[test]
    fn test_output_format_default() {
        let format = OutputFormat::default();
        assert!(matches!(format, OutputFormat::Text));
    }

    /// Test: OutputFormat variants are distinct
    #[test]
    fn test_output_format_variants_distinct() {
        let text = OutputFormat::Text;
        let json = OutputFormat::Json;

        assert_ne!(text, json);
        assert!(matches!(text, OutputFormat::Text));
        assert!(matches!(json, OutputFormat::Json));
    }
}

// ============================================================================
// SyncedFileInfo Tests
// ============================================================================

#[cfg(test)]
mod synced_file_info_tests {
    use super::*;

    /// Test: SyncedFileInfo structure
    #[test]
    fn test_synced_file_info_structure() {
        let info = SyncedFileInfo {
            path: "src/lib.rs".to_string(),
            size_bytes: 1024,
            action: "created".to_string(),
        };

        assert_eq!(info.path, "src/lib.rs");
        assert_eq!(info.size_bytes, 1024);
        assert_eq!(info.action, "created");
    }

    /// Test: SyncedFileInfo with different actions
    #[test]
    fn test_synced_file_info_actions() {
        let actions = ["created", "updated", "unchanged", "would create"];

        for action in actions {
            let info = SyncedFileInfo {
                path: "test.rs".to_string(),
                size_bytes: 100,
                action: action.to_string(),
            };

            assert_eq!(info.action, *action);
        }
    }

    /// Test: SyncedFileInfo with zero bytes
    #[test]
    fn test_synced_file_info_empty_file() {
        let info = SyncedFileInfo {
            path: "empty.rs".to_string(),
            size_bytes: 0,
            action: "created".to_string(),
        };

        assert_eq!(info.size_bytes, 0);
    }

    /// Test: SyncedFileInfo with large file
    #[test]
    fn test_synced_file_info_large_file() {
        let info = SyncedFileInfo {
            path: "large.rs".to_string(),
            size_bytes: 1_048_576, // 1 MB
            action: "updated".to_string(),
        };

        assert_eq!(info.size_bytes, 1_048_576);
    }
}

// ============================================================================
// ValidationCheck Tests
// ============================================================================

#[cfg(test)]
mod validation_check_tests {
    use super::*;

    /// Test: ValidationCheck with all fields
    #[test]
    fn test_validation_check_full() {
        let check = ValidationCheck {
            check: "SHACL validation".to_string(),
            passed: true,
            details: Some("All constraints satisfied".to_string()),
        };

        assert_eq!(check.check, "SHACL validation");
        assert!(check.passed);
        assert_eq!(check.details, Some("All constraints satisfied".to_string()));
    }

    /// Test: ValidationCheck with no details
    #[test]
    fn test_validation_check_no_details() {
        let check = ValidationCheck {
            check: "Syntax check".to_string(),
            passed: true,
            details: None,
        };

        assert!(check.details.is_none());
    }

    /// Test: ValidationCheck failed
    #[test]
    fn test_validation_check_failed() {
        let check = ValidationCheck {
            check: "Type checking".to_string(),
            passed: false,
            details: Some("Type mismatch on line 42".to_string()),
        };

        assert!(!check.passed);
        assert!(check.details.unwrap().contains("Type mismatch"));
    }

    /// Test: ValidationCheck with various check names
    #[test]
    fn test_validation_check_names() {
        let checks = vec![
            "SHACL validation",
            "SPARQL query validation",
            "Template syntax check",
            "File existence check",
            "Dependency validation",
        ];

        for name in checks {
            let check = ValidationCheck {
                check: name.to_string(),
                passed: true,
                details: None,
            };

            assert_eq!(check.check, name);
        }
    }
}

// ============================================================================
// SyncResult Tests
// ============================================================================

#[cfg(test)]
mod sync_result_tests {
    use super::*;

    /// Test: SyncResult with success status
    #[test]
    fn test_sync_result_success() {
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 5,
            duration_ms: 1000,
            files: vec![SyncedFileInfo {
                path: "src/lib.rs".to_string(),
                size_bytes: 1024,
                action: "created".to_string(),
            }],
            inference_rules_executed: 10,
            generation_rules_executed: 5,
            audit_trail: None,
            error: None,
        };

        assert_eq!(result.status, "success");
        assert_eq!(result.files_synced, 5);
        assert_eq!(result.duration_ms, 1000);
        assert_eq!(result.files.len(), 1);
        assert_eq!(result.inference_rules_executed, 10);
        assert_eq!(result.generation_rules_executed, 5);
    }

    /// Test: SyncResult with error status
    #[test]
    fn test_sync_result_error() {
        let result = SyncResult {
            status: "error".to_string(),
            files_synced: 0,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: Some("Manifest file not found".to_string()),
        };

        assert_eq!(result.status, "error");
        assert_eq!(result.files_synced, 0);
        assert!(result.error.is_some());
        assert_eq!(result.error.unwrap(), "Manifest file not found");
    }

    /// Test: SyncResult with audit trail
    #[test]
    fn test_sync_result_with_audit() {
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 3,
            duration_ms: 500,
            files: vec![],
            inference_rules_executed: 5,
            generation_rules_executed: 2,
            audit_trail: Some(".ggen/audit/sync-20250208-120000.json".to_string()),
            error: None,
        };

        assert_eq!(
            result.audit_trail,
            Some(".ggen/audit/sync-20250208-120000.json".to_string())
        );
    }

    /// Test: SyncResult with multiple files
    #[test]
    fn test_sync_result_multiple_files() {
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 3,
            duration_ms: 2000,
            files: vec![
                SyncedFileInfo {
                    path: "src/agent.rs".to_string(),
                    size_bytes: 2048,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "src/message.rs".to_string(),
                    size_bytes: 3072,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "src/task.rs".to_string(),
                    size_bytes: 1536,
                    action: "updated".to_string(),
                },
            ],
            inference_rules_executed: 15,
            generation_rules_executed: 8,
            audit_trail: None,
            error: None,
        };

        assert_eq!(result.files.len(), 3);
        assert_eq!(result.files_synced, 3);
        assert_eq!(result.files[0].path, "src/agent.rs");
        assert_eq!(result.files[1].path, "src/message.rs");
        assert_eq!(result.files[2].path, "src/task.rs");
    }

    /// Test: SyncResult with zero duration
    #[test]
    fn test_sync_result_zero_duration() {
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 0,
            duration_ms: 0,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
        };

        assert_eq!(result.duration_ms, 0);
    }
}

// ============================================================================
// SyncExecutor Tests
// ============================================================================

#[cfg(test)]
mod sync_executor_tests {
    use super::*;

    /// Test: SyncExecutor can be created with options
    #[test]
    fn test_sync_executor_creation() {
        let options = SyncOptions::new();
        let executor = SyncExecutor::new(options);

        // Executor is created - we can't test internal state directly
        // but we verify it compiles and the type is correct
        let _ = executor;
    }

    /// Test: SyncExecutor with custom options
    #[test]
    fn test_sync_executor_custom_options() {
        let mut options = SyncOptions::new();
        options.manifest_path = PathBuf::from("test/ggen.toml");
        options.verbose = true;
        options.audit = true;

        let executor = SyncExecutor::new(options);
        let _ = executor;
    }

    /// Test: SyncExecutor with dry_run options
    #[test]
    fn test_sync_executor_dry_run() {
        let mut options = SyncOptions::new();
        options.dry_run = true;

        let executor = SyncExecutor::new(options);
        let _ = executor;
    }

    /// Test: SyncExecutor with force and audit
    #[test]
    fn test_sync_executor_force_audit() {
        let mut options = SyncOptions::new();
        options.force = true;
        options.audit = true;

        let executor = SyncExecutor::new(options);
        let _ = executor;
    }

    /// Test: SyncExecutor with A2A stage
    #[test]
    fn test_sync_executor_a2a_stage() {
        let mut options = SyncOptions::new();
        options.a2a_stage = Some("μ₃".to_string());
        options.ontology_path = Some(PathBuf::from(
            ".specify/specs/014-a2a-integration/a2a-ontology.ttl",
        ));

        let executor = SyncExecutor::new(options);
        let _ = executor;
    }
}

// ============================================================================
// Integration Tests (Real Types - NO Mocks)
// ============================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: Full sync options with all fields set
    #[test]
    fn test_integration_full_sync_options() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("project/ggen.toml"),
            output_dir: Some(PathBuf::from("generated")),
            cache_dir: Some(PathBuf::from(".ggen/cache")),
            verbose: true,
            output_format: OutputFormat::Json,
            validate_only: false,
            dry_run: false,
            watch: false,
            selected_rules: Some(vec!["structs".to_string(), "traits".to_string()]),
            use_cache: true,
            force: false,
            audit: true,
            a2a_stage: Some("μ₅".to_string()),
            ontology_path: Some(PathBuf::from(
                ".specify/specs/014-a2a-integration/a2a-ontology.ttl",
            )),
        };

        assert_eq!(options.manifest_path, PathBuf::from("project/ggen.toml"));
        assert!(options.verbose);
        assert!(options.audit);
        assert_eq!(options.a2a_stage, Some("μ₅".to_string()));
    }

    /// Test: Real-world A2A sync configuration
    #[test]
    fn test_integration_a2a_sync_config() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: Some(PathBuf::from("crates/a2a-generated/src")),
            cache_dir: None,
            verbose: true,
            output_format: OutputFormat::Text,
            validate_only: false,
            dry_run: false,
            watch: false,
            selected_rules: None,
            use_cache: true,
            force: false,
            audit: true,
            a2a_stage: None, // Run all stages
            ontology_path: Some(PathBuf::from(
                ".specify/specs/014-a2a-integration/a2a-ontology.ttl",
            )),
        };

        assert!(options.audit);
        assert_eq!(
            options.ontology_path,
            Some(PathBuf::from(
                ".specify/specs/014-a2a-integration/a2a-ontology.ttl"
            ))
        );
    }

    /// Test: Validation-only workflow
    #[test]
    fn test_integration_validate_only_workflow() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: None,
            cache_dir: None,
            verbose: false,
            output_format: OutputFormat::Text,
            validate_only: true, // Only validate, don't generate
            dry_run: false,
            watch: false,
            selected_rules: None,
            use_cache: false,
            force: false,
            audit: false,
            a2a_stage: None,
            ontology_path: None,
        };

        assert!(options.validate_only);
    }

    /// Test: Dry-run workflow
    #[test]
    fn test_integration_dry_run_workflow() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: Some(PathBuf::from("output")),
            cache_dir: None,
            verbose: true,
            output_format: OutputFormat::Text,
            validate_only: false,
            dry_run: true, // Preview changes
            watch: false,
            selected_rules: None,
            use_cache: false,
            force: false,
            audit: true,
            a2a_stage: None,
            ontology_path: None,
        };

        assert!(options.dry_run);
        assert!(options.audit);
    }

    /// Test: Watch mode configuration
    #[test]
    fn test_integration_watch_mode() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: None,
            cache_dir: None,
            verbose: true,
            output_format: OutputFormat::Text,
            validate_only: false,
            dry_run: false,
            watch: true, // Continuous regeneration
            selected_rules: None,
            use_cache: true,
            force: false,
            audit: false,
            a2a_stage: None,
            ontology_path: None,
        };

        assert!(options.watch);
        assert!(options.use_cache);
    }

    /// Test: CI/CD workflow (JSON output, validate-only)
    #[test]
    fn test_integration_cicd_workflow() {
        let options = SyncOptions {
            manifest_path: PathBuf::from("ggen.toml"),
            output_dir: None,
            cache_dir: None,
            verbose: false,
            output_format: OutputFormat::Json, // Machine-readable
            validate_only: true,               // Pre-flight checks
            dry_run: false,
            watch: false,
            selected_rules: None,
            use_cache: true,
            force: false,
            audit: false,
            a2a_stage: None,
            ontology_path: None,
        };

        assert!(matches!(options.output_format, OutputFormat::Json));
        assert!(options.validate_only);
    }

    /// Test: Single μ stage execution
    #[test]
    fn test_integration_single_stage_execution() {
        for stage in &["μ₁", "μ₂", "μ₃", "μ₄", "μ₅"] {
            let options = SyncOptions {
                manifest_path: PathBuf::from("ggen.toml"),
                output_dir: None,
                cache_dir: None,
                verbose: true,
                output_format: OutputFormat::Text,
                validate_only: false,
                dry_run: false,
                watch: false,
                selected_rules: None,
                use_cache: false,
                force: false,
                audit: false,
                a2a_stage: Some(stage.to_string()),
                ontology_path: Some(PathBuf::from(
                    ".specify/specs/014-a2a-integration/a2a-ontology.ttl",
                )),
            };

            assert_eq!(options.a2a_stage, Some(stage.to_string()));
        }
    }

    /// Test: Complex sync result with all fields
    #[test]
    fn test_integration_complex_sync_result() {
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 6,
            duration_ms: 2340,
            files: vec![
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/agent.rs".to_string(),
                    size_bytes: 2450,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/message.rs".to_string(),
                    size_bytes: 3100,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/task.rs".to_string(),
                    size_bytes: 2800,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/transport.rs".to_string(),
                    size_bytes: 1200,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/skill.rs".to_string(),
                    size_bytes: 4500,
                    action: "created".to_string(),
                },
                SyncedFileInfo {
                    path: "crates/a2a-generated/src/lib.rs".to_string(),
                    size_bytes: 1800,
                    action: "updated".to_string(),
                },
            ],
            inference_rules_executed: 124,
            generation_rules_executed: 62,
            audit_trail: Some(".ggen/receipts/a2a-20250208-143022.json".to_string()),
            error: None,
        };

        assert_eq!(result.status, "success");
        assert_eq!(result.files_synced, 6);
        assert_eq!(result.files.len(), 6);
        assert_eq!(result.inference_rules_executed, 124);
        assert_eq!(result.generation_rules_executed, 62);
        assert!(result.audit_trail.is_some());
        assert!(result.audit_trail.unwrap().contains("a2a"));
    }

    /// Test: Selected rules filtering
    #[test]
    fn test_integration_selected_rules() {
        let rule_sets = vec![
            vec!["structs".to_string()],
            vec!["structs".to_string(), "impls".to_string()],
            vec![
                "structs".to_string(),
                "impls".to_string(),
                "traits".to_string(),
                "enums".to_string(),
            ],
        ];

        for rules in rule_sets {
            let options = SyncOptions {
                manifest_path: PathBuf::from("ggen.toml"),
                output_dir: None,
                cache_dir: None,
                verbose: false,
                output_format: OutputFormat::Text,
                validate_only: false,
                dry_run: false,
                watch: false,
                selected_rules: Some(rules.clone()),
                use_cache: false,
                force: false,
                audit: false,
                a2a_stage: None,
                ontology_path: None,
            };

            assert_eq!(options.selected_rules, Some(rules));
        }
    }
}
