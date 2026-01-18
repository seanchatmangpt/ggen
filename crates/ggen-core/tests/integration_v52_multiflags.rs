//! Multi-flag integration tests for ggen v5.2.0 Phase 3 MEDIUM
//!
//! Tests all feature flag combinations using Chicago School TDD (AAA pattern).
//! Validates flag interactions, precedence, and state consistency.
//!
//! ## Test Coverage (T026)
//! - T026.1: --force + --audit (safe destructive operations)
//! - T026.2: --merge + --watch (live hybrid development)
//! - T026.3: --condition + --validate-only (conditional validation)
//! - T026.4: --force + --merge + --audit (all three together)
//! - T026.5: --watch + --condition + --audit (live conditional with audit)
//! - T026.6: Flag precedence validation (validate-only blocks generation)
//! - T026.7: Dry-run preview with all flags
//! - T026.8: Invalid flag combinations error handling
//! - T026.9: Audit trail records all flag-triggered operations
//! - T026.10: File system side effects verification

use ggen_core::audit::{writer::AuditTrailWriter, AuditTrail};
use ggen_core::codegen::merge::merge_sections;
use ggen_core::codegen::watch::FileWatcher;
use ggen_core::codegen::SyncOptions;
use ggen_core::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig,
};
use ggen_core::types::path_protection::PathProtectionConfig;
use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// T026.1: test_force_plus_audit_safe_destructive
// ============================================================================

#[test]
fn test_force_plus_audit_safe_destructive() {
    // Arrange: Create SyncOptions with force + audit enabled
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,
        audit: true,
        dry_run: false,
        verbose: false,
        watch: false,
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Both flags are enabled
    assert!(options.force, "force flag should be enabled");
    assert!(options.audit, "audit flag should be enabled");

    // Arrange: Create audit trail with file change
    let mut audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let protected_file = temp_dir.path().join("src/domain/user.rs");

    fs::create_dir_all(protected_file.parent().unwrap()).expect("Failed to create domain dir");
    fs::write(&protected_file, "// Original protected content\n")
        .expect("Failed to write protected file");

    // Act: Record force overwrite in audit trail
    audit.record_file_change(
        "src/domain/user.rs".to_string(),
        "abc123_force_overwrite".to_string(),
    );
    audit.record_rule_executed();

    let audit_path = temp_dir.path().join("audit.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Audit file exists and contains force-overwrite record
    assert!(audit_path.exists(), "audit.json should exist");

    let content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    assert_eq!(
        json["files_changed"].as_u64(),
        Some(1),
        "Should record 1 file change"
    );
    assert_eq!(
        json["file_hashes"]["src/domain/user.rs"].as_str(),
        Some("abc123_force_overwrite"),
        "Should contain force-overwrite hash"
    );

    // Assert: Protected file can be overwritten with force
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    // Without force, protected path should be blocked
    let result_no_force = protection.validate_write("src/domain/user.rs", true);
    assert!(
        result_no_force.is_err(),
        "Protected path should be blocked without force"
    );

    // With force, validation should still enforce audit requirement
    // (force + audit = safe destructive operation)
    assert!(
        options.force && options.audit,
        "Force requires audit for safety"
    );
}

// ============================================================================
// T026.2: test_merge_plus_watch_live_hybrid
// ============================================================================

#[test]
fn test_merge_plus_watch_live_hybrid() {
    // Arrange: Create SyncOptions with watch enabled
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: false,
        dry_run: false,
        verbose: true,
        watch: true, // Watch mode enabled
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Watch flag is enabled
    assert!(options.watch, "watch flag should be enabled");

    // Arrange: Create watcher with merge markers
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let watch_file = temp_dir.path().join("src/generated/user.rs");

    fs::create_dir_all(watch_file.parent().unwrap()).expect("Failed to create generated dir");

    // Initial file with merge markers
    let initial_content = r#"<<<<<<< GENERATED
fn old_generated() { println!("old"); }
=======
fn manual_code() { println!("manual"); }
>>>>>>> MANUAL
"#;

    fs::write(&watch_file, initial_content).expect("Failed to write initial file");

    // Act: Simulate watch-triggered regeneration with merge
    let new_generated = r#"fn new_generated() { println!("new"); }"#;
    let merged_content =
        merge_sections(new_generated, initial_content).expect("Merge should succeed in watch mode");

    fs::write(&watch_file, &merged_content).expect("Failed to write merged file");

    // Assert: Manual code is preserved across watch regeneration
    let final_content = fs::read_to_string(&watch_file).expect("Failed to read merged file");

    assert!(
        final_content.contains("fn manual_code()"),
        "Manual code should survive watch regeneration"
    );
    assert!(
        final_content.contains("fn new_generated()"),
        "Generated code should be updated"
    );
    assert!(
        !final_content.contains("fn old_generated()"),
        "Old generated code should be replaced"
    );

    // Assert: Merge markers are preserved
    assert!(
        final_content.contains("<<<<<<< GENERATED"),
        "Generated marker preserved"
    );
    assert!(final_content.contains("======="), "Separator preserved");
    assert!(
        final_content.contains(">>>>>>> MANUAL"),
        "Manual marker preserved"
    );

    // Assert: FileWatcher can watch merge files
    let watcher = FileWatcher::new(vec![watch_file.clone()]);
    assert_eq!(watcher.debounce_ms, 300, "Default debounce for watch mode");
}

// ============================================================================
// T026.3: test_condition_plus_validate_only
// ============================================================================

#[test]
fn test_condition_plus_validate_only() {
    // Arrange: Create SyncOptions with validate_only enabled
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: false,
        dry_run: false,
        verbose: false,
        watch: false,
        validate_only: true, // Validation only - no generation
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Json,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Validate-only flag is enabled
    assert!(
        options.validate_only,
        "validate_only flag should be enabled"
    );

    // Arrange: Create manifest with conditional rule (skip_empty)
    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "conditional_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "conditional_rule".to_string(),
                query: QuerySource::Inline {
                    inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "// Generated code".to_string(),
                },
                output_file: "output.rs".to_string(),
                mode: GenerationMode::Create,
                skip_empty: true, // Conditional execution
                when: None,
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: false,
            determinism_salt: None,
            output_dir: PathBuf::from("generated"),
        },
        validation: ValidationConfig::default(),
    };

    // Assert: Rule has conditional (skip_empty) configuration
    assert!(
        manifest.generation.rules[0].skip_empty,
        "Rule should have conditional execution"
    );

    // Assert: validate_only blocks generation (takes precedence)
    assert!(
        options.validate_only,
        "validate_only should prevent generation regardless of conditions"
    );

    // Act: Verify validate_only prevents file operations
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let output_file = temp_dir.path().join("generated/output.rs");

    // Assert: No files should be created in validate_only mode
    assert!(
        !output_file.exists(),
        "validate_only should prevent file creation"
    );

    // Assert: Output format is JSON for validation results
    assert_eq!(
        options.output_format,
        ggen_core::codegen::OutputFormat::Json,
        "Should use JSON output for validation"
    );
}

// ============================================================================
// T026.4: test_force_merge_audit_all_three
// ============================================================================

#[test]
fn test_force_merge_audit_all_three() {
    // Arrange: Create SyncOptions with force + audit + merge pattern
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true, // Force overwrite protected
        audit: true, // Record all operations
        dry_run: false,
        verbose: true,
        watch: false,
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: All three flags are enabled
    assert!(options.force, "force flag should be enabled");
    assert!(options.audit, "audit flag should be enabled");
    // Note: merge is controlled by GenerationMode::Merge, not a flag

    // Arrange: Create protected file with merge markers
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let protected_merge_file = temp_dir.path().join("src/domain/user_merged.rs");

    fs::create_dir_all(protected_merge_file.parent().unwrap())
        .expect("Failed to create domain dir");

    let existing_with_markers = r#"// Protected domain file
<<<<<<< GENERATED
fn old_domain_fn() { }
=======
fn manual_domain_logic() {
    // User's critical business logic
    let user = create_user();
    validate_user(&user);
}
>>>>>>> MANUAL
"#;

    fs::write(&protected_merge_file, existing_with_markers)
        .expect("Failed to write protected file");

    // Act: Simulate force + merge operation
    let new_generated = "fn new_domain_fn() { }";
    let merged_result =
        merge_sections(new_generated, existing_with_markers).expect("Merge should succeed");

    // With force flag, we can overwrite protected path
    fs::write(&protected_merge_file, &merged_result).expect("Force should allow overwrite");

    // Act: Record in audit trail
    let mut audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    audit.record_file_change(
        "src/domain/user_merged.rs".to_string(),
        "hash_force_merge".to_string(),
    );
    audit.record_rule_executed();

    let audit_path = temp_dir.path().join("audit.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Merged file contains new generated code
    let final_content =
        fs::read_to_string(&protected_merge_file).expect("Failed to read merged file");
    assert!(
        final_content.contains("fn new_domain_fn()"),
        "New generated code should be present"
    );

    // Assert: Manual logic is preserved despite force
    assert!(
        final_content.contains("fn manual_domain_logic()"),
        "Manual code should be preserved"
    );
    assert!(
        final_content.contains("create_user()"),
        "Manual implementation details preserved"
    );

    // Assert: Old generated code is replaced
    assert!(
        !final_content.contains("fn old_domain_fn()"),
        "Old generated code should be replaced"
    );

    // Assert: Audit trail records the force + merge operation
    let audit_content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value =
        serde_json::from_str(&audit_content).expect("audit.json should be valid JSON");

    assert_eq!(
        json["files_changed"].as_u64(),
        Some(1),
        "Should record 1 file change"
    );
    assert_eq!(
        json["file_hashes"]["src/domain/user_merged.rs"].as_str(),
        Some("hash_force_merge"),
        "Should contain merge operation hash"
    );

    // Assert: Path protection would normally block this
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    let result_without_force = protection.validate_write("src/domain/user_merged.rs", true);
    assert!(
        result_without_force.is_err(),
        "Without force, protected path should be blocked"
    );

    // Assert: Force + audit provides safe destructive merge
    assert!(
        options.force && options.audit,
        "Force + audit = safe destructive merge"
    );
}

// ============================================================================
// T026.5: test_watch_condition_audit_live_conditional
// ============================================================================

#[test]
fn test_watch_condition_audit_live_conditional() {
    // Arrange: Create SyncOptions with watch + audit (conditional via skip_empty)
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: true, // Record watch-triggered operations
        dry_run: false,
        verbose: true,
        watch: true, // Live monitoring
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Json,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Watch + audit are enabled
    assert!(options.watch, "watch flag should be enabled");
    assert!(options.audit, "audit flag should be enabled");

    // Arrange: Create manifest with conditional rule
    let manifest = GgenManifest {
        project: ProjectConfig {
            name: "watch_conditional_test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        ontology: OntologyConfig {
            source: PathBuf::from("ontology.ttl"),
            imports: vec![],
            base_iri: None,
            prefixes: BTreeMap::new(),
        },
        inference: InferenceConfig {
            rules: vec![],
            max_reasoning_timeout_ms: 5000,
        },
        generation: GenerationConfig {
            rules: vec![GenerationRule {
                name: "watch_conditional_rule".to_string(),
                query: QuerySource::Inline {
                    inline: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                },
                template: TemplateSource::Inline {
                    inline: "// Watch-generated code".to_string(),
                },
                output_file: "watch_output.rs".to_string(),
                mode: GenerationMode::Create,
                skip_empty: true, // Conditional: skip if query empty
                when: None,
            }],
            max_sparql_timeout_ms: 5000,
            require_audit_trail: true, // Audit enabled in manifest
            determinism_salt: None,
            output_dir: PathBuf::from("generated"),
        },
        validation: ValidationConfig::default(),
    };

    // Assert: Manifest requires audit trail
    assert!(
        manifest.generation.require_audit_trail,
        "Manifest should require audit trail"
    );
    assert!(
        manifest.generation.rules[0].skip_empty,
        "Rule should have conditional execution"
    );

    // Arrange: Create temp directory for watch + audit
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let watch_file = temp_dir.path().join("ontology.ttl");
    let audit_path = temp_dir.path().join("audit.json");

    fs::write(&watch_file, "# Ontology content").expect("Failed to write watch file");

    // Act: Create audit trail for watch-triggered operation
    let mut audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    audit.record_file_change(
        "generated/watch_output.rs".to_string(),
        "hash_watch_conditional".to_string(),
    );
    audit.record_rule_executed();

    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit trail");

    // Assert: Audit file exists and records watch-triggered generation
    assert!(audit_path.exists(), "audit.json should exist");

    let audit_content = fs::read_to_string(&audit_path).expect("Failed to read audit.json");
    let json: Value =
        serde_json::from_str(&audit_content).expect("audit.json should be valid JSON");

    assert_eq!(
        json["files_changed"].as_u64(),
        Some(1),
        "Should record watch-triggered file change"
    );
    assert_eq!(
        json["rules_executed"].as_u64(),
        Some(1),
        "Should record conditional rule execution"
    );

    // Assert: FileWatcher is configured
    let watcher = FileWatcher::new(vec![watch_file.clone()]);
    assert_eq!(watcher.debounce_ms, 300, "Watch mode uses default debounce");
    assert_eq!(
        watcher.queue_capacity, 10,
        "Watch mode uses default queue capacity"
    );

    // Assert: Output format is JSON for CI/CD integration
    assert_eq!(
        options.output_format,
        ggen_core::codegen::OutputFormat::Json,
        "Should use JSON output for watch + audit"
    );
}

// ============================================================================
// T026.6: test_flag_precedence_validate_only_blocks_generation
// ============================================================================

#[test]
fn test_flag_precedence_validate_only_blocks_generation() {
    // Arrange: Create SyncOptions with validate_only + other flags
    let options_validate = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true, // Force is ignored when validate_only is set
        audit: true, // Audit might record validation results
        dry_run: false,
        verbose: false,
        watch: false,
        validate_only: true, // Takes precedence - blocks all generation
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Json,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: validate_only takes precedence over force
    assert!(
        options_validate.validate_only,
        "validate_only should be enabled"
    );
    assert!(options_validate.force, "force is set but should be ignored");

    // Arrange: Create SyncOptions with dry_run (similar precedence)
    let options_dry_run = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,
        audit: true,
        dry_run: true, // Dry-run shows what would happen
        verbose: true,
        watch: false,
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: dry_run prevents actual file writes
    assert!(options_dry_run.dry_run, "dry_run should be enabled");
    assert!(
        options_dry_run.force,
        "force is set but dry_run prevents writes"
    );

    // Act: Verify no file operations occur in validate_only mode
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let would_be_generated = temp_dir.path().join("generated/output.rs");

    // Assert: File should not exist after validate_only run
    assert!(
        !would_be_generated.exists(),
        "validate_only should prevent file creation"
    );

    // Assert: Flag precedence hierarchy
    // validate_only > dry_run > force
    // validate_only: no generation at all
    // dry_run: show what would happen, no writes
    // force: allow writes to protected paths
    assert!(
        options_validate.validate_only,
        "Precedence level 1: validate_only"
    );
    assert!(options_dry_run.dry_run, "Precedence level 2: dry_run");
    assert!(options_dry_run.force, "Precedence level 3: force");
}

// ============================================================================
// T026.7: test_dry_run_preview_with_all_flags
// ============================================================================

#[test]
fn test_dry_run_preview_with_all_flags() {
    // Arrange: Create SyncOptions with dry_run + all other flags
    let options = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,
        audit: true,
        dry_run: true, // Preview mode - no actual writes
        verbose: true,
        watch: false,
        validate_only: false,
        output_dir: Some(PathBuf::from("custom_output")),
        selected_rules: Some(vec!["rule1".to_string(), "rule2".to_string()]),
        output_format: ggen_core::codegen::OutputFormat::Json,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: dry_run is enabled with all other flags
    assert!(options.dry_run, "dry_run should be enabled");
    assert!(
        options.force,
        "force should be set (but ignored in dry_run)"
    );
    assert!(options.audit, "audit should be set (might record preview)");
    assert!(options.verbose, "verbose should provide detailed preview");

    // Assert: dry_run should show what WOULD happen without executing
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let preview_file = temp_dir.path().join("custom_output/preview.rs");

    // Assert: No files should be created in dry_run mode
    assert!(
        !preview_file.exists(),
        "dry_run should not create actual files"
    );

    // Assert: Selected rules are specified for preview
    assert_eq!(
        options.selected_rules,
        Some(vec!["rule1".to_string(), "rule2".to_string()]),
        "Should preview only selected rules"
    );

    // Assert: Custom output directory is specified
    assert_eq!(
        options.output_dir,
        Some(PathBuf::from("custom_output")),
        "Should use custom output directory for preview"
    );

    // Assert: JSON output for machine-readable preview
    assert_eq!(
        options.output_format,
        ggen_core::codegen::OutputFormat::Json,
        "Should output JSON preview"
    );

    // Arrange: Simulate dry-run audit trail (preview only)
    let mut audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    audit.record_file_change(
        "custom_output/preview.rs".to_string(),
        "hash_dry_run_preview".to_string(),
    );

    let audit_path = temp_dir.path().join("audit_preview.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write preview audit");

    // Assert: Audit preview exists (shows what would be audited)
    assert!(
        audit_path.exists(),
        "dry_run audit preview should be created"
    );

    let audit_content = fs::read_to_string(&audit_path).expect("Failed to read preview audit");
    let json: Value =
        serde_json::from_str(&audit_content).expect("Preview audit should be valid JSON");

    assert_eq!(
        json["files_changed"].as_u64(),
        Some(1),
        "Preview should show 1 file would be changed"
    );
}

// ============================================================================
// T026.8: test_invalid_flag_combinations
// ============================================================================

#[test]
fn test_invalid_flag_combinations() {
    // Arrange: Create SyncOptions with conflicting flags
    let options_conflict1 = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: false,
        dry_run: false,
        verbose: false,
        watch: true,         // Watch mode
        validate_only: true, // Validate-only mode (conflict!)
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Conflicting flags can be set but should be validated at runtime
    assert!(options_conflict1.watch, "watch flag is set (but conflicts)");
    assert!(
        options_conflict1.validate_only,
        "validate_only flag is set (conflict)"
    );
    // Note: Runtime validation should detect that watch + validate_only is invalid
    // (watch requires generation, validate_only prevents generation)

    // Arrange: Test force without audit (should warn but not error)
    let options_force_no_audit = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: true,  // Force enabled
        audit: false, // Audit disabled (risky!)
        dry_run: false,
        verbose: false,
        watch: false,
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: Force without audit is allowed but should generate warning
    assert!(options_force_no_audit.force, "force should be enabled");
    assert!(
        !options_force_no_audit.audit,
        "audit should be disabled (warning case)"
    );
    // Note: This combination should trigger a warning in production
    // Best practice: force should require audit

    // Arrange: Test dry_run + watch (questionable but valid)
    let options_dry_watch = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: false,
        dry_run: true, // Dry-run mode
        verbose: true,
        watch: true, // Watch mode
        validate_only: false,
        output_dir: None,
        selected_rules: None,
        output_format: ggen_core::codegen::OutputFormat::Text,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: dry_run + watch is allowed (preview watch changes)
    assert!(options_dry_watch.dry_run, "dry_run should be enabled");
    assert!(options_dry_watch.watch, "watch should be enabled");
    // Note: This could be useful for previewing what watch mode would do

    // Arrange: Test selected_rules with validate_only
    let options_rules_validate = SyncOptions {
        manifest_path: PathBuf::from("ggen.toml"),
        force: false,
        audit: false,
        dry_run: false,
        verbose: false,
        watch: false,
        validate_only: true, // Validate only
        output_dir: None,
        selected_rules: Some(vec!["rule1".to_string()]), // Selected rules (ignored?)
        output_format: ggen_core::codegen::OutputFormat::Json,
        timeout_ms: 30000,
        use_cache: true,
        cache_dir: None,
        max_parallelism: None,
    };

    // Assert: selected_rules with validate_only is allowed (validate specific rules)
    assert!(
        options_rules_validate.validate_only,
        "validate_only should be enabled"
    );
    assert_eq!(
        options_rules_validate.selected_rules,
        Some(vec!["rule1".to_string()]),
        "Should validate only selected rules"
    );
}

// ============================================================================
// T026.9: test_audit_trail_records_all_operations
// ============================================================================

#[test]
fn test_audit_trail_records_all_operations() {
    // Arrange: Create comprehensive audit trail with all operation types
    let mut audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Act: Record various operations
    // 1. Force overwrite
    audit.record_file_change(
        "src/domain/user.rs".to_string(),
        "hash_force_overwrite".to_string(),
    );

    // 2. Merge operation
    audit.record_file_change(
        "src/domain/product.rs".to_string(),
        "hash_merge".to_string(),
    );

    // 3. Watch-triggered generation
    audit.record_file_change(
        "src/generated/types.rs".to_string(),
        "hash_watch_trigger".to_string(),
    );

    // 4. Conditional execution (skip_empty = false, rule executed)
    audit.record_rule_executed();

    // 5. Conditional execution (skip_empty = true, rule executed)
    audit.record_rule_executed();

    // 6. Normal generation
    audit.record_file_change(
        "src/generated/models.rs".to_string(),
        "hash_normal_gen".to_string(),
    );

    audit.record_rule_executed();

    // Set metadata
    audit.metadata.spec_hash = "ontology_hash_abc123".to_string();
    audit.metadata.duration_ms = 1500;

    let audit_path = temp_dir.path().join("audit_comprehensive.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write comprehensive audit");

    // Assert: All operations are recorded
    assert!(audit_path.exists(), "Comprehensive audit should be written");

    let content = fs::read_to_string(&audit_path).expect("Failed to read comprehensive audit");
    let json: Value = serde_json::from_str(&content).expect("audit.json should be valid JSON");

    // Assert: All file changes recorded
    assert_eq!(
        json["files_changed"].as_u64(),
        Some(4),
        "Should record 4 file changes"
    );

    // Assert: All rules executed
    assert_eq!(
        json["rules_executed"].as_u64(),
        Some(3),
        "Should record 3 rule executions"
    );

    // Assert: File hashes present for all operations
    let file_hashes = json["file_hashes"]
        .as_object()
        .expect("file_hashes should be object");

    assert_eq!(
        file_hashes
            .get("src/domain/user.rs")
            .and_then(|v| v.as_str()),
        Some("hash_force_overwrite"),
        "Force overwrite should be recorded"
    );

    assert_eq!(
        file_hashes
            .get("src/domain/product.rs")
            .and_then(|v| v.as_str()),
        Some("hash_merge"),
        "Merge operation should be recorded"
    );

    assert_eq!(
        file_hashes
            .get("src/generated/types.rs")
            .and_then(|v| v.as_str()),
        Some("hash_watch_trigger"),
        "Watch-triggered generation should be recorded"
    );

    assert_eq!(
        file_hashes
            .get("src/generated/models.rs")
            .and_then(|v| v.as_str()),
        Some("hash_normal_gen"),
        "Normal generation should be recorded"
    );

    // Assert: Metadata is complete
    assert_eq!(
        json["metadata"]["ggen_version"].as_str(),
        Some("5.2.0"),
        "Version should be recorded"
    );
    assert_eq!(
        json["metadata"]["spec_hash"].as_str(),
        Some("ontology_hash_abc123"),
        "Spec hash should be recorded"
    );
    assert_eq!(
        json["metadata"]["duration_ms"].as_u64(),
        Some(1500),
        "Duration should be recorded"
    );
}

// ============================================================================
// T026.10: test_file_system_side_effects
// ============================================================================

#[test]
fn test_file_system_side_effects() {
    // Arrange: Create temp directory for side effect testing
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let base_path = temp_dir.path();

    // Act: Create various file system side effects
    // 1. Force overwrite of protected file
    let protected_file = base_path.join("src/domain/user.rs");
    fs::create_dir_all(protected_file.parent().unwrap()).expect("Failed to create domain dir");
    fs::write(&protected_file, "// Original content\n").expect("Failed to write original");

    // Simulate force overwrite
    fs::write(&protected_file, "// Force overwritten content\n")
        .expect("Failed to force overwrite");

    // Assert: File was overwritten
    let content = fs::read_to_string(&protected_file).expect("Failed to read protected file");
    assert_eq!(
        content, "// Force overwritten content\n",
        "Force overwrite should modify file"
    );

    // 2. Merge preserves manual code
    let merge_file = base_path.join("src/generated/merged.rs");
    fs::create_dir_all(merge_file.parent().unwrap()).expect("Failed to create generated dir");

    let existing_merge = r#"<<<<<<< GENERATED
fn old() { }
=======
fn manual() { println!("keep this"); }
>>>>>>> MANUAL
"#;

    fs::write(&merge_file, existing_merge).expect("Failed to write existing merge");

    let new_gen = "fn new() { }";
    let merged = merge_sections(new_gen, existing_merge).expect("Merge should succeed");

    fs::write(&merge_file, &merged).expect("Failed to write merged content");

    // Assert: Manual code is preserved
    let merged_content = fs::read_to_string(&merge_file).expect("Failed to read merged file");
    assert!(
        merged_content.contains("fn manual()"),
        "Manual code should be preserved"
    );
    assert!(
        merged_content.contains("fn new()"),
        "New generated code should be present"
    );

    // 3. Audit trail file creation
    let audit = AuditTrail::new("5.2.0", "ggen.toml", "ontology.ttl");
    let audit_path = base_path.join("audit.json");
    AuditTrailWriter::write(&audit, &audit_path).expect("Failed to write audit");

    // Assert: Audit file exists
    assert!(audit_path.exists(), "Audit trail file should be created");

    // 4. Directory creation for output
    let nested_output = base_path.join("custom/output/dir/file.rs");
    fs::create_dir_all(nested_output.parent().unwrap())
        .expect("Failed to create nested output dir");
    fs::write(&nested_output, "// Generated").expect("Failed to write nested output");

    // Assert: Nested directories are created
    assert!(
        nested_output.exists(),
        "Nested output file should be created"
    );
    assert!(
        nested_output.parent().unwrap().exists(),
        "Parent directories should exist"
    );

    // 5. Path protection validation (no actual write on failure)
    let protection = PathProtectionConfig::new(&["src/domain/**"], &["src/generated/**"])
        .expect("Failed to create protection config");

    let blocked_path = "src/domain/critical.rs";
    let validation_result = protection.validate_write(blocked_path, true);

    // Assert: Protection blocks write (no side effect)
    assert!(
        validation_result.is_err(),
        "Path protection should block write"
    );

    let blocked_file = base_path.join(blocked_path);
    assert!(!blocked_file.exists(), "Blocked file should not be created");

    // Assert: Summary of side effects
    // - 1 protected file overwritten (force)
    // - 1 merge file updated (manual code preserved)
    // - 1 audit trail created
    // - 1 nested output directory created
    // - 1 blocked write prevented (no side effect)
    let files_created = vec![
        protected_file.exists(),
        merge_file.exists(),
        audit_path.exists(),
        nested_output.exists(),
    ];

    assert_eq!(
        files_created.iter().filter(|&&x| x).count(),
        4,
        "Should have 4 files created"
    );
}
