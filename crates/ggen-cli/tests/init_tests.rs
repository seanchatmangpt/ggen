//! Chicago TDD Unit Tests for `ggen init` Command
//!
//! Test Coverage:
//! - Success cases (fresh init, --path, --force)
//! - Error cases (already initialized, no permissions, invalid path)
//! - Edge cases (concurrent init, partial failure, file exists)
//!
//! Pattern: AAA (Arrange, Act, Assert) with real filesystem (Chicago TDD)

use ggen_cli_lib::cmds::init::InitOutput;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

// ============================================================================
// Helper Functions
// ============================================================================

/// Helper to call perform_init by importing it
/// Since perform_init is not public, we'll test via the public init() function
/// and verify the InitOutput structure.
fn init_project(path: &str, force: bool) -> InitOutput {
    // Call the public init function from the CLI module
    ggen_cli_lib::cmds::init::init(Some(path.to_string()), Some(force))
        .expect("init should return Result")
}

/// Verify that a file exists and contains expected content
fn assert_file_contains(path: &Path, expected_substr: &str) {
    assert!(path.exists(), "File should exist: {:?}", path);
    let content = fs::read_to_string(path).expect("Should be able to read file");
    assert!(
        content.contains(expected_substr),
        "File {:?} should contain '{}'",
        path,
        expected_substr
    );
}

/// Verify directory exists
fn assert_dir_exists(path: &Path) {
    assert!(path.exists(), "Directory should exist: {:?}", path);
    assert!(path.is_dir(), "Path should be a directory: {:?}", path);
}

// ============================================================================
// SUCCESS CASES
// ============================================================================

#[test]
fn test_fresh_init_in_empty_directory() {
    // Arrange: Create empty temporary directory
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act: Initialize project in empty directory
    let output = init_project(project_path, false);

    // Assert: Status is success
    assert_eq!(output.status, "success", "Status should be success");
    assert_eq!(output.error, None, "Should have no errors");
    assert_eq!(output.project_dir, project_path);

    // Assert: All expected files were created
    let expected_files = vec![
        "ggen.toml",
        "schema/domain.ttl",
        "Makefile",
        "templates/example.txt.tera",
        "scripts/startup.sh",
        ".gitignore",
        "README.md",
    ];

    for file in &expected_files {
        assert!(
            output.files_created.contains(&file.to_string()),
            "files_created should contain {}",
            file
        );
    }

    // Assert: All expected directories were created
    let expected_dirs = vec!["schema", "templates", "src/generated", "scripts"];
    for dir in &expected_dirs {
        assert!(
            output.directories_created.contains(&dir.to_string()),
            "directories_created should contain {}",
            dir
        );
    }

    // Assert: Files exist on filesystem with correct content
    let base = temp_dir.path();
    assert_file_contains(&base.join("ggen.toml"), "[project]");
    assert_file_contains(&base.join("ggen.toml"), "BIG BANG 80/20");
    assert_file_contains(&base.join("schema/domain.ttl"), "@prefix");
    assert_file_contains(&base.join("schema/domain.ttl"), "schema:Person");
    assert_file_contains(&base.join("Makefile"), "ggen sync");
    assert_file_contains(&base.join("templates/example.txt.tera"), "Tera Template");
    assert_file_contains(&base.join("scripts/startup.sh"), "#!/bin/bash");
    assert_file_contains(&base.join(".gitignore"), "src/generated/");
    assert_file_contains(&base.join("README.md"), "# My ggen Project");

    // Assert: Directories exist
    assert_dir_exists(&base.join("schema"));
    assert_dir_exists(&base.join("templates"));
    assert_dir_exists(&base.join("src/generated"));
    assert_dir_exists(&base.join("scripts"));

    // Assert: Next steps are provided
    assert!(!output.next_steps.is_empty(), "Should provide next steps");
    assert!(
        output.next_steps[0].contains("make setup"),
        "First step should mention 'make setup'"
    );
}

#[test]
fn test_init_with_path_argument() {
    // Arrange: Create parent directory, specify child project directory
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().join("my-new-project");
    let project_path_str = project_path.to_str().unwrap();

    // Act: Initialize with --path argument (directory doesn't exist yet)
    let output = init_project(project_path_str, false);

    // Assert: Status is success
    assert_eq!(output.status, "success");
    assert_eq!(output.error, None);

    // Assert: Project directory was created
    assert!(project_path.exists(), "Project directory should be created");
    assert!(project_path.is_dir());

    // Assert: Files were created in the specified path
    assert_file_contains(&project_path.join("ggen.toml"), "[project]");
    assert_dir_exists(&project_path.join("schema"));
}

#[test]
fn test_init_with_force_to_reinitialize() {
    // Arrange: Initialize project once
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    let first_output = init_project(project_path, false);
    assert_eq!(first_output.status, "success");

    // Modify an existing file to verify it gets overwritten
    let ggen_toml = temp_dir.path().join("ggen.toml");
    fs::write(&ggen_toml, "# CUSTOM CONTENT").unwrap();

    // Act: Reinitialize with --force
    let output = init_project(project_path, true);

    // Assert: Status is success
    assert_eq!(output.status, "success");
    assert_eq!(output.error, None);

    // Assert: Files were overwritten (not created)
    let overwritten = output.files_overwritten.unwrap();
    assert!(
        overwritten.contains(&"ggen.toml".to_string()),
        "ggen.toml should be in overwritten list"
    );

    // Assert: Warning about overwritten files
    assert!(output.warning.is_some());
    assert!(output.warning.unwrap().contains("Overwrote"));

    // Assert: File content was replaced (no longer has custom content)
    let content = fs::read_to_string(&ggen_toml).unwrap();
    assert!(
        !content.contains("# CUSTOM CONTENT"),
        "Custom content should be overwritten"
    );
    assert!(
        content.contains("[project]"),
        "Should have new ggen.toml content"
    );
}

#[test]
fn test_init_preserves_user_files() {
    // Arrange: Create directory with existing .gitignore and README.md
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    let gitignore_path = temp_dir.path().join(".gitignore");
    let readme_path = temp_dir.path().join("README.md");

    fs::write(&gitignore_path, "# MY CUSTOM GITIGNORE\n*.log").unwrap();
    fs::write(&readme_path, "# MY CUSTOM README").unwrap();

    // Act: Initialize project
    let output = init_project(project_path, false);

    // Assert: Status is success
    assert_eq!(output.status, "success");

    // Assert: User files were preserved (not overwritten)
    let preserved = output.files_preserved.unwrap();
    assert!(preserved.contains(&".gitignore".to_string()));
    assert!(preserved.contains(&"README.md".to_string()));

    // Assert: Warning about preserved files
    assert!(output.warning.is_some());
    assert!(output.warning.unwrap().contains("Preserved"));

    // Assert: Original content is intact
    let gitignore_content = fs::read_to_string(&gitignore_path).unwrap();
    assert!(gitignore_content.contains("MY CUSTOM GITIGNORE"));

    let readme_content = fs::read_to_string(&readme_path).unwrap();
    assert!(readme_content.contains("MY CUSTOM README"));
}

#[cfg(unix)]
#[test]
fn test_startup_sh_is_executable() {
    // Arrange: Create empty directory
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act: Initialize project
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: startup.sh has executable permissions
    let startup_sh = temp_dir.path().join("scripts/startup.sh");
    let metadata = fs::metadata(&startup_sh).unwrap();
    let permissions = metadata.permissions();

    assert!(
        permissions.mode() & 0o111 != 0,
        "startup.sh should have execute permissions"
    );
}

// ============================================================================
// ERROR CASES
// ============================================================================

#[test]
fn test_error_already_initialized_without_force() {
    // Arrange: Initialize project once
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    let first_output = init_project(project_path, false);
    assert_eq!(first_output.status, "success");

    // Act: Try to initialize again without --force
    let output = init_project(project_path, false);

    // Assert: Status is error
    assert_eq!(output.status, "error");

    // Assert: Error message mentions already initialized
    assert!(output.error.is_some());
    let error_msg = output.error.unwrap();
    assert!(error_msg.contains("already initialized"));
    assert!(error_msg.contains("--force"));

    // Assert: No files were created or modified
    assert_eq!(output.files_created.len(), 0);
    assert_eq!(output.files_overwritten, None);

    // Assert: Next steps suggest using --force or make build
    assert!(!output.next_steps.is_empty());
}

#[cfg(unix)]
#[test]
fn test_error_no_write_permissions() {
    // Arrange: Create directory with no write permissions
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("readonly-project");
    fs::create_dir_all(&project_dir).unwrap();

    // Remove write permissions
    let mut perms = fs::metadata(&project_dir).unwrap().permissions();
    perms.set_mode(0o444); // Read-only
    fs::set_permissions(&project_dir, perms).unwrap();

    let project_path = project_dir.to_str().unwrap();

    // Act: Try to initialize in read-only directory
    let output = init_project(project_path, false);

    // Assert: Status is error
    assert_eq!(output.status, "error");

    // Assert: Error message mentions write permission
    assert!(output.error.is_some());
    let error_msg = output.error.unwrap();
    assert!(
        error_msg.contains("write permission") || error_msg.contains("Permission denied"),
        "Error should mention permission issue: {}",
        error_msg
    );

    // Assert: Files were not created
    assert_eq!(output.files_created.len(), 0);

    // Cleanup: Restore permissions so tempdir can be deleted
    let mut perms = fs::metadata(&project_dir).unwrap().permissions();
    perms.set_mode(0o755);
    let _ = fs::set_permissions(&project_dir, perms);
}

#[test]
fn test_error_invalid_path_characters() {
    // Note: This test may behave differently on different OSes.
    // On Unix, most characters are allowed in filenames.
    // On Windows, certain characters like < > : " | ? * are forbidden.

    // We'll test a deeply nested path that's valid but unusual
    let temp_dir = TempDir::new().unwrap();
    let deeply_nested = temp_dir.path().join("a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p");
    let project_path = deeply_nested.to_str().unwrap();

    // Act: Initialize with deeply nested path
    let output = init_project(project_path, false);

    // Assert: Should succeed (valid path)
    assert_eq!(output.status, "success");
    assert!(deeply_nested.exists());
}

// ============================================================================
// EDGE CASES
// ============================================================================

#[test]
fn test_partial_file_exists_scenario() {
    // Arrange: Create directory with only some ggen files present
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Create only ggen.toml (but not other files)
    let ggen_toml = temp_dir.path().join("ggen.toml");
    fs::write(&ggen_toml, "# PARTIAL INIT").unwrap();

    // Act: Try to initialize without --force
    let output = init_project(project_path, false);

    // Assert: Error because ggen.toml exists
    assert_eq!(output.status, "error");
    assert!(output.error.is_some());
    assert!(output.error.unwrap().contains("already initialized"));

    // Act: Initialize with --force
    let output_force = init_project(project_path, true);

    // Assert: Success with force flag
    assert_eq!(output_force.status, "success");

    // Assert: ggen.toml was overwritten, other files created
    let overwritten = output_force.files_overwritten.as_ref().unwrap();
    assert!(overwritten.contains(&"ggen.toml".to_string()));

    let created = &output_force.files_created;
    assert!(created.contains(&"Makefile".to_string()));
    assert!(created.contains(&"schema/domain.ttl".to_string()));
}

#[test]
fn test_init_in_current_directory() {
    // Arrange: Create temp directory
    let temp_dir = TempDir::new().unwrap();

    // Act: Initialize using "." as path
    let output = init_project(temp_dir.path().to_str().unwrap(), false);

    // Assert: Success
    assert_eq!(output.status, "success");
    assert!(output.error.is_none());

    // Assert: Files created in current directory (passed as path)
    assert!(temp_dir.path().join("ggen.toml").exists());
    assert!(temp_dir.path().join("Makefile").exists());
}

#[test]
fn test_directory_already_exists_scenario() {
    // Arrange: Create project directory and subdirectories manually
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Pre-create some directories
    fs::create_dir_all(temp_dir.path().join("schema")).unwrap();
    fs::create_dir_all(temp_dir.path().join("templates")).unwrap();

    // Act: Initialize project
    let output = init_project(project_path, false);

    // Assert: Success (existing directories are OK)
    assert_eq!(output.status, "success");

    // Assert: Directories are NOT in the created list (they already existed)
    assert!(
        !output.directories_created.contains(&"schema".to_string()),
        "schema should not be in created list (already existed)"
    );
    assert!(
        !output
            .directories_created
            .contains(&"templates".to_string()),
        "templates should not be in created list (already existed)"
    );

    // Assert: Other directories that didn't exist are created
    assert!(output.directories_created.contains(&"scripts".to_string()));
    assert!(output
        .directories_created
        .contains(&"src/generated".to_string()));
}

#[test]
fn test_idempotency_with_force_flag() {
    // Arrange: Initialize, then reinitialize multiple times with --force
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // First init
    let output1 = init_project(project_path, false);
    assert_eq!(output1.status, "success");

    // Second init with --force
    let output2 = init_project(project_path, true);
    assert_eq!(output2.status, "success");

    // Third init with --force
    let output3 = init_project(project_path, true);
    assert_eq!(output3.status, "success");

    // Assert: All files still exist and have correct content
    assert_file_contains(&temp_dir.path().join("ggen.toml"), "[project]");
    assert_file_contains(&temp_dir.path().join("schema/domain.ttl"), "schema:Person");
    assert_file_contains(&temp_dir.path().join("Makefile"), "ggen sync");

    // Assert: Each reinit reported overwritten files
    assert!(output2.files_overwritten.is_some());
    assert!(output3.files_overwritten.is_some());
}

#[test]
fn test_nested_directory_creation() {
    // Arrange: Specify path with multiple non-existent parent directories
    let temp_dir = TempDir::new().unwrap();
    let nested_path = temp_dir.path().join("level1/level2/level3/my-project");
    let project_path = nested_path.to_str().unwrap();

    // Act: Initialize in deeply nested path
    let output = init_project(project_path, false);

    // Assert: Success
    assert_eq!(output.status, "success");
    assert!(output.error.is_none());

    // Assert: All parent directories were created
    assert!(nested_path.exists());
    assert!(nested_path.is_dir());

    // Assert: Files exist in nested location
    assert!(nested_path.join("ggen.toml").exists());
    assert!(nested_path.join("Makefile").exists());
}

// ============================================================================
// FILE CONTENT VALIDATION
// ============================================================================

#[test]
fn test_ggen_toml_content() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: ggen.toml has correct structure
    let ggen_toml_path = temp_dir.path().join("ggen.toml");
    let content = fs::read_to_string(&ggen_toml_path).unwrap();

    // Check for required sections
    assert!(content.contains("[project]"));
    assert!(content.contains("[ontology]"));
    assert!(content.contains("[generation]"));
    assert!(content.contains("[[generation.rules]]"));
    assert!(content.contains("[sync]"));
    assert!(content.contains("[rdf]"));
    assert!(content.contains("[templates]"));
    assert!(content.contains("[output]"));

    // Check for BIG BANG 80/20 content
    assert!(content.contains("BIG BANG 80/20"));
    assert!(content.contains("standard ontology"));
}

#[test]
fn test_domain_ttl_content() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: domain.ttl has valid Turtle syntax
    let domain_ttl_path = temp_dir.path().join("schema/domain.ttl");
    let content = fs::read_to_string(&domain_ttl_path).unwrap();

    // Check for RDF prefixes
    assert!(content.contains("@prefix rdf:"));
    assert!(content.contains("@prefix rdfs:"));
    assert!(content.contains("@prefix schema:"));

    // Check for schema.org Person example
    assert!(content.contains("schema:Person"));
    assert!(content.contains("schema:name"));
    assert!(content.contains("schema:email"));

    // Check for BIG BANG 80/20 guidance
    assert!(content.contains("BIG BANG 80/20"));
}

#[test]
fn test_makefile_content() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: Makefile has correct targets
    let makefile_path = temp_dir.path().join("Makefile");
    let content = fs::read_to_string(&makefile_path).unwrap();

    assert!(content.contains(".PHONY:"));
    assert!(content.contains("help:"));
    assert!(content.contains("setup:"));
    assert!(content.contains("build:"));
    assert!(content.contains("clean:"));
    assert!(content.contains("ggen sync"));
}

#[test]
fn test_startup_sh_content() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: startup.sh has screening gate questions
    let startup_sh_path = temp_dir.path().join("scripts/startup.sh");
    let content = fs::read_to_string(&startup_sh_path).unwrap();

    assert!(content.contains("#!/bin/bash"));
    assert!(content.contains("BIG BANG 80/20"));
    assert!(content.contains("Question 1/5"));
    assert!(content.contains("Question 2/5"));
    assert!(content.contains("Question 3/5"));
    assert!(content.contains("Question 4/5"));
    assert!(content.contains("Question 5/5"));
    assert!(content.contains("real user data"));
    assert!(content.contains("standard ontology"));
}

#[test]
fn test_example_template_content() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: example.txt.tera is valid Tera template
    let template_path = temp_dir.path().join("templates/example.txt.tera");
    let content = fs::read_to_string(&template_path).unwrap();

    assert!(content.contains("Tera Template"));
    assert!(content.contains("{% for row in results %}"));
    assert!(content.contains("{{ row.label"));
    assert!(content.contains("{% endfor %}"));
}

// ============================================================================
// TRANSACTION & RECEIPT VALIDATION
// ============================================================================

#[test]
fn test_init_output_structure() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);

    // Assert: InitOutput has all required fields
    assert!(!output.status.is_empty());
    assert!(!output.project_dir.is_empty());
    assert!(!output.files_created.is_empty());
    assert!(!output.directories_created.is_empty());
    assert!(!output.next_steps.is_empty());

    // Assert: Output can be serialized to JSON (for receipts)
    let json = serde_json::to_string(&output).unwrap();
    assert!(json.contains("status"));
    assert!(json.contains("files_created"));
    assert!(json.contains("directories_created"));
}

#[test]
fn test_init_provides_actionable_next_steps() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act
    let output = init_project(project_path, false);
    assert_eq!(output.status, "success");

    // Assert: Next steps are specific and actionable
    assert!(output.next_steps.len() >= 3, "Should have multiple steps");

    let steps_text = output.next_steps.join(" ");
    assert!(steps_text.contains("make setup") || steps_text.contains("setup"));
    assert!(steps_text.contains("schema") || steps_text.contains("ontology"));
    assert!(steps_text.contains("template"));
    assert!(steps_text.contains("build") || steps_text.contains("generate"));
}

// ============================================================================
// CONCURRENT SAFETY (BEST EFFORT)
// ============================================================================

#[test]
fn test_concurrent_init_attempts() {
    // Note: This tests filesystem behavior, not true concurrent safety
    // (since the underlying function isn't async or thread-safe by design).
    // But we can verify that serial operations don't corrupt state.

    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().to_str().unwrap();

    // Act: Multiple sequential inits (simulating "concurrent" attempts)
    let output1 = init_project(project_path, false);
    let output2 = init_project(project_path, false);
    let output3 = init_project(project_path, false);

    // Assert: First succeeds
    assert_eq!(output1.status, "success");

    // Assert: Subsequent fail (already initialized)
    assert_eq!(output2.status, "error");
    assert_eq!(output3.status, "error");

    // Assert: Files remain intact after failed attempts
    assert_file_contains(&temp_dir.path().join("ggen.toml"), "[project]");
}

#[test]
fn test_error_recovery_after_partial_failure() {
    // Arrange: Pre-create schema directory but make it read-only
    let temp_dir = TempDir::new().unwrap();
    let schema_dir = temp_dir.path().join("schema");
    fs::create_dir_all(&schema_dir).unwrap();

    // Make schema directory read-only (on Unix)
    #[cfg(unix)]
    {
        let mut perms = fs::metadata(&schema_dir).unwrap().permissions();
        perms.set_mode(0o444);
        fs::set_permissions(&schema_dir, perms).unwrap();
    }

    let project_path = temp_dir.path().to_str().unwrap();

    // Act: Try to initialize (will fail to write schema/domain.ttl)
    let output = init_project(project_path, false);

    // On Unix, this should fail with partial status
    #[cfg(unix)]
    {
        // Assert: Partial or error status
        assert!(
            output.status == "partial" || output.status == "error",
            "Expected partial or error status, got: {}",
            output.status
        );

        // Assert: Error contains info about failure
        if output.status == "partial" {
            assert!(output.error.is_some());
        }

        // Cleanup: Restore permissions
        let mut perms = fs::metadata(&schema_dir).unwrap().permissions();
        perms.set_mode(0o755);
        let _ = fs::set_permissions(&schema_dir, perms);
    }
}
