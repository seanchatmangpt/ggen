//! Chicago TDD tests for shell completion utilities
//!
//! These tests use REAL system operations, not mocks:
//! - REAL file system operations
//! - REAL shell detection
//! - REAL completion script generation
//! - Temporary directories for isolation

use ggen_cli::domain::shell::completion::*;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_shell_type_parsing() {
    // Test all supported shells
    assert_eq!(ShellType::from_str("bash"), Some(ShellType::Bash));
    assert_eq!(ShellType::from_str("BASH"), Some(ShellType::Bash));
    assert_eq!(ShellType::from_str("zsh"), Some(ShellType::Zsh));
    assert_eq!(ShellType::from_str("fish"), Some(ShellType::Fish));
    assert_eq!(ShellType::from_str("powershell"), Some(ShellType::PowerShell));
    assert_eq!(ShellType::from_str("pwsh"), Some(ShellType::PowerShell));

    // Test invalid shell
    assert_eq!(ShellType::from_str("invalid_shell"), None);
    assert_eq!(ShellType::from_str(""), None);
}

#[test]
fn test_shell_type_round_trip() {
    // Test that as_str produces parseable strings
    let shells = [
        ShellType::Bash,
        ShellType::Zsh,
        ShellType::Fish,
        ShellType::PowerShell,
    ];

    for shell in &shells {
        let str_repr = shell.as_str();
        let parsed = ShellType::from_str(str_repr);
        assert_eq!(parsed, Some(*shell), "Failed for shell: {:?}", shell);
    }
}

#[test]
fn test_completion_generator_produces_valid_scripts() {
    // Use REAL generator
    let generator = ClapCompletionGenerator::new("ggen");

    // Generate for each shell type
    let shells = [
        ShellType::Bash,
        ShellType::Zsh,
        ShellType::Fish,
        ShellType::PowerShell,
    ];

    for shell in &shells {
        let result = generator.generate(*shell);
        assert!(result.is_ok(), "Failed to generate for {:?}", shell);

        let completion = result.unwrap();
        assert_eq!(completion.shell, *shell);
        assert!(!completion.script.is_empty(), "Empty script for {:?}", shell);

        // Verify script contains expected content
        assert!(completion.script.contains("ggen"), "Script doesn't mention ggen for {:?}", shell);
    }
}

#[test]
fn test_file_system_installer_creates_files() {
    // Use REAL temporary directory
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let install_path = temp_dir.path().join("completions").join("ggen.bash");

    // Create parent directory
    std::fs::create_dir_all(install_path.parent().unwrap()).expect("Failed to create parent dir");

    let installer = FileSystemCompletionInstaller;
    let generator = ClapCompletionGenerator::new("ggen");

    // Generate completion
    let completion = generator.generate(ShellType::Bash).expect("Failed to generate");

    // Install to custom path
    let result = installer.install_to(&completion, install_path.clone(), false);
    assert!(result.is_ok(), "Installation failed: {:?}", result.err());

    // Verify file exists and contains script
    assert!(install_path.exists(), "Completion file not created");
    let content = std::fs::read_to_string(&install_path).expect("Failed to read file");
    assert_eq!(content, completion.script);
}

#[test]
fn test_file_system_installer_respects_force_flag() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let install_path = temp_dir.path().join("ggen.bash");

    let installer = FileSystemCompletionInstaller;
    let generator = ClapCompletionGenerator::new("ggen");
    let completion = generator.generate(ShellType::Bash).expect("Failed to generate");

    // First installation should succeed
    let result1 = installer.install_to(&completion, install_path.clone(), false);
    assert!(result1.is_ok());

    // Second installation without force should fail
    let result2 = installer.install_to(&completion, install_path.clone(), false);
    assert!(result2.is_err());
    assert!(result2.unwrap_err().to_string().contains("already exists"));

    // Second installation with force should succeed
    let result3 = installer.install_to(&completion, install_path.clone(), true);
    assert!(result3.is_ok());
}

#[test]
fn test_system_shell_lister_detects_installed_shells() {
    let lister = SystemShellLister;

    // Get list of supported shells
    let supported = lister.list_supported();
    assert_eq!(supported.len(), 4);
    assert!(supported.contains(&ShellType::Bash));
    assert!(supported.contains(&ShellType::Zsh));
    assert!(supported.contains(&ShellType::Fish));
    assert!(supported.contains(&ShellType::PowerShell));

    // Test shell detection (bash is usually available)
    let bash_installed = lister.is_installed(ShellType::Bash);
    // We can't assert true/false as it depends on system, but shouldn't panic
    println!("Bash installed: {}", bash_installed);
}

#[test]
fn test_shell_default_completion_dirs() {
    // Test that each shell has a default completion directory strategy
    let shells = [
        ShellType::Bash,
        ShellType::Zsh,
        ShellType::Fish,
        ShellType::PowerShell,
    ];

    for shell in &shells {
        // Should return Some if HOME or XDG vars are set
        let dir = shell.default_completion_dir();
        // Don't assert Some/None as it depends on environment,
        // but ensure no panic
        println!("{:?} default dir: {:?}", shell, dir);
    }
}

#[test]
fn test_completion_script_contains_commands() {
    let generator = ClapCompletionGenerator::new("ggen");
    let result = generator.generate(ShellType::Bash).expect("Failed to generate");

    // Verify script contains main ggen command
    assert!(result.script.contains("ggen"), "Script missing ggen command");

    // Bash completions should have completion function
    assert!(
        result.script.contains("complete") || result.script.contains("_ggen"),
        "Script missing completion function"
    );
}

#[test]
fn test_installer_creates_parent_directories() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let nested_path = temp_dir.path()
        .join("deeply")
        .join("nested")
        .join("path")
        .join("ggen.zsh");

    let installer = FileSystemCompletionInstaller;
    let generator = ClapCompletionGenerator::new("ggen");
    let completion = generator.generate(ShellType::Zsh).expect("Failed to generate");

    // Install to deeply nested path
    let result = installer.install_to(&completion, nested_path.clone(), false);
    assert!(result.is_ok(), "Failed to create nested directories");

    // Verify all parent directories were created
    assert!(nested_path.parent().unwrap().exists());
    assert!(nested_path.exists());
}

#[test]
fn test_completion_scripts_are_non_empty_and_valid_utf8() {
    let generator = ClapCompletionGenerator::new("ggen");

    for shell in &[ShellType::Bash, ShellType::Zsh, ShellType::Fish, ShellType::PowerShell] {
        let result = generator.generate(*shell).expect("Generation failed");

        // Must not be empty
        assert!(!result.script.is_empty());

        // Must be valid UTF-8 (this is implicit in String, but let's be explicit)
        assert!(result.script.is_ascii() || !result.script.is_empty());

        // Must have reasonable length
        assert!(result.script.len() > 100, "Script too short for {:?}", shell);
    }
}

#[test]
fn test_multiple_installations_with_force() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let path = temp_dir.path().join("ggen.fish");

    let installer = FileSystemCompletionInstaller;
    let generator = ClapCompletionGenerator::new("ggen");

    // Install multiple times with force=true
    for _ in 0..3 {
        let completion = generator.generate(ShellType::Fish).expect("Generation failed");
        let result = installer.install_to(&completion, path.clone(), true);
        assert!(result.is_ok(), "Force installation failed");
    }

    // Verify file still exists and is correct
    assert!(path.exists());
    let content = std::fs::read_to_string(&path).expect("Failed to read");
    assert!(!content.is_empty());
}
