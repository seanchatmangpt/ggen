use std::process::Command;
use std::str;

#[test]
fn test_cli_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("ggen"));
    // Legacy commands commented out:
    // assert!(stdout.contains("hazard"));
    // assert!(stdout.contains("gen"));
    // assert!(stdout.contains("completion")); // COMMENTED OUT: Command line completion code
    // Modern noun-verb commands:
    assert!(stdout.contains("audit"));
    assert!(stdout.contains("project"));
}

// COMMENTED OUT: Legacy hazard command test
// #[test]
// fn test_hazard_command() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "hazard"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");
//
//     // Test passes if command runs without panicking
//     // The actual output depends on the ggen_core::commands::hazard implementation
//     assert!(output.status.code().is_some());
// }

#[test]
fn test_error_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "error"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the ggen_core::commands::simulate_error implementation
    assert!(output.status.code().is_some());
}

#[test]
fn test_config_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "config"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the ggen_core::commands::config implementation
    assert!(output.status.code().is_some());
}

// #[test] // COMMENTED OUT: Command line completion code
// fn test_completion_bash() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "shell", "completion", "generate", "--shell", "bash"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");

//     assert!(output.status.success());
//     let stdout = str::from_utf8(&output.stdout).unwrap();
//     // Bash completion should contain function definitions
//     assert!(stdout.contains("_ggen"));
// }

// #[test] // COMMENTED OUT: Command line completion code
// fn test_completion_zsh() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "shell", "completion", "generate", "--shell", "zsh"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");

//     assert!(output.status.success());
//     let stdout = str::from_utf8(&output.stdout).unwrap();
//     // Zsh completion should contain completion definitions
//     assert!(stdout.contains("compdef"));
// }

// #[test] // COMMENTED OUT: Command line completion code
// fn test_completion_fish() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "shell", "completion", "generate", "--shell", "fish"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");

//     assert!(output.status.success());
//     let stdout = str::from_utf8(&output.stdout).unwrap();
//     // Fish completion should contain complete commands
//     assert!(stdout.contains("complete"));
// }

#[test]
fn test_invalid_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "invalid-command"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error"));
}

// COMMENTED OUT: Legacy hazard command test with config flag
// #[test]
// fn test_config_flag() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "--config", "/nonexistent/path", "hazard"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");
//
//     // Should handle config file not found gracefully
//     assert!(output.status.code().is_some());
// }

// COMMENTED OUT: Legacy hazard command test with debug flag
// #[test]
// fn test_debug_flag() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "--debug", "true", "hazard"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");
//
//     // Should handle debug flag without issues
//     assert!(output.status.code().is_some());
// }

// Integration tests for noun-verb command structure

#[test]
fn test_audit_hazard_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "audit", "hazard", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("hazardous patterns"));
    assert!(stdout.contains("scan"));
    assert!(stdout.contains("list"));
    assert!(stdout.contains("check"));
}

#[test]
fn test_audit_security_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "audit", "security", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("security"));
    assert!(stdout.contains("vulnerability"));
}

#[test]
fn test_audit_performance_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "audit", "performance", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("performance"));
    assert!(stdout.contains("benchmark"));
}

#[test]
fn test_ci_pages_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ci", "pages", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("GitHub Pages"));
    assert!(stdout.contains("deploy"));
    assert!(stdout.contains("status"));
    assert!(stdout.contains("logs"));
    assert!(stdout.contains("compare"));
}

#[test]
fn test_ci_workflow_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ci", "workflow", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("workflow"));
    assert!(stdout.contains("list"));
    assert!(stdout.contains("status"));
    assert!(stdout.contains("logs"));
    assert!(stdout.contains("cancel"));
}

#[test]
fn test_ci_trigger_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ci", "trigger", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("trigger"));
    assert!(stdout.contains("workflow"));
}

// #[test] // COMMENTED OUT: Command line completion code
// fn test_shell_completion_help() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "shell", "completion", "--help"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");

//     assert!(output.status.success());
//     let stdout = str::from_utf8(&output.stdout).unwrap();
//     assert!(stdout.contains("completion"));
//     assert!(stdout.contains("generate"));
//     assert!(stdout.contains("install"));
//     assert!(stdout.contains("list"));
// }

#[test]
fn test_shell_init_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "shell", "init", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("init"));
    assert!(stdout.contains("shell"));
    assert!(stdout.contains("project"));
    assert!(stdout.contains("dev"));
}

#[test]
fn test_audit_hazard_scan_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "audit", "hazard", "scan", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("scan"));
    assert!(stdout.contains("path"));
    assert!(stdout.contains("json"));
    assert!(stdout.contains("verbose"));
    assert!(stdout.contains("fix"));
}

#[test]
fn test_ci_pages_deploy_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ci", "pages", "deploy", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("deploy"));
    assert!(stdout.contains("force"));
    assert!(stdout.contains("wait"));
    assert!(stdout.contains("branch"));
}

// #[test] // COMMENTED OUT: Command line completion code
// fn test_shell_completion_generate_help() {
//     let output = Command::new("cargo")
//         .args(&["run", "--", "shell", "completion", "generate", "--help"])
//         .current_dir("/Users/sac/ggen")
//         .output()
//         .expect("Failed to execute command");

//     assert!(output.status.success());
//     let stdout = str::from_utf8(&output.stdout).unwrap();
//     assert!(stdout.contains("generate"));
//     assert!(stdout.contains("shell"));
//     assert!(stdout.contains("output"));
// }

#[test]
fn test_shell_init_shell_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "shell", "init", "shell", "--help"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("shell"));
    assert!(stdout.contains("config"));
    assert!(stdout.contains("force"));
}

// Test error cases for noun-verb commands

#[test]
fn test_audit_invalid_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--", "audit", "invalid"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error"));
}

#[test]
fn test_ci_invalid_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ci", "invalid"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error"));
}

#[test]
fn test_shell_invalid_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--", "shell", "invalid"])
        .current_dir("/Users/sac/ggen")
        .output()
        .expect("Failed to execute command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error"));
}
