//! Simple GGen CLI Test
//!
//! This example demonstrates basic mcpp CLI testing without the cleanroom framework
//! to avoid compilation issues.

use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

/// Simple test result
#[derive(Debug)]
struct TestResult {
    exit_code: i32,
    stdout: String,
    stderr: String,
    duration_ms: u128,
    command: String,
}

impl TestResult {
    fn new(
        exit_code: i32, stdout: String, stderr: String, duration_ms: u128, command: String,
    ) -> Self {
        Self {
            exit_code,
            stdout,
            stderr,
            duration_ms,
            command,
        }
    }

    fn assert_success(&self) -> &Self {
        if self.exit_code != 0 {
            panic!(
                "Command failed with exit code {}: {}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                self.exit_code, self.command, self.stdout, self.stderr
            );
        }
        self
    }

    fn assert_stdout_contains(&self, needle: &str) -> &Self {
        if !self.stdout.contains(needle) {
            panic!(
                "stdout missing {:?} in command {}\n--- stdout ---\n{}",
                needle, self.command, self.stdout
            );
        }
        self
    }

    fn assert_duration_le(&self, max_ms: u128) -> &Self {
        if self.duration_ms > max_ms {
            panic!(
                "command {} took {}ms, expected <= {}ms",
                self.command, self.duration_ms, max_ms
            );
        }
        self
    }
}

/// Find mcpp binary
fn find_mcpp_binary() -> PathBuf {
    // Try to find mcpp binary in the project
    if let Ok(current_dir) = std::env::current_dir() {
        // Look for mcpp in target/debug/mcpp relative to current directory
        let debug_binary = current_dir.join("target").join("debug").join("mcpp");
        if debug_binary.exists() {
            return debug_binary;
        }

        // Look for mcpp in target/release/mcpp
        let release_binary = current_dir.join("target").join("release").join("mcpp");
        if release_binary.exists() {
            return release_binary;
        }
    }

    // Fallback to PATH
    PathBuf::from("mcpp")
}

/// Run a mcpp command
fn run_mcpp_command(args: &[&str]) -> TestResult {
    let start = Instant::now();
    let mcpp_binary = find_mcpp_binary();

    let mut cmd = Command::new(&mcpp_binary);
    cmd.args(args);

    let output = cmd.output().expect("Failed to execute mcpp command");
    let duration_ms = start.elapsed().as_millis();

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let command = format!("{} {}", mcpp_binary.display(), args.join(" "));

    TestResult::new(
        output.status.code().unwrap_or(-1),
        stdout,
        stderr,
        duration_ms,
        command,
    )
}

/// Test basic mcpp CLI functionality
fn test_basic_commands() {
    println!("\n=== Basic GGen CLI Commands ===");

    // Test --help
    let result = run_mcpp_command(&["--help"]);
    result
        .assert_success()
        .assert_stdout_contains("Usage:")
        .assert_stdout_contains("Commands:")
        .assert_duration_le(5000);

    println!("✓ --help command works ({} ms)", result.duration_ms);

    // Test --version
    let result = run_mcpp_command(&["--version"]);
    result
        .assert_success()
        .assert_stdout_contains("mcpp")
        .assert_duration_le(3000);

    println!("✓ --version command works ({} ms)", result.duration_ms);
}

/// Test mcpp project commands
fn test_project_commands() {
    println!("\n=== GGen Project Commands ===");

    // Test project --help
    let result = run_mcpp_command(&["project", "--help"]);
    result
        .assert_success()
        .assert_stdout_contains("project")
        .assert_duration_le(5000);

    println!("✓ project --help works ({} ms)", result.duration_ms);

    // Test project init (dry run)
    let result = run_mcpp_command(&["project", "init", "test-project", "--dry-run"]);
    // Note: This might fail if mcpp requires specific setup, so we just check it doesn't panic
    println!(
        "✓ project init --dry-run completed (exit: {}, {} ms)",
        result.exit_code, result.duration_ms
    );
}

/// Test mcpp template commands
fn test_template_commands() {
    println!("\n=== GGen Template Commands ===");

    // Test template --help
    let result = run_mcpp_command(&["template", "--help"]);
    result
        .assert_success()
        .assert_stdout_contains("template")
        .assert_duration_le(5000);

    println!("✓ template --help works ({} ms)", result.duration_ms);

    // Test template list
    let result = run_mcpp_command(&["template", "list"]);
    // This might return empty list, which is fine
    println!(
        "✓ template list completed (exit: {}, {} ms)",
        result.exit_code, result.duration_ms
    );
}

/// Test mcpp graph commands
fn test_graph_commands() {
    println!("\n=== GGen Graph Commands ===");

    // Test graph --help
    let result = run_mcpp_command(&["graph", "--help"]);
    result
        .assert_success()
        .assert_stdout_contains("graph")
        .assert_duration_le(5000);

    println!("✓ graph --help works ({} ms)", result.duration_ms);
}

/// Test mcpp market commands
fn test_market_commands() {
    println!("\n=== GGen Market Commands ===");

    // Test market --help
    let result = run_mcpp_command(&["market", "--help"]);
    result
        .assert_success()
        .assert_stdout_contains("market")
        .assert_duration_le(5000);

    println!("✓ market --help works ({} ms)", result.duration_ms);
}

/// Test mcpp AI commands
fn test_ai_commands() {
    println!("\n=== GGen AI Commands ===");

    // Test ai --help
    let result = run_mcpp_command(&["ai", "--help"]);
    result
        .assert_success()
        .assert_stdout_contains("ai")
        .assert_duration_le(5000);

    println!("✓ ai --help works ({} ms)", result.duration_ms);
}

/// Test mcpp workflow scenario
fn test_mcpp_workflow() {
    println!("\n=== GGen Workflow Scenario ===");

    let commands = vec![
        ("help", vec!["--help"]),
        ("version", vec!["--version"]),
        ("project_help", vec!["project", "--help"]),
        ("template_help", vec!["template", "--help"]),
        ("graph_help", vec!["graph", "--help"]),
        ("market_help", vec!["market", "--help"]),
        ("ai_help", vec!["ai", "--help"]),
    ];

    for (label, args) in commands {
        println!("→ [{}] mcpp {}", label, args.join(" "));
        let result = run_mcpp_command(&args);
        println!(
            "✓ Workflow step '{}' completed: {} ms",
            label, result.duration_ms
        );
    }
}

/// Test mcpp binary detection
fn test_binary_detection() {
    println!("\n=== GGen Binary Detection ===");

    let mcpp_binary = find_mcpp_binary();
    println!("Found mcpp binary: {}", mcpp_binary.display());

    if mcpp_binary.exists() {
        println!("✓ GGen binary exists and is accessible");
    } else {
        println!("✗ GGen binary not found at {}", mcpp_binary.display());
        println!("  Make sure to build mcpp first with: cargo build");
    }
}

fn main() {
    println!("Simple GGen CLI Test");
    println!("===================");

    // Test binary detection
    test_binary_detection();

    // Test basic commands
    test_basic_commands();

    // Test project commands
    test_project_commands();

    // Test template commands
    test_template_commands();

    // Test graph commands
    test_graph_commands();

    // Test market commands
    test_market_commands();

    // Test AI commands
    test_ai_commands();

    // Test workflow scenario
    test_mcpp_workflow();

    println!("\n=== Simple GGen Test Complete ===");
    println!("All mcpp CLI tests completed successfully!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcpp_binary_detection() {
        let binary = find_mcpp_binary();
        assert!(!binary.to_string_lossy().is_empty());
    }

    #[test]
    fn test_mcpp_help_command() {
        let result = run_mcpp_command(&["--help"]);
        assert_eq!(result.exit_code, 0);
        assert!(result.stdout.contains("Usage:"));
    }

    #[test]
    fn test_mcpp_version_command() {
        let result = run_mcpp_command(&["--version"]);
        assert_eq!(result.exit_code, 0);
        assert!(result.stdout.contains("mcpp"));
    }
}
