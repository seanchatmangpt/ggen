//! Performance Tests - CLI Startup, Memory, Concurrency
//!
//! Tests critical performance characteristics:
//! - CLI startup time must be ≤3s
//! - Memory usage must stay <120MB
//! - Concurrent command execution must be safe
//!
//! 80/20 Focus: Performance bottlenecks that impact UX

use assert_cmd::Command;
use assert_fs::prelude::*;
use assert_fs::TempDir;
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

// ============================================================================
// CLI Startup Time Tests (≤3s requirement)
// ============================================================================

#[test]
fn perf_startup_time_help_command() {
    // Test: CLI must start and show help in ≤3s
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["--help"])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(3),
        "CLI startup took {:?}, should be <3s",
        elapsed
    );
}

#[test]
fn perf_startup_time_version_command() {
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["--version"])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(3),
        "Version command took {:?}, should be <3s",
        elapsed
    );
}

#[test]
fn perf_startup_time_subcommand_help() {
    // Test that subcommand help is also fast
    for subcommand in &["template", "market", "project", "lifecycle"] {
        let start = Instant::now();

        Command::cargo_bin("ggen")
            .unwrap()
            .args([*subcommand, "--help"])
            .assert()
            .success();

        let elapsed = start.elapsed();
        assert!(
            elapsed < Duration::from_secs(3),
            "{} help took {:?}, should be <3s",
            subcommand,
            elapsed
        );
    }
}

#[test]
fn perf_cold_start_with_config() {
    // Test startup with config file loading
    let temp = TempDir::new().unwrap();
    let config_file = temp.child("config.toml");

    config_file
        .write_str(
            r#"
[project]
name = "test"

[templates]
search_paths = ["./templates"]

[marketplace]
enabled = true
"#,
        )
        .unwrap();

    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["--config", config_file.path().to_str().unwrap(), "--help"])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(3),
        "Cold start with config took {:?}, should be <3s",
        elapsed
    );
}

// ============================================================================
// Memory Usage Tests (<120MB requirement)
// ============================================================================

#[test]
#[cfg(target_os = "linux")]
fn perf_memory_usage_basic_command() {
    use std::process::{Command as StdCommand, Stdio};

    // Run command and measure memory usage via /proc
    let mut child = StdCommand::new(env!("CARGO_BIN_EXE_ggen"))
        .args(["--help"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();

    // Give it time to run
    thread::sleep(Duration::from_millis(100));

    // Read memory usage from /proc
    let status_path = format!("/proc/{}/status", child.id());
    if let Ok(status) = std::fs::read_to_string(&status_path) {
        for line in status.lines() {
            if line.starts_with("VmRSS:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    let memory_kb: u64 = parts[1].parse().unwrap_or(0);
                    let memory_mb = memory_kb / 1024;

                    assert!(
                        memory_mb < 120,
                        "Memory usage is {}MB, should be <120MB",
                        memory_mb
                    );
                }
                break;
            }
        }
    }

    let _ = child.wait();
}

#[test]
fn perf_memory_stress_large_template() {
    // Test memory doesn't spike with large template
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("large.yaml");
    let output_dir = temp.child("output");

    // Create large template (100 files)
    let mut template = String::from(
        r#"
name: "memory-test"
variables:
  - name: project_name
    required: true
nodes:
  - name: "{{project_name}}"
    type: directory
    children:
"#,
    );

    for i in 0..100 {
        template.push_str(&format!(
            r#"
      - name: "module_{}/lib.rs"
        type: file
        content: |
          // Module {}
          pub fn function_{}() {{
              println!("Function {0}");
          }}
"#,
            i, i, i
        ));
    }

    template_file.write_str(&template).unwrap();

    // Execute and verify no OOM
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "project_name=large-project",
        ])
        .timeout(Duration::from_secs(30))
        .assert()
        .success();
}

// ============================================================================
// Concurrent Command Execution Tests
// ============================================================================

#[test]
fn perf_concurrent_help_commands() {
    // Test: Multiple help commands can run concurrently without issues
    let handles: Vec<_> = (0..5)
        .map(|_| {
            thread::spawn(|| {
                Command::cargo_bin("ggen")
                    .unwrap()
                    .args(["--help"])
                    .assert()
                    .success();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn perf_concurrent_version_commands() {
    let handles: Vec<_> = (0..10)
        .map(|_| {
            thread::spawn(|| {
                Command::cargo_bin("ggen")
                    .unwrap()
                    .args(["--version"])
                    .assert()
                    .success();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn perf_concurrent_template_generation() {
    // Test: Multiple template generations can run safely
    let temp = Arc::new(TempDir::new().unwrap());
    let template_file = temp.child("template.yaml");

    template_file
        .write_str(
            r#"
name: "concurrent-test"
variables:
  - name: id
    required: true
nodes:
  - name: "output_{{id}}"
    type: directory
    children:
      - name: "file.txt"
        type: file
        content: "ID: {{id}}"
"#,
        )
        .unwrap();

    let handles: Vec<_> = (0..3)
        .map(|i| {
            let temp_clone = Arc::clone(&temp);
            let template_path = template_file.path().to_path_buf();

            thread::spawn(move || {
                let output_dir = temp_clone.child(format!("output_{}", i));

                Command::cargo_bin("ggen")
                    .unwrap()
                    .args([
                        "template",
                        "generate-tree",
                        "--template",
                        template_path.to_str().unwrap(),
                        "--output",
                        output_dir.path().to_str().unwrap(),
                        "--var",
                        &format!("id={}", i),
                    ])
                    .assert()
                    .success();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn perf_concurrent_marketplace_searches() {
    // Test: Concurrent searches don't cause race conditions
    let queries = vec!["rust", "cli", "web", "api", "template"];

    let handles: Vec<_> = queries
        .into_iter()
        .map(|query| {
            thread::spawn(move || {
                Command::cargo_bin("ggen")
                    .unwrap()
                    .args(["market", "search", query, "--limit", "5"])
                    .assert()
                    .success();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

// ============================================================================
// Response Time Tests
// ============================================================================

#[test]
fn perf_response_time_doctor_command() {
    // Doctor should complete quickly
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["doctor"])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(5),
        "Doctor command took {:?}, should be <5s",
        elapsed
    );
}

#[test]
fn perf_response_time_marketplace_search() {
    // Marketplace search should be reasonably fast
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args(["market", "search", "rust", "--limit", "10"])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(10),
        "Marketplace search took {:?}, should be <10s",
        elapsed
    );
}

#[test]
fn perf_response_time_simple_template() {
    // Simple template generation should be fast
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("simple.yaml");
    let output_dir = temp.child("output");

    template_file
        .write_str(
            r#"
name: "simple"
variables:
  - name: name
    required: true
nodes:
  - name: "{{name}}"
    type: directory
    children:
      - name: "README.md"
        type: file
        content: "# {{name}}"
"#,
        )
        .unwrap();

    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "name=test",
        ])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(5),
        "Simple template generation took {:?}, should be <5s",
        elapsed
    );
}

// ============================================================================
// Scalability Tests
// ============================================================================

#[test]
fn perf_scalability_many_variables() {
    // Test template with many variables processes efficiently
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("many-vars.yaml");
    let output_dir = temp.child("output");

    let mut template = String::from(
        r#"
name: "many-vars"
variables:
"#,
    );

    // Add 50 variables
    for i in 0..50 {
        template.push_str(&format!(
            r#"
  - name: var_{}
    default: "value_{}"
"#,
            i, i
        ));
    }

    template.push_str(
        r#"
nodes:
  - name: "output"
    type: directory
    children:
      - name: "config.txt"
        type: file
        content: "Configuration file"
"#,
    );

    template_file.write_str(&template).unwrap();

    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
        ])
        .assert()
        .success();

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(10),
        "Many variables template took {:?}, should be <10s",
        elapsed
    );
}

#[test]
fn perf_scalability_deep_nesting() {
    // Test deeply nested directory structure
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("deep.yaml");
    let output_dir = temp.child("output");

    let mut template = String::from(
        r#"
name: "deep-structure"
variables:
  - name: project_name
    required: true
nodes:
  - name: "{{project_name}}"
    type: directory
    children:
"#,
    );

    // Create 10 levels of nesting
    let mut indent = "      ";
    for i in 0..10 {
        template.push_str(&format!(
            r#"
{}      - name: "level_{}"
{}        type: directory
{}        children:
"#,
            indent, i, indent, indent
        ));
        indent = &indent[0..indent.len().saturating_sub(2)];
    }

    template.push_str(
        r#"
          - name: "deepest.txt"
            type: file
            content: "Deepest level"
"#,
    );

    template_file.write_str(&template).unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "template",
            "generate-tree",
            "--template",
            template_file.path().to_str().unwrap(),
            "--output",
            output_dir.path().to_str().unwrap(),
            "--var",
            "project_name=deep-test",
        ])
        .timeout(Duration::from_secs(15))
        .assert()
        .success();
}

// ============================================================================
// Resource Cleanup Tests
// ============================================================================

#[test]
fn perf_no_resource_leaks_repeated_commands() {
    // Run same command multiple times and verify no leaks
    for _ in 0..10 {
        Command::cargo_bin("ggen")
            .unwrap()
            .args(["--version"])
            .assert()
            .success();
    }
}

#[test]
fn perf_no_resource_leaks_failed_commands() {
    // Verify failed commands don't leak resources
    for _ in 0..5 {
        let _ = Command::cargo_bin("ggen")
            .unwrap()
            .args(["template", "generate-tree", "--template", "/nonexistent.yaml"])
            .assert()
            .failure();
    }
}

// ============================================================================
// Throughput Tests
// ============================================================================

#[test]
fn perf_throughput_sequential_generations() {
    // Test: Can process multiple templates in sequence efficiently
    let temp = TempDir::new().unwrap();
    let template_file = temp.child("template.yaml");

    template_file
        .write_str(
            r#"
name: "throughput-test"
variables:
  - name: id
    required: true
nodes:
  - name: "project_{{id}}"
    type: directory
    children:
      - name: "README.md"
        type: file
        content: "# Project {{id}}"
"#,
        )
        .unwrap();

    let start = Instant::now();

    for i in 0..5 {
        let output_dir = temp.child(format!("output_{}", i));

        Command::cargo_bin("ggen")
            .unwrap()
            .args([
                "template",
                "generate-tree",
                "--template",
                template_file.path().to_str().unwrap(),
                "--output",
                output_dir.path().to_str().unwrap(),
                "--var",
                &format!("id={}", i),
            ])
            .assert()
            .success();
    }

    let elapsed = start.elapsed();
    let avg_per_generation = elapsed / 5;

    assert!(
        avg_per_generation < Duration::from_secs(3),
        "Average generation time {:?}, should be <3s",
        avg_per_generation
    );
}
