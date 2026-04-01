//! CISO E2E Test Helpers
//!
//! Shared utilities for CISO enterprise-focused end-to-end tests.
//! All helpers invoke the REAL ggen binary via assert_cmd.

use anyhow::Result;
use assert_cmd::Command;
use serde_json::Value;
use std::path::Path;
use tempfile::TempDir;

// ==============================================================================
// Command Builders
// ==============================================================================

/// Create a new `ggen` command targeting the locally built binary.
pub fn ggen() -> Result<Command> {
    Ok(Command::cargo_bin("ggen")?)
}

/// Create a temporary workspace directory.
pub fn create_temp_workspace() -> Result<TempDir> {
    Ok(tempfile::tempdir()?)
}

/// Convenience: get the path of a temp directory.
pub fn workspace_path(temp: &TempDir) -> &Path {
    temp.path()
}

// ==============================================================================
// Runners
// ==============================================================================

/// Run `ggen` with the given arguments in the specified directory.
///
/// Returns `(stdout, exit_code)`.
pub fn run_ggen(args: &[&str], cwd: &Path) -> Result<(String, i32)> {
    let mut cmd = ggen()?;
    cmd.args(args);
    cmd.current_dir(cwd);
    let output = cmd.output()?;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let exit_code = output.status.code().unwrap_or(-1);
    Ok((stdout, exit_code))
}

/// Run `ggen` expecting a successful exit (code 0).
///
/// Returns stdout on success; panics with diagnostic info on failure.
pub fn run_ggen_success(args: &[&str], cwd: &Path) -> Result<String> {
    let (stdout, exit_code) = run_ggen(args, cwd)?;
    if exit_code != 0 {
        panic!(
            "ggen {} expected success (exit 0) but got exit {}. stdout:\n{}",
            args.join(" "),
            exit_code,
            &stdout[..stdout.len().min(500)]
        );
    }
    Ok(stdout)
}

/// Run `ggen` expecting a non-zero exit (failure).
///
/// Returns stdout on failure; panics if the command unexpectedly succeeds.
pub fn run_ggen_failure(args: &[&str], cwd: &Path) -> Result<String> {
    let (stdout, exit_code) = run_ggen(args, cwd)?;
    if exit_code == 0 {
        panic!(
            "ggen {} expected failure (exit != 0) but got exit 0. stdout:\n{}",
            args.join(" "),
            &stdout[..stdout.len().min(500)]
        );
    }
    Ok(stdout)
}

/// Run a ggen CLI command and return parsed JSON + raw combined output + success.
///
/// Many ggen commands emit WARN/INFO log lines before the JSON payload.
/// This helper extracts the last JSON line from stdout.
pub fn run_ggen_json(args: &[&str]) -> Result<(Value, String, bool)> {
    let mut cmd = ggen()?;
    cmd.args(args);
    let output = cmd.output()?;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let success = output.status.success();

    let json_line = stdout
        .lines()
        .rev()
        .find(|l| l.trim_start().starts_with('{'))
        .map(|s| s.to_string())
        .unwrap_or_default();

    let parsed: Value = if json_line.is_empty() {
        Value::Null
    } else {
        serde_json::from_str(&json_line)?
    };

    Ok((parsed, format!("{}\n{}", stdout, stderr), success))
}

/// Run a ggen CLI command with optional cwd and return raw output info.
pub fn run_ggen_raw(args: &[&str], cwd: Option<&Path>) -> Result<(String, String, bool, i32)> {
    let mut cmd = ggen()?;
    cmd.args(args);
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }
    let output = cmd.output()?;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let success = output.status.success();
    let code = output.status.code().unwrap_or(-1);
    Ok((stdout, stderr, success, code))
}

// ==============================================================================
// JSON Parsing
// ==============================================================================

/// Parse a JSON string into a `serde_json::Value`.
///
/// Panics with a detailed message on parse failure.
/// Use `extract_json_from_output` if you need a `Result` return.
pub fn parse_json_output(output: &str) -> Value {
    let cleaned = strip_ansi(output.trim());
    serde_json::from_str(&cleaned).unwrap_or_else(|e| {
        panic!(
            "Failed to parse JSON output:\n--- raw output (first 500 chars) ---\n{}\n--- error ---\n{}",
            &output[..output.len().min(500)],
            e
        );
    })
}

/// Extract the last line of output that is valid JSON.
///
/// CLI commands often emit WARN/INFO log lines before the JSON payload.
/// This scans lines from the bottom until valid JSON is found.
pub fn extract_json_from_output(output: &str) -> Result<Value> {
    for line in output.lines().rev() {
        let trimmed = strip_ansi(line.trim());
        if trimmed.starts_with('{') {
            if let Ok(value) = serde_json::from_str::<Value>(&trimmed) {
                return Ok(value);
            }
        }
    }
    // Fallback: try the entire output
    Ok(parse_json_output(output))
}

/// Parse a JSON value from a string, finding the first line that starts with '{'.
pub fn extract_json(text: &str) -> Result<Value> {
    let json_line = text
        .lines()
        .find(|l| l.trim_start().starts_with('{'))
        .map(|s| s.to_string())
        .unwrap_or_default();
    Ok(serde_json::from_str(&json_line)?)
}

// ==============================================================================
// Assertion Helpers
// ==============================================================================

/// Assert that a JSON object has a non-empty string field.
pub fn assert_nonempty_string(val: &Value, field: &str) {
    let s = val
        .get(field)
        .unwrap_or(&Value::Null)
        .as_str()
        .unwrap_or("");
    assert!(
        !s.is_empty(),
        "Expected non-empty string for field '{}', got: {:?}",
        field,
        val.get(field)
    );
}

/// Assert that a JSON object has a numeric field greater than zero.
pub fn assert_positive_number(val: &Value, field: &str) {
    let n = val
        .get(field)
        .unwrap_or(&Value::Null)
        .as_u64()
        .unwrap_or(0);
    assert!(
        n > 0,
        "Expected positive number for field '{}', got: {}",
        field,
        n
    );
}

// ==============================================================================
// Internal Utilities
// ==============================================================================

/// Strip ANSI escape codes from a string.
fn strip_ansi(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            // Skip until 'm' (end of ANSI sequence)
            while let Some(&next) = chars.peek() {
                chars.next();
                if next == 'm' {
                    break;
                }
            }
        } else {
            result.push(c);
        }
    }
    result
}

#[cfg(test)]
mod helper_unit_tests {
    use super::*;

    #[test]
    fn test_strip_ansi_removes_escape_codes() {
        let input = "\x1b[33mWARN\x1b[0m some text";
        let cleaned = strip_ansi(input);
        assert_eq!(cleaned, "WARN some text");
    }

    #[test]
    fn test_parse_json_output_valid() {
        let json = r#"{"total": 5}"#;
        let value = parse_json_output(json);
        assert_eq!(value["total"], 5);
    }

    #[test]
    fn test_extract_json_from_output_with_prefix_lines() {
        let output = "some log line\nmore logs\n{\"total\": 3}";
        let value = extract_json_from_output(output).unwrap();
        assert_eq!(value["total"], 3);
    }
}
