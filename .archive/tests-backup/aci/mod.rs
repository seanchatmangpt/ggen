//! Test utilities for ACI validation
//!
//! Provides shared helpers for parsing Makefile.toml, listing cargo make targets,
//! and validating tool descriptions against ACI quality standards.

#![allow(dead_code)]

use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Represents a cargo make target parsed from Makefile.toml
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct CargoMakeTarget {
    pub name: String,
    pub description: Option<String>,
    pub command: Option<String>,
    pub args: Vec<String>,
    pub script: Option<String>,
}

/// Parse Makefile.toml and extract all cargo make targets
///
/// Returns a HashMap of target_name -> CargoMakeTarget
pub fn parse_makefile_toml(path: &Path) -> Result<HashMap<String, CargoMakeTarget>, String> {
    let content =
        fs::read_to_string(path).map_err(|e| format!("Failed to read Makefile.toml: {}", e))?;

    let mut targets = HashMap::new();
    let mut current_target: Option<String> = None;
    let mut current_desc: Option<String> = None;
    let mut current_command: Option<String> = None;
    let mut current_script: Option<String> = None;
    let mut in_multiline_desc = false;
    let mut in_multiline_script = false;
    let mut multiline_buffer = String::new();

    for line in content.lines() {
        let trimmed = line.trim();

        // Detect task definition: [tasks.NAME]
        if trimmed.starts_with("[tasks.") && trimmed.ends_with(']') {
            // Save previous target if exists
            if let Some(name) = current_target.take() {
                targets.insert(
                    name.clone(),
                    CargoMakeTarget {
                        name,
                        description: current_desc.take(),
                        command: current_command.take(),
                        args: Vec::new(),
                        script: current_script.take(),
                    },
                );
            }

            // Extract new target name
            let name = trimmed
                .trim_start_matches("[tasks.")
                .trim_end_matches(']')
                .to_string();
            current_target = Some(name);
            current_desc = None;
            current_command = None;
            current_script = None;
            in_multiline_desc = false;
            in_multiline_script = false;
            multiline_buffer.clear();
        }
        // Detect description start
        else if trimmed.starts_with("description = ") {
            let desc_part = trimmed.trim_start_matches("description = ");

            if desc_part.starts_with("\"\"\"") {
                // Multi-line description starting
                in_multiline_desc = true;
                multiline_buffer = desc_part.trim_start_matches("\"\"\"").to_string();
                if desc_part.ends_with("\"\"\"") && desc_part.len() > 6 {
                    // Single-line triple-quoted string
                    multiline_buffer = desc_part
                        .trim_start_matches("\"\"\"")
                        .trim_end_matches("\"\"\"")
                        .to_string();
                    current_desc = Some(multiline_buffer.clone());
                    in_multiline_desc = false;
                    multiline_buffer.clear();
                }
            } else if desc_part.starts_with('"') && desc_part.ends_with('"') && desc_part.len() > 1
            {
                // Single-line description
                current_desc = Some(
                    desc_part
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string(),
                );
            }
        }
        // Detect command field
        else if trimmed.starts_with("command = ") {
            let cmd_part = trimmed.trim_start_matches("command = ");
            if cmd_part.starts_with('"') && cmd_part.ends_with('"') {
                current_command = Some(
                    cmd_part
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string(),
                );
            }
        }
        // Detect script field
        else if trimmed.starts_with("script = ") {
            let script_part = trimmed.trim_start_matches("script = ");
            if script_part.starts_with("'''") {
                // Multi-line script starting
                in_multiline_script = true;
                multiline_buffer = script_part.trim_start_matches("'''").to_string();
            }
        }
        // Continue multi-line description
        else if in_multiline_desc {
            if trimmed.ends_with("\"\"\"") {
                // End of multi-line description
                multiline_buffer.push_str("\n");
                multiline_buffer.push_str(trimmed.trim_end_matches("\"\"\""));
                current_desc = Some(multiline_buffer.trim().to_string());
                in_multiline_desc = false;
                multiline_buffer.clear();
            } else {
                multiline_buffer.push_str("\n");
                multiline_buffer.push_str(trimmed);
            }
        }
        // Continue multi-line script
        else if in_multiline_script {
            if trimmed.ends_with("'''") {
                // End of multi-line script
                multiline_buffer.push_str("\n");
                multiline_buffer.push_str(trimmed.trim_end_matches("'''"));
                current_script = Some(multiline_buffer.trim().to_string());
                in_multiline_script = false;
                multiline_buffer.clear();
            } else {
                multiline_buffer.push_str("\n");
                multiline_buffer.push_str(trimmed);
            }
        }
    }

    // Save final target
    if let Some(name) = current_target {
        targets.insert(
            name.clone(),
            CargoMakeTarget {
                name,
                description: current_desc,
                command: current_command,
                args: Vec::new(),
                script: current_script,
            },
        );
    }

    Ok(targets)
}

/// List all cargo make targets available in the project
///
/// Executes `cargo make --list-all-steps` and parses output
#[allow(dead_code)]
pub fn list_cargo_make_targets() -> Vec<String> {
    let output = Command::new("cargo")
        .args(["make", "--list-all-steps"])
        .output();

    match output {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            stdout
                .lines()
                .filter_map(|line| {
                    let trimmed = line.trim();
                    if trimmed.is_empty() || trimmed.starts_with('-') {
                        None
                    } else {
                        Some(trimmed.split_whitespace().next()?.to_string())
                    }
                })
                .collect()
        }
        Err(_) => Vec::new(),
    }
}

/// Extract description from a cargo make target
///
/// Returns None if target not found or has no description
#[allow(dead_code)]
pub fn extract_description(target_name: &str) -> Option<String> {
    let makefile_path = Path::new("Makefile.toml");
    if !makefile_path.exists() {
        return None;
    }

    let targets = parse_makefile_toml(makefile_path).ok()?;
    targets.get(target_name)?.description.clone()
}

/// Component check result for tool description validation
#[derive(Debug, PartialEq, Eq)]
pub struct ComponentCheck {
    pub has_purpose: bool,
    pub has_timing: bool,
    pub has_slo: bool,
    pub has_examples: bool,
    pub has_recovery: bool,
    pub length: usize,
}

impl ComponentCheck {
    /// Check if all 5 components are present
    pub fn is_complete(&self) -> bool {
        self.has_purpose
            && self.has_timing
            && self.has_slo
            && self.has_examples
            && self.has_recovery
            && self.length > 100
    }

    /// Count how many components are present
    pub fn component_count(&self) -> usize {
        let mut count = 0;
        if self.has_purpose {
            count += 1;
        }
        if self.has_timing {
            count += 1;
        }
        if self.has_slo {
            count += 1;
        }
        if self.has_examples {
            count += 1;
        }
        if self.has_recovery {
            count += 1;
        }
        count
    }
}

/// Validate that a tool description contains all 5 required components
///
/// Components (per Anthropic ACI guidelines):
/// 1. Purpose: What the tool does
/// 2. Timing: When to use it
/// 3. SLO: Performance threshold
/// 4. Examples: Sample outputs (RED/YELLOW/GREEN signals)
/// 5. Recovery: What to do when it fails
pub fn validate_description_components(desc: &str) -> ComponentCheck {
    let desc_lower = desc.to_lowercase();

    // Check for purpose indicators
    let has_purpose = desc_lower.contains("verify")
        || desc_lower.contains("check")
        || desc_lower.contains("run")
        || desc_lower.contains("compile")
        || desc_lower.contains("test")
        || desc_lower.contains("lint")
        || desc_lower.contains("build")
        || desc_lower.contains("format");

    // Check for timing indicators
    let has_timing = desc_lower.contains("when:")
        || desc_lower.contains("timing:")
        || desc_lower.contains("before")
        || desc_lower.contains("after")
        || desc_lower.contains("during");

    // Check for SLO indicators
    let has_slo = desc_lower.contains("slo:")
        || desc_lower.contains("timeout")
        || desc_lower.contains("<")
        || desc_lower.contains("target")
        || (desc_lower.contains("s") && desc_lower.contains("second"));

    // Check for examples (Andon signals)
    let has_examples = (desc_lower.contains("green") && desc_lower.contains("red"))
        || desc_lower.contains("example")
        || desc_lower.contains("output:");

    // Check for recovery guidance
    let has_recovery = desc_lower.contains("fix")
        || desc_lower.contains("recovery:")
        || desc_lower.contains("if fail")
        || desc_lower.contains("when fail")
        || desc_lower.contains("error:");

    ComponentCheck {
        has_purpose,
        has_timing,
        has_slo,
        has_examples,
        has_recovery,
        length: desc.len(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_component_check_complete() {
        let complete = ComponentCheck {
            has_purpose: true,
            has_timing: true,
            has_slo: true,
            has_examples: true,
            has_recovery: true,
            length: 150,
        };
        assert!(complete.is_complete());
        assert_eq!(complete.component_count(), 5);
    }

    #[test]
    fn test_component_check_incomplete() {
        let incomplete = ComponentCheck {
            has_purpose: true,
            has_timing: false,
            has_slo: true,
            has_examples: false,
            has_recovery: false,
            length: 80,
        };
        assert!(!incomplete.is_complete());
        assert_eq!(incomplete.component_count(), 2);
    }

    #[test]
    fn test_validate_sparse_description() {
        let sparse = "Check code without building";
        let check = validate_description_components(sparse);
        assert!(check.has_purpose);
        assert!(!check.has_timing);
        assert!(!check.has_examples);
        assert!(!check.is_complete());
    }

    #[test]
    fn test_validate_comprehensive_description() {
        let comprehensive = r#"
Fast compilation check (<5s target).
Verifies code compiles without running tests.

When: Before every commit, after code changes
SLO: <5s first build, <2s incremental

Example output:
  GREEN: "Finished dev [unoptimized + debuginfo] target(s) in 1.95s"
  RED: "error[E0425]: cannot find value `x` in this scope"

Recovery: Fix compilation errors and re-run
"#;
        let check = validate_description_components(comprehensive);
        assert!(check.has_purpose);
        assert!(check.has_timing);
        assert!(check.has_slo);
        assert!(check.has_examples);
        assert!(check.has_recovery);
        assert!(check.is_complete());
    }
}
