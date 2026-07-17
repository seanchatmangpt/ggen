#![allow(
    warnings,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::todo,
    clippy::unimplemented
)]
#![allow(missing_docs)] // Binary crate - documentation not required

//! Verify lint task uses --lib not --all-targets
//! This prevents regression to the configuration that hid production code warnings

use std::fs;

fn main() {
    if let Err(e) = verify_lint_config() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
    println!("✅ Lint configuration verified: uses --lib only with -D warnings");
}

fn verify_lint_config() -> Result<(), String> {
    let makefile_path = "Makefile.toml";
    let contents = fs::read_to_string(makefile_path)
        .map_err(|e| format!("Failed to read {makefile_path}: {e}"))?;

    // Extract args array specifically to check only args, not comments
    let args_section = extract_args_array(&contents);

    // Check for --all-targets in args array (should not be present)
    if args_section.contains("--all-targets") {
        return Err(
            "❌ ERROR: Lint task uses --all-targets (should use --lib only)\n   This would cause test code warnings to hide production code warnings\n   Root cause: See docs/ROOT_CAUSE_ANALYSIS_63_CLIPPY_WARNINGS.md"
                .to_string(),
        );
    }

    // Check that lint task uses --lib in args (must be present)
    if !args_section.contains("--lib") {
        return Err(
            "❌ ERROR: Lint task does not use --lib\n   Lint task must check production code only (--lib)\n   Root cause: See docs/ROOT_CAUSE_ANALYSIS_63_CLIPPY_WARNINGS.md"
                .to_string(),
        );
    }

    // Check that lint task has -D warnings in args
    if !args_section.contains("-D") || !args_section.contains("warnings") {
        return Err(
            "❌ ERROR: Lint task does not use -D warnings\n   Lint task must treat warnings as errors for production code"
                .to_string(),
        );
    }

    Ok(())
}

fn extract_args_array(contents: &str) -> String {
    let mut in_lint_section = false;
    let mut in_args_array = false;
    let mut args_lines = Vec::new();

    for line in contents.lines() {
        if line.trim().starts_with("[tasks.lint]") {
            in_lint_section = true;
        } else if in_lint_section {
            // Stop at next task section
            if line.trim().starts_with("[tasks.") && !line.trim().starts_with("[tasks.lint]") {
                break;
            }

            // Track args array specifically
            if line.trim().starts_with("args = [") {
                in_args_array = true;
                args_lines.push(line);
            } else if in_args_array {
                args_lines.push(line);
                if line.trim() == "]" {
                    break; // End of args array
                }
            }
        }
    }

    args_lines.join("\n")
}
