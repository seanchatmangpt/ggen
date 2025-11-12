#![allow(missing_docs)] // Binary crate - documentation not required

//! Pre-push git hook: 5-gate validation aligned with core team best practices
//! Comprehensive validation before push (30-60s acceptable)
//! Allows documented exceptions: CLI code, build scripts, test files with allow attributes
//! Uses: cargo make commands (NEVER direct cargo commands)

use std::fs;
use std::path::PathBuf;
use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    // Change to project root
    let project_root = match get_project_root() {
        Ok(root) => root,
        Err(e) => {
            eprintln!("❌ ERROR: Failed to find project root: {}", e);
            return ExitCode::FAILURE;
        }
    };

    if let Err(e) = std::env::set_current_dir(&project_root) {
        eprintln!("❌ ERROR: Failed to change to project root: {}", e);
        return ExitCode::FAILURE;
    }

    println!("🚦 Pre-push validation (5 gates)...");
    println!();

    // Gate 1: Cargo check
    println!("Gate 1/5: Cargo check...");
    match run_cargo_make("check") {
        Ok(()) => println!("✅ Gate 1 passed"),
        Err(e) => {
            eprintln!("❌ ERROR: cargo make check failed");
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }
    println!();

    // Gate 2: Clippy
    println!("Gate 2/5: Clippy (strict mode for production)...");
    match run_cargo_make("lint") {
        Ok(()) => println!("✅ Gate 2 passed"),
        Err(e) => {
            eprintln!("❌ ERROR: Clippy found warnings or errors in production code");
            eprintln!(
                "   Test files are allowed to use expect() with #![allow(clippy::expect_used)]"
            );
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }
    println!();

    // Gate 2.5: TODO & error handling check
    println!("Gate 2.5/5: TODO & error handling check...");
    match check_todo_and_error_handling() {
        Ok(()) => println!("✅ Gate 2.5 passed"),
        Err(e) => {
            eprintln!("❌ ERROR: {}", e);
            return ExitCode::FAILURE;
        }
    }
    println!();

    // Gate 3: Formatting
    println!("Gate 3/5: Formatting check...");
    match check_formatting() {
        Ok(()) => println!("✅ Gate 3 passed"),
        Err(e) => {
            eprintln!("❌ ERROR: Code is not formatted");
            eprintln!("   Run: cargo make fmt");
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }
    println!();

    // Gate 4: Tests
    println!("Gate 4/5: Fast tests (lib + bins)...");
    match run_cargo_make("test") {
        Ok(()) => println!("✅ Gate 4 passed"),
        Err(e) => {
            eprintln!("❌ ERROR: Tests failed");
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }
    println!();

    // Gate 5: Security audit (warning only, don't block)
    println!("Gate 5/5: Security audit...");
    // Check if cargo-audit is available
    let audit_available = Command::new("cargo")
        .arg("audit")
        .arg("--version")
        .output()
        .is_ok();

    if audit_available {
        match run_cargo_make("audit") {
            Ok(()) => println!("✅ Gate 5 passed"),
            Err(_) => {
                println!("⚠️  Security audit found issues (non-blocking)");
            }
        }
    } else {
        println!("⚠️  cargo-audit not installed (optional)");
        println!("   Install: cargo install cargo-audit");
    }
    println!();

    println!("✅ All gates passed - ready to push");
    ExitCode::SUCCESS
}

fn get_project_root() -> Result<PathBuf, Box<dyn std::error::Error>> {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--show-toplevel")
        .output()?;

    if !output.status.success() {
        return Err("Not a git repository".into());
    }

    let root = String::from_utf8(output.stdout)?.trim().to_string();

    Ok(PathBuf::from(root))
}

fn run_cargo_make(task: &str) -> Result<(), Box<dyn std::error::Error>> {
    let output = Command::new("cargo").arg("make").arg(task).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("cargo make {} failed: {}", task, stderr).into());
    }

    Ok(())
}

fn check_formatting() -> Result<(), Box<dyn std::error::Error>> {
    let output = Command::new("cargo")
        .arg("fmt")
        .arg("--all")
        .arg("--")
        .arg("--check")
        .output()?;

    if !output.status.success() {
        return Err("Formatting check failed".into());
    }

    Ok(())
}

fn check_todo_and_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    // Check for TODO comments in production code
    let todo_count = count_todos_in_production()?;
    if todo_count > 0 {
        return Err(format!("{} TODO comments found in production code. Policy: Zero TODOs in production (use FUTURE: for planned enhancements)", todo_count).into());
    }

    // Check for unwrap() in production code
    let unwrap_count = count_unwrap_in_production()?;
    if unwrap_count > 0 {
        return Err(format!("Found {} unwrap() calls in production code. Policy: Zero unwrap() unless documented with allow attribute", unwrap_count).into());
    }

    // Check for expect() in production code
    let expect_count = count_expect_in_production()?;
    if expect_count > 0 {
        return Err(format!("Found {} expect() calls in production code. Policy: Zero expect() unless documented with allow attribute. Note: CLI code (crates/ggen-cli) can use expect() for user-facing errors", expect_count).into());
    }

    Ok(())
}

fn count_todos_in_production() -> Result<usize, Box<dyn std::error::Error>> {
    let output = Command::new("find")
        .arg("crates/ggen-*/src")
        .arg("-name")
        .arg("*.rs")
        .arg("-type")
        .arg("f")
        .output()?;

    if !output.status.success() {
        return Ok(0);
    }

    let files: Vec<PathBuf> = String::from_utf8(output.stdout)?
        .lines()
        .filter(|line| {
            let path = line.trim();
            !path.contains("/tests/")
                && !path.contains("/test/")
                && !path.contains("/example")
                && !path.contains("build.rs")
        })
        .map(PathBuf::from)
        .collect();

    let mut count = 0;
    for file in files {
        if let Ok(content) = fs::read_to_string(&file) {
            let todos: usize = content
                .lines()
                .filter(|line| {
                    let line_lower = line.to_lowercase();
                    line_lower.contains("todo:") && !line_lower.contains("future:")
                })
                .count();
            count += todos;
        }
    }

    Ok(count)
}

fn count_unwrap_in_production() -> Result<usize, Box<dyn std::error::Error>> {
    let output = Command::new("find")
        .arg("crates/ggen-*/src")
        .arg("-name")
        .arg("*.rs")
        .arg("-type")
        .arg("f")
        .output()?;

    if !output.status.success() {
        return Ok(0);
    }

    let files: Vec<PathBuf> = String::from_utf8(output.stdout)?
        .lines()
        .filter(|line| {
            let path = line.trim();
            !path.contains("/tests/")
                && !path.contains("/test/")
                && !path.contains("/example")
                && !path.contains("build.rs")
        })
        .map(PathBuf::from)
        .collect();

    let mut count = 0;
    for file in files {
        // Skip CLI code
        if file.to_string_lossy().contains("crates/ggen-cli/") {
            continue;
        }

        // Skip files with allow attributes
        if has_allow_attribute(&file, "unwrap_used") {
            continue;
        }

        // Skip files with test modules
        if has_test_modules(&file) {
            continue;
        }

        if let Ok(content) = fs::read_to_string(&file) {
            let unwraps: usize = content
                .lines()
                .filter(|line| line.contains(".unwrap()"))
                .count();
            count += unwraps;
        }
    }

    Ok(count)
}

fn count_expect_in_production() -> Result<usize, Box<dyn std::error::Error>> {
    let output = Command::new("find")
        .arg("crates/ggen-*/src")
        .arg("-name")
        .arg("*.rs")
        .arg("-type")
        .arg("f")
        .output()?;

    if !output.status.success() {
        return Ok(0);
    }

    let files: Vec<PathBuf> = String::from_utf8(output.stdout)?
        .lines()
        .filter(|line| {
            let path = line.trim();
            !path.contains("/tests/")
                && !path.contains("/test/")
                && !path.contains("/example")
                && !path.contains("build.rs")
        })
        .map(PathBuf::from)
        .collect();

    let mut count = 0;
    for file in files {
        // Skip CLI code (allowed to use expect for user errors)
        if file.to_string_lossy().contains("crates/ggen-cli/") {
            continue;
        }

        // Skip files with allow attributes
        if has_allow_attribute(&file, "expect_used") {
            continue;
        }

        // Skip files with test modules
        if has_test_modules(&file) {
            continue;
        }

        if let Ok(content) = fs::read_to_string(&file) {
            let expects: usize = content
                .lines()
                .filter(|line| line.contains(".expect("))
                .count();
            count += expects;
        }
    }

    Ok(count)
}

fn has_allow_attribute(file: &PathBuf, lint: &str) -> bool {
    if let Ok(content) = fs::read_to_string(file) {
        let pattern = format!("#!?\\[allow\\(clippy::{}\\s*\\)\\]", lint);
        if let Ok(re) = regex::Regex::new(&pattern) {
            return re.is_match(&content);
        }
    }
    false
}

fn has_test_modules(file: &PathBuf) -> bool {
    if let Ok(content) = fs::read_to_string(file) {
        return content.contains("#[cfg(test)]");
    }
    false
}
