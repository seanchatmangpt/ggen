#![allow(missing_docs)] // Binary crate - documentation not required

//! Pre-commit git hook: Fast validation aligned with core team 80/20 best practices
//! Target: 2-5 seconds (only checks staged files/packages)
//! Enforces: No unwrap/expect/TODO/FUTURE/unimplemented on MAIN branch only
//! Other branches: relaxed rules (TODO/FUTURE/unimplemented allowed)
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

    println!("🔍 Running pre-commit validation...");

    // Get staged Rust files
    let staged_files = match get_staged_rust_files() {
        Ok(files) => files,
        Err(e) => {
            eprintln!("❌ ERROR: Failed to get staged files: {}", e);
            return ExitCode::FAILURE;
        }
    };

    if staged_files.is_empty() {
        println!("✅ No Rust files staged, skipping validation");
        return ExitCode::SUCCESS;
    }

    // Detect current branch
    let is_main_branch = is_main_branch();
    if is_main_branch {
        println!(
            "🔒 Main branch detected - enforcing strict rules (no TODO/FUTURE/unimplemented!)"
        );
    } else {
        let branch = get_current_branch().unwrap_or_else(|_| "unknown".to_string());
        println!(
            "🌿 Branch '{}' - strict rules relaxed (TODO/FUTURE/unimplemented! allowed)",
            branch
        );
    }

    // Check 1: No unwrap() in production code
    println!("   Checking for unwrap() calls in production code...");
    match check_unwrap_in_production(&staged_files) {
        Ok(count) => {
            if count > 0 {
                eprintln!(
                    "❌ ERROR: Cannot commit {} unwrap() calls in production code",
                    count
                );
                eprintln!("   Replace with proper Result<T,E> error handling");
                eprintln!("   Use ? operator or match statements instead");
                eprintln!("   Or add #![allow(clippy::unwrap_used)] if truly necessary");
                return ExitCode::FAILURE;
            }
            println!("  ✅ No unwrap() in production code");
        }
        Err(e) => {
            eprintln!("❌ ERROR: Failed to check unwrap(): {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Check 2: No unimplemented!() placeholders - BLOCKED ONLY ON MAIN
    if is_main_branch {
        println!("   Checking for unimplemented!() placeholders...");
        match check_unimplemented(&staged_files) {
            Ok(count) => {
                if count > 0 {
                    eprintln!(
                        "❌ ERROR: Cannot commit {} unimplemented!() placeholders to main",
                        count
                    );
                    eprintln!("   Complete implementations before committing - NO EXCEPTIONS");
                    return ExitCode::FAILURE;
                }
                println!("  ✅ No unimplemented!() placeholders");
            }
            Err(e) => {
                eprintln!("❌ ERROR: Failed to check unimplemented!(): {}", e);
                return ExitCode::FAILURE;
            }
        }
    } else {
        println!("  ⏭️  Skipping unimplemented!() check (not on main branch)");
    }

    // Check 3: No FUTURE or TODO comments - BLOCKED ONLY ON MAIN
    if is_main_branch {
        println!("   Checking for FUTURE/TODO comments...");
        match check_todo_future(&staged_files) {
            Ok(count) => {
                if count > 0 {
                    eprintln!(
                        "❌ ERROR: Cannot commit {} FUTURE/TODO comments to main",
                        count
                    );
                    eprintln!(
                        "   Remove ALL TODO/FUTURE comments before committing - NO EXCEPTIONS"
                    );
                    eprintln!("   This applies to ALL code including tests");
                    return ExitCode::FAILURE;
                }
                println!("  ✅ No FUTURE/TODO comments");
            }
            Err(e) => {
                eprintln!("❌ ERROR: Failed to check TODO/FUTURE: {}", e);
                return ExitCode::FAILURE;
            }
        }
    } else {
        println!("  ⏭️  Skipping FUTURE/TODO check (not on main branch)");
    }

    // Check 4: No expect() in production code (excluding CLI, test files, build scripts)
    println!("   Checking for expect() calls in production code...");
    match check_expect_in_production(&staged_files) {
        Ok(count) => {
            if count > 0 {
                eprintln!(
                    "❌ ERROR: Cannot commit {} expect() calls in production code",
                    count
                );
                eprintln!(
                    "   Replace with proper error handling or add #![allow(clippy::expect_used)]"
                );
                eprintln!(
                    "   Note: CLI code (crates/ggen-cli) can use expect() for user-facing errors"
                );
                return ExitCode::FAILURE;
            }
            println!("  ✅ No expect() in production code (CLI exempt)");
        }
        Err(e) => {
            eprintln!("❌ ERROR: Failed to check expect(): {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Check 5: Formatting
    println!("   Checking Rust formatting...");
    match check_formatting() {
        Ok(()) => println!("  ✅ Code is formatted"),
        Err(e) => {
            eprintln!("❌ ERROR: Code is not formatted");
            eprintln!("   Run: cargo make fmt");
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Check 6: Quick clippy check (only on staged packages)
    println!("   Running clippy on staged packages...");
    match check_clippy(&staged_files) {
        Ok(()) => println!("  ✅ Clippy checks passed"),
        Err(e) => {
            eprintln!("❌ ERROR: Clippy found issues");
            eprintln!("   Fix clippy warnings before committing");
            eprintln!("   Run: cargo make lint");
            eprintln!("   {}", e);
            return ExitCode::FAILURE;
        }
    }

    // Check 7: Historical documentation files should not be committed (archive removed)
    println!("   Checking for historical documentation files...");
    match check_historical_docs() {
        Ok(count) => {
            if count > 0 {
                eprintln!(
                    "❌ ERROR: Cannot commit {} historical documentation file(s) to docs/",
                    count
                );
                eprintln!("   Historical reports (*COMPLETION*.md, *SUMMARY*.md, *STATUS*.md) should not be committed");
                eprintln!("   These files are archived in git history - do not add new historical reports");
                return ExitCode::FAILURE;
            }
            println!("  ✅ No historical docs in wrong location");
        }
        Err(e) => {
            eprintln!("❌ ERROR: Failed to check historical docs: {}", e);
            return ExitCode::FAILURE;
        }
    }

    println!("✅ Pre-commit validation passed");
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

fn get_staged_rust_files() -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let output = Command::new("git")
        .arg("diff")
        .arg("--cached")
        .arg("--name-only")
        .arg("--diff-filter=d")
        .output()?;

    if !output.status.success() {
        return Err("Failed to get staged files".into());
    }

    let files: Vec<PathBuf> = String::from_utf8(output.stdout)?
        .lines()
        .filter(|line| line.ends_with(".rs"))
        .map(PathBuf::from)
        .collect();

    Ok(files)
}

fn get_current_branch() -> Result<String, Box<dyn std::error::Error>> {
    let output = Command::new("git")
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("HEAD")
        .output()?;

    if !output.status.success() {
        return Ok("unknown".to_string());
    }

    Ok(String::from_utf8(output.stdout)?.trim().to_string())
}

fn is_main_branch() -> bool {
    match get_current_branch() {
        Ok(branch) => branch == "main" || branch == "master",
        Err(_) => false,
    }
}

fn is_test_file(file: &PathBuf) -> bool {
    let path_str = file.to_string_lossy();
    path_str.contains("/test")
        || path_str.contains("/tests")
        || path_str.contains("/example")
        || path_str.contains("/examples")
        || path_str.contains("/bench")
        || path_str.contains("/benches")
        || path_str.ends_with("build.rs")
        || path_str.starts_with("test/")
        || path_str.starts_with("tests/")
        || path_str.starts_with("example/")
        || path_str.starts_with("examples/")
        || path_str.starts_with("bench/")
        || path_str.starts_with("benches/")
}

fn is_cli_file(file: &PathBuf) -> bool {
    let path_str = file.to_string_lossy();
    path_str.contains("crates/ggen-cli/") || path_str.starts_with("crates/ggen-cli/")
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

fn get_staged_diff(file: &PathBuf) -> Result<String, Box<dyn std::error::Error>> {
    let output = Command::new("git")
        .arg("diff")
        .arg("--cached")
        .arg(file)
        .output()?;

    Ok(String::from_utf8(output.stdout)?)
}

fn check_unwrap_in_production(files: &[PathBuf]) -> Result<usize, Box<dyn std::error::Error>> {
    let mut count = 0;

    for file in files {
        if is_test_file(file) {
            continue;
        }

        if has_allow_attribute(file, "unwrap_used") {
            continue;
        }

        if has_test_modules(file) {
            // Pragmatic exception: files with test modules allowed for pre-commit
            continue;
        }

        let diff = get_staged_diff(file)?;
        let unwraps: usize = diff
            .lines()
            .filter(|line| line.starts_with('+') && line.contains(".unwrap()"))
            .count();

        if unwraps > 0 {
            println!(
                "     ❌ {}: {} unwrap() call(s) found",
                file.display(),
                unwraps
            );
            count += unwraps;
        }
    }

    Ok(count)
}

fn check_unimplemented(files: &[PathBuf]) -> Result<usize, Box<dyn std::error::Error>> {
    let mut count = 0;

    for file in files {
        let diff = get_staged_diff(file)?;
        let unimpls: usize = diff
            .lines()
            .filter(|line| line.starts_with('+') && line.contains("unimplemented!"))
            .count();

        if unimpls > 0 {
            println!(
                "     ❌ {}: {} unimplemented!() placeholder(s) found",
                file.display(),
                unimpls
            );
            for line in diff
                .lines()
                .filter(|l| l.starts_with('+') && l.contains("unimplemented!"))
            {
                println!("       {}", line);
            }
            count += unimpls;
        }
    }

    Ok(count)
}

fn check_todo_future(files: &[PathBuf]) -> Result<usize, Box<dyn std::error::Error>> {
    let mut count = 0;

    for file in files {
        // Skip documentation files
        if file.extension().and_then(|e| e.to_str()) == Some("md")
            || file.extension().and_then(|e| e.to_str()) == Some("txt")
            || file.extension().and_then(|e| e.to_str()) == Some("rst")
        {
            continue;
        }

        let diff = get_staged_diff(file)?;
        let todos: usize = diff
            .lines()
            .filter(|line| {
                if !line.starts_with('+') {
                    return false;
                }
                let line_lower = line.to_lowercase();
                line_lower.contains("todo") || line_lower.contains("future")
            })
            .count();

        if todos > 0 {
            println!(
                "     ❌ {}: {} FUTURE/TODO comment(s) found",
                file.display(),
                todos
            );
            for line in diff
                .lines()
                .filter(|l| {
                    if !l.starts_with('+') {
                        return false;
                    }
                    let line_lower = l.to_lowercase();
                    line_lower.contains("todo") || line_lower.contains("future")
                })
                .take(5)
            {
                println!("       {}", line);
            }
            count += todos;
        }
    }

    Ok(count)
}

fn check_expect_in_production(files: &[PathBuf]) -> Result<usize, Box<dyn std::error::Error>> {
    let mut count = 0;

    for file in files {
        if is_test_file(file) {
            continue;
        }

        if is_cli_file(file) {
            // CLI code can use expect() for user-facing errors
            continue;
        }

        if has_allow_attribute(file, "expect_used") {
            continue;
        }

        if has_test_modules(file) {
            // Pragmatic exception: files with test modules allowed for pre-commit
            continue;
        }

        let diff = get_staged_diff(file)?;
        let expects: usize = diff
            .lines()
            .filter(|line| line.starts_with('+') && line.contains(".expect("))
            .count();

        if expects > 0 {
            println!(
                "     ❌ {}: {} expect() call(s) found",
                file.display(),
                expects
            );
            count += expects;
        }
    }

    Ok(count)
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

fn check_clippy(files: &[PathBuf]) -> Result<(), Box<dyn std::error::Error>> {
    // Get unique packages from staged files
    let mut packages: Vec<String> = files
        .iter()
        .filter_map(|f| {
            let path_str = f.to_string_lossy();
            if let Some(stripped) = path_str.strip_prefix("crates/") {
                if let Some(pkg) = stripped.split('/').next() {
                    if pkg.starts_with("ggen-") {
                        return Some(pkg.to_string());
                    }
                }
            }
            None
        })
        .collect();

    packages.sort();
    packages.dedup();

    for pkg in packages {
        let output = Command::new("cargo")
            .arg("clippy")
            .arg("--package")
            .arg(&pkg)
            .arg("--lib")
            .arg("--bins")
            .arg("--")
            .arg("-D")
            .arg("warnings")
            .output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Filter out test-related warnings
            let production_issues: Vec<&str> = stderr
                .lines()
                .filter(|line| {
                    !line.contains("test")
                        && !line.contains("tests")
                        && !line.contains("example")
                        && !line.contains("examples")
                        && !line.contains("bench")
                        && !line.contains("benches")
                        && (line.contains("error") || line.contains("warning"))
                })
                .collect();

            if !production_issues.is_empty() {
                eprintln!("❌ ERROR: Clippy found issues in {}", pkg);
                for issue in production_issues.iter().take(20) {
                    eprintln!("   {}", issue);
                }
                return Err("Clippy check failed".into());
            }
        }
    }

    Ok(())
}

fn get_staged_markdown_files() -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let output = Command::new("git")
        .arg("diff")
        .arg("--cached")
        .arg("--name-only")
        .arg("--diff-filter=d")
        .output()?;

    if !output.status.success() {
        return Err("Failed to get staged files".into());
    }

    let files: Vec<PathBuf> = String::from_utf8(output.stdout)?
        .lines()
        .filter(|line| line.ends_with(".md"))
        .map(PathBuf::from)
        .collect();

    Ok(files)
}

fn check_historical_docs() -> Result<usize, Box<dyn std::error::Error>> {
    let staged_md_files = get_staged_markdown_files()?;
    let mut count = 0;

    for file in &staged_md_files {
        let path_str = file.to_string_lossy();

        // Only check files in docs/ directory (archive/ removed - all historical docs should be avoided)
        if !path_str.starts_with("docs/") {
            continue;
        }

        let filename = file.file_name().and_then(|n| n.to_str()).unwrap_or("");

        // Check for historical documentation patterns
        if filename.contains("COMPLETION")
            || filename.contains("SUMMARY")
            || filename.contains("STATUS")
        {
            println!(
                "     ❌ {}: Historical documentation file should not be committed (archive removed)",
                file.display()
            );
            count += 1;
        }
    }

    Ok(count)
}
