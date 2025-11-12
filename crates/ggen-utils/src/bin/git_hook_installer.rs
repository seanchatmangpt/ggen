#![allow(missing_docs)] // Binary crate - documentation not required

//! Git hook installer: Install Rust-based git hooks
//! Copies Rust binaries directly to .git/hooks/ (no bash wrappers)
//! Ensures hooks are version-controlled and testable

use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Get project root
    let project_root = get_project_root()?;

    // Get .git/hooks directory
    let hooks_dir = project_root.join(".git/hooks");

    // Create hooks directory if it doesn't exist
    fs::create_dir_all(&hooks_dir)?;

    println!("🔧 Installing git hooks...");

    // Build hook binaries first
    println!("   Building hook binaries...");
    let build_output = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .arg("--package")
        .arg("ggen-utils")
        .arg("--bin")
        .arg("git_hook_pre_commit")
        .arg("--bin")
        .arg("git_hook_pre_push")
        .current_dir(&project_root)
        .output()?;

    if !build_output.status.success() {
        eprintln!("❌ ERROR: Failed to build hook binaries");
        eprintln!("{}", String::from_utf8_lossy(&build_output.stderr));
        std::process::exit(1);
    }

    // Determine binary path (release or debug)
    let release_binary = project_root.join("target/release/git_hook_pre_commit");
    let debug_binary = project_root.join("target/debug/git_hook_pre_commit");

    let binary_dir = if release_binary.exists() {
        project_root.join("target/release")
    } else if debug_binary.exists() {
        project_root.join("target/debug")
    } else {
        // Default to debug if neither exists (will be built)
        project_root.join("target/debug")
    };

    // Install pre-commit hook (copy binary directly, no bash wrapper)
    install_hook_binary(&hooks_dir, "pre-commit", &binary_dir, "git_hook_pre_commit")?;
    println!("  ✅ pre-commit hook installed");

    // Install pre-push hook (copy binary directly, no bash wrapper)
    install_hook_binary(&hooks_dir, "pre-push", &binary_dir, "git_hook_pre_push")?;
    println!("  ✅ pre-push hook installed");

    println!("✅ Git hooks installed successfully");
    println!("   Hooks are located in: {}", hooks_dir.display());
    println!("   Run hooks manually: cargo run --bin git_hook_pre_commit --package ggen-utils");

    Ok(())
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

fn install_hook_binary(
    hooks_dir: &std::path::Path, hook_name: &str, binary_dir: &std::path::Path, binary_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    use std::os::unix::fs::PermissionsExt;

    let hook_path = hooks_dir.join(hook_name);
    let source_binary = binary_dir.join(binary_name);

    // Verify source binary exists
    if !source_binary.exists() {
        return Err(format!(
            "Hook binary not found: {}. Build with: cargo build --package ggen-utils --bin {}",
            source_binary.display(),
            binary_name
        )
        .into());
    }

    // Copy binary directly to .git/hooks/ (no bash wrapper)
    fs::copy(&source_binary, &hook_path)?;

    // Make executable (Unix-like systems)
    // On Windows, executables are already executable by extension
    #[cfg(unix)]
    {
        let mut perms = fs::metadata(&hook_path)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&hook_path, perms)?;
    }

    Ok(())
}
