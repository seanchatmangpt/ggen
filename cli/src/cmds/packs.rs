use anyhow::Result;
use ggen_core::LockfileManager;
use ggen_utils::error::Result as GgenResult;
use std::env;

/// Validate packs command input (no arguments needed)
fn validate_packs_input() -> GgenResult<()> {
    // No specific validation needed for packs command
    // Main validation is ensuring current directory access
    Ok(())
}

pub fn run() -> Result<()> {
    validate_packs_input()?;

    // Get current working directory
    println!("🔍 Loading installed gpacks...");
    let current_dir = env::current_dir().map_err(|e| {
        ggen_utils::error::Error::new_fmt(format_args!("Failed to access current directory: {}", e))
    })?;
    let lockfile_manager = LockfileManager::new(&current_dir);

    // Load installed packs
    let installed_packs = lockfile_manager.list()?;

    if installed_packs.is_empty() {
        println!("No gpacks installed in this project");
        return Ok(());
    }

    println!("Installed gpacks ({}):", installed_packs.len());
    println!();

    // Header
    println!("{:<40} {:<12} {:<20} SHA256", "ID", "VERSION", "SOURCE");
    println!("{}", "-".repeat(80));

    // List packs
    for pack in installed_packs {
        let source = if pack.source.len() > 20 {
            format!("{}...", &pack.source[..17])
        } else {
            pack.source
        };

        let sha256 = if pack.sha256.len() > 20 {
            format!("{}...", &pack.sha256[..17])
        } else {
            pack.sha256
        };

        println!(
            "{:<40} {:<12} {:<20} {}",
            pack.id, pack.version, source, sha256
        );
    }

    Ok(())
}
