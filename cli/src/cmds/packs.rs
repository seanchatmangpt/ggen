use anyhow::Result;
use ggen_core::LockfileManager;
use std::env;

pub fn run() -> Result<()> {
    // Get current working directory
    let current_dir = env::current_dir()?;
    let lockfile_manager = LockfileManager::new(&current_dir);

    // Load installed packs
    let installed_packs = lockfile_manager.list()?;

    if installed_packs.is_empty() {
        println!("No rpacks installed in this project");
        return Ok(());
    }

    println!("Installed rpacks ({}):", installed_packs.len());
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
