use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let lockfile_path = PathBuf::from("/tmp/test/.ggen/packs.lock");

    // Create new lockfile
    let mut lockfile = PackLockfile::new("6.0.1");

    // Add a pack
    let pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Local {
            path: PathBuf::from("/tmp/test/.ggen/packs/surface-mcp"),
        },
        integrity: None,
        installed_at: chrono::Utc::now(),
        dependencies: vec![],
    };

    lockfile.add_pack("surface-mcp", pack);

    // Save lockfile
    lockfile.save(&lockfile_path)?;

    println!("✓ Lockfile created at {}", lockfile_path.display());

    // Load and verify
    let loaded = PackLockfile::from_file(&lockfile_path)?;
    println!("✓ Lockfile loaded with {} packs", loaded.packs.len());

    // Print contents
    let content = std::fs::read_to_string(&lockfile_path)?;
    println!("\nLockfile contents:\n{}", content);

    Ok(())
}
