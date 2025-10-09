use anyhow::{bail, Context, Result};
use clap::Args;
use rgen_core::{CacheManager, LockfileManager, RegistryClient};
use std::env;

#[derive(Args, Debug)]
pub struct AddArgs {
    /// Rpack ID with optional version (e.g., "io.rgen.rust.cli-subcommand@0.2.0")
    pub rpack_id: String,
}

pub async fn run(args: &AddArgs) -> Result<()> {
    let (pack_id, version) = parse_rpack_spec(&args.rpack_id)?;

    // Get current working directory
    let current_dir = env::current_dir().context("Failed to get current directory")?;

    // Initialize managers
    let registry_client = RegistryClient::new()?;
    let cache_manager = CacheManager::new()?;
    let lockfile_manager = LockfileManager::new(&current_dir);

    // Check if already installed
    if lockfile_manager.is_installed(&pack_id)? {
        println!("Rpack '{}' is already installed", pack_id);
        return Ok(());
    }

    // Resolve pack from registry
    println!("Resolving rpack '{}'...", pack_id);
    let resolved_pack = registry_client
        .resolve(&pack_id, version.as_deref())
        .await
        .with_context(|| format!("Failed to resolve rpack '{}'", pack_id))?;

    println!(
        "Found rpack '{}' version {}",
        resolved_pack.id, resolved_pack.version
    );

    // Download and cache the pack
    println!("Downloading rpack...");
    let cached_pack = cache_manager
        .ensure(&resolved_pack)
        .await
        .with_context(|| format!("Failed to download rpack '{}'", pack_id))?;

    println!("Cached rpack to: {}", cached_pack.path.display());

    // Update lockfile
    println!("Updating lockfile...");
    lockfile_manager.upsert(
        &resolved_pack.id,
        &resolved_pack.version,
        &resolved_pack.sha256,
        &resolved_pack.git_url,
    )?;

    println!(
        "✅ Successfully added rpack '{}' version {}",
        resolved_pack.id, resolved_pack.version
    );

    // Show available templates if any
    if let Some(manifest) = &cached_pack.manifest {
        if !manifest.templates.patterns.is_empty() {
            println!("\nAvailable template patterns:");
            for pattern in &manifest.templates.patterns {
                println!("  - {}:{}", pack_id, pattern);
            }
        }
    }

    Ok(())
}

fn parse_rpack_spec(spec: &str) -> Result<(String, Option<String>)> {
    if spec.is_empty() {
        bail!("Invalid rpack spec: empty string. Expected format: <rpack-id>[@version]");
    }

    if let Some(at_pos) = spec.rfind('@') {
        let id = spec[..at_pos].to_string();
        let version = spec[at_pos + 1..].to_string();

        if id.is_empty() || version.is_empty() {
            bail!(
                "Invalid rpack spec: '{}'. Expected format: <rpack-id>[@version]",
                spec
            );
        }

        Ok((id, Some(version)))
    } else {
        Ok((spec.to_string(), None))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_rpack_spec() {
        let (id, version) = parse_rpack_spec("io.rgen.test").unwrap();
        assert_eq!(id, "io.rgen.test");
        assert_eq!(version, None);

        let (id, version) = parse_rpack_spec("io.rgen.test@0.1.0").unwrap();
        assert_eq!(id, "io.rgen.test");
        assert_eq!(version, Some("0.1.0".to_string()));
    }

    #[test]
    fn test_parse_rpack_spec_invalid() {
        assert!(parse_rpack_spec("@0.1.0").is_err());
        assert!(parse_rpack_spec("io.rgen.test@").is_err());
        assert!(parse_rpack_spec("").is_err());
    }
}
