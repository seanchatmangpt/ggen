use anyhow::{bail, Context, Result};
use clap::Args;
use ggen_core::{CacheManager, LockfileManager, RegistryClient};
use ggen_utils::error::Result as GgenResult;
use std::env;

#[derive(Args, Debug)]
pub struct AddArgs {
    /// Gpack ID with optional version (e.g., "io.ggen.rust.cli-subcommand@0.2.0")
    pub gpack_id: String,
}

/// Validate and sanitize gpack specification input
fn validate_gpack_input(spec: &str) -> GgenResult<()> {
    // Validate gpack ID is not empty
    if spec.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Gpack ID cannot be empty"));
    }
    // Validate gpack ID length
    if spec.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Gpack ID too long (max 200 characters)",
        ));
    }
    // Validate gpack ID format (basic pattern check)
    if !spec
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '@' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid gpack ID format: only alphanumeric characters, dots, dashes, underscores, and @ allowed",
        ));
    }
    // Validate version format if present
    if let Some(pos) = spec.rfind('@') {
        let version = &spec[pos + 1..];
        if version.is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Version cannot be empty when @ is specified",
            ));
        }
        // Basic semantic version validation
        if !version
            .chars()
            .all(|c| c.is_alphanumeric() || c == '.' || c == '-')
        {
            return Err(ggen_utils::error::Error::new(
                "Invalid version format: only alphanumeric characters, dots, and dashes allowed",
            ));
        }
    }
    Ok(())
}

pub async fn run(args: &AddArgs) -> Result<()> {
    validate_gpack_input(&args.gpack_id)?;
    let (pack_id, version) = parse_gpack_spec(&args.gpack_id)?;

    // Get current working directory
    let current_dir = env::current_dir().context("Failed to get current directory")?;

    // Initialize managers
    let registry_client = RegistryClient::new()?;
    let cache_manager = CacheManager::new()?;
    let lockfile_manager = LockfileManager::new(&current_dir);

    // Check if already installed
    if lockfile_manager.is_installed(&pack_id)? {
        println!("Gpack '{}' is already installed", pack_id);
        return Ok(());
    }

    // Resolve pack from registry
    println!("🔍 Resolving gpack '{}'...", pack_id);
    let resolved_pack = registry_client
        .resolve(&pack_id, version.as_deref())
        .await
        .with_context(|| format!("Failed to resolve gpack '{}'", pack_id))?;

    println!(
        "Found gpack '{}' version {}",
        resolved_pack.id, resolved_pack.version
    );

    // Download and cache the pack
    println!("📦 Downloading gpack...");
    let cached_pack = cache_manager
        .ensure(&resolved_pack)
        .await
        .with_context(|| format!("Failed to download gpack '{}'", pack_id))?;

    println!("Cached gpack to: {}", cached_pack.path.display());

    // Update lockfile with actual calculated SHA256 from cached pack
    println!("📝 Updating lockfile...");
    lockfile_manager.upsert(
        &resolved_pack.id,
        &resolved_pack.version,
        &cached_pack.sha256, // Use actual calculated SHA256, not registry placeholder
        &resolved_pack.git_url,
    )?;

    println!(
        "✅ Successfully added gpack '{}' version {}",
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

fn parse_gpack_spec(spec: &str) -> Result<(String, Option<String>)> {
    if spec.is_empty() {
        bail!("Invalid gpack spec: empty string. Expected format: <gpack-id>[@version]");
    }

    if let Some(at_pos) = spec.rfind('@') {
        let id = spec[..at_pos].to_string();
        let version = spec[at_pos + 1..].to_string();

        if id.is_empty() || version.is_empty() {
            bail!(
                "Invalid gpack spec: '{}'. Expected format: <gpack-id>[@version]",
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
    fn test_parse_gpack_spec() {
        let (id, version) = parse_gpack_spec("io.ggen.test").unwrap();
        assert_eq!(id, "io.ggen.test");
        assert_eq!(version, None);

        let (id, version) = parse_gpack_spec("io.ggen.test@0.1.0").unwrap();
        assert_eq!(id, "io.ggen.test");
        assert_eq!(version, Some("0.1.0".to_string()));
    }

    #[test]
    fn test_parse_gpack_spec_invalid() {
        assert!(parse_gpack_spec("@0.1.0").is_err());
        assert!(parse_gpack_spec("io.ggen.test@").is_err());
        assert!(parse_gpack_spec("").is_err());
    }
}
