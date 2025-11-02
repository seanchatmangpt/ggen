//! Domain logic for marketplace package installation
//!
//! This module contains the core business logic for installing packages,
//! separated from CLI concerns for better testability and reusability.

use clap::Args;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Install command arguments
#[derive(Debug, Args)]
pub struct InstallArgs {
    /// Package name (format: name@version)
    pub package: String,

    /// Target directory
    #[arg(short = 't', long)]
    pub target: Option<String>,

    /// Force overwrite
    #[arg(short = 'f', long)]
    pub force: bool,

    /// Skip dependencies
    #[arg(long)]
    pub no_dependencies: bool,

    /// Dry run (simulate installation)
    #[arg(long)]
    pub dry_run: bool,
}

/// Package installation options
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InstallOptions {
    pub package_name: String,
    pub version: Option<String>,
    pub target_path: Option<PathBuf>,
    pub force: bool,
    pub with_dependencies: bool,
    pub dry_run: bool,
}

impl InstallOptions {
    pub fn new(package_name: impl Into<String>) -> Self {
        Self {
            package_name: package_name.into(),
            with_dependencies: true,
            ..Default::default()
        }
    }

    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }

    pub fn with_target(mut self, path: PathBuf) -> Self {
        self.target_path = Some(path);
        self
    }

    pub fn force(mut self) -> Self {
        self.force = true;
        self
    }

    pub fn dry_run(mut self) -> Self {
        self.dry_run = true;
        self
    }

    pub fn no_dependencies(mut self) -> Self {
        self.with_dependencies = false;
        self
    }
}

/// Install package result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallResult {
    pub package_name: String,
    pub version: String,
    pub install_path: PathBuf,
    pub dependencies_installed: Vec<String>,
}

/// Install a package from the marketplace
///
/// This is a placeholder implementation for Phase 1.
/// Phase 2 will implement actual installation logic with registry integration.
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult> {
    // Placeholder: Return a dummy result for now
    // In Phase 2, this will actually download and install the package
    let _ = options;

    Err(ggen_utils::error::Error::new("Install not yet implemented (Phase 2)"))
}

/// Install package and report progress
///
/// This function bridges the CLI to the domain layer.
pub async fn install_and_report(
    package: &str,
    target: Option<&str>,
    force: bool,
    with_dependencies: bool,
    dry_run: bool,
) -> Result<()> {
    // Parse package specification (name@version)
    let (package_name, version) = match package.rsplit_once('@') {
        Some((name, ver)) => (name.to_string(), Some(ver.to_string())),
        None => (package.to_string(), None),
    };

    // Build install options
    let mut options = InstallOptions::new(package_name);

    if let Some(ver) = version {
        options = options.with_version(ver);
    }

    if let Some(target_path) = target {
        options = options.with_target(PathBuf::from(target_path));
    }

    if force {
        options = options.force();
    }

    if !with_dependencies {
        options = options.no_dependencies();
    }

    if dry_run {
        options = options.dry_run();
    }

    // Display dry run information
    if dry_run {
        println!("🔍 Dry run: Would install package");
        println!("   Package: {}", options.package_name);
        if let Some(ref ver) = options.version {
            println!("   Version: {}", ver);
        }
        if let Some(ref path) = options.target_path {
            println!("   Target: {}", path.display());
        }
        println!("   Dependencies: {}", if with_dependencies { "yes" } else { "no" });
        return Ok(());
    }

    // Install package
    println!("📦 Installing {}...", package);

    match install_package(&options).await {
        Ok(result) => {
            println!("✅ Successfully installed {} v{}", result.package_name, result.version);
            println!("   Location: {}", result.install_path.display());

            if !result.dependencies_installed.is_empty() {
                println!("   Dependencies: {}", result.dependencies_installed.join(", "));
            }

            Ok(())
        }
        Err(e) => {
            // For Phase 1, show placeholder message
            println!("ℹ️  Package installation not yet implemented (Phase 2)");
            println!("   Package: {}", package);
            Err(e)
        }
    }
}

/// Run install command (sync wrapper for CLI)
pub fn run(args: &InstallArgs) -> Result<()> {
    crate::runtime::block_on(async {
        install_and_report(
            &args.package,
            args.target.as_deref(),
            args.force,
            !args.no_dependencies,
            args.dry_run,
        )
        .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_options_builder() {
        let options = InstallOptions::new("my-package")
            .with_version("1.0.0")
            .force()
            .dry_run();

        assert_eq!(options.package_name, "my-package");
        assert_eq!(options.version, Some("1.0.0".to_string()));
        assert!(options.force);
        assert!(options.dry_run);
        assert!(options.with_dependencies);
    }

    #[test]
    fn test_install_options_no_dependencies() {
        let options = InstallOptions::new("test").no_dependencies();
        assert!(!options.with_dependencies);
    }
}
