//! Domain logic for marketplace package installation
//!
//! This module contains the core business logic for installing packages,
//! separated from CLI concerns for better testability and reusability.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

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
