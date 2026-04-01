//! Marketplace integration for pack installation
//!
//! NOTE: This module is currently stubbed out because ggen-marketplace
//! was migrated to a separate project. The marketplace functionality
//! should be accessed through ggen-cli instead, which has ggen-marketplace
//! as a dependency.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

// Stub types to maintain API compatibility
// These should be removed or migrated to use ggen-cli's marketplace integration

/// Summary of a package for list display
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageSummary {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub downloads: u64,
    pub quality_score: Option<u32>,
}

/// Detailed package information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageDetail {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub authors: Vec<String>,
    pub license: String,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub keywords: Vec<String>,
    pub categories: Vec<String>,
    pub downloads: u64,
    pub quality_score: Option<u32>,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub dependencies: Vec<DependencySummary>,
}

/// Summary of a dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencySummary {
    pub id: String,
    pub version_req: String,
    pub optional: bool,
}

/// Dependency graph for a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyGraph {
    pub package_id: String,
    pub version: String,
    pub dependencies: Vec<DependencyNode>,
}

/// A node in the dependency graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyNode {
    pub id: String,
    pub version_req: String,
    pub optional: bool,
    pub dependencies: Vec<String>, // Child dependency IDs
}

/// Input for package installation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallInput {
    pub package_id: String,
    pub target_dir: String,
    pub dry_run: bool,
    pub force: bool,
    pub skip_dependencies: bool,
}

/// Result of package installation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallResult {
    pub success: bool,
    pub message: String,
    pub packages_installed: Vec<String>,
}

/// List all available packages from the marketplace
///
/// NOTE: This is a stub implementation. The real implementation should
/// use ggen-cli's marketplace integration.
pub fn list_all() -> Result<Vec<PackageSummary>> {
    Err(ggen_utils::Error::new(
        "Marketplace integration moved to ggen-cli. \
         Please use ggen-cli's marketplace commands instead.",
    ))
}

/// Get detailed information about a specific package
///
/// NOTE: This is a stub implementation. The real implementation should
/// use ggen-cli's marketplace integration.
pub fn get_package(_pack_id: &str) -> Result<PackageDetail> {
    Err(ggen_utils::Error::new(
        "Marketplace integration moved to ggen-cli. \
         Please use ggen-cli's marketplace commands instead.",
    ))
}

/// Resolve dependencies for a package
///
/// NOTE: This is a stub implementation. The real implementation should
/// use ggen-cli's marketplace integration.
pub fn resolve_dependencies(_pack_id: &str, _version: Option<&str>) -> Result<DependencyGraph> {
    Err(ggen_utils::Error::new(
        "Marketplace integration moved to ggen-cli. \
         Please use ggen-cli's marketplace commands instead.",
    ))
}

/// Execute package installation
///
/// NOTE: This is a stub implementation. The real implementation should
/// use ggen-cli's marketplace integration.
pub async fn execute_install(_input: InstallInput) -> Result<InstallResult> {
    Err(ggen_utils::Error::new(
        "Marketplace integration moved to ggen-cli. \
         Please use 'ggen packs install' instead.",
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stub_returns_error() {
        // Verify stub implementations return appropriate errors
        let result = list_all();
        assert!(result.is_err());

        let err = result.unwrap_err();
        let msg = format!("{}", err);
        assert!(msg.contains("moved to ggen-cli"));
    }
}
