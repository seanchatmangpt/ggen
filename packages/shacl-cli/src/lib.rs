use anyhow::{Context, Result};
use std::path::Path;

pub mod shapes;
pub mod constraints;
pub mod validation;
pub mod reports;

/// SHACL CLI library for validation, constraint enforcement, and reporting
///
/// This library provides functionality for:
/// - Shape management (create, list, show, compile)
/// - Constraint definition and enforcement
/// - Data validation against SHACL shapes
/// - Report generation and visualization

/// Initialize the SHACL engine
pub fn init() -> Result<()> {
    tracing::info!("Initializing SHACL CLI");
    Ok(())
}

/// Load RDF data from file
pub fn load_rdf_file(path: &Path) -> Result<String> {
    std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read RDF file: {:?}", path))
}

/// Save RDF data to file
pub fn save_rdf_file(path: &Path, content: &str) -> Result<()> {
    std::fs::write(path, content)
        .with_context(|| format!("Failed to write RDF file: {:?}", path))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init() {
        assert!(init().is_ok());
    }
}
