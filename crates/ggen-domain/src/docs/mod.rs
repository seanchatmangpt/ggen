//! Living documentation domain operations

use ggen_living_docs::{LivingDocSystem, Config};
use ggen_utils::errors::Result;
use std::path::Path;
use tracing::{info, instrument};

pub mod extract;
pub mod narrate;
pub mod serve;
pub mod sync;
pub mod query;
pub mod validate;
pub mod hooks;

/// Extract code ontology from source directory
#[instrument]
pub async fn extract_ontology(source_dir: impl AsRef<Path>) -> Result<()> {
    info!("Extracting code ontology from: {:?}", source_dir.as_ref());

    let config = Config::default();
    let mut system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    system.extract_ontology(source_dir).await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    info!("Ontology extraction complete");
    Ok(())
}

/// Generate documentation narratives
#[instrument]
pub async fn generate_narratives() -> Result<Vec<String>> {
    info!("Generating documentation narratives");

    let config = Config::default();
    let system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    let narratives = system.generate_narratives().await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    info!("Generated {} narratives", narratives.len());
    Ok(narratives)
}

/// Start interactive documentation server
#[instrument]
pub async fn serve_docs(addr: &str) -> Result<()> {
    info!("Starting documentation server at {}", addr);

    let config = Config::default();
    let system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    system.serve(addr).await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    Ok(())
}

/// Sync documentation from natural language
#[instrument]
pub async fn sync_from_nl(input: &str) -> Result<()> {
    info!("Syncing from natural language");

    let config = Config::default();
    let mut system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    system.sync_from_natural_language(input).await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    Ok(())
}

/// Query documentation using natural language
#[instrument]
pub async fn query_docs(query: &str) -> Result<String> {
    info!("Querying documentation: {}", query);

    let config = Config::default();
    let system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    let response = system.query(query).await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    Ok(response)
}

/// Validate documentation completeness
#[instrument]
pub async fn validate_docs() -> Result<ggen_living_docs::ValidationReport> {
    info!("Validating documentation");

    let config = Config::default();
    let system = LivingDocSystem::new(config)
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    let report = system.validate().await
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    Ok(report)
}

/// Install git hooks for automatic documentation evolution
#[instrument]
pub fn install_hooks(repo_path: impl AsRef<Path>) -> Result<()> {
    info!("Installing git hooks");

    let hooks_manager = ggen_living_docs::hooks::GitHooksManager::new(repo_path);
    hooks_manager.install_hooks()
        .map_err(|e| ggen_utils::errors::Error::Other(e.to_string()))?;

    info!("Git hooks installed successfully");
    Ok(())
}
