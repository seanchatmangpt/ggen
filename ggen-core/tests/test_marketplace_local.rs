use anyhow::Result;
use ggen_core::RegistryClient;
use std::env;
use std::path::PathBuf;

#[tokio::test]
async fn test_local_registry_index() -> Result<()> {
    // Get absolute path to registry directory
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    // Set environment variable for testing
    env::set_var("GGEN_REGISTRY_URL", &file_url);

    // Test fetching index
    let client = RegistryClient::new()?;
    let index = client.fetch_index().await?;

    assert!(!index.packs.is_empty());
    assert!(index.packs.contains_key("io.ggen.rust.cli-subcommand"));

    Ok(())
}

#[tokio::test]
async fn test_resolve_from_local_registry() -> Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    env::set_var("GGEN_REGISTRY_URL", &file_url);

    let client = RegistryClient::new()?;
    let resolved = client.resolve("io.ggen.rust.cli-subcommand", None).await?;

    assert_eq!(resolved.id, "io.ggen.rust.cli-subcommand");
    assert_eq!(resolved.version, "1.2.0");

    Ok(())
}

#[tokio::test]
async fn test_search_from_local_registry() -> Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    env::set_var("GGEN_REGISTRY_URL", &file_url);

    let client = RegistryClient::new()?;
    let results = client.search("rust").await?;

    assert!(!results.is_empty());
    assert!(results
        .iter()
        .any(|r| r.id == "io.ggen.rust.cli-subcommand"));

    Ok(())
}

#[tokio::test]
async fn test_advanced_search_from_local_registry() -> Result<()> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    env::set_var("GGEN_REGISTRY_URL", &file_url);

    let client = RegistryClient::new()?;
    let search_params = ggen_core::registry::SearchParams {
        query: "rust",
        category: Some("rust"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    let results = client.advanced_search(&search_params).await?;

    assert!(!results.is_empty());
    assert!(results
        .iter()
        .any(|r| r.id == "io.ggen.rust.cli-subcommand"));

    Ok(())
}
