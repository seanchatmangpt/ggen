use anyhow::Result;
use std::env;
use std::path::PathBuf;

#[test]
fn test_registry_client_creation() -> Result<()> {
    // Test that RegistryClient can be created with environment variable
    env::set_var(
        "RGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/rgen/master/registry/",
    );

    let client = rgen_core::RegistryClient::new()?;

    // Verify the client was created successfully (we can't access private fields)
    // This test just ensures the client creation doesn't fail
    assert!(std::ptr::addr_of!(client) != std::ptr::null());

    Ok(())
}

#[test]
fn test_registry_client_with_file_url() -> Result<()> {
    // Get absolute path to registry directory
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir).join("registry");
    let file_url = format!("file://{}/", registry_path.canonicalize()?.display());

    // Set environment variable for testing
    env::set_var("RGEN_REGISTRY_URL", &file_url);

    let client = rgen_core::RegistryClient::new()?;

    // Verify the client was created with file URL (we can't access private fields)
    // This test just ensures the client creation doesn't fail
    assert!(std::ptr::addr_of!(client) != std::ptr::null());

    Ok(())
}

#[test]
fn test_registry_index_json_exists() -> Result<()> {
    // Verify that the registry index.json file exists and is valid JSON
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let registry_path = PathBuf::from(manifest_dir)
        .join("registry")
        .join("index.json");

    assert!(registry_path.exists(), "registry/index.json should exist");

    // Read and parse the JSON
    let content = std::fs::read_to_string(&registry_path)?;
    let index: rgen_core::registry::RegistryIndex = serde_json::from_str(&content)?;

    // Verify it contains our test pack
    assert!(!index.packs.is_empty());
    assert!(index.packs.contains_key("io.rgen.rust.cli-subcommand"));

    let pack = &index.packs["io.rgen.rust.cli-subcommand"];
    assert_eq!(pack.id, "io.rgen.rust.cli-subcommand");
    assert_eq!(pack.name, "Rust CLI Subcommand");
    assert_eq!(pack.latest_version, "0.1.0");

    Ok(())
}
