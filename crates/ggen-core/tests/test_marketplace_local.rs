use chicago_tdd_tools::prelude::*;
use ggen_core::RegistryClient;
use std::env;
use std::path::PathBuf;

async_test_with_timeout!(test_local_registry_index, 30, async {
    // Arrange
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!(
        "file://{}/",
        registry_path.canonicalize().unwrap().display()
    );
    env::set_var("GGEN_REGISTRY_URL", &file_url);

    // Act
    let client = RegistryClient::new().unwrap();
    let index = client.fetch_index().await.unwrap();

    // Assert
    assert!(!index.packs.is_empty());
    assert!(index.packs.contains_key("io.ggen.rust.cli-subcommand"));
});

async_test_with_timeout!(test_resolve_from_local_registry, 30, async {
    // Arrange
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!(
        "file://{}/",
        registry_path.canonicalize().unwrap().display()
    );
    env::set_var("GGEN_REGISTRY_URL", &file_url);
    let client = RegistryClient::new().unwrap();

    // Act
    let resolved = client
        .resolve("io.ggen.rust.cli-subcommand", None)
        .await
        .unwrap();

    // Assert
    assert_eq!(resolved.id, "io.ggen.rust.cli-subcommand");
    assert_eq!(resolved.version, "1.2.0");
});

async_test_with_timeout!(test_search_from_local_registry, 30, async {
    // Arrange
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!(
        "file://{}/",
        registry_path.canonicalize().unwrap().display()
    );
    env::set_var("GGEN_REGISTRY_URL", &file_url);
    let client = RegistryClient::new().unwrap();

    // Act
    let results = client.search("rust").await.unwrap();

    // Assert
    assert!(!results.is_empty());
    assert!(results
        .iter()
        .any(|r| r.id == "io.ggen.rust.cli-subcommand"));
});

async_test_with_timeout!(test_advanced_search_from_local_registry, 30, async {
    // Arrange
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let registry_path = PathBuf::from(manifest_dir).join("../registry");
    let file_url = format!(
        "file://{}/",
        registry_path.canonicalize().unwrap().display()
    );
    env::set_var("GGEN_REGISTRY_URL", &file_url);
    let client = RegistryClient::new().unwrap();
    let search_params = ggen_core::registry::SearchParams {
        query: "rust",
        category: Some("rust"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    // Act
    let results = client.advanced_search(&search_params).await.unwrap();

    // Assert
    assert!(!results.is_empty());
    assert!(results
        .iter()
        .any(|r| r.id == "io.ggen.rust.cli-subcommand"));
});
