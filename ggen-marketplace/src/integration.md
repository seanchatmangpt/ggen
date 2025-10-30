# Integration Guide

This guide demonstrates how to integrate `ggen-marketplace` into your Rust projects, with specific examples for `ggen` and `clnrm`.

## Table of Contents

1. [Basic Integration](#basic-integration)
2. [Integration with Ggen CLI](#integration-with-ggen-cli)
3. [Integration with Clnrm](#integration-with-clnrm)
4. [Custom Implementations](#custom-implementations)
5. [Testing Integration](#testing-integration)

---

## Basic Integration

### Adding as a Dependency

```toml
# Cargo.toml
[dependencies]
ggen-marketplace = { version = "0.1", features = ["default"] }

# Optional features
# ggen-marketplace = { version = "0.1", features = ["s3-storage", "p2p", "graphql"] }
```

### Minimal Setup

```rust
use ggen_marketplace::prelude::*;
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Build marketplace with defaults
    let marketplace = MarketplaceBuilder::new()
        .with_local_registry("./data/registry.db")
        .with_file_storage("./data/packages")
        .with_tantivy_search("./data/search")
        .with_ed25519_crypto()
        .build()
        .await?;

    // Use the marketplace
    search_and_install(&marketplace).await?;

    Ok(())
}

async fn search_and_install(marketplace: &MarketplaceClient) -> Result<()> {
    // Search for packages
    let results = marketplace.search("web framework").await?;

    for result in results.iter().take(5) {
        println!("{} - {}", result.package.id.name, result.package.description);
    }

    // Install a package
    if let Some(package) = results.first() {
        let bytes = marketplace.download(&package.package.id).await?;
        println!("Downloaded {} bytes", bytes.len());
    }

    Ok(())
}
```

---

## Integration with Ggen CLI

### Project Structure

```
ggen/
├── Cargo.toml
├── src/
│   ├── main.rs
│   ├── commands/
│   │   ├── mod.rs
│   │   ├── market.rs        # Marketplace commands
│   │   ├── template.rs
│   │   └── lifecycle.rs
│   ├── marketplace/
│   │   ├── mod.rs
│   │   ├── client.rs        # Marketplace integration
│   │   └── config.rs        # Marketplace configuration
│   └── lib.rs
└── README.md
```

### Marketplace Configuration

```rust
// src/marketplace/config.rs
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceConfig {
    /// Local registry database path
    pub registry_path: PathBuf,

    /// Package storage directory
    pub packages_path: PathBuf,

    /// Search index directory
    pub search_path: PathBuf,

    /// Optional remote marketplace URLs
    pub remotes: Vec<String>,

    /// Enable peer-to-peer discovery
    #[cfg(feature = "p2p")]
    pub p2p_enabled: bool,
}

impl Default for MarketplaceConfig {
    fn default() -> Self {
        let data_dir = dirs::data_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("ggen");

        Self {
            registry_path: data_dir.join("registry.db"),
            packages_path: data_dir.join("packages"),
            search_path: data_dir.join("search"),
            remotes: vec![],
            #[cfg(feature = "p2p")]
            p2p_enabled: false,
        }
    }
}

impl MarketplaceConfig {
    /// Load configuration from file
    pub fn load(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: Self = toml::from_str(&content)?;
        Ok(config)
    }

    /// Save configuration to file
    pub fn save(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
}
```

### Marketplace Client Wrapper

```rust
// src/marketplace/client.rs
use ggen_marketplace::prelude::*;
use anyhow::{Context, Result};
use std::sync::Arc;

pub struct GgenMarketplace {
    client: MarketplaceClient,
    config: MarketplaceConfig,
}

impl GgenMarketplace {
    /// Initialize marketplace from configuration
    pub async fn new(config: MarketplaceConfig) -> Result<Self> {
        // Create directories if they don't exist
        std::fs::create_dir_all(&config.registry_path.parent().unwrap())?;
        std::fs::create_dir_all(&config.packages_path)?;
        std::fs::create_dir_all(&config.search_path)?;

        // Build marketplace client
        let client = MarketplaceBuilder::new()
            .registry(LocalRegistry::new(&config.registry_path).await?)
            .storage(FilePackageStore::new(&config.packages_path).await?)
            .search(TantivySearchEngine::new(&config.search_path).await?)
            .crypto(Ed25519CryptoVerifier::new())
            .metrics(PrometheusMetricsCollector::new())
            .build()
            .await?;

        Ok(Self { client, config })
    }

    /// Search for packages with query
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
        let search_query = SearchQuery {
            text: query.to_string(),
            categories: vec![],
            tags: vec![],
            min_downloads: None,
            limit: 50,
            offset: 0,
        };

        self.client
            .search(&search_query)
            .await
            .context("Failed to search marketplace")
    }

    /// Install a package by name and version
    pub async fn install(&self, name: &str, version: Option<&str>) -> Result<PathBuf> {
        // Resolve version if not specified
        let version = if let Some(v) = version {
            semver::Version::parse(v)?
        } else {
            // Get latest version
            let versions = self.client.list_versions(name, None).await?;
            versions.into_iter()
                .max()
                .context("No versions available for package")?
        };

        let package_id = PackageId {
            name: name.to_string(),
            version,
            namespace: None,
        };

        // Download package
        let bytes = self.client
            .download(&package_id)
            .await
            .context("Failed to download package")?;

        // Extract to packages directory
        let install_path = self.config.packages_path
            .join(&package_id.name)
            .join(package_id.version.to_string());

        std::fs::create_dir_all(&install_path)?;

        // Extract tarball (assuming tar.gz format)
        extract_tarball(&bytes, &install_path)?;

        Ok(install_path)
    }

    /// Publish a package
    pub async fn publish(&self, package_path: &Path) -> Result<PackageId> {
        // Build package metadata from Cargo.toml or package.json
        let metadata = build_package_metadata(package_path)?;

        // Create tarball
        let tarball = create_tarball(package_path)?;

        // Publish to marketplace
        let package_id = self.client
            .publish(&metadata, tarball)
            .await
            .context("Failed to publish package")?;

        Ok(package_id)
    }

    /// List installed packages
    pub async fn list_installed(&self) -> Result<Vec<InstalledPackage>> {
        let mut installed = Vec::new();

        for entry in std::fs::read_dir(&self.config.packages_path)? {
            let entry = entry?;
            let package_name = entry.file_name().to_string_lossy().to_string();

            // List versions
            for version_entry in std::fs::read_dir(entry.path())? {
                let version_entry = version_entry?;
                let version_str = version_entry.file_name().to_string_lossy().to_string();

                if let Ok(version) = semver::Version::parse(&version_str) {
                    installed.push(InstalledPackage {
                        name: package_name.clone(),
                        version,
                        path: version_entry.path(),
                    });
                }
            }
        }

        Ok(installed)
    }
}

#[derive(Debug)]
pub struct InstalledPackage {
    pub name: String,
    pub version: semver::Version,
    pub path: PathBuf,
}

fn build_package_metadata(path: &Path) -> Result<PackageMetadata> {
    // Implementation: Parse Cargo.toml, package.json, etc.
    todo!()
}

fn create_tarball(path: &Path) -> Result<Bytes> {
    // Implementation: Create tar.gz archive
    todo!()
}

fn extract_tarball(bytes: &[u8], dest: &Path) -> Result<()> {
    // Implementation: Extract tar.gz archive
    todo!()
}
```

### CLI Commands

```rust
// src/commands/market.rs
use clap::{Args, Subcommand};
use anyhow::Result;

#[derive(Debug, Args)]
pub struct MarketCommand {
    #[command(subcommand)]
    pub command: MarketSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum MarketSubcommand {
    /// Search for packages
    Search {
        /// Search query
        query: String,

        /// Filter by category
        #[arg(short, long)]
        category: Option<String>,

        /// Maximum results
        #[arg(short, long, default_value = "20")]
        limit: usize,
    },

    /// Install a package
    Add {
        /// Package name
        package: String,

        /// Specific version (latest if not specified)
        #[arg(short, long)]
        version: Option<String>,
    },

    /// List installed packages
    List {
        /// Show only outdated packages
        #[arg(short, long)]
        outdated: bool,
    },

    /// Publish a package
    Publish {
        /// Path to package directory
        #[arg(default_value = ".")]
        path: PathBuf,
    },

    /// Update installed packages
    Update {
        /// Specific package to update
        package: Option<String>,
    },

    /// Remove a package
    Remove {
        /// Package name
        package: String,

        /// Specific version (all if not specified)
        #[arg(short, long)]
        version: Option<String>,
    },

    /// Show package information
    Info {
        /// Package name
        package: String,
    },
}

pub async fn execute(cmd: MarketCommand, marketplace: &GgenMarketplace) -> Result<()> {
    match cmd.command {
        MarketSubcommand::Search { query, category, limit } => {
            search_packages(marketplace, &query, category.as_deref(), limit).await?;
        }
        MarketSubcommand::Add { package, version } => {
            install_package(marketplace, &package, version.as_deref()).await?;
        }
        MarketSubcommand::List { outdated } => {
            list_packages(marketplace, outdated).await?;
        }
        MarketSubcommand::Publish { path } => {
            publish_package(marketplace, &path).await?;
        }
        MarketSubcommand::Update { package } => {
            update_packages(marketplace, package.as_deref()).await?;
        }
        MarketSubcommand::Remove { package, version } => {
            remove_package(marketplace, &package, version.as_deref()).await?;
        }
        MarketSubcommand::Info { package } => {
            show_package_info(marketplace, &package).await?;
        }
    }

    Ok(())
}

async fn search_packages(
    marketplace: &GgenMarketplace,
    query: &str,
    category: Option<&str>,
    limit: usize,
) -> Result<()> {
    println!("Searching for '{}'...", query);

    let results = marketplace.search(query).await?;

    if results.is_empty() {
        println!("No packages found.");
        return Ok(());
    }

    println!("\nFound {} packages:\n", results.len());

    for result in results.iter().take(limit) {
        let pkg = &result.package;
        println!("  {} v{}", pkg.id.name, pkg.id.version);
        println!("    {}", pkg.description);
        println!("    Downloads: {}", result.score);
        println!();
    }

    Ok(())
}

async fn install_package(
    marketplace: &GgenMarketplace,
    name: &str,
    version: Option<&str>,
) -> Result<()> {
    let version_str = version.unwrap_or("latest");
    println!("Installing {} {}...", name, version_str);

    let install_path = marketplace.install(name, version).await?;

    println!("✓ Successfully installed to: {}", install_path.display());

    Ok(())
}

async fn list_packages(marketplace: &GgenMarketplace, outdated: bool) -> Result<()> {
    let installed = marketplace.list_installed().await?;

    if installed.is_empty() {
        println!("No packages installed.");
        return Ok(());
    }

    println!("Installed packages:\n");

    for pkg in installed {
        println!("  {} v{}", pkg.name, pkg.version);
        println!("    Path: {}", pkg.path.display());

        if outdated {
            // Check for newer version
            // ... implementation
        }
    }

    Ok(())
}

async fn publish_package(marketplace: &GgenMarketplace, path: &Path) -> Result<()> {
    println!("Publishing package from {}...", path.display());

    let package_id = marketplace.publish(path).await?;

    println!("✓ Successfully published {} v{}", package_id.name, package_id.version);

    Ok(())
}

async fn update_packages(marketplace: &GgenMarketplace, package: Option<&str>) -> Result<()> {
    // Implementation: Update one or all packages
    todo!()
}

async fn remove_package(
    marketplace: &GgenMarketplace,
    name: &str,
    version: Option<&str>,
) -> Result<()> {
    // Implementation: Remove package
    todo!()
}

async fn show_package_info(marketplace: &GgenMarketplace, name: &str) -> Result<()> {
    // Implementation: Display detailed package information
    todo!()
}
```

### Main CLI Integration

```rust
// src/main.rs
use clap::Parser;
use anyhow::Result;

mod commands;
mod marketplace;

use commands::market::{MarketCommand, execute as execute_market};
use marketplace::{GgenMarketplace, MarketplaceConfig};

#[derive(Parser)]
#[command(name = "ggen")]
#[command(about = "Ggen CLI with marketplace integration")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Marketplace commands
    Market(MarketCommand),

    // Other existing commands
    Template(TemplateCommand),
    Lifecycle(LifecycleCommand),
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    // Load marketplace configuration
    let config_path = dirs::config_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("ggen/marketplace.toml");

    let config = if config_path.exists() {
        MarketplaceConfig::load(&config_path)?
    } else {
        let config = MarketplaceConfig::default();
        std::fs::create_dir_all(config_path.parent().unwrap())?;
        config.save(&config_path)?;
        config
    };

    // Initialize marketplace
    let marketplace = GgenMarketplace::new(config).await?;

    // Parse CLI arguments
    let cli = Cli::parse();

    // Execute command
    match cli.command {
        Commands::Market(cmd) => {
            execute_market(cmd, &marketplace).await?;
        }
        Commands::Template(cmd) => {
            // Existing template command implementation
        }
        Commands::Lifecycle(cmd) => {
            // Existing lifecycle command implementation
        }
    }

    Ok(())
}
```

---

## Integration with Clnrm

Clnrm can use the marketplace as a source of clean room test environments and validation tools.

### Clnrm Marketplace Integration

```rust
// clnrm/src/marketplace.rs
use ggen_marketplace::prelude::*;
use anyhow::Result;

pub struct ClnrmMarketplace {
    client: MarketplaceClient,
}

impl ClnrmMarketplace {
    pub async fn new() -> Result<Self> {
        let client = MarketplaceBuilder::new()
            .with_local_registry("./data/clnrm-registry.db")
            .with_file_storage("./data/clnrm-packages")
            .with_tantivy_search("./data/clnrm-search")
            .with_ed25519_crypto()
            .build()
            .await?;

        Ok(Self { client })
    }

    /// Search for validation tools
    pub async fn search_validators(&self, query: &str) -> Result<Vec<SearchResult>> {
        let search_query = SearchQuery {
            text: query.to_string(),
            categories: vec!["validation".to_string(), "testing".to_string()],
            tags: vec!["cleanroom".to_string()],
            min_downloads: None,
            limit: 20,
            offset: 0,
        };

        self.client.search(&search_query).await
    }

    /// Install a validation tool
    pub async fn install_validator(&self, name: &str) -> Result<PathBuf> {
        // Get latest version
        let versions = self.client.list_versions(name, None).await?;
        let latest = versions.into_iter().max().unwrap();

        let package_id = PackageId {
            name: name.to_string(),
            version: latest,
            namespace: Some("clnrm-validators".to_string()),
        };

        // Download and install
        let bytes = self.client.download(&package_id).await?;
        let install_path = PathBuf::from("./validators")
            .join(&package_id.name)
            .join(package_id.version.to_string());

        std::fs::create_dir_all(&install_path)?;
        extract_validator(&bytes, &install_path)?;

        Ok(install_path)
    }

    /// Publish a new validator
    pub async fn publish_validator(
        &self,
        name: &str,
        version: semver::Version,
        binary_path: &Path,
        metadata: ValidatorMetadata,
    ) -> Result<PackageId> {
        // Build package metadata
        let package_metadata = PackageMetadata {
            id: PackageId {
                name: name.to_string(),
                version,
                namespace: Some("clnrm-validators".to_string()),
            },
            description: metadata.description,
            authors: metadata.authors,
            license: "MIT".to_string(),
            repository: metadata.repository,
            homepage: None,
            keywords: vec!["validation".to_string(), "cleanroom".to_string()],
            categories: vec!["validation".to_string()],
            dependencies: HashMap::new(),
            checksums: Checksums::default(),
            signature: Signature::default(),
            published_at: chrono::Utc::now(),
        };

        // Create package archive
        let tarball = create_validator_tarball(binary_path)?;

        // Publish
        self.client.publish(&package_metadata, tarball).await
    }
}

#[derive(Debug)]
pub struct ValidatorMetadata {
    pub description: String,
    pub authors: Vec<String>,
    pub repository: Option<Url>,
}

fn extract_validator(bytes: &[u8], dest: &Path) -> Result<()> {
    // Implementation
    todo!()
}

fn create_validator_tarball(path: &Path) -> Result<Bytes> {
    // Implementation
    todo!()
}
```

### Clnrm CLI Integration

```rust
// clnrm/src/commands/validator.rs
use clap::{Args, Subcommand};

#[derive(Debug, Args)]
pub struct ValidatorCommand {
    #[command(subcommand)]
    pub command: ValidatorSubcommand,
}

#[derive(Debug, Subcommand)]
pub enum ValidatorSubcommand {
    /// Search for validators
    Search {
        query: String,
    },

    /// Install a validator
    Install {
        name: String,
    },

    /// List installed validators
    List,

    /// Publish a validator
    Publish {
        name: String,
        version: String,
        binary: PathBuf,
    },
}

pub async fn execute(cmd: ValidatorCommand, marketplace: &ClnrmMarketplace) -> Result<()> {
    match cmd.command {
        ValidatorSubcommand::Search { query } => {
            let results = marketplace.search_validators(&query).await?;
            for result in results {
                println!("{} - {}", result.package.id.name, result.package.description);
            }
        }
        ValidatorSubcommand::Install { name } => {
            let path = marketplace.install_validator(&name).await?;
            println!("Installed validator to: {}", path.display());
        }
        ValidatorSubcommand::List => {
            // List installed validators
        }
        ValidatorSubcommand::Publish { name, version, binary } => {
            let version = semver::Version::parse(&version)?;
            let metadata = ValidatorMetadata {
                description: "Custom validator".to_string(),
                authors: vec!["author@example.com".to_string()],
                repository: None,
            };

            let id = marketplace.publish_validator(&name, version, &binary, metadata).await?;
            println!("Published validator: {:?}", id);
        }
    }

    Ok(())
}
```

---

## Custom Implementations

### Custom Storage Backend Example

```rust
use ggen_marketplace::prelude::*;
use bytes::Bytes;

/// Custom storage using MongoDB GridFS
pub struct GridFSStorage {
    client: mongodb::Client,
    database: String,
}

#[async_trait]
impl PackageStore for GridFSStorage {
    async fn store(&self, package_bytes: Bytes) -> Result<ContentHash> {
        let db = self.client.database(&self.database);
        let bucket = db.gridfs_bucket(None);

        // Upload to GridFS
        let mut upload_stream = bucket.open_upload_stream("package", None);
        upload_stream.write_all(&package_bytes).await?;
        let file_id = upload_stream.close().await?;

        // Compute hash
        let hash = ContentHash::sha256(&package_bytes);

        // Store mapping from hash to file_id
        // ... implementation

        Ok(hash)
    }

    async fn get(&self, hash: &ContentHash) -> Result<Bytes> {
        // Retrieve file_id from hash mapping
        // ... implementation

        let db = self.client.database(&self.database);
        let bucket = db.gridfs_bucket(None);

        // Download from GridFS
        let mut download_stream = bucket.open_download_stream(file_id).await?;
        let mut buffer = Vec::new();
        download_stream.read_to_end(&mut buffer).await?;

        Ok(Bytes::from(buffer))
    }

    // Implement other required methods...
}
```

### Custom Search Backend Example

```rust
/// Custom search using Elasticsearch
pub struct ElasticsearchEngine {
    client: elasticsearch::Elasticsearch,
    index_name: String,
}

#[async_trait]
impl SearchEngine for ElasticsearchEngine {
    async fn index(&self, metadata: &PackageMetadata) -> Result<()> {
        let body = serde_json::to_value(metadata)?;

        self.client
            .index(elasticsearch::IndexParts::IndexId(
                &self.index_name,
                &metadata.id.to_string(),
            ))
            .body(body)
            .send()
            .await?;

        Ok(())
    }

    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchResult>> {
        let search_body = json!({
            "query": {
                "multi_match": {
                    "query": query.text,
                    "fields": ["name^3", "description^2", "keywords"]
                }
            },
            "size": query.limit,
            "from": query.offset,
        });

        let response = self.client
            .search(elasticsearch::SearchParts::Index(&[&self.index_name]))
            .body(search_body)
            .send()
            .await?;

        // Parse response and convert to SearchResult
        // ... implementation

        todo!()
    }

    // Implement other required methods...
}
```

---

## Testing Integration

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_marketplace_integration() {
        let marketplace = GgenMarketplace::new(MarketplaceConfig::default())
            .await
            .unwrap();

        // Test search
        let results = marketplace.search("test").await.unwrap();
        assert!(results.is_empty() || !results.is_empty());
    }

    #[tokio::test]
    async fn test_install_package() {
        let marketplace = create_test_marketplace().await;

        // Publish a test package first
        let test_package = create_test_package();
        let package_id = marketplace.client.publish(&test_package, test_bytes).await.unwrap();

        // Install the package
        let install_path = marketplace.install(&package_id.name, Some(&package_id.version.to_string()))
            .await
            .unwrap();

        assert!(install_path.exists());
    }
}

async fn create_test_marketplace() -> GgenMarketplace {
    let config = MarketplaceConfig {
        registry_path: PathBuf::from(":memory:"),
        packages_path: PathBuf::from("/tmp/test-packages"),
        search_path: PathBuf::from("/tmp/test-search"),
        remotes: vec![],
    };

    GgenMarketplace::new(config).await.unwrap()
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_end_to_end_workflow() {
    let marketplace = create_test_marketplace().await;

    // Publish
    let package_path = create_test_package_dir();
    let package_id = marketplace.publish(&package_path).await.unwrap();

    // Search
    let results = marketplace.search(&package_id.name).await.unwrap();
    assert!(!results.is_empty());

    // Install
    let install_path = marketplace.install(&package_id.name, None).await.unwrap();
    assert!(install_path.exists());

    // List
    let installed = marketplace.list_installed().await.unwrap();
    assert!(installed.iter().any(|p| p.name == package_id.name));
}
```

---

## Performance Optimization Tips

1. **Connection Pooling**: Reuse database connections
2. **Caching**: Cache frequently accessed metadata
3. **Streaming**: Use streaming for large package downloads
4. **Parallel Operations**: Download dependencies in parallel
5. **Incremental Updates**: Only update changed search indices

## Security Best Practices

1. **Signature Verification**: Always verify package signatures
2. **Checksum Validation**: Validate package integrity
3. **Trust Management**: Maintain trusted key store
4. **Rate Limiting**: Prevent abuse of marketplace
5. **Audit Logging**: Log security-relevant operations

## Troubleshooting

### Common Issues

1. **Database Lock Errors**: Use connection pooling
2. **Search Index Corruption**: Rebuild index using `optimize()`
3. **Storage Quota**: Implement cleanup policies
4. **Network Timeouts**: Configure appropriate timeouts
5. **Permission Errors**: Check filesystem permissions

---

For more details, see:
- [Architecture](architecture.md) - System architecture
- [Traits System](traits.md) - Trait documentation
- [Performance](performance.md) - Performance tuning
