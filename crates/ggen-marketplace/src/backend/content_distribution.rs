//! Content Distribution HTTP Server (v2.4.0 Phase 2)
//!
//! This module provides an HTTP server for distributing package content over P2P networks.
//! It implements the content distribution protocol deferred from Phase 1.

use crate::error::{MarketplaceError, Result};
use crate::models::{Package, PackageId};
use axum::{extract::Path, http::StatusCode, response::Json, routing::get, Router};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_http::cors::CorsLayer;
use tracing::{info, instrument};

/// Content distribution server configuration
#[derive(Debug, Clone)]
pub struct ContentDistributionConfig {
    /// HTTP listen address
    pub listen_address: String,
    /// Port to listen on
    pub port: u16,
    /// Maximum package size in bytes
    pub max_package_size: u64,
    /// Enable CORS
    pub enable_cors: bool,
}

impl Default for ContentDistributionConfig {
    fn default() -> Self {
        Self {
            listen_address: "0.0.0.0".to_string(),
            port: 8080,
            max_package_size: 100 * 1024 * 1024, // 100 MB
            enable_cors: true,
        }
    }
}

/// Package download metadata
#[derive(Debug, Serialize, Deserialize)]
pub struct DownloadInfo {
    /// Package ID
    pub package_id: String,
    /// Package version
    pub version: String,
    /// Download URL
    pub download_url: String,
    /// SHA256 checksum
    pub sha256: String,
    /// Package size in bytes
    pub size: u64,
    /// Content type
    pub content_type: String,
}

/// Package server response
#[derive(Debug, Serialize)]
pub struct PackageServerInfo {
    /// Server version
    pub version: String,
    /// Number of packages available
    pub package_count: usize,
    /// Server capabilities
    pub capabilities: Vec<String>,
}

/// Content distribution server state
#[derive(Clone)]
pub struct ContentDistributionServer {
    /// Available packages indexed by ID
    packages: Arc<RwLock<HashMap<PackageId, Package>>>,
    /// Package content storage (in-memory for now, can be enhanced with filesystem)
    content: Arc<RwLock<HashMap<PackageId, Vec<u8>>>>,
    /// Configuration
    config: ContentDistributionConfig,
}

impl ContentDistributionServer {
    /// Create a new content distribution server
    pub fn new(config: ContentDistributionConfig) -> Self {
        Self {
            packages: Arc::new(RwLock::new(HashMap::new())),
            content: Arc::new(RwLock::new(HashMap::new())),
            config,
        }
    }

    /// Add a package to the server
    #[instrument(skip(self, package, content))]
    pub async fn add_package(&self, package: Package, content: Vec<u8>) -> Result<()> {
        // Validate package size
        if content.len() as u64 > self.config.max_package_size {
            return Err(MarketplaceError::validation_error(format!(
                "Package size {} exceeds maximum {}",
                content.len(),
                self.config.max_package_size
            )));
        }

        let package_id = package.id.clone();

        // Store package metadata
        self.packages
            .write()
            .await
            .insert(package_id.clone(), package);

        // Store package content
        self.content.write().await.insert(package_id, content);

        info!(
            "Added package to content distribution server: {}",
            package_id
        );

        Ok(())
    }

    /// Remove a package from the server
    pub async fn remove_package(&self, package_id: &PackageId) {
        self.packages.write().await.remove(package_id);
        self.content.write().await.remove(package_id);
        info!(
            "Removed package from content distribution server: {}",
            package_id
        );
    }

    /// Get package count
    pub async fn package_count(&self) -> usize {
        self.packages.read().await.len()
    }

    /// Create HTTP router for the server
    pub fn create_router(&self) -> Router {
        let server = self.clone();

        let mut router = Router::new()
            .route("/", get(root_handler))
            .route("/packages", get(list_packages_handler))
            .route("/packages/:id", get(get_package_handler))
            .route("/packages/:id/download", get(download_package_handler))
            .route("/packages/:id/info", get(get_package_info_handler))
            .with_state(server);

        // Add CORS if enabled
        if self.config.enable_cors {
            router = router.layer(CorsLayer::permissive());
        }

        router
    }

    /// Start the HTTP server
    #[instrument(skip(self))]
    pub async fn start(&self) -> Result<()> {
        let app = self.create_router();
        let addr = format!("{}:{}", self.config.listen_address, self.config.port)
            .parse()
            .map_err(|e| MarketplaceError::network_error(format!("Invalid address: {}", e)))?;

        info!("Starting content distribution server on {}", addr);

        let listener = tokio::net::TcpListener::bind(&addr).await.map_err(|e| {
            MarketplaceError::network_error(format!("Failed to bind to {}: {}", addr, e))
        })?;

        axum::serve(listener, app)
            .await
            .map_err(|e| MarketplaceError::network_error(format!("Server error: {}", e)))?;

        Ok(())
    }
}

/// Root handler - server information
async fn root_handler(
    axum::extract::State(server): axum::extract::State<ContentDistributionServer>,
) -> Json<PackageServerInfo> {
    let package_count = server.package_count().await;

    Json(PackageServerInfo {
        version: "2.4.0".to_string(),
        package_count,
        capabilities: vec![
            "download".to_string(),
            "info".to_string(),
            "list".to_string(),
        ],
    })
}

/// List all available packages
async fn list_packages_handler(
    axum::extract::State(server): axum::extract::State<ContentDistributionServer>,
) -> Json<Vec<String>> {
    let packages = server.packages.read().await;
    let package_ids: Vec<String> = packages.keys().map(|id| id.to_string()).collect();
    Json(package_ids)
}

/// Get package metadata
#[instrument(skip(server))]
async fn get_package_handler(
    axum::extract::State(server): axum::extract::State<ContentDistributionServer>,
    Path(id): Path<String>,
) -> Result<Json<Package>, StatusCode> {
    // Parse PackageId from string (format: "namespace/name")
    let package_id = parse_package_id(&id).ok_or(StatusCode::NOT_FOUND)?;
    let packages = server.packages.read().await;

    if let Some(package) = packages.get(&package_id) {
        Ok(Json(package.clone()))
    } else {
        Err(StatusCode::NOT_FOUND)
    }
}

/// Parse PackageId from string format "namespace/name"
fn parse_package_id(s: &str) -> Option<PackageId> {
    let parts: Vec<&str> = s.split('/').collect();
    if parts.len() == 2 {
        Some(PackageId::new(parts[0], parts[1]))
    } else {
        None
    }
}

/// Get package download information
#[instrument(skip(server))]
async fn get_package_info_handler(
    axum::extract::State(server): axum::extract::State<ContentDistributionServer>,
    Path(id): Path<String>,
) -> Result<Json<DownloadInfo>, StatusCode> {
    let package_id = parse_package_id(&id).ok_or(StatusCode::NOT_FOUND)?;

    let (package, content) = {
        let packages = server.packages.read().await;
        let content_store = server.content.read().await;

        let package = packages.get(&package_id).ok_or(StatusCode::NOT_FOUND)?;
        let content = content_store
            .get(&package_id)
            .ok_or(StatusCode::NOT_FOUND)?;

        (package.clone(), content.clone())
    };

    // Calculate SHA256 checksum
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(&content);
    let sha256 = hex::encode(hasher.finalize());

    let download_url = format!("/packages/{}/download", id);

    Ok(Json(DownloadInfo {
        package_id: id,
        version: package.version.to_string(),
        download_url,
        sha256,
        size: content.len() as u64,
        content_type: "application/octet-stream".to_string(),
    }))
}

/// Download package content
#[instrument(skip(server))]
async fn download_package_handler(
    axum::extract::State(server): axum::extract::State<ContentDistributionServer>,
    Path(id): Path<String>,
) -> Result<axum::body::Body, StatusCode> {
    let package_id = parse_package_id(&id).ok_or(StatusCode::NOT_FOUND)?;
    let content_store = server.content.read().await;

    if let Some(content) = content_store.get(&package_id) {
        Ok(axum::body::Body::from(content.clone()))
    } else {
        Err(StatusCode::NOT_FOUND)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::Version;

    #[tokio::test]
    async fn test_content_distribution_config_default() {
        let config = ContentDistributionConfig::default();
        assert_eq!(config.listen_address, "0.0.0.0");
        assert_eq!(config.port, 8080);
        assert_eq!(config.max_package_size, 100 * 1024 * 1024);
        assert!(config.enable_cors);
    }

    #[tokio::test]
    async fn test_add_package() {
        let server = ContentDistributionServer::new(ContentDistributionConfig::default());
        let package = Package {
            id: PackageId::new("test", "package"),
            version: Version::new(1, 0, 0),
            metadata: crate::models::PackageMetadata {
                title: "Test Package".to_string(),
                description: "A test package".to_string(),
                long_description: None,
                categories: vec![],
                tags: vec![],
                license: "MIT".to_string(),
                authors: vec![],
                homepage: None,
                repository: None,
                documentation: None,
                readme: None,
                changelog: None,
                custom_fields: std::collections::HashMap::new(),
            },
            content_id: crate::models::ContentId::new("test", crate::models::HashAlgorithm::Sha256),
            dependencies: vec![],
            stats: crate::models::PackageStats::default(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        };

        let content = b"package content".to_vec();

        let result = server.add_package(package, content).await;
        assert!(result.is_ok());
        assert_eq!(server.package_count().await, 1);
    }

    #[tokio::test]
    async fn test_package_size_limit() {
        let config = ContentDistributionConfig {
            max_package_size: 1000,
            ..Default::default()
        };
        let server = ContentDistributionServer::new(config);

        let package = Package {
            id: PackageId::new("test", "large-package"),
            version: Version::new(1, 0, 0),
            metadata: crate::models::PackageMetadata {
                title: "Large Package".to_string(),
                description: "A large package".to_string(),
                long_description: None,
                categories: vec![],
                tags: vec![],
                license: "MIT".to_string(),
                authors: vec![],
                homepage: None,
                repository: None,
                documentation: None,
                readme: None,
                changelog: None,
                custom_fields: std::collections::HashMap::new(),
            },
            content_id: crate::models::ContentId::new("test", crate::models::HashAlgorithm::Sha256),
            dependencies: vec![],
            stats: crate::models::PackageStats::default(),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        };

        // Create content larger than limit
        let content = vec![0u8; 2000];

        let result = server.add_package(package, content).await;
        assert!(result.is_err());
    }
}
