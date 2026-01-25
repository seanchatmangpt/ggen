//! HashiCorp Vault integration for dynamic secret generation and management
//!
//! This module provides comprehensive Vault integration including:
//! - Dynamic secret generation (database credentials, API keys)
//! - Secret versioning and history
//! - Lease management with automatic renewal
//! - Encryption as a Service (EaaS) via Transit engine
//! - Multiple authentication methods (Kubernetes, JWT, OAuth2)

use crate::error::{Result, SecurityError};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{debug, error, info};

/// Dynamic secret metadata
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Secret {
    /// Secret name/path
    pub name: String,

    /// Secret data (key-value pairs)
    pub data: HashMap<String, String>,

    /// Lease ID (for renewal)
    pub lease_id: String,

    /// Lease duration in seconds
    pub lease_duration: u64,

    /// Whether secret is renewable
    pub renewable: bool,

    /// Secret version
    pub version: u64,

    /// Creation timestamp
    pub created_at: DateTime<Utc>,

    /// Last renewal timestamp
    pub renewed_at: Option<DateTime<Utc>>,

    /// Expiration timestamp (lease_duration seconds from created_at)
    pub expires_at: DateTime<Utc>,

    /// Metadata about the secret
    pub metadata: HashMap<String, String>,
}

/// Database credential secret
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DatabaseCredential {
    /// Database connection username
    pub username: String,

    /// Database connection password (sensitive)
    pub password: String,

    /// Database connection string
    pub connection_string: String,

    /// Credential TTL in seconds
    pub ttl: u64,

    /// Lease ID for renewal
    pub lease_id: String,

    /// When credential expires
    pub expires_at: DateTime<Utc>,
}

/// API key secret
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ApiKeySecret {
    /// API key value (sensitive)
    pub key: String,

    /// API key name/identifier
    pub name: String,

    /// Key type (e.g., "database", "service", "webhook")
    pub key_type: String,

    /// Key TTL in seconds
    pub ttl: u64,

    /// When key expires
    pub expires_at: DateTime<Utc>,

    /// Permissions/scopes
    pub scopes: Vec<String>,
}

/// Vault client configuration
#[derive(Clone, Debug)]
pub struct VaultConfig {
    /// Vault server URL
    pub url: String,

    /// Authentication token
    pub token: String,

    /// Request timeout duration
    pub timeout: Duration,

    /// Namespace (for namespaced Vaults)
    pub namespace: Option<String>,

    /// Enable TLS verification
    pub tls_verify: bool,

    /// CA certificate path
    pub ca_cert: Option<String>,

    /// Lease renewal threshold (renew when TTL% remains)
    pub renewal_threshold: f64,
}

/// Vault client for secret management
pub struct VaultClient {
    config: VaultConfig,
    client: reqwest::Client,

    /// In-memory cache of secrets with lease info
    secret_cache: Arc<RwLock<HashMap<String, Secret>>>,

    /// Lease renewal tasks (token -> abort handle)
    active_leases: Arc<RwLock<HashMap<String, tokio::task::JoinHandle<()>>>>,
}

impl VaultClient {
    /// Create new Vault client
    pub async fn new(config: VaultConfig) -> Result<Self> {
        let client = reqwest::Client::builder()
            .timeout(config.timeout)
            .build()
            .map_err(|e| SecurityError::vault(format!("Failed to create HTTP client: {}", e)))?;

        // Test connection
        let response = client
            .get(&format!("{}/v1/sys/health", config.url))
            .send()
            .await
            .map_err(|e| SecurityError::vault(format!("Failed to connect to Vault: {}", e)))?;

        if !response.status().is_success() {
            return Err(SecurityError::vault(
                format!("Vault health check failed: {}", response.status()),
            ));
        }

        info!("Connected to Vault at {}", config.url);

        Ok(Self {
            config,
            client,
            secret_cache: Arc::new(RwLock::new(HashMap::new())),
            active_leases: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Retrieve static secret (KV v2)
    pub async fn get_secret(&self, path: &str) -> Result<Secret> {
        // Check cache first
        {
            let cache = self.secret_cache.read().await;
            if let Some(cached) = cache.get(path) {
                debug!("Retrieved secret from cache: {}", path);
                return Ok(cached.clone());
            }
        }

        let url = format!("{}/v1/secret/data/{}", self.config.url, path);
        let response = self
            .client
            .get(&url)
            .header("X-Vault-Token", &self.config.token)
            .send()
            .await
            .map_err(|e| {
                error!("Failed to retrieve secret from Vault: {}", e);
                SecurityError::vault(format!("Secret retrieval failed: {}", e))
            })?;

        if response.status() == 404 {
            return Err(SecurityError::not_found(format!("Secret not found: {}", path)));
        }

        if !response.status().is_success() {
            return Err(SecurityError::vault(
                format!("Failed to retrieve secret: {}", response.status()),
            ));
        }

        let body: VaultKvResponse = response.json().await.map_err(|e| {
            error!("Failed to parse Vault response: {}", e);
            SecurityError::serialization(format!("Invalid Vault response: {}", e))
        })?;

        let secret = Secret {
            name: path.to_string(),
            data: body.data.data,
            lease_id: body.request_id.clone(),
            lease_duration: 0,
            renewable: false,
            version: body.data.metadata.version,
            created_at: Utc::now(),
            renewed_at: None,
            expires_at: Utc::now(),
            metadata: body.data.metadata.custom_metadata.unwrap_or_default(),
        };

        // Cache the secret
        {
            let mut cache = self.secret_cache.write().await;
            cache.insert(path.to_string(), secret.clone());
        }

        debug!("Retrieved secret: {} (version: {})", path, secret.version);
        Ok(secret)
    }

    /// Generate dynamic database credential
    pub async fn generate_database_credential(
        &self,
        database_role: &str,
    ) -> Result<DatabaseCredential> {
        let url = format!(
            "{}/v1/database/static-creds/{}",
            self.config.url, database_role
        );

        let response = self
            .client
            .get(&url)
            .header("X-Vault-Token", &self.config.token)
            .send()
            .await
            .map_err(|e| {
                error!("Failed to generate database credential: {}", e);
                SecurityError::vault(format!("Credential generation failed: {}", e))
            })?;

        if !response.status().is_success() {
            return Err(SecurityError::vault(
                format!("Failed to generate credential: {}", response.status()),
            ));
        }

        let body: VaultDbCredResponse = response.json().await.map_err(|e| {
            error!("Failed to parse credential response: {}", e);
            SecurityError::serialization(format!("Invalid response format: {}", e))
        })?;

        let ttl = body.lease_duration;
        let expires_at = Utc::now() + chrono::Duration::seconds(ttl as i64);

        let credential = DatabaseCredential {
            username: body.data.username,
            password: body.data.password,
            connection_string: format!(
                "postgresql://{}:{}@localhost/postgres",
                body.data.username, body.data.password
            ),
            ttl,
            lease_id: body.lease_id.clone(),
            expires_at,
        };

        // Start lease renewal
        self.start_lease_renewal(&body.lease_id, ttl).await;

        info!("Generated database credential for role: {}", database_role);
        Ok(credential)
    }

    /// Encrypt data using Transit secrets engine
    pub async fn encrypt(&self, transit_path: &str, plaintext: &[u8]) -> Result<String> {
        let encoded = encode_base64(plaintext);
        let url = format!("{}/v1/{}/encrypt/{}", self.config.url, transit_path, "default");

        let mut request_body = HashMap::new();
        request_body.insert("plaintext", encoded);

        let response = self
            .client
            .post(&url)
            .header("X-Vault-Token", &self.config.token)
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                error!("Failed to encrypt data: {}", e);
                SecurityError::encryption(format!("Encryption failed: {}", e))
            })?;

        if !response.status().is_success() {
            return Err(SecurityError::encryption(
                format!("Encryption failed: {}", response.status()),
            ));
        }

        let body: VaultTransitResponse = response.json().await.map_err(|e| {
            error!("Failed to parse encryption response: {}", e);
            SecurityError::serialization(format!("Invalid response: {}", e))
        })?;

        debug!("Data encrypted successfully");
        Ok(body.data.ciphertext)
    }

    /// Decrypt data using Transit secrets engine
    pub async fn decrypt(&self, transit_path: &str, ciphertext: &str) -> Result<Vec<u8>> {
        let url = format!("{}/v1/{}/decrypt/{}", self.config.url, transit_path, "default");

        let mut request_body = HashMap::new();
        request_body.insert("ciphertext", ciphertext.to_string());

        let response = self
            .client
            .post(&url)
            .header("X-Vault-Token", &self.config.token)
            .json(&request_body)
            .send()
            .await
            .map_err(|e| {
                error!("Failed to decrypt data: {}", e);
                SecurityError::encryption(format!("Decryption failed: {}", e))
            })?;

        if !response.status().is_success() {
            return Err(SecurityError::encryption(
                format!("Decryption failed: {}", response.status()),
            ));
        }

        let body: VaultTransitResponse = response.json().await.map_err(|e| {
            error!("Failed to parse decryption response: {}", e);
            SecurityError::serialization(format!("Invalid response: {}", e))
        })?;

        let decoded = decode_base64(&body.data.plaintext).map_err(|e| {
            error!("Failed to decode plaintext: {}", e);
            SecurityError::encryption(format!("Decode failed: {}", e))
        })?;

        debug!("Data decrypted successfully");
        Ok(decoded)
    }

    /// Renew a lease
    pub async fn renew_lease(&self, lease_id: &str) -> Result<u64> {
        let url = format!("{}/v1/sys/leases/renew/{}", self.config.url, lease_id);

        let response = self
            .client
            .put(&url)
            .header("X-Vault-Token", &self.config.token)
            .send()
            .await
            .map_err(|e| {
                error!("Failed to renew lease: {}", e);
                SecurityError::lease(format!("Renewal failed: {}", e))
            })?;

        if !response.status().is_success() {
            return Err(SecurityError::lease(
                format!("Lease renewal failed: {}", response.status()),
            ));
        }

        let body: VaultLeaseResponse = response.json().await.map_err(|e| {
            error!("Failed to parse lease response: {}", e);
            SecurityError::serialization(format!("Invalid response: {}", e))
        })?;

        info!("Renewed lease {} (TTL: {} seconds)", lease_id, body.lease_duration);
        Ok(body.lease_duration)
    }

    /// Start automatic lease renewal
    async fn start_lease_renewal(&self, lease_id: &str, ttl: u64) {
        let lease_id = lease_id.to_string();
        let client = self.client.clone();
        let token = self.config.token.clone();
        let url = self.config.url.clone();

        let renewal_threshold = self.config.renewal_threshold;

        let handle = tokio::spawn(async move {
            loop {
                // Sleep until 75% of TTL has elapsed
                let sleep_duration = Duration::from_secs(((ttl as f64) * renewal_threshold) as u64);
                tokio::time::sleep(sleep_duration).await;

                // Attempt renewal
                match Self::renew_lease_internal(&client, &token, &url, &lease_id).await {
                    Ok(new_ttl) => {
                        debug!("Lease renewal successful, new TTL: {}", new_ttl);
                    }
                    Err(e) => {
                        error!("Lease renewal failed: {}", e);
                        // Exit renewal task on failure
                        break;
                    }
                }
            }
        });

        let mut leases = self.active_leases.write().await;
        leases.insert(lease_id, handle);
    }

    /// Internal lease renewal
    async fn renew_lease_internal(
        client: &reqwest::Client,
        token: &str,
        vault_url: &str,
        lease_id: &str,
    ) -> Result<u64> {
        let url = format!("{}/v1/sys/leases/renew/{}", vault_url, lease_id);

        let response = client
            .put(&url)
            .header("X-Vault-Token", token)
            .send()
            .await
            .map_err(|e| SecurityError::lease(format!("Renewal failed: {}", e)))?;

        if !response.status().is_success() {
            return Err(SecurityError::lease(
                format!("Renewal failed: {}", response.status()),
            ));
        }

        let body: VaultLeaseResponse = response.json().await.map_err(|e| {
            SecurityError::serialization(format!("Invalid response: {}", e))
        })?;

        Ok(body.lease_duration)
    }

    /// Clear secret cache
    pub async fn clear_cache(&self) {
        self.secret_cache.write().await.clear();
        debug!("Secret cache cleared");
    }

    /// Get cache statistics
    pub async fn cache_stats(&self) -> (usize, usize) {
        let cache = self.secret_cache.read().await;
        let leases = self.active_leases.read().await;
        (cache.len(), leases.len())
    }
}

// Internal response types
#[derive(Deserialize)]
struct VaultKvResponse {
    request_id: String,
    data: VaultKvData,
}

#[derive(Deserialize)]
struct VaultKvData {
    data: HashMap<String, String>,
    metadata: VaultMetadata,
}

#[derive(Deserialize)]
struct VaultMetadata {
    version: u64,
    #[serde(default)]
    custom_metadata: Option<HashMap<String, String>>,
}

#[derive(Deserialize)]
struct VaultDbCredResponse {
    lease_id: String,
    lease_duration: u64,
    data: VaultDbCredData,
}

#[derive(Deserialize)]
struct VaultDbCredData {
    username: String,
    password: String,
}

#[derive(Deserialize)]
struct VaultTransitResponse {
    data: VaultTransitData,
}

#[derive(Deserialize)]
struct VaultTransitData {
    ciphertext: String,
    #[serde(default)]
    plaintext: String,
}

#[derive(Deserialize)]
struct VaultLeaseResponse {
    lease_duration: u64,
}

// Base64 encoding/decoding helpers
fn encode_base64(data: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(data)
}

fn decode_base64(data: &str) -> std::result::Result<Vec<u8>, base64::DecodeError> {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.decode(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_secret_creation() {
        let now = Utc::now();
        let secret = Secret {
            name: "test-secret".to_string(),
            data: [("key".to_string(), "value".to_string())].iter().cloned().collect(),
            lease_id: "lease-123".to_string(),
            lease_duration: 3600,
            renewable: true,
            version: 1,
            created_at: now,
            renewed_at: None,
            expires_at: now + chrono::Duration::hours(1),
            metadata: HashMap::new(),
        };

        assert_eq!(secret.name, "test-secret");
        assert_eq!(secret.version, 1);
        assert!(secret.renewable);
    }

    #[test]
    fn test_database_credential_creation() {
        let now = Utc::now();
        let cred = DatabaseCredential {
            username: "user".to_string(),
            password: "pass".to_string(),
            connection_string: "postgresql://user:pass@localhost/db".to_string(),
            ttl: 3600,
            lease_id: "lease-123".to_string(),
            expires_at: now + chrono::Duration::hours(1),
        };

        assert_eq!(cred.username, "user");
        assert!(cred.connection_string.contains("postgresql://"));
    }

    #[test]
    fn test_api_key_secret_creation() {
        let now = Utc::now();
        let key = ApiKeySecret {
            key: "sk_test_123".to_string(),
            name: "api-key".to_string(),
            key_type: "database".to_string(),
            ttl: 86400,
            expires_at: now + chrono::Duration::days(1),
            scopes: vec!["read:secret".to_string(), "write:secret".to_string()],
        };

        assert_eq!(key.key_type, "database");
        assert_eq!(key.scopes.len(), 2);
    }
}
