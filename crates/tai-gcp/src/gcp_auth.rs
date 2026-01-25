//! GCP authentication layer with Workload Identity and ADC support
//!
//! This module provides authentication strategies with automatic fallback:
//! 1. **Workload Identity** (preferred): GCP-native service account federation
//! 2. **ADC** (Application Default Credentials): Google's standard flow with env vars and metadata server

use crate::gcp_errors::{GcpError, GcpErrorKind};
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::sync::Arc;
use tracing::{debug, info, warn};

/// Token cache entry with expiry tracking
#[derive(Debug, Clone)]
struct TokenCacheEntry {
    /// JWT token
    token: String,
    /// Expiry time
    expiry: DateTime<Utc>,
}

impl TokenCacheEntry {
    /// Creates a new token cache entry
    fn new(token: String, expires_in_secs: u64) -> Self {
        let expiry = Utc::now() + Duration::seconds(expires_in_secs as i64);
        Self { token, expiry }
    }

    /// Returns true if token is expired (with 5-minute buffer for safety)
    fn is_expired(&self) -> bool {
        Utc::now() >= (self.expiry - Duration::minutes(5))
    }
}

static TOKEN_CACHE: Lazy<RwLock<Option<TokenCacheEntry>>> = Lazy::new(|| RwLock::new(None));

/// GCP authentication strategy trait for pluggable auth implementations
#[async_trait]
pub trait GcpAuthStrategy: Send + Sync {
    /// Gets a valid access token for GCP API calls
    async fn get_token(&self) -> Result<String, GcpError>;

    /// Refreshes the stored token
    async fn refresh_token(&self) -> Result<String, GcpError>;
}

/// Workload Identity authentication strategy (preferred for GKE)
///
/// Uses GCP's native Workload Identity for automatic service account token exchange.
/// - No service account keys needed
/// - Automatic token rotation
/// - Least privilege principle
pub struct WorkloadIdentityAuth {
    /// GCP Project ID
    project_id: String,
    /// Kubernetes service account name
    ksa_name: String,
    /// GCP service account email
    gsa_email: String,
    /// Metadata server endpoint
    metadata_endpoint: String,
}

impl WorkloadIdentityAuth {
    /// Creates a new Workload Identity authenticator
    ///
    /// # Arguments
    /// * `project_id` - GCP Project ID
    /// * `ksa_name` - Kubernetes service account name
    /// * `gsa_email` - GCP service account email (format: gsa@project.iam.gserviceaccount.com)
    pub fn new(project_id: String, ksa_name: String, gsa_email: String) -> Self {
        Self {
            project_id,
            ksa_name,
            gsa_email,
            metadata_endpoint: "http://metadata.google.internal/computeMetadata/v1".to_string(),
        }
    }

    /// Creates from environment variables
    /// - `GCP_PROJECT_ID`: Project ID
    /// - `KSA_NAME`: Kubernetes service account
    /// - `GSA_EMAIL`: GCP service account email
    pub fn from_environment() -> Result<Self, GcpError> {
        let project_id = std::env::var("GCP_PROJECT_ID").map_err(|_| {
            GcpError::authentication_failure("GCP_PROJECT_ID environment variable not set")
        })?;
        let ksa_name = std::env::var("KSA_NAME").unwrap_or_else(|_| "default".to_string());
        let gsa_email = std::env::var("GSA_EMAIL").map_err(|_| {
            GcpError::authentication_failure(
                "GSA_EMAIL environment variable not set (expected format: gsa@project.iam.gserviceaccount.com)",
            )
        })?;

        Ok(Self::new(project_id, ksa_name, gsa_email))
    }
}

#[async_trait]
impl GcpAuthStrategy for WorkloadIdentityAuth {
    async fn get_token(&self) -> Result<String, GcpError> {
        // Check token cache first
        if let Some(entry) = TOKEN_CACHE.read().as_ref() {
            if !entry.is_expired() {
                debug!("Using cached Workload Identity token");
                return Ok(entry.token.clone());
            }
        }

        // Token expired or not cached, refresh
        self.refresh_token().await
    }

    async fn refresh_token(&self) -> Result<String, GcpError> {
        info!("Refreshing Workload Identity token for {}", self.gsa_email);

        let url = format!(
            "{}/instance/service-accounts/default/identity?audience={}&includeEmail=true",
            self.metadata_endpoint,
            urlencoding::encode(&self.gsa_email)
        );

        let client = reqwest::Client::new();
        let response = client
            .get(&url)
            .header("Metadata-Flavor", "Google")
            .send()
            .await
            .map_err(|e| GcpError::transient(format!("Failed to reach metadata server: {}", e)))?;

        if !response.status().is_success() {
            return Err(GcpError::authentication_failure(format!(
                "Workload Identity token request failed: {} {}",
                response.status(),
                response.text().await.unwrap_or_default()
            )));
        }

        let token = response
            .text()
            .await
            .map_err(|e| GcpError::transient(format!("Failed to read token response: {}", e)))?;

        // Cache token with 1 hour TTL
        {
            let mut cache = TOKEN_CACHE.write();
            *cache = Some(TokenCacheEntry::new(token.clone(), 3600));
        }

        Ok(token)
    }
}

/// Application Default Credentials (ADC) authentication strategy
///
/// Uses Google's standard credential discovery chain:
/// 1. Environment variable: `GOOGLE_APPLICATION_CREDENTIALS` (path to service account JSON)
/// 2. GCP metadata server (if running on Compute Engine, GKE, Cloud Run, etc.)
/// 3. Local `gcloud` credentials (`~/.config/gcloud/`)
pub struct AdcAuth {
    /// Service account key path (if using env var)
    key_path: Option<String>,
    /// Metadata server endpoint
    metadata_endpoint: String,
}

impl AdcAuth {
    /// Creates a new ADC authenticator
    pub fn new() -> Self {
        Self {
            key_path: std::env::var("GOOGLE_APPLICATION_CREDENTIALS").ok(),
            metadata_endpoint: "http://metadata.google.internal/computeMetadata/v1".to_string(),
        }
    }

    /// Creates ADC with specific service account key file
    pub fn with_key_path(key_path: String) -> Self {
        Self {
            key_path: Some(key_path),
            metadata_endpoint: "http://metadata.google.internal/computeMetadata/v1".to_string(),
        }
    }
}

impl Default for AdcAuth {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl GcpAuthStrategy for AdcAuth {
    async fn get_token(&self) -> Result<String, GcpError> {
        // Check cache first
        if let Some(entry) = TOKEN_CACHE.read().as_ref() {
            if !entry.is_expired() {
                debug!("Using cached ADC token");
                return Ok(entry.token.clone());
            }
        }

        // Token expired or not cached, refresh
        self.refresh_token().await
    }

    async fn refresh_token(&self) -> Result<String, GcpError> {
        info!("Refreshing ADC token");

        // Try metadata server first (Cloud Run, GKE, Compute Engine)
        let token_result = self.get_metadata_server_token().await;

        if token_result.is_ok() {
            return token_result;
        }

        // If metadata server fails, try service account key file
        if let Some(ref key_path) = self.key_path {
            warn!("Metadata server unavailable, trying service account key file");
            return self.get_service_account_token(key_path).await;
        }

        Err(GcpError::authentication_failure(
            "No valid credentials found (no metadata server and no GOOGLE_APPLICATION_CREDENTIALS)",
        ))
    }
}

impl AdcAuth {
    async fn get_metadata_server_token(&self) -> Result<String, GcpError> {
        let url = format!(
            "{}/instance/service-accounts/default/token",
            self.metadata_endpoint
        );

        let client = reqwest::Client::new();
        let response = client
            .get(&url)
            .header("Metadata-Flavor", "Google")
            .send()
            .await
            .map_err(|e| GcpError::transient(format!("Failed to reach metadata server: {}", e)))?;

        if !response.status().is_success() {
            return Err(GcpError::transient(format!(
                "Metadata server token request failed: {}",
                response.status()
            )));
        }

        let body: serde_json::Value = response.json().await.map_err(|e| {
            GcpError::transient(format!("Failed to parse metadata server response: {}", e))
        })?;

        let token = body
            .get("access_token")
            .and_then(|t| t.as_str())
            .ok_or_else(|| GcpError::transient("No access_token in metadata server response"))?;

        let expires_in: u64 = body
            .get("expires_in")
            .and_then(|e| e.as_u64())
            .unwrap_or(3600);

        // Cache token
        {
            let mut cache = TOKEN_CACHE.write();
            *cache = Some(TokenCacheEntry::new(token.to_string(), expires_in));
        }

        Ok(token.to_string())
    }

    async fn get_service_account_token(&self, key_path: &str) -> Result<String, GcpError> {
        let key_content = std::fs::read_to_string(key_path).map_err(|e| {
            GcpError::permanent(format!("Failed to read service account key file: {}", e))
        })?;

        let key: serde_json::Value = serde_json::from_str(&key_content)
            .map_err(|e| GcpError::permanent(format!("Invalid service account key JSON: {}", e)))?;

        // Extract client email and private key from service account JSON
        let client_email = key
            .get("client_email")
            .and_then(|e| e.as_str())
            .ok_or_else(|| GcpError::permanent("Missing client_email in service account key"))?;

        let private_key = key
            .get("private_key")
            .and_then(|k| k.as_str())
            .ok_or_else(|| GcpError::permanent("Missing private_key in service account key"))?;

        // Create JWT and exchange for access token
        // Note: In production, use yup-oauth2 crate for proper JWT handling
        warn!("Service account key file authentication requires external token exchange (not yet implemented)");
        Err(GcpError::permanent(
            "Service account key authentication requires yup-oauth2 integration",
        ))
    }
}

/// Multi-strategy authentication with fallback support
pub struct GcpAuthenticator {
    /// Primary strategy
    primary: Arc<dyn GcpAuthStrategy>,
    /// Fallback strategy (optional)
    fallback: Option<Arc<dyn GcpAuthStrategy>>,
}

impl GcpAuthenticator {
    /// Creates an authenticator with primary strategy
    pub fn with_primary(primary: Arc<dyn GcpAuthStrategy>) -> Self {
        Self {
            primary,
            fallback: None,
        }
    }

    /// Creates an authenticator with primary and fallback strategies
    pub fn with_fallback(
        primary: Arc<dyn GcpAuthStrategy>, fallback: Arc<dyn GcpAuthStrategy>,
    ) -> Self {
        Self {
            primary,
            fallback: Some(fallback),
        }
    }

    /// Creates an authenticator using environment-based auto-detection
    pub fn from_environment() -> Result<Self, GcpError> {
        // Try Workload Identity first
        if let Ok(wi_auth) = WorkloadIdentityAuth::from_environment() {
            info!("Using Workload Identity authentication");
            let adc_auth = Arc::new(AdcAuth::new());
            return Ok(Self::with_fallback(Arc::new(wi_auth), adc_auth));
        }

        // Fall back to ADC only
        info!("Using Application Default Credentials (ADC) authentication");
        Ok(Self::with_primary(Arc::new(AdcAuth::new())))
    }

    /// Gets a valid access token (with automatic retry on fallback)
    pub async fn get_token(&self) -> Result<String, GcpError> {
        match self.primary.get_token().await {
            Ok(token) => Ok(token),
            Err(e) => {
                if let Some(ref fallback) = self.fallback {
                    warn!("Primary auth failed ({}), trying fallback", e);
                    fallback.get_token().await
                } else {
                    Err(e)
                }
            }
        }
    }

    /// Refreshes the token
    pub async fn refresh_token(&self) -> Result<String, GcpError> {
        match self.primary.refresh_token().await {
            Ok(token) => Ok(token),
            Err(e) => {
                if let Some(ref fallback) = self.fallback {
                    warn!("Primary auth refresh failed ({}), trying fallback", e);
                    fallback.refresh_token().await
                } else {
                    Err(e)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_cache_entry_creation() {
        let token = TokenCacheEntry::new("test_token".to_string(), 3600);
        assert_eq!(token.token, "test_token");
        assert!(!token.is_expired());
    }

    #[test]
    fn test_token_cache_entry_expiration() {
        let mut token = TokenCacheEntry::new("test_token".to_string(), 1);
        token.expiry = Utc::now() - Duration::minutes(10);
        assert!(token.is_expired());
    }

    #[test]
    fn test_workload_identity_auth_creation() {
        let wi_auth = WorkloadIdentityAuth::new(
            "my-project".to_string(),
            "default".to_string(),
            "gsa@my-project.iam.gserviceaccount.com".to_string(),
        );
        assert_eq!(wi_auth.project_id, "my-project");
        assert_eq!(wi_auth.ksa_name, "default");
        assert_eq!(wi_auth.gsa_email, "gsa@my-project.iam.gserviceaccount.com");
    }

    #[test]
    fn test_adc_auth_creation() {
        let adc = AdcAuth::new();
        assert_eq!(
            adc.metadata_endpoint,
            "http://metadata.google.internal/computeMetadata/v1"
        );
    }

    #[test]
    fn test_authenticator_creation() {
        let primary = Arc::new(AdcAuth::new());
        let fallback = Arc::new(AdcAuth::new());
        let auth = GcpAuthenticator::with_fallback(primary, fallback);
        assert!(auth.fallback.is_some());
    }
}
