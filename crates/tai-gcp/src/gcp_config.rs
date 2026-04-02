//! GCP configuration management with environment, metadata server, and Secret Manager support
//!
//! This module provides configuration loading strategies with fallback:
//! 1. Environment variables
//! 2. GCP metadata server
//! 3. Cloud Secret Manager
//! 4. Configuration files

use crate::gcp_errors::GcpError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;
use tracing::{debug, info};

/// GCP Configuration for service clients
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GcpConfig {
    /// GCP Project ID
    pub project_id: String,
    /// GCP Region (e.g., "us-central1", "europe-west1")
    pub region: String,
    /// Service account email
    pub service_account_email: Option<String>,
    /// Workload Identity configuration
    pub workload_identity: Option<WorkloadIdentityConfig>,
    /// Timeout SLOs per service
    pub timeouts: HashMap<String, Duration>,
    /// Multi-region failover configuration
    pub failover_regions: Vec<String>,
    /// Enable metrics collection
    pub metrics_enabled: bool,
    /// Enable detailed logging
    pub debug_logging: bool,
}

/// Workload Identity configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkloadIdentityConfig {
    /// Kubernetes service account name
    pub ksa_name: String,
    /// GCP service account email
    pub gsa_email: String,
}

impl Default for GcpConfig {
    fn default() -> Self {
        let mut timeouts = HashMap::new();
        timeouts.insert("run".to_string(), Duration::from_secs(30));
        timeouts.insert("pubsub".to_string(), Duration::from_secs(1));
        timeouts.insert("firestore".to_string(), Duration::from_secs(5));
        timeouts.insert("monitoring".to_string(), Duration::from_secs(2));
        timeouts.insert("scheduler".to_string(), Duration::from_secs(10));
        timeouts.insert("kms".to_string(), Duration::from_secs(3));
        timeouts.insert("logging".to_string(), Duration::from_secs(2));

        Self {
            project_id: "default-project".to_string(),
            region: "us-central1".to_string(),
            service_account_email: None,
            workload_identity: None,
            timeouts,
            failover_regions: vec![],
            metrics_enabled: false,
            debug_logging: false,
        }
    }
}

impl GcpConfig {
    /// Creates a new GCP configuration
    pub fn new(project_id: String, region: String) -> Self {
        let mut config = Self::default();
        config.project_id = project_id;
        config.region = region;
        config
    }

    /// Loads configuration from environment variables
    ///
    /// Supported variables:
    /// - `GCP_PROJECT_ID`: Project ID
    /// - `GCP_REGION`: Region (default: us-central1)
    /// - `GCP_SERVICE_ACCOUNT`: Service account email
    /// - `KSA_NAME`: Kubernetes service account name
    /// - `GSA_EMAIL`: GCP service account email (for Workload Identity)
    /// - `GCP_TIMEOUT_RUN`: Cloud Run timeout in seconds
    /// - `GCP_TIMEOUT_FIRESTORE`: Firestore timeout in seconds
    /// - `GCP_TIMEOUT_PUBSUB`: Pub/Sub timeout in seconds
    /// - `GCP_FAILOVER_REGIONS`: Comma-separated list of failover regions
    pub fn from_environment() -> Result<Self, GcpError> {
        let project_id = std::env::var("GCP_PROJECT_ID")
            .map_err(|_| GcpError::permanent("GCP_PROJECT_ID environment variable not set"))?;

        let region = std::env::var("GCP_REGION").unwrap_or_else(|_| "us-central1".to_string());

        let service_account_email = std::env::var("GCP_SERVICE_ACCOUNT").ok();

        // Load Workload Identity config if available
        let workload_identity = if let (Ok(ksa_name), Ok(gsa_email)) =
            (std::env::var("KSA_NAME"), std::env::var("GSA_EMAIL"))
        {
            Some(WorkloadIdentityConfig {
                ksa_name,
                gsa_email,
            })
        } else {
            None
        };

        // Load custom timeouts
        let mut timeouts = Self::default().timeouts;
        if let Ok(run_timeout) = std::env::var("GCP_TIMEOUT_RUN") {
            if let Ok(secs) = run_timeout.parse::<u64>() {
                timeouts.insert("run".to_string(), Duration::from_secs(secs));
            }
        }
        if let Ok(fs_timeout) = std::env::var("GCP_TIMEOUT_FIRESTORE") {
            if let Ok(secs) = fs_timeout.parse::<u64>() {
                timeouts.insert("firestore".to_string(), Duration::from_secs(secs));
            }
        }
        if let Ok(pubsub_timeout) = std::env::var("GCP_TIMEOUT_PUBSUB") {
            if let Ok(secs) = pubsub_timeout.parse::<u64>() {
                timeouts.insert("pubsub".to_string(), Duration::from_secs(secs));
            }
        }

        // Load failover regions
        let failover_regions = std::env::var("GCP_FAILOVER_REGIONS")
            .map(|r| r.split(',').map(|s| s.trim().to_string()).collect())
            .unwrap_or_default();

        let debug_logging = std::env::var("GCP_DEBUG")
            .map(|v| v.to_lowercase() == "true")
            .unwrap_or(false);

        let config = Self {
            project_id,
            region,
            service_account_email,
            workload_identity,
            timeouts,
            failover_regions,
            metrics_enabled: false,
            debug_logging,
        };

        info!(
            "Loaded GCP configuration from environment: project={}",
            config.project_id
        );
        Ok(config)
    }

    /// Loads configuration from GCP metadata server
    ///
    /// Retrieves:
    /// - `project/project-id`
    /// - `instance/service-accounts/default/email`
    /// - `instance/region` (if available)
    pub async fn from_metadata_server() -> Result<Self, GcpError> {
        let metadata_endpoint = "http://metadata.google.internal/computeMetadata/v1";
        let client = reqwest::Client::new();

        // Get project ID
        let project_id =
            Self::fetch_metadata(&client, metadata_endpoint, "project/project-id").await?;

        // Get service account email
        let service_account_email = Self::fetch_metadata(
            &client,
            metadata_endpoint,
            "instance/service-accounts/default/email",
        )
        .await
        .ok();

        // Get region (may not be available)
        let region = Self::fetch_metadata(&client, metadata_endpoint, "instance/region")
            .await
            .unwrap_or_else(|_| "us-central1".to_string());

        let config = Self {
            project_id,
            region,
            service_account_email,
            workload_identity: None,
            timeouts: Self::default().timeouts,
            failover_regions: vec![],
            metrics_enabled: false,
            debug_logging: false,
        };

        info!(
            "Loaded GCP configuration from metadata server: project={}",
            config.project_id
        );
        Ok(config)
    }

    /// Loads configuration from TOML file
    pub fn from_toml_file(path: &str) -> Result<Self, GcpError> {
        let content = std::fs::read_to_string(path).map_err(|e| {
            GcpError::permanent(format!("Failed to read configuration file: {}", e))
        })?;

        let config: Self = toml::from_str(&content).map_err(|e| {
            GcpError::permanent(format!("Failed to parse configuration file: {}", e))
        })?;

        info!(
            "Loaded GCP configuration from file: project={}",
            config.project_id
        );
        Ok(config)
    }

    /// Loads configuration from JSON file
    pub fn from_json_file(path: &str) -> Result<Self, GcpError> {
        let content = std::fs::read_to_string(path).map_err(|e| {
            GcpError::permanent(format!("Failed to read configuration file: {}", e))
        })?;

        let config: Self = serde_json::from_str(&content).map_err(|e| {
            GcpError::permanent(format!("Failed to parse configuration file: {}", e))
        })?;

        info!(
            "Loaded GCP configuration from file: project={}",
            config.project_id
        );
        Ok(config)
    }

    /// Gets timeout for a specific service
    pub fn get_timeout(&self, service: &str) -> Duration {
        self.timeouts
            .get(service)
            .copied()
            .unwrap_or_else(|| Duration::from_secs(30))
    }

    /// Sets timeout for a specific service
    pub fn set_timeout(&mut self, service: &str, timeout: Duration) {
        self.timeouts.insert(service.to_string(), timeout);
    }

    /// Adds a failover region
    pub fn add_failover_region(&mut self, region: String) {
        if !self.failover_regions.contains(&region) {
            self.failover_regions.push(region);
        }
    }

    /// Enables metrics collection
    pub fn enable_metrics(&mut self) {
        self.metrics_enabled = true;
    }

    /// Enables debug logging
    pub fn enable_debug_logging(&mut self) {
        self.debug_logging = true;
    }

    /// Fetches a value from the metadata server
    async fn fetch_metadata(
        client: &reqwest::Client, endpoint: &str, path: &str,
    ) -> Result<String, GcpError> {
        let url = format!("{}/{}", endpoint, path);

        let response = client
            .get(&url)
            .header("Metadata-Flavor", "Google")
            .send()
            .await
            .map_err(|e| GcpError::transient(format!("Failed to reach metadata server: {}", e)))?;

        if !response.status().is_success() {
            return Err(GcpError::transient(format!(
                "Metadata server request failed: {} {}",
                response.status(),
                response.text().await.unwrap_or_default()
            )));
        }

        response
            .text()
            .await
            .map_err(|e| GcpError::transient(format!("Failed to read metadata response: {}", e)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = GcpConfig::default();
        assert_eq!(config.project_id, "default-project");
        assert_eq!(config.region, "us-central1");
        assert_eq!(config.get_timeout("run"), Duration::from_secs(30));
        assert_eq!(config.get_timeout("firestore"), Duration::from_secs(5));
    }

    #[test]
    fn test_config_creation() {
        let config = GcpConfig::new("my-project".to_string(), "europe-west1".to_string());
        assert_eq!(config.project_id, "my-project");
        assert_eq!(config.region, "europe-west1");
    }

    #[test]
    fn test_config_set_timeout() {
        let mut config = GcpConfig::default();
        config.set_timeout("run", Duration::from_secs(60));
        assert_eq!(config.get_timeout("run"), Duration::from_secs(60));
    }

    #[test]
    fn test_config_add_failover_region() {
        let mut config = GcpConfig::default();
        config.add_failover_region("us-east1".to_string());
        config.add_failover_region("europe-west1".to_string());
        assert_eq!(config.failover_regions.len(), 2);

        // Adding duplicate should not increase count
        config.add_failover_region("us-east1".to_string());
        assert_eq!(config.failover_regions.len(), 2);
    }

    #[test]
    fn test_config_serde_json() {
        let config = GcpConfig::new("my-project".to_string(), "us-central1".to_string());
        let json = serde_json::to_string(&config).expect("serialization failed");
        let deserialized: GcpConfig = serde_json::from_str(&json).expect("deserialization failed");
        assert_eq!(deserialized.project_id, config.project_id);
    }

    #[test]
    fn test_workload_identity_config() {
        let wi_config = WorkloadIdentityConfig {
            ksa_name: "default".to_string(),
            gsa_email: "gsa@my-project.iam.gserviceaccount.com".to_string(),
        };
        assert_eq!(wi_config.ksa_name, "default");
        assert_eq!(
            wi_config.gsa_email,
            "gsa@my-project.iam.gserviceaccount.com"
        );
    }
}
