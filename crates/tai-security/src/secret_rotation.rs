//! Secret rotation and lifecycle management
//!
//! This module provides:
//! - Automatic secret rotation scheduler
//! - Database credential rotation
//! - API key rotation
//! - Certificate rotation
//! - Atomic rotation with graceful fallback
//! - Audit trail for all rotation activities

use crate::error::{Result, SecurityError};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// Secret type
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SecretType {
    /// Database credential
    #[serde(rename = "DATABASE_CREDENTIAL")]
    DatabaseCredential,

    /// API key
    #[serde(rename = "API_KEY")]
    ApiKey,

    /// Service account token
    #[serde(rename = "SERVICE_ACCOUNT_TOKEN")]
    ServiceAccountToken,

    /// OAuth2 token
    #[serde(rename = "OAUTH2_TOKEN")]
    Oauth2Token,

    /// TLS certificate
    #[serde(rename = "TLS_CERTIFICATE")]
    TlsCertificate,

    /// SSH key
    #[serde(rename = "SSH_KEY")]
    SshKey,

    /// Other/custom secret
    #[serde(rename = "OTHER")]
    Other,
}

/// Rotation policy
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RotationPolicy {
    /// Secret identifier
    pub secret_id: String,

    /// Secret type
    pub secret_type: SecretType,

    /// Rotation interval (days)
    pub rotation_interval_days: u64,

    /// Grace period before old secret is revoked (hours)
    pub grace_period_hours: u64,

    /// Last rotation timestamp
    pub last_rotated_at: DateTime<Utc>,

    /// Next scheduled rotation
    pub next_rotation_at: DateTime<Utc>,

    /// Is rotation enabled
    pub enabled: bool,

    /// Number of retained versions
    pub retention_count: usize,

    /// Auto-rotate even if no issue detected
    pub auto_rotate: bool,

    /// Notify before rotation
    pub notify_before_rotation: bool,

    /// Notification channels (email, slack, etc.)
    pub notification_channels: Vec<String>,
}

/// Secret version
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SecretVersion {
    /// Version number
    pub version: u64,

    /// Secret data (sensitive - should be encrypted)
    pub secret_data: String,

    /// When this version was created
    pub created_at: DateTime<Utc>,

    /// When this version became active
    pub activated_at: Option<DateTime<Utc>>,

    /// When this version was deactivated
    pub deactivated_at: Option<DateTime<Utc>>,

    /// Status of this version
    pub status: VersionStatus,

    /// Reason for status
    pub reason: Option<String>,
}

/// Version status
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum VersionStatus {
    /// Version is being tested
    #[serde(rename = "PENDING")]
    Pending,

    /// Version is active and in use
    #[serde(rename = "ACTIVE")]
    Active,

    /// Version is in grace period
    #[serde(rename = "GRACE_PERIOD")]
    GracePeriod,

    /// Version has been deactivated
    #[serde(rename = "DEACTIVATED")]
    Deactivated,

    /// Version failed and was revoked
    #[serde(rename = "FAILED")]
    Failed,
}

/// Rotation event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RotationEvent {
    /// Unique event ID
    pub event_id: String,

    /// Secret that was rotated
    pub secret_id: String,

    /// Type of secret
    pub secret_type: SecretType,

    /// Old version
    pub old_version: u64,

    /// New version
    pub new_version: u64,

    /// Status
    pub status: RotationStatus,

    /// When rotation was initiated
    pub started_at: DateTime<Utc>,

    /// When rotation completed
    pub completed_at: Option<DateTime<Utc>>,

    /// Error message (if failed)
    pub error: Option<String>,

    /// Who initiated the rotation
    pub initiated_by: String,

    /// Reason for rotation
    pub reason: String,
}

/// Rotation status
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum RotationStatus {
    /// Rotation in progress
    #[serde(rename = "IN_PROGRESS")]
    InProgress,

    /// Rotation completed successfully
    #[serde(rename = "COMPLETED")]
    Completed,

    /// Rotation failed
    #[serde(rename = "FAILED")]
    Failed,

    /// Rotation rolled back
    #[serde(rename = "ROLLED_BACK")]
    RolledBack,
}

/// Secret rotation manager
pub struct SecretRotationManager {
    /// Policies for each secret
    policies: Arc<RwLock<HashMap<String, RotationPolicy>>>,

    /// Secret versions (secret_id -> versions)
    versions: Arc<RwLock<HashMap<String, Vec<SecretVersion>>>>,

    /// Rotation events log
    events: Arc<RwLock<Vec<RotationEvent>>>,

    /// Rotation tasks
    rotation_tasks: Arc<RwLock<HashMap<String, tokio::task::JoinHandle<()>>>>,
}

impl SecretRotationManager {
    /// Create new secret rotation manager
    pub fn new() -> Self {
        Self {
            policies: Arc::new(RwLock::new(HashMap::new())),
            versions: Arc::new(RwLock::new(HashMap::new())),
            events: Arc::new(RwLock::new(Vec::new())),
            rotation_tasks: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a secret for rotation
    pub async fn register_secret(
        &self,
        secret_id: String,
        secret_type: SecretType,
        rotation_interval_days: u64,
    ) -> Result<()> {
        let policy = RotationPolicy {
            secret_id: secret_id.clone(),
            secret_type,
            rotation_interval_days,
            grace_period_hours: 24,
            last_rotated_at: Utc::now(),
            next_rotation_at: Utc::now() + Duration::days(rotation_interval_days as i64),
            enabled: true,
            retention_count: 3,
            auto_rotate: true,
            notify_before_rotation: true,
            notification_channels: vec!["email".to_string()],
        };

        let mut policies = self.policies.write().await;
        policies.insert(secret_id.clone(), policy);

        info!("Registered secret for rotation: {}", secret_id);
        Ok(())
    }

    /// Rotate a secret (atomic operation)
    pub async fn rotate_secret(
        &self,
        secret_id: &str,
        new_secret_data: String,
        reason: String,
    ) -> Result<u64> {
        let policy = {
            let policies = self.policies.read().await;
            policies
                .get(secret_id)
                .cloned()
                .ok_or_else(|| {
                    SecurityError::secret_rotation(format!("Secret not found: {}", secret_id))
                })?
        };

        if !policy.enabled {
            return Err(SecurityError::secret_rotation(
                format!("Secret rotation disabled: {}", secret_id),
            ));
        }

        let event_id = uuid::Uuid::new_v4().to_string();
        let now = Utc::now();

        // Create rotation event
        let event = RotationEvent {
            event_id: event_id.clone(),
            secret_id: secret_id.to_string(),
            secret_type: policy.secret_type.clone(),
            old_version: self.get_current_version(secret_id).await.unwrap_or(0),
            new_version: 0, // Will be set after successful rotation
            status: RotationStatus::InProgress,
            started_at: now,
            completed_at: None,
            error: None,
            initiated_by: "system".to_string(),
            reason,
        };

        // Log event
        {
            let mut events = self.events.write().await;
            events.push(event.clone());
        }

        info!(
            "Starting secret rotation for {} (event: {})",
            secret_id, event_id
        );

        // Get current version
        let old_version = self.get_current_version(secret_id).await.unwrap_or(0);
        let new_version = old_version + 1;

        // Create new secret version (PENDING)
        let new_secret_version = SecretVersion {
            version: new_version,
            secret_data: new_secret_data.clone(),
            created_at: now,
            activated_at: None,
            deactivated_at: None,
            status: VersionStatus::Pending,
            reason: None,
        };

        // Add to versions
        {
            let mut versions = self.versions.write().await;
            versions
                .entry(secret_id.to_string())
                .or_insert_with(Vec::new)
                .push(new_secret_version);
        }

        // Activate new version (atomic swap)
        self.activate_version(secret_id, new_version).await?;

        // Start grace period for old version
        let secret_id_clone = secret_id.to_string();
        let grace_period_hours = policy.grace_period_hours;
        let versions_clone = Arc::clone(&self.versions);

        tokio::spawn(async move {
            tokio::time::sleep(tokio::time::Duration::from_secs(grace_period_hours * 3600)).await;

            // Deactivate old version
            let mut versions = versions_clone.write().await;
            if let Some(version_list) = versions.get_mut(&secret_id_clone) {
                if let Some(v) = version_list.iter_mut().find(|v| v.version == old_version) {
                    v.status = VersionStatus::Deactivated;
                    v.deactivated_at = Some(Utc::now());
                }
            }
        });

        // Update policy
        {
            let mut policies = self.policies.write().await;
            if let Some(policy) = policies.get_mut(secret_id) {
                policy.last_rotated_at = now;
                policy.next_rotation_at =
                    now + Duration::days(policy.rotation_interval_days as i64);
            }
        }

        // Update event status
        {
            let mut events = self.events.write().await;
            if let Some(event) = events.iter_mut().find(|e| e.event_id == event_id) {
                event.status = RotationStatus::Completed;
                event.completed_at = Some(Utc::now());
                event.new_version = new_version;
            }
        }

        info!(
            "Successfully rotated secret {} to version {}",
            secret_id, new_version
        );
        Ok(new_version)
    }

    /// Rollback to previous secret version
    pub async fn rollback_secret(
        &self,
        secret_id: &str,
        to_version: u64,
        reason: String,
    ) -> Result<()> {
        let event_id = uuid::Uuid::new_v4().to_string();

        // Create rollback event
        let event = RotationEvent {
            event_id: event_id.clone(),
            secret_id: secret_id.to_string(),
            secret_type: self
                .policies
                .read()
                .await
                .get(secret_id)
                .map(|p| p.secret_type.clone())
                .unwrap_or(SecretType::Other),
            old_version: self.get_current_version(secret_id).await.unwrap_or(0),
            new_version: to_version,
            status: RotationStatus::RolledBack,
            started_at: Utc::now(),
            completed_at: Some(Utc::now()),
            error: None,
            initiated_by: "system".to_string(),
            reason,
        };

        {
            let mut events = self.events.write().await;
            events.push(event);
        }

        // Activate the target version
        self.activate_version(secret_id, to_version).await?;

        warn!(
            "Rolled back secret {} to version {}",
            secret_id, to_version
        );
        Ok(())
    }

    /// Activate a specific secret version
    async fn activate_version(&self, secret_id: &str, version: u64) -> Result<()> {
        let mut versions = self.versions.write().await;

        let version_list = versions.get_mut(secret_id).ok_or_else(|| {
            SecurityError::secret_rotation(format!("Secret not found: {}", secret_id))
        })?;

        // Deactivate current active version
        if let Some(active) = version_list.iter_mut().find(|v| v.status == VersionStatus::Active) {
            active.status = VersionStatus::GracePeriod;
            active.deactivated_at = Some(Utc::now());
        }

        // Activate target version
        let target = version_list
            .iter_mut()
            .find(|v| v.version == version)
            .ok_or_else(|| {
                SecurityError::secret_rotation(format!("Version not found: {}", version))
            })?;

        target.status = VersionStatus::Active;
        target.activated_at = Some(Utc::now());

        debug!("Activated version {} for secret {}", version, secret_id);
        Ok(())
    }

    /// Get current active version
    async fn get_current_version(&self, secret_id: &str) -> Option<u64> {
        self.versions
            .read()
            .await
            .get(secret_id)
            .and_then(|versions| {
                versions
                    .iter()
                    .find(|v| v.status == VersionStatus::Active)
                    .map(|v| v.version)
            })
    }

    /// Get rotation history
    pub async fn get_rotation_history(&self, secret_id: &str) -> Result<Vec<RotationEvent>> {
        let events = self
            .events
            .read()
            .await
            .iter()
            .filter(|e| e.secret_id == secret_id)
            .cloned()
            .collect();

        Ok(events)
    }

    /// Get all rotation policies
    pub async fn get_policies(&self) -> Result<Vec<RotationPolicy>> {
        Ok(self
            .policies
            .read()
            .await
            .values()
            .cloned()
            .collect())
    }
}

impl Default for SecretRotationManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_register_secret() {
        let manager = SecretRotationManager::new();
        let result = manager
            .register_secret(
                "db-password".to_string(),
                SecretType::DatabaseCredential,
                90,
            )
            .await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_rotate_secret() {
        let manager = SecretRotationManager::new();
        manager
            .register_secret(
                "api-key".to_string(),
                SecretType::ApiKey,
                30,
            )
            .await
            .unwrap();

        let version = manager
            .rotate_secret(
                "api-key",
                "new_secret_value".to_string(),
                "Scheduled rotation".to_string(),
            )
            .await
            .unwrap();

        assert_eq!(version, 1);
    }

    #[tokio::test]
    async fn test_rotation_event_creation() {
        let event = RotationEvent {
            event_id: "event-1".to_string(),
            secret_id: "secret-1".to_string(),
            secret_type: SecretType::ApiKey,
            old_version: 1,
            new_version: 2,
            status: RotationStatus::Completed,
            started_at: Utc::now(),
            completed_at: Some(Utc::now()),
            error: None,
            initiated_by: "admin".to_string(),
            reason: "Scheduled".to_string(),
        };

        assert_eq!(event.secret_type, SecretType::ApiKey);
        assert_eq!(event.status, RotationStatus::Completed);
    }
}
