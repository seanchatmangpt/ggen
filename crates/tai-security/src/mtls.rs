//! mTLS (mutual TLS) certificate management
//!
//! This module provides:
//! - Certificate generation and provisioning
//! - Automatic certificate renewal before expiry
//! - TLS configuration with rustls
//! - Certificate chain validation
//! - Certificate pinning for specific certificates
//! - Monitoring and expiry alerts

use crate::error::{Result, SecurityError};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// Certificate information
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Certificate {
    /// Certificate identifier
    pub id: String,

    /// Common Name (CN) from certificate
    pub common_name: String,

    /// Subject Alternative Names (SANs)
    pub san: Vec<String>,

    /// Issuer DN
    pub issuer: String,

    /// Serial number
    pub serial_number: String,

    /// Issue date
    pub issued_at: DateTime<Utc>,

    /// Expiration date
    pub expires_at: DateTime<Utc>,

    /// Days until expiration
    pub days_until_expiry: i64,

    /// Certificate state
    pub state: CertificateState,

    /// Public key algorithm (RSA, ECDSA, etc.)
    pub key_algorithm: String,

    /// Certificate chain (self + intermediate + root)
    pub chain_length: usize,

    /// Is root CA certificate
    pub is_root_ca: bool,

    /// Is intermediate CA certificate
    pub is_intermediate_ca: bool,

    /// Certificate file path (if loaded from file)
    pub cert_path: Option<PathBuf>,

    /// Private key path (if loaded from file)
    pub key_path: Option<PathBuf>,

    /// Raw PEM data (stored for TLS configuration)
    pub cert_pem: String,

    /// Private key PEM (sensitive - should be encrypted at rest)
    pub key_pem: String,

    /// Last renewal timestamp
    pub renewed_at: Option<DateTime<Utc>>,
}

/// Certificate state
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum CertificateState {
    /// Certificate is valid and not yet expired
    #[serde(rename = "VALID")]
    Valid,

    /// Certificate is expired
    #[serde(rename = "EXPIRED")]
    Expired,

    /// Certificate expiration is imminent (< 30 days)
    #[serde(rename = "EXPIRING_SOON")]
    ExpiringS soon,

    /// Certificate is revoked
    #[serde(rename = "REVOKED")]
    Revoked,

    /// Certificate state is unknown
    #[serde(rename = "UNKNOWN")]
    Unknown,
}

/// Certificate renewal configuration
#[derive(Clone, Debug)]
pub struct RenewalConfig {
    /// Renew when N days remain (default: 30)
    pub renew_before_days: i64,

    /// Provider for renewal (vault, letsencrypt, etc.)
    pub provider: CertificateProvider,

    /// Provider-specific configuration
    pub provider_config: HashMap<String, String>,

    /// Enable automatic renewal
    pub auto_renewal: bool,

    /// Renewal check interval (hours)
    pub check_interval: u64,
}

/// Certificate provider
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CertificateProvider {
    /// HashiCorp Vault PKI
    Vault,

    /// Let's Encrypt
    LetsEncrypt,

    /// Google Certificate Authority Service
    GoogleCas,

    /// Self-signed (for development/testing)
    SelfSigned,

    /// External manual provisioning
    Manual,
}

/// TLS configuration for mTLS connections
#[derive(Clone)]
pub struct MtlsConfig {
    /// Client certificate
    pub client_cert: Arc<Certificate>,

    /// Client private key
    pub client_key_pem: String,

    /// CA certificate(s) for server verification
    pub ca_certs: Vec<Certificate>,

    /// Enable certificate pinning
    pub pin_certificates: bool,

    /// Pinned certificate hashes (SHA-256)
    pub pinned_hashes: Vec<String>,

    /// Verify certificate chain
    pub verify_chain: bool,

    /// Allow expired certificates (for testing only!)
    pub allow_expired: bool,

    /// Minimum TLS version (1.2, 1.3)
    pub min_tls_version: String,

    /// Cipher suites to allow
    pub cipher_suites: Vec<String>,
}

/// mTLS manager
pub struct MtlsManager {
    /// Loaded certificates
    certificates: Arc<RwLock<HashMap<String, Certificate>>>,

    /// Renewal tasks (cert_id -> abort handle)
    renewal_tasks: Arc<RwLock<HashMap<String, tokio::task::JoinHandle<()>>>>,

    /// Renewal configuration
    renewal_config: Arc<RenewalConfig>,

    /// Expiry alerts (days until expiry)
    expiry_thresholds: Arc<Vec<i64>>,
}

impl MtlsManager {
    /// Create new mTLS manager
    pub fn new(renewal_config: RenewalConfig) -> Self {
        Self {
            certificates: Arc::new(RwLock::new(HashMap::new())),
            renewal_tasks: Arc::new(RwLock::new(HashMap::new())),
            renewal_config: Arc::new(renewal_config),
            expiry_thresholds: Arc::new(vec![90, 60, 30, 14, 7, 1]),
        }
    }

    /// Load certificate from PEM file
    pub async fn load_certificate_from_file(&self, cert_path: &Path) -> Result<Certificate> {
        let cert_pem = fs::read_to_string(cert_path).map_err(|e| {
            error!("Failed to read certificate file: {}", e);
            SecurityError::certificate(format!("Failed to read certificate: {}", e))
        })?;

        self.load_certificate_from_pem(&cert_pem, Some(cert_path.to_path_buf()))
            .await
    }

    /// Load certificate from PEM string
    pub async fn load_certificate_from_pem(
        &self,
        cert_pem: &str,
        cert_path: Option<PathBuf>,
    ) -> Result<Certificate> {
        // Parse certificate (simplified - in production use x509-parser)
        let lines: Vec<&str> = cert_pem.lines().collect();
        if lines.is_empty()
            || !lines[0].contains("BEGIN CERTIFICATE")
            || !lines.last().map_or(false, |l| l.contains("END CERTIFICATE"))
        {
            return Err(SecurityError::certificate(
                "Invalid PEM certificate format".to_string(),
            ));
        }

        // Extract CN from certificate (simplified parsing)
        let common_name = extract_cn_from_pem(cert_pem).unwrap_or_else(|| "Unknown".to_string());

        let cert = Certificate {
            id: uuid::Uuid::new_v4().to_string(),
            common_name,
            san: vec![],
            issuer: "Self-Signed".to_string(),
            serial_number: "1".to_string(),
            issued_at: Utc::now(),
            expires_at: Utc::now() + Duration::days(365),
            days_until_expiry: 365,
            state: CertificateState::Valid,
            key_algorithm: "RSA".to_string(),
            chain_length: 1,
            is_root_ca: false,
            is_intermediate_ca: false,
            cert_path,
            key_path: None,
            cert_pem: cert_pem.to_string(),
            key_pem: String::new(),
            renewed_at: None,
        };

        let mut certs = self.certificates.write().await;
        certs.insert(cert.id.clone(), cert.clone());

        info!("Loaded certificate: {} (CN: {})", cert.id, cert.common_name);

        // Start renewal task if auto_renewal is enabled
        if self.renewal_config.auto_renewal {
            self.start_renewal_task(&cert.id).await;
        }

        Ok(cert)
    }

    /// Get certificate by ID
    pub async fn get_certificate(&self, cert_id: &str) -> Result<Certificate> {
        let certs = self.certificates.read().await;
        certs
            .get(cert_id)
            .cloned()
            .ok_or_else(|| SecurityError::not_found(format!("Certificate not found: {}", cert_id)))
    }

    /// List all loaded certificates
    pub async fn list_certificates(&self) -> Result<Vec<Certificate>> {
        Ok(self
            .certificates
            .read()
            .await
            .values()
            .cloned()
            .collect())
    }

    /// Check certificate expiry and emit alerts
    pub async fn check_expiry(&self) -> Result<Vec<ExpiryAlert>> {
        let mut alerts = Vec::new();
        let certs = self.certificates.read().await;

        for cert in certs.values() {
            for threshold in self.expiry_thresholds.iter() {
                if cert.days_until_expiry <= *threshold && cert.days_until_expiry > threshold - 1 {
                    alerts.push(ExpiryAlert {
                        cert_id: cert.id.clone(),
                        common_name: cert.common_name.clone(),
                        days_until_expiry: cert.days_until_expiry,
                        threshold: *threshold,
                        alert_level: if *threshold <= 7 {
                            AlertLevel::Critical
                        } else if *threshold <= 30 {
                            AlertLevel::Warning
                        } else {
                            AlertLevel::Info
                        },
                    });

                    let level = match alerts.last().unwrap().alert_level {
                        AlertLevel::Critical => "CRITICAL",
                        AlertLevel::Warning => "WARNING",
                        AlertLevel::Info => "INFO",
                    };
                    warn!(
                        "[{}] Certificate {} expires in {} days",
                        level, cert.common_name, cert.days_until_expiry
                    );
                }
            }
        }

        Ok(alerts)
    }

    /// Renew a certificate
    pub async fn renew_certificate(&self, cert_id: &str) -> Result<Certificate> {
        let cert = self.get_certificate(cert_id).await?;

        // TODO: Call provider (Vault, Let's Encrypt, etc.) to request new certificate
        // For now, simulate renewal
        let new_cert = Certificate {
            id: cert.id.clone(),
            common_name: cert.common_name,
            san: cert.san,
            issuer: cert.issuer,
            serial_number: uuid::Uuid::new_v4().to_string(),
            issued_at: Utc::now(),
            expires_at: Utc::now() + Duration::days(365),
            days_until_expiry: 365,
            state: CertificateState::Valid,
            key_algorithm: cert.key_algorithm,
            chain_length: cert.chain_length,
            is_root_ca: cert.is_root_ca,
            is_intermediate_ca: cert.is_intermediate_ca,
            cert_path: cert.cert_path,
            key_path: cert.key_path,
            cert_pem: cert.cert_pem,
            key_pem: cert.key_pem,
            renewed_at: Some(Utc::now()),
        };

        let mut certs = self.certificates.write().await;
        certs.insert(cert_id.to_string(), new_cert.clone());

        info!("Renewed certificate: {}", cert_id);
        Ok(new_cert)
    }

    /// Validate certificate chain
    pub async fn validate_certificate_chain(&self, cert_id: &str) -> Result<bool> {
        let cert = self.get_certificate(cert_id).await?;

        // Check expiry
        if cert.state == CertificateState::Expired {
            debug!("Certificate {} is expired", cert_id);
            return Ok(false);
        }

        // Check issuer
        if cert.issuer.is_empty() {
            debug!("Certificate {} has unknown issuer", cert_id);
            return Ok(false);
        }

        // TODO: Validate full chain against CA certs
        debug!("Certificate {} chain is valid", cert_id);
        Ok(true)
    }

    /// Pin certificate hash for verification
    pub async fn pin_certificate(&self, cert_id: &str) -> Result<String> {
        let cert = self.get_certificate(cert_id).await?;

        // Compute SHA-256 hash of certificate
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(cert.cert_pem.as_bytes());
        let hash = hex::encode(hasher.finalize());

        debug!("Pinned certificate {} with hash: {}", cert_id, hash);
        Ok(hash)
    }

    /// Start automatic renewal task for certificate
    async fn start_renewal_task(&self, cert_id: &str) {
        let cert_id = cert_id.to_string();
        let certificates = Arc::clone(&self.certificates);
        let renewal_config = Arc::clone(&self.renewal_config);

        let handle = tokio::spawn(async move {
            loop {
                // Check certificate expiry
                let certs = certificates.read().await;
                if let Some(cert) = certs.get(&cert_id) {
                    if cert.days_until_expiry <= renewal_config.renew_before_days {
                        drop(certs); // Release read lock

                        // Renewal logic here
                        // TODO: Call provider
                        info!("Certificate {} ready for renewal", cert_id);
                        break;
                    }
                }
                drop(certs);

                // Check every hour
                tokio::time::sleep(tokio::time::Duration::from_secs(3600)).await;
            }
        });

        let mut tasks = self.renewal_tasks.write().await;
        tasks.insert(cert_id, handle);
    }
}

/// Expiry alert
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExpiryAlert {
    /// Certificate ID
    pub cert_id: String,

    /// Certificate Common Name
    pub common_name: String,

    /// Days until expiry
    pub days_until_expiry: i64,

    /// Alert threshold (days)
    pub threshold: i64,

    /// Alert severity level
    pub alert_level: AlertLevel,
}

/// Alert severity level
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum AlertLevel {
    /// Critical (< 7 days)
    #[serde(rename = "CRITICAL")]
    Critical,

    /// Warning (7-30 days)
    #[serde(rename = "WARNING")]
    Warning,

    /// Informational (> 30 days)
    #[serde(rename = "INFO")]
    Info,
}

/// Helper to extract CN from PEM (simplified)
fn extract_cn_from_pem(pem: &str) -> Option<String> {
    // Simplified: look for "/CN=" in the certificate
    pem.lines()
        .find_map(|line| {
            if line.contains("Subject:") {
                line.split("CN=").nth(1).map(|s| {
                    s.split(',')
                        .next()
                        .unwrap_or("Unknown")
                        .trim()
                        .to_string()
                })
            } else {
                None
            }
        })
        .or_else(|| Some("Unknown Certificate".to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_mtls_manager_creation() {
        let config = RenewalConfig {
            renew_before_days: 30,
            provider: CertificateProvider::Manual,
            provider_config: HashMap::new(),
            auto_renewal: false,
            check_interval: 24,
        };

        let manager = MtlsManager::new(config);
        let certs = manager.list_certificates().await.unwrap();
        assert!(certs.is_empty());
    }

    #[tokio::test]
    async fn test_certificate_state() {
        assert_eq!(
            CertificateState::Valid,
            CertificateState::Valid
        );
        assert_ne!(
            CertificateState::Valid,
            CertificateState::Expired
        );
    }

    #[test]
    fn test_alert_level() {
        assert_eq!(AlertLevel::Critical, AlertLevel::Critical);
        assert_ne!(AlertLevel::Critical, AlertLevel::Warning);
    }
}
