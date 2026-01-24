//! TLS 1.3 enforcement and certificate management

mod config;
mod error;
mod pinning;
mod validator;
mod pool;
mod hsts;

pub use config::{TlsConfig, TlsConfigBuilder, CipherSuite};
pub use error::{TlsError, TlsResult};
pub use pinning::{CertificatePin, PinningStrategy};
pub use validator::{CertificateValidator, ValidationPolicy};
pub use pool::{ConnectionPool, PoolConfig};
pub use hsts::{HstsPolicy, HstsMiddleware};

use std::sync::Arc;
use tokio_rustls::TlsConnector as RustlsConnector;

/// High-level TLS connector with security enforcement
#[derive(Clone)]
pub struct TlsConnector {
    inner: RustlsConnector,
    config: Arc<TlsConfig>,
    validator: Arc<CertificateValidator>,
    pins: Arc<Vec<CertificatePin>>,
}

impl TlsConnector {
    /// Create a new TLS connector with security policies
    ///
    /// # Errors
    /// Returns `TlsError` if configuration is invalid or certificates cannot be loaded
    pub fn new(config: TlsConfig) -> TlsResult<Self> {
        let validator = CertificateValidator::new(config.validation_policy.clone());
        let pins = config.certificate_pins.clone();

        let tls_config = config.build_rustls_config()?;
        let inner = RustlsConnector::from(Arc::new(tls_config));

        Ok(Self {
            inner,
            config: Arc::new(config),
            validator: Arc::new(validator),
            pins: Arc::new(pins),
        })
    }

    /// Get the underlying rustls connector
    #[must_use]
    pub fn inner(&self) -> &RustlsConnector {
        &self.inner
    }

    /// Get the TLS configuration
    #[must_use]
    pub fn config(&self) -> &TlsConfig {
        &self.config
    }

    /// Get the certificate validator
    #[must_use]
    pub fn validator(&self) -> &CertificateValidator {
        &self.validator
    }

    /// Get the certificate pins
    #[must_use]
    pub fn pins(&self) -> &[CertificatePin] {
        &self.pins
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tls_connector_creation() {
        // Arrange
        let config = TlsConfigBuilder::default()
            .build()
            .expect("Failed to build TLS config");

        // Act
        let connector = TlsConnector::new(config);

        // Assert
        assert!(connector.is_ok(), "TLS connector should be created successfully");
    }

    #[test]
    fn test_tls_connector_config_access() {
        // Arrange
        let config = TlsConfigBuilder::default()
            .build()
            .expect("Failed to build TLS config");
        let connector = TlsConnector::new(config)
            .expect("Failed to create connector");

        // Act
        let retrieved_config = connector.config();

        // Assert
        assert!(!retrieved_config.cipher_suites.is_empty(), "Config should have cipher suites");
    }
}
