//! TLS configuration and cipher suite management

use super::error::{TlsError, TlsResult};
use super::pinning::{CertificatePin, PinningStrategy};
use super::validator::ValidationPolicy;
use rustls::{ClientConfig, RootCertStore, SupportedCipherSuite};
use std::sync::Arc;

/// Supported cipher suites (TLS 1.3 only)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CipherSuite {
    /// ChaCha20-Poly1305 (IETF)
    ChaCha20Poly1305,
    /// AES-256-GCM
    Aes256Gcm,
    /// AES-128-GCM
    Aes128Gcm,
}

impl CipherSuite {
    /// Convert to rustls cipher suite
    #[must_use]
    pub fn to_rustls(self) -> SupportedCipherSuite {
        match self {
            CipherSuite::ChaCha20Poly1305 => rustls::cipher_suite::TLS13_CHACHA20_POLY1305_SHA256,
            CipherSuite::Aes256Gcm => rustls::cipher_suite::TLS13_AES_256_GCM_SHA384,
            CipherSuite::Aes128Gcm => rustls::cipher_suite::TLS13_AES_128_GCM_SHA256,
        }
    }

    /// Get cipher suite name
    #[must_use]
    pub fn name(self) -> &'static str {
        match self {
            CipherSuite::ChaCha20Poly1305 => "TLS13_CHACHA20_POLY1305_SHA256",
            CipherSuite::Aes256Gcm => "TLS13_AES_256_GCM_SHA384",
            CipherSuite::Aes128Gcm => "TLS13_AES_128_GCM_SHA256",
        }
    }
}

/// TLS configuration
#[derive(Clone)]
pub struct TlsConfig {
    /// Allowed cipher suites (TLS 1.3 only)
    pub cipher_suites: Vec<CipherSuite>,
    /// Certificate pins for known hosts
    pub certificate_pins: Vec<CertificatePin>,
    /// Certificate validation policy
    pub validation_policy: ValidationPolicy,
    /// Enable OCSP stapling
    pub enable_ocsp_stapling: bool,
    /// Enable SNI (Server Name Indication)
    pub enable_sni: bool,
    /// Disable TLS compression (CRIME attack prevention)
    pub disable_compression: bool,
    /// Root certificate store
    pub root_cert_store: Option<RootCertStore>,
    /// Pinning strategy
    pub pinning_strategy: PinningStrategy,
}

impl TlsConfig {
    /// Build rustls client configuration
    ///
    /// # Errors
    /// Returns `TlsError` if configuration is invalid
    pub fn build_rustls_config(&self) -> TlsResult<ClientConfig> {
        let mut config = ClientConfig::builder()
            .with_cipher_suites(
                &self
                    .cipher_suites
                    .iter()
                    .map(|cs| cs.to_rustls())
                    .collect::<Vec<_>>(),
            )
            .with_safe_default_kx_groups()
            .with_protocol_versions(&[&rustls::version::TLS13])
            .map_err(|e| TlsError::ConfigError(e.to_string()))?
            .with_root_certificates(self.root_cert_store.clone().unwrap_or_else(|| {
                let mut store = RootCertStore::empty();
                store.add_trust_anchors(webpki_roots::TLS_SERVER_ROOTS.iter().map(|ta| {
                    rustls::OwnedTrustAnchor::from_subject_spki_name_constraints(
                        ta.subject,
                        ta.spki,
                        ta.name_constraints,
                    )
                }));
                store
            }))
            .with_no_client_auth();

        // Disable session resumption for maximum security (can be enabled later for performance)
        config.resumption = rustls::client::Resumption::disabled();

        // Enable SNI if configured
        if !self.enable_sni {
            config.enable_sni = false;
        }

        Ok(config)
    }
}

/// Builder for TLS configuration
#[derive(Default)]
pub struct TlsConfigBuilder {
    cipher_suites: Option<Vec<CipherSuite>>,
    certificate_pins: Vec<CertificatePin>,
    validation_policy: Option<ValidationPolicy>,
    enable_ocsp_stapling: bool,
    enable_sni: bool,
    disable_compression: bool,
    root_cert_store: Option<RootCertStore>,
    pinning_strategy: Option<PinningStrategy>,
}

impl TlsConfigBuilder {
    /// Create a new builder with secure defaults
    #[must_use]
    pub fn new() -> Self {
        Self {
            cipher_suites: Some(vec![CipherSuite::ChaCha20Poly1305, CipherSuite::Aes256Gcm]),
            certificate_pins: Vec::new(),
            validation_policy: Some(ValidationPolicy::default()),
            enable_ocsp_stapling: true,
            enable_sni: true,
            disable_compression: true,
            root_cert_store: None,
            pinning_strategy: Some(PinningStrategy::FailOpen),
        }
    }

    /// Set cipher suites
    #[must_use]
    pub fn cipher_suites(mut self, suites: Vec<CipherSuite>) -> Self {
        self.cipher_suites = Some(suites);
        self
    }

    /// Add a certificate pin
    #[must_use]
    pub fn add_certificate_pin(mut self, pin: CertificatePin) -> Self {
        self.certificate_pins.push(pin);
        self
    }

    /// Set validation policy
    #[must_use]
    pub fn validation_policy(mut self, policy: ValidationPolicy) -> Self {
        self.validation_policy = Some(policy);
        self
    }

    /// Enable OCSP stapling
    #[must_use]
    pub fn enable_ocsp_stapling(mut self, enable: bool) -> Self {
        self.enable_ocsp_stapling = enable;
        self
    }

    /// Enable SNI
    #[must_use]
    pub fn enable_sni(mut self, enable: bool) -> Self {
        self.enable_sni = enable;
        self
    }

    /// Set pinning strategy
    #[must_use]
    pub fn pinning_strategy(mut self, strategy: PinningStrategy) -> Self {
        self.pinning_strategy = Some(strategy);
        self
    }

    /// Build the configuration
    ///
    /// # Errors
    /// Returns `TlsError` if configuration is invalid
    pub fn build(self) -> TlsResult<TlsConfig> {
        let cipher_suites = self
            .cipher_suites
            .ok_or_else(|| TlsError::ConfigError("Cipher suites must be specified".to_string()))?;

        if cipher_suites.is_empty() {
            return Err(TlsError::ConfigError(
                "At least one cipher suite must be specified".to_string(),
            ));
        }

        Ok(TlsConfig {
            cipher_suites,
            certificate_pins: self.certificate_pins,
            validation_policy: self.validation_policy.unwrap_or_default(),
            enable_ocsp_stapling: self.enable_ocsp_stapling,
            enable_sni: self.enable_sni,
            disable_compression: self.disable_compression,
            root_cert_store: self.root_cert_store,
            pinning_strategy: self.pinning_strategy.unwrap_or(PinningStrategy::FailOpen),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cipher_suite_conversion() {
        // Arrange
        let cipher = CipherSuite::ChaCha20Poly1305;

        // Act
        let rustls_cipher = cipher.to_rustls();
        let name = cipher.name();

        // Assert
        assert_eq!(
            rustls_cipher.suite(),
            rustls::CipherSuite::TLS13_CHACHA20_POLY1305_SHA256
        );
        assert_eq!(name, "TLS13_CHACHA20_POLY1305_SHA256");
    }

    #[test]
    fn test_config_builder_defaults() {
        // Arrange & Act
        let config = TlsConfigBuilder::default().build();

        // Assert
        assert!(config.is_ok(), "Default config should build successfully");
        let config = config.expect("Config should be valid");
        assert!(
            !config.cipher_suites.is_empty(),
            "Should have default cipher suites"
        );
        assert!(
            config.enable_ocsp_stapling,
            "OCSP should be enabled by default"
        );
        assert!(config.enable_sni, "SNI should be enabled by default");
        assert!(
            config.disable_compression,
            "Compression should be disabled by default"
        );
    }

    #[test]
    fn test_config_builder_custom_ciphers() {
        // Arrange
        let ciphers = vec![CipherSuite::Aes256Gcm];

        // Act
        let config = TlsConfigBuilder::new()
            .cipher_suites(ciphers.clone())
            .build();

        // Assert
        assert!(config.is_ok(), "Custom cipher config should build");
        let config = config.expect("Config should be valid");
        assert_eq!(
            config.cipher_suites.len(),
            1,
            "Should have one cipher suite"
        );
        assert_eq!(
            config.cipher_suites[0],
            CipherSuite::Aes256Gcm,
            "Should have custom cipher"
        );
    }

    #[test]
    fn test_config_builder_empty_ciphers_fails() {
        // Arrange
        let builder = TlsConfigBuilder::new().cipher_suites(vec![]);

        // Act
        let result = builder.build();

        // Assert
        assert!(result.is_err(), "Empty cipher suites should fail");
        assert!(
            matches!(result, Err(TlsError::ConfigError(_))),
            "Should be config error"
        );
    }

    #[test]
    fn test_rustls_config_creation() {
        // Arrange
        let config = TlsConfigBuilder::new()
            .build()
            .expect("Failed to build config");

        // Act
        let rustls_config = config.build_rustls_config();

        // Assert
        assert!(rustls_config.is_ok(), "Rustls config should be created");
    }

    #[test]
    fn test_config_with_pins() {
        // Arrange
        let pin = CertificatePin::new("example.com".to_string(), vec![0x1, 0x2, 0x3, 0x4]);

        // Act
        let config = TlsConfigBuilder::new().add_certificate_pin(pin).build();

        // Assert
        assert!(config.is_ok(), "Config with pins should build");
        let config = config.expect("Config should be valid");
        assert_eq!(config.certificate_pins.len(), 1, "Should have one pin");
    }

    #[test]
    fn test_config_sni_disabled() {
        // Arrange & Act
        let config = TlsConfigBuilder::new().enable_sni(false).build();

        // Assert
        assert!(config.is_ok(), "Config should build");
        let config = config.expect("Config should be valid");
        assert!(!config.enable_sni, "SNI should be disabled");
    }
}
