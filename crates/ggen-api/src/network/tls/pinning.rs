//! Certificate pinning for known hosts

use super::error::{TlsError, TlsResult};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// Certificate pinning strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinningStrategy {
    /// Fail open if pin validation fails (log warning but allow connection)
    FailOpen,
    /// Fail closed if pin validation fails (reject connection)
    FailClosed,
}

/// Certificate pin for a specific host
#[derive(Debug, Clone)]
pub struct CertificatePin {
    /// Hostname this pin applies to
    hostname: String,
    /// SHA-256 hash of the certificate's Subject Public Key Info (SPKI)
    spki_hash: Vec<u8>,
}

impl CertificatePin {
    /// Create a new certificate pin
    #[must_use]
    pub fn new(hostname: String, spki_hash: Vec<u8>) -> Self {
        Self {
            hostname,
            spki_hash,
        }
    }

    /// Create a pin from a base64-encoded hash
    ///
    /// # Errors
    /// Returns `TlsError` if base64 decoding fails
    pub fn from_base64(hostname: String, hash_b64: &str) -> TlsResult<Self> {
        let spki_hash = base64::decode(hash_b64)
            .map_err(|e| TlsError::CertificateParseError(format!("Invalid base64: {e}")))?;
        Ok(Self::new(hostname, spki_hash))
    }

    /// Get the hostname
    #[must_use]
    pub fn hostname(&self) -> &str {
        &self.hostname
    }

    /// Get the SPKI hash
    #[must_use]
    pub fn spki_hash(&self) -> &[u8] {
        &self.spki_hash
    }

    /// Verify a certificate against this pin
    ///
    /// # Errors
    /// Returns `TlsError` if the certificate doesn't match the pin
    pub fn verify(&self, cert_spki: &[u8]) -> TlsResult<()> {
        let computed_hash = Sha256::digest(cert_spki);

        if computed_hash.as_slice() == self.spki_hash {
            Ok(())
        } else {
            Err(TlsError::PinningFailed(format!(
                "Certificate pin mismatch for {}",
                self.hostname
            )))
        }
    }
}

/// Certificate pinning manager
#[derive(Default)]
pub struct PinningManager {
    pins: HashMap<String, Vec<CertificatePin>>,
    strategy: PinningStrategy,
}

impl PinningManager {
    /// Create a new pinning manager
    #[must_use]
    pub fn new(strategy: PinningStrategy) -> Self {
        Self {
            pins: HashMap::new(),
            strategy,
        }
    }

    /// Add a certificate pin
    pub fn add_pin(&mut self, pin: CertificatePin) {
        self.pins
            .entry(pin.hostname.clone())
            .or_insert_with(Vec::new)
            .push(pin);
    }

    /// Add multiple pins
    pub fn add_pins(&mut self, pins: Vec<CertificatePin>) {
        for pin in pins {
            self.add_pin(pin);
        }
    }

    /// Verify a certificate against known pins
    ///
    /// # Errors
    /// Returns `TlsError` if verification fails and strategy is `FailClosed`
    pub fn verify(&self, hostname: &str, cert_spki: &[u8]) -> TlsResult<()> {
        if let Some(pins) = self.pins.get(hostname) {
            // Try all pins for this hostname
            let any_match = pins.iter().any(|pin| pin.verify(cert_spki).is_ok());

            if !any_match {
                match self.strategy {
                    PinningStrategy::FailOpen => {
                        tracing::warn!(
                            hostname = %hostname,
                            "Certificate pin verification failed, but allowing connection (fail-open)"
                        );
                        Ok(())
                    }
                    PinningStrategy::FailClosed => Err(TlsError::PinningFailed(format!(
                        "No matching certificate pin for {hostname}"
                    ))),
                }
            } else {
                Ok(())
            }
        } else {
            // No pins configured for this hostname
            Ok(())
        }
    }

    /// Get all pins for a hostname
    #[must_use]
    pub fn get_pins(&self, hostname: &str) -> Option<&[CertificatePin]> {
        self.pins.get(hostname).map(|v| v.as_slice())
    }

    /// Check if a hostname has pins
    #[must_use]
    pub fn has_pins(&self, hostname: &str) -> bool {
        self.pins.contains_key(hostname)
    }
}

// Base64 encoding/decoding utilities
mod base64 {
    use super::TlsError;

    pub fn decode(input: &str) -> Result<Vec<u8>, TlsError> {
        // Simple base64 decoder (in production, use base64 crate)
        let bytes = input.as_bytes();
        let mut result = Vec::new();
        let mut buffer = 0u32;
        let mut bits = 0;

        for &byte in bytes {
            let value = match byte {
                b'A'..=b'Z' => byte - b'A',
                b'a'..=b'z' => byte - b'a' + 26,
                b'0'..=b'9' => byte - b'0' + 52,
                b'+' => 62,
                b'/' => 63,
                b'=' => break,
                _ => {
                    return Err(TlsError::CertificateParseError(
                        "Invalid base64 character".to_string(),
                    ))
                }
            };

            buffer = (buffer << 6) | u32::from(value);
            bits += 6;

            if bits >= 8 {
                bits -= 8;
                result.push((buffer >> bits) as u8);
                buffer &= (1 << bits) - 1;
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_certificate_pin_creation() {
        // Arrange
        let hostname = "example.com".to_string();
        let hash = vec![0x1, 0x2, 0x3, 0x4];

        // Act
        let pin = CertificatePin::new(hostname.clone(), hash.clone());

        // Assert
        assert_eq!(pin.hostname(), "example.com");
        assert_eq!(pin.spki_hash(), &hash);
    }

    #[test]
    fn test_certificate_pin_verify_success() {
        // Arrange
        let spki = b"test certificate data";
        let hash = Sha256::digest(spki);
        let pin = CertificatePin::new("example.com".to_string(), hash.to_vec());

        // Act
        let result = pin.verify(spki);

        // Assert
        assert!(
            result.is_ok(),
            "Verification should succeed with matching hash"
        );
    }

    #[test]
    fn test_certificate_pin_verify_failure() {
        // Arrange
        let spki = b"test certificate data";
        let wrong_hash = vec![0x1, 0x2, 0x3, 0x4];
        let pin = CertificatePin::new("example.com".to_string(), wrong_hash);

        // Act
        let result = pin.verify(spki);

        // Assert
        assert!(
            result.is_err(),
            "Verification should fail with non-matching hash"
        );
        assert!(matches!(result, Err(TlsError::PinningFailed(_))));
    }

    #[test]
    fn test_pinning_manager_add_pin() {
        // Arrange
        let mut manager = PinningManager::new(PinningStrategy::FailClosed);
        let pin = CertificatePin::new("example.com".to_string(), vec![0x1, 0x2]);

        // Act
        manager.add_pin(pin);

        // Assert
        assert!(
            manager.has_pins("example.com"),
            "Manager should have pins for example.com"
        );
    }

    #[test]
    fn test_pinning_manager_verify_success() {
        // Arrange
        let mut manager = PinningManager::new(PinningStrategy::FailClosed);
        let spki = b"test data";
        let hash = Sha256::digest(spki);
        let pin = CertificatePin::new("example.com".to_string(), hash.to_vec());
        manager.add_pin(pin);

        // Act
        let result = manager.verify("example.com", spki);

        // Assert
        assert!(result.is_ok(), "Verification should succeed");
    }

    #[test]
    fn test_pinning_manager_verify_fail_closed() {
        // Arrange
        let mut manager = PinningManager::new(PinningStrategy::FailClosed);
        let pin = CertificatePin::new("example.com".to_string(), vec![0x1, 0x2]);
        manager.add_pin(pin);

        // Act
        let result = manager.verify("example.com", b"wrong data");

        // Assert
        assert!(result.is_err(), "Verification should fail with wrong data");
    }

    #[test]
    fn test_pinning_manager_no_pins() {
        // Arrange
        let manager = PinningManager::new(PinningStrategy::FailClosed);

        // Act
        let result = manager.verify("unknown.com", b"any data");

        // Assert
        assert!(result.is_ok(), "Should succeed when no pins configured");
    }

    #[test]
    fn test_pinning_strategy_equality() {
        // Arrange & Act & Assert
        assert_eq!(PinningStrategy::FailOpen, PinningStrategy::FailOpen);
        assert_eq!(PinningStrategy::FailClosed, PinningStrategy::FailClosed);
        assert_ne!(PinningStrategy::FailOpen, PinningStrategy::FailClosed);
    }

    #[test]
    fn test_add_multiple_pins() {
        // Arrange
        let mut manager = PinningManager::new(PinningStrategy::FailOpen);
        let pins = vec![
            CertificatePin::new("example.com".to_string(), vec![0x1]),
            CertificatePin::new("test.com".to_string(), vec![0x2]),
        ];

        // Act
        manager.add_pins(pins);

        // Assert
        assert!(manager.has_pins("example.com"));
        assert!(manager.has_pins("test.com"));
    }
}
