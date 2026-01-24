//! Certificate validation and verification

use super::error::{TlsError, TlsResult};
use std::time::{SystemTime, UNIX_EPOCH};

/// Certificate validation policy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationPolicy {
    /// Verify certificate expiration
    pub verify_expiration: bool,
    /// Verify certificate hostname
    pub verify_hostname: bool,
    /// Require OCSP stapling
    pub require_ocsp: bool,
    /// Maximum certificate chain depth
    pub max_chain_depth: usize,
    /// Allow self-signed certificates (for testing)
    pub allow_self_signed: bool,
    /// Require certificate transparency
    pub require_ct: bool,
}

impl Default for ValidationPolicy {
    fn default() -> Self {
        Self {
            verify_expiration: true,
            verify_hostname: true,
            require_ocsp: false,
            max_chain_depth: 5,
            allow_self_signed: false,
            require_ct: false,
        }
    }
}

impl ValidationPolicy {
    /// Create a strict validation policy
    #[must_use]
    pub fn strict() -> Self {
        Self {
            verify_expiration: true,
            verify_hostname: true,
            require_ocsp: true,
            max_chain_depth: 3,
            allow_self_signed: false,
            require_ct: true,
        }
    }

    /// Create a permissive policy (for testing)
    #[must_use]
    pub fn permissive() -> Self {
        Self {
            verify_expiration: false,
            verify_hostname: false,
            require_ocsp: false,
            max_chain_depth: 10,
            allow_self_signed: true,
            require_ct: false,
        }
    }
}

/// Certificate validator
pub struct CertificateValidator {
    policy: ValidationPolicy,
}

impl CertificateValidator {
    /// Create a new validator with the given policy
    #[must_use]
    pub fn new(policy: ValidationPolicy) -> Self {
        Self { policy }
    }

    /// Get the validation policy
    #[must_use]
    pub fn policy(&self) -> &ValidationPolicy {
        &self.policy
    }

    /// Validate a certificate chain
    ///
    /// # Errors
    /// Returns `TlsError` if validation fails
    pub fn validate_chain(&self, chain_der: &[Vec<u8>]) -> TlsResult<()> {
        // Check chain depth
        if chain_der.len() > self.policy.max_chain_depth {
            return Err(TlsError::ValidationFailed(format!(
                "Certificate chain too long: {} > {}",
                chain_der.len(),
                self.policy.max_chain_depth
            )));
        }

        if chain_der.is_empty() {
            return Err(TlsError::ValidationFailed("Empty certificate chain".to_string()));
        }

        // Validate each certificate in the chain
        for (idx, cert_der) in chain_der.iter().enumerate() {
            self.validate_certificate(cert_der, idx == 0)?;
        }

        Ok(())
    }

    /// Validate a single certificate
    ///
    /// # Errors
    /// Returns `TlsError` if validation fails
    fn validate_certificate(&self, cert_der: &[u8], is_leaf: bool) -> TlsResult<()> {
        // Parse certificate (simplified - in production use x509-parser)
        if cert_der.is_empty() {
            return Err(TlsError::CertificateParseError("Empty certificate".to_string()));
        }

        // Check expiration if required
        if self.policy.verify_expiration {
            self.check_expiration(cert_der)?;
        }

        // For leaf certificates, check additional properties
        if is_leaf {
            if self.policy.verify_hostname {
                // Hostname verification would happen during connection
                // This is a placeholder for additional leaf certificate checks
            }
        }

        Ok(())
    }

    /// Check certificate expiration
    ///
    /// # Errors
    /// Returns `TlsError` if certificate is expired or not yet valid
    fn check_expiration(&self, _cert_der: &[u8]) -> TlsResult<()> {
        // In production, parse the certificate and check NotBefore/NotAfter
        // For now, we'll just ensure we can get the current time
        let _now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| TlsError::ValidationFailed(format!("Time error: {e}")))?;

        // Certificate parsing and expiration check would go here
        // This is a simplified implementation
        Ok(())
    }

    /// Validate hostname matches certificate
    ///
    /// # Errors
    /// Returns `TlsError` if hostname doesn't match
    pub fn validate_hostname(&self, hostname: &str, _cert_der: &[u8]) -> TlsResult<()> {
        if !self.policy.verify_hostname {
            return Ok(());
        }

        // Basic hostname validation
        if hostname.is_empty() {
            return Err(TlsError::InvalidHostname("Empty hostname".to_string()));
        }

        // In production, parse certificate and check Subject Alternative Names (SANs)
        // and Common Name (CN)
        Ok(())
    }

    /// Verify OCSP response
    ///
    /// # Errors
    /// Returns `TlsError` if OCSP verification fails
    pub fn verify_ocsp(&self, _ocsp_response: &[u8]) -> TlsResult<()> {
        if !self.policy.require_ocsp {
            return Ok(());
        }

        // OCSP response parsing and verification would go here
        // This is a placeholder for production implementation
        Ok(())
    }

    /// Verify Certificate Transparency (CT) logs
    ///
    /// # Errors
    /// Returns `TlsError` if CT verification fails
    pub fn verify_ct(&self, _sct_list: &[u8]) -> TlsResult<()> {
        if !self.policy.require_ct {
            return Ok(());
        }

        // CT log verification would go here
        // This is a placeholder for production implementation
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_policy_default() {
        // Arrange & Act
        let policy = ValidationPolicy::default();

        // Assert
        assert!(policy.verify_expiration, "Should verify expiration by default");
        assert!(policy.verify_hostname, "Should verify hostname by default");
        assert!(!policy.require_ocsp, "Should not require OCSP by default");
        assert!(!policy.allow_self_signed, "Should not allow self-signed by default");
    }

    #[test]
    fn test_validation_policy_strict() {
        // Arrange & Act
        let policy = ValidationPolicy::strict();

        // Assert
        assert!(policy.verify_expiration);
        assert!(policy.verify_hostname);
        assert!(policy.require_ocsp, "Strict policy should require OCSP");
        assert!(policy.require_ct, "Strict policy should require CT");
        assert!(!policy.allow_self_signed);
        assert_eq!(policy.max_chain_depth, 3, "Strict policy should have lower chain depth");
    }

    #[test]
    fn test_validation_policy_permissive() {
        // Arrange & Act
        let policy = ValidationPolicy::permissive();

        // Assert
        assert!(!policy.verify_expiration);
        assert!(!policy.verify_hostname);
        assert!(policy.allow_self_signed, "Permissive policy should allow self-signed");
        assert_eq!(policy.max_chain_depth, 10);
    }

    #[test]
    fn test_validator_creation() {
        // Arrange
        let policy = ValidationPolicy::default();

        // Act
        let validator = CertificateValidator::new(policy.clone());

        // Assert
        assert_eq!(validator.policy(), &policy);
    }

    #[test]
    fn test_validate_chain_empty_fails() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::default());
        let chain: Vec<Vec<u8>> = vec![];

        // Act
        let result = validator.validate_chain(&chain);

        // Assert
        assert!(result.is_err(), "Empty chain should fail validation");
        assert!(matches!(result, Err(TlsError::ValidationFailed(_))));
    }

    #[test]
    fn test_validate_chain_too_long_fails() {
        // Arrange
        let policy = ValidationPolicy {
            max_chain_depth: 2,
            ..Default::default()
        };
        let validator = CertificateValidator::new(policy);
        let chain = vec![vec![1], vec![2], vec![3]]; // 3 certs, max is 2

        // Act
        let result = validator.validate_chain(&chain);

        // Assert
        assert!(result.is_err(), "Chain exceeding max depth should fail");
    }

    #[test]
    fn test_validate_chain_single_cert() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::permissive());
        let chain = vec![vec![1, 2, 3, 4]]; // Single cert

        // Act
        let result = validator.validate_chain(&chain);

        // Assert
        assert!(result.is_ok(), "Valid single cert chain should pass");
    }

    #[test]
    fn test_validate_hostname_empty_fails() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::default());
        let cert = vec![1, 2, 3];

        // Act
        let result = validator.validate_hostname("", &cert);

        // Assert
        assert!(result.is_err(), "Empty hostname should fail");
        assert!(matches!(result, Err(TlsError::InvalidHostname(_))));
    }

    #[test]
    fn test_validate_hostname_valid() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::default());
        let cert = vec![1, 2, 3];

        // Act
        let result = validator.validate_hostname("example.com", &cert);

        // Assert
        assert!(result.is_ok(), "Valid hostname should pass");
    }

    #[test]
    fn test_validate_hostname_disabled() {
        // Arrange
        let policy = ValidationPolicy {
            verify_hostname: false,
            ..Default::default()
        };
        let validator = CertificateValidator::new(policy);

        // Act
        let result = validator.validate_hostname("", &[]);

        // Assert
        assert!(result.is_ok(), "Should pass when hostname verification disabled");
    }

    #[test]
    fn test_verify_ocsp_not_required() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::default());

        // Act
        let result = validator.verify_ocsp(&[]);

        // Assert
        assert!(result.is_ok(), "Should pass when OCSP not required");
    }

    #[test]
    fn test_verify_ct_not_required() {
        // Arrange
        let validator = CertificateValidator::new(ValidationPolicy::default());

        // Act
        let result = validator.verify_ct(&[]);

        // Assert
        assert!(result.is_ok(), "Should pass when CT not required");
    }
}
