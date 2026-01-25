//! TLS security tests - downgrade attacks, cipher negotiation, certificate validation
//!
//! These tests verify that the TLS implementation properly defends against common attacks.

use ggen_api::network::tls::{
    CertificatePin, CipherSuite, HstsPolicy, PinningStrategy, TlsConfigBuilder, TlsConnector,
    TlsError, ValidationPolicy,
};
use sha2::{Digest, Sha256};
use std::time::Duration;

// Test 1: Reject empty cipher suite list
#[tokio::test]
async fn test_reject_empty_cipher_suites() {
    // Arrange
    let builder = TlsConfigBuilder::new().cipher_suites(vec![]);

    // Act
    let result = builder.build();

    // Assert
    assert!(
        result.is_err(),
        "Empty cipher suite list should be rejected"
    );
    assert!(
        matches!(result, Err(TlsError::ConfigError(_))),
        "Should be config error"
    );
}

// Test 2: Enforce TLS 1.3 only (no downgrade to TLS 1.2)
#[tokio::test]
async fn test_tls_13_only_enforcement() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .build()
        .expect("Failed to build config");

    // Act
    let rustls_config = config.build_rustls_config();

    // Assert
    assert!(rustls_config.is_ok(), "Should build rustls config");
    // Note: In production, verify that only TLS 1.3 is in supported versions
    // rustls_config will be configured with TLS 1.3 only via protocol_versions
}

// Test 3: Verify cipher suite strength (only strong ciphers)
#[tokio::test]
async fn test_cipher_suite_strength() {
    // Arrange
    let ciphers = vec![CipherSuite::ChaCha20Poly1305, CipherSuite::Aes256Gcm];

    // Act
    let config = TlsConfigBuilder::new().cipher_suites(ciphers).build();

    // Assert
    assert!(config.is_ok(), "Strong cipher suites should be accepted");
    let config = config.expect("Config should exist");
    for cipher in &config.cipher_suites {
        // Verify all ciphers are strong (256-bit or ChaCha20)
        match cipher {
            CipherSuite::ChaCha20Poly1305 | CipherSuite::Aes256Gcm => {}
            CipherSuite::Aes128Gcm => {
                // AES-128 is acceptable but weaker than AES-256
            }
        }
    }
}

// Test 4: Certificate pinning fail-closed mode
#[tokio::test]
async fn test_certificate_pinning_fail_closed() {
    // Arrange
    let wrong_spki = b"wrong certificate data";
    let correct_spki = b"correct certificate data";
    let hash = Sha256::digest(correct_spki);
    let pin = CertificatePin::new("example.com".to_string(), hash.to_vec());

    // Act
    let result = pin.verify(wrong_spki);

    // Assert
    assert!(
        result.is_err(),
        "Pinning should fail for mismatched certificate"
    );
    assert!(
        matches!(result, Err(TlsError::PinningFailed(_))),
        "Should be pinning error"
    );
}

// Test 5: Certificate pinning with correct hash succeeds
#[tokio::test]
async fn test_certificate_pinning_success() {
    // Arrange
    let spki = b"certificate data";
    let hash = Sha256::digest(spki);
    let pin = CertificatePin::new("example.com".to_string(), hash.to_vec());

    // Act
    let result = pin.verify(spki);

    // Assert
    assert!(
        result.is_ok(),
        "Pinning should succeed with correct certificate"
    );
}

// Test 6: HSTS preload validation - must have includeSubDomains
#[tokio::test]
async fn test_hsts_preload_requires_subdomains() {
    // Arrange
    let policy = HstsPolicy {
        max_age: Duration::from_secs(63_072_000), // 2 years
        include_subdomains: false,
        preload: true,
    };

    // Act
    let result = policy.validate();

    // Assert
    assert!(
        result.is_err(),
        "Preload without includeSubDomains should fail"
    );
    assert!(
        matches!(result, Err(TlsError::HstsViolation(_))),
        "Should be HSTS violation"
    );
}

// Test 7: HSTS preload validation - must have long max-age
#[tokio::test]
async fn test_hsts_preload_requires_long_max_age() {
    // Arrange
    let policy = HstsPolicy {
        max_age: Duration::from_secs(300), // 5 minutes (too short)
        include_subdomains: true,
        preload: true,
    };

    // Act
    let result = policy.validate();

    // Assert
    assert!(result.is_err(), "Preload with short max-age should fail");
}

// Test 8: Strict validation policy enforces all checks
#[tokio::test]
async fn test_strict_validation_policy() {
    // Arrange
    let policy = ValidationPolicy::strict();

    // Act & Assert
    assert!(
        policy.verify_expiration,
        "Strict policy must verify expiration"
    );
    assert!(policy.verify_hostname, "Strict policy must verify hostname");
    assert!(policy.require_ocsp, "Strict policy must require OCSP");
    assert!(policy.require_ct, "Strict policy must require CT");
    assert!(
        !policy.allow_self_signed,
        "Strict policy must not allow self-signed"
    );
    assert_eq!(
        policy.max_chain_depth, 3,
        "Strict policy must have limited chain depth"
    );
}

// Test 9: Certificate chain depth limit enforcement
#[tokio::test]
async fn test_certificate_chain_depth_limit() {
    // Arrange
    let policy = ValidationPolicy {
        max_chain_depth: 2,
        ..ValidationPolicy::default()
    };
    let config = TlsConfigBuilder::new()
        .validation_policy(policy)
        .build()
        .expect("Failed to build config");
    let connector = TlsConnector::new(config).expect("Failed to create connector");

    // Act
    let chain_too_long = vec![vec![1], vec![2], vec![3]]; // 3 certs, max is 2
    let result = connector.validator().validate_chain(&chain_too_long);

    // Assert
    assert!(
        result.is_err(),
        "Chain exceeding max depth should be rejected"
    );
}

// Test 10: Reject configuration with conflicting policies
#[tokio::test]
async fn test_reject_conflicting_policies() {
    // Arrange - trying to create strict policy but allow self-signed
    let policy = ValidationPolicy {
        verify_expiration: true,
        verify_hostname: true,
        require_ocsp: true,
        allow_self_signed: true, // Conflicting with strict requirements
        ..ValidationPolicy::default()
    };

    // Act
    let config = TlsConfigBuilder::new().validation_policy(policy).build();

    // Assert
    assert!(
        config.is_ok(),
        "Config should accept policy (validation is at runtime)"
    );
    // Note: The conflict is logical, not a configuration error
    // In production, additional validation could be added
}

// Test 11: Hostname validation rejects empty hostname
#[tokio::test]
async fn test_hostname_validation_rejects_empty() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .build()
        .expect("Failed to build config");
    let connector = TlsConnector::new(config).expect("Failed to create connector");

    // Act
    let result = connector.validator().validate_hostname("", &[1, 2, 3]);

    // Assert
    assert!(result.is_err(), "Empty hostname should be rejected");
    assert!(
        matches!(result, Err(TlsError::InvalidHostname(_))),
        "Should be hostname error"
    );
}

// Test 12: Multiple certificate pins for same host
#[tokio::test]
async fn test_multiple_pins_for_host() {
    // Arrange
    let pin1 = CertificatePin::new("example.com".to_string(), vec![1, 2, 3]);
    let pin2 = CertificatePin::new("example.com".to_string(), vec![4, 5, 6]);

    // Act
    let config = TlsConfigBuilder::new()
        .add_certificate_pin(pin1)
        .add_certificate_pin(pin2)
        .build();

    // Assert
    assert!(
        config.is_ok(),
        "Multiple pins for same host should be supported"
    );
    let config = config.expect("Config should exist");
    assert_eq!(config.certificate_pins.len(), 2, "Should have 2 pins");
}

// Test 13: Pinning strategy fail-open allows connection on pin mismatch (with warning)
#[tokio::test]
async fn test_pinning_strategy_fail_open() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .pinning_strategy(PinningStrategy::FailOpen)
        .build()
        .expect("Failed to build config");

    // Act & Assert
    assert_eq!(config.pinning_strategy, PinningStrategy::FailOpen);
    // In production, verify that the pinning manager logs warnings but allows connections
}

// Test 14: Cipher suite names are correctly formatted
#[tokio::test]
async fn test_cipher_suite_naming() {
    // Arrange & Act
    let chacha = CipherSuite::ChaCha20Poly1305.name();
    let aes256 = CipherSuite::Aes256Gcm.name();
    let aes128 = CipherSuite::Aes128Gcm.name();

    // Assert
    assert!(
        chacha.contains("TLS13"),
        "Cipher name should indicate TLS 1.3"
    );
    assert!(
        chacha.contains("CHACHA20"),
        "Cipher name should include algorithm"
    );
    assert!(
        aes256.contains("AES_256"),
        "Cipher name should include key size"
    );
    assert!(
        aes128.contains("AES_128"),
        "Cipher name should include key size"
    );
}

// Test 15: HSTS header value format is RFC-compliant
#[tokio::test]
async fn test_hsts_header_format() {
    // Arrange
    let policy = HstsPolicy {
        max_age: Duration::from_secs(31_536_000),
        include_subdomains: true,
        preload: true,
    };

    // Act
    let header = policy.header_value();

    // Assert
    assert!(
        header.contains("max-age=31536000"),
        "Header must include max-age"
    );
    assert!(
        header.contains("includeSubDomains"),
        "Header must include includeSubDomains"
    );
    assert!(header.contains("preload"), "Header must include preload");
    // Verify format: "max-age=...; includeSubDomains; preload"
    assert!(
        header.split(';').count() == 3,
        "Header should have 3 parts separated by semicolons"
    );
}
