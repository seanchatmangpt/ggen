//! TLS 1.3 integration tests
//!
//! These tests verify TLS configuration, certificate validation, pinning, HSTS, and connection pooling.

use ggen_api::network::tls::{
    CertificatePin, CipherSuite, ConnectionPool, HstsMiddleware, HstsPolicy, PinningStrategy,
    PoolConfig, TlsConfig, TlsConfigBuilder, TlsConnector, TlsError, ValidationPolicy,
};
use std::time::Duration;

// Test 1: TLS connector creation with default config
#[tokio::test]
async fn test_tls_connector_with_default_config() {
    // Arrange
    let config = TlsConfigBuilder::default()
        .build()
        .expect("Failed to build default config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should be created with default config"
    );
}

// Test 2: TLS connector with custom cipher suites
#[tokio::test]
async fn test_tls_connector_with_custom_ciphers() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .cipher_suites(vec![CipherSuite::ChaCha20Poly1305, CipherSuite::Aes256Gcm])
        .build()
        .expect("Failed to build config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should support custom cipher suites"
    );
    let connector = connector.expect("Connector should exist");
    assert_eq!(
        connector.config().cipher_suites.len(),
        2,
        "Should have 2 cipher suites"
    );
}

// Test 3: TLS connector with certificate pinning
#[tokio::test]
async fn test_tls_connector_with_certificate_pinning() {
    // Arrange
    let pin = CertificatePin::new("example.com".to_string(), vec![0x1, 0x2, 0x3, 0x4]);
    let config = TlsConfigBuilder::new()
        .add_certificate_pin(pin)
        .pinning_strategy(PinningStrategy::FailClosed)
        .build()
        .expect("Failed to build config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should support certificate pinning"
    );
    let connector = connector.expect("Connector should exist");
    assert_eq!(connector.pins().len(), 1, "Should have 1 certificate pin");
}

// Test 4: TLS connector with strict validation policy
#[tokio::test]
async fn test_tls_connector_with_strict_validation() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .validation_policy(ValidationPolicy::strict())
        .build()
        .expect("Failed to build config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should support strict validation"
    );
    let connector = connector.expect("Connector should exist");
    assert!(
        connector.validator().policy().require_ocsp,
        "Strict policy should require OCSP"
    );
    assert!(
        connector.validator().policy().require_ct,
        "Strict policy should require CT"
    );
}

// Test 5: TLS connector with OCSP stapling enabled
#[tokio::test]
async fn test_tls_connector_with_ocsp_stapling() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .enable_ocsp_stapling(true)
        .build()
        .expect("Failed to build config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should support OCSP stapling"
    );
    let connector = connector.expect("Connector should exist");
    assert!(
        connector.config().enable_ocsp_stapling,
        "OCSP stapling should be enabled"
    );
}

// Test 6: TLS connector with SNI disabled
#[tokio::test]
async fn test_tls_connector_with_sni_disabled() {
    // Arrange
    let config = TlsConfigBuilder::new()
        .enable_sni(false)
        .build()
        .expect("Failed to build config");

    // Act
    let connector = TlsConnector::new(config);

    // Assert
    assert!(
        connector.is_ok(),
        "TLS connector should support SNI configuration"
    );
    let connector = connector.expect("Connector should exist");
    assert!(!connector.config().enable_sni, "SNI should be disabled");
}

// Test 7: Connection pool creation and basic operations
#[tokio::test]
async fn test_connection_pool_basic_operations() {
    // Arrange
    let pool = ConnectionPool::new(PoolConfig::default());

    // Act
    let handle = pool.get_connection("example.com").await;

    // Assert
    assert!(handle.is_ok(), "Should get connection from pool");
    let handle = handle.expect("Handle should exist");
    assert_eq!(handle.hostname(), "example.com", "Hostname should match");
}

// Test 8: Connection pool with custom limits
#[tokio::test]
async fn test_connection_pool_with_limits() {
    // Arrange
    let config = PoolConfig {
        max_connections_per_host: 2,
        max_pool_size: 5,
        ..Default::default()
    };
    let pool = ConnectionPool::new(config);

    // Act
    let h1 = pool.get_connection("host1.com").await;
    let h2 = pool.get_connection("host1.com").await;
    let h3 = pool.get_connection("host1.com").await;

    // Assert
    assert!(h1.is_ok(), "First connection should succeed");
    assert!(h2.is_ok(), "Second connection should succeed");
    assert!(h3.is_err(), "Third connection should fail (exceeds limit)");
}

// Test 9: Connection pool reuse
#[tokio::test]
async fn test_connection_pool_reuse() {
    // Arrange
    let pool = ConnectionPool::new(PoolConfig::default());
    let handle1 = pool
        .get_connection("example.com")
        .await
        .expect("Failed to get connection");
    let id1 = handle1.id();
    pool.return_connection(handle1)
        .await
        .expect("Failed to return connection");

    // Act
    let handle2 = pool
        .get_connection("example.com")
        .await
        .expect("Failed to get connection");

    // Assert
    assert_eq!(handle2.id(), id1, "Should reuse the same connection");
}

// Test 10: Connection pool cleanup
#[tokio::test]
async fn test_connection_pool_cleanup() {
    // Arrange
    let config = PoolConfig {
        max_idle_time: Duration::from_millis(1),
        ..Default::default()
    };
    let pool = ConnectionPool::new(config);
    let handle = pool.get_connection("example.com").await.expect("Failed");
    pool.return_connection(handle)
        .await
        .expect("Failed to return");

    // Act
    tokio::time::sleep(Duration::from_millis(10)).await;
    pool.cleanup_expired().await;
    let stats = pool.stats().await;

    // Assert
    assert_eq!(
        stats.total_connections, 0,
        "Expired connections should be cleaned up"
    );
}

// Test 11: HSTS policy creation and validation
#[tokio::test]
async fn test_hsts_policy_validation() {
    // Arrange
    let policy = HstsPolicy::strict();

    // Act
    let result = policy.validate();

    // Assert
    assert!(result.is_ok(), "Strict HSTS policy should be valid");
}

// Test 12: HSTS policy with invalid preload configuration
#[tokio::test]
async fn test_hsts_policy_invalid_preload() {
    // Arrange
    let policy = HstsPolicy {
        max_age: Duration::from_secs(300),
        include_subdomains: false,
        preload: true,
    };

    // Act
    let result = policy.validate();

    // Assert
    assert!(
        result.is_err(),
        "Invalid preload configuration should fail validation"
    );
}

// Test 13: HSTS middleware enforcement
#[tokio::test]
async fn test_hsts_middleware_enforcement() {
    // Arrange
    let middleware = HstsMiddleware::new(HstsPolicy::default());
    middleware
        .add_host("example.com".to_string(), HstsPolicy::default())
        .await
        .expect("Failed to add host");

    // Act
    let should_enforce = middleware.should_enforce_https("example.com").await;

    // Assert
    assert!(
        should_enforce,
        "HTTPS should be enforced for registered host"
    );
}

// Test 14: HSTS middleware subdomain enforcement
#[tokio::test]
async fn test_hsts_middleware_subdomain_enforcement() {
    // Arrange
    let middleware = HstsMiddleware::new(HstsPolicy::default());
    let policy = HstsPolicy {
        max_age: Duration::from_secs(31_536_000),
        include_subdomains: true,
        preload: false,
    };
    middleware
        .add_host("example.com".to_string(), policy)
        .await
        .expect("Failed to add host");

    // Act
    let should_enforce = middleware.should_enforce_https("www.example.com").await;

    // Assert
    assert!(should_enforce, "HTTPS should be enforced for subdomains");
}

// Test 15: HSTS header parsing
#[tokio::test]
async fn test_hsts_header_parsing() {
    // Arrange
    let middleware = HstsMiddleware::new(HstsPolicy::default());
    let header = "max-age=31536000; includeSubDomains; preload";

    // Act
    let result = middleware
        .process_header("example.com".to_string(), header)
        .await;

    // Assert
    assert!(
        result.is_ok(),
        "Valid HSTS header should parse successfully"
    );
    assert!(
        middleware.should_enforce_https("example.com").await,
        "Host should be registered"
    );
}

// Test 16: Multiple cipher suites configuration
#[tokio::test]
async fn test_multiple_cipher_suites() {
    // Arrange
    let ciphers = vec![
        CipherSuite::ChaCha20Poly1305,
        CipherSuite::Aes256Gcm,
        CipherSuite::Aes128Gcm,
    ];

    // Act
    let config = TlsConfigBuilder::new()
        .cipher_suites(ciphers.clone())
        .build();

    // Assert
    assert!(config.is_ok(), "Multiple cipher suites should be supported");
    let config = config.expect("Config should exist");
    assert_eq!(config.cipher_suites.len(), 3, "Should have 3 cipher suites");
}

// Test 17: Connection pool statistics
#[tokio::test]
async fn test_connection_pool_statistics() {
    // Arrange
    let pool = ConnectionPool::new(PoolConfig::default());
    let _h1 = pool.get_connection("host1.com").await.expect("Failed");
    let _h2 = pool.get_connection("host2.com").await.expect("Failed");

    // Act
    let stats = pool.stats().await;

    // Assert
    assert_eq!(stats.total_connections, 2, "Should track total connections");
    assert!(
        stats.connections_by_host.contains_key("host1.com"),
        "Should track by host"
    );
    assert!(
        stats.connections_by_host.contains_key("host2.com"),
        "Should track by host"
    );
}

// Test 18: Validation policy with permissive settings (for testing)
#[tokio::test]
async fn test_permissive_validation_policy() {
    // Arrange
    let policy = ValidationPolicy::permissive();

    // Act
    let config = TlsConfigBuilder::new().validation_policy(policy).build();

    // Assert
    assert!(config.is_ok(), "Permissive policy should be supported");
    let config = config.expect("Config should exist");
    assert!(
        config.validation_policy.allow_self_signed,
        "Should allow self-signed certs"
    );
}
