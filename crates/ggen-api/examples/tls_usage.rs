//! Example usage of TLS 1.3 network security module
//!
//! Demonstrates:
//! - TLS connector configuration
//! - Certificate pinning
//! - HSTS enforcement
//! - Connection pooling

use ggen_api::network::tls::{
    CertificatePin, CipherSuite, ConnectionPool, HstsMiddleware, HstsPolicy, PinningStrategy,
    PoolConfig, TlsConfigBuilder, TlsConnector, ValidationPolicy,
};
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    println!("=== TLS 1.3 Network Security Example ===\n");

    // Example 1: Basic TLS connector with default settings
    println!("1. Creating TLS connector with default settings...");
    let default_config = TlsConfigBuilder::default().build()?;
    let _connector = TlsConnector::new(default_config)?;
    println!("   ✓ TLS connector created with secure defaults");
    println!("   - TLS 1.3 only (no downgrade)");
    println!("   - ChaCha20-Poly1305 and AES-256-GCM ciphers");
    println!("   - OCSP stapling enabled");
    println!("   - SNI enabled\n");

    // Example 2: Strict TLS configuration with certificate pinning
    println!("2. Creating strict TLS configuration with certificate pinning...");
    let pin = CertificatePin::new(
        "api.example.com".to_string(),
        vec![
            0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, // Example SPKI hash
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
            0xff, 0x00, 0xa1, 0xb2, 0xc3, 0xd4, 0xe5, 0xf6, 0x07, 0x18,
        ],
    );

    let strict_config = TlsConfigBuilder::new()
        .cipher_suites(vec![
            CipherSuite::ChaCha20Poly1305,
            CipherSuite::Aes256Gcm,
        ])
        .add_certificate_pin(pin)
        .pinning_strategy(PinningStrategy::FailClosed)
        .validation_policy(ValidationPolicy::strict())
        .enable_ocsp_stapling(true)
        .enable_sni(true)
        .build()?;

    let _strict_connector = TlsConnector::new(strict_config)?;
    println!("   ✓ Strict TLS connector created");
    println!("   - Certificate pinning: ENABLED (fail-closed)");
    println!("   - OCSP required: YES");
    println!("   - Certificate Transparency required: YES");
    println!("   - Max chain depth: 3\n");

    // Example 3: Connection pool with TLS
    println!("3. Creating secure connection pool...");
    let pool_config = PoolConfig {
        max_connections_per_host: 10,
        max_idle_time: Duration::from_secs(300),
        connection_timeout: Duration::from_secs(30),
        enable_session_resumption: true,
        max_pool_size: 100,
    };

    let pool = ConnectionPool::new(pool_config);
    println!("   ✓ Connection pool created");
    println!("   - Max connections per host: 10");
    println!("   - Max idle time: 5 minutes");
    println!("   - TLS session resumption: ENABLED\n");

    // Get and return connections
    println!("4. Testing connection pool operations...");
    let handle1 = pool.get_connection("api.example.com").await?;
    println!("   ✓ Got connection #{} to {}", handle1.id(), handle1.hostname());

    let handle2 = pool.get_connection("api.example.com").await?;
    println!("   ✓ Got connection #{} to {}", handle2.id(), handle2.hostname());

    pool.return_connection(handle1).await?;
    println!("   ✓ Returned connection to pool");

    let handle3 = pool.get_connection("api.example.com").await?;
    println!(
        "   ✓ Got connection #{} (reused from pool)",
        handle3.id()
    );

    let stats = pool.stats().await;
    println!("   - Total connections: {}", stats.total_connections);
    println!(
        "   - Connections to api.example.com: {}\n",
        stats
            .connections_by_host
            .get("api.example.com")
            .unwrap_or(&0)
    );

    // Example 4: HSTS enforcement
    println!("5. Configuring HSTS (HTTP Strict Transport Security)...");
    let hsts_policy = HstsPolicy {
        max_age: Duration::from_secs(31_536_000), // 1 year
        include_subdomains: true,
        preload: true,
    };

    hsts_policy.validate()?;
    let hsts = HstsMiddleware::new(hsts_policy.clone());

    println!("   ✓ HSTS policy created");
    println!("   - Max age: 1 year");
    println!("   - Include subdomains: YES");
    println!("   - Preload: YES");
    println!("   - Header: {}\n", hsts_policy.header_value());

    // Register host with HSTS
    hsts.add_host("example.com".to_string(), hsts_policy)
        .await?;
    println!("6. Testing HSTS enforcement...");
    let should_enforce = hsts.should_enforce_https("example.com").await;
    println!("   - Enforce HTTPS for example.com: {should_enforce}");

    let should_enforce_subdomain = hsts.should_enforce_https("www.example.com").await;
    println!("   - Enforce HTTPS for www.example.com: {should_enforce_subdomain}");

    let should_enforce_unknown = hsts.should_enforce_https("unknown.com").await;
    println!("   - Enforce HTTPS for unknown.com: {should_enforce_unknown}\n");

    // Example 5: Custom cipher suite selection
    println!("7. Custom cipher suite configuration...");
    let custom_config = TlsConfigBuilder::new()
        .cipher_suites(vec![CipherSuite::Aes256Gcm])
        .build()?;

    println!("   ✓ Custom TLS config created");
    println!("   - Cipher suites: AES-256-GCM only");
    println!("   - Use case: Environments without ChaCha20 hardware acceleration\n");

    // Example 6: Development/testing configuration
    println!("8. Development/testing configuration...");
    let dev_config = TlsConfigBuilder::new()
        .validation_policy(ValidationPolicy::permissive())
        .build()?;

    let _dev_connector = TlsConnector::new(dev_config)?;
    println!("   ✓ Development TLS connector created");
    println!("   - Self-signed certificates: ALLOWED");
    println!("   - Hostname verification: DISABLED");
    println!("   - ⚠️  WARNING: Only use in development/testing!\n");

    println!("=== All examples completed successfully ===");

    Ok(())
}
