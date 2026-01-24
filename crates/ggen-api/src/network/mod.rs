//! Network security and TLS enforcement
//!
//! This module provides enterprise-grade network security with:
//! - TLS 1.3 enforcement (minimum version)
//! - Certificate validation and pinning
//! - HSTS (HTTP Strict Transport Security)
//! - Secure connection pooling
//! - OCSP stapling
//! - Certificate transparency monitoring

pub mod tls;

pub use tls::{
    TlsConfig, TlsConfigBuilder, TlsConnector, TlsError, TlsResult,
    CertificatePin, CertificateValidator, ConnectionPool, HstsPolicy,
};
