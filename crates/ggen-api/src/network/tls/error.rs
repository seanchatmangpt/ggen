//! TLS error types

use thiserror::Error;

/// TLS-related errors
#[derive(Error, Debug)]
pub enum TlsError {
    /// Configuration error
    #[error("TLS configuration error: {0}")]
    ConfigError(String),

    /// Certificate validation failed
    #[error("Certificate validation failed: {0}")]
    ValidationFailed(String),

    /// Certificate pinning failed
    #[error("Certificate pinning verification failed: {0}")]
    PinningFailed(String),

    /// OCSP stapling error
    #[error("OCSP stapling error: {0}")]
    OcspError(String),

    /// Unsupported TLS version
    #[error("Unsupported TLS version: {0}")]
    UnsupportedVersion(String),

    /// Invalid cipher suite
    #[error("Invalid cipher suite: {0}")]
    InvalidCipher(String),

    /// Connection pool error
    #[error("Connection pool error: {0}")]
    PoolError(String),

    /// Certificate parsing error
    #[error("Certificate parsing error: {0}")]
    CertificateParseError(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Rustls error
    #[error("Rustls error: {0}")]
    Rustls(String),

    /// Invalid hostname
    #[error("Invalid hostname: {0}")]
    InvalidHostname(String),

    /// HSTS policy violation
    #[error("HSTS policy violation: {0}")]
    HstsViolation(String),
}

/// Result type for TLS operations
pub type TlsResult<T> = Result<T, TlsError>;

impl From<rustls::Error> for TlsError {
    fn from(err: rustls::Error) -> Self {
        TlsError::Rustls(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        // Arrange
        let error = TlsError::ConfigError("test error".to_string());

        // Act
        let display = format!("{error}");

        // Assert
        assert!(
            display.contains("test error"),
            "Error message should contain the description"
        );
    }

    #[test]
    fn test_error_from_io() {
        // Arrange
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");

        // Act
        let tls_error = TlsError::from(io_error);

        // Assert
        assert!(
            matches!(tls_error, TlsError::Io(_)),
            "Should convert IO error to TLS error"
        );
    }
}
