//! HTTP Strict Transport Security (HSTS) implementation

use super::error::{TlsError, TlsResult};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::RwLock;

/// HSTS policy configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HstsPolicy {
    /// Maximum age in seconds (RFC 6797 recommends at least 31536000 = 1 year)
    pub max_age: Duration,
    /// Include subdomains
    pub include_subdomains: bool,
    /// Enable HSTS preload (requires includeSubDomains and max-age >= 1 year)
    pub preload: bool,
}

impl Default for HstsPolicy {
    fn default() -> Self {
        Self {
            max_age: Duration::from_secs(31_536_000), // 1 year
            include_subdomains: true,
            preload: false,
        }
    }
}

impl HstsPolicy {
    /// Create a strict HSTS policy
    #[must_use]
    pub fn strict() -> Self {
        Self {
            max_age: Duration::from_secs(63_072_000), // 2 years
            include_subdomains: true,
            preload: true,
        }
    }

    /// Create HSTS policy for development (short max-age)
    #[must_use]
    pub fn development() -> Self {
        Self {
            max_age: Duration::from_secs(300), // 5 minutes
            include_subdomains: false,
            preload: false,
        }
    }

    /// Get the HSTS header value
    #[must_use]
    pub fn header_value(&self) -> String {
        let mut parts = vec![format!("max-age={}", self.max_age.as_secs())];

        if self.include_subdomains {
            parts.push("includeSubDomains".to_string());
        }

        if self.preload {
            parts.push("preload".to_string());
        }

        parts.join("; ")
    }

    /// Validate policy configuration
    ///
    /// # Errors
    /// Returns `TlsError` if policy is invalid
    pub fn validate(&self) -> TlsResult<()> {
        // Preload requires includeSubDomains and max-age >= 1 year
        if self.preload {
            if !self.include_subdomains {
                return Err(TlsError::HstsViolation(
                    "HSTS preload requires includeSubDomains".to_string(),
                ));
            }

            if self.max_age < Duration::from_secs(31_536_000) {
                return Err(TlsError::HstsViolation(
                    "HSTS preload requires max-age >= 31536000 (1 year)".to_string(),
                ));
            }
        }

        Ok(())
    }
}

/// HSTS entry for a specific host
#[derive(Debug, Clone)]
struct HstsEntry {
    /// When this entry was created
    created_at: SystemTime,
    /// Policy for this host
    policy: HstsPolicy,
}

impl HstsEntry {
    fn new(policy: HstsPolicy) -> Self {
        Self {
            created_at: SystemTime::now(),
            policy,
        }
    }

    fn is_expired(&self) -> bool {
        if let Ok(elapsed) = self.created_at.elapsed() {
            elapsed > self.policy.max_age
        } else {
            true // If we can't determine elapsed time, consider it expired
        }
    }

    fn should_enforce(&self) -> bool {
        !self.is_expired()
    }
}

/// HSTS enforcement manager
pub struct HstsMiddleware {
    /// HSTS cache keyed by hostname
    cache: Arc<RwLock<HashMap<String, HstsEntry>>>,
    /// Default policy for new entries
    default_policy: HstsPolicy,
}

impl HstsMiddleware {
    /// Create a new HSTS middleware
    #[must_use]
    pub fn new(default_policy: HstsPolicy) -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            default_policy,
        }
    }

    /// Add HSTS policy for a hostname
    pub async fn add_host(&self, hostname: String, policy: HstsPolicy) -> TlsResult<()> {
        policy.validate()?;

        let mut cache = self.cache.write().await;
        cache.insert(hostname, HstsEntry::new(policy));

        Ok(())
    }

    /// Check if HTTPS should be enforced for a hostname
    #[must_use]
    pub async fn should_enforce_https(&self, hostname: &str) -> bool {
        let cache = self.cache.read().await;

        if let Some(entry) = cache.get(hostname) {
            if entry.should_enforce() {
                return true;
            }
        }

        // Check parent domains if includeSubDomains is set
        if let Some(parent) = self.get_parent_domain(hostname) {
            if let Some(entry) = cache.get(parent) {
                if entry.policy.include_subdomains && entry.should_enforce() {
                    return true;
                }
            }
        }

        false
    }

    /// Process HSTS header from server response
    ///
    /// # Errors
    /// Returns `TlsError` if header is malformed
    pub async fn process_header(&self, hostname: String, header_value: &str) -> TlsResult<()> {
        let policy = Self::parse_header(header_value)?;
        self.add_host(hostname, policy).await
    }

    /// Parse HSTS header value
    fn parse_header(header_value: &str) -> TlsResult<HstsPolicy> {
        let mut max_age = None;
        let mut include_subdomains = false;
        let mut preload = false;

        for part in header_value.split(';') {
            let part = part.trim();

            if let Some(age_str) = part.strip_prefix("max-age=") {
                max_age = Some(
                    age_str
                        .parse::<u64>()
                        .map_err(|e| TlsError::HstsViolation(format!("Invalid max-age: {e}")))?,
                );
            } else if part.eq_ignore_ascii_case("includeSubDomains") {
                include_subdomains = true;
            } else if part.eq_ignore_ascii_case("preload") {
                preload = true;
            }
        }

        let max_age = max_age.ok_or_else(|| {
            TlsError::HstsViolation("Missing required max-age directive".to_string())
        })?;

        let policy = HstsPolicy {
            max_age: Duration::from_secs(max_age),
            include_subdomains,
            preload,
        };

        policy.validate()?;
        Ok(policy)
    }

    /// Get parent domain (e.g., "www.example.com" -> "example.com")
    fn get_parent_domain<'a>(&self, hostname: &'a str) -> Option<&'a str> {
        hostname.split_once('.').map(|(_, parent)| parent)
    }

    /// Clean up expired entries
    pub async fn cleanup_expired(&self) {
        let mut cache = self.cache.write().await;
        cache.retain(|_, entry| !entry.is_expired());
    }

    /// Get the default policy
    #[must_use]
    pub fn default_policy(&self) -> &HstsPolicy {
        &self.default_policy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hsts_policy_default() {
        // Arrange & Act
        let policy = HstsPolicy::default();

        // Assert
        assert_eq!(policy.max_age, Duration::from_secs(31_536_000));
        assert!(policy.include_subdomains);
        assert!(!policy.preload);
    }

    #[test]
    fn test_hsts_policy_strict() {
        // Arrange & Act
        let policy = HstsPolicy::strict();

        // Assert
        assert_eq!(policy.max_age, Duration::from_secs(63_072_000));
        assert!(policy.include_subdomains);
        assert!(policy.preload);
    }

    #[test]
    fn test_hsts_policy_header_value() {
        // Arrange
        let policy = HstsPolicy {
            max_age: Duration::from_secs(31_536_000),
            include_subdomains: true,
            preload: true,
        };

        // Act
        let header = policy.header_value();

        // Assert
        assert!(header.contains("max-age=31536000"));
        assert!(header.contains("includeSubDomains"));
        assert!(header.contains("preload"));
    }

    #[test]
    fn test_hsts_policy_validate_success() {
        // Arrange
        let policy = HstsPolicy::strict();

        // Act
        let result = policy.validate();

        // Assert
        assert!(result.is_ok(), "Valid policy should pass validation");
    }

    #[test]
    fn test_hsts_policy_validate_preload_requires_subdomains() {
        // Arrange
        let policy = HstsPolicy {
            max_age: Duration::from_secs(31_536_000),
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
    }

    #[test]
    fn test_hsts_policy_validate_preload_requires_long_age() {
        // Arrange
        let policy = HstsPolicy {
            max_age: Duration::from_secs(300),
            include_subdomains: true,
            preload: true,
        };

        // Act
        let result = policy.validate();

        // Assert
        assert!(result.is_err(), "Preload with short max-age should fail");
    }

    #[tokio::test]
    async fn test_hsts_middleware_creation() {
        // Arrange
        let policy = HstsPolicy::default();

        // Act
        let middleware = HstsMiddleware::new(policy);

        // Assert
        assert_eq!(
            middleware.default_policy().max_age,
            Duration::from_secs(31_536_000)
        );
    }

    #[tokio::test]
    async fn test_add_host() {
        // Arrange
        let middleware = HstsMiddleware::new(HstsPolicy::default());
        let policy = HstsPolicy::default();

        // Act
        let result = middleware.add_host("example.com".to_string(), policy).await;

        // Assert
        assert!(result.is_ok(), "Adding host should succeed");
    }

    #[tokio::test]
    async fn test_should_enforce_https() {
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

    #[tokio::test]
    async fn test_should_not_enforce_unknown_host() {
        // Arrange
        let middleware = HstsMiddleware::new(HstsPolicy::default());

        // Act
        let should_enforce = middleware.should_enforce_https("unknown.com").await;

        // Assert
        assert!(
            !should_enforce,
            "HTTPS should not be enforced for unknown host"
        );
    }

    #[tokio::test]
    async fn test_include_subdomains() {
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
        assert!(
            should_enforce,
            "HTTPS should be enforced for subdomain when includeSubDomains is set"
        );
    }

    #[tokio::test]
    async fn test_parse_header_valid() {
        // Arrange
        let header = "max-age=31536000; includeSubDomains; preload";

        // Act
        let result = HstsMiddleware::parse_header(header);

        // Assert
        assert!(result.is_ok(), "Valid header should parse successfully");
        let policy = result.expect("Policy should exist");
        assert_eq!(policy.max_age, Duration::from_secs(31_536_000));
        assert!(policy.include_subdomains);
        assert!(policy.preload);
    }

    #[tokio::test]
    async fn test_parse_header_missing_max_age() {
        // Arrange
        let header = "includeSubDomains";

        // Act
        let result = HstsMiddleware::parse_header(header);

        // Assert
        assert!(result.is_err(), "Header without max-age should fail");
    }

    #[tokio::test]
    async fn test_process_header() {
        // Arrange
        let middleware = HstsMiddleware::new(HstsPolicy::default());
        let header = "max-age=31536000; includeSubDomains";

        // Act
        let result = middleware
            .process_header("example.com".to_string(), header)
            .await;

        // Assert
        assert!(result.is_ok(), "Processing valid header should succeed");
        assert!(middleware.should_enforce_https("example.com").await);
    }
}
