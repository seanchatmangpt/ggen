use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Represents an API version
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ApiVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl ApiVersion {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self { major, minor, patch }
    }

    pub fn v1_0_0() -> Self {
        Self {
            major: 1,
            minor: 0,
            patch: 0,
        }
    }

    pub fn increment_major(&mut self) {
        self.major += 1;
        self.minor = 0;
        self.patch = 0;
    }

    pub fn increment_minor(&mut self) {
        self.minor += 1;
        self.patch = 0;
    }

    pub fn increment_patch(&mut self) {
        self.patch += 1;
    }

    pub fn to_string_format(&self) -> String {
        format!("{}.{}.{}", self.major, self.minor, self.patch)
    }

    pub fn to_path(&self) -> String {
        format!("/api/v{}", self.major)
    }
}

/// Endpoint status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EndpointStatus {
    Active,
    Deprecated,
    Sunset,
}

/// Endpoint definition with versioning info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Endpoint {
    pub path: String,
    pub method: String,
    pub name: String,
    pub description: Option<String>,
    pub version_introduced: ApiVersion,
    pub status: EndpointStatus,
    pub deprecated_at: Option<DateTime<Utc>>,
    pub sunset_at: Option<DateTime<Utc>>,
}

impl Endpoint {
    pub fn new(path: String, method: String, name: String) -> Self {
        Self {
            path,
            method,
            name,
            description: None,
            version_introduced: ApiVersion::v1_0_0(),
            status: EndpointStatus::Active,
            deprecated_at: None,
            sunset_at: None,
        }
    }

    pub fn with_version(mut self, version: ApiVersion) -> Self {
        self.version_introduced = version;
        self
    }

    pub fn deprecated(mut self) -> Self {
        self.status = EndpointStatus::Deprecated;
        self.deprecated_at = Some(Utc::now());
        self
    }

    pub fn sunset(mut self) -> Self {
        self.status = EndpointStatus::Sunset;
        self.sunset_at = Some(Utc::now());
        self
    }

    pub fn is_active(&self) -> bool {
        self.status == EndpointStatus::Active
    }

    pub fn is_deprecated(&self) -> bool {
        self.status == EndpointStatus::Deprecated
    }

    pub fn is_sunset(&self) -> bool {
        self.status == EndpointStatus::Sunset
    }
}

/// Deprecation policy for endpoints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeprecationPolicy {
    pub notice_period_days: i64,
    pub sunset_period_days: i64,
    pub replacement_endpoint: Option<String>,
}

impl DeprecationPolicy {
    pub fn new(notice_period_days: i64, sunset_period_days: i64) -> Self {
        Self {
            notice_period_days,
            sunset_period_days,
            replacement_endpoint: None,
        }
    }

    pub fn with_replacement(mut self, replacement: String) -> Self {
        self.replacement_endpoint = Some(replacement);
        self
    }

    pub fn default_policy() -> Self {
        Self {
            notice_period_days: 30,
            sunset_period_days: 90,
            replacement_endpoint: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_api_version_creation() {
        let version = ApiVersion::new(1, 2, 3);
        assert_eq!(version.major, 1);
        assert_eq!(version.minor, 2);
        assert_eq!(version.patch, 3);
    }

    #[test]
    fn test_api_version_to_string() {
        let version = ApiVersion::new(1, 2, 3);
        assert_eq!(version.to_string_format(), "1.2.3");
    }

    #[test]
    fn test_api_version_to_path() {
        let version = ApiVersion::new(2, 0, 0);
        assert_eq!(version.to_path(), "/api/v2");
    }

    #[test]
    fn test_api_version_increment_major() {
        let mut version = ApiVersion::new(1, 2, 3);
        version.increment_major();
        assert_eq!(version.to_string_format(), "2.0.0");
    }

    #[test]
    fn test_api_version_increment_minor() {
        let mut version = ApiVersion::new(1, 2, 3);
        version.increment_minor();
        assert_eq!(version.to_string_format(), "1.3.0");
    }

    #[test]
    fn test_api_version_increment_patch() {
        let mut version = ApiVersion::new(1, 2, 3);
        version.increment_patch();
        assert_eq!(version.to_string_format(), "1.2.4");
    }

    #[test]
    fn test_endpoint_creation() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        assert_eq!(endpoint.path, "/users");
        assert!(endpoint.is_active());
    }

    #[test]
    fn test_endpoint_deprecated() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string())
            .deprecated();
        assert!(endpoint.is_deprecated());
        assert!(!endpoint.is_active());
    }

    #[test]
    fn test_endpoint_sunset() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string())
            .sunset();
        assert!(endpoint.is_sunset());
    }

    #[test]
    fn test_deprecation_policy_creation() {
        let policy = DeprecationPolicy::new(30, 90);
        assert_eq!(policy.notice_period_days, 30);
        assert_eq!(policy.sunset_period_days, 90);
    }

    #[test]
    fn test_deprecation_policy_default() {
        let policy = DeprecationPolicy::default_policy();
        assert_eq!(policy.notice_period_days, 30);
        assert_eq!(policy.sunset_period_days, 90);
    }
}
