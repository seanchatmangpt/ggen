use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

mod agents;
mod errors;
mod mcp;
mod versioning;

pub use agents::{
    CompatibilityAgent, DeprecationAgent, MigrationAgent as ApiMigrationAgent, VersionManagerAgent,
};
pub use errors::{ApiError, ApiResult};
pub use mcp::{DiscoveryTool, MigrationTool, VersionTool};
pub use versioning::{ApiVersion, DeprecationPolicy};

/// Represents a FastAPI endpoint
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Endpoint {
    pub path: String,
    pub method: String,
    pub name: String,
    pub version: u32,
    pub description: Option<String>,
}

impl Endpoint {
    pub fn new(path: String, method: String, name: String) -> Self {
        Self {
            path,
            method,
            name,
            version: 1,
            description: None,
        }
    }

    pub fn with_description(mut self, desc: String) -> Self {
        self.description = Some(desc);
        self
    }

    pub fn with_version(mut self, version: u32) -> Self {
        self.version = version;
        self
    }
}

/// Represents an API with endpoints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Api {
    pub name: String,
    pub version: u32,
    pub endpoints: Vec<Endpoint>,
    pub created_at: DateTime<Utc>,
    pub base_path: String,
}

impl Api {
    pub fn new(name: String, base_path: String) -> Self {
        Self {
            name,
            version: 1,
            endpoints: Vec::new(),
            created_at: Utc::now(),
            base_path,
        }
    }

    pub fn add_endpoint(&mut self, endpoint: Endpoint) -> ApiResult<()> {
        // Check for duplicate paths with same method
        if self
            .endpoints
            .iter()
            .any(|e| e.path == endpoint.path && e.method == endpoint.method)
        {
            return Err(ApiError::Conflict(format!(
                "Endpoint {} {} already exists",
                endpoint.method, endpoint.path
            )));
        }

        self.endpoints.push(endpoint);
        Ok(())
    }

    pub fn remove_endpoint(&mut self, path: &str, method: &str) -> ApiResult<()> {
        let initial_len = self.endpoints.len();
        self.endpoints
            .retain(|e| !(e.path == path && e.method == method));

        if self.endpoints.len() == initial_len {
            return Err(ApiError::NotFound(format!(
                "Endpoint {} {} not found",
                method, path
            )));
        }

        Ok(())
    }

    pub fn get_endpoint(&self, path: &str, method: &str) -> Option<&Endpoint> {
        self.endpoints
            .iter()
            .find(|e| e.path == path && e.method == method)
    }

    pub fn list_endpoints(&self) -> Vec<&Endpoint> {
        self.endpoints.iter().collect()
    }

    pub fn endpoint_count(&self) -> usize {
        self.endpoints.len()
    }
}

impl Default for Api {
    fn default() -> Self {
        Self::new("default".to_string(), "/api/v1".to_string())
    }
}

/// API compatibility information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompatibilityInfo {
    pub is_backward_compatible: bool,
    pub breaking_changes: Vec<String>,
    pub deprecated_endpoints: Vec<String>,
    pub new_endpoints: Vec<String>,
}

impl CompatibilityInfo {
    pub fn new() -> Self {
        Self {
            is_backward_compatible: true,
            breaking_changes: Vec::new(),
            deprecated_endpoints: Vec::new(),
            new_endpoints: Vec::new(),
        }
    }

    pub fn with_breaking_change(mut self, change: String) -> Self {
        self.is_backward_compatible = false;
        self.breaking_changes.push(change);
        self
    }

    pub fn add_deprecated(mut self, endpoint: String) -> Self {
        self.deprecated_endpoints.push(endpoint);
        self
    }

    pub fn add_new(mut self, endpoint: String) -> Self {
        self.new_endpoints.push(endpoint);
        self
    }
}

impl Default for CompatibilityInfo {
    fn default() -> Self {
        Self::new()
    }
}

/// Endpoint deprecation notice
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeprecationNotice {
    pub endpoint: String,
    pub deprecated_at: DateTime<Utc>,
    pub sunset_at: DateTime<Utc>,
    pub replacement: Option<String>,
    pub message: String,
}

impl DeprecationNotice {
    pub fn new(endpoint: String, message: String, sunset_days: i64) -> Self {
        let now = Utc::now();
        Self {
            endpoint,
            deprecated_at: now,
            sunset_at: now + chrono::Duration::days(sunset_days),
            replacement: None,
            message,
        }
    }

    pub fn with_replacement(mut self, replacement: String) -> Self {
        self.replacement = Some(replacement);
        self
    }

    pub fn is_sunset(&self) -> bool {
        Utc::now() > self.sunset_at
    }
}

/// API migration from one version to another
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Migration {
    pub id: String,
    pub from_version: u32,
    pub to_version: u32,
    pub changes: Vec<MigrationStep>,
    pub status: MigrationStatus,
    pub executed_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MigrationStatus {
    Planned,
    InProgress,
    Completed,
    Failed,
    Rolled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MigrationStep {
    AddEndpoint(Endpoint),
    RemoveEndpoint { path: String, method: String },
    ModifyEndpoint { path: String, method: String, new_endpoint: Endpoint },
    Deprecate { path: String, method: String, replacement: Option<String> },
}

impl Migration {
    pub fn new(from_version: u32, to_version: u32) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            from_version,
            to_version,
            changes: Vec::new(),
            status: MigrationStatus::Planned,
            executed_at: None,
        }
    }

    pub fn add_step(&mut self, step: MigrationStep) {
        self.changes.push(step);
    }

    pub fn mark_in_progress(&mut self) {
        self.status = MigrationStatus::InProgress;
    }

    pub fn mark_completed(&mut self) {
        self.status = MigrationStatus::Completed;
        self.executed_at = Some(Utc::now());
    }

    pub fn mark_failed(&mut self) {
        self.status = MigrationStatus::Failed;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_endpoint_creation() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        assert_eq!(endpoint.path, "/users");
        assert_eq!(endpoint.method, "GET");
        assert_eq!(endpoint.name, "list_users");
        assert_eq!(endpoint.version, 1);
    }

    #[test]
    fn test_endpoint_with_description() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string())
            .with_description("List all users".to_string());
        assert_eq!(endpoint.description, Some("List all users".to_string()));
    }

    #[test]
    fn test_endpoint_with_version() {
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string())
            .with_version(2);
        assert_eq!(endpoint.version, 2);
    }

    #[test]
    fn test_api_creation() {
        let api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        assert_eq!(api.name, "MyAPI");
        assert_eq!(api.base_path, "/api/v1");
        assert_eq!(api.version, 1);
        assert!(api.endpoints.is_empty());
    }

    #[test]
    fn test_api_add_endpoint() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        assert!(api.add_endpoint(endpoint).is_ok());
        assert_eq!(api.endpoint_count(), 1);
    }

    #[test]
    fn test_api_add_duplicate_endpoint_fails() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        assert!(api.add_endpoint(endpoint.clone()).is_ok());
        assert!(api.add_endpoint(endpoint).is_err());
    }

    #[test]
    fn test_api_remove_endpoint() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        api.add_endpoint(endpoint).unwrap();
        assert!(api.remove_endpoint("/users", "GET").is_ok());
        assert_eq!(api.endpoint_count(), 0);
    }

    #[test]
    fn test_api_remove_nonexistent_endpoint_fails() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        assert!(api.remove_endpoint("/users", "GET").is_err());
    }

    #[test]
    fn test_api_get_endpoint() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        api.add_endpoint(endpoint).unwrap();
        assert!(api.get_endpoint("/users", "GET").is_some());
    }

    #[test]
    fn test_compatibility_info_creation() {
        let info = CompatibilityInfo::new();
        assert!(info.is_backward_compatible);
        assert!(info.breaking_changes.is_empty());
    }

    #[test]
    fn test_compatibility_info_breaking_change() {
        let info = CompatibilityInfo::new()
            .with_breaking_change("Removed /users endpoint".to_string());
        assert!(!info.is_backward_compatible);
    }

    #[test]
    fn test_deprecation_notice_creation() {
        let notice = DeprecationNotice::new(
            "/old-endpoint".to_string(),
            "Use /new-endpoint instead".to_string(),
            30,
        );
        assert_eq!(notice.endpoint, "/old-endpoint");
        assert!(!notice.is_sunset());
    }

    #[test]
    fn test_deprecation_notice_with_replacement() {
        let notice = DeprecationNotice::new(
            "/old-endpoint".to_string(),
            "Use /new-endpoint instead".to_string(),
            30,
        )
        .with_replacement("/new-endpoint".to_string());
        assert_eq!(notice.replacement, Some("/new-endpoint".to_string()));
    }

    #[test]
    fn test_migration_creation() {
        let migration = Migration::new(1, 2);
        assert_eq!(migration.from_version, 1);
        assert_eq!(migration.to_version, 2);
        assert_eq!(migration.status, MigrationStatus::Planned);
    }

    #[test]
    fn test_migration_status_transitions() {
        let mut migration = Migration::new(1, 2);
        assert_eq!(migration.status, MigrationStatus::Planned);

        migration.mark_in_progress();
        assert_eq!(migration.status, MigrationStatus::InProgress);

        migration.mark_completed();
        assert_eq!(migration.status, MigrationStatus::Completed);
        assert!(migration.executed_at.is_some());
    }

    #[test]
    fn test_migration_add_steps() {
        let mut migration = Migration::new(1, 2);
        let endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        migration.add_step(MigrationStep::AddEndpoint(endpoint));
        assert_eq!(migration.changes.len(), 1);
    }

    #[test]
    fn test_api_list_endpoints() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        api.add_endpoint(Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string()))
            .unwrap();
        api.add_endpoint(Endpoint::new("/users".to_string(), "POST".to_string(), "create_user".to_string()))
            .unwrap();
        assert_eq!(api.list_endpoints().len(), 2);
    }

    #[test]
    fn test_multiple_methods_same_path() {
        let mut api = Api::new("MyAPI".to_string(), "/api/v1".to_string());
        let get_endpoint = Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string());
        let post_endpoint = Endpoint::new("/users".to_string(), "POST".to_string(), "create_user".to_string());

        assert!(api.add_endpoint(get_endpoint).is_ok());
        assert!(api.add_endpoint(post_endpoint).is_ok());
        assert_eq!(api.endpoint_count(), 2);
    }
}
