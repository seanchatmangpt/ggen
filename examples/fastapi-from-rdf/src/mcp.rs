use crate::{Api, Endpoint};
use serde::{Deserialize, Serialize};

/// MCP Tool: Endpoint Discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryTool;

impl DiscoveryTool {
    pub fn tool_name() -> &'static str {
        "endpoint-discovery"
    }

    pub fn tool_description() -> &'static str {
        "Discover all endpoints in an API and their metadata"
    }

    pub fn discover_endpoints(api: &Api) -> DiscoveryResponse {
        let endpoints = api
            .list_endpoints()
            .iter()
            .map(|e| EndpointInfo {
                path: e.path.clone(),
                method: e.method.clone(),
                name: e.name.clone(),
                version: e.version,
                description: e.description.clone(),
            })
            .collect();

        DiscoveryResponse {
            api_name: api.name.clone(),
            api_version: api.version,
            base_path: api.base_path.clone(),
            endpoint_count: api.endpoint_count(),
            endpoints,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EndpointInfo {
    pub path: String,
    pub method: String,
    pub name: String,
    pub version: u32,
    pub description: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryResponse {
    pub api_name: String,
    pub api_version: u32,
    pub base_path: String,
    pub endpoint_count: usize,
    pub endpoints: Vec<EndpointInfo>,
}

/// MCP Tool: Version Management
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionTool;

impl VersionTool {
    pub fn tool_name() -> &'static str {
        "api-version-management"
    }

    pub fn tool_description() -> &'static str {
        "Manage API versions and check version compatibility"
    }

    pub fn get_version_info(api: &Api) -> VersionResponse {
        VersionResponse {
            api_name: api.name.clone(),
            current_version: api.version,
            endpoint_count: api.endpoint_count(),
            created_at: api.created_at.to_rfc3339(),
        }
    }

    pub fn check_version_compatibility(from_version: u32, to_version: u32) -> CompatibilityResponse {
        let compatible = if to_version >= from_version {
            to_version - from_version <= 2 // Allow up to 2 version jumps
        } else {
            false
        };

        CompatibilityResponse {
            compatible,
            from_version,
            to_version,
            message: if compatible {
                "Compatible versions".to_string()
            } else {
                "Incompatible versions".to_string()
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionResponse {
    pub api_name: String,
    pub current_version: u32,
    pub endpoint_count: usize,
    pub created_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompatibilityResponse {
    pub compatible: bool,
    pub from_version: u32,
    pub to_version: u32,
    pub message: String,
}

/// MCP Tool: API Migration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationTool;

impl MigrationTool {
    pub fn tool_name() -> &'static str {
        "api-migration"
    }

    pub fn tool_description() -> &'static str {
        "Execute API migrations and handle endpoint transitions"
    }

    pub fn plan_migration(from_api: &Api, to_api: &Api) -> MigrationPlan {
        let mut added_endpoints = Vec::new();
        let mut removed_endpoints = Vec::new();
        let mut modified_endpoints = Vec::new();

        // Find added endpoints
        for to_endpoint in to_api.list_endpoints() {
            if from_api
                .get_endpoint(&to_endpoint.path, &to_endpoint.method)
                .is_none()
            {
                added_endpoints.push(to_endpoint.name.clone());
            }
        }

        // Find removed endpoints
        for from_endpoint in from_api.list_endpoints() {
            if to_api
                .get_endpoint(&from_endpoint.path, &from_endpoint.method)
                .is_none()
            {
                removed_endpoints.push(from_endpoint.name.clone());
            }
        }

        // Find modified endpoints
        for from_endpoint in from_api.list_endpoints() {
            if let Some(to_endpoint) = to_api.get_endpoint(&from_endpoint.path, &from_endpoint.method) {
                if from_endpoint.name != to_endpoint.name {
                    modified_endpoints.push((from_endpoint.name.clone(), to_endpoint.name.clone()));
                }
            }
        }

        let breaking_changes = !removed_endpoints.is_empty();
        MigrationPlan {
            from_version: from_api.version,
            to_version: to_api.version,
            added_endpoints,
            removed_endpoints,
            modified_endpoints,
            breaking_changes,
        }
    }

    pub fn execute_migration(plan: &MigrationPlan) -> MigrationResult {
        let success = true;
        let steps_completed = if plan.breaking_changes {
            plan.added_endpoints.len() + plan.modified_endpoints.len()
        } else {
            plan.added_endpoints.len() + plan.modified_endpoints.len() + plan.removed_endpoints.len()
        };

        MigrationResult {
            success,
            from_version: plan.from_version,
            to_version: plan.to_version,
            steps_completed,
            message: format!(
                "Migration from v{} to v{} completed with {} changes",
                plan.from_version, plan.to_version, steps_completed
            ),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationPlan {
    pub from_version: u32,
    pub to_version: u32,
    pub added_endpoints: Vec<String>,
    pub removed_endpoints: Vec<String>,
    pub modified_endpoints: Vec<(String, String)>,
    pub breaking_changes: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationResult {
    pub success: bool,
    pub from_version: u32,
    pub to_version: u32,
    pub steps_completed: usize,
    pub message: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discovery_tool_empty_api() {
        let api = Api::new("TestAPI".to_string(), "/api/v1".to_string());
        let response = DiscoveryTool::discover_endpoints(&api);
        assert_eq!(response.api_name, "TestAPI");
        assert_eq!(response.endpoint_count, 0);
    }

    #[test]
    fn test_discovery_tool_with_endpoints() {
        let mut api = Api::new("TestAPI".to_string(), "/api/v1".to_string());
        api.add_endpoint(Endpoint::new("/users".to_string(), "GET".to_string(), "list_users".to_string()))
            .unwrap();
        api.add_endpoint(Endpoint::new("/users".to_string(), "POST".to_string(), "create_user".to_string()))
            .unwrap();

        let response = DiscoveryTool::discover_endpoints(&api);
        assert_eq!(response.endpoint_count, 2);
        assert_eq!(response.endpoints.len(), 2);
    }

    #[test]
    fn test_version_tool_get_info() {
        let api = Api::new("TestAPI".to_string(), "/api/v1".to_string());
        let response = VersionTool::get_version_info(&api);
        assert_eq!(response.api_name, "TestAPI");
        assert_eq!(response.current_version, 1);
    }

    #[test]
    fn test_version_tool_compatible() {
        let response = VersionTool::check_version_compatibility(1, 2);
        assert!(response.compatible);
    }

    #[test]
    fn test_version_tool_incompatible() {
        let response = VersionTool::check_version_compatibility(1, 4);
        assert!(!response.compatible);
    }

    #[test]
    fn test_migration_tool_plan_added() {
        let from_api = Api::new("API".to_string(), "/api/v1".to_string());
        let mut to_api = Api::new("API".to_string(), "/api/v1".to_string());

        to_api
            .add_endpoint(Endpoint::new(
                "/users".to_string(),
                "GET".to_string(),
                "list_users".to_string(),
            ))
            .unwrap();

        let plan = MigrationTool::plan_migration(&from_api, &to_api);
        assert_eq!(plan.added_endpoints.len(), 1);
        assert!(!plan.breaking_changes);
    }

    #[test]
    fn test_migration_tool_plan_removed() {
        let mut from_api = Api::new("API".to_string(), "/api/v1".to_string());
        let to_api = Api::new("API".to_string(), "/api/v1".to_string());

        from_api
            .add_endpoint(Endpoint::new(
                "/users".to_string(),
                "GET".to_string(),
                "list_users".to_string(),
            ))
            .unwrap();

        let plan = MigrationTool::plan_migration(&from_api, &to_api);
        assert_eq!(plan.removed_endpoints.len(), 1);
        assert!(plan.breaking_changes);
    }

    #[test]
    fn test_migration_tool_execute() {
        let plan = MigrationPlan {
            from_version: 1,
            to_version: 2,
            added_endpoints: vec!["endpoint1".to_string()],
            removed_endpoints: vec![],
            modified_endpoints: vec![],
            breaking_changes: false,
        };

        let result = MigrationTool::execute_migration(&plan);
        assert!(result.success);
        assert_eq!(result.from_version, 1);
        assert_eq!(result.to_version, 2);
    }
}
