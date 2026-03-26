use crate::service_mesh::{ServiceMesh, Service};
use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceDiscoveryTool {
    pub id: String,
    pub name: String,
}

impl ServiceDiscoveryTool {
    pub fn new() -> Self {
        Self {
            id: "discover-services".to_string(),
            name: "Service Discovery".to_string(),
        }
    }

    pub async fn discover(&self, mesh: &ServiceMesh, service_name: &str) -> Result<Vec<Service>> {
        mesh.discover_service(service_name)
    }
}

impl Default for ServiceDiscoveryTool {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckTool {
    pub id: String,
    pub name: String,
}

impl HealthCheckTool {
    pub fn new() -> Self {
        Self {
            id: "health-check".to_string(),
            name: "Health Check".to_string(),
        }
    }
}

impl Default for HealthCheckTool {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoadBalancingTool {
    pub id: String,
    pub name: String,
}

impl LoadBalancingTool {
    pub fn new() -> Self {
        Self {
            id: "load-balance".to_string(),
            name: "Load Balancing".to_string(),
        }
    }
}

impl Default for LoadBalancingTool {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ToolRegistry {
    tools: HashMap<String, String>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    pub fn register_tool(&mut self, tool_id: String, tool_name: String) -> Result<()> {
        if self.tools.contains_key(&tool_id) {
            return Err(crate::error::MicroserviceError::ToolExecutionFailed(
                format!("Tool {} already registered", tool_id),
            ));
        }
        self.tools.insert(tool_id, tool_name);
        Ok(())
    }

    pub fn get_tool(&self, tool_id: &str) -> Option<&str> {
        self.tools.get(tool_id).map(|s| s.as_str())
    }

    pub fn list_tools(&self) -> Vec<(&str, &str)> {
        self.tools
            .iter()
            .map(|(id, name)| (id.as_str(), name.as_str()))
            .collect()
    }

    pub fn discover_tools(&self, capability: &str) -> Vec<(&str, &str)> {
        self.tools
            .iter()
            .filter(|(_, name)| name.to_lowercase().contains(&capability.to_lowercase()))
            .map(|(id, name)| (id.as_str(), name.as_str()))
            .collect()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_default_tools() -> ToolRegistry {
    let mut registry = ToolRegistry::new();

    let _ = registry.register_tool("discover-services".to_string(), "Service Discovery".to_string());
    let _ = registry.register_tool("health-check".to_string(), "Health Check".to_string());
    let _ = registry.register_tool("load-balance".to_string(), "Load Balancing".to_string());
    let _ = registry.register_tool("recover-service".to_string(), "Service Recovery".to_string());

    registry
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_discovery_tool() {
        let tool = ServiceDiscoveryTool::new();
        assert_eq!(tool.id, "discover-services");
    }

    #[test]
    fn test_health_check_tool() {
        let tool = HealthCheckTool::new();
        assert_eq!(tool.id, "health-check");
    }

    #[test]
    fn test_load_balancing_tool() {
        let tool = LoadBalancingTool::new();
        assert_eq!(tool.id, "load-balance");
    }

    #[test]
    fn test_tool_registry_creation() {
        let registry = ToolRegistry::new();
        assert!(registry.list_tools().is_empty());
    }

    #[test]
    fn test_register_tool() {
        let mut registry = ToolRegistry::new();
        assert!(registry
            .register_tool("test".to_string(), "Test Tool".to_string())
            .is_ok());
        assert_eq!(registry.list_tools().len(), 1);
    }

    #[test]
    fn test_register_duplicate_tool() {
        let mut registry = ToolRegistry::new();
        registry
            .register_tool("test".to_string(), "Test Tool".to_string())
            .unwrap();
        let result = registry.register_tool("test".to_string(), "Test Tool".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_get_tool() {
        let mut registry = ToolRegistry::new();
        registry
            .register_tool("test".to_string(), "Test Tool".to_string())
            .unwrap();
        assert_eq!(registry.get_tool("test"), Some("Test Tool"));
    }

    #[test]
    fn test_discover_tools() {
        let registry = create_default_tools();
        let discovered = registry.discover_tools("health");
        assert!(discovered.iter().any(|(_, name)| *name == "Health Check"));
    }

    #[test]
    fn test_create_default_tools() {
        let registry = create_default_tools();
        assert_eq!(registry.list_tools().len(), 4);
    }

    #[test]
    fn test_list_tools() {
        let registry = create_default_tools();
        let tools = registry.list_tools();
        assert!(tools.iter().any(|(id, _)| *id == "discover-services"));
        assert!(tools.iter().any(|(id, _)| *id == "health-check"));
    }
}
