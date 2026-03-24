pub mod service_mesh;
pub mod agents;
pub mod mcp_tools;
pub mod fault_tolerance;
pub mod error;

pub use service_mesh::{Service, ServiceRegistry, ServiceMesh};
pub use agents::{ServiceManagerAgent, HealthCheckAgent, LoadBalancerAgent, RecoveryAgent};
pub use mcp_tools::{ToolRegistry, ServiceDiscoveryTool};
pub use fault_tolerance::{CircuitBreaker, ServiceHealthMonitor};
pub use error::{MicroserviceError, Result};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        let _ = std::any::type_name::<Service>();
        let _ = std::any::type_name::<ServiceRegistry>();
        let _ = std::any::type_name::<ServiceMesh>();
    }
}
