use crate::service_mesh::{Service, ServiceMesh, ServiceStatus};
use crate::error::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use async_trait::async_trait;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceManagerAgent {
    pub id: Uuid,
    pub name: String,
    pub tasks_completed: usize,
}

impl ServiceManagerAgent {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            name: "ServiceManager".to_string(),
            tasks_completed: 0,
        }
    }

    pub async fn register_service(&mut self, mesh: &ServiceMesh, service: Service) -> Result<()> {
        mesh.get_registry().register(service)?;
        self.tasks_completed += 1;
        Ok(())
    }

    pub async fn deregister_service(&mut self, mesh: &ServiceMesh, service_id: &Uuid) -> Result<()> {
        mesh.get_registry().deregister(service_id)?;
        self.tasks_completed += 1;
        Ok(())
    }
}

impl Default for ServiceManagerAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckAgent {
    pub id: Uuid,
    pub name: String,
    pub checks_performed: usize,
}

impl HealthCheckAgent {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            name: "HealthCheck".to_string(),
            checks_performed: 0,
        }
    }

    pub async fn check_service(&mut self, mesh: &ServiceMesh, service_name: &str) -> Result<bool> {
        let services = mesh.discover_service(service_name)?;
        let healthy = !services.is_empty() && services.iter().all(|s| s.status == ServiceStatus::Healthy);
        self.checks_performed += 1;
        Ok(healthy)
    }

    pub async fn update_service_health(
        &mut self,
        mesh: &ServiceMesh,
        service_id: &Uuid,
        status: ServiceStatus,
    ) -> Result<()> {
        mesh.get_registry().update_status(service_id, status)?;
        self.checks_performed += 1;
        Ok(())
    }
}

impl Default for HealthCheckAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoadBalancerAgent {
    pub id: Uuid,
    pub name: String,
    pub requests_balanced: usize,
}

impl LoadBalancerAgent {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            name: "LoadBalancer".to_string(),
            requests_balanced: 0,
        }
    }

    pub async fn select_instance(&mut self, mesh: &ServiceMesh, service_name: &str) -> Result<Service> {
        let services = mesh.get_healthy_services(service_name)?;
        
        if services.is_empty() {
            return Err(crate::error::MicroserviceError::LoadBalancingFailed(
                "No healthy instances available".to_string(),
            ));
        }

        let index = self.requests_balanced % services.len();
        self.requests_balanced += 1;
        Ok(services[index].clone())
    }

    pub fn get_load_distribution(&self, total_services: usize) -> Vec<f64> {
        if total_services == 0 {
            return vec![];
        }
        vec![1.0 / total_services as f64; total_services]
    }
}

impl Default for LoadBalancerAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryAgent {
    pub id: Uuid,
    pub name: String,
    pub recoveries_attempted: usize,
}

impl RecoveryAgent {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            name: "Recovery".to_string(),
            recoveries_attempted: 0,
        }
    }

    pub async fn attempt_recovery(&mut self, mesh: &ServiceMesh, service_id: &Uuid) -> Result<()> {
        mesh.get_registry()
            .update_status(service_id, ServiceStatus::Healthy)?;
        self.recoveries_attempted += 1;
        Ok(())
    }

    pub async fn mark_unhealthy(&mut self, mesh: &ServiceMesh, service_id: &Uuid) -> Result<()> {
        mesh.get_registry()
            .update_status(service_id, ServiceStatus::Unhealthy)?;
        self.recoveries_attempted += 1;
        Ok(())
    }
}

impl Default for RecoveryAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_manager_creation() {
        let agent = ServiceManagerAgent::new();
        assert_eq!(agent.name, "ServiceManager");
    }

    #[tokio::test]
    async fn test_service_manager_register() {
        let mut manager = ServiceManagerAgent::new();
        let mesh = ServiceMesh::default();
        let service = Service::new("test", 8000);

        assert!(manager.register_service(&mesh, service).await.is_ok());
        assert_eq!(manager.tasks_completed, 1);
    }

    #[test]
    fn test_health_check_creation() {
        let agent = HealthCheckAgent::new();
        assert_eq!(agent.name, "HealthCheck");
    }

    #[test]
    fn test_load_balancer_creation() {
        let agent = LoadBalancerAgent::new();
        assert_eq!(agent.name, "LoadBalancer");
    }

    #[tokio::test]
    async fn test_load_balancer_distribution() {
        let lb = LoadBalancerAgent::new();
        let dist = lb.get_load_distribution(4);
        assert_eq!(dist.len(), 4);
        assert!((dist[0] - 0.25).abs() < 0.001);
    }

    #[test]
    fn test_recovery_agent_creation() {
        let agent = RecoveryAgent::new();
        assert_eq!(agent.name, "Recovery");
    }

    #[tokio::test]
    async fn test_recovery_attempt() {
        let mut recovery = RecoveryAgent::new();
        let mesh = ServiceMesh::default();
        let service = Service::new("test", 8000);
        let id = service.id;

        mesh.get_registry().register(service).unwrap();
        assert!(recovery.attempt_recovery(&mesh, &id).await.is_ok());
        assert_eq!(recovery.recoveries_attempted, 1);
    }
}
