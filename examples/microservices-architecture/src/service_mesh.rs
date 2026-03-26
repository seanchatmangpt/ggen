use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::SocketAddr;
use uuid::Uuid;
use dashmap::DashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ServiceStatus {
    Healthy,
    Degraded,
    Unhealthy,
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Service {
    pub id: Uuid,
    pub name: String,
    pub address: String,
    pub port: u16,
    pub status: ServiceStatus,
    pub replicas: usize,
}

impl Service {
    pub fn new(name: impl Into<String>, port: u16) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            address: "127.0.0.1".to_string(),
            port,
            status: ServiceStatus::Unknown,
            replicas: 1,
        }
    }

    pub fn with_address(mut self, address: impl Into<String>) -> Self {
        self.address = address.into();
        self
    }

    pub fn with_replicas(mut self, replicas: usize) -> Self {
        self.replicas = replicas;
        self
    }

    pub fn get_endpoint(&self) -> String {
        format!("{}:{}", self.address, self.port)
    }
}

#[derive(Debug, Clone)]
pub struct ServiceRegistry {
    services: Arc<DashMap<Uuid, Service>>,
}

impl ServiceRegistry {
    pub fn new() -> Self {
        Self {
            services: Arc::new(DashMap::new()),
        }
    }

    pub fn register(&self, service: Service) -> Result<()> {
        if self.services.contains_key(&service.id) {
            return Err(crate::error::MicroserviceError::RegistrationFailed(
                format!("Service {} already registered", service.name),
            ));
        }
        self.services.insert(service.id, service);
        Ok(())
    }

    pub fn deregister(&self, service_id: &Uuid) -> Result<()> {
        self.services.remove(service_id);
        Ok(())
    }

    pub fn get_service(&self, service_id: &Uuid) -> Option<Service> {
        self.services.get(service_id).map(|r| r.clone())
    }

    pub fn get_services_by_name(&self, name: &str) -> Vec<Service> {
        self.services
            .iter()
            .filter(|entry| entry.value().name == name)
            .map(|entry| entry.value().clone())
            .collect()
    }

    pub fn list_services(&self) -> Vec<Service> {
        self.services
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }

    pub fn update_status(&self, service_id: &Uuid, status: ServiceStatus) -> Result<()> {
        if let Some(mut service) = self.services.get_mut(service_id) {
            service.status = status;
            Ok(())
        } else {
            Err(crate::error::MicroserviceError::ServiceNotFound(
                format!("Service {:?} not found", service_id),
            ))
        }
    }
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ServiceMesh {
    registry: ServiceRegistry,
    routing_table: Arc<DashMap<String, Vec<Uuid>>>,
}

impl ServiceMesh {
    pub fn new(registry: ServiceRegistry) -> Self {
        Self {
            registry,
            routing_table: Arc::new(DashMap::new()),
        }
    }

    pub fn register_route(&self, route: String, service_ids: Vec<Uuid>) -> Result<()> {
        self.routing_table.insert(route, service_ids);
        Ok(())
    }

    pub fn get_route(&self, route: &str) -> Option<Vec<Uuid>> {
        self.routing_table.get(route).map(|r| r.clone())
    }

    pub fn discover_service(&self, service_name: &str) -> Result<Vec<Service>> {
        let services = self.registry.get_services_by_name(service_name);
        if services.is_empty() {
            Err(crate::error::MicroserviceError::ServiceNotFound(
                service_name.to_string(),
            ))
        } else {
            Ok(services)
        }
    }

    pub fn get_healthy_services(&self, service_name: &str) -> Result<Vec<Service>> {
        let services = self
            .registry
            .get_services_by_name(service_name)
            .into_iter()
            .filter(|s| s.status == ServiceStatus::Healthy)
            .collect::<Vec<_>>();

        if services.is_empty() {
            Err(crate::error::MicroserviceError::ServiceNotFound(
                format!("{} (healthy)", service_name),
            ))
        } else {
            Ok(services)
        }
    }

    pub fn get_registry(&self) -> &ServiceRegistry {
        &self.registry
    }
}

impl Default for ServiceMesh {
    fn default() -> Self {
        Self::new(ServiceRegistry::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_creation() {
        let service = Service::new("auth", 8001);
        assert_eq!(service.name, "auth");
        assert_eq!(service.port, 8001);
    }

    #[test]
    fn test_service_endpoint() {
        let service = Service::new("api", 8000);
        assert_eq!(service.get_endpoint(), "127.0.0.1:8000");
    }

    #[test]
    fn test_service_with_address() {
        let service = Service::new("api", 8000).with_address("192.168.1.1");
        assert_eq!(service.get_endpoint(), "192.168.1.1:8000");
    }

    #[test]
    fn test_registry_register() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        assert!(registry.register(service).is_ok());
    }

    #[test]
    fn test_registry_register_duplicate() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        registry.register(service.clone()).unwrap();
        assert!(registry.register(service).is_err());
    }

    #[test]
    fn test_registry_get_service() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        let id = service.id;
        registry.register(service).unwrap();

        let retrieved = registry.get_service(&id);
        assert!(retrieved.is_some());
    }

    #[test]
    fn test_registry_deregister() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        let id = service.id;
        registry.register(service).unwrap();
        assert!(registry.deregister(&id).is_ok());
        assert!(registry.get_service(&id).is_none());
    }

    #[test]
    fn test_registry_list_services() {
        let registry = ServiceRegistry::new();
        registry.register(Service::new("auth", 8001)).unwrap();
        registry.register(Service::new("api", 8000)).unwrap();

        let services = registry.list_services();
        assert_eq!(services.len(), 2);
    }

    #[test]
    fn test_registry_get_by_name() {
        let registry = ServiceRegistry::new();
        registry.register(Service::new("auth", 8001)).unwrap();
        registry.register(Service::new("api", 8000)).unwrap();

        let services = registry.get_services_by_name("auth");
        assert_eq!(services.len(), 1);
        assert_eq!(services[0].name, "auth");
    }

    #[test]
    fn test_registry_update_status() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        let id = service.id;
        registry.register(service).unwrap();

        assert!(registry
            .update_status(&id, ServiceStatus::Healthy)
            .is_ok());
        let updated = registry.get_service(&id).unwrap();
        assert_eq!(updated.status, ServiceStatus::Healthy);
    }

    #[test]
    fn test_service_mesh_creation() {
        let mesh = ServiceMesh::default();
        let services = mesh.get_registry().list_services();
        assert!(services.is_empty());
    }

    #[test]
    fn test_service_mesh_discover() {
        let registry = ServiceRegistry::new();
        registry.register(Service::new("auth", 8001)).unwrap();

        let mesh = ServiceMesh::new(registry);
        let services = mesh.discover_service("auth");
        assert!(services.is_ok());
    }

    #[test]
    fn test_service_mesh_healthy_services() {
        let registry = ServiceRegistry::new();
        let service = Service::new("auth", 8001);
        let id = service.id;
        registry.register(service).unwrap();
        registry.update_status(&id, ServiceStatus::Healthy).unwrap();

        let mesh = ServiceMesh::new(registry);
        let healthy = mesh.get_healthy_services("auth");
        assert!(healthy.is_ok());
        assert_eq!(healthy.unwrap().len(), 1);
    }
}
