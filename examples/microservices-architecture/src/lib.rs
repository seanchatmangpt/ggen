use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Service {
    pub id: Uuid,
    pub name: String,
    pub port: u16,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceRegistry {
    services: Vec<Service>,
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceRegistry {
    pub fn new() -> Self {
        Self {
            services: Vec::new(),
        }
    }

    pub fn register(&mut self, service: Service) {
        self.services.push(service);
    }

    pub fn services(&self) -> &[Service] {
        &self.services
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_registration() {
        let mut registry = ServiceRegistry::new();
        let svc = Service {
            id: Uuid::new_v4(),
            name: "auth".to_string(),
            port: 8001,
        };
        registry.register(svc.clone());
        assert_eq!(registry.services().len(), 1);
    }

    #[test]
    fn test_multiple_services() {
        let mut registry = ServiceRegistry::new();
        for i in 0..3 {
            registry.register(Service {
                id: Uuid::new_v4(),
                name: format!("service{}", i),
                port: 8000 + i as u16,
            });
        }
        assert_eq!(registry.services().len(), 3);
    }
}
