//! Domain Management System
//!
//! Manages life domains within the OSIRIS system

use crate::{OSIRISSignal, SignalLevel};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Represents a life domain in the OSIRIS system
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Domain {
    /// Unique domain identifier
    pub id: String,
    /// Human-readable domain name
    pub name: String,
    /// Domain description
    pub description: Option<String>,
    /// Domain status
    pub status: DomainStatus,
    /// Domain configuration
    pub config: Value,
    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// Last update timestamp
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

/// Domain status lifecycle
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DomainStatus {
    /// Domain is being created
    Creating,
    /// Domain is active and operational
    Active,
    /// Domain is paused
    Paused,
    /// Domain is undergoing maintenance
    Maintenance,
    /// Domain is disabled
    Disabled,
    /// Domain is being deleted
    Deleting,
    /// Domain has been deleted
    Deleted,
}

impl Domain {
    /// Create a new domain
    pub fn new(id: String, name: String) -> Self {
        Self {
            id,
            name,
            description: None,
            status: DomainStatus::Creating,
            config: json!({}),
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }

    /// Create a new domain with description
    pub fn with_description(id: String, name: String, description: String) -> Self {
        let mut domain = Self::new(id, name);
        domain.description = Some(description);
        domain
    }

    /// Update domain status
    pub fn update_status(&mut self, status: DomainStatus) {
        self.status = status;
        self.updated_at = chrono::Utc::now();
    }

    /// Update domain configuration
    pub fn update_config(&mut self, config: Value) {
        self.config = config;
        self.updated_at = chrono::Utc::now();
    }

    /// Set domain description
    pub fn set_description(&mut self, description: String) {
        self.description = Some(description);
        self.updated_at = chrono::Utc::now();
    }

    /// Check if domain is active
    pub fn is_active(&self) -> bool {
        matches!(self.status, DomainStatus::Active)
    }

    /// Check if domain can execute patterns
    pub fn can_execute(&self) -> bool {
        matches!(self.status, DomainStatus::Active | DomainStatus::Maintenance)
    }
}

/// Domain registry for managing all domains
pub struct DomainRegistry {
    domains: Arc<RwLock<HashMap<String, Domain>>>,
    max_domains: usize,
}

impl DomainRegistry {
    /// Create a new domain registry
    pub fn new(max_domains: usize) -> Self {
        Self {
            domains: Arc::new(RwLock::new(HashMap::new())),
            max_domains,
        }
    }

    /// Register a new domain
    pub async fn register(&mut self, domain: Domain) -> Result<(), String> {
        let mut domains = self.domains.write().await;

        // Check domain limit
        if domains.len() >= self.max_domains {
            let error = format!("Domain limit reached: {}", self.max_domains);
            warn!("{}", error);
            return Err(error);
        }

        // Check if domain already exists
        if domains.contains_key(&domain.id) {
            let error = format!("Domain already exists: {}", domain.id);
            warn!("{}", error);
            return Err(error);
        }

        // Update status to active
        let mut domain_with_status = domain;
        domain_with_status.update_status(DomainStatus::Active);

        domains.insert(domain_with_status.id.clone(), domain_with_status);

        info!("Domain registered: {}", domain_with_status.id);

        // Emit domain registration signal
        let signal = OSIRISSignal::new(
            "domain_registered",
            format!("Domain {} registered successfully", domain_with_status.id),
            SignalLevel::Info,
        );

        // Signal would be emitted here in full implementation
        debug!("Domain registration signal: {}", signal.message);

        Ok(())
    }

    /// Unregister a domain
    pub async fn unregister(&mut self, domain_id: &str) -> Result<(), String> {
        let mut domains = self.domains.write().await;

        if let Some(mut domain) = domains.get_mut(domain_id) {
            domain.update_status(DomainStatus::Deleting);

            // Emit domain deletion signal
            let signal = OSIRISSignal::new(
                "domain_unregistered",
                format!("Domain {} is being deleted", domain_id),
                SignalLevel::Info,
            );

            // Signal would be emitted here in full implementation
            debug!("Domain deletion signal: {}", signal.message);

            domains.remove(domain_id);

            info!("Domain unregistered: {}", domain_id);
            Ok(())
        } else {
            let error = format!("Domain not found: {}", domain_id);
            warn!("{}", error);
            Err(error)
        }
    }

    /// Get a domain by ID
    pub async fn get(&self, domain_id: &str) -> Option<Domain> {
        let domains = self.domains.read().await;
        domains.get(domain_id).cloned()
    }

    /// Get all domains
    pub async fn list(&self) -> Vec<Domain> {
        let domains = self.domains.read().await;
        domains.values().cloned().collect()
    }

    /// Update domain status
    pub async fn update_status(&mut self, domain_id: &str, status: DomainStatus) -> Result<(), String> {
        let mut domains = self.domains.write().await;

        if let Some(domain) = domains.get_mut(domain_id) {
            domain.update_status(status);

            // Emit domain status change signal
            let signal = OSIRISSignal::new(
                "domain_status_changed",
                format!("Domain {} status changed to {:?}", domain_id, status),
                SignalLevel::Info,
            );

            // Signal would be emitted here in full implementation
            debug!("Domain status change signal: {}", signal.message);

            Ok(())
        } else {
            let error = format!("Domain not found: {}", domain_id);
            warn!("{}", error);
            Err(error)
        }
    }

    /// Update domain configuration
    pub async fn update_config(&mut self, domain_id: &str, config: Value) -> Result<(), String> {
        let mut domains = self.domains.write().await;

        if let Some(domain) = domains.get_mut(domain_id) {
            domain.update_config(config);

            // Emit domain config update signal
            let signal = OSIRISSignal::new(
                "domain_config_updated",
                format!("Domain {} configuration updated", domain_id),
                SignalLevel::Info,
            );

            // Signal would be emitted here in full implementation
            debug!("Domain config update signal: {}", signal.message);

            Ok(())
        } else {
            let error = format!("Domain not found: {}", domain_id);
            warn!("{}", error);
            Err(error)
        }
    }

    /// Get active domains
    pub async fn get_active_domains(&self) -> Vec<Domain> {
        let domains = self.domains.read().await;
        domains
            .values()
            .filter(|domain| domain.is_active())
            .cloned()
            .collect()
    }

    /// Get domains that can execute patterns
    pub async fn get_executable_domains(&self) -> Vec<Domain> {
        let domains = self.domains.read().await;
        domains
            .values()
            .filter(|domain| domain.can_execute())
            .cloned()
            .collect()
    }

    /// Get domain count
    pub async fn count(&self) -> usize {
        let domains = self.domains.read().await;
        domains.len()
    }

    /// Check if domain exists
    pub async fn exists(&self, domain_id: &str) -> bool {
        let domains = self.domains.read().await;
        domains.contains_key(domain_id)
    }

    /// Clear all domains (for testing/reset)
    pub async fn clear(&mut self) {
        let mut domains = self.domains.write().await;
        domains.clear();
        info!("All domains cleared");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    #[test]
    fn test_domain_creation() {
        let domain = Domain::new("test-domain".to_string(), "Test Domain".to_string());
        assert_eq!(domain.id, "test-domain");
        assert_eq!(domain.name, "Test Domain");
        assert!(domain.description.is_none());
        assert_eq!(domain.status, DomainStatus::Creating);
        assert_eq!(domain.config, json!({}));
    }

    #[test]
    fn test_domain_with_description() {
        let domain = Domain::with_description(
            "test-domain".to_string(),
            "Test Domain".to_string(),
            "Test description".to_string(),
        );
        assert_eq!(domain.id, "test-domain");
        assert_eq!(domain.name, "Test Domain");
        assert_eq!(domain.description, Some("Test description".to_string()));
    }

    #[test]
    fn test_domain_status_update() {
        let mut domain = Domain::new("test-domain".to_string(), "Test Domain".to_string());
        let original_updated = domain.updated_at;

        std::thread::sleep(std::time::Duration::from_millis(1));

        domain.update_status(DomainStatus::Active);
        assert_eq!(domain.status, DomainStatus::Active);
        assert!(domain.updated_at > original_updated);
    }

    #[test]
    fn test_domain_state_checks() {
        let mut domain = Domain::new("test-domain".to_string(), "Test Domain".to_string());

        assert!(!domain.is_active());
        assert!(!domain.can_execute());

        domain.update_status(DomainStatus::Active);
        assert!(domain.is_active());
        assert!(domain.can_execute());

        domain.update_status(DomainStatus::Paused);
        assert!(!domain.is_active());
        assert!(!domain.can_execute());

        domain.update_status(DomainStatus::Maintenance);
        assert!(!domain.is_active());
        assert!(domain.can_execute());
    }

    #[tokio::test]
    async fn test_domain_registry_operations() {
        let mut registry = DomainRegistry::new(10);

        let domain = Domain::new("test-domain".to_string(), "Test Domain".to_string());
        let result = registry.register(domain).await;
        assert!(result.is_ok());

        // Check domain exists
        assert!(registry.exists("test-domain").await);
        assert_eq!(registry.count().await, 1);

        // Get domain
        let retrieved = registry.get("test-domain").await;
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name, "Test Domain");

        // Update status
        let result = registry.update_status("test-domain", DomainStatus::Paused).await;
        assert!(result.is_ok());

        // Get active domains
        let active = registry.get_active_domains().await;
        assert_eq!(active.len(), 0);

        // Unregister domain
        let result = registry.unregister("test-domain").await;
        assert!(result.is_ok);
        assert_eq!(registry.count().await, 0);
    }

    #[tokio::test]
    async fn test_domain_registry_limit() {
        let mut registry = DomainRegistry::new(2);

        // Register two domains
        let domain1 = Domain::new("domain1".to_string(), "Domain 1".to_string());
        let domain2 = Domain::new("domain2".to_string(), "Domain 2".to_string());

        assert!(registry.register(domain1).await.is_ok());
        assert!(registry.register(domain2).await.is_ok());

        // Try to register third domain - should fail
        let domain3 = Domain::new("domain3".to_string(), "Domain 3".to_string());
        let result = registry.register(domain3).await;
        assert!(result.is_err());
        assert_eq!(registry.count().await, 2);
    }

    #[tokio::test]
    async fn test_domain_registry_duplicates() {
        let mut registry = DomainRegistry::new(10);

        let domain1 = Domain::new("domain1".to_string(), "Domain 1".to_string());
        let domain2 = Domain::new("domain1".to_string(), "Domain 1 Duplicate".to_string());

        assert!(registry.register(domain1).await.is_ok());
        assert!(registry.register(domain2).await.is_err()); // Should fail - duplicate ID
        assert_eq!(registry.count().await, 1);
    }
}