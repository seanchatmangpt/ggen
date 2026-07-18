//! Coordinator for property-based validation
//!
//! Manages the 7 property agents and aggregates their results.

use crate::agents::PropertyAgent;
use crate::properties::{PropertyCheck, PropertyResult, PropertyType};
use std::collections::HashMap;
use std::sync::Arc;

/// Coordinator for property-based validation
///
/// Manages 7 property agents and orchestrates validation checks.
pub struct PropertyCoordinator {
    /// Map of property type to agent
    agents: HashMap<PropertyType, PropertyAgent>,
}

impl PropertyCoordinator {
    /// Create a new property coordinator
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let agents = PropertyAgent::create_all()
            .into_iter()
            .map(|agent| (agent.property_type.clone(), agent))
            .collect();

        Ok(Self { agents })
    }

    /// Check a specific property using its assigned agent
    ///
    /// # Arguments
    /// * `property_type` - Which property to check
    /// * `package_path` - Path to ggen-generated package
    /// * `generation_receipt` - BLAKE3 receipt from ggen μ₅
    ///
    /// # Returns
    /// Property check result with violations if any
    pub async fn check_property(
        &self,
        property_type: &PropertyType,
        package_path: &str,
        generation_receipt: &str,
    ) -> Result<PropertyResult, Box<dyn std::error::Error>> {
        let agent = self
            .agents
            .get(property_type)
            .ok_or_else(|| format!("No agent found for property: {:?}", property_type))?;

        tracing::debug!(
            agent = %agent.id,
            property = %property_type,
            "Dispatching property check"
        );

        agent.check(package_path, generation_receipt).await
    }

    /// Get all registered agents
    pub fn agents(&self) -> Vec<&PropertyAgent> {
        self.agents.values().collect()
    }

    /// Get agent by property type
    pub fn get_agent(&self, property_type: &PropertyType) -> Option<&PropertyAgent> {
        self.agents.get(property_type)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_coordinator_creation() {
        let coordinator = PropertyCoordinator::new().await.unwrap();
        assert_eq!(coordinator.agents().len(), 7);
    }

    #[tokio::test]
    async fn test_get_agent_by_property() {
        let coordinator = PropertyCoordinator::new().await.unwrap();
        let agent = coordinator.get_agent(&PropertyType::Determinism);
        assert!(agent.is_some());
        assert_eq!(agent.unwrap().property_type, PropertyType::Determinism);
    }
}
