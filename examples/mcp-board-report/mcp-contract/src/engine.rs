//! Contract engine
//!
//! High-level API for managing contracts and their execution.

use crate::executor::{ContractExecutor, ExecutionResult};
use crate::state::ContractState;
use mcp_core::crypto::KeyPair;
use mcp_core::error::{McpError, McpResult};
use mcp_core::types::{Capability, Envelope, EvidenceBundle};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Contract definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractDefinition {
    /// Contract ID
    pub contract_id: String,
    /// Contract family
    pub contract_family: String,
    /// Contract version
    pub version: String,
    /// Envelope constraints
    pub envelope: Envelope,
    /// Contract description
    pub description: String,
}

impl ContractDefinition {
    /// Create a new contract definition
    pub fn new(
        contract_id: impl Into<String>,
        contract_family: impl Into<String>,
        envelope: Envelope,
    ) -> Self {
        Self {
            contract_id: contract_id.into(),
            contract_family: contract_family.into(),
            version: "1.0.0".to_string(),
            envelope,
            description: String::new(),
        }
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set version
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = version.into();
        self
    }
}

/// Contract engine for managing multiple contracts
pub struct ContractEngine {
    /// Signing key pair
    keypair: KeyPair,
    /// Registered contracts
    contracts: HashMap<String, (ContractDefinition, ContractState)>,
}

impl ContractEngine {
    /// Create a new contract engine
    pub fn new(keypair: KeyPair) -> Self {
        Self {
            keypair,
            contracts: HashMap::new(),
        }
    }

    /// Register a new contract
    pub fn register(&mut self, definition: ContractDefinition) -> McpResult<()> {
        if self.contracts.contains_key(&definition.contract_id) {
            return Err(McpError::ContractError(format!(
                "Contract '{}' already registered",
                definition.contract_id
            )));
        }

        let state = ContractState::new(
            &definition.contract_id,
            &definition.contract_family,
        );

        self.contracts.insert(
            definition.contract_id.clone(),
            (definition, state),
        );

        Ok(())
    }

    /// Get a contract's state
    pub fn get_state(&self, contract_id: &str) -> Option<&ContractState> {
        self.contracts.get(contract_id).map(|(_, state)| state)
    }

    /// Get a contract's definition
    pub fn get_definition(&self, contract_id: &str) -> Option<&ContractDefinition> {
        self.contracts.get(contract_id).map(|(def, _)| def)
    }

    /// Execute an operation on a contract
    pub fn execute<F, T>(
        &mut self,
        contract_id: &str,
        operation: &str,
        input: &[u8],
        required_capability: Capability,
        func: F,
    ) -> McpResult<ExecutionResult>
    where
        F: FnOnce() -> McpResult<T>,
        T: Serialize,
    {
        let (definition, state) = self.contracts.get_mut(contract_id)
            .ok_or_else(|| McpError::ContractError(format!(
                "Contract '{}' not found",
                contract_id
            )))?;

        let executor = ContractExecutor::new(
            self.keypair.clone(),
            definition.envelope.clone(),
        );

        Ok(executor.execute(state, operation, input, required_capability, func))
    }

    /// Activate global kill switch (affects all contracts)
    pub fn kill_global(&mut self) {
        for (_, state) in self.contracts.values_mut() {
            state.kill_switch.activate_global();
        }
    }

    /// Kill a specific contract family
    pub fn kill_family(&mut self, family: &str) {
        for (def, state) in self.contracts.values_mut() {
            if def.contract_family == family {
                state.kill_switch.kill_family(family);
            }
        }
    }

    /// Kill a specific contract
    pub fn kill_contract(&mut self, contract_id: &str) {
        if let Some((_, state)) = self.contracts.get_mut(contract_id) {
            state.kill_switch.activate_global();
        }
    }

    /// Rotate epoch for a contract
    pub fn rotate_epoch(&mut self, contract_id: &str) -> McpResult<u64> {
        let (_, state) = self.contracts.get_mut(contract_id)
            .ok_or_else(|| McpError::ContractError(format!(
                "Contract '{}' not found",
                contract_id
            )))?;

        state.rotate_epoch();
        Ok(state.current_epoch.epoch_number)
    }

    /// Generate evidence bundle for a contract
    pub fn generate_bundle(&self, contract_id: &str) -> McpResult<EvidenceBundle> {
        let (definition, state) = self.contracts.get(contract_id)
            .ok_or_else(|| McpError::ContractError(format!(
                "Contract '{}' not found",
                contract_id
            )))?;

        let mut bundle = EvidenceBundle::new_empty(&definition.contract_family);
        bundle.receipt_chain_root = state.receipt_chain_root.clone();
        bundle.receipt_count = state.total_operations;
        bundle.refusal_count = state.total_refusals;

        Ok(bundle)
    }

    /// List all registered contract IDs
    pub fn list_contracts(&self) -> Vec<&str> {
        self.contracts.keys().map(|s| s.as_str()).collect()
    }

    /// Get metrics for all contracts
    pub fn metrics(&self) -> EngineMetrics {
        let mut metrics = EngineMetrics::default();

        for (_, state) in self.contracts.values() {
            metrics.total_contracts += 1;
            metrics.total_operations += state.total_operations;
            metrics.total_refusals += state.total_refusals;
            if !state.can_execute() {
                metrics.killed_contracts += 1;
            }
        }

        metrics
    }
}

/// Engine-wide metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EngineMetrics {
    pub total_contracts: u64,
    pub total_operations: u64,
    pub total_refusals: u64,
    pub killed_contracts: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_engine() -> ContractEngine {
        let keypair = KeyPair::generate().unwrap();
        ContractEngine::new(keypair)
    }

    #[test]
    fn test_register_contract() {
        let mut engine = setup_engine();

        let def = ContractDefinition::new(
            "contract-001",
            "test-family",
            Envelope::default(),
        ).with_description("Test contract");

        engine.register(def).unwrap();

        assert!(engine.get_definition("contract-001").is_some());
        assert!(engine.get_state("contract-001").is_some());
    }

    #[test]
    fn test_register_duplicate_fails() {
        let mut engine = setup_engine();

        let def = ContractDefinition::new("c1", "family", Envelope::default());
        engine.register(def.clone()).unwrap();

        let result = engine.register(def);
        assert!(result.is_err());
    }

    #[test]
    fn test_execute_operation() {
        let mut engine = setup_engine();

        let def = ContractDefinition::new("c1", "family", Envelope::default());
        engine.register(def).unwrap();

        let result = engine.execute(
            "c1",
            "test.op",
            b"input",
            Capability::Read,
            || Ok("output"),
        ).unwrap();

        assert!(result.is_success());
    }

    #[test]
    fn test_execute_nonexistent_contract() {
        let mut engine = setup_engine();

        let result = engine.execute(
            "nonexistent",
            "op",
            b"input",
            Capability::Read,
            || Ok(()),
        );

        assert!(result.is_err());
    }

    #[test]
    fn test_kill_global() {
        let mut engine = setup_engine();

        let def1 = ContractDefinition::new("c1", "family", Envelope::default());
        let def2 = ContractDefinition::new("c2", "family", Envelope::default());
        engine.register(def1).unwrap();
        engine.register(def2).unwrap();

        engine.kill_global();

        assert!(!engine.get_state("c1").unwrap().can_execute());
        assert!(!engine.get_state("c2").unwrap().can_execute());
    }

    #[test]
    fn test_kill_family() {
        let mut engine = setup_engine();

        let def1 = ContractDefinition::new("c1", "target", Envelope::default());
        let def2 = ContractDefinition::new("c2", "other", Envelope::default());
        engine.register(def1).unwrap();
        engine.register(def2).unwrap();

        engine.kill_family("target");

        assert!(!engine.get_state("c1").unwrap().can_execute());
        assert!(engine.get_state("c2").unwrap().can_execute());
    }

    #[test]
    fn test_rotate_epoch() {
        let mut engine = setup_engine();

        let def = ContractDefinition::new("c1", "family", Envelope::default());
        engine.register(def).unwrap();

        let new_epoch = engine.rotate_epoch("c1").unwrap();
        assert_eq!(new_epoch, 2);

        let state = engine.get_state("c1").unwrap();
        assert_eq!(state.current_epoch.epoch_number, 2);
    }

    #[test]
    fn test_generate_bundle() {
        let mut engine = setup_engine();

        let def = ContractDefinition::new("c1", "family", Envelope::default());
        engine.register(def).unwrap();

        // Execute some operations
        for _ in 0..3 {
            engine.execute("c1", "op", b"data", Capability::Read, || Ok(1)).unwrap();
        }

        let bundle = engine.generate_bundle("c1").unwrap();
        assert_eq!(bundle.receipt_count, 3);
        assert_eq!(bundle.contract_family, "family");
    }

    #[test]
    fn test_engine_metrics() {
        let mut engine = setup_engine();

        let def1 = ContractDefinition::new("c1", "family", Envelope::default());
        let def2 = ContractDefinition::new("c2", "family", Envelope::default());
        engine.register(def1).unwrap();
        engine.register(def2).unwrap();

        // Execute operations
        engine.execute("c1", "op", b"data", Capability::Read, || Ok(1)).unwrap();
        engine.execute("c1", "op", b"data", Capability::Read, || Ok(2)).unwrap();
        engine.execute("c2", "op", b"data", Capability::Read, || Ok(3)).unwrap();

        // Kill one contract
        engine.kill_contract("c2");

        let metrics = engine.metrics();
        assert_eq!(metrics.total_contracts, 2);
        assert_eq!(metrics.total_operations, 3);
        assert_eq!(metrics.killed_contracts, 1);
    }

    #[test]
    fn test_list_contracts() {
        let mut engine = setup_engine();

        let def1 = ContractDefinition::new("alpha", "family", Envelope::default());
        let def2 = ContractDefinition::new("beta", "family", Envelope::default());
        engine.register(def1).unwrap();
        engine.register(def2).unwrap();

        let contracts = engine.list_contracts();
        assert_eq!(contracts.len(), 2);
        assert!(contracts.contains(&"alpha"));
        assert!(contracts.contains(&"beta"));
    }
}
