//! Contract executor
//!
//! Executes operations within envelope constraints, generating
//! receipts or refusals as appropriate.

use crate::state::ContractState;
// Timestamp from mcp_core types
use mcp_core::crypto::{hash_sha256, hash_sha256_bytes, KeyPair};
use mcp_core::error::{McpError, McpResult};
use mcp_core::refusal::{Refusal, RefusalCode};
use mcp_core::types::{Capability, Envelope, ExecutionMetrics, Receipt};
use serde::{Deserialize, Serialize};
use std::time::Instant;

/// Result of contract execution
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ExecutionResult {
    /// Operation succeeded, receipt generated
    Success {
        receipt: Receipt,
        output_hash: String,
    },
    /// Operation refused
    Refused {
        refusal: Refusal,
    },
}

impl ExecutionResult {
    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        matches!(self, ExecutionResult::Success { .. })
    }

    /// Get the receipt if successful
    pub fn receipt(&self) -> Option<&Receipt> {
        match self {
            ExecutionResult::Success { receipt, .. } => Some(receipt),
            ExecutionResult::Refused { .. } => None,
        }
    }

    /// Get the refusal if refused
    pub fn refusal(&self) -> Option<&Refusal> {
        match self {
            ExecutionResult::Success { .. } => None,
            ExecutionResult::Refused { refusal } => Some(refusal),
        }
    }
}

/// Contract executor that enforces envelopes
pub struct ContractExecutor {
    /// Signing key pair
    keypair: KeyPair,
    /// Envelope constraints
    envelope: Envelope,
}

impl ContractExecutor {
    /// Create a new executor with the given key pair and envelope
    pub fn new(keypair: KeyPair, envelope: Envelope) -> Self {
        Self { keypair, envelope }
    }

    /// Execute an operation
    pub fn execute<F, T>(
        &self,
        state: &mut ContractState,
        operation: &str,
        input: &[u8],
        required_capability: Capability,
        func: F,
    ) -> ExecutionResult
    where
        F: FnOnce() -> McpResult<T>,
        T: Serialize,
    {
        let input_hash = hash_sha256(input);
        let start = Instant::now();

        // Check 1: Kill switch
        if !state.can_execute() {
            let code = if state.kill_switch.global_active {
                RefusalCode::kill_global()
            } else if state.kill_switch.is_family_killed(&state.contract_family) {
                RefusalCode::kill_family(&state.contract_family)
            } else {
                RefusalCode::kill_epoch(state.current_epoch.epoch_number)
            };

            let refusal = Refusal::new(
                code,
                "Contract execution blocked by kill switch",
                operation,
                &input_hash,
            ).with_contract(&state.contract_id);

            state.add_refusal();
            return ExecutionResult::Refused { refusal };
        }

        // Check 2: Capability
        if !self.envelope.has_capability(required_capability) {
            let refusal = Refusal::new(
                RefusalCode::cap_unauthorized(required_capability.as_str()),
                format!("Capability '{}' not in envelope", required_capability.as_str()),
                operation,
                &input_hash,
            ).with_contract(&state.contract_id);

            state.add_refusal();
            return ExecutionResult::Refused { refusal };
        }

        // Check 3: Operation count
        if let Some(max_ops) = self.envelope.max_operations {
            if state.total_operations >= max_ops {
                let refusal = Refusal::new(
                    RefusalCode::res_quota_exceeded("operations"),
                    format!("Max operations ({}) exceeded", max_ops),
                    operation,
                    &input_hash,
                ).with_contract(&state.contract_id);

                state.add_refusal();
                return ExecutionResult::Refused { refusal };
            }
        }

        // Execute the operation
        match func() {
            Ok(output) => {
                let duration = start.elapsed();

                // Check 4: Duration limit
                if let Some(max_duration_ms) = self.envelope.max_duration_ms {
                    if duration.as_millis() as u64 > max_duration_ms {
                        let refusal = Refusal::new(
                            RefusalCode::res_quota_exceeded("duration"),
                            format!("Execution exceeded {}ms limit", max_duration_ms),
                            operation,
                            &input_hash,
                        ).with_contract(&state.contract_id);

                        state.add_refusal();
                        return ExecutionResult::Refused { refusal };
                    }
                }

                // Serialize output and hash it
                let output_json = serde_json::to_vec(&output).unwrap_or_default();
                let output_hash = hash_sha256(&output_json);

                // Calculate metrics
                let utilization = self.envelope.max_operations
                    .map(|max| (state.total_operations + 1) as f64 / max as f64)
                    .unwrap_or(0.0);

                let metrics = ExecutionMetrics {
                    duration_us: duration.as_micros() as u64,
                    memory_bytes: output_json.len() as u64,
                    operations: 1,
                    envelope_utilization: utilization,
                };

                // Create receipt
                let prev_hash = if state.receipt_tree.is_empty() {
                    mcp_core::GENESIS_HASH.to_string()
                } else {
                    state.receipt_chain_root.clone()
                };

                let mut receipt = Receipt::new(
                    state.total_operations + 1,
                    prev_hash,
                    operation,
                    &state.contract_id,
                    input_hash,
                    &output_hash,
                    metrics,
                );

                // Sign the receipt
                receipt.sign(&self.keypair);

                // Update state
                let receipt_hash = hash_sha256_bytes(receipt.receipt_hash.as_bytes());
                state.add_receipt(&receipt_hash);

                ExecutionResult::Success {
                    receipt,
                    output_hash,
                }
            }
            Err(e) => {
                // Operation failed - generate refusal
                let refusal = Refusal::new(
                    RefusalCode::val_invalid_input("operation", &e.to_string()),
                    e.to_string(),
                    operation,
                    &input_hash,
                ).with_contract(&state.contract_id);

                state.add_refusal();
                ExecutionResult::Refused { refusal }
            }
        }
    }

    /// Execute a read-only operation (no state changes)
    pub fn execute_read<F, T>(
        &self,
        state: &ContractState,
        _operation: &str,
        func: F,
    ) -> McpResult<T>
    where
        F: FnOnce() -> McpResult<T>,
    {
        // Check kill switch
        if !state.can_execute() {
            return Err(McpError::KillSwitchActive(
                "Contract execution blocked".to_string()
            ));
        }

        // Check read capability
        if !self.envelope.has_capability(Capability::Read) {
            return Err(McpError::EnvelopeViolation(
                "Read capability not in envelope".to_string()
            ));
        }

        func()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (ContractExecutor, ContractState) {
        let keypair = KeyPair::generate().unwrap();
        let envelope = Envelope::default()
            .with_capability(Capability::Write)
            .with_capability(Capability::Execute);
        let executor = ContractExecutor::new(keypair, envelope);
        let state = ContractState::new("test-contract", "test-family");
        (executor, state)
    }

    #[test]
    fn test_successful_execution() {
        let (executor, mut state) = setup();

        let result = executor.execute(
            &mut state,
            "test.operation",
            b"input data",
            Capability::Read,
            || Ok("success"),
        );

        assert!(result.is_success());
        assert!(result.receipt().is_some());
        assert_eq!(state.total_operations, 1);
    }

    #[test]
    fn test_execution_with_signed_receipt() {
        let keypair = KeyPair::generate().unwrap();
        let envelope = Envelope::default();
        let executor = ContractExecutor::new(keypair.clone(), envelope);
        let mut state = ContractState::new("c1", "family");

        let result = executor.execute(
            &mut state,
            "op",
            b"data",
            Capability::Read,
            || Ok(42),
        );

        if let ExecutionResult::Success { receipt, .. } = result {
            assert!(receipt.signature.is_some());
            assert!(receipt.verify(&keypair).is_ok());
        } else {
            panic!("Expected success");
        }
    }

    #[test]
    fn test_refusal_unauthorized_capability() {
        let (executor, mut state) = setup();

        // Admin is not in the envelope
        let result = executor.execute(
            &mut state,
            "admin.operation",
            b"input",
            Capability::Admin,
            || Ok("should not run"),
        );

        assert!(!result.is_success());
        let refusal = result.refusal().expect("should have refusal");
        assert!(refusal.code.to_string_code().contains("UNAUTHORIZED"));
        assert_eq!(state.total_refusals, 1);
    }

    #[test]
    fn test_refusal_kill_switch_global() {
        let (executor, mut state) = setup();
        state.kill_switch.activate_global();

        let result = executor.execute(
            &mut state,
            "any.operation",
            b"input",
            Capability::Read,
            || Ok("blocked"),
        );

        assert!(!result.is_success());
        let refusal = result.refusal().expect("should have refusal");
        assert!(refusal.is_kill_switch());
    }

    #[test]
    fn test_refusal_max_operations() {
        let keypair = KeyPair::generate().unwrap();
        let envelope = Envelope::default().with_max_operations(2);
        let executor = ContractExecutor::new(keypair, envelope);
        let mut state = ContractState::new("c1", "family");

        // First two succeed
        for _ in 0..2 {
            let result = executor.execute(
                &mut state,
                "op",
                b"data",
                Capability::Read,
                || Ok(1),
            );
            assert!(result.is_success());
        }

        // Third is refused
        let result = executor.execute(
            &mut state,
            "op",
            b"data",
            Capability::Read,
            || Ok(1),
        );
        assert!(!result.is_success());
        assert!(result.refusal().unwrap().code.to_string_code().contains("QUOTA"));
    }

    #[test]
    fn test_read_only_execution() {
        let (executor, state) = setup();

        let result: McpResult<i32> = executor.execute_read(
            &state,
            "read.data",
            || Ok(42),
        );

        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_read_blocked_by_kill_switch() {
        let (executor, mut state) = setup();
        state.kill_switch.activate_global();

        let result: McpResult<i32> = executor.execute_read(
            &state,
            "read.data",
            || Ok(42),
        );

        assert!(result.is_err());
    }

    #[test]
    fn test_operation_failure_generates_refusal() {
        let (executor, mut state) = setup();

        let result = executor.execute(
            &mut state,
            "failing.operation",
            b"input",
            Capability::Read,
            || Err::<(), _>(McpError::InvalidInput("bad input".to_string())),
        );

        assert!(!result.is_success());
        let refusal = result.refusal().expect("should have refusal");
        assert!(refusal.reason.contains("bad input"));
    }

    #[test]
    fn test_receipt_chain_integrity() {
        let (executor, mut state) = setup();

        // Execute multiple operations
        for i in 0..3 {
            let result = executor.execute(
                &mut state,
                "op",
                format!("input-{}", i).as_bytes(),
                Capability::Read,
                || Ok(i),
            );
            assert!(result.is_success());
        }

        // Verify receipt chain is built
        assert_eq!(state.total_operations, 3);
        assert_eq!(state.receipt_tree.len(), 3);
        assert_ne!(state.receipt_chain_root, mcp_core::GENESIS_HASH);
    }
}
