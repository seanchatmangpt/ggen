//! Knowledge Hook Composition: Chaining Operations Across Sectors
//!
//! Enables composing operations from multiple sectors into deterministic chains.
//! A composed operation chains knowledge hooks in sequence, with output of one
//! becoming input to the next.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single step in an operation chain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompositionStep {
    /// Step identifier
    pub id: String,
    /// Sector this step operates in
    pub sector: String,
    /// Operation/hook to execute
    pub operation: String,
    /// Input for this step (can use previous step's output)
    pub input: String,
    /// Output from this step
    pub output: String,
    /// Execution order (lower first)
    pub order: u32,
}

impl CompositionStep {
    /// Create a new composition step
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: uses String::new()
    pub fn new(id: String, sector: String, operation: String, input: String) -> Self {
        Self { id, sector, operation, input, output: String::new(), order: 0 }
    }

    /// Set execution order
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub fn with_order(mut self, order: u32) -> Self {
        self.order = order;
        self
    }
}

/// A composition of operations across sectors
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationChain {
    /// Unique chain ID
    pub id: String,
    /// Display name
    pub name: String,
    /// Steps in this chain
    steps: Vec<CompositionStep>,
    /// Current execution step
    current_step: usize,
    /// Whether chain has completed
    is_completed: bool,
    /// Determinism flag (chain is deterministic if all steps are)
    is_deterministic: bool,
}

impl OperationChain {
    /// Create a new operation chain
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: uses Vec::new()
    pub fn new(id: String, name: String) -> Self {
        Self {
            id,
            name,
            steps: Vec::new(),
            current_step: 0,
            is_completed: false,
            is_deterministic: true,
        }
    }

    /// Add a step to the chain
    pub fn add_step(&mut self, step: CompositionStep) {
        self.steps.push(step);
        // Auto-order by insertion order if not specified
        self.steps.sort_by(|a, b| a.order.cmp(&b.order));
    }

    /// Get current step
    #[must_use]
    pub fn current(&self) -> Option<&CompositionStep> {
        self.steps.get(self.current_step)
    }

    /// Get current step (mutable)
    pub fn current_mut(&mut self) -> Option<&mut CompositionStep> {
        self.steps.get_mut(self.current_step)
    }

    /// Move to next step
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub fn advance(&mut self) -> bool {
        if self.current_step < self.steps.len() - 1 {
            self.current_step += 1;
            true
        } else {
            self.is_completed = true;
            false
        }
    }

    /// Get all steps
    #[must_use]
    pub fn steps(&self) -> &[CompositionStep] {
        &self.steps
    }

    /// Check if chain is complete
    #[must_use]
    pub const fn is_completed(&self) -> bool {
        self.is_completed
    }

    /// Check if chain is deterministic
    #[must_use]
    pub const fn is_deterministic(&self) -> bool {
        self.is_deterministic
    }

    /// Get step count
    #[must_use]
    pub const fn step_count(&self) -> usize {
        self.steps.len()
    }

    /// Get sectors involved
    #[must_use]
    pub fn sectors(&self) -> Vec<String> {
        let mut sectors = self.steps.iter().map(|s| s.sector.clone()).collect::<Vec<_>>();
        sectors.sort();
        sectors.dedup();
        sectors
    }
}

/// Result of composing operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComposedOperation {
    /// Composed operation ID
    pub id: String,
    /// Operation chain
    pub chain: OperationChain,
    /// Final result
    pub result: String,
    /// Merkle root of entire composition (for proof)
    pub composition_merkle: String,
    /// Execution trace (step → output)
    pub trace: HashMap<String, String>,
    /// Total execution time in ms
    pub total_time_ms: u64,
}

impl ComposedOperation {
    /// Create a new composed operation
    #[must_use]
    pub fn new(id: String, chain: OperationChain) -> Self {
        Self {
            id,
            chain,
            result: String::new(),
            composition_merkle: String::new(),
            trace: HashMap::new(),
            total_time_ms: 0,
        }
    }

    /// Add result from a step
    pub fn record_step_result(&mut self, step_id: String, result: String) {
        self.trace.insert(step_id, result);
    }

    /// Set final result
    pub fn set_result(&mut self, result: String) {
        self.result = result;
    }

    /// Set composition merkle (proof of composition)
    pub fn set_merkle(&mut self, merkle: String) {
        self.composition_merkle = merkle;
    }

    /// Set total execution time
    #[allow(clippy::missing_const_for_fn)] // Mutating function cannot be const
    pub fn set_total_time(&mut self, ms: u64) {
        self.total_time_ms = ms;
    }

    /// Check if composition succeeded
    #[must_use]
    pub const fn is_success(&self) -> bool {
        self.chain.is_completed && !self.result.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_composition_step() {
        let step = CompositionStep::new(
            "step-1".to_string(),
            "Academic".to_string(),
            "desk-review".to_string(),
            "paper".to_string(),
        );

        assert_eq!(step.id, "step-1");
        assert_eq!(step.sector, "Academic");
        assert_eq!(step.order, 0);
    }

    #[test]
    fn test_operation_chain_creation() {
        let chain = OperationChain::new("chain-1".to_string(), "Academic Review".to_string());

        assert_eq!(chain.id, "chain-1");
        assert_eq!(chain.step_count(), 0);
        assert!(!chain.is_completed());
    }

    #[test]
    fn test_operation_chain_add_steps() {
        let mut chain = OperationChain::new("chain-1".to_string(), "Review".to_string());

        chain.add_step(
            CompositionStep::new(
                "s1".to_string(),
                "Academic".to_string(),
                "desk-review".to_string(),
                "paper".to_string(),
            )
            .with_order(1),
        );

        chain.add_step(
            CompositionStep::new(
                "s2".to_string(),
                "Academic".to_string(),
                "assignment".to_string(),
                "paper".to_string(),
            )
            .with_order(2),
        );

        assert_eq!(chain.step_count(), 2);
        assert!(chain.current().is_some());
    }

    #[test]
    fn test_operation_chain_advancement() {
        let mut chain = OperationChain::new("chain-1".to_string(), "Chain".to_string());

        chain.add_step(CompositionStep::new(
            "s1".to_string(),
            "Academic".to_string(),
            "op".to_string(),
            "data".to_string(),
        ));
        chain.add_step(CompositionStep::new(
            "s2".to_string(),
            "Academic".to_string(),
            "op".to_string(),
            "data".to_string(),
        ));

        assert!(!chain.is_completed());

        chain.advance();
        assert!(!chain.is_completed());

        chain.advance();
        assert!(chain.is_completed());
    }

    #[test]
    fn test_operation_chain_sectors() {
        let mut chain = OperationChain::new("chain-1".to_string(), "Chain".to_string());

        chain.add_step(CompositionStep::new(
            "s1".to_string(),
            "Academic".to_string(),
            "op".to_string(),
            "data".to_string(),
        ));
        chain.add_step(CompositionStep::new(
            "s2".to_string(),
            "Claims".to_string(),
            "op".to_string(),
            "data".to_string(),
        ));

        let sectors = chain.sectors();
        assert_eq!(sectors.len(), 2);
        assert!(sectors.contains(&"Academic".to_string()));
        assert!(sectors.contains(&"Claims".to_string()));
    }

    #[test]
    fn test_composed_operation() {
        let chain = OperationChain::new("chain-1".to_string(), "Multi-Sector".to_string());
        let composed = ComposedOperation::new("comp-1".to_string(), chain);

        assert_eq!(composed.id, "comp-1");
        assert!(!composed.is_success());
    }

    #[test]
    fn test_composed_operation_results() {
        let chain = OperationChain::new("chain-1".to_string(), "Chain".to_string());
        let mut composed = ComposedOperation::new("comp-1".to_string(), chain);

        composed.record_step_result("step-1".to_string(), "result-1".to_string());
        assert_eq!(composed.trace.len(), 1);
    }
}
