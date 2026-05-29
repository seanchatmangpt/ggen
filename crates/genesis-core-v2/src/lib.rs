//! KNHK V2 Core - Pattern trait system and pattern implementations
//!
//! This is the foundation of KNHK V2. It defines:
//! - The Pattern trait that all 43 YAWL patterns implement
//! - Pattern registry for discovery and execution
//! - Composition system for building complex workflows
//! - Zero-copy, zero-allocation execution paths

use async_trait::async_trait;
use dashmap::DashMap;
use genesis_schema::PatternMetadata;
use genesis_types::{Error, ExecutionContext, PatternId, Result, WorkflowStep};
use serde_json::json;
use std::sync::Arc;

pub mod inventory;
pub mod patterns;
pub mod primitives;
pub mod registry;
pub mod revelation;
pub mod split_laws;

pub use inventory::{ArtifactStatus, ClassifiedArtifact, ConnectionMechanism, FinishStep};
pub use primitives::{
    Construct8, Pair2, Receipt, Refusal, RefusalReason, RelationPage, ReplayCursor,
};
pub use registry::PatternRegistry;
pub use revelation::{
    passes_all_gates, verify_lamb_authority, BabylonClaim, Church, ChurchJudgment, ChurchVerdict,
    JerusalemGate, Plague, PlagueRecord, Seal, SealState,
};
pub use split_laws::{need257_split, need9_split, SplitResult};

/// Core Pattern trait - all 43 YAWL patterns implement this
///
/// This trait enables:
/// - Async execution of workflow steps
/// - Composition of patterns into complex workflows
/// - Zero-coupling from observability
/// - Deterministic, measurable behavior
#[async_trait]
pub trait Pattern: Send + Sync {
    /// Pattern metadata
    fn metadata(&self) -> &PatternMetadata;

    /// Execute this pattern step
    ///
    /// Input: ExecutionContext with current data
    /// Output: Updated context with results
    ///
    /// CRITICAL: Must be ≤5µs hot path, non-blocking
    async fn execute(&self, context: &mut ExecutionContext) -> Result<()>;

    /// Validate inputs before execution
    fn validate(&self, context: &ExecutionContext) -> Result<()>;

    /// Get next steps in execution (for control flow patterns)
    fn get_next_steps(&self, context: &ExecutionContext) -> Vec<PatternId> {
        let mut out = [PatternId::new(0); 16];
        let n = self.write_next_steps(context, &mut out);
        out[..n].to_vec()
    }

    /// Get next steps without allocation.
    /// Writes the next step IDs into the provided slice and returns the number of items written.
    fn write_next_steps(&self, _context: &ExecutionContext, _out: &mut [PatternId]) -> usize {
        0
    }
}

/// Pattern that executes two branches sequentially (AND pattern)
pub struct SequencePattern {
    metadata: PatternMetadata,
}

impl SequencePattern {
    pub fn new() -> Self {
        Self {
            metadata: PatternMetadata {
                id: 1,
                name: "Sequence".to_string(),
                category: "Basic".to_string(),
                description: "Execute tasks in sequence".to_string(),
                yawl_pattern_id: "WCP-1".to_string(),
                is_control_flow: true,
            },
        }
    }
}

impl Default for SequencePattern {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Pattern for SequencePattern {
    fn metadata(&self) -> &PatternMetadata {
        &self.metadata
    }

    async fn execute(&self, _context: &mut ExecutionContext) -> Result<()> {
        // Sequence: just mark as executed, don't modify data
        Ok(())
    }

    fn validate(&self, context: &ExecutionContext) -> Result<()> {
        if context.data.is_empty() {
            return Err(Error::InvalidInput(
                "Sequence requires input data".to_string(),
            ));
        }
        Ok(())
    }
}

/// Pattern that executes one of multiple branches (XOR pattern)
pub struct ExclusiveChoicePattern {
    metadata: PatternMetadata,
}

impl ExclusiveChoicePattern {
    pub fn new() -> Self {
        Self {
            metadata: PatternMetadata {
                id: 2,
                name: "Exclusive Choice".to_string(),
                category: "Basic".to_string(),
                description: "Choose one of multiple branches".to_string(),
                yawl_pattern_id: "WCP-3".to_string(),
                is_control_flow: true,
            },
        }
    }
}

impl Default for ExclusiveChoicePattern {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Pattern for ExclusiveChoicePattern {
    fn metadata(&self) -> &PatternMetadata {
        &self.metadata
    }

    async fn execute(&self, context: &mut ExecutionContext) -> Result<()> {
        // XOR: evaluate condition, update context with chosen branch
        if let Some(condition) = context.data.get("condition") {
            if condition.as_bool().unwrap_or(false) {
                context
                    .data
                    .insert("branch".to_string(), json!("true_branch"));
            } else {
                context
                    .data
                    .insert("branch".to_string(), json!("false_branch"));
            }
        }
        Ok(())
    }

    fn validate(&self, context: &ExecutionContext) -> Result<()> {
        if !context.data.contains_key("condition") {
            return Err(Error::InvalidInput(
                "XOR requires 'condition' field".to_string(),
            ));
        }
        Ok(())
    }
}

/// Pattern that executes multiple branches in parallel (AND pattern)
pub struct ParallelSplitPattern {
    metadata: PatternMetadata,
}

impl ParallelSplitPattern {
    pub fn new() -> Self {
        Self {
            metadata: PatternMetadata {
                id: 3,
                name: "Parallel Split".to_string(),
                category: "Basic".to_string(),
                description: "Split into multiple parallel branches".to_string(),
                yawl_pattern_id: "WCP-2".to_string(),
                is_control_flow: true,
            },
        }
    }
}

impl Default for ParallelSplitPattern {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Pattern for ParallelSplitPattern {
    fn metadata(&self) -> &PatternMetadata {
        &self.metadata
    }

    async fn execute(&self, context: &mut ExecutionContext) -> Result<()> {
        context
            .data
            .insert("parallel_branches".to_string(), json!([]));
        Ok(())
    }

    fn validate(&self, _context: &ExecutionContext) -> Result<()> {
        Ok(())
    }
}

/// Core registry service for patterns
pub struct CorePatternRegistry {
    registry: Arc<DashMap<u32, Arc<dyn Pattern>>>,
}

impl CorePatternRegistry {
    pub fn new() -> Self {
        Self {
            registry: Arc::new(DashMap::new()),
        }
    }

    /// Register a pattern for discovery and execution
    pub fn register(&self, pattern: Arc<dyn Pattern>) {
        let metadata = pattern.metadata();
        self.registry.insert(metadata.id, pattern);
    }

    /// Get a pattern by ID
    pub fn get(&self, id: u32) -> Option<Arc<dyn Pattern>> {
        self.registry.get(&id).map(|entry| Arc::clone(&entry))
    }

    /// Execute a step using a registered pattern
    pub async fn execute_step(
        &self, step: &WorkflowStep, context: &mut ExecutionContext,
    ) -> Result<()> {
        let pattern = self.get(step.pattern_id.0).ok_or_else(|| {
            Error::PatternNotFound(format!("Pattern {} not found", step.pattern_id.0))
        })?;

        pattern.validate(context)?;
        pattern.execute(context).await?;

        Ok(())
    }

    /// List all registered patterns
    pub fn list_patterns(&self) -> Vec<Arc<dyn Pattern>> {
        self.registry
            .iter()
            .map(|entry| Arc::clone(entry.value()))
            .collect()
    }

    pub fn pattern_count(&self) -> usize {
        self.registry.len()
    }
}

impl Default for CorePatternRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sequence_pattern() {
        let pattern = SequencePattern::new();
        let mut context = ExecutionContext::new("test".to_string());
        context.data.insert("input".to_string(), json!("test"));

        pattern.validate(&context).unwrap();
        pattern.execute(&mut context).await.unwrap();
    }

    #[tokio::test]
    async fn test_exclusive_choice_pattern() {
        let pattern = ExclusiveChoicePattern::new();
        let mut context = ExecutionContext::new("test".to_string());
        context.data.insert("condition".to_string(), json!(true));

        pattern.validate(&context).unwrap();
        pattern.execute(&mut context).await.unwrap();

        assert_eq!(
            context.data.get("branch").unwrap().as_str(),
            Some("true_branch")
        );
    }

    #[tokio::test]
    async fn test_core_registry() {
        let registry = CorePatternRegistry::new();
        registry.register(Arc::new(SequencePattern::new()));
        registry.register(Arc::new(ExclusiveChoicePattern::new()));
        registry.register(Arc::new(ParallelSplitPattern::new()));

        assert_eq!(registry.pattern_count(), 3);
        assert!(registry.get(1).is_some());
        assert!(registry.get(999).is_none());
    }

    #[tokio::test]
    async fn test_execute_step() {
        let registry = CorePatternRegistry::new();
        registry.register(Arc::new(SequencePattern::new()));

        let step = WorkflowStep {
            id: genesis_types::StepId::generate(),
            pattern_id: PatternId::new(1),
            inputs: Default::default(),
            outputs: Default::default(),
        };

        let mut context = ExecutionContext::new("test".to_string());
        context.data.insert("input".to_string(), json!("test"));

        registry.execute_step(&step, &mut context).await.unwrap();
    }
}
