//! Decision system: Tracking and queryable decision history

use crate::error::DoDResult;
use crate::kernel::KernelDecision;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

/// Unique identifier for decisions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DecisionId(Uuid);

impl DecisionId {
    /// Generate a new decision ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for DecisionId {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for DecisionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Decision outcome
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DecisionOutcome {
    /// Decision executed successfully
    Success,
    /// Decision rolled back
    RolledBack,
    /// Decision rejected
    Rejected,
    /// Decision pending
    Pending,
}

/// A tracked decision with all context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Decision {
    /// Decision ID
    id: DecisionId,
    /// Associated kernel decision
    kernel_decision: KernelDecision,
    /// Outcome
    outcome: DecisionOutcome,
    /// When decision was made
    timestamp: DateTime<Utc>,
    /// Tenant ID
    tenant_id: String,
    /// Related decision IDs (dependencies)
    related_decisions: Vec<DecisionId>,
    /// Explanation
    explanation: String,
}

impl Decision {
    /// Create a new decision
    pub fn new(
        kernel_decision: KernelDecision, tenant_id: impl Into<String>,
        explanation: impl Into<String>,
    ) -> Self {
        Self {
            id: DecisionId::new(),
            kernel_decision,
            outcome: DecisionOutcome::Pending,
            timestamp: Utc::now(),
            tenant_id: tenant_id.into(),
            related_decisions: Vec::new(),
            explanation: explanation.into(),
        }
    }

    /// Get decision ID
    pub fn id(&self) -> DecisionId {
        self.id
    }

    /// Get kernel decision
    pub fn kernel_decision(&self) -> &KernelDecision {
        &self.kernel_decision
    }

    /// Get outcome
    pub fn outcome(&self) -> DecisionOutcome {
        self.outcome
    }

    /// Set outcome
    pub fn with_outcome(mut self, outcome: DecisionOutcome) -> Self {
        self.outcome = outcome;
        self
    }

    /// Get tenant ID
    pub fn tenant_id(&self) -> &str {
        &self.tenant_id
    }

    /// Get timestamp
    pub fn timestamp(&self) -> DateTime<Utc> {
        self.timestamp
    }

    /// Add related decision
    pub fn with_related(mut self, decision_id: DecisionId) -> Self {
        self.related_decisions.push(decision_id);
        self
    }

    /// Get explanation
    pub fn explanation(&self) -> &str {
        &self.explanation
    }

    /// Is this decision successful?
    pub fn is_success(&self) -> bool {
        self.outcome == DecisionOutcome::Success
    }
}

/// Store for decisions
pub struct DecisionStore {
    /// All decisions
    decisions: BTreeMap<DecisionId, Decision>,
    /// Decisions indexed by tenant
    by_tenant: BTreeMap<String, Vec<DecisionId>>,
    /// Decisions indexed by outcome
    by_outcome: BTreeMap<String, Vec<DecisionId>>,
}

impl DecisionStore {
    /// Create a new decision store
    pub fn new() -> Self {
        Self {
            decisions: BTreeMap::new(),
            by_tenant: BTreeMap::new(),
            by_outcome: BTreeMap::new(),
        }
    }

    /// Store a decision
    pub fn store(&mut self, decision: Decision) -> DoDResult<()> {
        let id = decision.id();
        let tenant_id = decision.tenant_id().to_string();
        let outcome = format!("{:?}", decision.outcome());

        self.decisions.insert(id, decision);

        self.by_tenant
            .entry(tenant_id)
            .or_insert_with(Vec::new)
            .push(id);

        self.by_outcome
            .entry(outcome)
            .or_insert_with(Vec::new)
            .push(id);

        Ok(())
    }

    /// Get decision by ID
    pub fn get(&self, id: DecisionId) -> Option<&Decision> {
        self.decisions.get(&id)
    }

    /// Get all decisions for tenant
    pub fn get_by_tenant(&self, tenant_id: &str) -> Vec<&Decision> {
        self.by_tenant
            .get(tenant_id)
            .map(|ids| ids.iter().filter_map(|id| self.decisions.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get successful decisions
    pub fn get_successful(&self) -> Vec<&Decision> {
        self.decisions.values().filter(|d| d.is_success()).collect()
    }

    /// Query decisions
    pub fn query<F>(&self, predicate: F) -> Vec<&Decision>
    where
        F: Fn(&Decision) -> bool,
    {
        self.decisions.values().filter(|d| predicate(d)).collect()
    }

    /// Get all decisions
    pub fn all(&self) -> Vec<&Decision> {
        self.decisions.values().collect()
    }

    /// Get count
    pub fn count(&self) -> usize {
        self.decisions.len()
    }
}

impl Default for DecisionStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decision_creation() {
        let kernel_decision = KernelDecision::new();
        let decision = Decision::new(kernel_decision, "tenant-1", "test");
        assert_eq!(decision.outcome(), DecisionOutcome::Pending);
    }

    #[test]
    fn test_decision_store() -> DoDResult<()> {
        let mut store = DecisionStore::new();
        let kernel_decision = KernelDecision::new();
        let decision = Decision::new(kernel_decision, "tenant-1", "test");

        store.store(decision)?;
        assert_eq!(store.count(), 1);
        Ok(())
    }
}
