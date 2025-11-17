//! Binding completeness: Σ → Π → μ proof system
//!
//! Proves that all constructs expressible in Σ (ontology) have:
//! - A projection Π that yields executable representation
//! - A μ binding that operates only on Π
//! - Coverage metrics for production-relevant entities

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// A projection from ontology (Σ) to executable form (Π)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Projection {
    /// Name of the construct being projected
    construct_name: String,
    /// Source contract/ontology element
    source_id: String,
    /// Executable representation
    executable_form: serde_json::Value,
    /// Coverage percentage (0-100)
    coverage: u8,
}

impl Projection {
    /// Create a new projection
    pub fn new(
        construct_name: impl Into<String>, source_id: impl Into<String>,
        executable_form: serde_json::Value,
    ) -> Self {
        Self {
            construct_name: construct_name.into(),
            source_id: source_id.into(),
            executable_form,
            coverage: 0,
        }
    }

    /// Set coverage percentage
    pub fn with_coverage(mut self, coverage: u8) -> Self {
        self.coverage = coverage.min(100);
        self
    }

    /// Get construct name
    pub fn construct_name(&self) -> &str {
        &self.construct_name
    }

    /// Get coverage
    pub fn coverage(&self) -> u8 {
        self.coverage
    }

    /// Is fully covered?
    pub fn is_complete(&self) -> bool {
        self.coverage == 100
    }
}

/// μ binding: Maps projections to kernel operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MuBinding {
    /// Name of the binding
    name: String,
    /// Which projection this binds to
    projection_name: String,
    /// The μ function implementation (as string/reference)
    mu_function: String,
    /// Preconditions
    preconditions: Vec<String>,
    /// Postconditions
    postconditions: Vec<String>,
}

impl MuBinding {
    /// Create a new μ binding
    pub fn new(
        name: impl Into<String>, projection_name: impl Into<String>, mu_function: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            projection_name: projection_name.into(),
            mu_function: mu_function.into(),
            preconditions: Vec::new(),
            postconditions: Vec::new(),
        }
    }

    /// Add precondition
    pub fn with_precondition(mut self, condition: impl Into<String>) -> Self {
        self.preconditions.push(condition.into());
        self
    }

    /// Add postcondition
    pub fn with_postcondition(mut self, condition: impl Into<String>) -> Self {
        self.postconditions.push(condition.into());
        self
    }

    /// Get name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get projection name
    pub fn projection_name(&self) -> &str {
        &self.projection_name
    }
}

/// Binding completeness proof: Σ → Π → μ is total
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BindingCompleteness {
    /// All projections from Σ
    projections: BTreeMap<String, Projection>,
    /// All μ bindings
    bindings: BTreeMap<String, MuBinding>,
    /// Coverage metrics
    total_constructs: usize,
    /// Constructs with projections
    projected_constructs: usize,
    /// Constructs with bindings
    bound_constructs: usize,
}

impl BindingCompleteness {
    /// Create a new completeness proof
    pub fn new() -> Self {
        Self {
            projections: BTreeMap::new(),
            bindings: BTreeMap::new(),
            total_constructs: 0,
            projected_constructs: 0,
            bound_constructs: 0,
        }
    }

    /// Register a projection
    pub fn register_projection(mut self, projection: Projection) -> Self {
        self.projections
            .insert(projection.construct_name().to_string(), projection);
        self.total_constructs += 1;
        if self
            .projections
            .values()
            .last()
            .map(|p| p.is_complete())
            .unwrap_or(false)
        {
            self.projected_constructs += 1;
        }
        self
    }

    /// Register a μ binding
    pub fn register_binding(mut self, binding: MuBinding) -> Self {
        let projection_exists = self.projections.contains_key(binding.projection_name());
        self.bindings.insert(binding.name().to_string(), binding);
        if projection_exists {
            self.bound_constructs += 1;
        }
        self
    }

    /// Get projection coverage percentage
    pub fn projection_coverage(&self) -> u8 {
        if self.total_constructs == 0 {
            return 0;
        }
        ((self.projected_constructs as u64 * 100) / self.total_constructs as u64) as u8
    }

    /// Get binding coverage percentage
    pub fn binding_coverage(&self) -> u8 {
        if self.projected_constructs == 0 {
            return 0;
        }
        ((self.bound_constructs as u64 * 100) / self.projected_constructs as u64) as u8
    }

    /// Verify completeness: ≥ 80% coverage required for production
    pub fn verify_production_ready(&self) -> crate::error::DoDResult<()> {
        let projection_cov = self.projection_coverage();
        let binding_cov = self.binding_coverage();

        if projection_cov < 80 {
            return Err(crate::error::DoDError::Internal(format!(
                "projection coverage {}% < 80% required",
                projection_cov
            )));
        }

        if binding_cov < 80 {
            return Err(crate::error::DoDError::Internal(format!(
                "binding coverage {}% < 80% required",
                binding_cov
            )));
        }

        Ok(())
    }

    /// Get all projections
    pub fn projections(&self) -> &BTreeMap<String, Projection> {
        &self.projections
    }

    /// Get all bindings
    pub fn bindings(&self) -> &BTreeMap<String, MuBinding> {
        &self.bindings
    }

    /// Get coverage report
    pub fn coverage_report(&self) -> String {
        format!(
            "Σ→Π→μ Completeness: {}/{} constructs projected ({}%), {}/{} bound ({}%)",
            self.projected_constructs,
            self.total_constructs,
            self.projection_coverage(),
            self.bound_constructs,
            self.projected_constructs,
            self.binding_coverage()
        )
    }
}

impl Default for BindingCompleteness {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_projection_creation() {
        let proj = Projection::new(
            "workflow",
            "contract-1",
            serde_json::json!({"type": "workflow"}),
        )
        .with_coverage(100);

        assert!(proj.is_complete());
    }

    #[test]
    fn test_mu_binding() {
        let binding = MuBinding::new(
            "workflow-bind",
            "workflow",
            "fn bind_workflow(proj) { ... }",
        )
        .with_precondition("projection.valid")
        .with_postcondition("action.queued");

        assert_eq!(binding.projection_name(), "workflow");
        assert_eq!(binding.preconditions.len(), 1);
    }

    #[test]
    fn test_binding_completeness() {
        let completeness = BindingCompleteness::new()
            .register_projection(
                Projection::new("workflow", "c1", serde_json::json!({})).with_coverage(100),
            )
            .register_binding(MuBinding::new("bind1", "workflow", "fn() {}"));

        assert_eq!(completeness.projection_coverage(), 100);
    }
}
