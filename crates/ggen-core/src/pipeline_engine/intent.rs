//! Manufacturing Intent - Semantic objectives and constraints for projection.
//!
//! Intent allows tracking the *why* of a manufacturing run and enforcing
//! constraints that go beyond simple syntax validation.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A success criterion for a manufacturing run.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct SuccessCriterion {
    /// Criterion name (e.g., "schema-valid", "zero-violations")
    pub name: String,
    /// What this measures
    pub description: String,
    /// Threshold for passing (if applicable)
    pub threshold: Option<f64>,
}

/// A constraint on the manufacturing process.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Constraint {
    /// Constraint name
    pub name: String,
    /// What this constrains
    pub description: String,
    /// Constraint limit
    pub limit: String,
}

/// Semantic intent for a manufacturing run.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct ManufacturingIntent {
    /// Primary objective of the manufacturing run
    pub objective: String,
    /// Measurable success criteria
    pub success_criteria: Vec<SuccessCriterion>,
    /// Constraints on execution
    pub constraints: Vec<Constraint>,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl ManufacturingIntent {
    /// Create a new intent with an objective
    pub fn new(objective: impl Into<String>) -> Self {
        Self {
            objective: objective.into(),
            ..Default::default()
        }
    }

    /// Add a success criterion
    pub fn with_criterion(mut self, name: &str, description: &str) -> Self {
        self.success_criteria.push(SuccessCriterion {
            name: name.to_string(),
            description: description.to_string(),
            threshold: None,
        });
        self
    }

    /// Add a constraint
    pub fn with_constraint(mut self, name: &str, limit: &str) -> Self {
        self.constraints.push(Constraint {
            name: name.to_string(),
            description: "".to_string(),
            limit: limit.to_string(),
        });
        self
    }
}
