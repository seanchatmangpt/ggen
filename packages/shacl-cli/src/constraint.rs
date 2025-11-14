//! SHACL constraint definitions and management

use crate::{Error, Result};

/// Represents a SHACL constraint
#[derive(Debug, Clone)]
pub struct Constraint {
    pub constraint_type: ConstraintType,
    pub severity: Severity,
}

#[derive(Debug, Clone)]
pub enum ConstraintType {
    MinCount(u32),
    MaxCount(u32),
    Datatype(String),
    Pattern(String),
    Class(String),
}

#[derive(Debug, Clone)]
pub enum Severity {
    Violation,
    Warning,
    Info,
}
