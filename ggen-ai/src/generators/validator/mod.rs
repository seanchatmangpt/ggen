//! Template validation and quality assessment system
//!
//! Provides comprehensive validation and quality assessment for generated templates through:
//! - Validation rules and constraints
//! - SPARQL constraints checking

pub mod constraints;

// Placeholder types for backward compatibility until engine and rules are implemented
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateValidator;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub valid: bool,
    pub issues: Vec<ValidationIssue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRules;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationIssue {
    pub severity: Severity,
    pub issue_type: IssueType,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IssueType {
    Syntax,
    Semantic,
    Performance,
    Security,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityMetrics {
    pub score: f64,
}
