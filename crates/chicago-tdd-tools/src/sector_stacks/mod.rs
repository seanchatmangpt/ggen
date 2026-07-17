//! Sector-Grade Reference Stacks
//!
//! Production-grade implementations of the Chatman Equation in two sectors:
//! 1. Academic Publishing: Complete paper review workflow
//! 2. Enterprise Claims: Insurance claims processing with 100+ test cases
//!
//! Both demonstrate:
//! - Deterministic knowledge hooks
//! - Guard-based safety constraints
//! - Cryptographic receipt generation
//! - Multi-stage workflow validation
//!
//! ## RDF Integration
//!
//! The `rdf` module provides semantic web integration for loading and validating
//! sector ontologies defined in TTL format. Use with `--features rdf` to enable
//! RDF-driven operation validation.

pub mod academic;
pub mod claims;
pub mod rdf;

use serde::{Deserialize, Serialize};
use std::fmt;

/// Generic receipt for all sector operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationReceipt {
    /// Unique operation ID
    pub id: String,
    /// Sector (Academic, Claims, etc.)
    pub sector: String,
    /// Operation type (Submission, Decision, Settlement, etc.)
    pub operation: String,
    /// Status of operation
    pub status: OperationStatus,
    /// Detailed result
    pub result: String,
    /// Cryptographic merkle root
    pub merkle_root: String,
    /// Timestamp
    pub timestamp: String,
}

/// Status of a sector operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OperationStatus {
    /// Operation completed successfully
    Success,
    /// Operation completed with partial success
    PartialSuccess,
    /// Operation failed
    Failed,
    /// Operation is pending review
    PendingReview,
}

impl fmt::Display for OperationStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Success => write!(f, "Success"),
            Self::PartialSuccess => write!(f, "PartialSuccess"),
            Self::Failed => write!(f, "Failed"),
            Self::PendingReview => write!(f, "PendingReview"),
        }
    }
}

/// Trait for sector-specific operations
pub trait SectorOperation: Send + Sync {
    /// Get sector name
    fn sector_name(&self) -> &'static str;

    /// Get operation description
    fn description(&self) -> &'static str;

    /// Check if operation is deterministic
    fn is_deterministic(&self) -> bool;

    /// Generate operation receipt
    fn generate_receipt(&self, status: OperationStatus) -> OperationReceipt;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operation_status_display() {
        assert_eq!(OperationStatus::Success.to_string(), "Success");
        assert_eq!(OperationStatus::Failed.to_string(), "Failed");
    }

    #[test]
    fn test_receipt_creation() {
        let receipt = OperationReceipt {
            id: "test-123".to_string(),
            sector: "Academic".to_string(),
            operation: "Decision".to_string(),
            status: OperationStatus::Success,
            result: "Paper accepted".to_string(),
            merkle_root: "abc123".to_string(),
            timestamp: "2025-11-16T00:00:00Z".to_string(),
        };

        assert_eq!(receipt.sector, "Academic");
        assert_eq!(receipt.status, OperationStatus::Success);
    }
}
