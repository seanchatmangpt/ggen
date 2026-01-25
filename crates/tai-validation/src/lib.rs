#![warn(missing_docs)]

//! # tai-validation: SLO/SLA Validation & Compliance Framework
//!
//! Comprehensive validation system for ensuring production readiness:
//! - **SLO/SLA Tracking**: Automated performance and reliability validation
//! - **Compliance Framework**: Multi-framework support (FISMA, FedRAMP, SOC 2, HIPAA, 21 CFR Part 11, NIST 800-53, DFARS)
//! - **Security Scanning**: SAST, DAST, dependency scanning with SBOM generation
//! - **SHACL Validation**: RDF ontology conformance checking
//! - **Pre-flight Validation**: Resource, permission, and configuration checks
//! - **Evidence Collection**: Cryptographic receipt generation with audit trails
//! - **Test Coverage Analysis**: Gap analysis and traceability matrix generation
//! - **Acceptance Criteria Matching**: Map RDF requirements to test coverage
//! - **Test Execution Pipeline**: Parallel batching with timeout enforcement

pub mod acceptance;
pub mod compliance;
pub mod coverage;
pub mod error;
pub mod evidence;
pub mod execution;
pub mod preflight;
pub mod security;
pub mod shacl;
pub mod slo;

pub use acceptance::{AcceptanceCriteria, CriteriaValidator};
pub use compliance::{ComplianceFramework, ComplianceResult, FrameworkType};
pub use compliance::frameworks::ComplianceViolation;
pub use coverage::{CoverageGapAnalysis, CoverageMetrics};
pub use error::{Result, ValidationError};
pub use evidence::{EvidenceCollector, ValidationReceipt};
pub use execution::{ExecutionPipeline, TestBatch, TestResult};
pub use preflight::{PreFlightChecklist, PreFlightResult};
pub use security::{SecurityScanner, SecurityScanResult};
pub use shacl::{ShaclValidator, ShapeViolation};
pub use slo::{SloMetrics, SloValidator};

/// Common error types for validation operations
pub use error::ValidationError as Error;

/// Result type for validation operations
pub type ValidationOutput<T> = std::result::Result<T, ValidationError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        // Verify all public types are exported
        let _ = std::any::type_name::<SloValidator>();
        let _ = std::any::type_name::<ComplianceFramework>();
        let _ = std::any::type_name::<SecurityScanner>();
        let _ = std::any::type_name::<ShaclValidator>();
        let _ = std::any::type_name::<PreFlightChecklist>();
        let _ = std::any::type_name::<EvidenceCollector>();
        let _ = std::any::type_name::<CoverageMetrics>();
        let _ = std::any::type_name::<CriteriaValidator>();
        let _ = std::any::type_name::<ExecutionPipeline>();
    }
}
