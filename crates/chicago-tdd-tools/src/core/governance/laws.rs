//! Law enforcement macros and backing logic for agent governance.

#![allow(missing_docs)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(clippy::must_use_candidate, clippy::missing_errors_doc)]

#[macro_export]
macro_rules! assert_admitted {
    ($metadata:expr) => {{
        let metadata = &$metadata;
        if metadata.credentials.is_empty() {
            let diag = $crate::core::governance::Diagnostic {
                code: $crate::core::governance::DiagnosticCode::new(
                    $crate::core::governance::channel::get_domain(),
                    $crate::core::governance::DiagnosticCategory::Admission,
                    1,
                ),
                category: $crate::core::governance::DiagnosticCategory::Admission,
                run_id: $crate::core::governance::channel::get_run_id(),
                agent_id: None,
                location: Some($crate::source_location!()),
                message: "Artifact failed admission: empty credentials".to_string(),
                severity: $crate::core::governance::Severity::Andon,
                source_module: "governance",
                context: ::std::collections::HashMap::new(),
                elapsed_ns: 0,
            };
            $crate::core::governance::emit_diagnostic(&diag);
        }
    }};
}

#[macro_export]
macro_rules! assert_crown_receipt {
    ($metadata:expr) => {{
        let metadata = &$metadata;
        if metadata.crown_receipt.is_none() {
            let diag = $crate::core::governance::Diagnostic {
                code: $crate::core::governance::DiagnosticCode::new(
                    $crate::core::governance::channel::get_domain(),
                    $crate::core::governance::DiagnosticCategory::Lineage,
                    1,
                ),
                category: $crate::core::governance::DiagnosticCategory::Lineage,
                run_id: $crate::core::governance::channel::get_run_id(),
                agent_id: None,
                location: Some($crate::source_location!()),
                message: "Missing crown receipt".to_string(),
                severity: $crate::core::governance::Severity::Andon,
                source_module: "governance",
                context: ::std::collections::HashMap::new(),
                elapsed_ns: 0,
            };
            $crate::core::governance::emit_diagnostic(&diag);
        }
    }};
}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdmissionMetadata {
    pub id: String,
    pub credentials: String,
    pub crown_receipt: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ContributionKind {
    SchemaConstraint = 1,
    Falsifier = 2,
    CognitiveStrategy = 3,
    ProcessPattern = 4,
    Receipt = 5,
    BoundaryRule = 6,
    ResidualClass = 7,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SubstrateDelta {
    pub contributions: Vec<(ContributionKind, String)>,
}

impl SubstrateDelta {
    pub fn is_evolutionary(&self) -> bool {
        !self.contributions.is_empty()
    }
}
