//! FMEA (Failure Mode and Effects Analysis) for RDF Ontology Systems
//!
//! This module provides comprehensive risk assessment for:
//! - RDF schema changes and breaking changes
//! - SPARQL query path analysis with SOD scoring
//! - Template transformation risks with RPN calculation
//! - Proactive failure prevention in semantic code generation
//!
//! # Architecture
//!
//! ```text
//! FMEA System
//! ├── Core Types (SOD scoring, RPN calculation)
//! ├── Schema Analyzer (RDF change detection)
//! ├── Query Analyzer (SPARQL path risk)
//! ├── Template Analyzer (transformation risk)
//! └── Prevention (mitigation strategies)
//! ```

pub mod types;
pub mod schema_analyzer;
pub mod query_analyzer;
pub mod template_analyzer;
pub mod prevention;
pub mod scoring;

pub use types::{
    FailureMode, FailureModeCategory, RiskPriorityNumber, Severity, Occurrence, Detection,
    FmeaAnalysis, FmeaReport, MitigationStrategy, MitigationStatus,
};

pub use schema_analyzer::{SchemaChangeAnalyzer, SchemaChange, SchemaChangeType, SchemaImpact};
pub use query_analyzer::{QueryPathAnalyzer, QueryPath, QueryRisk, QueryDependency};
pub use template_analyzer::{TemplateRiskAnalyzer, TemplateRisk, TemplateChangeType};
pub use prevention::{FailurePreventionEngine, PreventionRule, PreventionAction};
pub use scoring::{SodScorer, RpnCalculator};

/// FMEA system version
pub const FMEA_VERSION: &str = "1.0.0";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmea_version() {
        assert_eq!(FMEA_VERSION, "1.0.0");
    }
}
