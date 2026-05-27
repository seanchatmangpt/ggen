pub mod coverage;
pub mod gall_projection;
pub mod ocel_types;
pub mod projection;
pub mod prov_types;
pub mod self_audit;

pub use coverage::{generate_coverage_matrix, CoverageMatrix, RequirementEvidence};
pub use gall_projection::{extract_self_audit, project_self_audit, query_relationship};
pub use ocel_types::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
pub use projection::EvidenceProjector;
pub use prov_types::{
    ProvActivity, ProvAgent, ProvDerivation, ProvDocument, ProvEntity, ProvGeneration, ProvUsage,
};
pub use self_audit::generate_self_audit_log;
