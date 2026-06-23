pub mod conformance;
pub mod coverage;
pub mod dfg;
pub mod gall_projection;
pub mod ocel_types;
pub mod pack_events;
pub mod projection;
pub mod prov_types;
pub mod self_audit;

pub use conformance::{check_guard, check_lifecycle_order};
pub use coverage::{generate_coverage_matrix, CoverageMatrix, RequirementEvidence};
pub use dfg::{discover_dfg, DfgEdge};
pub use gall_projection::{extract_self_audit, project_self_audit, query_relationship};
pub use ocel_types::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
pub use pack_events::{
    emit_lockfile_write, emit_pack_install, emit_pack_publish, emit_pack_remove, emit_pack_verify,
    lockfile_entry_object, lockfile_entry_object_id, pack_object, pack_object_id, receipt_object,
    receipt_object_id, ACT_LOCKFILE_WRITE, ACT_PACK_INSTALL, ACT_PACK_PUBLISH, ACT_PACK_REMOVE,
    ACT_PACK_VERIFY, OBJ_TYPE_LOCKFILE_ENTRY, OBJ_TYPE_PACK, OBJ_TYPE_RECEIPT,
};
pub use projection::EvidenceProjector;
pub use prov_types::{
    ProvActivity, ProvAgent, ProvDerivation, ProvDocument, ProvEntity, ProvGeneration, ProvUsage,
};
pub use self_audit::generate_self_audit_log;
