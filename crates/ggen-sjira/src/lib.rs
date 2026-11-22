//! ggen-sjira — native Rust port of the ggen_igniter `GgenIgniter.SemanticJira`
//! admission kernel, core slice.
//!
//! Ported (this slice):
//! - `WorkOrder` representation aligned to Elixir `admit_work_order/1`:
//!   `@required` fields plus B3 `path_scope`, definition defaults
//!   (`authority_requirement`, `dependencies`, `required_receipt_classes`,
//!   …), and the standing vocabulary (`@standings` + `REFUSED(<reason>)`).
//! - `admit_work_order/1`: required-field, regex, closed-standing, B3
//!   path_scope, and list validation with typed refusals (lines 100-151).
//! - `definition_digest`: sha256 over the CLOSED 19-field
//!   `@definition_fields` take (lines 38-41) — standing, candidate_sha,
//!   origin_observation, dims, and every derived digest excluded.
//!
//! Ported modules (see each module's own doc for the exact Elixir lines):
//! `work_order` + `admit` (representation/digests/admission), `receipts`
//! (blake3 transition journal), `authority` (pinned index + origin
//! resolution), `frontier` (frontier/schedule/project_standing), `lease` +
//! `epoch` (fencing), `execute` (E80-8), `ledger` (append/fetch/vc law),
//! `controller` (ordered gate pipeline), `edits_bridge`.
//!
//! Deferred to later slices (present in the igniter spec, not ported here):
//! execution_package, do_intent, replay_check, semantic_diff,
//! repair/composition, machine_experience, view, render_projection.
//! (promote landed in `promote.rs`; descriptor landed in `descriptor.rs`.)
//! Slice 5 (PG-2, ds-3): `shacl` + `target_pack` + `pack_health` (Phase 1) —
//! the pack integration surface; template rendering is a disclosed
//! UNSUPPORTED follow-on (`render.rs`).
//!
//! `#![forbid(unsafe_code)]`. IO-free. Authority is stamped `NONE` at
//! admission; this kernel never creates authority.

#![forbid(unsafe_code)]

pub mod admit;
pub mod authority;
pub mod complete;
pub mod controller;
pub mod descriptor;
pub mod edits_bridge;
pub mod epoch;
pub mod execute;
pub mod frontier;
pub mod generated;
pub mod igniter_cleanroom;
pub mod lease;
pub mod ledger;
pub mod pack_health;
pub mod promote;
pub mod receipts;
pub mod reconcile;
pub mod shacl;
pub mod target_pack;
pub mod work_order;

pub use admit::{admit_work_order, AdmittedWorkOrder, Refusal};
pub use authority::{
    canonical_authority_graph, canonical_index, load_pack_authority_index,
    OBJECTIVE_CODE_WORK_AUTHORITY, OBJECTIVE_PROJECT_MANUFACTURER, OBJECTIVE_SEMANTIC_JIRA_MVP,
};
pub use descriptor::{
    build_xaas_contract, identity, receipt_digest, receipt_from_xaas, DescriptorOpts,
    DescriptorRefusal, ReceiptRefusal, SCHEMA,
};
pub use frontier::{
    frontier, orders_from_json, project_standing, schedule, ActiveLease, AuthorityIndex,
    BlockedEntry, DependencyEdge, EligibleCandidate, Evidence, FrontierInput, FrontierResult,
    Standings, TransitionEvent,
};
pub use promote::{promote, PromoteRefusal, PromotionIntent};
pub use receipts::{
    AdmissionTransition, ChainError, Decision, TransitionJournal, GENESIS_PREV_HASH,
};
pub use reconcile::{
    reconcile, tail_digest, ReconcileOpts, ReconcileRefusal, Reconciled, GENESIS_DIGEST,
};
pub use work_order::{
    canonical, definition_digest, digest_of, Standing, WorkOrder, WorkOrderDependency,
};
