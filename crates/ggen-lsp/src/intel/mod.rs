//! Process intelligence: agent-edit OCEL capture + repair receipts.
//!
//! The hot path emits events (cheap, best-effort) into an append-only NDJSON log;
//! the offline miner (`ggen lsp mine`) reads it back, projects to RDF, and runs
//! SPARQL discovery. Event types reuse ggen-graph's OCEL model — no new log.

pub mod events;
pub mod field;
pub mod history;
pub mod log;
pub mod metrics;
pub mod mine;
pub mod receipt;
pub mod replay;

pub use events::Attribution;
pub use field::{field_status, FieldReadiness, FieldStatus};
pub use history::{default_history_path, PromotionHistory, RoutePromotionRecord, RouteStatus};
pub use log::{default_path, IntelLog};
pub use metrics::{compute_metrics, ImproveMetrics, MetricValue};
pub use mine::{mine, MineReport};
pub use receipt::{hash_content, RepairReceipt};
pub use replay::{replay_case, verify_promotion, CaseReplay, PromotionReplay};
