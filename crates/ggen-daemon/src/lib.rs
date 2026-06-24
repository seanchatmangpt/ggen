#![allow(
    clippy::expect_used,
    clippy::unwrap_used,
    clippy::panic,
)]

pub mod amplifier;
pub mod campaign;
pub mod cascade;
pub mod catalog;
pub mod dispatch;
pub mod error;
pub mod expansion;
pub mod health;
pub mod manifest_cache;
pub mod mcp_server;
pub mod metrics;
pub mod ocel_log;
pub mod ontology;
pub mod parallel_dispatch;
pub mod repo_manager;
pub mod retry;
pub mod scheduler;
pub mod state;

pub use amplifier::{CommitMultiplier, MultiplierSummary};
pub use campaign::{CampaignResult, CampaignRunner};
pub use cascade::{CascadeRunner, CascadeWave};
pub use catalog::{load_catalog, RepoCatalogEntry};
pub use expansion::{ExpansionPlan, ExpansionSummary};
pub use manifest_cache::ManifestCache;
pub use error::{DaemonError, Result};
pub use health::{check_repo, RepoHealth, RepoHealthStatus};
pub use mcp_server::GgenDaemonMcp;
pub use metrics::{CampaignDashboard, MetricsStore};
pub use ocel_log::{OcelEvent, OcelLog};
pub use ontology::{load_jobs, JobDef};
pub use scheduler::DaemonScheduler;
pub use state::DaemonState;
