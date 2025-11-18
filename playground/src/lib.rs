//! Hyper-Thesis Framework (HTF) Library
//!
//! A unified μ-architecture blending IMRaD, Papers, Argument, Contribution,
//! Monograph, DSR, and Narrative modes for thesis research planning.
//!
//! Core components:
//! - **Λ-scheduler**: Maps research into canonical ordering for chapter planning
//! - **Π-profiler**: Shows how your shards fit into HTF categories
//! - **Γ-checker**: Validates consistency against Q-invariants

pub mod checker;
pub mod error;
pub mod models;
pub mod ontology;
pub mod profiler;
pub mod scheduler;

pub use error::{Error, Result};
pub use models::*;

/// Initialize HTF with RDF store
pub async fn init_htf(data_dir: &str) -> Result<()> {
    tracing::info!("Initializing HTF with data directory: {}", data_dir);
    Ok(())
}
