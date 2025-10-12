//! Universal lifecycle system for ggen
//!
//! This module implements the lifecycle orchestration system that enables
//! cross-language project management through make.toml

pub mod cache;
pub mod dag;
pub mod dx;
pub mod error;
pub mod exec;
pub mod loader;
pub mod model;
pub mod state;

#[cfg(test)]
mod integration_test;

#[cfg(test)]
mod behavior_tests;

pub use model::{Hooks, Make, Phase, Project, Workspace};
pub use error::{LifecycleError, Result};
pub use loader::load_make;
pub use state::{LifecycleState, load_state, save_state};
pub use exec::{Context, run_phase, run_pipeline};
pub use cache::cache_key;
pub use dag::topo;
pub use dx::{ExecutionMode, ExecutionMetrics, Output, StateVisualizer};
