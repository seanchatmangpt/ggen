//! Runtime surface abstractions for deterministic execution
//!
//! Provides implementations for controlling deterministic surfaces:
//! time, RNG, filesystem, network, and process isolation.

pub mod fs;
pub mod net;
pub mod proc;
pub mod rng;
pub mod runner;
pub mod time;

// Re-export runtime surface types
pub use fs::FsController;
pub use net::NetController;
pub use proc::ProcController;
pub use rng::RngController;
pub use runner::{Config as RunnerConfig, Runner};
pub use time::TimeController;
