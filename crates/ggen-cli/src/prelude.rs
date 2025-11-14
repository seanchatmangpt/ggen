//! Common imports and re-exports for CLI commands

pub use crate::runtime_helper;
// Re-export anyhow for backwards compatibility (used in main.rs and external code)
pub use anyhow::{Context, Result as AnyhowResult};
// Re-export ggen_utils error types for library code
pub use clap::Args;
pub use clap_noun_verb::Result;
pub use ggen_utils::error::{Context as ErrorContext, Error, Result};
pub use std::path::PathBuf;
