//! Common imports and re-exports for CLI commands

// Core CLI types and error handling
pub use crate::error::{GgenError, Result, GgenResultExt};
pub use crate::runtime_helper;

// clap-noun-verb types (use carefully - convert to GgenError)
pub use clap::Args;
pub use clap_noun_verb::Result as ClapNounVerbResult;
pub use clap_noun_verb_macros::verb;

// Standard library types
pub use std::path::PathBuf;
pub use std::fs;
pub use std::io::{self, Read, Write};
