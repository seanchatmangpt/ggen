//! Common imports and re-exports for CLI commands
//!
//! This module provides a canonical prelude for all CLI commands,
//! ensuring consistent type resolution and avoiding ambiguous Result imports.

// Core CLI types and error handling
// NOTE: This prelude defines a single canonical Result<T> = Result<T, CliError>
// All CLI code should use this Result type exclusively.
pub use crate::error::{GgenError, GgenResultExt};

// Import the canonical Result type alias (from error module)
pub use crate::error::Result;

// Runtime helper
pub use crate::runtime_helper;

// clap-noun-verb types (use carefully - convert to GgenError)
pub use clap_noun_verb::Result as ClapNounVerbResult;
pub use clap_noun_verb_macros::verb;

// Standard library types
pub use std::fs;
pub use std::io::{self, Read, Write};
pub use std::path::PathBuf;
