//! Shell utilities domain layer
//!
//! Pure business logic for shell operations including completion generation and installation.
//! This module is independent of CLI presentation concerns.

pub mod completion;

pub use completion::{CompletionGenerator, CompletionInstaller, ShellLister};
