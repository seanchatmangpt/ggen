//! CLI Proof Testing — real boundary-crossing harness primitives
//!
//! Feature: `cli-proof`
//!
//! Provides four types for Chicago TDD boundary-crossing CLI tests:
//! - [`CliHarness`] — runs a real binary, returns [`CliOutput`]
//! - [`TempWorkspace`] — hermetic TempDir with write/assert helpers
//! - [`ReceiptAssertions`] — verifies signed receipt JSON on disk
//! - [`SabotageFixture`] — corrupts files to test fail-closed behavior

#[cfg(feature = "cli-proof")]
pub mod harness;
#[cfg(feature = "cli-proof")]
pub mod receipt;
#[cfg(feature = "cli-proof")]
pub mod sabotage;
#[cfg(feature = "cli-proof")]
pub mod workspace;

#[cfg(feature = "cli-proof")]
pub use harness::{CliHarness, CliOutput};
#[cfg(feature = "cli-proof")]
pub use receipt::ReceiptAssertions;
#[cfg(feature = "cli-proof")]
pub use sabotage::SabotageFixture;
#[cfg(feature = "cli-proof")]
pub use workspace::TempWorkspace;
