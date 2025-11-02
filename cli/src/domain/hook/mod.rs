//! Hook management domain layer - CLI wrappers
//!
//! Pure business logic for hook operations including creation, listing, removal, and monitoring.
//! This module wraps the async domain logic from the `domain` crate.

pub mod create;
pub mod list;
pub mod monitor;
pub mod remove;

pub use create::{CreateArgs, run as run_create};
pub use list::{ListArgs, run as run_list};
pub use monitor::{MonitorArgs, run as run_monitor};
pub use remove::{RemoveArgs, run as run_remove};
