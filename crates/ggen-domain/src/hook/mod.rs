//! Hook management domain layer
//!
//! Pure business logic for hook operations including creation, listing, removal, and monitoring.

pub mod create;
pub mod list;
pub mod monitor;
pub mod remove;

pub use create::{CreateInput, execute_create, HookResult, HookStatus};
pub use list::{ListInput, execute_list, HookInfo};
pub use monitor::{MonitorInput, execute_monitor, MonitorResult};
pub use remove::{RemoveInput, execute_remove};
