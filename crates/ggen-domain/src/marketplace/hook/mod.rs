//! Hook management domain layer
//!
//! Pure business logic for hook operations including creation, listing, removal, and monitoring.

pub mod create;
pub mod list;
pub mod monitor;
pub mod remove;

pub use create::{execute_create, CreateInput, HookResult, HookStatus};
pub use list::{execute_list, HookInfo, ListInput};
pub use monitor::{execute_monitor, MonitorInput, MonitorResult};
pub use remove::{execute_remove, RemoveInput};
