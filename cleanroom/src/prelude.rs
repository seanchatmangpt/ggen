//! Prelude module for convenient imports.

pub use crate::backend::{AutoBackend, Backend, Cmd, RunResult};
pub use crate::error::{CleanroomError, Result};
pub use crate::policy::{NetProfile, Policy, ProcProfile, RngProfile, TimeProfile};
pub use crate::run;
pub use crate::scenario::{scenario, Scenario};
