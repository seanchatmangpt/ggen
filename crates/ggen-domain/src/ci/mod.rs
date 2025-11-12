//! CI/CD utilities domain layer
//!
//! Pure business logic for continuous integration and deployment operations.

pub mod workflow;

pub use workflow::{WorkflowLogViewer, WorkflowManager, WorkflowStatusChecker};
