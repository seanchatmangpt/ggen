//! Workflow pattern implementations
//!
//! Complete catalog of 43 workflow patterns organized by category

pub mod basic;
pub mod advanced;
pub mod structural;
pub mod multi_instance;
pub mod state_based;
pub mod cancellation;
pub mod iteration;

// Re-export all patterns
pub use basic::*;
pub use advanced::*;
pub use structural::*;
pub use multi_instance::*;
pub use state_based::*;
pub use cancellation::*;
pub use iteration::*;
