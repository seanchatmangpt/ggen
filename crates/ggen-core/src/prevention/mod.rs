//! Prevention Systems - Design for Lean Six Sigma (DfLSS)
//!
//! This module implements comprehensive prevention systems that stop
//! defects and waste at the design phase.
//!
//! # Prevention Systems
//!
//! 1. **State Machines** - PhantomData-based compile-time guarantees
//! 2. **Contracts** - Trait-based architectural integration contracts
//! 3. **Errors** - Comprehensive error taxonomy and propagation
//! 4. **Reviews** - DfLSS design review process
//! 5. **Kaizen** - Continuous improvement cycle
//!
//! # Philosophy
//!
//! Prevention is 10x cheaper than detection, 100x cheaper than correction.

pub mod state_machine;
pub mod contracts;
pub mod errors;

// Re-export commonly used types
pub use state_machine::{Registry, Initialized, Uninitialized, Validated};
pub use contracts::{TemplateProvider, CliBridge, RenderEngine};
pub use errors::{GgenError, Result, ErrorContext};
