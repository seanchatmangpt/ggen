//! Quality & Validation
//!
//! Quality assurance and constraint validation: test coverage analysis,
//! guard constraints (runtime and compile-time), Jobs To Be Done validation,
//! and performance validation.

pub mod advanced_phases;
pub mod coverage;
pub mod guards;
pub mod jtbd;
pub mod performance;
pub mod thermal;

// Re-export commonly used items
pub use advanced_phases::*;
pub use coverage::*;
pub use guards::*;
pub use jtbd::*;
pub use performance::*;
pub use thermal::*;
