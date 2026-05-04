//! # ggen-semantic-bit: The Vision 2030 Kernel
//!
//! This crate provides the zero-cost substrate for compiling operational law
//! into lawful semantic automata.

pub mod compound;
pub mod field;
pub mod law;
pub mod lifecycle;
pub mod machine;
pub mod phase;
pub mod receipt;
pub mod root;

pub use field::Field8;
pub use law::{Law, TableLaw};
pub use machine::Machine;
pub use receipt::{Receipt, Replay};

/// Common result type for semantic operations
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Validation failed: {0}")]
    ValidationFailed(String),
    #[error("Illegal transition from {from} to {to}")]
    IllegalTransition { from: String, to: String },
    #[error("Replay mismatch: {0}")]
    ReplayMismatch(String),
}
