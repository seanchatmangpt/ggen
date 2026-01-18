//! Generated CLI Library
//!
//! Re-exports domain logic for use as a library.

pub mod domain;
pub mod error;

pub use domain::*;
pub use error::{DomainError, DomainResult};
