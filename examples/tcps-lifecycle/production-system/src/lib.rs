#![forbid(unsafe_code)]

#[path = "../../src/production.rs"]
mod kernel;
pub mod policy;

pub use kernel::*;
