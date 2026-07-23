#![forbid(unsafe_code)]

#[path = "../../src/production.rs"]
mod kernel;

pub use kernel::*;
