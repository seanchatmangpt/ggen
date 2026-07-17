//! Macros Module
//!
//! Re-exports all macro modules for convenient access.
//! Note: `#[macro_use]` is used here to re-export macros from submodules

#[macro_use]
pub mod test;
#[macro_use]
pub mod assert;

#[cfg(all(feature = "weaver", feature = "otel"))]
#[macro_use]
pub mod weaver_test;
