//! Commands module - Feature-gated command implementations
//!
//! This module contains optional command implementations that can be enabled
//! via feature flags. By default, only core commands are available through the
//! main CLI.

/// PaaS Submodule Management Commands
///
/// Implements semantic noun-verb CLI for managing ggen-spec-kit and clap-noun-verb
/// submodules. Includes operations for initialization, validation, synchronization,
/// and deployment.
///
/// Enable with `--features=paas` or via the `full` feature set.
#[cfg(feature = "paas")]
pub mod paas;
