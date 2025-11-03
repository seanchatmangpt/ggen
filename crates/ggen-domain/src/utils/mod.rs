//! Utility functions domain layer
//!
//! Pure business logic for utility operations including system diagnostics and environment management.

pub mod doctor;
pub mod env;

pub use doctor::{SystemCheck, SystemChecker};
pub use env::{EnvironmentManager, GgenEnvironment, EnvArgs};
