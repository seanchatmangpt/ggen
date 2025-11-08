//! Utility functions domain layer
//!
//! Pure business logic for utility operations including system diagnostics and environment management.

pub mod doctor;
pub mod env;

pub use doctor::{CheckStatus, CheckResult, DoctorInput, DoctorResult, EnvironmentInfo, execute_doctor};
pub use env::{
    EnvironmentManager, GgenEnvironment, DefaultEnvironmentManager,
    execute_env_list, execute_env_get, execute_env_set,
    execute_env_show_dirs, execute_env_ensure_dirs, execute_env_clear_cache,
};
