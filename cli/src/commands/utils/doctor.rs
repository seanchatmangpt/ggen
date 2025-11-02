//! Sync CLI wrapper for doctor command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic. This is the proof-of-concept for the v2.0 architecture pattern.
//!
//! ## Architecture Pattern
//! ```
//! User CLI Input → Sync Wrapper (this file) → Runtime Bridge → Async Domain Logic
//! ```
//!
//! ## Responsibilities
//! - Parse CLI arguments via clap
//! - Validate and transform user input
//! - Spawn Tokio runtime
//! - Call async domain functions
//! - Handle errors and format output
//!
//! ## Pattern Benefits
//! - Clear separation of concerns
//! - Testable business logic (domain layer)
//! - Type-safe CLI interface
//! - Consistent error handling
//! - Easy to maintain and extend

use clap::Args;
use ggen_utils::error::Result;

/// Arguments for the doctor command
#[derive(Args, Debug, Clone)]
pub struct DoctorArgs {
    /// Show verbose output with detailed diagnostics
    #[arg(short, long)]
    pub verbose: bool,

    /// Run only a specific check (e.g., "rust", "cargo", "git")
    #[arg(short, long)]
    pub check: Option<String>,

    /// Show environment information
    #[arg(short, long)]
    pub env: bool,
}

/// Execute the doctor command (sync wrapper)
///
/// This function:
/// 1. Validates CLI arguments
/// 2. Spawns a Tokio runtime
/// 3. Calls the async domain function
/// 4. Handles errors and formats output
pub fn run(args: &DoctorArgs) -> Result<()> {
    // Use the runtime bridge to execute async domain logic
    crate::runtime::execute(async {
        crate::domain::utils::doctor::run_doctor(args.verbose, args.check.as_deref(), args.env)
            .await
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doctor_args_defaults() {
        let args = DoctorArgs {
            verbose: false,
            check: None,
            env: false,
        };
        assert!(!args.verbose);
        assert!(args.check.is_none());
        assert!(!args.env);
    }

    #[test]
    fn test_doctor_args_with_options() {
        let args = DoctorArgs {
            verbose: true,
            check: Some("rust".to_string()),
            env: true,
        };
        assert!(args.verbose);
        assert_eq!(args.check.as_deref(), Some("rust"));
        assert!(args.env);
    }
}
