//! Runtime utilities for bridging async/sync boundaries
//!
//! This module provides utilities for executing async code in sync contexts,
//! particularly for CLI commands that need to call async domain functions.

use ggen_utils::error::Result;
use std::future::Future;

/// Execute an async function in a sync context
///
/// This creates a new Tokio runtime and blocks on the provided future.
/// Used by CLI commands to bridge to async domain functions.
///
/// # Examples
///
/// ```no_run
/// use ggen_utils::error::Result;
///
/// fn sync_command() -> Result<()> {
///     crate::runtime::execute(async {
///         // Async domain logic here
///         Ok(())
///     })
/// }
/// ```
pub fn execute<F>(future: F) -> Result<()>
where
    F: Future<Output = Result<()>>,
{
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!(
            "Failed to create Tokio runtime: {}",
            e
        )))?;

    runtime.block_on(future)
}

/// Block on an async function with a generic return type
///
/// Similar to `execute` but supports any return type, not just Result<()>.
/// Used for domain functions that return values.
///
/// # Examples
///
/// ```no_run
/// use ggen_utils::error::Result;
///
/// fn get_data() -> Result<String> {
///     crate::runtime::block_on(async {
///         Ok("data".to_string())
///     })
/// }
/// ```
pub fn block_on<F, T>(future: F) -> T
where
    F: Future<Output = T>,
{
    let runtime = tokio::runtime::Runtime::new()
        .expect("Failed to create Tokio runtime");

    runtime.block_on(future)
}
