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
    let runtime = match tokio::runtime::Runtime::new() {
        Ok(rt) => rt,
        Err(e) => {
            let msg = format!("Failed to create Tokio runtime: {}", e);
            log::error!("{}", msg);
            return Err(ggen_utils::error::Error::new(&msg));
        }
    };

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
pub fn block_on<F, T>(async_op: F) -> Result<T, String>
where
    F: Future<Output = T> + Send,
    T: Send,
{
    // Check if we're already in a runtime
    match tokio::runtime::Handle::try_current() {
        Ok(_) => {
            // Already in runtime - spawn new thread with new runtime to avoid nesting
            std::thread::scope(|s| {
                s.spawn(move || {
                    let rt = match tokio::runtime::Runtime::new() {
                        Ok(runtime) => runtime,
                        Err(e) => {
                            let msg = format!("Failed to create Tokio runtime: {}", e);
                            log::error!("{}", msg);
                            return Err(msg);
                        }
                    };
                    Ok(rt.block_on(async_op))
                })
                .join()
                .map_err(|_| "Runtime thread panicked".to_string())?
            })
        }
        Err(_) => {
            // Not in runtime - create one on current thread
            match tokio::runtime::Runtime::new() {
                Ok(runtime) => Ok(runtime.block_on(async_op)),
                Err(e) => {
                    let msg = format!("Failed to create Tokio runtime: {}", e);
                    log::error!("{}", msg);
                    Err(msg)
                }
            }
        }
    }
}
