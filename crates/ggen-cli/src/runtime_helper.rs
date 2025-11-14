//! Runtime helper for sync CLI wrappers
//!
//! This module provides utilities for executing async operations in sync CLI contexts.
//! Required because clap-noun-verb v3.4.0 uses sync verb functions, but business logic
//! may require async operations (file I/O, network requests, etc.).
//!
//! # Examples
//!
//! ```rust
//! use cli::runtime_helper::execute_async;
//! use clap_noun_verb::Result;
//!
//! fn my_sync_command() -> Result<Output> {
//!     execute_async(async {
//!         // Async business logic here
//!         let result = async_operation().await?;
//!         Ok(result)
//!     })
//!     .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e))
//! }
//! ```

use tokio::runtime::Runtime;

/// Create a new tokio runtime for async operations in sync context
///
/// IMPORTANT: This function detects if we're already inside a tokio runtime
/// (e.g., when using #[tokio::main]) and returns an error in that case.
/// Use execute_async() or execute_async_verb() instead, which handle this properly.
///
/// # Errors
///
/// Returns error if runtime creation fails (rare, usually indicates system resource issues)
/// or if called from within an existing runtime.
pub fn create_runtime() -> Result<Runtime, String> {
    // Check if we're already in a tokio runtime
    if tokio::runtime::Handle::try_current().is_ok() {
        return Err(
            "Cannot create runtime from within a runtime. Use Handle::current() instead."
                .to_string(),
        );
    }
    Runtime::new().map_err(|e| format!("Failed to create async runtime: {}", e))
}

/// Execute an async function in a sync context
///
/// Detects if we're already in a tokio runtime and uses Handle::current() if so,
/// otherwise creates a new runtime. This prevents nested runtime panics.
///
/// # Examples
///
/// ```rust
/// use cli::runtime_helper::execute_async;
///
/// fn sync_function() -> Result<String, String> {
///     execute_async(async {
///         let data = fetch_data().await?;
///         Ok(data)
///     })
/// }
/// ```
///
/// # Errors
///
/// Returns error if:
/// - Runtime creation fails
/// - The async future returns an error
pub fn execute_async<F, T>(future: F) -> Result<T, String>
where
    F: std::future::Future<Output = Result<T, String>> + Send + 'static,
    T: Send + 'static,
{
    // Check if we're already in a tokio runtime
    match tokio::runtime::Handle::try_current() {
        Ok(_handle) => {
            // We're in a runtime, spawn a blocking task to run the future in a new thread
            std::thread::scope(|s| {
                s.spawn(|| {
                    // Create a new runtime in this thread
                    let rt = Runtime::new()
                        .map_err(|e| format!("Failed to create async runtime: {}", e))?;
                    rt.block_on(future)
                })
                .join()
                .unwrap_or_else(|e| Err(format!("Thread panicked: {:?}", e)))
            })
        }
        Err(_) => {
            // No runtime, create one
            let rt =
                Runtime::new().map_err(|e| format!("Failed to create async runtime: {}", e))?;
            rt.block_on(future)
        }
    }
}

/// Execute an async function and convert errors to clap_noun_verb::NounVerbError
///
/// Detects if we're already in a tokio runtime and uses Handle::current() if so,
/// otherwise creates a new runtime. Automatically converts anyhow errors to
/// NounVerbError for use in verb functions.
///
/// # Examples
///
/// ```rust
/// use cli::runtime_helper::execute_async_verb;
/// use clap_noun_verb::Result;
///
/// #[verb("doctor", "utils")]
/// fn utils_doctor() -> Result<DoctorOutput> {
///     execute_async_verb(async {
///         run_diagnostics().await
///     })
/// }
/// ```
pub fn execute_async_verb<F, T>(future: F) -> clap_noun_verb::Result<T>
where
    F: std::future::Future<Output = anyhow::Result<T>> + Send + 'static,
    T: Send + 'static,
{
    // Check if we're already in a tokio runtime
    match tokio::runtime::Handle::try_current() {
        Ok(_handle) => {
            // We're in a runtime, spawn a blocking task to run the future
            // This prevents "Cannot start a runtime from within a runtime" error
            std::thread::scope(|s| {
                s.spawn(|| {
                    // Create a new runtime in this thread
                    let rt = Runtime::new().map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Failed to create async runtime: {}",
                            e
                        ))
                    })?;
                    rt.block_on(future)
                        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))
                })
                .join()
                .unwrap_or_else(|e| {
                    Err(clap_noun_verb::NounVerbError::execution_error(format!(
                        "Thread panicked: {:?}",
                        e
                    )))
                })
            })
        }
        Err(_) => {
            // No runtime, create one
            let rt = Runtime::new().map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to create async runtime: {}",
                    e
                ))
            })?;
            rt.block_on(future)
                .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_runtime() {
        let result = create_runtime();
        assert!(result.is_ok(), "Runtime creation should succeed");
    }

    #[test]
    fn test_execute_async_success() {
        let result = execute_async(async { Ok::<i32, String>(42) });
        assert_eq!(result, Ok(42));
    }

    #[test]
    fn test_execute_async_error() {
        let result = execute_async(async { Err::<i32, String>("test error".to_string()) });
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "test error");
    }

    #[test]
    fn test_execute_async_verb_success() {
        let result = execute_async_verb(async { Ok::<i32, String>(42) });
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_async_verb_error() {
        let result = execute_async_verb(async { Err::<i32, String>("test error".to_string()) });
        assert!(result.is_err());
    }
}
