//! Runtime helper for sync CLI wrappers
//!
//! This module provides utilities for executing async operations in sync CLI contexts.
//! Required because clap-noun-verb v3.0.0 uses sync verb functions, but business logic
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
/// # Errors
///
/// Returns error if runtime creation fails (rare, usually indicates system resource issues)
pub fn create_runtime() -> Result<Runtime, String> {
    Runtime::new().map_err(|e| format!("Failed to create async runtime: {}", e))
}

/// Execute an async function in a sync context
///
/// Creates a tokio runtime and blocks on the provided future.
/// Useful for CLI commands that need to call async business logic.
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
    F: std::future::Future<Output = Result<T, String>>,
{
    let rt = create_runtime()?;
    rt.block_on(future)
}

/// Execute an async function and convert errors to clap_noun_verb::NounVerbError
///
/// Convenience wrapper around `execute_async` that automatically converts
/// string errors to NounVerbError for use in verb functions.
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
    F: std::future::Future<Output = Result<T, String>>,
{
    execute_async(future).map_err(clap_noun_verb::NounVerbError::execution_error)
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
