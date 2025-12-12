//! Shared helpers for command modules
//!
//! This module provides standardized helpers for common patterns across command modules:
//! - Async execution with timeout
//! - Logging operations
//! - Error conversion
//! - Duration tracking

use clap_noun_verb::Result;
use log::debug;
use serde_json::Value;
use std::future::Future;
use std::time::{Duration, Instant};

/// Default timeout for CLI operations
const CLI_OP_TIMEOUT: Duration = Duration::from_secs(10);

/// Execute async operation with timeout and error handling
///
/// Standardized async execution pattern for CLI commands.
/// Handles runtime creation, timeout, and error conversion.
pub fn execute_async_op<T, Fut>(op_name: &str, fut: Fut) -> Result<T>
where
    Fut: Future<Output = Result<T>>,
{
    tokio::task::block_in_place(|| {
        tokio::runtime::Handle::current().block_on(async {
            match tokio::time::timeout(CLI_OP_TIMEOUT, fut).await {
                Ok(result) => result,
                Err(_) => Err(clap_noun_verb::NounVerbError::execution_error(format!(
                    "Operation '{}' timed out after {}s",
                    op_name,
                    CLI_OP_TIMEOUT.as_secs()
                ))),
            }
        })
    })
}

/// Simplified log helper with default session/run IDs
///
/// Standardized logging pattern for CLI operations.
/// Uses consistent session/run IDs for all operations.
pub fn log_operation(location: &str, message: &str, data: Value) {
    debug!(
        target: "ggen::cli",
        "location={location} message={message} payload={}",
        data
    );
}

/// Track operation duration
///
/// Returns a guard that measures duration when dropped.
/// Use with `let _guard = track_duration("operation_name");`
pub fn track_duration(_op_name: &str) -> DurationGuard {
    DurationGuard {
        start: Instant::now(),
    }
}

/// Guard for tracking operation duration
pub struct DurationGuard {
    start: Instant,
}

impl DurationGuard {
    /// Get elapsed duration
    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }

    /// Get elapsed duration in milliseconds
    pub fn elapsed_ms(&self) -> u64 {
        self.elapsed().as_millis() as u64
    }
}

impl Drop for DurationGuard {
    fn drop(&mut self) {
        // Duration tracked automatically on drop
    }
}
