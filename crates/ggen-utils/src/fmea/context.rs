//! FMEA instrumentation trait and macro for error tracking.
//!
//! Provides ergonomic API for recording failure events:
//! - `FmeaContext` trait extends Result<T, E>
//! - `fmea_track!` macro for concise usage
//! - Zero overhead on success path
//! - Minimal overhead on error path (<1μs)

use super::{FailureEvent, FMEA_REGISTRY};
use crate::error::{Error, Result};

/// Extension trait for instrumenting Result<T, E> with FMEA tracking.
///
/// Adds `.fmea_context()` method to Result types for recording failure events.
/// Only records on error path (zero cost on success).
pub trait FmeaContext<T> {
    /// Records failure event if Result is Err, then returns the Result unchanged.
    ///
    /// # Parameters
    ///
    /// - `mode_id`: Failure mode ID (e.g., "file_io_write_fail")
    /// - `operation`: Operation name (e.g., "template_generation")
    ///
    /// # Example
    ///
    /// ```ignore
    /// std::fs::write(&path, &content)
    ///     .fmea_context("file_io_write_fail", "template_generation")?;
    /// ```
    fn fmea_context(self, mode_id: &str, operation: &str) -> Result<T>;
}

impl<T, E: Into<Error>> FmeaContext<T> for std::result::Result<T, E> {
    fn fmea_context(self, mode_id: &str, operation: &str) -> Result<T> {
        self.map_err(|e| {
            let error: Error = e.into();

            // Record event in FMEA registry (cold path only)
            if let Ok(mut registry) = FMEA_REGISTRY.write() {
                let event = FailureEvent::new(
                    mode_id.to_string(),
                    operation.to_string(),
                    error.to_string(),
                );
                registry.record_event(event);
            }

            error
        })
    }
}

/// Macro for concise FMEA tracking with automatic error propagation.
///
/// # Example
///
/// ```ignore
/// fmea_track!("file_io_write_fail", "template_generation", {
///     std::fs::write(&path, &content)?;
///     std::fs::set_permissions(&path, perms)?;
/// })?;
/// ```
///
/// Equivalent to:
///
/// ```ignore
/// (|| -> Result<_> {
///     std::fs::write(&path, &content)?;
///     std::fs::set_permissions(&path, perms)?;
///     Ok(())
/// })().fmea_context("file_io_write_fail", "template_generation")?;
/// ```
#[macro_export]
macro_rules! fmea_track {
    ($mode:expr, $op:expr, $block:expr) => {{
        use $crate::fmea::FmeaContext;
        (|| -> $crate::error::Result<_> { $block })().fmea_context($mode, $op)
    }};
}
