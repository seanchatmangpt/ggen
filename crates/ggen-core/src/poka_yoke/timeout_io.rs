//! Timeout enforcement for I/O operations.
//!
//! Prevents CLI hangs by enforcing SLO-based timeouts.

use std::time::Duration;

use ggen_utils::error::{Error, Result};

/// SLO-based timeout constants.
pub mod timeouts {
    use super::Duration;

    /// Quick operations (5 seconds).
    pub const QUICK_CHECK: Duration = Duration::from_secs(5);

    /// Compilation operations (30 seconds).
    pub const COMPILATION: Duration = Duration::from_secs(30);

    /// Network operations (30 seconds).
    pub const NETWORK: Duration = Duration::from_secs(30);

    /// File I/O operations (10 seconds).
    pub const FILE_IO: Duration = Duration::from_secs(10);
}

/// Timeout enforcement for I/O operations.
pub struct TimeoutIO;

impl TimeoutIO {
    /// Creates an HTTP client with timeout configuration.
    ///
    /// # Timeouts
    ///
    /// - Connect timeout: 10 seconds
    /// - Request timeout: Configurable
    ///
    /// # Example
    ///
    /// ```no_run
    /// use ggen_core::poka_yoke::{TimeoutIO, timeouts};
    ///
    /// let client = TimeoutIO::http_client(timeouts::NETWORK)?;
    /// # Ok::<(), ggen_core::error::Error>(())
    /// ```
    pub fn http_client(timeout: Duration) -> Result<reqwest::Client> {
        reqwest::Client::builder()
            .timeout(timeout)
            .connect_timeout(Duration::from_secs(10))
            .pool_max_idle_per_host(10)
            .build()
            .map_err(|e| Error::network_error(&format!("Failed to create HTTP client: {}", e)))
    }
}
