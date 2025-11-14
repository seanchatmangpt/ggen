//! Structured logging infrastructure
//!
//! This module provides logging setup and configuration for ggen applications.
//! It uses the `slog` crate for structured logging with support for multiple
//! output destinations including terminal, syslog, and systemd journald.
//!
//! ## Features
//!
//! - **Structured logging**: Key-value structured log entries
//! - **Multiple drains**: Support for terminal, syslog, and journald outputs
//! - **Async logging**: Non-blocking async log processing
//! - **Feature gating**: Optional features for different logging backends
//!
//! ## Logging Backends
//!
//! The module supports multiple logging backends via feature flags:
//!
//! - **termlog**: Terminal output with colored formatting
//! - **journald**: systemd journald integration (Linux only)
//! - **syslog**: Unix syslog integration (currently disabled)
//!
//! ## Examples
//!
//! ### Setting Up Logging
//!
//! ```rust,no_run
//! use ggen_utils::logger::setup_logging;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Setup logging (returns guard that must be kept alive)
//! let _guard = setup_logging()?;
//!
//! // Now you can use slog macros
//! slog::info!(slog_scope::logger(), "Application started");
//! # Ok(())
//! # }
//! ```
//!
//! ### Using Structured Logging
//!
//! ```rust,no_run
//! use slog::info;
//!
//! # fn main() {
//! let logger = slog_scope::logger();
//! info!(logger, "Processing request";
//!       "method" => "GET",
//!       "path" => "/api/users",
//!       "status" => 200);
//! # }
//! ```

use slog::o;
use slog::Drain;
#[cfg(all(target_os = "linux", feature = "journald"))]
use slog_journald::JournaldDrain;
// #[cfg(feature = "syslog")]
// use slog_syslog_jl::Facility;  // Temporarily disabled due to vulnerability

use super::error::Result;

/// Setup logging for the application.
///
/// # Errors
///
/// Returns an error if logging setup fails.
pub fn setup_logging() -> Result<slog_scope::GlobalLoggerGuard> {
    // Setup Logging
    let guard = slog_scope::set_global_logger(default_root_logger()?);
    slog_stdlog::init()?;

    Ok(guard)
}

/// Create the default root logger.
///
/// # Errors
///
/// Returns an error if logger creation fails.
pub fn default_root_logger() -> Result<slog::Logger> {
    // Create drains
    let drain = slog::Duplicate(default_discard(), default_discard()).fuse();

    // Merge drains
    #[cfg(feature = "termlog")]
    let drain = slog::Duplicate(default_term_drain().unwrap_or(default_discard()), drain).fuse();
    // #[cfg(feature = "syslog")]
    // let drain = slog::Duplicate(default_syslog_drain().unwrap_or(default_discard()), drain).fuse();
    #[cfg(feature = "journald")]
    #[cfg(target_os = "linux")]
    let drain =
        slog::Duplicate(default_journald_drain().unwrap_or(default_discard()), drain).fuse();

    // Create Logger
    let logger = slog::Logger::root(drain, o!("who" => "rust-starter"));

    // Return Logger
    Ok(logger)
}

fn default_discard() -> slog_async::Async {
    slog_async::Async::default(slog::Discard)
}

// term drain: Log to Terminal
#[cfg(feature = "termlog")]
fn default_term_drain() -> Result<slog_async::Async> {
    let plain = slog_term::PlainSyncDecorator::new(std::io::stdout());
    let term = slog_term::FullFormat::new(plain);

    let drain = slog_async::Async::default(term.build().fuse());

    Ok(drain)
}

// syslog drain: Log to syslog
// #[cfg(feature = "syslog")]
// fn default_syslog_drain() -> Result<slog_async::Async> {
//     let syslog = slog_syslog_jl::unix_3164(Facility::LOG_USER)?;
//
//     let drain = slog_async::Async::default(syslog.fuse());
//
//     Ok(drain)
// }

#[cfg(all(target_os = "linux", feature = "journald"))]
fn default_journald_drain() -> Result<slog_async::Async> {
    let journald = JournaldDrain.ignore_res();
    let drain = slog_async::Async::default(journald);

    Ok(drain)
}
