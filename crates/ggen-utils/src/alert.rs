//! Alert Helpers
//!
//! Provides macros and functions for emitting visual alerts (problem indicators)
//! similar to chicago-tdd-tools alert macros. Alerts make problems immediately visible
//! and provide actionable guidance for resolution.
//!
//! ## Usage
//!
//! ```rust
//! use ggen_utils::alert_critical;
//! use ggen_utils::alert_warning;
//! use ggen_utils::alert_info;
//!
//! // Critical alert - must stop immediately
//! alert_critical!("Docker daemon is not running", "Start Docker Desktop");
//!
//! // Warning alert - should stop
//! alert_warning!("Container operation failed", "Check container state");
//!
//! // Info alert - informational
//! alert_info!("Container started successfully");
//! ```
//!
//! ## Alert Levels
//!
//! - **Critical** (🚨): Must stop immediately - cannot proceed
//! - **Warning** (⚠️): Should stop - investigate before proceeding
//! - **Info** (ℹ️): Informational - no action required
//! - **Success** (✅): Success indicator - operation completed
//! - **Debug** (🔍): Debug information - detailed diagnostics
//!
//! ## Integration with Slog
//!
//! When slog logger is initialized, alerts use structured logging.
//! When slog is not available, alerts fall back to eprintln! with emoji formatting.

use std::io::{self, Write};

/// Emit a critical alert (🚨)
///
/// Critical alerts indicate problems that must stop work immediately.
/// Use for errors that prevent further execution.
///
/// # Arguments
///
/// * `message` - The error message
/// * `fix` - Suggested fix (optional)
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert_critical;
///
/// alert_critical!("Docker daemon is not running", "Start Docker Desktop");
/// ```
#[macro_export]
macro_rules! alert_critical {
    ($message:expr, $fix:expr, $($action:expr),+) => {
        {
            let actions: Vec<String> = vec![$($action.to_string()),+];
            let action_str = actions.join("\n   📋 ");
            $crate::alert::_alert_critical_impl($message, Some(&format!("{}\n   📋 {}", $fix, action_str)));
        }
    };
    ($message:expr, $fix:expr) => {
        $crate::alert::_alert_critical_impl($message, Some($fix));
    };
    ($format_str:expr, $($arg:expr),+ $(,)?) => {
        {
            let msg = format!($format_str, $($arg),+);
            $crate::alert::_alert_critical_impl(&msg, None::<&str>);
        }
    };
    ($message:expr) => {
        $crate::alert::_alert_critical_impl($message, None::<&str>);
    };
}

/// Emit a warning alert (⚠️)
///
/// Warning alerts indicate problems that should stop work.
/// Use for errors that may cause issues but don't prevent execution.
///
/// # Arguments
///
/// * `message` - The warning message
/// * `fix` - Suggested fix (optional)
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert_warning;
///
/// alert_warning!("Container operation failed", "Check container state");
/// ```
#[macro_export]
macro_rules! alert_warning {
    ($message:expr, $fix:expr) => {
        $crate::alert::_alert_warning_impl($message, Some($fix));
    };
    ($format_str:expr, $($arg:expr),+ $(,)?) => {
        {
            let msg = format!($format_str, $($arg),+);
            $crate::alert::_alert_warning_impl(&msg, None::<&str>);
        }
    };
    ($message:expr) => {
        $crate::alert::_alert_warning_impl($message, None::<&str>);
    };
}

/// Emit an info alert (ℹ️)
///
/// Info alerts provide informational messages.
/// Use for status updates and non-critical information.
///
/// # Arguments
///
/// * `message` - The info message
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert_info;
///
/// alert_info!("Container started successfully");
/// ```
#[macro_export]
macro_rules! alert_info {
    ($message:expr) => {
        $crate::alert::_alert_info_impl($message);
    };
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            $crate::alert::_alert_info_impl(&msg);
        }
    };
}

/// Emit a success alert (✅)
///
/// Success alerts indicate successful operations.
/// Use to confirm operations completed successfully.
///
/// # Arguments
///
/// * `message` - The success message
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert_success;
///
/// alert_success!("Container started successfully");
/// ```
#[macro_export]
macro_rules! alert_success {
    ($message:expr) => {
        $crate::alert::_alert_success_impl($message);
    };
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            $crate::alert::_alert_success_impl(&msg);
        }
    };
}

/// Emit a debug alert (🔍)
///
/// Debug alerts provide detailed debugging information.
/// Use for detailed diagnostic information during development.
///
/// # Arguments
///
/// * `message` - The debug message
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert_debug;
///
/// alert_debug!("Container state: {:?}", container_state);
/// ```
#[macro_export]
macro_rules! alert_debug {
    ($message:expr) => {
        $crate::alert::_alert_debug_impl($message);
    };
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            $crate::alert::_alert_debug_impl(&msg);
        }
    };
}

/// Emit an alert with custom severity
///
/// Allows emitting custom alerts with user-defined severity levels.
///
/// # Arguments
///
/// * `severity` - Severity emoji (🚨, ⚠️, ℹ️, ✅, 🔍)
/// * `message` - The message
/// * `stop` - Stop message (optional)
/// * `fix` - Fix suggestion (optional)
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert;
///
/// alert!("🚨", "Custom critical error", "STOP: Cannot proceed", "FIX: Resolve issue");
/// ```
#[macro_export]
macro_rules! alert {
    ($severity:expr, $message:expr) => {
        $crate::alert::_alert_custom_impl($severity, $message, None::<&str>, None::<&str>);
    };
    ($severity:expr, $message:expr, $stop:expr, $fix:expr) => {
        $crate::alert::_alert_custom_impl($severity, $message, Some($stop), Some($fix));
    };
    ($severity:expr, $message:expr, $stop:expr, $fix:expr, $($action:expr),+) => {
        {
            let actions: Vec<String> = vec![$($action.to_string()),+];
            let action_str = actions.join("\n   📋 ");
            $crate::alert::_alert_custom_impl($severity, $message, Some($stop), Some(&format!("{}\n   📋 {}", $fix, action_str)));
        }
    };
}

// Implementation functions that check for slog availability

/// Internal implementation for critical alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_critical_impl(message: &str, fix: Option<&str>) {
    if let Some(fix_msg) = fix {
        _try_slog_error(&format!(
            "{}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}",
            message, fix_msg
        ));
        eprintln!(
            "🚨 {}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}",
            message, fix_msg
        );
    } else {
        _try_slog_error(&format!(
            "{}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: Investigate and resolve",
            message
        ));
        eprintln!(
            "🚨 {}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: Investigate and resolve",
            message
        );
    }
}

/// Internal implementation for warning alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_warning_impl(message: &str, fix: Option<&str>) {
    if let Some(fix_msg) = fix {
        _try_slog_warn(&format!(
            "{}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}",
            message, fix_msg
        ));
        eprintln!(
            "⚠️  {}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}",
            message, fix_msg
        );
    } else {
        _try_slog_warn(&format!(
            "{}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: Check and resolve",
            message
        ));
        eprintln!(
            "⚠️  {}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: Check and resolve",
            message
        );
    }
}

/// Internal implementation for info alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_info_impl(message: &str) {
    _try_slog_info(message);
    eprintln!("ℹ️  {}", message);
}

/// Internal implementation for success alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_success_impl(message: &str) {
    _try_slog_info(&format!("✅ {}", message));
    eprintln!("✅ {}", message);
}

/// Internal implementation for debug alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_debug_impl(message: &str) {
    _try_slog_debug(message);
    eprintln!("🔍 {}", message);
}

/// Internal implementation for custom alerts
#[allow(clippy::module_name_repetitions)]
pub fn _alert_custom_impl(severity: &str, message: &str, stop: Option<&str>, fix: Option<&str>) {
    if let (Some(stop_msg), Some(fix_msg)) = (stop, fix) {
        _try_slog_warn(&format!(
            "{} {}\n   {} {}\n   💡 FIX: {}",
            severity, message, severity, stop_msg, fix_msg
        ));
        eprintln!(
            "{} {}\n   {} {}\n   💡 FIX: {}",
            severity, message, severity, stop_msg, fix_msg
        );
    } else {
        _try_slog_info(&format!("{} {}", severity, message));
        eprintln!("{} {}", severity, message);
    }
}

// Try to use slog if available, otherwise fall back to eprintln!

// Try to use slog if available via slog_scope
// Note: slog_scope may not be initialized, so we always fall back to eprintln!
// This ensures alerts are always visible even if slog isn't initialized
fn _try_slog_error(msg: &str) {
    // Try to use slog if available, but always output to stderr as well
    // slog_scope::logger() may panic if not initialized, so we catch that
    let _ = std::panic::catch_unwind(|| {
        let logger = slog_scope::logger();
        slog::error!(logger, "{}", msg);
    });
}

fn _try_slog_warn(msg: &str) {
    let _ = std::panic::catch_unwind(|| {
        let logger = slog_scope::logger();
        slog::warn!(logger, "{}", msg);
    });
}

fn _try_slog_info(msg: &str) {
    let _ = std::panic::catch_unwind(|| {
        let logger = slog_scope::logger();
        slog::info!(logger, "{}", msg);
    });
}

fn _try_slog_debug(msg: &str) {
    let _ = std::panic::catch_unwind(|| {
        let logger = slog_scope::logger();
        slog::debug!(logger, "{}", msg);
    });
}

/// Write alert to a writer
///
/// Allows writing alerts to custom writers (e.g., files, buffers).
///
/// # Arguments
///
/// * `writer` - Writer to write to
/// * `severity` - Severity emoji
/// * `message` - The message
/// * `stop` - Stop message (optional)
/// * `fix` - Fix suggestion (optional)
///
/// # Example
///
/// ```rust
/// use ggen_utils::alert::write_alert;
/// use std::io::BufWriter;
/// use std::fs::File;
///
/// let file = File::create("alert.log").unwrap();
/// let mut writer = BufWriter::new(file);
/// write_alert(&mut writer, "🚨", "Critical error", "STOP: Cannot proceed", "FIX: Resolve issue").unwrap();
/// ```
///
/// # Errors
///
/// Returns an error if writing to the writer fails.
pub fn write_alert<W: Write>(
    writer: &mut W, severity: &str, message: &str, stop: Option<&str>, fix: Option<&str>,
) -> io::Result<()> {
    if let (Some(stop_msg), Some(fix_msg)) = (stop, fix) {
        writeln!(
            writer,
            "{severity} {message}\n   {severity} {stop_msg}\n   💡 FIX: {fix_msg}"
        )?;
    } else if let Some(stop_msg) = stop {
        writeln!(writer, "{severity} {message}\n   {severity} {stop_msg}")?;
    } else {
        writeln!(writer, "{severity} {message}")?;
    }
    Ok(())
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    #[test]
    fn test_alert_critical() {
        // Test critical alert without fix
        alert_critical!("Test critical error");

        // Test critical alert with fix
        alert_critical!("Test critical error", "Test fix");

        // Test critical alert with fix and actions
        alert_critical!("Test critical error", "Test fix", "Action 1", "Action 2");
    }

    #[test]
    fn test_alert_warning() {
        // Test warning alert without fix
        alert_warning!("Test warning");

        // Test warning alert with fix
        alert_warning!("Test warning", "Test fix");

        // Test warning alert with fix and actions
        alert_warning!(
            "Test warning: {} - Actions: {}, {}",
            "Test fix",
            "Action 1",
            "Action 2"
        );
    }

    #[test]
    fn test_alert_info() {
        // Test info alert
        alert_info!("Test info");

        // Test info alert with details
        alert_info!("Test info: {}, {}", "Detail 1", "Detail 2");
    }

    #[test]
    fn test_alert_success() {
        // Test success alert
        alert_success!("Test success");

        // Test success alert with details
        alert_success!("Test success: {}, {}", "Detail 1", "Detail 2");
    }

    #[test]
    fn test_alert_debug() {
        // Test debug alert
        alert_debug!("Test debug");

        // Test debug alert with format
        alert_debug!("Test debug: {}", "value");
    }

    #[test]
    fn test_alert_custom() {
        // Test custom alert
        alert!("🚨", "Custom critical");

        // Test custom alert with stop and fix
        alert!(
            "🚨",
            "Custom critical",
            "STOP: Cannot proceed",
            "FIX: Resolve issue"
        );

        // Test custom alert with stop, fix, and actions
        alert!(
            "🚨",
            "Custom critical",
            "STOP: Cannot proceed",
            "FIX: Resolve issue",
            "Action 1",
            "Action 2"
        );
    }

    #[test]
    fn test_write_alert() {
        let mut buffer = Vec::new();

        // Test write alert without stop/fix
        write_alert(&mut buffer, "🚨", "Test error", None, None).unwrap();
        let output = String::from_utf8_lossy(&buffer);
        assert!(output.contains("🚨 Test error"));

        // Test write alert with stop
        buffer.clear();
        write_alert(
            &mut buffer,
            "🚨",
            "Test error",
            Some("STOP: Cannot proceed"),
            None,
        )
        .unwrap();
        let output = String::from_utf8_lossy(&buffer);
        assert!(output.contains("🚨 Test error"));
        assert!(output.contains("STOP: Cannot proceed"));

        // Test write alert with stop and fix
        buffer.clear();
        write_alert(
            &mut buffer,
            "🚨",
            "Test error",
            Some("STOP: Cannot proceed"),
            Some("FIX: Resolve issue"),
        )
        .unwrap();
        let output = String::from_utf8_lossy(&buffer);
        assert!(output.contains("🚨 Test error"));
        assert!(output.contains("STOP: Cannot proceed"));
        assert!(output.contains("FIX: Resolve issue"));
    }
}
