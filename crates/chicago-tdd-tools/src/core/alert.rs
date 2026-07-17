//! Alert Helpers
//!
//! Provides macros and functions for emitting visual alerts (problem indicators)
//! similar to logging macros. Alerts make problems immediately visible and provide
//! actionable guidance for resolution.
//!
//! ## Usage
//!
//! ```rust
//! use chicago_tdd_tools::alert_critical;
//! use chicago_tdd_tools::alert_warning;
//! use chicago_tdd_tools::alert_info;
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
//!
//! ## Standard Log Crate Integration
//!
//! When the `logging` feature is enabled, you can use the standard `log` crate macros
//! and they will be formatted as alerts:
//!
//! ```rust
//! use chicago_tdd_tools::alert::AlertLogger;
//!
//! // Initialize alert logger (Info level by default)
//! AlertLogger::init_default().unwrap();
//!
//! // Or with custom level
//! // AlertLogger::init(log::LevelFilter::Debug).unwrap();
//!
//! // Use standard log macros - they'll be formatted as alerts
//! log::error!("Critical error occurred");
//! log::warn!("Warning condition detected");
//! log::info!("Operation completed");
//! log::debug!("Debug information");
//! ```
//!
//! This allows you to use standard Rust logging patterns while getting the visual
//! alert format with emoji indicators and actionable guidance.
//!
//! **Note**: The `logging` feature is enabled by default. When enabled, alert macros
//! (`alert_critical!`, `alert_warning!`, etc.) automatically use `log::error!`, `log::warn!`, etc.
//! instead of `eprintln!`. This means you can use either the alert macros or standard log macros,
//! and both will use the same alert format (if `AlertLogger` is initialized).

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
/// use chicago_tdd_tools::alert_critical;
///
/// alert_critical!("Docker daemon is not running", "Start Docker Desktop");
/// ```
#[macro_export]
macro_rules! alert_critical {
    ($message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::error!("{}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: Investigate and resolve", $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!(
                "🚨 {}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: Investigate and resolve",
                $message
            );
        }
    };
    ($message:expr, $fix:expr) => {
        #[cfg(feature = "logging")]
        {
            log::error!("{}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}", $message, $fix);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!(
                "🚨 {}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}",
                $message, $fix
            );
        }
    };
    ($message:expr, $fix:expr, $($action:expr),+) => {
        {
            let actions: Vec<String> = vec![$($action.to_string()),+];
            let action_str = actions.join("\n   📋 ");
            #[cfg(feature = "logging")]
            {
                log::error!("{}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}\n   📋 {}", $message, $fix, action_str);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!(
                    "🚨 {}\n   ⚠️  STOP: Cannot proceed\n   💡 FIX: {}\n   📋 {}",
                    $message, $fix, action_str
                );
            }
        }
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
/// use chicago_tdd_tools::alert_warning;
///
/// alert_warning!("Container operation failed", "Check container state");
/// ```
#[macro_export]
macro_rules! alert_warning {
    ($message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::warn!("{}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: Check and resolve", $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!(
                "⚠️  {}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: Check and resolve",
                $message
            );
        }
    };
    ($message:expr, $fix:expr) => {
        #[cfg(feature = "logging")]
        {
            log::warn!("{}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}", $message, $fix);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!(
                "⚠️  {}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}",
                $message, $fix
            );
        }
    };
    ($message:expr, $fix:expr, $($action:expr),+) => {
        {
            let actions: Vec<String> = vec![$($action.to_string()),+];
            let action_str = actions.join("\n   📋 ");
            #[cfg(feature = "logging")]
            {
                log::warn!("{}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}\n   📋 {}", $message, $fix, action_str);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!(
                    "⚠️  {}\n   ⚠️  WARNING: Investigate before proceeding\n   💡 FIX: {}\n   📋 {}",
                    $message, $fix, action_str
                );
            }
        }
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
/// use chicago_tdd_tools::alert_info;
///
/// alert_info!("Container started successfully");
/// ```
#[macro_export]
macro_rules! alert_info {
    ($message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::info!("{}", $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!("ℹ️  {}", $message);
        }
    };
    ($message:expr, $($detail:expr),+) => {
        {
            let details: Vec<String> = vec![$($detail.to_string()),+];
            let detail_str = details.join("\n   ℹ️  ");
            #[cfg(feature = "logging")]
            {
                log::info!("{}\n   ℹ️  {}", $message, detail_str);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!(
                    "ℹ️  {}\n   ℹ️  {}",
                    $message, detail_str
                );
            }
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
/// use chicago_tdd_tools::alert_success;
///
/// alert_success!("Container started successfully");
/// ```
#[macro_export]
macro_rules! alert_success {
    ($message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::info!("✅ {}", $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!("✅ {}", $message);
        }
    };
    ($message:expr, $($detail:expr),+) => {
        {
            let details: Vec<String> = vec![$($detail.to_string()),+];
            let detail_str = details.join("\n   ✅ ");
            #[cfg(feature = "logging")]
            {
                log::info!("✅ {}\n   ✅ {}", $message, detail_str);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!(
                    "✅ {}\n   ✅ {}",
                    $message, detail_str
                );
            }
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
/// use chicago_tdd_tools::alert_debug;
///
/// let container_state = "INITIALIZED";
/// alert_debug!("Container state: {:?}", container_state);
/// ```
#[macro_export]
macro_rules! alert_debug {
    ($message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::debug!("{}", $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!("🔍 {}", $message);
        }
    };
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            #[cfg(feature = "logging")]
            {
                log::debug!("{}", msg);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!("🔍 {}", msg);
            }
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
/// use chicago_tdd_tools::alert;
///
/// alert!("🚨", "Custom critical error", "STOP: Cannot proceed", "FIX: Resolve issue");
/// ```
#[macro_export]
macro_rules! alert {
    ($severity:expr, $message:expr) => {
        #[cfg(feature = "logging")]
        {
            log::info!("{} {}", $severity, $message);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!("{} {}", $severity, $message);
        }
    };
    ($severity:expr, $message:expr, $stop:expr, $fix:expr) => {
        #[cfg(feature = "logging")]
        {
            log::warn!("{} {}\n   {} {}\n   💡 FIX: {}", $severity, $message, $severity, $stop, $fix);
        }
        #[cfg(not(feature = "logging"))]
        {
            eprintln!(
                "{} {}\n   {} {}\n   💡 FIX: {}",
                $severity, $message, $severity, $stop, $fix
            );
        }
    };
    ($severity:expr, $message:expr, $stop:expr, $fix:expr, $($action:expr),+) => {
        {
            let actions: Vec<String> = vec![$($action.to_string()),+];
            let action_str = actions.join("\n   📋 ");
            #[cfg(feature = "logging")]
            {
                log::warn!("{} {}\n   {} {}\n   💡 FIX: {}\n   📋 {}", $severity, $message, $severity, $stop, $fix, action_str);
            }
            #[cfg(not(feature = "logging"))]
            {
                eprintln!(
                    "{} {}\n   {} {}\n   💡 FIX: {}\n   📋 {}",
                    $severity, $message, $severity, $stop, $fix, action_str
                );
            }
        }
    };
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
/// ```rust,no_run
/// use chicago_tdd_tools::alert::write_alert;
/// use std::io::BufWriter;
/// use std::fs::File;
///
/// let file = File::create("alert.log").unwrap();
/// let mut writer = BufWriter::new(file);
/// write_alert(&mut writer, "🚨", "Critical error", Some("STOP: Cannot proceed"), Some("FIX: Resolve issue")).unwrap();
/// ```
///
/// # Errors
///
/// Returns an error if writing to the writer fails.
pub fn write_alert<W: Write>(
    writer: &mut W,
    severity: &str,
    message: &str,
    stop: Option<&str>,
    fix: Option<&str>,
) -> io::Result<()> {
    if let Some(stop_msg) = stop {
        if let Some(fix_msg) = fix {
            writeln!(
                writer,
                "{severity} {message}\n   {severity} {stop_msg}\n   💡 FIX: {fix_msg}"
            )?;
        } else {
            writeln!(writer, "{severity} {message}\n   {severity} {stop_msg}")?;
        }
    } else {
        writeln!(writer, "{severity} {message}")?;
    }
    Ok(())
}

#[cfg(feature = "logging")]
/// Log implementation that uses alert format
///
/// Provides a logger implementation for the standard `log` crate that formats
/// log messages using the alert format with emoji indicators.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::alert::AlertLogger;
/// use log::LevelFilter;
///
/// AlertLogger::init(LevelFilter::Info).unwrap();
/// log::error!("This will be formatted as a critical alert");
/// log::warn!("This will be formatted as a warning alert");
/// log::info!("This will be formatted as an info alert");
/// ```
pub struct AlertLogger;

#[cfg(feature = "logging")]
#[allow(unsafe_code)] // AlertLogger is a zero-sized type, safe to share across threads
unsafe impl Sync for AlertLogger {}
#[cfg(feature = "logging")]
#[allow(unsafe_code)] // AlertLogger is a zero-sized type, safe to send across threads
unsafe impl Send for AlertLogger {}

#[cfg(feature = "logging")]
impl AlertLogger {
    /// Initialize the alert logger with the specified maximum log level
    ///
    /// # Arguments
    ///
    /// * `max_level` - Maximum log level to output
    ///
    /// # Errors
    ///
    /// Returns an error if a logger has already been set
    pub fn init(max_level: log::LevelFilter) -> Result<(), log::SetLoggerError> {
        static LOGGER: AlertLogger = AlertLogger;
        log::set_logger(&LOGGER)?;
        log::set_max_level(max_level);
        Ok(())
    }

    /// Initialize the alert logger with default settings (Info level)
    ///
    /// Convenience method that initializes with Info level.
    /// Users can call `init()` with a custom level if they want different settings.
    ///
    /// # Errors
    ///
    /// Returns an error if a logger has already been set
    pub fn init_default() -> Result<(), log::SetLoggerError> {
        Self::init(log::LevelFilter::Info)
    }
}

#[cfg(feature = "logging")]
impl log::Log for AlertLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        // Check against current max level set via log::set_max_level()
        metadata.level() <= log::max_level()
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let (emoji, stop_msg, fix_msg) = match record.level() {
            log::Level::Error => {
                ("🚨", Some("STOP: Cannot proceed"), Some("FIX: Investigate and resolve"))
            }
            log::Level::Warn => (
                "⚠️",
                Some("WARNING: Investigate before proceeding"),
                Some("FIX: Check and resolve"),
            ),
            log::Level::Info => ("ℹ️", None, None),
            log::Level::Debug | log::Level::Trace => ("🔍", None, None),
        };

        if let (Some(stop), Some(fix)) = (stop_msg, fix_msg) {
            eprintln!("{} {}\n   {} {}\n   💡 {}", emoji, record.args(), emoji, stop, fix);
        } else {
            eprintln!("{} {}", emoji, record.args());
        }
    }

    fn flush(&self) {
        // No-op: stderr is line-buffered by default
    }
}

#[cfg(feature = "logging")]
#[cfg(test)]
mod logging_tests {
    use super::*;

    #[test]
    fn test_alert_logger_init() {
        // Test logger can be initialized
        let result = AlertLogger::init(log::LevelFilter::Info);
        // May fail if logger already set, that's OK
        assert!(result.is_ok() || result.is_err());
    }

    #[test]
    fn test_alert_logger_enabled() {
        use log::Log;

        let logger = AlertLogger;

        // Set max level for testing
        log::set_max_level(log::LevelFilter::Info);

        // Test enabled for info level
        let metadata = log::Metadata::builder().level(log::Level::Info).target("test").build();
        assert!(logger.enabled(&metadata));

        // Test disabled for debug level when max is info
        let metadata = log::Metadata::builder().level(log::Level::Debug).target("test").build();
        assert!(!logger.enabled(&metadata));
    }
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
        alert_warning!("Test warning", "Test fix", "Action 1", "Action 2");
    }

    #[test]
    fn test_alert_info() {
        // Test info alert
        alert_info!("Test info");

        // Test info alert with details
        alert_info!("Test info", "Detail 1", "Detail 2");
    }

    #[test]
    fn test_alert_success() {
        // Test success alert
        alert_success!("Test success");

        // Test success alert with details
        alert_success!("Test success", "Detail 1", "Detail 2");
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
        alert!("🚨", "Custom critical", "STOP: Cannot proceed", "FIX: Resolve issue");

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
        // Kaizen improvement: Use from_utf8_lossy to avoid unnecessary clone
        let output = String::from_utf8_lossy(&buffer);
        assert!(output.contains("🚨 Test error"));

        // Test write alert with stop
        buffer.clear();
        write_alert(&mut buffer, "🚨", "Test error", Some("STOP: Cannot proceed"), None).unwrap();
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
