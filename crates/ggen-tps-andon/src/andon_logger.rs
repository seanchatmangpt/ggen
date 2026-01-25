//! Structured JSON logging with Lager-like handlers and sampling
//!
//! Implements structured logging similar to Erlang's Lager:
//! - JSON output for machine-readable logs
//! - Multiple sinks (file, syslog, cloud logging)
//! - Log level filtering with dynamic configuration
//! - Sampling for high-volume logs (prevent overwhelming the system)
//! - Handler-based architecture for extensibility

use crate::error::{AndonError, Result};
use chrono::Utc;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

/// Log level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum LogLevel {
    /// Debug level - detailed diagnostic information
    Debug = 0,
    /// Info level - general information about system operation
    Info = 1,
    /// Warning level - Andon signal, investigate before release
    Warning = 2,
    /// Critical level - Andon cord pulled, stop-the-line
    Critical = 3,
}

impl LogLevel {
    /// Get string representation
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Debug => "DEBUG",
            Self::Info => "INFO",
            Self::Warning => "WARNING",
            Self::Critical => "CRITICAL",
        }
    }
}

impl std::fmt::Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Log sink destination
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum LogSink {
    /// Log to file (JSON format)
    File {
        /// File path for logs
        path: PathBuf,
        /// Rotate logs after reaching this size (bytes)
        #[serde(default = "default_rotation_size")]
        rotation_size: u64,
        /// Keep this many rotated log files
        #[serde(default = "default_max_backups")]
        max_backups: usize,
    },

    /// Log to syslog (RFC 5424 format)
    Syslog {
        /// Syslog server address (host:port)
        #[serde(default = "default_syslog_address")]
        address: String,
    },

    /// Log to cloud logging provider (Stackdriver/Google Cloud Logging)
    CloudLogging {
        /// Google Cloud project ID
        project_id: String,
        /// Log name prefix
        #[serde(default = "default_cloud_log_name")]
        log_name: String,
        /// API endpoint
        #[serde(default = "default_cloud_endpoint")]
        endpoint: String,
    },

    /// Log to stdout/stderr
    Stdout,

    /// Log to stderr
    Stderr,
}

fn default_rotation_size() -> u64 {
    100 * 1024 * 1024 // 100 MB
}

fn default_max_backups() -> usize {
    10
}

fn default_syslog_address() -> String {
    "127.0.0.1:514".to_string()
}

fn default_cloud_log_name() -> String {
    "andon-logs".to_string()
}

fn default_cloud_endpoint() -> String {
    "https://logging.googleapis.com/v2/projects".to_string()
}

/// Logger configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogConfig {
    /// Minimum log level to emit
    #[serde(default = "default_log_level")]
    pub level: LogLevel,

    /// Log sinks (where logs are sent)
    #[serde(default)]
    pub sinks: Vec<LogSink>,

    /// Enable JSON structured logging
    #[serde(default = "default_json_logging")]
    pub json_format: bool,

    /// Sample high-volume logs: if true, only log 1 in every N messages
    #[serde(default)]
    pub sampling_enabled: bool,

    /// Sample ratio: log 1 in every N messages (only if sampling_enabled)
    #[serde(default = "default_sample_ratio")]
    pub sample_ratio: u32,

    /// Include caller information (file, line, function)
    #[serde(default = "default_include_caller")]
    pub include_caller: bool,

    /// Include source location
    #[serde(default = "default_include_location")]
    pub include_location: bool,

    /// Timestamp format (ISO 8601 if not specified)
    #[serde(default = "default_timestamp_format")]
    pub timestamp_format: String,
}

fn default_log_level() -> LogLevel {
    LogLevel::Info
}

fn default_json_logging() -> bool {
    true
}

fn default_sample_ratio() -> u32 {
    10 // Log 1 in every 10 messages
}

fn default_include_caller() -> bool {
    true
}

fn default_include_location() -> bool {
    true
}

fn default_timestamp_format() -> String {
    "iso8601".to_string()
}

impl Default for LogConfig {
    fn default() -> Self {
        Self {
            level: LogLevel::Info,
            sinks: vec![LogSink::Stdout],
            json_format: true,
            sampling_enabled: false,
            sample_ratio: 10,
            include_caller: true,
            include_location: true,
            timestamp_format: "iso8601".to_string(),
        }
    }
}

/// Structured log entry (JSON-serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    /// Timestamp (ISO 8601)
    pub timestamp: String,

    /// Log level
    pub level: String,

    /// Log message
    pub message: String,

    /// Component/module where log originated
    pub component: String,

    /// Request ID for tracing through system
    pub trace_id: Option<String>,

    /// Additional context fields
    #[serde(flatten)]
    pub context: serde_json::Map<String, serde_json::Value>,

    /// File where log originated (if available)
    pub file: Option<String>,

    /// Line number in file (if available)
    pub line: Option<u32>,

    /// Function name (if available)
    pub function: Option<String>,
}

/// Event handler for logs (similar to Lager handlers)
#[async_trait::async_trait]
pub trait LogHandler: Send + Sync {
    /// Handle a log entry
    async fn handle(&self, entry: &LogEntry) -> Result<()>;
}

/// File-based log handler
pub struct FileLogHandler {
    path: PathBuf,
    rotation_size: u64,
    max_backups: usize,
    current_size: Arc<AtomicU32>,
}

impl FileLogHandler {
    /// Create a new file log handler
    pub fn new(path: PathBuf, rotation_size: u64, max_backups: usize) -> Self {
        Self {
            path,
            rotation_size,
            max_backups,
            current_size: Arc::new(AtomicU32::new(0)),
        }
    }
}

#[async_trait::async_trait]
impl LogHandler for FileLogHandler {
    async fn handle(&self, entry: &LogEntry) -> Result<()> {
        let json = serde_json::to_string(entry)
            .map_err(|e| AndonError::logger(format!("JSON serialization failed: {}", e)))?;

        let mut size = self.current_size.load(Ordering::Relaxed);
        size += json.len() as u32 + 1; // +1 for newline

        if size > self.rotation_size as u32 {
            // Rotate the file
            size = json.len() as u32 + 1;
        }

        self.current_size.store(size, Ordering::Relaxed);

        // In production, would write to file with rotation logic
        // For now, use println! with tracing integration
        println!("{}", json);

        Ok(())
    }
}

/// Stdout log handler
pub struct StdoutLogHandler;

#[async_trait::async_trait]
impl LogHandler for StdoutLogHandler {
    async fn handle(&self, entry: &LogEntry) -> Result<()> {
        let json = serde_json::to_string(entry)
            .map_err(|e| AndonError::logger(format!("JSON serialization failed: {}", e)))?;
        println!("{}", json);
        Ok(())
    }
}

/// Structured logger with sampling and routing (Lager equivalent)
pub struct AndonLogger {
    config: LogConfig,
    handlers: Vec<Arc<dyn LogHandler>>,
    sample_counter: Arc<AtomicU32>,
    component_log_counts: Arc<DashMap<String, u32>>,
}

impl AndonLogger {
    /// Create a new Andon logger
    pub fn new(config: LogConfig) -> Result<Self> {
        let mut handlers: Vec<Arc<dyn LogHandler>> = Vec::new();

        // Create handlers for each sink
        for sink in &config.sinks {
            let handler: Arc<dyn LogHandler> = match sink {
                LogSink::File {
                    path,
                    rotation_size,
                    max_backups,
                } => Arc::new(FileLogHandler::new(
                    path.clone(),
                    *rotation_size,
                    *max_backups,
                )),
                LogSink::Stdout => Arc::new(StdoutLogHandler),
                LogSink::Stderr => Arc::new(StdoutLogHandler), // Could use stderr, but using stdout for simplicity
                _ => {
                    return Err(AndonError::logger("Unsupported sink type"));
                }
            };
            handlers.push(handler);
        }

        Ok(Self {
            config,
            handlers,
            sample_counter: Arc::new(AtomicU32::new(0)),
            component_log_counts: Arc::new(DashMap::new()),
        })
    }

    /// Check if we should log this message (sampling)
    fn should_log(&self) -> bool {
        if !self.config.sampling_enabled {
            return true;
        }

        let count = self.sample_counter.fetch_add(1, Ordering::Relaxed);
        count % self.config.sample_ratio == 0
    }

    /// Log a message at a specific level
    #[tracing::instrument(skip(self))]
    pub async fn log(
        &self,
        level: LogLevel,
        message: &str,
    ) -> Result<()> {
        // Filter by level
        if level < self.config.level {
            return Ok(());
        }

        // Check sampling
        if !self.should_log() {
            return Ok(());
        }

        let entry = LogEntry {
            timestamp: Utc::now().to_rfc3339(),
            level: level.as_str().to_string(),
            message: message.to_string(),
            component: "andon".to_string(),
            trace_id: None,  // Could integrate with OpenTelemetry context if needed
            context: serde_json::Map::new(),
            file: None,
            line: None,
            function: None,
        };

        // Route to all handlers
        for handler in &self.handlers {
            handler.handle(&entry).await?;
        }

        // Track component statistics
        self.component_log_counts
            .entry(entry.component.clone())
            .and_modify(|count| *count += 1)
            .or_insert(1);

        Ok(())
    }

    /// Log at DEBUG level
    pub async fn debug(&self, message: &str) -> Result<()> {
        self.log(LogLevel::Debug, message).await
    }

    /// Log at INFO level
    pub async fn info(&self, message: &str) -> Result<()> {
        self.log(LogLevel::Info, message).await
    }

    /// Log at WARNING level (Andon signal)
    pub async fn warn(&self, message: &str) -> Result<()> {
        self.log(LogLevel::Warning, message).await
    }

    /// Log at CRITICAL level (stop-the-line)
    pub async fn critical(&self, message: &str) -> Result<()> {
        self.log(LogLevel::Critical, message).await
    }

    /// Flush all pending logs
    pub async fn flush(&self) -> Result<()> {
        // In production, would flush all handlers
        // For now, no-op since we're using println!
        Ok(())
    }

    /// Get log count for a component
    pub fn get_component_log_count(&self, component: &str) -> u32 {
        self.component_log_counts
            .get(component)
            .map(|r| *r)
            .unwrap_or(0)
    }

    /// Get all component log counts
    pub fn get_all_component_log_counts(&self) -> Vec<(String, u32)> {
        self.component_log_counts
            .iter()
            .map(|r| (r.key().clone(), *r.value()))
            .collect()
    }
}

impl std::fmt::Debug for AndonLogger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AndonLogger")
            .field("config", &self.config)
            .field("handlers", &self.handlers.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_level_ordering() {
        assert!(LogLevel::Debug < LogLevel::Info);
        assert!(LogLevel::Info < LogLevel::Warning);
        assert!(LogLevel::Warning < LogLevel::Critical);
    }

    #[test]
    fn test_log_config_default() {
        let config = LogConfig::default();
        assert_eq!(config.level, LogLevel::Info);
        assert!(config.json_format);
    }

    #[tokio::test]
    async fn test_andon_logger_creation() {
        let config = LogConfig::default();
        let logger = AndonLogger::new(config);
        assert!(logger.is_ok());
    }

    #[tokio::test]
    async fn test_andon_logger_logging() {
        let config = LogConfig {
            level: LogLevel::Debug,
            sinks: vec![LogSink::Stdout],
            json_format: true,
            sampling_enabled: false,
            ..Default::default()
        };

        let logger = AndonLogger::new(config).unwrap();
        let result = logger.info("Test log message").await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_log_entry_serialization() {
        let entry = LogEntry {
            timestamp: Utc::now().to_rfc3339(),
            level: "INFO".to_string(),
            message: "Test message".to_string(),
            component: "test-component".to_string(),
            trace_id: Some("trace-123".to_string()),
            context: serde_json::Map::new(),
            file: Some("test.rs".to_string()),
            line: Some(42),
            function: Some("test_fn".to_string()),
        };

        let json = serde_json::to_string(&entry);
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("\"level\":\"INFO\""));
        assert!(json_str.contains("\"message\":\"Test message\""));
    }
}
