//! Comprehensive security logging system
//!
//! This module provides a unified security logging interface that combines:
//! - Event logging with structured data
//! - Immutable audit trail with tamper-proofing
//! - Intrusion detection with pattern matching
//! - Security metrics collection
//! - Real-time alerting
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_core::security::logging::SecurityLogger;
//! use ggen_core::security::events::{SecurityEvent, SecuritySeverity, EventCategory};
//! use std::path::PathBuf;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Create logger with audit trail persistence
//! let mut logger = SecurityLogger::with_repository(PathBuf::from("./audit"))?;
//!
//! // Log a security event
//! let event = SecurityEvent::new(
//!     SecuritySeverity::High,
//!     EventCategory::Authentication,
//!     "Login attempt failed"
//! );
//! logger.log(event)?;
//!
//! // Get security metrics
//! let metrics = logger.get_metrics_for_last_hour();
//! println!("Failed logins in last hour: {}", metrics.failed_auth_count);
//! # Ok(())
//! # }
//! ```

use super::alerting::{AlertManager, ConsoleAlertHandler, FileAlertHandler};
use super::audit_trail::{AuditError, AuditTrail};
use super::events::SecurityEvent;
use super::intrusion_detection::{DetectionError, IntrusionDetector};
use super::metrics::{MetricsCollector, SecurityMetrics, TimeWindow};
use std::path::PathBuf;
use std::sync::Arc;
use thiserror::Error;
use tracing::{error, info, warn};

/// Security logging errors
#[derive(Debug, Error)]
pub enum LoggingError {
    #[error("Audit error: {0}")]
    AuditError(#[from] AuditError),

    #[error("Detection error: {0}")]
    DetectionError(#[from] DetectionError),

    #[error("Alert error: {0}")]
    AlertError(#[from] super::alerting::AlertError),

    #[error("Metrics error: {0}")]
    MetricsError(#[from] super::metrics::MetricsError),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

/// Security logger configuration
#[derive(Debug, Clone)]
pub struct SecurityLoggerConfig {
    /// Enable audit trail
    pub enable_audit: bool,
    /// Repository path for audit trail
    pub audit_repo_path: Option<PathBuf>,
    /// Enable intrusion detection
    pub enable_intrusion_detection: bool,
    /// Enable metrics collection
    pub enable_metrics: bool,
    /// Enable alerting
    pub enable_alerting: bool,
    /// Alert log file path
    pub alert_log_path: Option<PathBuf>,
}

impl Default for SecurityLoggerConfig {
    fn default() -> Self {
        Self {
            enable_audit: true,
            enable_intrusion_detection: true,
            enable_metrics: true,
            enable_alerting: true,
            audit_repo_path: None,
            alert_log_path: None,
        }
    }
}

/// Comprehensive security logger
pub struct SecurityLogger {
    /// Configuration
    config: SecurityLoggerConfig,
    /// Audit trail
    audit_trail: Option<AuditTrail>,
    /// Intrusion detector
    intrusion_detector: Option<IntrusionDetector>,
    /// Metrics collector
    metrics: Option<MetricsCollector>,
    /// Alert manager
    alert_manager: Option<AlertManager>,
}

impl SecurityLogger {
    /// Create a new security logger with default configuration
    pub fn new() -> Result<Self, LoggingError> {
        Self::with_config(SecurityLoggerConfig::default())
    }

    /// Create a security logger with custom configuration
    pub fn with_config(config: SecurityLoggerConfig) -> Result<Self, LoggingError> {
        let audit_trail = if config.enable_audit {
            Some(if let Some(ref path) = config.audit_repo_path {
                AuditTrail::with_repository(path.clone())?
            } else {
                AuditTrail::new()
            })
        } else {
            None
        };

        let intrusion_detector = if config.enable_intrusion_detection {
            Some(IntrusionDetector::new()?)
        } else {
            None
        };

        let metrics = if config.enable_metrics {
            Some(MetricsCollector::new())
        } else {
            None
        };

        let alert_manager = if config.enable_alerting {
            let mut manager = AlertManager::new();

            // Register console handler
            manager.register_handler(Arc::new(ConsoleAlertHandler));

            // Register file handler if path provided
            if let Some(ref path) = config.alert_log_path {
                manager.register_handler(Arc::new(FileAlertHandler::new(
                    path.to_string_lossy().to_string(),
                )));
            }

            Some(manager)
        } else {
            None
        };

        Ok(Self {
            config,
            audit_trail,
            intrusion_detector,
            metrics,
            alert_manager,
        })
    }

    /// Create a security logger with repository-based audit trail
    pub fn with_repository(repo_path: PathBuf) -> Result<Self, LoggingError> {
        let config = SecurityLoggerConfig {
            enable_audit: true,
            audit_repo_path: Some(repo_path),
            ..Default::default()
        };
        Self::with_config(config)
    }

    /// Log a security event
    pub fn log(&mut self, event: SecurityEvent) -> Result<(), LoggingError> {
        let event_id = event.id.clone();
        let severity = event.severity;
        let category = event.category.clone();

        // Log to tracing
        match severity {
            super::events::SecuritySeverity::Critical | super::events::SecuritySeverity::High => {
                error!(
                    event_id = %event_id,
                    severity = ?severity,
                    category = ?category,
                    message = %event.message,
                    "Security event"
                );
            }
            super::events::SecuritySeverity::Medium => {
                warn!(
                    event_id = %event_id,
                    severity = ?severity,
                    category = ?category,
                    message = %event.message,
                    "Security event"
                );
            }
            _ => {
                info!(
                    event_id = %event_id,
                    severity = ?severity,
                    category = ?category,
                    message = %event.message,
                    "Security event"
                );
            }
        }

        // Append to audit trail
        if let Some(ref mut audit) = self.audit_trail {
            audit.append(event.clone())?;
        }

        // Collect metrics
        if let Some(ref mut metrics) = self.metrics {
            metrics.record(event.clone());
        }

        // Send alert if required
        if let Some(ref mut alerter) = self.alert_manager {
            alerter.send_from_event(&event)?;
        }

        Ok(())
    }

    /// Analyze input for attack patterns
    pub fn analyze_input(&mut self, input: &str) -> Result<Option<SecurityEvent>, LoggingError> {
        if let Some(ref detector) = self.intrusion_detector {
            if let Some(event) = detector.analyze_input(input) {
                self.log(event.clone())?;
                return Ok(Some(event));
            }
        }
        Ok(None)
    }

    /// Check authentication rate limit
    pub fn check_auth_rate(&mut self, source: &str) -> Result<(), LoggingError> {
        if let Some(ref mut detector) = self.intrusion_detector {
            if let Some(event) = detector.check_auth_rate(source) {
                self.log(event)?;
            }
        }
        Ok(())
    }

    /// Check request rate limit
    pub fn check_request_rate(&mut self, source: &str) -> Result<(), LoggingError> {
        if let Some(ref mut detector) = self.intrusion_detector {
            if let Some(event) = detector.check_request_rate(source) {
                self.log(event)?;
            }
        }
        Ok(())
    }

    /// Get metrics for a time window
    pub fn get_metrics(&self, window: TimeWindow) -> Option<SecurityMetrics> {
        self.metrics.as_ref().map(|m| m.get_metrics(window))
    }

    /// Get metrics for the last hour
    pub fn get_metrics_for_last_hour(&self) -> Option<SecurityMetrics> {
        self.get_metrics(TimeWindow::Hour)
    }

    /// Get metrics for the last day
    pub fn get_metrics_for_last_day(&self) -> Option<SecurityMetrics> {
        self.get_metrics(TimeWindow::Day)
    }

    /// Verify audit trail integrity
    pub fn verify_audit_trail(&self) -> Result<bool, LoggingError> {
        if let Some(ref audit) = self.audit_trail {
            Ok(audit.verify_chain()?)
        } else {
            Ok(true) // No audit trail to verify
        }
    }

    /// Export audit trail to JSON
    pub fn export_audit_trail(&self) -> Result<String, LoggingError> {
        if let Some(ref audit) = self.audit_trail {
            Ok(audit.export_json()?)
        } else {
            Ok("[]".to_string())
        }
    }

    /// Export metrics to JSON
    pub fn export_metrics(&self, window: TimeWindow) -> Result<String, LoggingError> {
        if let Some(ref metrics) = self.metrics {
            Ok(metrics.export_json(window)?)
        } else {
            Ok("{}".to_string())
        }
    }

    /// Get audit entry count
    pub fn audit_entry_count(&self) -> usize {
        self.audit_trail.as_ref().map(|a| a.len()).unwrap_or(0)
    }

    /// Get total event count
    pub fn total_events(&self) -> usize {
        self.metrics.as_ref().map(|m| m.total_events()).unwrap_or(0)
    }
}

impl Default for SecurityLogger {
    fn default() -> Self {
        Self::new().expect("Default security logger should always initialize")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::security::events::{EventCategory, SecuritySeverity};
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_security_logger_creation() {
        // Arrange & Act
        let result = SecurityLogger::new();

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_log_event() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Test event",
        );

        // Act
        let result = logger.log(event);

        // Assert
        assert!(result.is_ok());
        assert_eq!(logger.audit_entry_count(), 1);
        assert_eq!(logger.total_events(), 1);
    }

    #[test]
    fn test_analyze_input_attack_detected() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        // Act
        let result = logger.analyze_input("SELECT * FROM users WHERE id = 1");

        // Assert
        assert!(result.is_ok());
        let event = result.unwrap();
        assert!(event.is_some());
        assert_eq!(event.unwrap().attack_pattern, AttackPattern::SqlInjection);
    }

    #[test]
    fn test_analyze_input_no_attack() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        // Act
        let result = logger.analyze_input("normal user input");

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }

    #[test]
    fn test_check_auth_rate() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        // Act - make multiple auth attempts
        for _ in 0..6 {
            let result = logger.check_auth_rate("192.168.1.1");
            assert!(result.is_ok());
        }

        // Assert - should have logged rate limit event
        assert!(logger.total_events() > 0);
    }

    #[test]
    fn test_get_metrics() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        logger
            .log(SecurityEvent::new(
                SecuritySeverity::High,
                EventCategory::Authentication,
                "Event 1",
            ))
            .unwrap();
        logger
            .log(SecurityEvent::new(
                SecuritySeverity::Medium,
                EventCategory::Authorization,
                "Event 2",
            ))
            .unwrap();

        // Act
        let metrics = logger.get_metrics(TimeWindow::AllTime);

        // Assert
        assert!(metrics.is_some());
        let m = metrics.unwrap();
        assert_eq!(m.total_events, 2);
    }

    #[test]
    fn test_verify_audit_trail() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        logger
            .log(SecurityEvent::new(
                SecuritySeverity::Info,
                EventCategory::DataAccess,
                "Access event",
            ))
            .unwrap();

        // Act
        let result = logger.verify_audit_trail();

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap());
    }

    #[test]
    fn test_export_audit_trail() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        logger
            .log(SecurityEvent::new(
                SecuritySeverity::Critical,
                EventCategory::Integrity,
                "Breach detected",
            ))
            .unwrap();

        // Act
        let result = logger.export_audit_trail();

        // Assert
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("Breach detected"));
    }

    #[test]
    fn test_export_metrics() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();

        logger
            .log(SecurityEvent::new(
                SecuritySeverity::High,
                EventCategory::Authentication,
                "Login failed",
            ))
            .unwrap();

        // Act
        let result = logger.export_metrics(TimeWindow::AllTime);

        // Assert
        assert!(result.is_ok());
        let json = result.unwrap();
        assert!(json.contains("total_events"));
    }

    #[test]
    fn test_disabled_features() {
        // Arrange
        let config = SecurityLoggerConfig {
            enable_audit: false,
            enable_intrusion_detection: false,
            enable_metrics: false,
            enable_alerting: false,
            ..Default::default()
        };

        // Act
        let logger = SecurityLogger::with_config(config).unwrap();

        // Assert
        assert!(logger.audit_trail.is_none());
        assert!(logger.intrusion_detector.is_none());
        assert!(logger.metrics.is_none());
        assert!(logger.alert_manager.is_none());
    }

    #[test]
    fn test_with_repository() {
        // Arrange
        let temp_dir = tempfile::tempdir().unwrap();
        let repo_path = temp_dir.path().to_path_buf();

        // Act
        let result = SecurityLogger::with_repository(repo_path);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiple_events_logging() {
        // Arrange
        let mut logger = SecurityLogger::new().unwrap();
        let ip = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));

        // Act
        logger
            .log(SecurityEvent::authentication_failed("user1", ip))
            .unwrap();
        logger
            .log(SecurityEvent::authorization_failed("user2", "/admin", "read"))
            .unwrap();
        logger
            .log(SecurityEvent::input_validation_failed(
                "' OR '1'='1",
                AttackPattern::SqlInjection,
            ))
            .unwrap();

        // Assert
        assert_eq!(logger.audit_entry_count(), 3);
        assert_eq!(logger.total_events(), 3);

        let metrics = logger.get_metrics(TimeWindow::AllTime).unwrap();
        assert_eq!(metrics.total_events, 3);
        assert_eq!(metrics.total_attacks, 1);
        assert_eq!(metrics.failed_auth_count, 1);
        assert_eq!(metrics.failed_authz_count, 1);
    }

    #[test]
    fn test_logger_with_custom_config() {
        // Arrange
        let temp_dir = tempfile::tempdir().unwrap();
        let config = SecurityLoggerConfig {
            enable_audit: true,
            audit_repo_path: Some(temp_dir.path().to_path_buf()),
            enable_intrusion_detection: true,
            enable_metrics: true,
            enable_alerting: true,
            alert_log_path: Some(temp_dir.path().join("alerts.log")),
        };

        // Act
        let logger = SecurityLogger::with_config(config);

        // Assert
        assert!(logger.is_ok());
    }
}
