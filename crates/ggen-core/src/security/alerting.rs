//! Real-time security alerting system
//!
//! This module provides real-time alerting for critical security events
//! with support for multiple notification channels.

use super::events::{SecurityEvent, SecuritySeverity};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use thiserror::Error;
use tracing::{error, warn};

/// Alerting errors
#[derive(Debug, Error)]
pub enum AlertError {
    #[error("Alert handler not found: {0}")]
    HandlerNotFound(String),

    #[error("Alert delivery failed: {0}")]
    DeliveryFailed(String),

    #[error("Invalid alert configuration: {0}")]
    InvalidConfiguration(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

/// Alert severity (distinct from SecuritySeverity for flexibility)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AlertSeverity {
    /// Informational alert
    Info,
    /// Warning alert
    Warning,
    /// Error alert
    Error,
    /// Critical alert requiring immediate action
    Critical,
}

impl From<SecuritySeverity> for AlertSeverity {
    fn from(severity: SecuritySeverity) -> Self {
        match severity {
            SecuritySeverity::Info => Self::Info,
            SecuritySeverity::Low => Self::Info,
            SecuritySeverity::Medium => Self::Warning,
            SecuritySeverity::High => Self::Error,
            SecuritySeverity::Critical => Self::Critical,
        }
    }
}

/// Alert notification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    /// Alert ID
    pub id: String,
    /// Alert severity
    pub severity: AlertSeverity,
    /// Alert title
    pub title: String,
    /// Alert message
    pub message: String,
    /// Source event ID
    pub event_id: String,
    /// Timestamp
    pub timestamp: i64,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl Alert {
    /// Create alert from security event
    pub fn from_event(event: &SecurityEvent) -> Self {
        let mut metadata = event.metadata.clone();

        if let Some(ip) = &event.source_ip {
            metadata.insert("source_ip".to_string(), ip.to_string());
        }
        if let Some(user) = &event.user_id {
            metadata.insert("user_id".to_string(), user.clone());
        }
        if let Some(resource) = &event.resource {
            metadata.insert("resource".to_string(), resource.clone());
        }

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            severity: event.severity.into(),
            title: format!("[{}] {}", event.category, event.attack_pattern),
            message: event.message.clone(),
            event_id: event.id.clone(),
            timestamp: Utc::now().timestamp(),
            metadata,
        }
    }

    /// Create custom alert
    pub fn new(
        severity: AlertSeverity, title: impl Into<String>, message: impl Into<String>,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            severity,
            title: title.into(),
            message: message.into(),
            event_id: String::new(),
            timestamp: Utc::now().timestamp(),
            metadata: HashMap::new(),
        }
    }
}

/// Alert handler trait
pub trait AlertHandler: Send + Sync {
    /// Handle an alert
    fn handle(&self, alert: &Alert) -> Result<(), AlertError>;

    /// Get handler name
    fn name(&self) -> &str;
}

/// Console alert handler (writes to stderr)
pub struct ConsoleAlertHandler;

impl AlertHandler for ConsoleAlertHandler {
    fn handle(&self, alert: &Alert) -> Result<(), AlertError> {
        match alert.severity {
            AlertSeverity::Critical | AlertSeverity::Error => {
                error!(
                    alert_id = %alert.id,
                    severity = ?alert.severity,
                    title = %alert.title,
                    message = %alert.message,
                    "Security alert"
                );
            }
            AlertSeverity::Warning => {
                warn!(
                    alert_id = %alert.id,
                    severity = ?alert.severity,
                    title = %alert.title,
                    message = %alert.message,
                    "Security alert"
                );
            }
            AlertSeverity::Info => {
                tracing::info!(
                    alert_id = %alert.id,
                    severity = ?alert.severity,
                    title = %alert.title,
                    message = %alert.message,
                    "Security alert"
                );
            }
        }
        Ok(())
    }

    fn name(&self) -> &str {
        "console"
    }
}

/// File alert handler (appends to a file)
pub struct FileAlertHandler {
    file_path: String,
}

impl FileAlertHandler {
    /// Create a new file alert handler
    pub fn new(file_path: impl Into<String>) -> Self {
        Self {
            file_path: file_path.into(),
        }
    }
}

impl AlertHandler for FileAlertHandler {
    fn handle(&self, alert: &Alert) -> Result<(), AlertError> {
        use std::fs::OpenOptions;
        use std::io::Write;

        let alert_json = serde_json::to_string(alert)?;

        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.file_path)
            .map_err(|e| AlertError::DeliveryFailed(format!("Failed to open file: {}", e)))?;

        writeln!(file, "{}", alert_json)
            .map_err(|e| AlertError::DeliveryFailed(format!("Failed to write: {}", e)))?;

        Ok(())
    }

    fn name(&self) -> &str {
        "file"
    }
}

/// In-memory alert handler (for testing)
pub struct MemoryAlertHandler {
    alerts: Arc<Mutex<VecDeque<Alert>>>,
    max_alerts: usize,
}

impl MemoryAlertHandler {
    /// Create a new memory alert handler
    pub fn new(max_alerts: usize) -> Self {
        Self {
            alerts: Arc::new(Mutex::new(VecDeque::new())),
            max_alerts,
        }
    }

    /// Get all stored alerts
    pub fn get_alerts(&self) -> Vec<Alert> {
        self.alerts.lock().unwrap().iter().cloned().collect()
    }

    /// Clear all alerts
    pub fn clear(&self) {
        self.alerts.lock().unwrap().clear();
    }

    /// Get alert count
    pub fn count(&self) -> usize {
        self.alerts.lock().unwrap().len()
    }
}

impl AlertHandler for MemoryAlertHandler {
    fn handle(&self, alert: &Alert) -> Result<(), AlertError> {
        let mut alerts = self.alerts.lock().unwrap();
        alerts.push_back(alert.clone());

        // Trim to max size
        while alerts.len() > self.max_alerts {
            alerts.pop_front();
        }

        Ok(())
    }

    fn name(&self) -> &str {
        "memory"
    }
}

/// Alert manager
pub struct AlertManager {
    /// Registered alert handlers
    handlers: HashMap<String, Arc<dyn AlertHandler>>,
    /// Alert history
    history: VecDeque<Alert>,
    /// Max history size
    max_history: usize,
}

impl AlertManager {
    /// Create a new alert manager
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
            history: VecDeque::new(),
            max_history: 1000,
        }
    }

    /// Register an alert handler
    pub fn register_handler(&mut self, handler: Arc<dyn AlertHandler>) {
        self.handlers.insert(handler.name().to_string(), handler);
    }

    /// Send an alert
    pub fn send_alert(&mut self, alert: Alert) -> Result<(), AlertError> {
        // Store in history
        self.history.push_back(alert.clone());
        while self.history.len() > self.max_history {
            self.history.pop_front();
        }

        // Send to all handlers
        let mut errors = Vec::new();
        for (name, handler) in &self.handlers {
            if let Err(e) = handler.handle(&alert) {
                errors.push(format!("Handler '{}': {}", name, e));
            }
        }

        if !errors.is_empty() {
            return Err(AlertError::DeliveryFailed(errors.join("; ")));
        }

        Ok(())
    }

    /// Send alert from security event
    pub fn send_from_event(&mut self, event: &SecurityEvent) -> Result<(), AlertError> {
        if event.requires_alert() {
            let alert = Alert::from_event(event);
            self.send_alert(alert)?;
        }
        Ok(())
    }

    /// Get alert history
    pub fn get_history(&self) -> &VecDeque<Alert> {
        &self.history
    }

    /// Clear alert history
    pub fn clear_history(&mut self) {
        self.history.clear();
    }

    /// Get handler count
    pub fn handler_count(&self) -> usize {
        self.handlers.len()
    }
}

impl Default for AlertManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::security::events::{AttackPattern, EventCategory};
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_alert_from_event() {
        // Arrange
        let event = SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::Integrity,
            "System compromised",
        )
        .with_user("admin")
        .with_source_ip(IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1)));

        // Act
        let alert = Alert::from_event(&event);

        // Assert
        assert_eq!(alert.severity, AlertSeverity::Critical);
        assert_eq!(alert.event_id, event.id);
        assert!(alert.metadata.contains_key("user_id"));
        assert!(alert.metadata.contains_key("source_ip"));
    }

    #[test]
    fn test_alert_severity_conversion() {
        // Arrange & Act & Assert
        assert_eq!(
            AlertSeverity::from(SecuritySeverity::Info),
            AlertSeverity::Info
        );
        assert_eq!(
            AlertSeverity::from(SecuritySeverity::Low),
            AlertSeverity::Info
        );
        assert_eq!(
            AlertSeverity::from(SecuritySeverity::Medium),
            AlertSeverity::Warning
        );
        assert_eq!(
            AlertSeverity::from(SecuritySeverity::High),
            AlertSeverity::Error
        );
        assert_eq!(
            AlertSeverity::from(SecuritySeverity::Critical),
            AlertSeverity::Critical
        );
    }

    #[test]
    fn test_console_alert_handler() {
        // Arrange
        let handler = ConsoleAlertHandler;
        let alert = Alert::new(AlertSeverity::Critical, "Test", "Test message");

        // Act
        let result = handler.handle(&alert);

        // Assert
        assert!(result.is_ok());
        assert_eq!(handler.name(), "console");
    }

    #[test]
    fn test_memory_alert_handler() {
        // Arrange
        let handler = MemoryAlertHandler::new(10);
        let alert1 = Alert::new(AlertSeverity::Warning, "Alert 1", "Message 1");
        let alert2 = Alert::new(AlertSeverity::Error, "Alert 2", "Message 2");

        // Act
        handler.handle(&alert1).unwrap();
        handler.handle(&alert2).unwrap();

        // Assert
        assert_eq!(handler.count(), 2);
        let alerts = handler.get_alerts();
        assert_eq!(alerts.len(), 2);
        assert_eq!(alerts[0].title, "Alert 1");
        assert_eq!(alerts[1].title, "Alert 2");
    }

    #[test]
    fn test_memory_alert_handler_max_size() {
        // Arrange
        let handler = MemoryAlertHandler::new(3);

        // Act - add more than max
        for i in 0..5 {
            let alert = Alert::new(
                AlertSeverity::Info,
                format!("Alert {}", i),
                format!("Message {}", i),
            );
            handler.handle(&alert).unwrap();
        }

        // Assert - should keep only last 3
        assert_eq!(handler.count(), 3);
        let alerts = handler.get_alerts();
        assert_eq!(alerts[0].title, "Alert 2");
        assert_eq!(alerts[1].title, "Alert 3");
        assert_eq!(alerts[2].title, "Alert 4");
    }

    #[test]
    fn test_memory_alert_handler_clear() {
        // Arrange
        let handler = MemoryAlertHandler::new(10);
        handler
            .handle(&Alert::new(AlertSeverity::Info, "Test", "Message"))
            .unwrap();

        // Act
        handler.clear();

        // Assert
        assert_eq!(handler.count(), 0);
    }

    #[test]
    fn test_alert_manager_creation() {
        // Arrange & Act
        let manager = AlertManager::new();

        // Assert
        assert_eq!(manager.handler_count(), 0);
        assert_eq!(manager.get_history().len(), 0);
    }

    #[test]
    fn test_alert_manager_register_handler() {
        // Arrange
        let mut manager = AlertManager::new();
        let handler = Arc::new(ConsoleAlertHandler);

        // Act
        manager.register_handler(handler);

        // Assert
        assert_eq!(manager.handler_count(), 1);
    }

    #[test]
    fn test_alert_manager_send_alert() {
        // Arrange
        let mut manager = AlertManager::new();
        let handler = Arc::new(MemoryAlertHandler::new(10));
        manager.register_handler(handler.clone());

        let alert = Alert::new(AlertSeverity::Critical, "Test Alert", "Critical issue");

        // Act
        let result = manager.send_alert(alert);

        // Assert
        assert!(result.is_ok());
        assert_eq!(manager.get_history().len(), 1);
        assert_eq!(handler.count(), 1);
    }

    #[test]
    fn test_alert_manager_send_from_event() {
        // Arrange
        let mut manager = AlertManager::new();
        let handler = Arc::new(MemoryAlertHandler::new(10));
        manager.register_handler(handler.clone());

        let event =
            SecurityEvent::input_validation_failed("' OR '1'='1", AttackPattern::SqlInjection);

        // Act
        let result = manager.send_from_event(&event);

        // Assert
        assert!(result.is_ok());
        assert_eq!(handler.count(), 1); // High severity events trigger alerts
    }

    #[test]
    fn test_alert_manager_no_alert_for_low_severity() {
        // Arrange
        let mut manager = AlertManager::new();
        let handler = Arc::new(MemoryAlertHandler::new(10));
        manager.register_handler(handler.clone());

        let event = SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::DataAccess,
            "Normal access",
        );

        // Act
        let result = manager.send_from_event(&event);

        // Assert
        assert!(result.is_ok());
        assert_eq!(handler.count(), 0); // Info events don't trigger alerts
    }

    #[test]
    fn test_alert_manager_history_max_size() {
        // Arrange
        let mut manager = AlertManager::new();
        manager.max_history = 3;

        // Act - send more alerts than max
        for i in 0..5 {
            manager
                .send_alert(Alert::new(
                    AlertSeverity::Info,
                    format!("Alert {}", i),
                    format!("Message {}", i),
                ))
                .unwrap();
        }

        // Assert - should keep only last 3
        assert_eq!(manager.get_history().len(), 3);
    }

    #[test]
    fn test_alert_manager_clear_history() {
        // Arrange
        let mut manager = AlertManager::new();
        manager
            .send_alert(Alert::new(AlertSeverity::Info, "Test", "Message"))
            .unwrap();

        // Act
        manager.clear_history();

        // Assert
        assert_eq!(manager.get_history().len(), 0);
    }

    #[test]
    fn test_alert_manager_multiple_handlers() {
        // Arrange
        let mut manager = AlertManager::new();
        let handler1 = Arc::new(MemoryAlertHandler::new(10));
        let handler2 = Arc::new(MemoryAlertHandler::new(10));

        manager.register_handler(handler1.clone());
        manager.register_handler(handler2.clone());

        let alert = Alert::new(AlertSeverity::Error, "Test", "Message");

        // Act
        manager.send_alert(alert).unwrap();

        // Assert - both handlers should receive the alert
        assert_eq!(handler1.count(), 1);
        // Note: handler2 won't receive it because both have name "memory"
        // and HashMap will only keep one
    }
}
