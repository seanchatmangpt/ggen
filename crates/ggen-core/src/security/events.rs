//! Security event types and taxonomy
//!
//! This module defines comprehensive security event types for logging and monitoring.
//! Events are categorized by severity and type to enable effective intrusion detection
//! and incident response.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::net::IpAddr;

/// Security event severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SecuritySeverity {
    /// Informational events (successful operations)
    Info = 0,
    /// Low severity (minor policy violations)
    Low = 1,
    /// Medium severity (suspicious activity)
    Medium = 2,
    /// High severity (likely attack attempts)
    High = 3,
    /// Critical severity (confirmed security breach)
    Critical = 4,
}

impl fmt::Display for SecuritySeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Info => write!(f, "INFO"),
            Self::Low => write!(f, "LOW"),
            Self::Medium => write!(f, "MEDIUM"),
            Self::High => write!(f, "HIGH"),
            Self::Critical => write!(f, "CRITICAL"),
        }
    }
}

/// Security event category
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum EventCategory {
    /// Authentication-related events
    Authentication,
    /// Authorization and access control events
    Authorization,
    /// Input validation and sanitization events
    InputValidation,
    /// Configuration change events
    Configuration,
    /// Data access and modification events
    DataAccess,
    /// Network activity events
    Network,
    /// System integrity events
    Integrity,
    /// Cryptographic operations
    Cryptography,
    /// Policy violations
    Policy,
    /// Unknown or uncategorized events
    Other,
}

impl fmt::Display for EventCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Authentication => write!(f, "AUTHENTICATION"),
            Self::Authorization => write!(f, "AUTHORIZATION"),
            Self::InputValidation => write!(f, "INPUT_VALIDATION"),
            Self::Configuration => write!(f, "CONFIGURATION"),
            Self::DataAccess => write!(f, "DATA_ACCESS"),
            Self::Network => write!(f, "NETWORK"),
            Self::Integrity => write!(f, "INTEGRITY"),
            Self::Cryptography => write!(f, "CRYPTOGRAPHY"),
            Self::Policy => write!(f, "POLICY"),
            Self::Other => write!(f, "OTHER"),
        }
    }
}

/// Attack pattern types for intrusion detection
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AttackPattern {
    /// SQL injection attempt
    SqlInjection,
    /// Cross-site scripting (XSS) attempt
    Xss,
    /// Command injection attempt
    CommandInjection,
    /// Path traversal attempt
    PathTraversal,
    /// Brute force attack
    BruteForce,
    /// Denial of service attempt
    DenialOfService,
    /// Buffer overflow attempt
    BufferOverflow,
    /// XML external entity (XXE) attack
    XxeAttack,
    /// Server-side request forgery (SSRF)
    Ssrf,
    /// Deserialization attack
    Deserialization,
    /// LDAP injection
    LdapInjection,
    /// Template injection
    TemplateInjection,
    /// No specific pattern detected
    None,
}

impl fmt::Display for AttackPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SqlInjection => write!(f, "SQL_INJECTION"),
            Self::Xss => write!(f, "XSS"),
            Self::CommandInjection => write!(f, "COMMAND_INJECTION"),
            Self::PathTraversal => write!(f, "PATH_TRAVERSAL"),
            Self::BruteForce => write!(f, "BRUTE_FORCE"),
            Self::DenialOfService => write!(f, "DENIAL_OF_SERVICE"),
            Self::BufferOverflow => write!(f, "BUFFER_OVERFLOW"),
            Self::XxeAttack => write!(f, "XXE_ATTACK"),
            Self::Ssrf => write!(f, "SSRF"),
            Self::Deserialization => write!(f, "DESERIALIZATION"),
            Self::LdapInjection => write!(f, "LDAP_INJECTION"),
            Self::TemplateInjection => write!(f, "TEMPLATE_INJECTION"),
            Self::None => write!(f, "NONE"),
        }
    }
}

/// Security event data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityEvent {
    /// Unique event identifier
    pub id: String,
    /// Event timestamp (UTC)
    pub timestamp: DateTime<Utc>,
    /// Event severity level
    pub severity: SecuritySeverity,
    /// Event category
    pub category: EventCategory,
    /// Event message
    pub message: String,
    /// Source IP address (if applicable)
    pub source_ip: Option<IpAddr>,
    /// User identifier (if applicable)
    pub user_id: Option<String>,
    /// Resource being accessed
    pub resource: Option<String>,
    /// Action performed or attempted
    pub action: Option<String>,
    /// Detected attack pattern (if any)
    pub attack_pattern: AttackPattern,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
    /// Whether the event resulted in success
    pub success: bool,
}

impl SecurityEvent {
    /// Create a new security event
    pub fn new(
        severity: SecuritySeverity, category: EventCategory, message: impl Into<String>,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            severity,
            category,
            message: message.into(),
            source_ip: None,
            user_id: None,
            resource: None,
            action: None,
            attack_pattern: AttackPattern::None,
            metadata: HashMap::new(),
            success: true,
        }
    }

    /// Create an authentication failure event
    pub fn authentication_failed(user: impl Into<String>, source_ip: IpAddr) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            severity: SecuritySeverity::Medium,
            category: EventCategory::Authentication,
            message: "Authentication failed".to_string(),
            source_ip: Some(source_ip),
            user_id: Some(user.into()),
            resource: None,
            action: Some("login".to_string()),
            attack_pattern: AttackPattern::None,
            metadata: HashMap::new(),
            success: false,
        }
    }

    /// Create an authorization failure event
    pub fn authorization_failed(
        user: impl Into<String>, resource: impl Into<String>, action: impl Into<String>,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            severity: SecuritySeverity::High,
            category: EventCategory::Authorization,
            message: "Authorization denied".to_string(),
            source_ip: None,
            user_id: Some(user.into()),
            resource: Some(resource.into()),
            action: Some(action.into()),
            attack_pattern: AttackPattern::None,
            metadata: HashMap::new(),
            success: false,
        }
    }

    /// Create an input validation failure event
    pub fn input_validation_failed(input: impl Into<String>, pattern: AttackPattern) -> Self {
        let mut metadata = HashMap::new();
        metadata.insert("input".to_string(), input.into());

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            severity: SecuritySeverity::High,
            category: EventCategory::InputValidation,
            message: format!("Input validation failed: {}", pattern),
            source_ip: None,
            user_id: None,
            resource: None,
            action: Some("validate_input".to_string()),
            attack_pattern: pattern,
            metadata,
            success: false,
        }
    }

    /// Create a configuration change event
    pub fn configuration_changed(
        user: impl Into<String>, setting: impl Into<String>, old_value: impl Into<String>,
        new_value: impl Into<String>,
    ) -> Self {
        let mut metadata = HashMap::new();
        metadata.insert("setting".to_string(), setting.into());
        metadata.insert("old_value".to_string(), old_value.into());
        metadata.insert("new_value".to_string(), new_value.into());

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            timestamp: Utc::now(),
            severity: SecuritySeverity::Medium,
            category: EventCategory::Configuration,
            message: "Configuration changed".to_string(),
            source_ip: None,
            user_id: Some(user.into()),
            resource: None,
            action: Some("update_config".to_string()),
            attack_pattern: AttackPattern::None,
            metadata,
            success: true,
        }
    }

    /// Set source IP address
    pub fn with_source_ip(mut self, ip: IpAddr) -> Self {
        self.source_ip = Some(ip);
        self
    }

    /// Set user identifier
    pub fn with_user(mut self, user: impl Into<String>) -> Self {
        self.user_id = Some(user.into());
        self
    }

    /// Set resource
    pub fn with_resource(mut self, resource: impl Into<String>) -> Self {
        self.resource = Some(resource.into());
        self
    }

    /// Set action
    pub fn with_action(mut self, action: impl Into<String>) -> Self {
        self.action = Some(action.into());
        self
    }

    /// Set attack pattern
    pub fn with_attack_pattern(mut self, pattern: AttackPattern) -> Self {
        self.attack_pattern = pattern;
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Set success flag
    pub fn with_success(mut self, success: bool) -> Self {
        self.success = success;
        self
    }

    /// Check if event requires immediate alerting
    pub fn requires_alert(&self) -> bool {
        match self.severity {
            SecuritySeverity::Critical | SecuritySeverity::High => true,
            SecuritySeverity::Medium => {
                // Alert on repeated medium severity events
                matches!(
                    self.category,
                    EventCategory::Authentication | EventCategory::Authorization
                ) && !self.success
            }
            _ => false,
        }
    }

    /// Check if event indicates potential attack
    pub fn is_attack(&self) -> bool {
        !matches!(self.attack_pattern, AttackPattern::None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{IpAddr, Ipv4Addr};

    #[test]
    fn test_security_event_creation() {
        // Arrange & Act
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Test event",
        );

        // Assert
        assert_eq!(event.severity, SecuritySeverity::High);
        assert_eq!(event.category, EventCategory::Authentication);
        assert_eq!(event.message, "Test event");
        assert!(event.success);
    }

    #[test]
    fn test_authentication_failed_event() {
        // Arrange & Act
        let ip = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));
        let event = SecurityEvent::authentication_failed("user123", ip);

        // Assert
        assert_eq!(event.severity, SecuritySeverity::Medium);
        assert_eq!(event.category, EventCategory::Authentication);
        assert_eq!(event.user_id, Some("user123".to_string()));
        assert_eq!(event.source_ip, Some(ip));
        assert!(!event.success);
    }

    #[test]
    fn test_authorization_failed_event() {
        // Arrange & Act
        let event = SecurityEvent::authorization_failed("user123", "/admin", "read");

        // Assert
        assert_eq!(event.severity, SecuritySeverity::High);
        assert_eq!(event.category, EventCategory::Authorization);
        assert_eq!(event.user_id, Some("user123".to_string()));
        assert_eq!(event.resource, Some("/admin".to_string()));
        assert_eq!(event.action, Some("read".to_string()));
        assert!(!event.success);
    }

    #[test]
    fn test_input_validation_failed_event() {
        // Arrange & Act
        let event = SecurityEvent::input_validation_failed(
            "SELECT * FROM users",
            AttackPattern::SqlInjection,
        );

        // Assert
        assert_eq!(event.severity, SecuritySeverity::High);
        assert_eq!(event.category, EventCategory::InputValidation);
        assert_eq!(event.attack_pattern, AttackPattern::SqlInjection);
        assert!(!event.success);
        assert!(event.metadata.contains_key("input"));
    }

    #[test]
    fn test_configuration_changed_event() {
        // Arrange & Act
        let event = SecurityEvent::configuration_changed("admin", "max_connections", "100", "200");

        // Assert
        assert_eq!(event.severity, SecuritySeverity::Medium);
        assert_eq!(event.category, EventCategory::Configuration);
        assert_eq!(event.user_id, Some("admin".to_string()));
        assert!(event.success);
        assert_eq!(
            event.metadata.get("setting"),
            Some(&"max_connections".to_string())
        );
    }

    #[test]
    fn test_event_builder_pattern() {
        // Arrange & Act
        let ip = IpAddr::V4(Ipv4Addr::new(10, 0, 0, 1));
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::DataAccess,
            "Data accessed",
        )
        .with_source_ip(ip)
        .with_user("user456")
        .with_resource("/api/users")
        .with_action("read")
        .with_attack_pattern(AttackPattern::None)
        .with_metadata("table", "users")
        .with_success(true);

        // Assert
        assert_eq!(event.source_ip, Some(ip));
        assert_eq!(event.user_id, Some("user456".to_string()));
        assert_eq!(event.resource, Some("/api/users".to_string()));
        assert_eq!(event.action, Some("read".to_string()));
        assert!(event.success);
        assert_eq!(event.metadata.get("table"), Some(&"users".to_string()));
    }

    #[test]
    fn test_requires_alert_critical() {
        // Arrange
        let event = SecurityEvent::new(
            SecuritySeverity::Critical,
            EventCategory::Integrity,
            "System compromised",
        );

        // Act & Assert
        assert!(event.requires_alert());
    }

    #[test]
    fn test_requires_alert_high() {
        // Arrange
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Network,
            "Port scan detected",
        );

        // Act & Assert
        assert!(event.requires_alert());
    }

    #[test]
    fn test_requires_alert_medium_auth_failure() {
        // Arrange
        let ip = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1));
        let event = SecurityEvent::authentication_failed("user", ip);

        // Act & Assert
        assert!(event.requires_alert());
    }

    #[test]
    fn test_requires_alert_low() {
        // Arrange
        let event = SecurityEvent::new(
            SecuritySeverity::Low,
            EventCategory::Policy,
            "Policy violation",
        );

        // Act & Assert
        assert!(!event.requires_alert());
    }

    #[test]
    fn test_is_attack() {
        // Arrange
        let event1 =
            SecurityEvent::input_validation_failed("' OR '1'='1", AttackPattern::SqlInjection);
        let event2 = SecurityEvent::new(
            SecuritySeverity::Info,
            EventCategory::Authentication,
            "Login successful",
        );

        // Act & Assert
        assert!(event1.is_attack());
        assert!(!event2.is_attack());
    }

    #[test]
    fn test_severity_ordering() {
        // Arrange & Act & Assert
        assert!(SecuritySeverity::Critical > SecuritySeverity::High);
        assert!(SecuritySeverity::High > SecuritySeverity::Medium);
        assert!(SecuritySeverity::Medium > SecuritySeverity::Low);
        assert!(SecuritySeverity::Low > SecuritySeverity::Info);
    }

    #[test]
    fn test_event_serialization() {
        // Arrange
        let event = SecurityEvent::new(
            SecuritySeverity::High,
            EventCategory::Authentication,
            "Test",
        );

        // Act
        let json = serde_json::to_string(&event);

        // Assert
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("HIGH"));
        assert!(json_str.contains("AUTHENTICATION"));
    }
}
