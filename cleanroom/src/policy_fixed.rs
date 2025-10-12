//! Fixed policy enforcement for cleanroom testing
//!
//! This module provides policy enforcement following core team best practices:
//! - Security boundaries and isolation
//! - Resource limits and constraints
//! - Execution policies and rules
//! - Compliance and audit trails

use crate::error::{Result, CleanroomError};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Policy configuration for cleanroom testing
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Policy {
    /// Security policy
    pub security: SecurityPolicy,
    /// Resource policy
    pub resources: ResourcePolicy,
    /// Execution policy
    pub execution: ExecutionPolicy,
    /// Compliance policy
    pub compliance: CompliancePolicy,
    /// Network policy
    pub network: NetworkPolicy,
}

/// Security policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityPolicy {
    /// Enable network isolation
    pub enable_network_isolation: bool,
    /// Enable filesystem isolation
    pub enable_filesystem_isolation: bool,
    /// Enable process isolation
    pub enable_process_isolation: bool,
    /// Allowed network ports
    pub allowed_ports: Vec<u16>,
    /// Blocked network addresses
    pub blocked_addresses: Vec<String>,
    /// Enable sensitive data redaction
    pub enable_data_redaction: bool,
    /// Redaction patterns
    pub redaction_patterns: Vec<String>,
    /// Enable audit logging
    pub enable_audit_logging: bool,
    /// Security level
    pub security_level: SecurityLevel,
}

/// Network policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkPolicy {
    /// Enable network isolation
    pub enable_network_isolation: bool,
    /// Enable port scanning
    pub enable_port_scanning: bool,
    /// Enable file system isolation
    pub enable_file_system_isolation: bool,
    /// Allowed ports
    pub allowed_ports: Vec<u16>,
    /// Blocked addresses
    pub blocked_addresses: Vec<String>,
}

/// Resource policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourcePolicy {
    /// Maximum CPU usage percentage
    pub max_cpu_usage_percent: f64,
    /// Maximum memory usage bytes
    pub max_memory_usage_bytes: u64,
    /// Maximum disk usage bytes
    pub max_disk_usage_bytes: u64,
    /// Maximum network bandwidth bytes per second
    pub max_network_bandwidth_bytes_per_sec: u64,
    /// Maximum container count
    pub max_container_count: u32,
    /// Maximum test execution time
    pub max_test_execution_time: Duration,
    /// Enable resource monitoring
    pub enable_resource_monitoring: bool,
    /// Resource cleanup timeout
    pub resource_cleanup_timeout: Duration,
}

/// Execution policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionPolicy {
    /// Enable deterministic execution
    pub enable_deterministic_execution: bool,
    /// Fixed seed for deterministic runs
    pub deterministic_seed: Option<u64>,
    /// Enable parallel execution
    pub enable_parallel_execution: bool,
    /// Maximum parallel tasks
    pub max_parallel_tasks: u32,
    /// Enable test isolation
    pub enable_test_isolation: bool,
    /// Test timeout
    pub test_timeout: Duration,
    /// Enable retry on failure
    pub enable_retry_on_failure: bool,
    /// Maximum retry attempts
    pub max_retry_attempts: u32,
    /// Retry delay
    pub retry_delay: Duration,
}

/// Compliance policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompliancePolicy {
    /// Enable compliance reporting
    pub enable_compliance_reporting: bool,
    /// Compliance standards
    pub compliance_standards: Vec<ComplianceStandard>,
    /// Enable audit trails
    pub enable_audit_trails: bool,
    /// Audit retention period
    pub audit_retention_period: Duration,
    /// Enable policy validation
    pub enable_policy_validation: bool,
    /// Policy validation rules
    pub policy_validation_rules: Vec<PolicyValidationRule>,
}

/// Security level enumeration
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum SecurityLevel {
    /// Low security level
    Low,
    /// Medium security level
    Medium,
    /// High security level
    High,
    /// Maximum security level
    Maximum,
    /// Standard security level
    Standard,
    /// Locked security level
    Locked,
    /// Permissive security level
    Permissive,
    /// Strict security level
    Strict,
}

impl fmt::Display for SecurityLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SecurityLevel::Low => write!(f, "Low"),
            SecurityLevel::Medium => write!(f, "Medium"),
            SecurityLevel::High => write!(f, "High"),
            SecurityLevel::Maximum => write!(f, "Maximum"),
            SecurityLevel::Standard => write!(f, "Standard"),
            SecurityLevel::Locked => write!(f, "Locked"),
            SecurityLevel::Permissive => write!(f, "Permissive"),
            SecurityLevel::Strict => write!(f, "Strict"),
        }
    }
}

/// Audit level enumeration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AuditLevel {
    /// Debug audit level
    Debug,
    /// Info audit level
    Info,
    /// Warning audit level
    Warn,
    /// Error audit level
    Error,
}

/// Compliance standard enumeration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ComplianceStandard {
    /// SOC 2 compliance
    Soc2,
    /// ISO 27001 compliance
    Iso27001,
    /// PCI DSS compliance
    PciDss,
    /// HIPAA compliance
    Hipaa,
    /// GDPR compliance
    Gdpr,
    /// Custom compliance standard
    Custom(String),
}

/// Policy validation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyValidationRule {
    /// Rule name
    pub name: String,
    /// Rule description
    pub description: String,
    /// Rule condition
    pub condition: String,
    /// Rule action
    pub action: PolicyValidationAction,
    /// Rule severity
    pub severity: PolicyValidationSeverity,
}

/// Policy validation action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PolicyValidationAction {
    /// Allow operation
    Allow,
    /// Deny operation
    Deny,
    /// Warn about operation
    Warn,
    /// Require approval
    RequireApproval,
}

/// Policy validation severity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PolicyValidationSeverity {
    /// Low severity
    Low,
    /// Medium severity
    Medium,
    /// High severity
    High,
    /// Critical severity
    Critical,
}

impl SecurityPolicy {
    /// Create a new security policy with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a security policy with a specific security level
    pub fn with_security_level(security_level: SecurityLevel) -> Self {
        let mut policy = Self {
            security_level,
            ..Default::default()
        };
        
        // Adjust isolation settings based on security level
        match security_level {
            SecurityLevel::Low | SecurityLevel::Permissive => {
                policy.enable_network_isolation = false;
                policy.enable_filesystem_isolation = false;
                policy.enable_process_isolation = false;
                policy.enable_data_redaction = false;
                policy.enable_audit_logging = false;
            },
            SecurityLevel::Medium | SecurityLevel::Standard => {
                // Keep default settings
            },
            SecurityLevel::High | SecurityLevel::Maximum | SecurityLevel::Locked | SecurityLevel::Strict => {
                // Keep all isolation enabled (default)
            },
        }
        
        policy
    }
}

impl Default for SecurityPolicy {
    fn default() -> Self {
        Self {
            enable_network_isolation: true,
            enable_filesystem_isolation: true,
            enable_process_isolation: true,
            allowed_ports: vec![5432, 6379, 8080, 9090],
            blocked_addresses: vec!["127.0.0.1".to_string()],
            enable_data_redaction: true,
            redaction_patterns: vec![
                r"password\s*=\s*[^\s]+".to_string(),
                r"token\s*=\s*[^\s]+".to_string(),
                r"key\s*=\s*[^\s]+".to_string(),
            ],
            enable_audit_logging: true,
            security_level: SecurityLevel::Standard,
        }
    }
}

impl Default for NetworkPolicy {
    fn default() -> Self {
        Self {
            enable_network_isolation: true,
            enable_port_scanning: false,
            enable_file_system_isolation: true,
            allowed_ports: vec![5432, 6379, 8080, 9090],
            blocked_addresses: vec!["127.0.0.1".to_string()],
        }
    }
}

impl ResourcePolicy {
    /// Create a new resource policy with default settings
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for ResourcePolicy {
    fn default() -> Self {
        Self {
            max_cpu_usage_percent: 80.0,
            max_memory_usage_bytes: 1024 * 1024 * 1024, // 1GB
            max_disk_usage_bytes: 2 * 1024 * 1024 * 1024, // 2GB
            max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024, // 100MB/s
            max_container_count: 10,
            max_test_execution_time: Duration::from_secs(300),
            enable_resource_monitoring: true,
            resource_cleanup_timeout: Duration::from_secs(30),
        }
    }
}

impl Default for ExecutionPolicy {
    fn default() -> Self {
        Self {
            enable_deterministic_execution: true,
            deterministic_seed: Some(42),
            enable_parallel_execution: true,
            max_parallel_tasks: 4,
            enable_test_isolation: true,
            test_timeout: Duration::from_secs(300),
            enable_retry_on_failure: true,
            max_retry_attempts: 3,
            retry_delay: Duration::from_secs(1),
        }
    }
}

impl Default for CompliancePolicy {
    fn default() -> Self {
        Self {
            enable_compliance_reporting: false,
            compliance_standards: Vec::new(),
            enable_audit_trails: true,
            audit_retention_period: Duration::from_secs(30 * 24 * 60 * 60), // 30 days
            enable_policy_validation: true,
            policy_validation_rules: Vec::new(),
        }
    }
}

impl Policy {
    /// Create a new policy with default values
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create a policy with custom security level
    pub fn with_security_level(security_level: SecurityLevel) -> Self {
        Self {
            security: SecurityPolicy::with_security_level(security_level),
            ..Default::default()
        }
    }
    
    /// Create a policy with custom resource limits
    pub fn with_resource_limits(
        max_cpu_percent: f64,
        max_memory_bytes: u64,
        max_disk_bytes: u64,
    ) -> Self {
        let mut policy = Self::default();
        policy.resources.max_cpu_usage_percent = max_cpu_percent;
        policy.resources.max_memory_usage_bytes = max_memory_bytes;
        policy.resources.max_disk_usage_bytes = max_disk_bytes;
        policy
    }
    
    /// Create a locked-down policy with maximum security
    pub fn locked() -> Self {
        Self::with_security_level(SecurityLevel::Locked)
    }

    /// Create a high security policy
    pub fn high_security() -> Self {
        Self::with_security_level(SecurityLevel::High)
    }

    /// Create a standard security policy
    pub fn standard() -> Self {
        Self::with_security_level(SecurityLevel::Standard)
    }

    /// Create a low security policy
    pub fn low_security() -> Self {
        Self::with_security_level(SecurityLevel::Low)
    }

    /// Disable network access
    pub fn with_network_disabled(mut self) -> Self {
        self.security.enable_network_isolation = true;
        self.network.enable_network_isolation = true;
        self
    }

    /// Set network isolation
    pub fn with_network_isolation(mut self, enable: bool) -> Self {
        self.security.enable_network_isolation = enable;
        self.network.enable_network_isolation = enable;
        self
    }

    /// Check if network access is allowed
    pub fn allows_network(&self) -> bool {
        !self.security.enable_network_isolation && !self.network.enable_network_isolation
    }

    /// Validate the policy configuration
    pub fn validate(&self) -> Result<()> {
        // Validate security policy
        if self.security.security_level == SecurityLevel::Locked && self.allows_network() {
            return Err(CleanroomError::policy_error(
                "Locked security level cannot allow network access"
            ));
        }

        // Validate resource limits
        if self.resources.max_cpu_usage_percent <= 0.0 || self.resources.max_cpu_usage_percent > 100.0 {
            return Err(CleanroomError::policy_error(
                "CPU usage percentage must be between 0 and 100"
            ));
        }

        if self.resources.max_memory_usage_bytes == 0 {
            return Err(CleanroomError::policy_error(
                "Memory usage limit must be greater than 0"
            ));
        }

        if self.resources.max_disk_usage_bytes == 0 {
            return Err(CleanroomError::policy_error(
                "Disk usage limit must be greater than 0"
            ));
        }

        // Validate execution policy
        if self.execution.max_parallel_tasks == 0 {
            return Err(CleanroomError::policy_error(
                "Maximum parallel tasks must be greater than 0"
            ));
        }

        if self.execution.test_timeout.as_secs() == 0 {
            return Err(CleanroomError::policy_error(
                "Test timeout must be greater than 0"
            ));
        }

        Ok(())
    }

    /// Get a summary of the policy
    pub fn summary(&self) -> String {
        format!(
            "Policy Summary:\n\
            Security Level: {}\n\
            Network Isolation: {}\n\
            Filesystem Isolation: {}\n\
            Process Isolation: {}\n\
            Data Redaction: {}\n\
            Audit Logging: {}\n\
            Resource Monitoring: {}\n\
            Deterministic Execution: {}\n\
            Parallel Execution: {}\n\
            Test Isolation: {}",
            self.security.security_level,
            self.security.enable_network_isolation,
            self.security.enable_filesystem_isolation,
            self.security.enable_process_isolation,
            self.security.enable_data_redaction,
            self.security.enable_audit_logging,
            self.resources.enable_resource_monitoring,
            self.execution.enable_deterministic_execution,
            self.execution.enable_parallel_execution,
            self.execution.enable_test_isolation
        )
    }
}

use std::fmt;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_policy_creation() {
        let policy = Policy::new();
        assert_eq!(policy.security.security_level, SecurityLevel::Standard);
        assert!(policy.security.enable_network_isolation);
    }

    #[test]
    fn test_policy_with_security_level() {
        let policy = Policy::with_security_level(SecurityLevel::High);
        assert_eq!(policy.security.security_level, SecurityLevel::High);
    }

    #[test]
    fn test_policy_locked() {
        let policy = Policy::locked();
        assert_eq!(policy.security.security_level, SecurityLevel::Locked);
        assert!(!policy.allows_network());
    }

    #[test]
    fn test_policy_allows_network() {
        let policy = Policy::new();
        assert!(!policy.allows_network()); // Default has network isolation enabled

        let permissive_policy = Policy::with_security_level(SecurityLevel::Permissive);
        assert!(permissive_policy.allows_network());
    }

    #[test]
    fn test_policy_validation() {
        let policy = Policy::new();
        assert!(policy.validate().is_ok());

        let mut invalid_policy = Policy::new();
        invalid_policy.resources.max_cpu_usage_percent = 150.0; // Invalid
        assert!(invalid_policy.validate().is_err());
    }

    #[test]
    fn test_policy_summary() {
        let policy = Policy::new();
        let summary = policy.summary();
        assert!(summary.contains("Policy Summary"));
        assert!(summary.contains("Security Level"));
        assert!(summary.contains("Network Isolation"));
    }

    #[test]
    fn test_security_level_display() {
        assert_eq!(format!("{}", SecurityLevel::Standard), "Standard");
        assert_eq!(format!("{}", SecurityLevel::Locked), "Locked");
        assert_eq!(format!("{}", SecurityLevel::High), "High");
    }

    #[test]
    fn test_network_policy_default() {
        let network_policy = NetworkPolicy::default();
        assert!(network_policy.enable_network_isolation);
        assert!(!network_policy.enable_port_scanning);
        assert!(network_policy.enable_file_system_isolation);
    }

    #[test]
    fn test_policy_with_network_isolation() {
        let policy = Policy::new().with_network_isolation(false);
        assert!(!policy.security.enable_network_isolation);
        assert!(!policy.network.enable_network_isolation);
        assert!(policy.allows_network());
    }

    #[test]
    fn test_policy_with_network_disabled() {
        let policy = Policy::new().with_network_disabled();
        assert!(policy.security.enable_network_isolation);
        assert!(policy.network.enable_network_isolation);
        assert!(!policy.allows_network());
    }
}
