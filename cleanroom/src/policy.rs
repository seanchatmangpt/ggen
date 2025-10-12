//! Policy enforcement for cleanroom testing
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    /// Security policy
    pub security: SecurityPolicy,
    /// Resource policy
    pub resources: ResourcePolicy,
    /// Execution policy
    pub execution: ExecutionPolicy,
    /// Compliance policy
    pub compliance: CompliancePolicy,
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SecurityLevel {
    /// Low security level
    Low,
    /// Medium security level
    Medium,
    /// High security level
    High,
    /// Maximum security level
    Maximum,
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

impl Default for Policy {
    fn default() -> Self {
        Self {
            security: SecurityPolicy::default(),
            resources: ResourcePolicy::default(),
            execution: ExecutionPolicy::default(),
            compliance: CompliancePolicy::default(),
        }
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
            security_level: SecurityLevel::Medium,
        }
    }
}

impl Default for ResourcePolicy {
    fn default() -> Self {
        Self {
            max_cpu_usage_percent: 80.0,
            max_memory_usage_bytes: 1024 * 1024 * 1024, // 1GB
            max_disk_usage_bytes: 10 * 1024 * 1024 * 1024, // 10GB
            max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024, // 100MB/s
            max_container_count: 10,
            max_test_execution_time: Duration::from_secs(300), // 5 minutes
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
            test_timeout: Duration::from_secs(60),
            enable_retry_on_failure: true,
            max_retry_attempts: 3,
            retry_delay: Duration::from_secs(1),
        }
    }
}

impl Default for CompliancePolicy {
    fn default() -> Self {
        Self {
            enable_compliance_reporting: true,
            compliance_standards: vec![ComplianceStandard::Soc2],
            enable_audit_trails: true,
            audit_retention_period: Duration::from_secs(30 * 24 * 60 * 60), // 30 days
            enable_policy_validation: true,
            policy_validation_rules: vec![],
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
        let mut policy = Self::default();
        policy.security.security_level = security_level;
        policy
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
    
    /// Validate policy configuration
    pub fn validate(&self) -> Result<()> {
        // Validate security policy
        if self.security.allowed_ports.is_empty() {
            return Err(CleanroomError::policy_violation("No allowed ports configured"));
        }
        
        // Validate resource policy
        if self.resources.max_cpu_usage_percent <= 0.0 || self.resources.max_cpu_usage_percent > 100.0 {
            return Err(CleanroomError::policy_violation("Invalid CPU usage percentage"));
        }
        
        if self.resources.max_memory_usage_bytes == 0 {
            return Err(CleanroomError::policy_violation("Invalid memory usage limit"));
        }
        
        if self.resources.max_disk_usage_bytes == 0 {
            return Err(CleanroomError::policy_violation("Invalid disk usage limit"));
        }
        
        // Validate execution policy
        if self.execution.max_parallel_tasks == 0 {
            return Err(CleanroomError::policy_violation("Invalid parallel task count"));
        }
        
        if self.execution.max_retry_attempts == 0 {
            return Err(CleanroomError::policy_violation("Invalid retry attempt count"));
        }
        
        Ok(())
    }
    
    /// Check if operation is allowed by policy
    pub fn is_operation_allowed(&self, operation: &str, context: &HashMap<String, String>) -> Result<bool> {
        // Check security policy
        if self.security.enable_network_isolation {
            if let Some(port) = context.get("port") {
                if let Ok(port_num) = port.parse::<u16>() {
                    if !self.security.allowed_ports.contains(&port_num) {
                        return Ok(false);
                    }
                }
            }
        }
        
        // Check resource policy
        if let Some(cpu_usage) = context.get("cpu_usage") {
            if let Ok(cpu_percent) = cpu_usage.parse::<f64>() {
                if cpu_percent > self.resources.max_cpu_usage_percent {
                    return Ok(false);
                }
            }
        }
        
        if let Some(memory_usage) = context.get("memory_usage") {
            if let Ok(memory_bytes) = memory_usage.parse::<u64>() {
                if memory_bytes > self.resources.max_memory_usage_bytes {
                    return Ok(false);
                }
            }
        }
        
        // Check execution policy
        if let Some(parallel_tasks) = context.get("parallel_tasks") {
            if let Ok(task_count) = parallel_tasks.parse::<u32>() {
                if task_count > self.execution.max_parallel_tasks {
                    return Ok(false);
                }
            }
        }
        
        Ok(true)
    }
    
    /// Get environment variables for policy enforcement
    pub fn to_env(&self) -> HashMap<String, String> {
        let mut env = HashMap::new();
        
        // Security policy environment variables
        env.insert("CLEANROOM_SECURITY_LEVEL".to_string(), format!("{:?}", self.security.security_level));
        env.insert("CLEANROOM_NETWORK_ISOLATION".to_string(), self.security.enable_network_isolation.to_string());
        env.insert("CLEANROOM_FILESYSTEM_ISOLATION".to_string(), self.security.enable_filesystem_isolation.to_string());
        env.insert("CLEANROOM_PROCESS_ISOLATION".to_string(), self.security.enable_process_isolation.to_string());
        env.insert("CLEANROOM_ALLOWED_PORTS".to_string(), self.security.allowed_ports.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(","));
        
        // Resource policy environment variables
        env.insert("CLEANROOM_MAX_CPU_PERCENT".to_string(), self.resources.max_cpu_usage_percent.to_string());
        env.insert("CLEANROOM_MAX_MEMORY_BYTES".to_string(), self.resources.max_memory_usage_bytes.to_string());
        env.insert("CLEANROOM_MAX_DISK_BYTES".to_string(), self.resources.max_disk_usage_bytes.to_string());
        env.insert("CLEANROOM_MAX_CONTAINER_COUNT".to_string(), self.resources.max_container_count.to_string());
        
        // Execution policy environment variables
        env.insert("CLEANROOM_DETERMINISTIC_EXECUTION".to_string(), self.execution.enable_deterministic_execution.to_string());
        env.insert("CLEANROOM_PARALLEL_EXECUTION".to_string(), self.execution.enable_parallel_execution.to_string());
        env.insert("CLEANROOM_MAX_PARALLEL_TASKS".to_string(), self.execution.max_parallel_tasks.to_string());
        env.insert("CLEANROOM_TEST_ISOLATION".to_string(), self.execution.enable_test_isolation.to_string());
        
        env
    }
    
    /// Get policy summary
    pub fn summary(&self) -> String {
        format!(
            "Policy Summary:\n\
            Security Level: {:?}\n\
            Network Isolation: {}\n\
            Filesystem Isolation: {}\n\
            Process Isolation: {}\n\
            Max CPU Usage: {}%\n\
            Max Memory Usage: {} bytes\n\
            Max Disk Usage: {} bytes\n\
            Max Container Count: {}\n\
            Deterministic Execution: {}\n\
            Parallel Execution: {}\n\
            Max Parallel Tasks: {}\n\
            Test Isolation: {}",
            self.security.security_level,
            self.security.enable_network_isolation,
            self.security.enable_filesystem_isolation,
            self.security.enable_process_isolation,
            self.resources.max_cpu_usage_percent,
            self.resources.max_memory_usage_bytes,
            self.resources.max_disk_usage_bytes,
            self.resources.max_container_count,
            self.execution.enable_deterministic_execution,
            self.execution.enable_parallel_execution,
            self.execution.max_parallel_tasks,
            self.execution.enable_test_isolation
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_policy_creation() {
        let policy = Policy::new();
        assert!(policy.validate().is_ok());
    }
    
    #[test]
    fn test_policy_with_security_level() {
        let policy = Policy::with_security_level(SecurityLevel::High);
        assert_eq!(policy.security.security_level, SecurityLevel::High);
    }
    
    #[test]
    fn test_policy_with_resource_limits() {
        let policy = Policy::with_resource_limits(50.0, 512 * 1024 * 1024, 5 * 1024 * 1024 * 1024);
        assert_eq!(policy.resources.max_cpu_usage_percent, 50.0);
        assert_eq!(policy.resources.max_memory_usage_bytes, 512 * 1024 * 1024);
        assert_eq!(policy.resources.max_disk_usage_bytes, 5 * 1024 * 1024 * 1024);
    }
    
    #[test]
    fn test_policy_validation() {
        let policy = Policy::new();
        assert!(policy.validate().is_ok());
    }
    
    #[test]
    fn test_policy_operation_allowed() {
        let policy = Policy::new();
        let mut context = HashMap::new();
        context.insert("port".to_string(), "5432".to_string());
        context.insert("cpu_usage".to_string(), "50.0".to_string());
        context.insert("memory_usage".to_string(), "256000000".to_string());
        
        assert!(policy.is_operation_allowed("test_operation", &context).unwrap());
    }
    
    #[test]
    fn test_policy_operation_denied() {
        let policy = Policy::new();
        let mut context = HashMap::new();
        context.insert("port".to_string(), "9999".to_string()); // Not in allowed ports
        context.insert("cpu_usage".to_string(), "90.0".to_string()); // Exceeds limit
        
        assert!(!policy.is_operation_allowed("test_operation", &context).unwrap());
    }
    
    #[test]
    fn test_policy_env_variables() {
        let policy = Policy::new();
        let env = policy.to_env();
        
        assert!(env.contains_key("CLEANROOM_SECURITY_LEVEL"));
        assert!(env.contains_key("CLEANROOM_NETWORK_ISOLATION"));
        assert!(env.contains_key("CLEANROOM_MAX_CPU_PERCENT"));
        assert!(env.contains_key("CLEANROOM_MAX_MEMORY_BYTES"));
    }
    
    #[test]
    fn test_policy_summary() {
        let policy = Policy::new();
        let summary = policy.summary();
        
        assert!(summary.contains("Policy Summary"));
        assert!(summary.contains("Security Level"));
        assert!(summary.contains("Network Isolation"));
    }
}