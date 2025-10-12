# Type System

This document provides a comprehensive reference for the type system used in the Cleanroom Testing Framework.

## Overview

### Type Categories

The Cleanroom type system is organized into the following categories:

1. **Core Types**: Essential types for cleanroom operations
2. **Configuration Types**: Types for configuration and settings
3. **Result Types**: Types for operation results and outcomes
4. **Error Types**: Types for error handling and reporting
5. **Utility Types**: Supporting types and utilities
6. **Trait Types**: Trait definitions and implementations

### Type Design Principles

1. **Type Safety**: Strong typing throughout the system
2. **Immutability**: Prefer immutable types where possible
3. **Serialization**: Support for serialization and deserialization
4. **Validation**: Built-in validation and constraints
5. **Documentation**: Comprehensive type documentation

## Core Types

### `CleanroomEnvironment`

The main environment type for cleanroom operations.

```rust
/// Main environment for cleanroom testing
pub struct CleanroomEnvironment {
    /// Unique session identifier
    pub session_id: Uuid,
    /// Configuration for the environment
    config: CleanroomConfig,
    /// Performance metrics
    metrics: Arc<RwLock<CleanroomMetrics>>,
    /// Container registry
    container_registry: Arc<RwLock<HashMap<String, String>>>,
    /// Backend for container operations
    backend: TestcontainerBackend,
    /// Service manager (optional)
    #[cfg(feature = "services")]
    services: ServiceManager,
    /// Environment start time
    start_time: Instant,
}

impl CleanroomEnvironment {
    /// Create a new cleanroom environment
    pub async fn new(config: CleanroomConfig) -> Result<Self, CleanroomError>;
    
    /// Execute a single test command
    pub async fn execute_test(&self, command: &str) -> Result<RunResult, CleanroomError>;
    
    /// Execute a scenario with multiple steps
    pub async fn execute_scenario(&self, scenario: &Scenario) -> Result<ScenarioResult, CleanroomError>;
    
    /// Get current performance metrics
    pub fn get_metrics(&self) -> CleanroomMetrics;
    
    /// Clean up all resources
    pub async fn cleanup(&self) -> Result<(), CleanroomError>;
}
```

### `CleanroomConfig`

Configuration type for cleanroom environment setup.

```rust
/// Configuration for cleanroom environment
pub struct CleanroomConfig {
    /// Backend configuration
    pub backend: BackendConfig,
    /// Security configuration
    pub security: SecurityConfig,
    /// Performance configuration
    pub performance: PerformanceConfig,
    /// Resource configuration
    pub resources: ResourceConfig,
    /// Logging configuration
    pub logging: LoggingConfig,
    /// Monitoring configuration
    pub monitoring: MonitoringConfig,
    /// Policy configuration
    pub policy: Option<Policy>,
    /// Feature flags
    pub features: FeatureFlags,
}

impl Default for CleanroomConfig {
    fn default() -> Self {
        Self {
            backend: BackendConfig::default(),
            security: SecurityConfig::default(),
            performance: PerformanceConfig::default(),
            resources: ResourceConfig::default(),
            logging: LoggingConfig::default(),
            monitoring: MonitoringConfig::default(),
            policy: None,
            features: FeatureFlags::default(),
        }
    }
}
```

### `Policy`

Policy type for security, resource, and execution constraints.

```rust
/// Main policy container
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

impl Policy {
    /// Create a new policy
    pub fn new() -> Self;
    
    /// Validate the policy
    pub fn validate(&self) -> Result<(), PolicyError>;
    
    /// Merge with another policy
    pub fn merge(&self, other: &Policy) -> Policy;
    
    /// Check if policy allows operation
    pub fn allows(&self, operation: &Operation) -> bool;
}
```

## Result Types

### `RunResult`

Result type for single command execution.

```rust
/// Result of command execution
pub struct RunResult {
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration
    pub duration: Duration,
    /// Resource usage during execution
    pub resource_usage: Option<ResourceUsage>,
    /// Execution metadata
    pub metadata: HashMap<String, String>,
}

impl RunResult {
    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        self.exit_code == 0
    }
    
    /// Check if execution failed
    pub fn is_failure(&self) -> bool {
        self.exit_code != 0
    }
    
    /// Get execution status
    pub fn status(&self) -> ExecutionStatus {
        if self.is_success() {
            ExecutionStatus::Success
        } else {
            ExecutionStatus::Failure
        }
    }
}
```

### `ScenarioResult`

Result type for scenario execution.

```rust
/// Result of scenario execution
pub struct ScenarioResult {
    /// Scenario name
    pub name: String,
    /// Overall success status
    pub success: bool,
    /// Total execution time
    pub total_duration: Duration,
    /// Individual step results
    pub steps: Vec<StepResult>,
    /// Scenario metrics
    pub metrics: Option<ScenarioMetrics>,
    /// Scenario metadata
    pub metadata: HashMap<String, String>,
}

impl ScenarioResult {
    /// Check if scenario was successful
    pub fn is_success(&self) -> bool {
        self.success
    }
    
    /// Get failed steps
    pub fn failed_steps(&self) -> Vec<&StepResult> {
        self.steps.iter().filter(|step| !step.success).collect()
    }
    
    /// Get successful steps
    pub fn successful_steps(&self) -> Vec<&StepResult> {
        self.steps.iter().filter(|step| step.success).collect()
    }
    
    /// Get step by name
    pub fn get_step(&self, name: &str) -> Option<&StepResult> {
        self.steps.iter().find(|step| step.name == name)
    }
}
```

### `StepResult`

Result type for individual step execution.

```rust
/// Result of individual step execution
pub struct StepResult {
    /// Step name
    pub name: String,
    /// Exit code
    pub exit_code: i32,
    /// Standard output
    pub stdout: String,
    /// Standard error
    pub stderr: String,
    /// Execution duration
    pub duration: Duration,
    /// Success status
    pub success: bool,
    /// Step metrics
    pub metrics: Option<StepMetrics>,
    /// Step metadata
    pub metadata: HashMap<String, String>,
}

impl StepResult {
    /// Check if step was successful
    pub fn is_success(&self) -> bool {
        self.success
    }
    
    /// Check if step failed
    pub fn is_failure(&self) -> bool {
        !self.success
    }
    
    /// Get step status
    pub fn status(&self) -> StepStatus {
        if self.success {
            StepStatus::Success
        } else {
            StepStatus::Failure
        }
    }
}
```

## Error Types

### `CleanroomError`

Main error type for cleanroom operations.

```rust
/// Main error type for cleanroom operations
#[derive(Debug, thiserror::Error)]
pub enum CleanroomError {
    /// Container-related errors
    #[error("Container error: {0}")]
    Container(#[from] ContainerError),
    
    /// Backend-related errors
    #[error("Backend error: {0}")]
    Backend(#[from] BackendError),
    
    /// Policy-related errors
    #[error("Policy error: {0}")]
    Policy(#[from] PolicyError),
    
    /// Configuration-related errors
    #[error("Configuration error: {0}")]
    Configuration(#[from] ConfigurationError),
    
    /// Execution-related errors
    #[error("Execution error: {0}")]
    Execution(#[from] ExecutionError),
    
    /// Scenario-related errors
    #[error("Scenario error: {0}")]
    Scenario(#[from] ScenarioError),
    
    /// Performance-related errors
    #[error("Performance error: {0}")]
    Performance(#[from] PerformanceError),
    
    /// Security-related errors
    #[error("Security error: {0}")]
    Security(#[from] SecurityError),
    
    /// Resource-related errors
    #[error("Resource error: {0}")]
    Resource(#[from] ResourceError),
    
    /// Validation-related errors
    #[error("Validation error: {0}")]
    Validation(#[from] ValidationError),
}

impl CleanroomError {
    /// Get error category
    pub fn category(&self) -> ErrorCategory {
        match self {
            CleanroomError::Container(_) => ErrorCategory::Container,
            CleanroomError::Backend(_) => ErrorCategory::Backend,
            CleanroomError::Policy(_) => ErrorCategory::Policy,
            CleanroomError::Configuration(_) => ErrorCategory::Configuration,
            CleanroomError::Execution(_) => ErrorCategory::Execution,
            CleanroomError::Scenario(_) => ErrorCategory::Scenario,
            CleanroomError::Performance(_) => ErrorCategory::Performance,
            CleanroomError::Security(_) => ErrorCategory::Security,
            CleanroomError::Resource(_) => ErrorCategory::Resource,
            CleanroomError::Validation(_) => ErrorCategory::Validation,
        }
    }
    
    /// Check if error is retryable
    pub fn is_retryable(&self) -> bool {
        match self {
            CleanroomError::Backend(BackendError::ConnectionFailed(_)) => true,
            CleanroomError::Backend(BackendError::Timeout(_)) => true,
            CleanroomError::Execution(ExecutionError::Timeout(_)) => true,
            CleanroomError::Execution(ExecutionError::ResourceUnavailable(_)) => true,
            _ => false,
        }
    }
}
```

### `ContainerError`

Container-specific error type.

```rust
/// Container-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ContainerError {
    /// Container startup failed
    #[error("Container startup failed: {0}")]
    StartupFailed(String),
    
    /// Container execution failed
    #[error("Container execution failed: {0}")]
    ExecutionFailed(String),
    
    /// Container cleanup failed
    #[error("Container cleanup failed: {0}")]
    CleanupFailed(String),
    
    /// Container not found
    #[error("Container not found: {0}")]
    NotFound(String),
    
    /// Container already exists
    #[error("Container already exists: {0}")]
    AlreadyExists(String),
    
    /// Container state invalid
    #[error("Container state invalid: {0}")]
    InvalidState(String),
    
    /// Container resource limit exceeded
    #[error("Container resource limit exceeded: {0}")]
    ResourceLimitExceeded(String),
}

impl ContainerError {
    /// Get container ID from error
    pub fn container_id(&self) -> Option<&str> {
        match self {
            ContainerError::NotFound(id) => Some(id),
            ContainerError::AlreadyExists(id) => Some(id),
            ContainerError::InvalidState(id) => Some(id),
            ContainerError::ResourceLimitExceeded(id) => Some(id),
            _ => None,
        }
    }
    
    /// Check if error is recoverable
    pub fn is_recoverable(&self) -> bool {
        match self {
            ContainerError::StartupFailed(_) => true,
            ContainerError::ExecutionFailed(_) => true,
            ContainerError::CleanupFailed(_) => true,
            ContainerError::ResourceLimitExceeded(_) => true,
            _ => false,
        }
    }
}
```

## Configuration Types

### `BackendConfig`

Backend configuration type.

```rust
/// Backend configuration
pub struct BackendConfig {
    /// Backend type
    pub backend_type: BackendType,
    /// Backend endpoint
    pub endpoint: Option<String>,
    /// Backend credentials
    pub credentials: Option<BackendCredentials>,
    /// Backend timeout
    pub timeout: Duration,
    /// Backend retry configuration
    pub retry_config: RetryConfig,
}

/// Backend type enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum BackendType {
    /// Docker backend
    Docker,
    /// Podman backend
    Podman,
    /// Kubernetes backend
    Kubernetes,
    /// Custom backend
    Custom(String),
}

/// Backend credentials
pub struct BackendCredentials {
    /// Username
    pub username: Option<String>,
    /// Password
    pub password: Option<String>,
    /// Token
    pub token: Option<String>,
    /// Certificate
    pub certificate: Option<PathBuf>,
    /// Key
    pub key: Option<PathBuf>,
}
```

### `SecurityConfig`

Security configuration type.

```rust
/// Security configuration
pub struct SecurityConfig {
    /// Enable security features
    pub enable_security: bool,
    /// Security policy
    pub security_policy: SecurityPolicy,
    /// Encryption configuration
    pub encryption: EncryptionConfig,
    /// Authentication configuration
    pub authentication: AuthenticationConfig,
    /// Authorization configuration
    pub authorization: AuthorizationConfig,
}

/// Security policy
pub struct SecurityPolicy {
    /// Enable network isolation
    pub enable_network_isolation: bool,
    /// Enable filesystem isolation
    pub enable_filesystem_isolation: bool,
    /// Enable process isolation
    pub enable_process_isolation: bool,
    /// Allowed network ports
    pub allowed_ports: Vec<u16>,
    /// Blocked network hosts
    pub blocked_hosts: Vec<String>,
    /// Allowed filesystem paths
    pub allowed_paths: Vec<PathBuf>,
    /// Blocked filesystem paths
    pub blocked_paths: Vec<PathBuf>,
    /// Allowed commands
    pub allowed_commands: Vec<String>,
    /// Blocked commands
    pub blocked_commands: Vec<String>,
    /// Enable audit logging
    pub enable_audit_logging: bool,
    /// Audit log level
    pub audit_log_level: LogLevel,
}
```

### `ResourceConfig`

Resource configuration type.

```rust
/// Resource configuration
pub struct ResourceConfig {
    /// Resource limits
    pub limits: ResourceLimits,
    /// Resource allocation
    pub allocation: ResourceAllocation,
    /// Resource monitoring
    pub monitoring: ResourceMonitoring,
    /// Resource cleanup
    pub cleanup: ResourceCleanup,
}

/// Resource limits
pub struct ResourceLimits {
    /// Maximum memory usage in MB
    pub max_memory_mb: u64,
    /// Maximum CPU usage percentage
    pub max_cpu_percent: f64,
    /// Maximum disk usage in MB
    pub max_disk_mb: u64,
    /// Maximum network usage in MB
    pub max_network_mb: u64,
    /// Maximum execution time
    pub max_execution_time: Duration,
    /// Maximum number of containers
    pub max_containers: u32,
    /// Maximum number of concurrent operations
    pub max_concurrent_operations: u32,
}

/// Resource allocation
pub struct ResourceAllocation {
    /// Memory allocation strategy
    pub memory_strategy: AllocationStrategy,
    /// CPU allocation strategy
    pub cpu_strategy: AllocationStrategy,
    /// Disk allocation strategy
    pub disk_strategy: AllocationStrategy,
    /// Network allocation strategy
    pub network_strategy: AllocationStrategy,
}

/// Allocation strategy
#[derive(Debug, Clone, PartialEq)]
pub enum AllocationStrategy {
    /// Static allocation
    Static,
    /// Dynamic allocation
    Dynamic,
    /// Shared allocation
    Shared,
    /// Exclusive allocation
    Exclusive,
}
```

## Utility Types

### `CleanroomMetrics`

Performance metrics type.

```rust
/// Performance metrics for the environment
pub struct CleanroomMetrics {
    /// Session identifier
    pub session_id: Uuid,
    /// Environment start time
    pub start_time: SerializableInstant,
    /// Environment end time
    pub end_time: Option<SerializableInstant>,
    /// Total containers created
    pub containers_created: u64,
    /// Total containers destroyed
    pub containers_destroyed: u64,
    /// Total tests executed
    pub tests_executed: u64,
    /// Total execution time
    pub total_execution_time: Duration,
    /// Average execution time per test
    pub average_execution_time: Duration,
    /// Peak memory usage
    pub peak_memory_usage: u64,
    /// Peak CPU usage
    pub peak_cpu_usage: f64,
    /// Total network usage
    pub total_network_usage: u64,
    /// Total disk usage
    pub total_disk_usage: u64,
}

impl CleanroomMetrics {
    /// Calculate uptime
    pub fn uptime(&self) -> Duration {
        match self.end_time {
            Some(end_time) => end_time.0.duration_since(self.start_time.0),
            None => self.start_time.0.elapsed(),
        }
    }
    
    /// Calculate success rate
    pub fn success_rate(&self) -> f64 {
        if self.tests_executed == 0 {
            0.0
        } else {
            // This would need to be calculated from actual success/failure counts
            // For now, return a placeholder
            0.0
        }
    }
    
    /// Calculate resource efficiency
    pub fn resource_efficiency(&self) -> f64 {
        if self.tests_executed == 0 {
            0.0
        } else {
            // Calculate efficiency based on resource usage vs tests executed
            // This is a simplified calculation
            self.tests_executed as f64 / (self.peak_memory_usage as f64 / 1024.0 / 1024.0)
        }
    }
}
```

### `ResourceUsage`

Resource usage information type.

```rust
/// Resource usage information
pub struct ResourceUsage {
    /// Current memory usage in bytes
    pub memory_usage: u64,
    /// Current CPU usage percentage
    pub cpu_usage: f64,
    /// Current disk usage in bytes
    pub disk_usage: u64,
    /// Current network usage in bytes
    pub network_usage: u64,
    /// Timestamp of measurement
    pub timestamp: SerializableInstant,
}

impl ResourceUsage {
    /// Check if usage exceeds limits
    pub fn exceeds_limits(&self, limits: &ResourceLimits) -> bool {
        self.memory_usage > limits.max_memory_mb * 1024 * 1024 ||
        self.cpu_usage > limits.max_cpu_percent ||
        self.disk_usage > limits.max_disk_mb * 1024 * 1024 ||
        self.network_usage > limits.max_network_mb * 1024 * 1024
    }
    
    /// Calculate usage percentage for memory
    pub fn memory_usage_percentage(&self, limit_mb: u64) -> f64 {
        if limit_mb == 0 {
            0.0
        } else {
            (self.memory_usage as f64 / (limit_mb as f64 * 1024.0 * 1024.0)) * 100.0
        }
    }
    
    /// Calculate usage percentage for disk
    pub fn disk_usage_percentage(&self, limit_mb: u64) -> f64 {
        if limit_mb == 0 {
            0.0
        } else {
            (self.disk_usage as f64 / (limit_mb as f64 * 1024.0 * 1024.0)) * 100.0
        }
    }
}
```

### `SerializableInstant`

Serializable instant type for timestamps.

```rust
/// Serializable instant for timestamps
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SerializableInstant(Instant);

impl SerializableInstant {
    /// Create a new serializable instant
    pub fn now() -> Self {
        Self(Instant::now())
    }
    
    /// Get the underlying instant
    pub fn instant(&self) -> Instant {
        self.0
    }
    
    /// Calculate duration since another instant
    pub fn duration_since(&self, earlier: SerializableInstant) -> Duration {
        self.0.duration_since(earlier.0)
    }
    
    /// Calculate elapsed time since this instant
    pub fn elapsed(&self) -> Duration {
        self.0.elapsed()
    }
}

impl Serialize for SerializableInstant {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Convert to milliseconds since epoch
        let millis = self.0.elapsed().as_millis();
        serializer.serialize_u128(millis)
    }
}

impl<'de> Deserialize<'de> for SerializableInstant {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let millis = u128::deserialize(deserializer)?;
        // This is a simplified implementation
        // In practice, you'd need to handle the conversion properly
        Ok(Self(Instant::now()))
    }
}
```

## Trait Types

### `Backend`

Backend trait for container operations.

```rust
/// Backend trait for container operations
pub trait Backend: Send + Sync {
    /// Start a container
    async fn start_container(&self, image: &str, config: &ContainerConfig) -> Result<Container, BackendError>;
    
    /// Stop a container
    async fn stop_container(&self, container_id: &str) -> Result<(), BackendError>;
    
    /// Execute command in container
    async fn execute_command(&self, container_id: &str, cmd: &Cmd) -> Result<RunResult, BackendError>;
    
    /// Get container status
    async fn get_container_status(&self, container_id: &str) -> Result<ContainerStatus, BackendError>;
    
    /// List containers
    async fn list_containers(&self) -> Result<Vec<Container>, BackendError>;
    
    /// Clean up resources
    async fn cleanup(&self) -> Result<(), BackendError>;
}

/// Backend implementation for Docker
pub struct DockerBackend {
    client: Docker,
    config: DockerConfig,
}

impl Backend for DockerBackend {
    async fn start_container(&self, image: &str, config: &ContainerConfig) -> Result<Container, BackendError> {
        // Docker-specific implementation
        todo!()
    }
    
    async fn stop_container(&self, container_id: &str) -> Result<(), BackendError> {
        // Docker-specific implementation
        todo!()
    }
    
    // ... other methods
}
```

### `Assert`

Assertion trait for test validation.

```rust
/// Assertion trait
pub trait Assert {
    /// Assert success
    fn assert_success(&self) -> &Self;
    
    /// Assert failure
    fn assert_failure(&self) -> &Self;
    
    /// Assert exit code
    fn assert_exit_code(&self, expected: i32) -> &Self;
    
    /// Assert stdout contains
    fn assert_stdout_contains(&self, text: &str) -> &Self;
    
    /// Assert stderr contains
    fn assert_stderr_contains(&self, text: &str) -> &Self;
    
    /// Assert stdout equals
    fn assert_stdout_equals(&self, text: &str) -> &Self;
    
    /// Assert stderr equals
    fn assert_stderr_equals(&self, text: &str) -> &Self;
    
    /// Assert duration
    fn assert_duration(&self, max_duration: Duration) -> &Self;
    
    /// Assert resource usage
    fn assert_resource_usage(&self, limits: &ResourceLimits) -> &Self;
}

impl Assert for RunResult {
    fn assert_success(&self) -> &Self {
        assert_eq!(self.exit_code, 0, "Expected success (exit code 0), got {}", self.exit_code);
        self
    }
    
    fn assert_failure(&self) -> &Self {
        assert_ne!(self.exit_code, 0, "Expected failure (non-zero exit code), got {}", self.exit_code);
        self
    }
    
    fn assert_exit_code(&self, expected: i32) -> &Self {
        assert_eq!(self.exit_code, expected, "Expected exit code {}, got {}", expected, self.exit_code);
        self
    }
    
    fn assert_stdout_contains(&self, text: &str) -> &Self {
        assert!(self.stdout.contains(text), "Expected stdout to contain '{}', got '{}'", text, self.stdout);
        self
    }
    
    fn assert_stderr_contains(&self, text: &str) -> &Self {
        assert!(self.stderr.contains(text), "Expected stderr to contain '{}', got '{}'", text, self.stderr);
        self
    }
    
    fn assert_stdout_equals(&self, text: &str) -> &Self {
        assert_eq!(self.stdout.trim(), text.trim(), "Expected stdout '{}', got '{}'", text, self.stdout);
        self
    }
    
    fn assert_stderr_equals(&self, text: &str) -> &Self {
        assert_eq!(self.stderr.trim(), text.trim(), "Expected stderr '{}', got '{}'", text, self.stderr);
        self
    }
    
    fn assert_duration(&self, max_duration: Duration) -> &Self {
        assert!(self.duration <= max_duration, "Expected duration <= {:?}, got {:?}", max_duration, self.duration);
        self
    }
    
    fn assert_resource_usage(&self, limits: &ResourceLimits) -> &Self {
        if let Some(usage) = &self.resource_usage {
            assert!(!usage.exceeds_limits(limits), "Resource usage exceeds limits: {:?}", usage);
        }
        self
    }
}
```

## Type Relationships

### Inheritance Hierarchy

```
CleanroomError
├── ContainerError
├── BackendError
├── PolicyError
├── ConfigurationError
├── ExecutionError
├── ScenarioError
├── PerformanceError
├── SecurityError
├── ResourceError
└── ValidationError
```

### Composition Relationships

```
CleanroomEnvironment
├── CleanroomConfig
│   ├── BackendConfig
│   ├── SecurityConfig
│   ├── PerformanceConfig
│   ├── ResourceConfig
│   ├── LoggingConfig
│   ├── MonitoringConfig
│   └── Policy
│       ├── SecurityPolicy
│       ├── ResourcePolicy
│       ├── ExecutionPolicy
│       └── CompliancePolicy
├── CleanroomMetrics
└── Backend (trait)
```

### Result Type Hierarchy

```
RunResult
├── exit_code: i32
├── stdout: String
├── stderr: String
├── duration: Duration
├── resource_usage: Option<ResourceUsage>
└── metadata: HashMap<String, String>

ScenarioResult
├── name: String
├── success: bool
├── total_duration: Duration
├── steps: Vec<StepResult>
├── metrics: Option<ScenarioMetrics>
└── metadata: HashMap<String, String>

StepResult
├── name: String
├── exit_code: i32
├── stdout: String
├── stderr: String
├── duration: Duration
├── success: bool
├── metrics: Option<StepMetrics>
└── metadata: HashMap<String, String>
```

## Type Validation

### Validation Traits

```rust
/// Trait for validating types
pub trait Validate {
    /// Validate the type
    fn validate(&self) -> Result<(), ValidationError>;
}

/// Validation error
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    /// Field validation failed
    #[error("Field validation failed: {field} - {message}")]
    FieldValidation { field: String, message: String },
    
    /// Range validation failed
    #[error("Range validation failed: {field} - {value} not in range {min}..{max}")]
    RangeValidation { field: String, value: String, min: String, max: String },
    
    /// Format validation failed
    #[error("Format validation failed: {field} - {value} has invalid format")]
    FormatValidation { field: String, value: String },
    
    /// Dependency validation failed
    #[error("Dependency validation failed: {field} - {message}")]
    DependencyValidation { field: String, message: String },
}

impl Validate for CleanroomConfig {
    fn validate(&self) -> Result<(), ValidationError> {
        // Validate backend configuration
        self.backend.validate()?;
        
        // Validate security configuration
        self.security.validate()?;
        
        // Validate performance configuration
        self.performance.validate()?;
        
        // Validate resource configuration
        self.resources.validate()?;
        
        // Validate logging configuration
        self.logging.validate()?;
        
        // Validate monitoring configuration
        self.monitoring.validate()?;
        
        // Validate policy if present
        if let Some(policy) = &self.policy {
            policy.validate()?;
        }
        
        Ok(())
    }
}

impl Validate for ResourceLimits {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.max_memory_mb == 0 {
            return Err(ValidationError::FieldValidation {
                field: "max_memory_mb".to_string(),
                message: "Memory limit must be greater than 0".to_string(),
            });
        }
        
        if self.max_cpu_percent < 0.0 || self.max_cpu_percent > 100.0 {
            return Err(ValidationError::RangeValidation {
                field: "max_cpu_percent".to_string(),
                value: self.max_cpu_percent.to_string(),
                min: "0.0".to_string(),
                max: "100.0".to_string(),
            });
        }
        
        if self.max_disk_mb == 0 {
            return Err(ValidationError::FieldValidation {
                field: "max_disk_mb".to_string(),
                message: "Disk limit must be greater than 0".to_string(),
            });
        }
        
        if self.max_network_mb == 0 {
            return Err(ValidationError::FieldValidation {
                field: "max_network_mb".to_string(),
                message: "Network limit must be greater than 0".to_string(),
            });
        }
        
        if self.max_execution_time.is_zero() {
            return Err(ValidationError::FieldValidation {
                field: "max_execution_time".to_string(),
                message: "Execution time limit must be greater than 0".to_string(),
            });
        }
        
        if self.max_containers == 0 {
            return Err(ValidationError::FieldValidation {
                field: "max_containers".to_string(),
                message: "Container limit must be greater than 0".to_string(),
            });
        }
        
        if self.max_concurrent_operations == 0 {
            return Err(ValidationError::FieldValidation {
                field: "max_concurrent_operations".to_string(),
                message: "Concurrent operations limit must be greater than 0".to_string(),
            });
        }
        
        Ok(())
    }
}
```

## Serialization Support

### Serde Integration

```rust
use serde::{Deserialize, Serialize};

/// Serializable configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SerializableCleanroomConfig {
    /// Backend configuration
    pub backend: SerializableBackendConfig,
    /// Security configuration
    pub security: SerializableSecurityConfig,
    /// Performance configuration
    pub performance: SerializablePerformanceConfig,
    /// Resource configuration
    pub resources: SerializableResourceConfig,
    /// Logging configuration
    pub logging: SerializableLoggingConfig,
    /// Monitoring configuration
    pub monitoring: SerializableMonitoringConfig,
    /// Policy configuration
    pub policy: Option<SerializablePolicy>,
    /// Feature flags
    pub features: SerializableFeatureFlags,
}

impl From<CleanroomConfig> for SerializableCleanroomConfig {
    fn from(config: CleanroomConfig) -> Self {
        Self {
            backend: config.backend.into(),
            security: config.security.into(),
            performance: config.performance.into(),
            resources: config.resources.into(),
            logging: config.logging.into(),
            monitoring: config.monitoring.into(),
            policy: config.policy.map(|p| p.into()),
            features: config.features.into(),
        }
    }
}

impl From<SerializableCleanroomConfig> for CleanroomConfig {
    fn from(config: SerializableCleanroomConfig) -> Self {
        Self {
            backend: config.backend.into(),
            security: config.security.into(),
            performance: config.performance.into(),
            resources: config.resources.into(),
            logging: config.logging.into(),
            monitoring: config.monitoring.into(),
            policy: config.policy.map(|p| p.into()),
            features: config.features.into(),
        }
    }
}
```

## Type Safety Features

### Phantom Types

```rust
/// Phantom type for type safety
pub struct Phantom<T> {
    _phantom: std::marker::PhantomData<T>,
}

impl<T> Phantom<T> {
    pub fn new() -> Self {
        Self {
            _phantom: std::marker::PhantomData,
        }
    }
}

/// Type-safe container ID
pub struct ContainerId<T> {
    id: String,
    _phantom: Phantom<T>,
}

impl<T> ContainerId<T> {
    pub fn new(id: String) -> Self {
        Self {
            id,
            _phantom: Phantom::new(),
        }
    }
    
    pub fn id(&self) -> &str {
        &self.id
    }
}

/// Type-safe command
pub struct Command<T> {
    command: String,
    _phantom: Phantom<T>,
}

impl<T> Command<T> {
    pub fn new(command: String) -> Self {
        Self {
            command,
            _phantom: Phantom::new(),
        }
    }
    
    pub fn command(&self) -> &str {
        &self.command
    }
}
```

### Type-Level Programming

```rust
/// Type-level boolean
pub trait Bool {
    const VALUE: bool;
}

/// True type
pub struct True;

impl Bool for True {
    const VALUE: bool = true;
}

/// False type
pub struct False;

impl Bool for False {
    const VALUE: bool = false;
}

/// Type-level conditional
pub trait If<B: Bool> {
    type Output;
}

impl<T, F> If<True> for (T, F) {
    type Output = T;
}

impl<T, F> If<False> for (T, F) {
    type Output = F;
}

/// Type-safe configuration based on features
pub struct FeatureConfig<const ENABLE_SECURITY: bool, const ENABLE_MONITORING: bool> {
    pub security: If<(SecurityConfig, ()), ENABLE_SECURITY>::Output,
    pub monitoring: If<(MonitoringConfig, ()), ENABLE_MONITORING>::Output,
}
```

## Summary

### Type System Features

1. **Type Safety**: Strong typing throughout the system
2. **Immutability**: Prefer immutable types where possible
3. **Serialization**: Support for serialization and deserialization
4. **Validation**: Built-in validation and constraints
5. **Documentation**: Comprehensive type documentation

### Type Categories

1. **Core Types**: Essential types for cleanroom operations
2. **Configuration Types**: Types for configuration and settings
3. **Result Types**: Types for operation results and outcomes
4. **Error Types**: Types for error handling and reporting
5. **Utility Types**: Supporting types and utilities
6. **Trait Types**: Trait definitions and implementations

### Design Principles

1. **Composition over Inheritance**: Prefer composition
2. **Type Safety**: Use type system to prevent errors
3. **Immutability**: Prefer immutable data structures
4. **Validation**: Validate data at type boundaries
5. **Serialization**: Support for data persistence

### Best Practices

1. **Use Strong Types**: Avoid primitive obsession
2. **Validate Early**: Validate data as soon as possible
3. **Use Traits**: Define behavior through traits
4. **Document Types**: Document all public types
5. **Test Types**: Test type behavior and validation

This type system provides a solid foundation for the Cleanroom Testing Framework, ensuring type safety, validation, and clear APIs throughout the system.
