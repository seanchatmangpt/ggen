# Configuration Reference

This document provides a comprehensive reference for all configuration options in the Cleanroom Testing Framework.

## Overview

### Configuration Structure

The Cleanroom configuration system is organized into the following sections:

1. **Backend Configuration**: Container backend settings
2. **Security Configuration**: Security policies and settings
3. **Performance Configuration**: Performance monitoring and optimization
4. **Resource Configuration**: Resource limits and allocation
5. **Logging Configuration**: Logging and audit settings
6. **Monitoring Configuration**: Monitoring and alerting settings
7. **Policy Configuration**: Security and execution policies
8. **Feature Flags**: Feature enablement and configuration

### Configuration Sources

Configuration can be loaded from multiple sources in order of precedence:

1. **Command Line Arguments**: Highest precedence
2. **Environment Variables**: Second highest precedence
3. **Configuration Files**: Third highest precedence
4. **Default Values**: Lowest precedence

## Main Configuration

### `CleanroomConfig`

The main configuration structure that contains all configuration options.

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
```

#### Default Configuration

```rust
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

## Backend Configuration

### `BackendConfig`

Configuration for the container backend.

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
    /// Backend connection pool
    pub connection_pool: ConnectionPoolConfig,
    /// Backend health check
    pub health_check: HealthCheckConfig,
}
```

#### Backend Types

```rust
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
```

#### Backend Credentials

```rust
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
    /// CA certificate
    pub ca_certificate: Option<PathBuf>,
}
```

#### Retry Configuration

```rust
/// Retry configuration
pub struct RetryConfig {
    /// Maximum number of retries
    pub max_retries: u32,
    /// Initial retry delay
    pub initial_delay: Duration,
    /// Maximum retry delay
    pub max_delay: Duration,
    /// Retry delay multiplier
    pub delay_multiplier: f64,
    /// Retryable error types
    pub retryable_errors: Vec<String>,
}
```

#### Connection Pool Configuration

```rust
/// Connection pool configuration
pub struct ConnectionPoolConfig {
    /// Maximum number of connections
    pub max_connections: u32,
    /// Minimum number of connections
    pub min_connections: u32,
    /// Connection timeout
    pub connection_timeout: Duration,
    /// Idle timeout
    pub idle_timeout: Duration,
    /// Connection validation interval
    pub validation_interval: Duration,
}
```

#### Health Check Configuration

```rust
/// Health check configuration
pub struct HealthCheckConfig {
    /// Enable health checks
    pub enabled: bool,
    /// Health check interval
    pub interval: Duration,
    /// Health check timeout
    pub timeout: Duration,
    /// Health check retries
    pub retries: u32,
    /// Health check command
    pub command: Option<String>,
}
```

### Backend Configuration Examples

#### Docker Backend

```toml
[backend]
backend_type = "Docker"
endpoint = "unix:///var/run/docker.sock"
timeout = "30s"

[backend.retry_config]
max_retries = 3
initial_delay = "1s"
max_delay = "10s"
delay_multiplier = 2.0

[backend.connection_pool]
max_connections = 10
min_connections = 2
connection_timeout = "5s"
idle_timeout = "300s"

[backend.health_check]
enabled = true
interval = "30s"
timeout = "5s"
retries = 3
```

#### Podman Backend

```toml
[backend]
backend_type = "Podman"
endpoint = "unix:///run/podman/podman.sock"
timeout = "30s"

[backend.retry_config]
max_retries = 3
initial_delay = "1s"
max_delay = "10s"
delay_multiplier = 2.0
```

#### Kubernetes Backend

```toml
[backend]
backend_type = "Kubernetes"
endpoint = "https://kubernetes.default.svc"
timeout = "60s"

[backend.credentials]
token = "/var/run/secrets/kubernetes.io/serviceaccount/token"
ca_certificate = "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"

[backend.retry_config]
max_retries = 5
initial_delay = "2s"
max_delay = "30s"
delay_multiplier = 1.5
```

## Security Configuration

### `SecurityConfig`

Configuration for security features and policies.

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
    /// Audit configuration
    pub audit: AuditConfig,
}
```

#### Security Policy

```rust
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

#### Encryption Configuration

```rust
/// Encryption configuration
pub struct EncryptionConfig {
    /// Enable encryption
    pub enabled: bool,
    /// Encryption algorithm
    pub algorithm: EncryptionAlgorithm,
    /// Key size
    pub key_size: u32,
    /// Key derivation function
    pub key_derivation: KeyDerivationFunction,
    /// Initialization vector
    pub iv: Option<Vec<u8>>,
    /// Salt
    pub salt: Option<Vec<u8>>,
}
```

#### Authentication Configuration

```rust
/// Authentication configuration
pub struct AuthenticationConfig {
    /// Enable authentication
    pub enabled: bool,
    /// Authentication method
    pub method: AuthenticationMethod,
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

#### Authorization Configuration

```rust
/// Authorization configuration
pub struct AuthorizationConfig {
    /// Enable authorization
    pub enabled: bool,
    /// Authorization method
    pub method: AuthorizationMethod,
    /// User roles
    pub roles: Vec<String>,
    /// Permissions
    pub permissions: Vec<String>,
    /// Access control lists
    pub acls: Vec<AccessControlList>,
}
```

#### Audit Configuration

```rust
/// Audit configuration
pub struct AuditConfig {
    /// Enable audit logging
    pub enabled: bool,
    /// Audit log level
    pub level: LogLevel,
    /// Audit log format
    pub format: AuditFormat,
    /// Audit log destination
    pub destination: AuditDestination,
    /// Audit log retention
    pub retention: Duration,
    /// Audit log rotation
    pub rotation: AuditRotation,
}
```

### Security Configuration Examples

#### Basic Security

```toml
[security]
enable_security = true

[security.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443, 8080]
blocked_hosts = ["malicious.com", "phishing.com"]
allowed_paths = ["/tmp", "/var/tmp"]
blocked_paths = ["/etc", "/root"]
allowed_commands = ["echo", "cat", "ls", "pwd"]
blocked_commands = ["rm", "format", "shutdown"]
enable_audit_logging = true
audit_log_level = "Info"
```

#### Advanced Security

```toml
[security]
enable_security = true

[security.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443]
blocked_hosts = ["malicious.com"]
allowed_paths = ["/tmp"]
blocked_paths = ["/etc", "/root", "/home"]
allowed_commands = ["echo", "cat"]
blocked_commands = ["rm", "format", "shutdown", "reboot"]
enable_audit_logging = true
audit_log_level = "Debug"

[security.encryption]
enabled = true
algorithm = "AES-256-GCM"
key_size = 256
key_derivation = "PBKDF2"

[security.authentication]
enabled = true
method = "Token"
token = "/var/run/secrets/token"

[security.authorization]
enabled = true
method = "RBAC"
roles = ["user", "admin"]
permissions = ["read", "write", "execute"]

[security.audit]
enabled = true
level = "Info"
format = "JSON"
destination = "File"
retention = "30d"
```

## Performance Configuration

### `PerformanceConfig`

Configuration for performance monitoring and optimization.

```rust
/// Performance configuration
pub struct PerformanceConfig {
    /// Enable performance monitoring
    pub enable_monitoring: bool,
    /// Performance monitoring configuration
    pub monitoring_config: PerformanceMonitoringConfig,
    /// Performance thresholds
    pub thresholds: PerformanceThresholds,
    /// Performance optimization
    pub optimization: PerformanceOptimization,
    /// Performance profiling
    pub profiling: PerformanceProfiling,
}
```

#### Performance Monitoring Configuration

```rust
/// Performance monitoring configuration
pub struct PerformanceMonitoringConfig {
    /// Enable CPU monitoring
    pub enable_cpu_monitoring: bool,
    /// Enable memory monitoring
    pub enable_memory_monitoring: bool,
    /// Enable disk monitoring
    pub enable_disk_monitoring: bool,
    /// Enable network monitoring
    pub enable_network_monitoring: bool,
    /// Sampling interval
    pub sampling_interval: Duration,
    /// Sampling duration
    pub sampling_duration: Duration,
    /// Metrics retention
    pub metrics_retention: Duration,
    /// Metrics aggregation
    pub metrics_aggregation: MetricsAggregation,
}
```

#### Performance Thresholds

```rust
/// Performance thresholds
pub struct PerformanceThresholds {
    /// CPU usage threshold
    pub cpu_threshold: f64,
    /// Memory usage threshold
    pub memory_threshold: f64,
    /// Disk usage threshold
    pub disk_threshold: f64,
    /// Network usage threshold
    pub network_threshold: f64,
    /// Response time threshold
    pub response_time_threshold: Duration,
    /// Throughput threshold
    pub throughput_threshold: u64,
    /// Error rate threshold
    pub error_rate_threshold: f64,
}
```

#### Performance Optimization

```rust
/// Performance optimization
pub struct PerformanceOptimization {
    /// Enable optimization
    pub enabled: bool,
    /// Optimization level
    pub level: OptimizationLevel,
    /// Optimization targets
    pub targets: Vec<OptimizationTarget>,
    /// Optimization strategies
    pub strategies: Vec<OptimizationStrategy>,
    /// Optimization constraints
    pub constraints: Vec<OptimizationConstraint>,
}
```

#### Performance Profiling

```rust
/// Performance profiling
pub struct PerformanceProfiling {
    /// Enable profiling
    pub enabled: bool,
    /// Profiling mode
    pub mode: ProfilingMode,
    /// Profiling duration
    pub duration: Duration,
    /// Profiling interval
    pub interval: Duration,
    /// Profiling output
    pub output: ProfilingOutput,
    /// Profiling filters
    pub filters: Vec<String>,
}
```

### Performance Configuration Examples

#### Basic Performance Monitoring

```toml
[performance]
enable_monitoring = true

[performance.monitoring_config]
enable_cpu_monitoring = true
enable_memory_monitoring = true
enable_disk_monitoring = true
enable_network_monitoring = true
sampling_interval = "1s"
sampling_duration = "60s"
metrics_retention = "24h"

[performance.thresholds]
cpu_threshold = 80.0
memory_threshold = 80.0
disk_threshold = 80.0
network_threshold = 80.0
response_time_threshold = "5s"
throughput_threshold = 1000
error_rate_threshold = 0.01
```

#### Advanced Performance Monitoring

```toml
[performance]
enable_monitoring = true

[performance.monitoring_config]
enable_cpu_monitoring = true
enable_memory_monitoring = true
enable_disk_monitoring = true
enable_network_monitoring = true
sampling_interval = "100ms"
sampling_duration = "300s"
metrics_retention = "7d"
metrics_aggregation = "Average"

[performance.thresholds]
cpu_threshold = 70.0
memory_threshold = 75.0
disk_threshold = 85.0
network_threshold = 90.0
response_time_threshold = "3s"
throughput_threshold = 5000
error_rate_threshold = 0.005

[performance.optimization]
enabled = true
level = "Aggressive"
targets = ["CPU", "Memory", "Network"]
strategies = ["Caching", "Pooling", "Batching"]

[performance.profiling]
enabled = true
mode = "Continuous"
duration = "300s"
interval = "10s"
output = "File"
```

## Resource Configuration

### `ResourceConfig`

Configuration for resource management and allocation.

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
```

#### Resource Limits

```rust
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
    /// Maximum number of connections
    pub max_connections: u32,
    /// Maximum number of files
    pub max_files: u32,
    /// Maximum number of processes
    pub max_processes: u32,
}
```

#### Resource Allocation

```rust
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
    /// Allocation timeout
    pub allocation_timeout: Duration,
    /// Allocation retries
    pub allocation_retries: u32,
    /// Allocation backoff
    pub allocation_backoff: Duration,
}
```

#### Resource Monitoring

```rust
/// Resource monitoring
pub struct ResourceMonitoring {
    /// Enable monitoring
    pub enabled: bool,
    /// Monitoring interval
    pub interval: Duration,
    /// Monitoring timeout
    pub timeout: Duration,
    /// Monitoring retries
    pub retries: u32,
    /// Monitoring alerts
    pub alerts: Vec<ResourceAlert>,
    /// Monitoring metrics
    pub metrics: Vec<ResourceMetric>,
}
```

#### Resource Cleanup

```rust
/// Resource cleanup
pub struct ResourceCleanup {
    /// Enable cleanup
    pub enabled: bool,
    /// Cleanup interval
    pub interval: Duration,
    /// Cleanup timeout
    pub timeout: Duration,
    /// Cleanup retries
    pub retries: u32,
    /// Cleanup strategies
    pub strategies: Vec<CleanupStrategy>,
    /// Cleanup policies
    pub policies: Vec<CleanupPolicy>,
}
```

### Resource Configuration Examples

#### Basic Resource Limits

```toml
[resources]
limits.max_memory_mb = 1024
limits.max_cpu_percent = 80.0
limits.max_disk_mb = 2048
limits.max_network_mb = 100
limits.max_execution_time = "300s"
limits.max_containers = 50
limits.max_concurrent_operations = 100
limits.max_connections = 200
limits.max_files = 1000
limits.max_processes = 500
```

#### Advanced Resource Management

```toml
[resources]
limits.max_memory_mb = 2048
limits.max_cpu_percent = 70.0
limits.max_disk_mb = 4096
limits.max_network_mb = 200
limits.max_execution_time = "600s"
limits.max_containers = 100
limits.max_concurrent_operations = 200
limits.max_connections = 500
limits.max_files = 2000
limits.max_processes = 1000

[resources.allocation]
memory_strategy = "Dynamic"
cpu_strategy = "Static"
disk_strategy = "Shared"
network_strategy = "Exclusive"
allocation_timeout = "30s"
allocation_retries = 3
allocation_backoff = "5s"

[resources.monitoring]
enabled = true
interval = "1s"
timeout = "5s"
retries = 3

[resources.cleanup]
enabled = true
interval = "300s"
timeout = "60s"
retries = 3
```

## Logging Configuration

### `LoggingConfig`

Configuration for logging and audit settings.

```rust
/// Logging configuration
pub struct LoggingConfig {
    /// Enable logging
    pub enabled: bool,
    /// Log level
    pub level: LogLevel,
    /// Log format
    pub format: LogFormat,
    /// Log destination
    pub destination: LogDestination,
    /// Log rotation
    pub rotation: LogRotation,
    /// Log filtering
    pub filtering: LogFiltering,
    /// Log buffering
    pub buffering: LogBuffering,
}
```

#### Log Levels

```rust
/// Log level enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum LogLevel {
    /// Trace level
    Trace,
    /// Debug level
    Debug,
    /// Info level
    Info,
    /// Warn level
    Warn,
    /// Error level
    Error,
}
```

#### Log Formats

```rust
/// Log format enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum LogFormat {
    /// JSON format
    Json,
    /// Text format
    Text,
    /// Compact format
    Compact,
    /// Pretty format
    Pretty,
    /// Structured format
    Structured,
}
```

#### Log Destinations

```rust
/// Log destination enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum LogDestination {
    /// Standard output
    Stdout,
    /// Standard error
    Stderr,
    /// File output
    File(PathBuf),
    /// Network output
    Network(String),
    /// Custom output
    Custom(String),
}
```

#### Log Rotation

```rust
/// Log rotation configuration
pub struct LogRotation {
    /// Enable rotation
    pub enabled: bool,
    /// Rotation size
    pub size: u64,
    /// Rotation count
    pub count: u32,
    /// Rotation time
    pub time: Duration,
    /// Rotation strategy
    pub strategy: RotationStrategy,
}
```

#### Log Filtering

```rust
/// Log filtering configuration
pub struct LogFiltering {
    /// Enable filtering
    pub enabled: bool,
    /// Include filters
    pub include_filters: Vec<String>,
    /// Exclude filters
    pub exclude_filters: Vec<String>,
    /// Filter patterns
    pub filter_patterns: Vec<String>,
    /// Filter rules
    pub filter_rules: Vec<FilterRule>,
}
```

#### Log Buffering

```rust
/// Log buffering configuration
pub struct LogBuffering {
    /// Enable buffering
    pub enabled: bool,
    /// Buffer size
    pub buffer_size: usize,
    /// Buffer timeout
    pub buffer_timeout: Duration,
    /// Buffer strategy
    pub buffer_strategy: BufferStrategy,
}
```

### Logging Configuration Examples

#### Basic Logging

```toml
[logging]
enabled = true
level = "Info"
format = "Text"
destination = "Stdout"

[logging.rotation]
enabled = true
size = 10485760  # 10MB
count = 5
time = "24h"
```

#### Advanced Logging

```toml
[logging]
enabled = true
level = "Debug"
format = "JSON"
destination = "File"

[logging.rotation]
enabled = true
size = 52428800  # 50MB
count = 10
time = "7d"
strategy = "TimeBased"

[logging.filtering]
enabled = true
include_filters = ["cleanroom", "container", "execution"]
exclude_filters = ["debug", "trace"]
filter_patterns = ["*error*", "*warning*"]

[logging.buffering]
enabled = true
buffer_size = 8192
buffer_timeout = "1s"
buffer_strategy = "Async"
```

## Monitoring Configuration

### `MonitoringConfig`

Configuration for monitoring and alerting.

```rust
/// Monitoring configuration
pub struct MonitoringConfig {
    /// Enable monitoring
    pub enabled: bool,
    /// Monitoring interval
    pub interval: Duration,
    /// Monitoring timeout
    pub timeout: Duration,
    /// Monitoring retries
    pub retries: u32,
    /// Monitoring metrics
    pub metrics: MetricsConfig,
    /// Monitoring alerts
    pub alerts: AlertsConfig,
    /// Monitoring dashboards
    pub dashboards: DashboardsConfig,
}
```

#### Metrics Configuration

```rust
/// Metrics configuration
pub struct MetricsConfig {
    /// Enable metrics collection
    pub enabled: bool,
    /// Metrics collection interval
    pub collection_interval: Duration,
    /// Metrics retention period
    pub retention_period: Duration,
    /// Metrics aggregation
    pub aggregation: MetricsAggregation,
    /// Metrics export
    pub export: MetricsExport,
    /// Metrics storage
    pub storage: MetricsStorage,
}
```

#### Alerts Configuration

```rust
/// Alerts configuration
pub struct AlertsConfig {
    /// Enable alerts
    pub enabled: bool,
    /// Alert rules
    pub rules: Vec<AlertRule>,
    /// Alert channels
    pub channels: Vec<AlertChannel>,
    /// Alert escalation
    pub escalation: AlertEscalation,
    /// Alert suppression
    pub suppression: AlertSuppression,
}
```

#### Dashboards Configuration

```rust
/// Dashboards configuration
pub struct DashboardsConfig {
    /// Enable dashboards
    pub enabled: bool,
    /// Dashboard templates
    pub templates: Vec<DashboardTemplate>,
    /// Dashboard refresh interval
    pub refresh_interval: Duration,
    /// Dashboard export
    pub export: DashboardExport,
}
```

### Monitoring Configuration Examples

#### Basic Monitoring

```toml
[monitoring]
enabled = true
interval = "30s"
timeout = "10s"
retries = 3

[monitoring.metrics]
enabled = true
collection_interval = "1s"
retention_period = "7d"
aggregation = "Average"

[monitoring.alerts]
enabled = true
```

#### Advanced Monitoring

```toml
[monitoring]
enabled = true
interval = "10s"
timeout = "5s"
retries = 5

[monitoring.metrics]
enabled = true
collection_interval = "100ms"
retention_period = "30d"
aggregation = "Percentile"
export = "Prometheus"
storage = "InfluxDB"

[monitoring.alerts]
enabled = true
rules = [
    { name = "High CPU", condition = "cpu > 80%", duration = "5m" },
    { name = "High Memory", condition = "memory > 85%", duration = "3m" },
    { name = "Low Disk", condition = "disk < 10%", duration = "1m" }
]
channels = ["email", "slack", "webhook"]

[monitoring.dashboards]
enabled = true
refresh_interval = "30s"
export = "Grafana"
```

## Policy Configuration

### `Policy`

Configuration for security and execution policies.

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
```

#### Security Policy

```rust
/// Security policy configuration
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

#### Resource Policy

```rust
/// Resource policy configuration
pub struct ResourcePolicy {
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
```

#### Execution Policy

```rust
/// Execution policy configuration
pub struct ExecutionPolicy {
    /// Enable deterministic execution
    pub enable_deterministic_execution: bool,
    /// Random seed for deterministic execution
    pub random_seed: Option<u64>,
    /// Enable time mocking
    pub enable_time_mocking: bool,
    /// Mocked time offset
    pub mocked_time_offset: Option<Duration>,
    /// Enable network mocking
    pub enable_network_mocking: bool,
    /// Mocked network latency
    pub mocked_network_latency: Option<Duration>,
    /// Enable filesystem mocking
    pub enable_filesystem_mocking: bool,
    /// Mocked filesystem behavior
    pub mocked_filesystem_behavior: Option<FilesystemMockBehavior>,
}
```

#### Compliance Policy

```rust
/// Compliance policy configuration
pub struct CompliancePolicy {
    /// Enable GDPR compliance
    pub enable_gdpr_compliance: bool,
    /// Enable HIPAA compliance
    pub enable_hipaa_compliance: bool,
    /// Enable SOX compliance
    pub enable_sox_compliance: bool,
    /// Data retention period
    pub data_retention_period: Duration,
    /// Enable data encryption
    pub enable_data_encryption: bool,
    /// Encryption algorithm
    pub encryption_algorithm: Option<EncryptionAlgorithm>,
    /// Enable data redaction
    pub enable_data_redaction: bool,
    /// Redaction patterns
    pub redaction_patterns: Vec<RedactionPattern>,
}
```

### Policy Configuration Examples

#### Basic Security Policy

```toml
[policy.security]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443]
blocked_hosts = ["malicious.com"]
allowed_paths = ["/tmp"]
blocked_paths = ["/etc", "/root"]
allowed_commands = ["echo", "cat", "ls"]
blocked_commands = ["rm", "format", "shutdown"]
enable_audit_logging = true
audit_log_level = "Info"
```

#### Advanced Security Policy

```toml
[policy.security]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443, 8080, 8443]
blocked_hosts = ["malicious.com", "phishing.com", "spam.com"]
allowed_paths = ["/tmp", "/var/tmp", "/opt/cleanroom"]
blocked_paths = ["/etc", "/root", "/home", "/var/log"]
allowed_commands = ["echo", "cat", "ls", "pwd", "whoami"]
blocked_commands = ["rm", "format", "shutdown", "reboot", "halt"]
enable_audit_logging = true
audit_log_level = "Debug"

[policy.resources]
max_memory_mb = 1024
max_cpu_percent = 80.0
max_disk_mb = 2048
max_network_mb = 100
max_execution_time = "300s"
max_containers = 50
max_concurrent_operations = 100

[policy.execution]
enable_deterministic_execution = true
random_seed = 12345
enable_time_mocking = false
enable_network_mocking = false
enable_filesystem_mocking = false

[policy.compliance]
enable_gdpr_compliance = true
enable_hipaa_compliance = false
enable_sox_compliance = false
data_retention_period = "30d"
enable_data_encryption = true
encryption_algorithm = "AES-256-GCM"
enable_data_redaction = true
```

## Feature Flags

### `FeatureFlags`

Configuration for feature enablement and configuration.

```rust
/// Feature flags
pub struct FeatureFlags {
    /// Enable coverage tracking
    pub enable_coverage: bool,
    /// Enable snapshot testing
    pub enable_snapshots: bool,
    /// Enable tracing
    pub enable_tracing: bool,
    /// Enable services
    pub enable_services: bool,
    /// Enable determinism
    pub enable_determinism: bool,
    /// Enable redaction
    pub enable_redaction: bool,
    /// Enable attestation
    pub enable_attestation: bool,
    /// Enable limits
    pub enable_limits: bool,
    /// Enable skip functionality
    pub enable_skip: bool,
    /// Enable artifacts
    pub enable_artifacts: bool,
}
```

### Feature Flags Examples

#### Basic Features

```toml
[features]
enable_coverage = true
enable_snapshots = true
enable_tracing = true
enable_services = false
enable_determinism = false
enable_redaction = false
enable_attestation = false
enable_limits = true
enable_skip = true
enable_artifacts = true
```

#### Advanced Features

```toml
[features]
enable_coverage = true
enable_snapshots = true
enable_tracing = true
enable_services = true
enable_determinism = true
enable_redaction = true
enable_attestation = true
enable_limits = true
enable_skip = true
enable_artifacts = true
```

## Environment Variables

### Environment Variable Mapping

Configuration can be overridden using environment variables:

```bash
# Backend configuration
export CLEANROOM_BACKEND_TYPE=docker
export CLEANROOM_BACKEND_ENDPOINT=unix:///var/run/docker.sock
export CLEANROOM_BACKEND_TIMEOUT=30s

# Security configuration
export CLEANROOM_ENABLE_NETWORK_ISOLATION=true
export CLEANROOM_ENABLE_FILESYSTEM_ISOLATION=true
export CLEANROOM_ENABLE_PROCESS_ISOLATION=true
export CLEANROOM_ALLOWED_PORTS=80,443,8080
export CLEANROOM_BLOCKED_HOSTS=malicious.com,phishing.com

# Performance configuration
export CLEANROOM_ENABLE_PERFORMANCE_MONITORING=true
export CLEANROOM_SAMPLING_INTERVAL=1s
export CLEANROOM_CPU_THRESHOLD=80.0
export CLEANROOM_MEMORY_THRESHOLD=80.0

# Resource configuration
export CLEANROOM_MAX_MEMORY_MB=1024
export CLEANROOM_MAX_CPU_PERCENT=80.0
export CLEANROOM_MAX_DISK_MB=2048
export CLEANROOM_MAX_NETWORK_MB=100
export CLEANROOM_MAX_EXECUTION_TIME=300s
export CLEANROOM_MAX_CONTAINERS=50

# Logging configuration
export CLEANROOM_LOG_LEVEL=info
export CLEANROOM_LOG_FORMAT=text
export CLEANROOM_LOG_DESTINATION=stdout

# Monitoring configuration
export CLEANROOM_ENABLE_MONITORING=true
export CLEANROOM_MONITORING_INTERVAL=30s
export CLEANROOM_MONITORING_TIMEOUT=10s

# Feature flags
export CLEANROOM_ENABLE_COVERAGE=true
export CLEANROOM_ENABLE_SNAPSHOTS=true
export CLEANROOM_ENABLE_TRACING=true
export CLEANROOM_ENABLE_SERVICES=false
export CLEANROOM_ENABLE_DETERMINISM=false
```

## Configuration Validation

### Validation Rules

Configuration is validated using the following rules:

```rust
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

## Configuration Loading

### Configuration Loader

```rust
/// Configuration loader
pub struct ConfigLoader {
    /// Configuration sources
    sources: Vec<ConfigSource>,
    /// Configuration validators
    validators: Vec<Box<dyn ConfigValidator>>,
}

impl ConfigLoader {
    /// Load configuration from all sources
    pub fn load(&self) -> Result<CleanroomConfig, ConfigError> {
        let mut config = CleanroomConfig::default();
        
        // Load from each source in order
        for source in &self.sources {
            let source_config = source.load()?;
            config = self.merge_config(config, source_config)?;
        }
        
        // Validate configuration
        for validator in &self.validators {
            validator.validate(&config)?;
        }
        
        Ok(config)
    }
    
    /// Merge configurations
    fn merge_config(&self, base: CleanroomConfig, overlay: CleanroomConfig) -> Result<CleanroomConfig, ConfigError> {
        // Implementation for merging configurations
        // This would use a deep merge strategy
        todo!()
    }
}
```

### Configuration Sources

```rust
/// Configuration source trait
pub trait ConfigSource {
    /// Load configuration from source
    fn load(&self) -> Result<CleanroomConfig, ConfigError>;
    
    /// Get source name
    fn name(&self) -> &str;
    
    /// Get source priority
    fn priority(&self) -> u32;
}

/// File configuration source
pub struct FileConfigSource {
    /// File path
    pub path: PathBuf,
    /// File format
    pub format: ConfigFormat,
}

impl ConfigSource for FileConfigSource {
    fn load(&self) -> Result<CleanroomConfig, ConfigError> {
        let content = std::fs::read_to_string(&self.path)?;
        match self.format {
            ConfigFormat::Toml => toml::from_str(&content).map_err(ConfigError::from),
            ConfigFormat::Json => serde_json::from_str(&content).map_err(ConfigError::from),
            ConfigFormat::Yaml => serde_yaml::from_str(&content).map_err(ConfigError::from),
        }
    }
    
    fn name(&self) -> &str {
        "file"
    }
    
    fn priority(&self) -> u32 {
        100
    }
}

/// Environment configuration source
pub struct EnvConfigSource {
    /// Environment variable prefix
    pub prefix: String,
}

impl ConfigSource for EnvConfigSource {
    fn load(&self) -> Result<CleanroomConfig, ConfigError> {
        // Implementation for loading from environment variables
        todo!()
    }
    
    fn name(&self) -> &str {
        "environment"
    }
    
    fn priority(&self) -> u32 {
        200
    }
}
```

## Configuration Examples

### Complete Configuration Example

```toml
# Cleanroom Testing Framework Configuration

# Backend Configuration
[backend]
backend_type = "Docker"
endpoint = "unix:///var/run/docker.sock"
timeout = "30s"

[backend.retry_config]
max_retries = 3
initial_delay = "1s"
max_delay = "10s"
delay_multiplier = 2.0

[backend.connection_pool]
max_connections = 10
min_connections = 2
connection_timeout = "5s"
idle_timeout = "300s"

[backend.health_check]
enabled = true
interval = "30s"
timeout = "5s"
retries = 3

# Security Configuration
[security]
enable_security = true

[security.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443, 8080]
blocked_hosts = ["malicious.com", "phishing.com"]
allowed_paths = ["/tmp", "/var/tmp"]
blocked_paths = ["/etc", "/root"]
allowed_commands = ["echo", "cat", "ls", "pwd"]
blocked_commands = ["rm", "format", "shutdown"]
enable_audit_logging = true
audit_log_level = "Info"

[security.encryption]
enabled = true
algorithm = "AES-256-GCM"
key_size = 256
key_derivation = "PBKDF2"

[security.authentication]
enabled = true
method = "Token"
token = "/var/run/secrets/token"

[security.authorization]
enabled = true
method = "RBAC"
roles = ["user", "admin"]
permissions = ["read", "write", "execute"]

[security.audit]
enabled = true
level = "Info"
format = "JSON"
destination = "File"
retention = "30d"

# Performance Configuration
[performance]
enable_monitoring = true

[performance.monitoring_config]
enable_cpu_monitoring = true
enable_memory_monitoring = true
enable_disk_monitoring = true
enable_network_monitoring = true
sampling_interval = "1s"
sampling_duration = "60s"
metrics_retention = "24h"
metrics_aggregation = "Average"

[performance.thresholds]
cpu_threshold = 80.0
memory_threshold = 80.0
disk_threshold = 80.0
network_threshold = 80.0
response_time_threshold = "5s"
throughput_threshold = 1000
error_rate_threshold = 0.01

[performance.optimization]
enabled = true
level = "Moderate"
targets = ["CPU", "Memory", "Network"]
strategies = ["Caching", "Pooling"]

[performance.profiling]
enabled = false
mode = "OnDemand"
duration = "300s"
interval = "10s"
output = "File"

# Resource Configuration
[resources]
limits.max_memory_mb = 1024
limits.max_cpu_percent = 80.0
limits.max_disk_mb = 2048
limits.max_network_mb = 100
limits.max_execution_time = "300s"
limits.max_containers = 50
limits.max_concurrent_operations = 100
limits.max_connections = 200
limits.max_files = 1000
limits.max_processes = 500

[resources.allocation]
memory_strategy = "Dynamic"
cpu_strategy = "Static"
disk_strategy = "Shared"
network_strategy = "Exclusive"
allocation_timeout = "30s"
allocation_retries = 3
allocation_backoff = "5s"

[resources.monitoring]
enabled = true
interval = "1s"
timeout = "5s"
retries = 3

[resources.cleanup]
enabled = true
interval = "300s"
timeout = "60s"
retries = 3

# Logging Configuration
[logging]
enabled = true
level = "Info"
format = "Text"
destination = "Stdout"

[logging.rotation]
enabled = true
size = 10485760  # 10MB
count = 5
time = "24h"
strategy = "SizeBased"

[logging.filtering]
enabled = true
include_filters = ["cleanroom", "container", "execution"]
exclude_filters = ["debug", "trace"]

[logging.buffering]
enabled = true
buffer_size = 8192
buffer_timeout = "1s"
buffer_strategy = "Async"

# Monitoring Configuration
[monitoring]
enabled = true
interval = "30s"
timeout = "10s"
retries = 3

[monitoring.metrics]
enabled = true
collection_interval = "1s"
retention_period = "7d"
aggregation = "Average"
export = "Prometheus"
storage = "InMemory"

[monitoring.alerts]
enabled = true
rules = [
    { name = "High CPU", condition = "cpu > 80%", duration = "5m" },
    { name = "High Memory", condition = "memory > 85%", duration = "3m" },
    { name = "Low Disk", condition = "disk < 10%", duration = "1m" }
]
channels = ["email", "slack"]

[monitoring.dashboards]
enabled = true
refresh_interval = "30s"
export = "Grafana"

# Policy Configuration
[policy.security]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [80, 443]
blocked_hosts = ["malicious.com"]
allowed_paths = ["/tmp"]
blocked_paths = ["/etc", "/root"]
allowed_commands = ["echo", "cat", "ls"]
blocked_commands = ["rm", "format", "shutdown"]
enable_audit_logging = true
audit_log_level = "Info"

[policy.resources]
max_memory_mb = 1024
max_cpu_percent = 80.0
max_disk_mb = 2048
max_network_mb = 100
max_execution_time = "300s"
max_containers = 50
max_concurrent_operations = 100

[policy.execution]
enable_deterministic_execution = true
random_seed = 12345
enable_time_mocking = false
enable_network_mocking = false
enable_filesystem_mocking = false

[policy.compliance]
enable_gdpr_compliance = true
enable_hipaa_compliance = false
enable_sox_compliance = false
data_retention_period = "30d"
enable_data_encryption = true
encryption_algorithm = "AES-256-GCM"
enable_data_redaction = true

# Feature Flags
[features]
enable_coverage = true
enable_snapshots = true
enable_tracing = true
enable_services = false
enable_determinism = false
enable_redaction = false
enable_attestation = false
enable_limits = true
enable_skip = true
enable_artifacts = true
```

## Summary

### Configuration Features

1. **Hierarchical Structure**: Clear configuration hierarchy
2. **Multiple Sources**: Support for multiple configuration sources
3. **Validation**: Built-in configuration validation
4. **Environment Override**: Environment variable support
5. **Type Safety**: Strong typing for configuration

### Configuration Categories

1. **Backend Configuration**: Container backend settings
2. **Security Configuration**: Security policies and settings
3. **Performance Configuration**: Performance monitoring and optimization
4. **Resource Configuration**: Resource limits and allocation
5. **Logging Configuration**: Logging and audit settings
6. **Monitoring Configuration**: Monitoring and alerting settings
7. **Policy Configuration**: Security and execution policies
8. **Feature Flags**: Feature enablement and configuration

### Configuration Best Practices

1. **Use Defaults**: Start with default configuration
2. **Validate Early**: Validate configuration at startup
3. **Document Settings**: Document all configuration options
4. **Test Configuration**: Test configuration changes
5. **Monitor Configuration**: Monitor configuration changes

### Configuration Security

1. **Sensitive Data**: Protect sensitive configuration data
2. **Access Control**: Control access to configuration
3. **Encryption**: Encrypt sensitive configuration
4. **Audit Logging**: Log configuration changes
5. **Validation**: Validate configuration security

This configuration reference provides comprehensive documentation for all configuration options in the Cleanroom Testing Framework, enabling users to effectively configure and customize the system.
