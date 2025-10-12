# Module Reference

This document provides a comprehensive reference for all modules in the Cleanroom Testing Framework.

## Core Modules

### `cleanroom` (Root Module)

The main module that re-exports all public APIs and provides top-level functions.

#### Public API

```rust
//! Cleanroom: Hermetic, deterministic execution environment for testing and validation.
//!
//! Provides a unified API for running commands in isolated environments with
//! deterministic results, security policies, and backend abstraction.

// Re-exports
pub use cleanroom::CleanroomEnvironment;
pub use config::CleanroomConfig;
pub use policy::Policy;
pub use scenario::{RunResult, StepResult, Scenario};
pub use error::CleanroomError;

// Top-level functions
pub fn run<I, S>(args: I) -> Result<RunResult>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>;

pub fn run_with_policy<I, S>(args: I, policy: &Policy) -> Result<RunResult>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>;
```

#### Usage Example

```rust
use cleanroom::{run, run_with_policy, Policy};

// Simple execution
let result = run(["echo", "Hello World"])?;
println!("Output: {}", result.stdout);

// Execution with policy
let policy = Policy::default();
let result = run_with_policy(["python", "--version"], &policy)?;
println!("Python version: {}", result.stdout);
```

### `cleanroom::cleanroom`

Core implementation of the Cleanroom environment and orchestration.

#### Public Types

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
```

#### Public Methods

```rust
impl CleanroomEnvironment {
    /// Create a new cleanroom environment
    pub async fn new(config: CleanroomConfig) -> Result<Self, CleanroomError>;
    
    /// Execute a single test command
    pub async fn execute_test(&self, command: &str) -> Result<RunResult, CleanroomError>;
    
    /// Execute a scenario with multiple steps
    pub async fn execute_scenario(&self, scenario: &Scenario) -> Result<ScenarioResult, CleanroomError>;
    
    /// Execute multiple tests in batch
    pub async fn execute_batch_tests(&self, commands: Vec<String>) -> Result<Vec<RunResult>, CleanroomError>;
    
    /// Get current performance metrics
    pub fn get_metrics(&self) -> CleanroomMetrics;
    
    /// Get current resource usage
    pub async fn get_resource_usage(&self) -> Result<ResourceUsage, CleanroomError>;
    
    /// Start a container
    pub async fn start_container(&self, image: &str) -> Result<Container, CleanroomError>;
    
    /// Stop a container
    pub async fn stop_container(&self, container_id: &str) -> Result<(), CleanroomError>;
    
    /// Execute command in specific container
    pub async fn execute_in_container(&self, container_id: &str, command: &str) -> Result<RunResult, CleanroomError>;
    
    /// Clean up all resources
    pub async fn cleanup(&self) -> Result<(), CleanroomError>;
}
```

#### Usage Example

```rust
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute a test
    let result = environment.execute_test("echo 'Hello World'").await?;
    println!("Output: {}", result.stdout);
    
    // Get metrics
    let metrics = environment.get_metrics();
    println!("Tests executed: {}", metrics.tests_executed);
    
    // Clean up
    environment.cleanup().await?;
    
    Ok(())
}
```

### `cleanroom::backend`

Backend abstraction layer for container operations.

#### Public Types

```rust
/// Command structure for backend execution
pub struct Cmd {
    /// Binary to execute
    pub bin: String,
    /// Command arguments
    pub args: Vec<String>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// Working directory
    pub work_dir: Option<PathBuf>,
    /// Timeout for execution
    pub timeout: Option<Duration>,
    /// Resource limits
    pub limits: Option<ResourceLimits>,
}

/// Result of backend command execution
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
```

#### Usage Example

```rust
use cleanroom::backend::{Backend, Cmd, RunResult};

async fn execute_with_backend(backend: &dyn Backend) -> Result<(), BackendError> {
    // Start container
    let container = backend.start_container("alpine:latest", &Default::default()).await?;
    
    // Execute command
    let cmd = Cmd {
        bin: "echo".to_string(),
        args: vec!["Hello World".to_string()],
        env: HashMap::new(),
        work_dir: None,
        timeout: Some(Duration::from_secs(30)),
        limits: None,
    };
    
    let result = backend.execute_command(&container.id, &cmd).await?;
    println!("Output: {}", result.stdout);
    
    // Stop container
    backend.stop_container(&container.id).await?;
    
    Ok(())
}
```

### `cleanroom::policy`

Policy system for security, resource, and execution constraints.

#### Public Types

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

#### Usage Example

```rust
use cleanroom::policy::{Policy, SecurityPolicy, ResourcePolicy};

let security = SecurityPolicy {
    enable_network_isolation: true,
    enable_filesystem_isolation: true,
    enable_process_isolation: true,
    allowed_ports: vec![80, 443],
    blocked_hosts: vec!["malicious.com".to_string()],
    allowed_paths: vec![PathBuf::from("/tmp")],
    blocked_paths: vec![PathBuf::from("/etc")],
    allowed_commands: vec!["echo".to_string(), "cat".to_string()],
    blocked_commands: vec!["rm".to_string(), "format".to_string()],
    enable_audit_logging: true,
    audit_log_level: LogLevel::Info,
};

let resources = ResourcePolicy {
    max_memory_mb: 1024,
    max_cpu_percent: 80.0,
    max_disk_mb: 2048,
    max_network_mb: 100,
    max_execution_time: Duration::from_secs(300),
    max_containers: 50,
    max_concurrent_operations: 100,
};

let policy = Policy {
    security,
    resources,
    execution: ExecutionPolicy::default(),
    compliance: CompliancePolicy::default(),
};
```

### `cleanroom::scenario`

Scenario execution system for multi-step workflows.

#### Public Types

```rust
/// Scenario definition
pub struct Scenario {
    /// Scenario name
    pub name: String,
    /// Scenario steps
    pub steps: Vec<Step>,
    /// Scenario configuration
    pub config: ScenarioConfig,
    /// Scenario metadata
    pub metadata: HashMap<String, String>,
}

/// Individual step in a scenario
pub struct Step {
    /// Step name
    pub name: String,
    /// Command to execute
    pub command: String,
    /// Step configuration
    pub config: StepConfig,
    /// Step dependencies
    pub dependencies: Vec<String>,
    /// Step metadata
    pub metadata: HashMap<String, String>,
}

/// Scenario execution result
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

/// Individual step result
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

/// Scenario configuration
pub struct ScenarioConfig {
    /// Maximum execution time
    pub max_execution_time: Duration,
    /// Retry failed steps
    pub retry_failed_steps: bool,
    /// Maximum retries
    pub max_retries: u32,
    /// Stop on first failure
    pub stop_on_first_failure: bool,
    /// Parallel execution
    pub parallel_execution: bool,
    /// Maximum parallel steps
    pub max_parallel_steps: u32,
    /// Enable step dependencies
    pub enable_step_dependencies: bool,
    /// Enable step rollback
    pub enable_step_rollback: bool,
}

/// Step configuration
pub struct StepConfig {
    /// Step timeout
    pub timeout: Option<Duration>,
    /// Retry on failure
    pub retry_on_failure: bool,
    /// Maximum retries
    pub max_retries: u32,
    /// Retry delay
    pub retry_delay: Duration,
    /// Ignore failures
    pub ignore_failures: bool,
    /// Required for success
    pub required_for_success: bool,
    /// Enable rollback
    pub enable_rollback: bool,
    /// Rollback command
    pub rollback_command: Option<String>,
}
```

#### Usage Example

```rust
use cleanroom::scenario::{Scenario, Step, ScenarioConfig, StepConfig};

let scenario = Scenario::new("python_test")
    .add_step(Step::new("install_deps", "pip install requests")
        .with_config(StepConfig {
            timeout: Some(Duration::from_secs(60)),
            retry_on_failure: true,
            max_retries: 3,
            retry_delay: Duration::from_secs(5),
            ignore_failures: false,
            required_for_success: true,
            enable_rollback: true,
            rollback_command: Some("pip uninstall requests -y".to_string()),
        }))
    .add_step(Step::new("run_test", "python test.py")
        .with_config(StepConfig {
            timeout: Some(Duration::from_secs(120)),
            retry_on_failure: false,
            max_retries: 1,
            retry_delay: Duration::from_secs(0),
            ignore_failures: false,
            required_for_success: true,
            enable_rollback: false,
            rollback_command: None,
        }))
    .add_step(Step::new("cleanup", "pip uninstall requests -y")
        .with_config(StepConfig {
            timeout: Some(Duration::from_secs(30)),
            retry_on_failure: false,
            max_retries: 1,
            retry_delay: Duration::from_secs(0),
            ignore_failures: true,
            required_for_success: false,
            enable_rollback: false,
            rollback_command: None,
        }))
    .with_config(ScenarioConfig {
        max_execution_time: Duration::from_secs(600),
        retry_failed_steps: true,
        max_retries: 3,
        stop_on_first_failure: false,
        parallel_execution: false,
        max_parallel_steps: 1,
        enable_step_dependencies: true,
        enable_step_rollback: true,
    });
```

### `cleanroom::error`

Comprehensive error handling system.

#### Public Types

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

/// Backend-specific errors
#[derive(Debug, thiserror::Error)]
pub enum BackendError {
    /// Backend connection failed
    #[error("Backend connection failed: {0}")]
    ConnectionFailed(String),
    
    /// Backend operation failed
    #[error("Backend operation failed: {0}")]
    OperationFailed(String),
    
    /// Backend timeout
    #[error("Backend timeout: {0}")]
    Timeout(String),
    
    /// Backend authentication failed
    #[error("Backend authentication failed: {0}")]
    AuthenticationFailed(String),
    
    /// Backend authorization failed
    #[error("Backend authorization failed: {0}")]
    AuthorizationFailed(String),
    
    /// Backend not available
    #[error("Backend not available: {0}")]
    NotAvailable(String),
}

/// Policy-specific errors
#[derive(Debug, thiserror::Error)]
pub enum PolicyError {
    /// Policy validation failed
    #[error("Policy validation failed: {0}")]
    ValidationFailed(String),
    
    /// Policy enforcement failed
    #[error("Policy enforcement failed: {0}")]
    EnforcementFailed(String),
    
    /// Policy violation detected
    #[error("Policy violation detected: {0}")]
    ViolationDetected(String),
    
    /// Policy not found
    #[error("Policy not found: {0}")]
    NotFound(String),
    
    /// Policy conflict
    #[error("Policy conflict: {0}")]
    Conflict(String),
}

/// Configuration-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ConfigurationError {
    /// Configuration validation failed
    #[error("Configuration validation failed: {0}")]
    ValidationFailed(String),
    
    /// Configuration not found
    #[error("Configuration not found: {0}")]
    NotFound(String),
    
    /// Configuration format invalid
    #[error("Configuration format invalid: {0}")]
    InvalidFormat(String),
    
    /// Configuration value invalid
    #[error("Configuration value invalid: {0}")]
    InvalidValue(String),
    
    /// Configuration conflict
    #[error("Configuration conflict: {0}")]
    Conflict(String),
}

/// Execution-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    /// Command execution failed
    #[error("Command execution failed: {0}")]
    CommandFailed(String),
    
    /// Command timeout
    #[error("Command timeout: {0}")]
    Timeout(String),
    
    /// Command not found
    #[error("Command not found: {0}")]
    CommandNotFound(String),
    
    /// Permission denied
    #[error("Permission denied: {0}")]
    PermissionDenied(String),
    
    /// Resource unavailable
    #[error("Resource unavailable: {0}")]
    ResourceUnavailable(String),
    
    /// Execution interrupted
    #[error("Execution interrupted: {0}")]
    Interrupted(String),
}
```

#### Usage Example

```rust
use cleanroom::{CleanroomError, CleanroomEnvironment, CleanroomConfig};

async fn handle_errors() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    match environment.execute_test("invalid_command").await {
        Ok(result) => {
            println!("Command executed successfully");
            println!("Exit code: {}", result.exit_code);
            println!("Output: {}", result.stdout);
        }
        Err(CleanroomError::Container(err)) => {
            match err {
                ContainerError::StartupFailed(msg) => {
                    eprintln!("Container startup failed: {}", msg);
                    // Handle container startup failure
                }
                ContainerError::ExecutionFailed(msg) => {
                    eprintln!("Container execution failed: {}", msg);
                    // Handle container execution failure
                }
                ContainerError::CleanupFailed(msg) => {
                    eprintln!("Container cleanup failed: {}", msg);
                    // Handle container cleanup failure
                }
                _ => {
                    eprintln!("Container error: {}", err);
                }
            }
        }
        Err(CleanroomError::Backend(err)) => {
            match err {
                BackendError::ConnectionFailed(msg) => {
                    eprintln!("Backend connection failed: {}", msg);
                    // Handle backend connection failure
                }
                BackendError::OperationFailed(msg) => {
                    eprintln!("Backend operation failed: {}", msg);
                    // Handle backend operation failure
                }
                _ => {
                    eprintln!("Backend error: {}", err);
                }
            }
        }
        Err(CleanroomError::Policy(err)) => {
            match err {
                PolicyError::ViolationDetected(msg) => {
                    eprintln!("Policy violation detected: {}", msg);
                    // Handle policy violation
                }
                PolicyError::EnforcementFailed(msg) => {
                    eprintln!("Policy enforcement failed: {}", msg);
                    // Handle policy enforcement failure
                }
                _ => {
                    eprintln!("Policy error: {}", err);
                }
            }
        }
        Err(err) => {
            eprintln!("Unexpected error: {}", err);
        }
    }
    
    Ok(())
}
```

## Advanced Modules

### `cleanroom::containers`

Container lifecycle management and operations.

#### Public Types

```rust
/// Container information
pub struct Container {
    /// Container ID
    pub id: String,
    /// Container image
    pub image: String,
    /// Container status
    pub status: ContainerStatus,
    /// Container configuration
    pub config: ContainerConfig,
    /// Container metadata
    pub metadata: HashMap<String, String>,
}

/// Container status
#[derive(Debug, Clone, PartialEq)]
pub enum ContainerStatus {
    /// Container is starting
    Starting,
    /// Container is running
    Running,
    /// Container is stopping
    Stopping,
    /// Container is stopped
    Stopped,
    /// Container is paused
    Paused,
    /// Container is restarting
    Restarting,
    /// Container is dead
    Dead,
    /// Container status unknown
    Unknown,
}

/// Container configuration
pub struct ContainerConfig {
    /// Container image
    pub image: String,
    /// Container command
    pub command: Option<Vec<String>>,
    /// Container environment variables
    pub env: HashMap<String, String>,
    /// Container working directory
    pub work_dir: Option<PathBuf>,
    /// Container resource limits
    pub limits: ResourceLimits,
    /// Container network configuration
    pub network: NetworkConfig,
    /// Container volume mounts
    pub volumes: Vec<VolumeMount>,
    /// Container port mappings
    pub ports: Vec<PortMapping>,
    /// Container labels
    pub labels: HashMap<String, String>,
    /// Container restart policy
    pub restart_policy: RestartPolicy,
    /// Container health check
    pub health_check: Option<HealthCheck>,
}
```

### `cleanroom::determinism`

Deterministic execution support.

#### Public Types

```rust
/// Deterministic execution context
pub struct DeterministicContext {
    /// Random seed
    pub seed: u64,
    /// Time offset
    pub time_offset: Duration,
    /// Network latency
    pub network_latency: Duration,
    /// Filesystem behavior
    pub filesystem_behavior: FilesystemMockBehavior,
    /// Environment variables
    pub env: HashMap<String, String>,
}

/// Filesystem mock behavior
pub enum FilesystemMockBehavior {
    /// Read-only filesystem
    ReadOnly,
    /// Write-only filesystem
    WriteOnly,
    /// No filesystem access
    NoAccess,
    /// Custom filesystem behavior
    Custom(HashMap<PathBuf, FilesystemOperation>),
}

/// Filesystem operation
pub enum FilesystemOperation {
    /// Allow operation
    Allow,
    /// Deny operation
    Deny,
    /// Mock operation
    Mock(String),
    /// Redirect operation
    Redirect(PathBuf),
}
```

### `cleanroom::coverage`

Test coverage tracking and analysis.

#### Public Types

```rust
/// Coverage information
pub struct CoverageInfo {
    /// Total lines of code
    pub total_lines: u64,
    /// Covered lines of code
    pub covered_lines: u64,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Coverage by file
    pub file_coverage: HashMap<PathBuf, FileCoverage>,
    /// Coverage by function
    pub function_coverage: HashMap<String, FunctionCoverage>,
    /// Coverage by branch
    pub branch_coverage: HashMap<String, BranchCoverage>,
}

/// File coverage information
pub struct FileCoverage {
    /// File path
    pub path: PathBuf,
    /// Total lines in file
    pub total_lines: u64,
    /// Covered lines in file
    pub covered_lines: u64,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Line coverage details
    pub line_coverage: HashMap<u32, bool>,
}

/// Function coverage information
pub struct FunctionCoverage {
    /// Function name
    pub name: String,
    /// Function signature
    pub signature: String,
    /// Total lines in function
    pub total_lines: u64,
    /// Covered lines in function
    pub covered_lines: u64,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Function calls
    pub calls: u64,
    /// Function execution time
    pub execution_time: Duration,
}

/// Branch coverage information
pub struct BranchCoverage {
    /// Branch name
    pub name: String,
    /// Branch condition
    pub condition: String,
    /// Total branches
    pub total_branches: u64,
    /// Covered branches
    pub covered_branches: u64,
    /// Coverage percentage
    pub coverage_percentage: f64,
    /// Branch execution count
    pub execution_count: u64,
}
```

### `cleanroom::snapshots`

Snapshot testing and comparison.

#### Public Types

```rust
/// Snapshot information
pub struct Snapshot {
    /// Snapshot ID
    pub id: String,
    /// Snapshot name
    pub name: String,
    /// Snapshot content
    pub content: String,
    /// Snapshot metadata
    pub metadata: HashMap<String, String>,
    /// Snapshot timestamp
    pub timestamp: SerializableInstant,
    /// Snapshot hash
    pub hash: String,
    /// Snapshot size
    pub size: u64,
}

/// Snapshot comparison result
pub struct SnapshotComparison {
    /// Snapshot A
    pub snapshot_a: Snapshot,
    /// Snapshot B
    pub snapshot_b: Snapshot,
    /// Comparison result
    pub result: ComparisonResult,
    /// Differences found
    pub differences: Vec<Difference>,
    /// Similarity score
    pub similarity_score: f64,
}

/// Comparison result
pub enum ComparisonResult {
    /// Snapshots are identical
    Identical,
    /// Snapshots are similar
    Similar,
    /// Snapshots are different
    Different,
    /// Comparison failed
    Failed,
}

/// Difference between snapshots
pub struct Difference {
    /// Difference type
    pub diff_type: DifferenceType,
    /// Line number
    pub line_number: Option<u32>,
    /// Expected content
    pub expected: Option<String>,
    /// Actual content
    pub actual: Option<String>,
    /// Difference description
    pub description: String,
}

/// Difference type
pub enum DifferenceType {
    /// Line added
    Added,
    /// Line removed
    Removed,
    /// Line modified
    Modified,
    /// Content changed
    ContentChanged,
    /// Format changed
    FormatChanged,
}
```

### `cleanroom::tracing`

Tracing and observability support.

#### Public Types

```rust
/// Tracing configuration
pub struct TracingConfig {
    /// Enable tracing
    pub enable_tracing: bool,
    /// Tracing level
    pub tracing_level: TracingLevel,
    /// Tracing format
    pub tracing_format: TracingFormat,
    /// Tracing output
    pub tracing_output: TracingOutput,
    /// Tracing filters
    pub tracing_filters: Vec<String>,
    /// Tracing sampling rate
    pub sampling_rate: f64,
    /// Tracing buffer size
    pub buffer_size: usize,
}

/// Tracing level
pub enum TracingLevel {
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

/// Tracing format
pub enum TracingFormat {
    /// JSON format
    Json,
    /// Text format
    Text,
    /// Compact format
    Compact,
    /// Pretty format
    Pretty,
}

/// Tracing output
pub enum TracingOutput {
    /// Standard output
    Stdout,
    /// Standard error
    Stderr,
    /// File output
    File(PathBuf),
    /// Network output
    Network(String),
    /// Custom output
    Custom(Box<dyn TracingOutputHandler>),
}

/// Tracing span
pub struct TracingSpan {
    /// Span ID
    pub id: String,
    /// Span name
    pub name: String,
    /// Span start time
    pub start_time: SerializableInstant,
    /// Span end time
    pub end_time: Option<SerializableInstant>,
    /// Span duration
    pub duration: Option<Duration>,
    /// Span attributes
    pub attributes: HashMap<String, String>,
    /// Span events
    pub events: Vec<TracingEvent>,
    /// Span status
    pub status: SpanStatus,
}

/// Tracing event
pub struct TracingEvent {
    /// Event name
    pub name: String,
    /// Event timestamp
    pub timestamp: SerializableInstant,
    /// Event attributes
    pub attributes: HashMap<String, String>,
    /// Event message
    pub message: Option<String>,
}

/// Span status
pub enum SpanStatus {
    /// Span is active
    Active,
    /// Span is completed
    Completed,
    /// Span failed
    Failed,
    /// Span cancelled
    Cancelled,
}
```

## Utility Modules

### `cleanroom::config`

Configuration management and validation.

#### Public Types

```rust
/// Main configuration structure
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
}

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

### `cleanroom::assertions`

Assertion utilities for test validation.

#### Public Types

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

/// Assertion result
pub struct AssertionResult {
    /// Assertion name
    pub name: String,
    /// Assertion success
    pub success: bool,
    /// Assertion message
    pub message: String,
    /// Assertion details
    pub details: HashMap<String, String>,
}

/// Assertion context
pub struct AssertionContext {
    /// Context name
    pub name: String,
    /// Context variables
    pub variables: HashMap<String, String>,
    /// Context metadata
    pub metadata: HashMap<String, String>,
}
```

## Summary

### Module Organization

The Cleanroom Testing Framework is organized into the following modules:

1. **Core Modules**: Essential functionality for cleanroom operations
2. **Advanced Modules**: Specialized features for specific use cases
3. **Utility Modules**: Supporting functionality and utilities

### Key Design Principles

1. **Modularity**: Clear separation of concerns
2. **Extensibility**: Easy to extend and customize
3. **Type Safety**: Strong typing throughout
4. **Error Handling**: Comprehensive error handling
5. **Performance**: Optimized for performance

### Usage Patterns

1. **Simple Usage**: Use top-level functions for basic operations
2. **Advanced Usage**: Use environment and configuration for complex scenarios
3. **Custom Usage**: Extend and customize for specific needs
4. **Testing Usage**: Use assertions and scenarios for testing

### Best Practices

1. **Resource Management**: Use RAII for automatic cleanup
2. **Error Handling**: Handle errors appropriately
3. **Configuration**: Use configuration for customization
4. **Performance**: Monitor and optimize performance
5. **Security**: Follow security best practices

This module reference provides comprehensive documentation for all public APIs in the Cleanroom Testing Framework, enabling developers to effectively use and extend the system.
