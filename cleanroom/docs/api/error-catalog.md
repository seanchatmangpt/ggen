# Error Catalog

This document provides a comprehensive catalog of all errors in the Cleanroom Testing Framework.

## Overview

### Error Categories

The Cleanroom error system is organized into the following categories:

1. **Container Errors**: Container lifecycle and operation errors
2. **Backend Errors**: Backend communication and operation errors
3. **Policy Errors**: Policy validation and enforcement errors
4. **Configuration Errors**: Configuration validation and parsing errors
5. **Execution Errors**: Command execution and runtime errors
6. **Scenario Errors**: Scenario execution and workflow errors
7. **Performance Errors**: Performance monitoring and optimization errors
8. **Security Errors**: Security policy and enforcement errors
9. **Resource Errors**: Resource management and allocation errors
10. **Validation Errors**: Data validation and constraint errors

### Error Design Principles

1. **Hierarchical**: Errors are organized in a clear hierarchy
2. **Contextual**: Errors provide meaningful context and information
3. **Actionable**: Errors suggest possible actions or solutions
4. **Traceable**: Errors can be traced back to their source
5. **Recoverable**: Errors indicate whether recovery is possible

## Main Error Type

### `CleanroomError`

The main error type that encompasses all other error types.

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
```

#### Error Properties

```rust
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
    
    /// Get error severity
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            CleanroomError::Container(ContainerError::StartupFailed(_)) => ErrorSeverity::High,
            CleanroomError::Container(ContainerError::ExecutionFailed(_)) => ErrorSeverity::High,
            CleanroomError::Backend(BackendError::ConnectionFailed(_)) => ErrorSeverity::Medium,
            CleanroomError::Backend(BackendError::Timeout(_)) => ErrorSeverity::Medium,
            CleanroomError::Policy(PolicyError::ViolationDetected(_)) => ErrorSeverity::High,
            CleanroomError::Security(SecurityError::ViolationDetected(_)) => ErrorSeverity::Critical,
            _ => ErrorSeverity::Low,
        }
    }
    
    /// Get error context
    pub fn context(&self) -> ErrorContext {
        ErrorContext {
            category: self.category(),
            severity: self.severity(),
            retryable: self.is_retryable(),
            timestamp: SerializableInstant::now(),
            source: std::any::type_name::<Self>().to_string(),
        }
    }
}
```

## Container Errors

### `ContainerError`

Errors related to container lifecycle and operations.

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
    
    /// Container image not found
    #[error("Container image not found: {0}")]
    ImageNotFound(String),
    
    /// Container image pull failed
    #[error("Container image pull failed: {0}")]
    ImagePullFailed(String),
    
    /// Container network error
    #[error("Container network error: {0}")]
    NetworkError(String),
    
    /// Container filesystem error
    #[error("Container filesystem error: {0}")]
    FilesystemError(String),
    
    /// Container permission denied
    #[error("Container permission denied: {0}")]
    PermissionDenied(String),
    
    /// Container timeout
    #[error("Container timeout: {0}")]
    Timeout(String),
    
    /// Container health check failed
    #[error("Container health check failed: {0}")]
    HealthCheckFailed(String),
    
    /// Container port conflict
    #[error("Container port conflict: {0}")]
    PortConflict(String),
    
    /// Container volume mount failed
    #[error("Container volume mount failed: {0}")]
    VolumeMountFailed(String),
    
    /// Container environment variable error
    #[error("Container environment variable error: {0}")]
    EnvironmentVariableError(String),
    
    /// Container command not found
    #[error("Container command not found: {0}")]
    CommandNotFound(String),
    
    /// Container process error
    #[error("Container process error: {0}")]
    ProcessError(String),
}
```

#### Container Error Examples

```rust
// Container startup failure
let error = ContainerError::StartupFailed("Failed to start container: insufficient memory".to_string());
println!("Error: {}", error); // "Container startup failed: Failed to start container: insufficient memory"

// Container not found
let error = ContainerError::NotFound("container-123".to_string());
println!("Error: {}", error); // "Container not found: container-123"

// Container resource limit exceeded
let error = ContainerError::ResourceLimitExceeded("Memory limit exceeded: 512MB".to_string());
println!("Error: {}", error); // "Container resource limit exceeded: Memory limit exceeded: 512MB"
```

#### Container Error Handling

```rust
use cleanroom::{CleanroomError, ContainerError};

async fn handle_container_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Container(container_error) => {
            match container_error {
                ContainerError::StartupFailed(msg) => {
                    eprintln!("Container startup failed: {}", msg);
                    // Possible actions:
                    // 1. Check Docker daemon status
                    // 2. Verify container image availability
                    // 3. Check resource availability
                    // 4. Retry with different configuration
                }
                ContainerError::ExecutionFailed(msg) => {
                    eprintln!("Container execution failed: {}", msg);
                    // Possible actions:
                    // 1. Check command syntax
                    // 2. Verify dependencies
                    // 3. Check permissions
                    // 4. Retry with different command
                }
                ContainerError::CleanupFailed(msg) => {
                    eprintln!("Container cleanup failed: {}", msg);
                    // Possible actions:
                    // 1. Force cleanup
                    // 2. Check container state
                    // 3. Retry cleanup
                    // 4. Log for manual cleanup
                }
                ContainerError::NotFound(container_id) => {
                    eprintln!("Container not found: {}", container_id);
                    // Possible actions:
                    // 1. Check container ID
                    // 2. List available containers
                    // 3. Create new container
                    // 4. Use different container
                }
                ContainerError::AlreadyExists(container_id) => {
                    eprintln!("Container already exists: {}", container_id);
                    // Possible actions:
                    // 1. Use existing container
                    // 2. Remove existing container
                    // 3. Use different container ID
                    // 4. Check container state
                }
                ContainerError::InvalidState(msg) => {
                    eprintln!("Container state invalid: {}", msg);
                    // Possible actions:
                    // 1. Check container state
                    // 2. Restart container
                    // 3. Recreate container
                    // 4. Wait for state change
                }
                ContainerError::ResourceLimitExceeded(msg) => {
                    eprintln!("Container resource limit exceeded: {}", msg);
                    // Possible actions:
                    // 1. Increase resource limits
                    // 2. Optimize resource usage
                    // 3. Check for resource leaks
                    // 4. Use different container
                }
                _ => {
                    eprintln!("Container error: {}", container_error);
                }
            }
        }
        _ => {
            eprintln!("Non-container error: {}", error);
        }
    }
    
    Ok(())
}
```

## Backend Errors

### `BackendError`

Errors related to backend communication and operations.

```rust
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
    
    /// Backend protocol error
    #[error("Backend protocol error: {0}")]
    ProtocolError(String),
    
    /// Backend serialization error
    #[error("Backend serialization error: {0}")]
    SerializationError(String),
    
    /// Backend deserialization error
    #[error("Backend deserialization error: {0}")]
    DeserializationError(String),
    
    /// Backend network error
    #[error("Backend network error: {0}")]
    NetworkError(String),
    
    /// Backend SSL/TLS error
    #[error("Backend SSL/TLS error: {0}")]
    SslError(String),
    
    /// Backend rate limit exceeded
    #[error("Backend rate limit exceeded: {0}")]
    RateLimitExceeded(String),
    
    /// Backend quota exceeded
    #[error("Backend quota exceeded: {0}")]
    QuotaExceeded(String),
    
    /// Backend maintenance mode
    #[error("Backend maintenance mode: {0}")]
    MaintenanceMode(String),
    
    /// Backend version mismatch
    #[error("Backend version mismatch: {0}")]
    VersionMismatch(String),
    
    /// Backend configuration error
    #[error("Backend configuration error: {0}")]
    ConfigurationError(String),
    
    /// Backend initialization error
    #[error("Backend initialization error: {0}")]
    InitializationError(String),
    
    /// Backend shutdown error
    #[error("Backend shutdown error: {0}")]
    ShutdownError(String),
}
```

#### Backend Error Examples

```rust
// Backend connection failure
let error = BackendError::ConnectionFailed("Failed to connect to Docker daemon".to_string());
println!("Error: {}", error); // "Backend connection failed: Failed to connect to Docker daemon"

// Backend timeout
let error = BackendError::Timeout("Operation timed out after 30 seconds".to_string());
println!("Error: {}", error); // "Backend timeout: Operation timed out after 30 seconds"

// Backend authentication failure
let error = BackendError::AuthenticationFailed("Invalid credentials".to_string());
println!("Error: {}", error); // "Backend authentication failed: Invalid credentials"
```

#### Backend Error Handling

```rust
use cleanroom::{CleanroomError, BackendError};

async fn handle_backend_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Backend(backend_error) => {
            match backend_error {
                BackendError::ConnectionFailed(msg) => {
                    eprintln!("Backend connection failed: {}", msg);
                    // Possible actions:
                    // 1. Check Docker daemon status
                    // 2. Verify network connectivity
                    // 3. Check firewall settings
                    // 4. Retry connection
                }
                BackendError::OperationFailed(msg) => {
                    eprintln!("Backend operation failed: {}", msg);
                    // Possible actions:
                    // 1. Check operation parameters
                    // 2. Verify backend state
                    // 3. Retry operation
                    // 4. Use alternative operation
                }
                BackendError::Timeout(msg) => {
                    eprintln!("Backend timeout: {}", msg);
                    // Possible actions:
                    // 1. Increase timeout
                    // 2. Check backend performance
                    // 3. Retry operation
                    // 4. Use different backend
                }
                BackendError::AuthenticationFailed(msg) => {
                    eprintln!("Backend authentication failed: {}", msg);
                    // Possible actions:
                    // 1. Check credentials
                    // 2. Verify authentication method
                    // 3. Update credentials
                    // 4. Use different authentication
                }
                BackendError::AuthorizationFailed(msg) => {
                    eprintln!("Backend authorization failed: {}", msg);
                    // Possible actions:
                    // 1. Check permissions
                    // 2. Verify user roles
                    // 3. Update permissions
                    // 4. Use different user
                }
                BackendError::NotAvailable(msg) => {
                    eprintln!("Backend not available: {}", msg);
                    // Possible actions:
                    // 1. Check backend status
                    // 2. Wait for backend to be available
                    // 3. Use different backend
                    // 4. Retry later
                }
                _ => {
                    eprintln!("Backend error: {}", backend_error);
                }
            }
        }
        _ => {
            eprintln!("Non-backend error: {}", error);
        }
    }
    
    Ok(())
}
```

## Policy Errors

### `PolicyError`

Errors related to policy validation and enforcement.

```rust
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
    
    /// Policy syntax error
    #[error("Policy syntax error: {0}")]
    SyntaxError(String),
    
    /// Policy semantic error
    #[error("Policy semantic error: {0}")]
    SemanticError(String),
    
    /// Policy dependency error
    #[error("Policy dependency error: {0}")]
    DependencyError(String),
    
    /// Policy version error
    #[error("Policy version error: {0}")]
    VersionError(String),
    
    /// Policy format error
    #[error("Policy format error: {0}")]
    FormatError(String),
    
    /// Policy parsing error
    #[error("Policy parsing error: {0}")]
    ParsingError(String),
    
    /// Policy compilation error
    #[error("Policy compilation error: {0}")]
    CompilationError(String),
    
    /// Policy execution error
    #[error("Policy execution error: {0}")]
    ExecutionError(String),
    
    /// Policy timeout
    #[error("Policy timeout: {0}")]
    Timeout(String),
    
    /// Policy resource error
    #[error("Policy resource error: {0}")]
    ResourceError(String),
    
    /// Policy security error
    #[error("Policy security error: {0}")]
    SecurityError(String),
}
```

#### Policy Error Examples

```rust
// Policy validation failure
let error = PolicyError::ValidationFailed("Security policy requires network isolation".to_string());
println!("Error: {}", error); // "Policy validation failed: Security policy requires network isolation"

// Policy violation detected
let error = PolicyError::ViolationDetected("Command 'rm' is not allowed".to_string());
println!("Error: {}", error); // "Policy violation detected: Command 'rm' is not allowed"

// Policy conflict
let error = PolicyError::Conflict("Security policy conflicts with performance policy".to_string());
println!("Error: {}", error); // "Policy conflict: Security policy conflicts with performance policy"
```

#### Policy Error Handling

```rust
use cleanroom::{CleanroomError, PolicyError};

async fn handle_policy_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Policy(policy_error) => {
            match policy_error {
                PolicyError::ValidationFailed(msg) => {
                    eprintln!("Policy validation failed: {}", msg);
                    // Possible actions:
                    // 1. Check policy syntax
                    // 2. Verify policy requirements
                    // 3. Update policy
                    // 4. Use different policy
                }
                PolicyError::EnforcementFailed(msg) => {
                    eprintln!("Policy enforcement failed: {}", msg);
                    // Possible actions:
                    // 1. Check policy configuration
                    // 2. Verify enforcement mechanism
                    // 3. Update policy
                    // 4. Disable policy temporarily
                }
                PolicyError::ViolationDetected(msg) => {
                    eprintln!("Policy violation detected: {}", msg);
                    // Possible actions:
                    // 1. Review operation
                    // 2. Update policy
                    // 3. Use different operation
                    // 4. Request policy exception
                }
                PolicyError::NotFound(policy_name) => {
                    eprintln!("Policy not found: {}", policy_name);
                    // Possible actions:
                    // 1. Check policy name
                    // 2. Create policy
                    // 3. Use default policy
                    // 4. List available policies
                }
                PolicyError::Conflict(msg) => {
                    eprintln!("Policy conflict: {}", msg);
                    // Possible actions:
                    // 1. Resolve conflict
                    // 2. Update policies
                    // 3. Use different policies
                    // 4. Prioritize policies
                }
                _ => {
                    eprintln!("Policy error: {}", policy_error);
                }
            }
        }
        _ => {
            eprintln!("Non-policy error: {}", error);
        }
    }
    
    Ok(())
}
```

## Configuration Errors

### `ConfigurationError`

Errors related to configuration validation and parsing.

```rust
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
    
    /// Configuration parsing error
    #[error("Configuration parsing error: {0}")]
    ParsingError(String),
    
    /// Configuration syntax error
    #[error("Configuration syntax error: {0}")]
    SyntaxError(String),
    
    /// Configuration type error
    #[error("Configuration type error: {0}")]
    TypeError(String),
    
    /// Configuration range error
    #[error("Configuration range error: {0}")]
    RangeError(String),
    
    /// Configuration required field missing
    #[error("Configuration required field missing: {0}")]
    RequiredFieldMissing(String),
    
    /// Configuration unknown field
    #[error("Configuration unknown field: {0}")]
    UnknownField(String),
    
    /// Configuration file not found
    #[error("Configuration file not found: {0}")]
    FileNotFound(String),
    
    /// Configuration file permission denied
    #[error("Configuration file permission denied: {0}")]
    PermissionDenied(String),
    
    /// Configuration file corrupted
    #[error("Configuration file corrupted: {0}")]
    FileCorrupted(String),
    
    /// Configuration environment variable error
    #[error("Configuration environment variable error: {0}")]
    EnvironmentVariableError(String),
    
    /// Configuration dependency error
    #[error("Configuration dependency error: {0}")]
    DependencyError(String),
    
    /// Configuration version error
    #[error("Configuration version error: {0}")]
    VersionError(String),
}
```

#### Configuration Error Examples

```rust
// Configuration validation failure
let error = ConfigurationError::ValidationFailed("Memory limit must be greater than 0".to_string());
println!("Error: {}", error); // "Configuration validation failed: Memory limit must be greater than 0"

// Configuration not found
let error = ConfigurationError::NotFound("cleanroom.toml".to_string());
println!("Error: {}", error); // "Configuration not found: cleanroom.toml"

// Configuration format invalid
let error = ConfigurationError::InvalidFormat("Invalid TOML format".to_string());
println!("Error: {}", error); // "Configuration format invalid: Invalid TOML format"
```

#### Configuration Error Handling

```rust
use cleanroom::{CleanroomError, ConfigurationError};

async fn handle_configuration_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Configuration(config_error) => {
            match config_error {
                ConfigurationError::ValidationFailed(msg) => {
                    eprintln!("Configuration validation failed: {}", msg);
                    // Possible actions:
                    // 1. Check configuration values
                    // 2. Verify configuration format
                    // 3. Update configuration
                    // 4. Use default configuration
                }
                ConfigurationError::NotFound(config_name) => {
                    eprintln!("Configuration not found: {}", config_name);
                    // Possible actions:
                    // 1. Check file path
                    // 2. Create configuration file
                    // 3. Use default configuration
                    // 4. Specify different path
                }
                ConfigurationError::InvalidFormat(msg) => {
                    eprintln!("Configuration format invalid: {}", msg);
                    // Possible actions:
                    // 1. Check file format
                    // 2. Validate syntax
                    // 3. Fix format errors
                    // 4. Use different format
                }
                ConfigurationError::InvalidValue(msg) => {
                    eprintln!("Configuration value invalid: {}", msg);
                    // Possible actions:
                    // 1. Check value ranges
                    // 2. Verify value types
                    // 3. Update values
                    // 4. Use default values
                }
                ConfigurationError::Conflict(msg) => {
                    eprintln!("Configuration conflict: {}", msg);
                    // Possible actions:
                    // 1. Resolve conflicts
                    // 2. Update configuration
                    // 3. Use different configuration
                    // 4. Prioritize settings
                }
                _ => {
                    eprintln!("Configuration error: {}", config_error);
                }
            }
        }
        _ => {
            eprintln!("Non-configuration error: {}", error);
        }
    }
    
    Ok(())
}
```

## Execution Errors

### `ExecutionError`

Errors related to command execution and runtime operations.

```rust
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
    
    /// Command syntax error
    #[error("Command syntax error: {0}")]
    SyntaxError(String),
    
    /// Command argument error
    #[error("Command argument error: {0}")]
    ArgumentError(String),
    
    /// Command environment error
    #[error("Command environment error: {0}")]
    EnvironmentError(String),
    
    /// Command working directory error
    #[error("Command working directory error: {0}")]
    WorkingDirectoryError(String),
    
    /// Command input/output error
    #[error("Command input/output error: {0}")]
    IoError(String),
    
    /// Command signal error
    #[error("Command signal error: {0}")]
    SignalError(String),
    
    /// Command process error
    #[error("Command process error: {0}")]
    ProcessError(String),
    
    /// Command dependency error
    #[error("Command dependency error: {0}")]
    DependencyError(String),
    
    /// Command version error
    #[error("Command version error: {0}")]
    VersionError(String),
    
    /// Command configuration error
    #[error("Command configuration error: {0}")]
    ConfigurationError(String),
}
```

#### Execution Error Examples

```rust
// Command execution failure
let error = ExecutionError::CommandFailed("Command returned exit code 1".to_string());
println!("Error: {}", error); // "Command execution failed: Command returned exit code 1"

// Command timeout
let error = ExecutionError::Timeout("Command timed out after 30 seconds".to_string());
println!("Error: {}", error); // "Command timeout: Command timed out after 30 seconds"

// Command not found
let error = ExecutionError::CommandNotFound("python3".to_string());
println!("Error: {}", error); // "Command not found: python3"
```

#### Execution Error Handling

```rust
use cleanroom::{CleanroomError, ExecutionError};

async fn handle_execution_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Execution(exec_error) => {
            match exec_error {
                ExecutionError::CommandFailed(msg) => {
                    eprintln!("Command execution failed: {}", msg);
                    // Possible actions:
                    // 1. Check command syntax
                    // 2. Verify dependencies
                    // 3. Check permissions
                    // 4. Retry with different parameters
                }
                ExecutionError::Timeout(msg) => {
                    eprintln!("Command timeout: {}", msg);
                    // Possible actions:
                    // 1. Increase timeout
                    // 2. Optimize command
                    // 3. Check system performance
                    // 4. Retry operation
                }
                ExecutionError::CommandNotFound(cmd) => {
                    eprintln!("Command not found: {}", cmd);
                    // Possible actions:
                    // 1. Install missing command
                    // 2. Check PATH environment
                    // 3. Use full path
                    // 4. Use alternative command
                }
                ExecutionError::PermissionDenied(msg) => {
                    eprintln!("Permission denied: {}", msg);
                    // Possible actions:
                    // 1. Check file permissions
                    // 2. Run with elevated privileges
                    // 3. Change ownership
                    // 4. Use different user
                }
                ExecutionError::ResourceUnavailable(msg) => {
                    eprintln!("Resource unavailable: {}", msg);
                    // Possible actions:
                    // 1. Check resource availability
                    // 2. Wait for resource
                    // 3. Use alternative resource
                    // 4. Retry later
                }
                ExecutionError::Interrupted(msg) => {
                    eprintln!("Execution interrupted: {}", msg);
                    // Possible actions:
                    // 1. Check for interruptions
                    // 2. Resume execution
                    // 3. Clean up resources
                    // 4. Retry operation
                }
                _ => {
                    eprintln!("Execution error: {}", exec_error);
                }
            }
        }
        _ => {
            eprintln!("Non-execution error: {}", error);
        }
    }
    
    Ok(())
}
```

## Scenario Errors

### `ScenarioError`

Errors related to scenario execution and workflow management.

```rust
/// Scenario-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ScenarioError {
    /// Scenario execution failed
    #[error("Scenario execution failed: {0}")]
    ExecutionFailed(String),
    
    /// Scenario step failed
    #[error("Scenario step failed: {0}")]
    StepFailed(String),
    
    /// Scenario timeout
    #[error("Scenario timeout: {0}")]
    Timeout(String),
    
    /// Scenario not found
    #[error("Scenario not found: {0}")]
    NotFound(String),
    
    /// Scenario validation failed
    #[error("Scenario validation failed: {0}")]
    ValidationFailed(String),
    
    /// Scenario dependency error
    #[error("Scenario dependency error: {0}")]
    DependencyError(String),
    
    /// Scenario rollback failed
    #[error("Scenario rollback failed: {0}")]
    RollbackFailed(String),
    
    /// Scenario state error
    #[error("Scenario state error: {0}")]
    StateError(String),
    
    /// Scenario resource error
    #[error("Scenario resource error: {0}")]
    ResourceError(String),
    
    /// Scenario configuration error
    #[error("Scenario configuration error: {0}")]
    ConfigurationError(String),
    
    /// Scenario step dependency error
    #[error("Scenario step dependency error: {0}")]
    StepDependencyError(String),
    
    /// Scenario step timeout
    #[error("Scenario step timeout: {0}")]
    StepTimeout(String),
    
    /// Scenario step retry failed
    #[error("Scenario step retry failed: {0}")]
    StepRetryFailed(String),
    
    /// Scenario step rollback failed
    #[error("Scenario step rollback failed: {0}")]
    StepRollbackFailed(String),
    
    /// Scenario parallel execution error
    #[error("Scenario parallel execution error: {0}")]
    ParallelExecutionError(String),
    
    /// Scenario serialization error
    #[error("Scenario serialization error: {0}")]
    SerializationError(String),
    
    /// Scenario deserialization error
    #[error("Scenario deserialization error: {0}")]
    DeserializationError(String),
}
```

#### Scenario Error Examples

```rust
// Scenario execution failure
let error = ScenarioError::ExecutionFailed("Scenario failed due to step failure".to_string());
println!("Error: {}", error); // "Scenario execution failed: Scenario failed due to step failure"

// Scenario step failure
let error = ScenarioError::StepFailed("Step 'install_deps' failed".to_string());
println!("Error: {}", error); // "Scenario step failed: Step 'install_deps' failed"

// Scenario timeout
let error = ScenarioError::Timeout("Scenario timed out after 300 seconds".to_string());
println!("Error: {}", error); // "Scenario timeout: Scenario timed out after 300 seconds"
```

#### Scenario Error Handling

```rust
use cleanroom::{CleanroomError, ScenarioError};

async fn handle_scenario_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Scenario(scenario_error) => {
            match scenario_error {
                ScenarioError::ExecutionFailed(msg) => {
                    eprintln!("Scenario execution failed: {}", msg);
                    // Possible actions:
                    // 1. Check scenario steps
                    // 2. Verify dependencies
                    // 3. Retry scenario
                    // 4. Use different scenario
                }
                ScenarioError::StepFailed(msg) => {
                    eprintln!("Scenario step failed: {}", msg);
                    // Possible actions:
                    // 1. Check step configuration
                    // 2. Verify step dependencies
                    // 3. Retry step
                    // 4. Skip step if optional
                }
                ScenarioError::Timeout(msg) => {
                    eprintln!("Scenario timeout: {}", msg);
                    // Possible actions:
                    // 1. Increase timeout
                    // 2. Optimize scenario
                    // 3. Check system performance
                    // 4. Retry scenario
                }
                ScenarioError::NotFound(scenario_name) => {
                    eprintln!("Scenario not found: {}", scenario_name);
                    // Possible actions:
                    // 1. Check scenario name
                    // 2. Create scenario
                    // 3. Use different scenario
                    // 4. List available scenarios
                }
                ScenarioError::ValidationFailed(msg) => {
                    eprintln!("Scenario validation failed: {}", msg);
                    // Possible actions:
                    // 1. Check scenario syntax
                    // 2. Verify scenario requirements
                    // 3. Update scenario
                    // 4. Use different scenario
                }
                ScenarioError::DependencyError(msg) => {
                    eprintln!("Scenario dependency error: {}", msg);
                    // Possible actions:
                    // 1. Check dependencies
                    // 2. Install missing dependencies
                    // 3. Update dependencies
                    // 4. Use different scenario
                }
                ScenarioError::RollbackFailed(msg) => {
                    eprintln!("Scenario rollback failed: {}", msg);
                    // Possible actions:
                    // 1. Check rollback configuration
                    // 2. Verify rollback steps
                    // 3. Manual cleanup
                    // 4. Retry rollback
                }
                _ => {
                    eprintln!("Scenario error: {}", scenario_error);
                }
            }
        }
        _ => {
            eprintln!("Non-scenario error: {}", error);
        }
    }
    
    Ok(())
}
```

## Performance Errors

### `PerformanceError`

Errors related to performance monitoring and optimization.

```rust
/// Performance-specific errors
#[derive(Debug, thiserror::Error)]
pub enum PerformanceError {
    /// Performance threshold exceeded
    #[error("Performance threshold exceeded: {0}")]
    ThresholdExceeded(String),
    
    /// Performance monitoring failed
    #[error("Performance monitoring failed: {0}")]
    MonitoringFailed(String),
    
    /// Performance data collection failed
    #[error("Performance data collection failed: {0}")]
    DataCollectionFailed(String),
    
    /// Performance analysis failed
    #[error("Performance analysis failed: {0}")]
    AnalysisFailed(String),
    
    /// Performance optimization failed
    #[error("Performance optimization failed: {0}")]
    OptimizationFailed(String),
    
    /// Performance benchmark failed
    #[error("Performance benchmark failed: {0}")]
    BenchmarkFailed(String),
    
    /// Performance profiling failed
    #[error("Performance profiling failed: {0}")]
    ProfilingFailed(String),
    
    /// Performance metrics error
    #[error("Performance metrics error: {0}")]
    MetricsError(String),
    
    /// Performance sampling error
    #[error("Performance sampling error: {0}")]
    SamplingError(String),
    
    /// Performance aggregation error
    #[error("Performance aggregation error: {0}")]
    AggregationError(String),
    
    /// Performance reporting error
    #[error("Performance reporting error: {0}")]
    ReportingError(String),
    
    /// Performance storage error
    #[error("Performance storage error: {0}")]
    StorageError(String),
    
    /// Performance retrieval error
    #[error("Performance retrieval error: {0}")]
    RetrievalError(String),
    
    /// Performance comparison error
    #[error("Performance comparison error: {0}")]
    ComparisonError(String),
    
    /// Performance trend analysis error
    #[error("Performance trend analysis error: {0}")]
    TrendAnalysisError(String),
    
    /// Performance alert error
    #[error("Performance alert error: {0}")]
    AlertError(String),
}
```

#### Performance Error Examples

```rust
// Performance threshold exceeded
let error = PerformanceError::ThresholdExceeded("CPU usage exceeded 80%".to_string());
println!("Error: {}", error); // "Performance threshold exceeded: CPU usage exceeded 80%"

// Performance monitoring failed
let error = PerformanceError::MonitoringFailed("Failed to start performance monitoring".to_string());
println!("Error: {}", error); // "Performance monitoring failed: Failed to start performance monitoring"

// Performance benchmark failed
let error = PerformanceError::BenchmarkFailed("Benchmark execution failed".to_string());
println!("Error: {}", error); // "Performance benchmark failed: Benchmark execution failed"
```

#### Performance Error Handling

```rust
use cleanroom::{CleanroomError, PerformanceError};

async fn handle_performance_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Performance(perf_error) => {
            match perf_error {
                PerformanceError::ThresholdExceeded(msg) => {
                    eprintln!("Performance threshold exceeded: {}", msg);
                    // Possible actions:
                    // 1. Check system resources
                    // 2. Optimize performance
                    // 3. Increase thresholds
                    // 4. Scale resources
                }
                PerformanceError::MonitoringFailed(msg) => {
                    eprintln!("Performance monitoring failed: {}", msg);
                    // Possible actions:
                    // 1. Check monitoring configuration
                    // 2. Verify monitoring tools
                    // 3. Restart monitoring
                    // 4. Use alternative monitoring
                }
                PerformanceError::DataCollectionFailed(msg) => {
                    eprintln!("Performance data collection failed: {}", msg);
                    // Possible actions:
                    // 1. Check data collection tools
                    // 2. Verify permissions
                    // 3. Retry collection
                    // 4. Use different collection method
                }
                PerformanceError::AnalysisFailed(msg) => {
                    eprintln!("Performance analysis failed: {}", msg);
                    // Possible actions:
                    // 1. Check analysis tools
                    // 2. Verify data quality
                    // 3. Retry analysis
                    // 4. Use different analysis method
                }
                PerformanceError::OptimizationFailed(msg) => {
                    eprintln!("Performance optimization failed: {}", msg);
                    // Possible actions:
                    // 1. Check optimization parameters
                    // 2. Verify system state
                    // 3. Retry optimization
                    // 4. Use different optimization strategy
                }
                PerformanceError::BenchmarkFailed(msg) => {
                    eprintln!("Performance benchmark failed: {}", msg);
                    // Possible actions:
                    // 1. Check benchmark configuration
                    // 2. Verify system resources
                    // 3. Retry benchmark
                    // 4. Use different benchmark
                }
                _ => {
                    eprintln!("Performance error: {}", perf_error);
                }
            }
        }
        _ => {
            eprintln!("Non-performance error: {}", error);
        }
    }
    
    Ok(())
}
```

## Security Errors

### `SecurityError`

Errors related to security policy and enforcement.

```rust
/// Security-specific errors
#[derive(Debug, thiserror::Error)]
pub enum SecurityError {
    /// Security violation detected
    #[error("Security violation detected: {0}")]
    ViolationDetected(String),
    
    /// Security policy violation
    #[error("Security policy violation: {0}")]
    PolicyViolation(String),
    
    /// Security authentication failed
    #[error("Security authentication failed: {0}")]
    AuthenticationFailed(String),
    
    /// Security authorization failed
    #[error("Security authorization failed: {0}")]
    AuthorizationFailed(String),
    
    /// Security encryption error
    #[error("Security encryption error: {0}")]
    EncryptionError(String),
    
    /// Security decryption error
    #[error("Security decryption error: {0}")]
    DecryptionError(String),
    
    /// Security key error
    #[error("Security key error: {0}")]
    KeyError(String),
    
    /// Security certificate error
    #[error("Security certificate error: {0}")]
    CertificateError(String),
    
    /// Security signature error
    #[error("Security signature error: {0}")]
    SignatureError(String),
    
    /// Security hash error
    #[error("Security hash error: {0}")]
    HashError(String),
    
    /// Security token error
    #[error("Security token error: {0}")]
    TokenError(String),
    
    /// Security session error
    #[error("Security session error: {0}")]
    SessionError(String),
    
    /// Security audit error
    #[error("Security audit error: {0}")]
    AuditError(String),
    
    /// Security compliance error
    #[error("Security compliance error: {0}")]
    ComplianceError(String),
    
    /// Security isolation error
    #[error("Security isolation error: {0}")]
    IsolationError(String),
    
    /// Security access control error
    #[error("Security access control error: {0}")]
    AccessControlError(String),
    
    /// Security data protection error
    #[error("Security data protection error: {0}")]
    DataProtectionError(String),
}
```

#### Security Error Examples

```rust
// Security violation detected
let error = SecurityError::ViolationDetected("Unauthorized network access detected".to_string());
println!("Error: {}", error); // "Security violation detected: Unauthorized network access detected"

// Security policy violation
let error = SecurityError::PolicyViolation("Command 'rm' is not allowed".to_string());
println!("Error: {}", error); // "Security policy violation: Command 'rm' is not allowed"

// Security authentication failed
let error = SecurityError::AuthenticationFailed("Invalid credentials".to_string());
println!("Error: {}", error); // "Security authentication failed: Invalid credentials"
```

#### Security Error Handling

```rust
use cleanroom::{CleanroomError, SecurityError};

async fn handle_security_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Security(security_error) => {
            match security_error {
                SecurityError::ViolationDetected(msg) => {
                    eprintln!("Security violation detected: {}", msg);
                    // Possible actions:
                    // 1. Review operation
                    // 2. Check security policies
                    // 3. Update policies
                    // 4. Block operation
                }
                SecurityError::PolicyViolation(msg) => {
                    eprintln!("Security policy violation: {}", msg);
                    // Possible actions:
                    // 1. Review policy
                    // 2. Update policy
                    // 3. Request exception
                    // 4. Use different operation
                }
                SecurityError::AuthenticationFailed(msg) => {
                    eprintln!("Security authentication failed: {}", msg);
                    // Possible actions:
                    // 1. Check credentials
                    // 2. Verify authentication method
                    // 3. Update credentials
                    // 4. Use different authentication
                }
                SecurityError::AuthorizationFailed(msg) => {
                    eprintln!("Security authorization failed: {}", msg);
                    // Possible actions:
                    // 1. Check permissions
                    // 2. Verify user roles
                    // 3. Update permissions
                    // 4. Use different user
                }
                SecurityError::EncryptionError(msg) => {
                    eprintln!("Security encryption error: {}", msg);
                    // Possible actions:
                    // 1. Check encryption keys
                    // 2. Verify encryption algorithm
                    // 3. Update keys
                    // 4. Use different encryption
                }
                SecurityError::DecryptionError(msg) => {
                    eprintln!("Security decryption error: {}", msg);
                    // Possible actions:
                    // 1. Check decryption keys
                    // 2. Verify decryption algorithm
                    // 3. Update keys
                    // 4. Use different decryption
                }
                _ => {
                    eprintln!("Security error: {}", security_error);
                }
            }
        }
        _ => {
            eprintln!("Non-security error: {}", error);
        }
    }
    
    Ok(())
}
```

## Resource Errors

### `ResourceError`

Errors related to resource management and allocation.

```rust
/// Resource-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ResourceError {
    /// Resource limit exceeded
    #[error("Resource limit exceeded: {0}")]
    LimitExceeded(String),
    
    /// Resource unavailable
    #[error("Resource unavailable: {0}")]
    Unavailable(String),
    
    /// Resource allocation failed
    #[error("Resource allocation failed: {0}")]
    AllocationFailed(String),
    
    /// Resource deallocation failed
    #[error("Resource deallocation failed: {0}")]
    DeallocationFailed(String),
    
    /// Resource monitoring failed
    #[error("Resource monitoring failed: {0}")]
    MonitoringFailed(String),
    
    /// Resource cleanup failed
    #[error("Resource cleanup failed: {0}")]
    CleanupFailed(String),
    
    /// Resource leak detected
    #[error("Resource leak detected: {0}")]
    LeakDetected(String),
    
    /// Resource contention
    #[error("Resource contention: {0}")]
    Contention(String),
    
    /// Resource deadlock
    #[error("Resource deadlock: {0}")]
    Deadlock(String),
    
    /// Resource starvation
    #[error("Resource starvation: {0}")]
    Starvation(String),
    
    /// Resource fragmentation
    #[error("Resource fragmentation: {0}")]
    Fragmentation(String),
    
    /// Resource corruption
    #[error("Resource corruption: {0}")]
    Corruption(String),
    
    /// Resource validation failed
    #[error("Resource validation failed: {0}")]
    ValidationFailed(String),
    
    /// Resource configuration error
    #[error("Resource configuration error: {0}")]
    ConfigurationError(String),
    
    /// Resource performance error
    #[error("Resource performance error: {0}")]
    PerformanceError(String),
    
    /// Resource security error
    #[error("Resource security error: {0}")]
    SecurityError(String),
}
```

#### Resource Error Examples

```rust
// Resource limit exceeded
let error = ResourceError::LimitExceeded("Memory limit exceeded: 512MB".to_string());
println!("Error: {}", error); // "Resource limit exceeded: Memory limit exceeded: 512MB"

// Resource unavailable
let error = ResourceError::Unavailable("CPU cores unavailable".to_string());
println!("Error: {}", error); // "Resource unavailable: CPU cores unavailable"

// Resource allocation failed
let error = ResourceError::AllocationFailed("Failed to allocate memory".to_string());
println!("Error: {}", error); // "Resource allocation failed: Failed to allocate memory"
```

#### Resource Error Handling

```rust
use cleanroom::{CleanroomError, ResourceError};

async fn handle_resource_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Resource(resource_error) => {
            match resource_error {
                ResourceError::LimitExceeded(msg) => {
                    eprintln!("Resource limit exceeded: {}", msg);
                    // Possible actions:
                    // 1. Increase resource limits
                    // 2. Optimize resource usage
                    // 3. Check for resource leaks
                    // 4. Use different resources
                }
                ResourceError::Unavailable(msg) => {
                    eprintln!("Resource unavailable: {}", msg);
                    // Possible actions:
                    // 1. Check resource availability
                    // 2. Wait for resource
                    // 3. Use alternative resource
                    // 4. Retry later
                }
                ResourceError::AllocationFailed(msg) => {
                    eprintln!("Resource allocation failed: {}", msg);
                    // Possible actions:
                    // 1. Check allocation parameters
                    // 2. Verify resource availability
                    // 3. Retry allocation
                    // 4. Use different allocation strategy
                }
                ResourceError::DeallocationFailed(msg) => {
                    eprintln!("Resource deallocation failed: {}", msg);
                    // Possible actions:
                    // 1. Check deallocation parameters
                    // 2. Verify resource state
                    // 3. Retry deallocation
                    // 4. Force deallocation
                }
                ResourceError::MonitoringFailed(msg) => {
                    eprintln!("Resource monitoring failed: {}", msg);
                    // Possible actions:
                    // 1. Check monitoring configuration
                    // 2. Verify monitoring tools
                    // 3. Restart monitoring
                    // 4. Use alternative monitoring
                }
                ResourceError::CleanupFailed(msg) => {
                    eprintln!("Resource cleanup failed: {}", msg);
                    // Possible actions:
                    // 1. Check cleanup configuration
                    // 2. Verify resource state
                    // 3. Retry cleanup
                    // 4. Force cleanup
                }
                ResourceError::LeakDetected(msg) => {
                    eprintln!("Resource leak detected: {}", msg);
                    // Possible actions:
                    // 1. Check for leaks
                    // 2. Fix leak sources
                    // 3. Clean up leaked resources
                    // 4. Monitor for future leaks
                }
                _ => {
                    eprintln!("Resource error: {}", resource_error);
                }
            }
        }
        _ => {
            eprintln!("Non-resource error: {}", error);
        }
    }
    
    Ok(())
}
```

## Validation Errors

### `ValidationError`

Errors related to data validation and constraint checking.

```rust
/// Validation-specific errors
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
    
    /// Type validation failed
    #[error("Type validation failed: {field} - expected {expected}, got {actual}")]
    TypeValidation { field: String, expected: String, actual: String },
    
    /// Length validation failed
    #[error("Length validation failed: {field} - length {length} not in range {min}..{max}")]
    LengthValidation { field: String, length: usize, min: usize, max: usize },
    
    /// Pattern validation failed
    #[error("Pattern validation failed: {field} - {value} does not match pattern {pattern}")]
    PatternValidation { field: String, value: String, pattern: String },
    
    /// Required field missing
    #[error("Required field missing: {field}")]
    RequiredFieldMissing { field: String },
    
    /// Unknown field
    #[error("Unknown field: {field}")]
    UnknownField { field: String },
    
    /// Duplicate field
    #[error("Duplicate field: {field}")]
    DuplicateField { field: String },
    
    /// Invalid enum value
    #[error("Invalid enum value: {field} - {value} is not a valid {enum_type}")]
    InvalidEnumValue { field: String, value: String, enum_type: String },
    
    /// Invalid date/time
    #[error("Invalid date/time: {field} - {value} is not a valid date/time")]
    InvalidDateTime { field: String, value: String },
    
    /// Invalid URL
    #[error("Invalid URL: {field} - {value} is not a valid URL")]
    InvalidUrl { field: String, value: String },
    
    /// Invalid email
    #[error("Invalid email: {field} - {value} is not a valid email")]
    InvalidEmail { field: String, value: String },
    
    /// Invalid UUID
    #[error("Invalid UUID: {field} - {value} is not a valid UUID")]
    InvalidUuid { field: String, value: String },
    
    /// Invalid JSON
    #[error("Invalid JSON: {field} - {value} is not valid JSON")]
    InvalidJson { field: String, value: String },
    
    /// Invalid TOML
    #[error("Invalid TOML: {field} - {value} is not valid TOML")]
    InvalidToml { field: String, value: String },
    
    /// Invalid YAML
    #[error("Invalid YAML: {field} - {value} is not valid YAML")]
    InvalidYaml { field: String, value: String },
}
```

#### Validation Error Examples

```rust
// Field validation failure
let error = ValidationError::FieldValidation {
    field: "memory_limit".to_string(),
    message: "Memory limit must be greater than 0".to_string(),
};
println!("Error: {}", error); // "Field validation failed: memory_limit - Memory limit must be greater than 0"

// Range validation failure
let error = ValidationError::RangeValidation {
    field: "cpu_percent".to_string(),
    value: "150.0".to_string(),
    min: "0.0".to_string(),
    max: "100.0".to_string(),
};
println!("Error: {}", error); // "Range validation failed: cpu_percent - 150.0 not in range 0.0..100.0"

// Format validation failure
let error = ValidationError::FormatValidation {
    field: "email".to_string(),
    value: "invalid-email".to_string(),
};
println!("Error: {}", error); // "Format validation failed: email - invalid-email has invalid format"
```

#### Validation Error Handling

```rust
use cleanroom::{CleanroomError, ValidationError};

async fn handle_validation_error(error: CleanroomError) -> Result<(), Box<dyn std::error::Error>> {
    match error {
        CleanroomError::Validation(validation_error) => {
            match validation_error {
                ValidationError::FieldValidation { field, message } => {
                    eprintln!("Field validation failed: {} - {}", field, message);
                    // Possible actions:
                    // 1. Check field value
                    // 2. Verify field requirements
                    // 3. Update field value
                    // 4. Use default value
                }
                ValidationError::RangeValidation { field, value, min, max } => {
                    eprintln!("Range validation failed: {} - {} not in range {}..{}", field, value, min, max);
                    // Possible actions:
                    // 1. Check value range
                    // 2. Verify range constraints
                    // 3. Update value
                    // 4. Use valid range
                }
                ValidationError::FormatValidation { field, value } => {
                    eprintln!("Format validation failed: {} - {} has invalid format", field, value);
                    // Possible actions:
                    // 1. Check format requirements
                    // 2. Verify format syntax
                    // 3. Update format
                    // 4. Use valid format
                }
                ValidationError::DependencyValidation { field, message } => {
                    eprintln!("Dependency validation failed: {} - {}", field, message);
                    // Possible actions:
                    // 1. Check dependencies
                    // 2. Verify dependency requirements
                    // 3. Update dependencies
                    // 4. Use different dependencies
                }
                ValidationError::TypeValidation { field, expected, actual } => {
                    eprintln!("Type validation failed: {} - expected {}, got {}", field, expected, actual);
                    // Possible actions:
                    // 1. Check type requirements
                    // 2. Verify type conversion
                    // 3. Update type
                    // 4. Use correct type
                }
                ValidationError::LengthValidation { field, length, min, max } => {
                    eprintln!("Length validation failed: {} - length {} not in range {}..{}", field, length, min, max);
                    // Possible actions:
                    // 1. Check length requirements
                    // 2. Verify length constraints
                    // 3. Update length
                    // 4. Use valid length
                }
                ValidationError::PatternValidation { field, value, pattern } => {
                    eprintln!("Pattern validation failed: {} - {} does not match pattern {}", field, value, pattern);
                    // Possible actions:
                    // 1. Check pattern requirements
                    // 2. Verify pattern syntax
                    // 3. Update pattern
                    // 4. Use valid pattern
                }
                ValidationError::RequiredFieldMissing { field } => {
                    eprintln!("Required field missing: {}", field);
                    // Possible actions:
                    // 1. Check field requirements
                    // 2. Add missing field
                    // 3. Use default value
                    // 4. Make field optional
                }
                ValidationError::UnknownField { field } => {
                    eprintln!("Unknown field: {}", field);
                    // Possible actions:
                    // 1. Check field name
                    // 2. Remove unknown field
                    // 3. Add field definition
                    // 4. Use correct field name
                }
                _ => {
                    eprintln!("Validation error: {}", validation_error);
                }
            }
        }
        _ => {
            eprintln!("Non-validation error: {}", error);
        }
    }
    
    Ok(())
}
```

## Error Context and Metadata

### `ErrorContext`

Context information for errors.

```rust
/// Error context information
pub struct ErrorContext {
    /// Error category
    pub category: ErrorCategory,
    /// Error severity
    pub severity: ErrorSeverity,
    /// Whether error is retryable
    pub retryable: bool,
    /// Error timestamp
    pub timestamp: SerializableInstant,
    /// Error source
    pub source: String,
    /// Error metadata
    pub metadata: HashMap<String, String>,
}

/// Error category enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorCategory {
    /// Container errors
    Container,
    /// Backend errors
    Backend,
    /// Policy errors
    Policy,
    /// Configuration errors
    Configuration,
    /// Execution errors
    Execution,
    /// Scenario errors
    Scenario,
    /// Performance errors
    Performance,
    /// Security errors
    Security,
    /// Resource errors
    Resource,
    /// Validation errors
    Validation,
}

/// Error severity enumeration
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorSeverity {
    /// Low severity
    Low,
    /// Medium severity
    Medium,
    /// High severity
    High,
    /// Critical severity
    Critical,
}
```

### Error Recovery Strategies

```rust
/// Error recovery strategy
pub enum RecoveryStrategy {
    /// Retry the operation
    Retry {
        max_attempts: u32,
        delay: Duration,
    },
    /// Use alternative approach
    Alternative {
        approach: String,
        parameters: HashMap<String, String>,
    },
    /// Skip the operation
    Skip {
        reason: String,
    },
    /// Fail the operation
    Fail {
        reason: String,
    },
    /// Escalate the error
    Escalate {
        level: String,
        contact: String,
    },
}

/// Error recovery handler
pub trait ErrorRecoveryHandler {
    /// Handle error recovery
    fn handle_recovery(&self, error: &CleanroomError) -> Result<RecoveryStrategy, Box<dyn std::error::Error>>;
}

/// Default error recovery handler
pub struct DefaultErrorRecoveryHandler;

impl ErrorRecoveryHandler for DefaultErrorRecoveryHandler {
    fn handle_recovery(&self, error: &CleanroomError) -> Result<RecoveryStrategy, Box<dyn std::error::Error>> {
        match error {
            CleanroomError::Backend(BackendError::ConnectionFailed(_)) => {
                Ok(RecoveryStrategy::Retry {
                    max_attempts: 3,
                    delay: Duration::from_secs(5),
                })
            }
            CleanroomError::Backend(BackendError::Timeout(_)) => {
                Ok(RecoveryStrategy::Retry {
                    max_attempts: 2,
                    delay: Duration::from_secs(10),
                })
            }
            CleanroomError::Execution(ExecutionError::Timeout(_)) => {
                Ok(RecoveryStrategy::Retry {
                    max_attempts: 2,
                    delay: Duration::from_secs(5),
                })
            }
            CleanroomError::Execution(ExecutionError::ResourceUnavailable(_)) => {
                Ok(RecoveryStrategy::Retry {
                    max_attempts: 3,
                    delay: Duration::from_secs(15),
                })
            }
            CleanroomError::Security(SecurityError::ViolationDetected(_)) => {
                Ok(RecoveryStrategy::Fail {
                    reason: "Security violation cannot be recovered".to_string(),
                })
            }
            CleanroomError::Policy(PolicyError::ViolationDetected(_)) => {
                Ok(RecoveryStrategy::Fail {
                    reason: "Policy violation cannot be recovered".to_string(),
                })
            }
            _ => {
                Ok(RecoveryStrategy::Fail {
                    reason: "No recovery strategy available".to_string(),
                })
            }
        }
    }
}
```

## Error Logging and Monitoring

### Error Logging

```rust
/// Error logger
pub struct ErrorLogger {
    /// Logger configuration
    config: LoggerConfig,
    /// Log sink
    sink: Box<dyn LogSink>,
}

impl ErrorLogger {
    /// Log an error
    pub fn log_error(&self, error: &CleanroomError, context: &ErrorContext) -> Result<(), LogError> {
        let log_entry = LogEntry {
            timestamp: context.timestamp,
            level: LogLevel::Error,
            category: context.category.clone(),
            severity: context.severity.clone(),
            message: error.to_string(),
            source: context.source.clone(),
            metadata: context.metadata.clone(),
            stack_trace: Some(format!("{:?}", error)),
        };
        
        self.sink.write(&log_entry)?;
        Ok(())
    }
    
    /// Log error with recovery
    pub fn log_error_with_recovery(
        &self,
        error: &CleanroomError,
        context: &ErrorContext,
        recovery: &RecoveryStrategy,
    ) -> Result<(), LogError> {
        let mut metadata = context.metadata.clone();
        metadata.insert("recovery_strategy".to_string(), format!("{:?}", recovery));
        
        let log_entry = LogEntry {
            timestamp: context.timestamp,
            level: LogLevel::Warn,
            category: context.category.clone(),
            severity: context.severity.clone(),
            message: format!("{} - Recovery: {:?}", error, recovery),
            source: context.source.clone(),
            metadata,
            stack_trace: Some(format!("{:?}", error)),
        };
        
        self.sink.write(&log_entry)?;
        Ok(())
    }
}
```

### Error Monitoring

```rust
/// Error monitor
pub struct ErrorMonitor {
    /// Monitor configuration
    config: MonitorConfig,
    /// Error metrics
    metrics: Arc<RwLock<ErrorMetrics>>,
    /// Alert handlers
    alert_handlers: Vec<Box<dyn AlertHandler>>,
}

impl ErrorMonitor {
    /// Record an error
    pub fn record_error(&self, error: &CleanroomError, context: &ErrorContext) -> Result<(), MonitorError> {
        let mut metrics = self.metrics.write().unwrap();
        
        // Update error counts
        metrics.total_errors += 1;
        metrics.errors_by_category.entry(context.category.clone()).or_insert(0) += 1;
        metrics.errors_by_severity.entry(context.severity.clone()).or_insert(0) += 1;
        
        // Update error rates
        let now = SerializableInstant::now();
        let time_window = Duration::from_secs(60); // 1 minute window
        
        // Clean old entries
        metrics.error_timestamps.retain(|timestamp| {
            now.duration_since(*timestamp) < time_window
        });
        
        // Add new timestamp
        metrics.error_timestamps.push(now);
        
        // Calculate error rate
        metrics.error_rate = metrics.error_timestamps.len() as f64 / time_window.as_secs() as f64;
        
        // Check for alerts
        self.check_alerts(&metrics, error, context)?;
        
        Ok(())
    }
    
    /// Check for alerts
    fn check_alerts(
        &self,
        metrics: &ErrorMetrics,
        error: &CleanroomError,
        context: &ErrorContext,
    ) -> Result<(), MonitorError> {
        // Check error rate threshold
        if metrics.error_rate > self.config.error_rate_threshold {
            let alert = Alert {
                level: AlertLevel::High,
                message: format!("High error rate detected: {:.2} errors/second", metrics.error_rate),
                timestamp: SerializableInstant::now(),
                metadata: HashMap::new(),
            };
            
            for handler in &self.alert_handlers {
                handler.handle_alert(&alert)?;
            }
        }
        
        // Check critical errors
        if context.severity == ErrorSeverity::Critical {
            let alert = Alert {
                level: AlertLevel::Critical,
                message: format!("Critical error detected: {}", error),
                timestamp: SerializableInstant::now(),
                metadata: context.metadata.clone(),
            };
            
            for handler in &self.alert_handlers {
                handler.handle_alert(&alert)?;
            }
        }
        
        Ok(())
    }
}
```

## Summary

### Error System Features

1. **Hierarchical Structure**: Clear error hierarchy with main error type
2. **Contextual Information**: Rich context and metadata for each error
3. **Recovery Strategies**: Built-in recovery strategies for common errors
4. **Logging and Monitoring**: Comprehensive error logging and monitoring
5. **Type Safety**: Strong typing for error handling

### Error Categories

1. **Container Errors**: Container lifecycle and operation errors
2. **Backend Errors**: Backend communication and operation errors
3. **Policy Errors**: Policy validation and enforcement errors
4. **Configuration Errors**: Configuration validation and parsing errors
5. **Execution Errors**: Command execution and runtime errors
6. **Scenario Errors**: Scenario execution and workflow errors
7. **Performance Errors**: Performance monitoring and optimization errors
8. **Security Errors**: Security policy and enforcement errors
9. **Resource Errors**: Resource management and allocation errors
10. **Validation Errors**: Data validation and constraint errors

### Error Handling Best Practices

1. **Handle Errors Appropriately**: Use appropriate error handling for each error type
2. **Provide Context**: Include meaningful context in error messages
3. **Suggest Actions**: Provide actionable suggestions for error resolution
4. **Log Errors**: Log errors with appropriate detail and context
5. **Monitor Errors**: Monitor error rates and patterns for system health

### Error Recovery Strategies

1. **Retry**: Retry operations for transient errors
2. **Alternative**: Use alternative approaches when possible
3. **Skip**: Skip non-critical operations when appropriate
4. **Fail**: Fail operations when recovery is not possible
5. **Escalate**: Escalate critical errors to appropriate personnel

This error catalog provides comprehensive documentation for all error types in the Cleanroom Testing Framework, enabling developers to effectively handle and recover from errors.
