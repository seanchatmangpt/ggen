# ADR-004: Error Handling Hierarchy

## Status
Accepted

## Context

Error handling in testing frameworks needs to be comprehensive and user-friendly:
- **Clear Error Messages**: Users need to understand what went wrong
- **Actionable Information**: Errors should suggest how to fix the problem
- **Error Recovery**: Some errors should be recoverable
- **Debugging Support**: Errors should provide context for debugging
- **Consistency**: All errors should follow the same patterns

Current error handling challenges:
- Generic error types don't provide enough context
- No guidance on how to resolve errors
- Difficult to distinguish between different error categories
- No structured error information for programmatic handling
- Inconsistent error handling across modules

We need an error hierarchy that:
1. Categorizes errors by type and severity
2. Provides actionable error messages
3. Includes context for debugging
4. Supports error recovery where possible
5. Maintains consistency across the codebase

## Decision

Implement a **comprehensive error hierarchy** with the following design:

### Core Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum CleanroomError {
    // Container-related errors
    #[error("Container error: {message}")]
    ContainerError { message: String },
    
    #[error("Container startup failed: {reason}")]
    ContainerStartupFailed { reason: String },
    
    #[error("Container not found: {name}")]
    ContainerNotFound { name: String },
    
    // Backend-related errors
    #[error("Backend error: {message}")]
    BackendError { message: String },
    
    #[error("No backend available")]
    NoBackendAvailable,
    
    #[error("Backend feature not supported: {feature}")]
    BackendFeatureNotSupported { feature: String },
    
    // Resource-related errors
    #[error("Resource limit exceeded: {resource} (limit: {limit}, current: {current})")]
    ResourceLimitExceeded { 
        resource: String, 
        limit: String, 
        current: String 
    },
    
    #[error("Insufficient resources: {message}")]
    InsufficientResources { message: String },
    
    // Security-related errors
    #[error("Security policy violation: {policy}")]
    SecurityPolicyViolation { policy: String },
    
    #[error("Access denied: {resource}")]
    AccessDenied { resource: String },
    
    // Configuration errors
    #[error("Configuration error: {message}")]
    ConfigurationError { message: String },
    
    #[error("Invalid configuration: {field} = {value}")]
    InvalidConfiguration { field: String, value: String },
    
    // Execution errors
    #[error("Execution failed: {command} (exit code: {exit_code})")]
    ExecutionFailed { 
        command: String, 
        exit_code: i32,
        stdout: String,
        stderr: String,
    },
    
    #[error("Timeout: {operation} exceeded {timeout}s")]
    Timeout { 
        operation: String, 
        timeout: u64 
    },
    
    // Network errors
    #[error("Network error: {message}")]
    NetworkError { message: String },
    
    #[error("Connection failed: {host}:{port}")]
    ConnectionFailed { host: String, port: u16 },
    
    // File system errors
    #[error("File system error: {message}")]
    FileSystemError { message: String },
    
    #[error("File not found: {path}")]
    FileNotFound { path: String },
    
    // Validation errors
    #[error("Validation error: {message}")]
    ValidationError { message: String },
    
    // Internal errors
    #[error("Internal error: {message}")]
    InternalError { message: String },
    
    // Wrapped errors
    #[error("Wrapped error: {source}")]
    Wrapped {
        #[from]
        source: Box<dyn std::error::Error + Send + Sync>,
    },
}
```

### Error Context and Metadata

```rust
#[derive(Debug, Clone)]
pub struct ErrorContext {
    pub operation: String,
    pub timestamp: SystemTime,
    pub session_id: Option<Uuid>,
    pub container_id: Option<String>,
    pub backend: Option<String>,
    pub additional_info: HashMap<String, String>,
}

impl CleanroomError {
    pub fn with_context(self, context: ErrorContext) -> Self {
        // Add context to error
        self
    }
    
    pub fn get_context(&self) -> Option<&ErrorContext> {
        // Return error context
        None
    }
}
```

### Error Recovery Strategies

```rust
pub trait ErrorRecovery {
    fn can_recover(&self) -> bool;
    fn recovery_strategy(&self) -> Option<RecoveryStrategy>;
    fn attempt_recovery(&self) -> Result<()>;
}

#[derive(Debug)]
pub enum RecoveryStrategy {
    Retry { max_attempts: u32, delay: Duration },
    Fallback { alternative: String },
    Skip { reason: String },
    Manual { instructions: String },
}
```

## Consequences

### Positive

- **Clear Error Messages**: Users understand what went wrong
- **Actionable Guidance**: Errors suggest how to fix problems
- **Better Debugging**: Rich context for troubleshooting
- **Programmatic Handling**: Structured error information
- **Consistent Experience**: Uniform error handling across modules

### Negative

- **Complexity**: More complex error handling logic
- **Performance**: Slight overhead from error context
- **Maintenance**: More error types to maintain
- **Learning Curve**: Users need to understand error hierarchy

### Neutral

- **Memory Usage**: Slightly higher due to error context
- **Code Size**: More error handling code
- **Testing**: More error scenarios to test

## Alternatives Considered

### 1. Simple String Errors

Use simple string-based errors.

**Rejected because:**
- No structured information
- Difficult to handle programmatically
- No context for debugging
- Inconsistent error messages

### 2. Generic Error Types

Use generic error types with minimal information.

**Rejected because:**
- Not specific enough
- Hard to provide actionable guidance
- Difficult to categorize errors
- Poor user experience

### 3. Exception-Based Errors

Use exceptions instead of Result types.

**Rejected because:**
- Not idiomatic Rust
- Difficult to handle gracefully
- No compile-time error handling
- Performance overhead

### 4. External Error Libraries

Use external error handling libraries.

**Rejected because:**
- Additional dependencies
- Less control over error format
- Potential compatibility issues
- Learning curve for users

## Implementation Details

### Error Creation Helpers

```rust
impl CleanroomError {
    pub fn container_error(message: impl Into<String>) -> Self {
        Self::ContainerError { message: message.into() }
    }
    
    pub fn container_startup_failed(reason: impl Into<String>) -> Self {
        Self::ContainerStartupFailed { reason: reason.into() }
    }
    
    pub fn resource_limit_exceeded(
        resource: impl Into<String>,
        limit: impl Into<String>,
        current: impl Into<String>,
    ) -> Self {
        Self::ResourceLimitExceeded {
            resource: resource.into(),
            limit: limit.into(),
            current: current.into(),
        }
    }
    
    pub fn execution_failed(
        command: impl Into<String>,
        exit_code: i32,
        stdout: impl Into<String>,
        stderr: impl Into<String>,
    ) -> Self {
        Self::ExecutionFailed {
            command: command.into(),
            exit_code,
            stdout: stdout.into(),
            stderr: stderr.into(),
        }
    }
    
    pub fn timeout(operation: impl Into<String>, timeout: u64) -> Self {
        Self::Timeout {
            operation: operation.into(),
            timeout,
        }
    }
    
    pub fn validation_error(message: impl Into<String>) -> Self {
        Self::ValidationError { message: message.into() }
    }
    
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self::InternalError { message: message.into() }
    }
}
```

### Error Context Builder

```rust
pub struct ErrorContextBuilder {
    context: ErrorContext,
}

impl ErrorContextBuilder {
    pub fn new(operation: impl Into<String>) -> Self {
        Self {
            context: ErrorContext {
                operation: operation.into(),
                timestamp: SystemTime::now(),
                session_id: None,
                container_id: None,
                backend: None,
                additional_info: HashMap::new(),
            },
        }
    }
    
    pub fn session_id(mut self, session_id: Uuid) -> Self {
        self.context.session_id = Some(session_id);
        self
    }
    
    pub fn container_id(mut self, container_id: impl Into<String>) -> Self {
        self.context.container_id = Some(container_id.into());
        self
    }
    
    pub fn backend(mut self, backend: impl Into<String>) -> Self {
        self.context.backend = Some(backend.into());
        self
    }
    
    pub fn add_info(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.context.additional_info.insert(key.into(), value.into());
        self
    }
    
    pub fn build(self) -> ErrorContext {
        self.context
    }
}
```

### Error Handling Patterns

```rust
impl CleanroomEnvironment {
    pub async fn execute_with_recovery<F, R>(&self, operation: F) -> Result<R>
    where
        F: Fn() -> Result<R>,
    {
        let mut attempts = 0;
        let max_attempts = 3;
        
        loop {
            match operation() {
                Ok(result) => return Ok(result),
                Err(error) => {
                    attempts += 1;
                    
                    if attempts >= max_attempts {
                        return Err(error);
                    }
                    
                    // Check if error is recoverable
                    if let Some(recovery) = error.recovery_strategy() {
                        match recovery {
                            RecoveryStrategy::Retry { delay, .. } => {
                                tokio::time::sleep(delay).await;
                                continue;
                            }
                            RecoveryStrategy::Fallback { alternative } => {
                                // Try alternative approach
                                continue;
                            }
                            RecoveryStrategy::Skip { .. } => {
                                return Err(error);
                            }
                            RecoveryStrategy::Manual { .. } => {
                                return Err(error);
                            }
                        }
                    } else {
                        return Err(error);
                    }
                }
            }
        }
    }
}
```

### Error Reporting and Logging

```rust
impl CleanroomError {
    pub fn report(&self) -> ErrorReport {
        ErrorReport {
            error_type: self.error_type(),
            message: self.to_string(),
            context: self.get_context().cloned(),
            suggestions: self.get_suggestions(),
            severity: self.severity(),
            timestamp: SystemTime::now(),
        }
    }
    
    pub fn error_type(&self) -> &'static str {
        match self {
            CleanroomError::ContainerError { .. } => "container_error",
            CleanroomError::BackendError { .. } => "backend_error",
            CleanroomError::ResourceLimitExceeded { .. } => "resource_limit_exceeded",
            CleanroomError::SecurityPolicyViolation { .. } => "security_policy_violation",
            CleanroomError::ConfigurationError { .. } => "configuration_error",
            CleanroomError::ExecutionFailed { .. } => "execution_failed",
            CleanroomError::Timeout { .. } => "timeout",
            CleanroomError::NetworkError { .. } => "network_error",
            CleanroomError::FileSystemError { .. } => "file_system_error",
            CleanroomError::ValidationError { .. } => "validation_error",
            CleanroomError::InternalError { .. } => "internal_error",
            _ => "unknown_error",
        }
    }
    
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            CleanroomError::ContainerError { .. } => ErrorSeverity::Error,
            CleanroomError::BackendError { .. } => ErrorSeverity::Error,
            CleanroomError::ResourceLimitExceeded { .. } => ErrorSeverity::Warning,
            CleanroomError::SecurityPolicyViolation { .. } => ErrorSeverity::Critical,
            CleanroomError::ConfigurationError { .. } => ErrorSeverity::Error,
            CleanroomError::ExecutionFailed { .. } => ErrorSeverity::Error,
            CleanroomError::Timeout { .. } => ErrorSeverity::Warning,
            CleanroomError::NetworkError { .. } => ErrorSeverity::Error,
            CleanroomError::FileSystemError { .. } => ErrorSeverity::Error,
            CleanroomError::ValidationError { .. } => ErrorSeverity::Error,
            CleanroomError::InternalError { .. } => ErrorSeverity::Critical,
            _ => ErrorSeverity::Unknown,
        }
    }
    
    pub fn get_suggestions(&self) -> Vec<String> {
        match self {
            CleanroomError::ContainerError { .. } => vec![
                "Check if Docker is running".to_string(),
                "Verify container image exists".to_string(),
                "Check container logs for details".to_string(),
            ],
            CleanroomError::ResourceLimitExceeded { .. } => vec![
                "Increase resource limits".to_string(),
                "Optimize resource usage".to_string(),
                "Check for resource leaks".to_string(),
            ],
            CleanroomError::Timeout { .. } => vec![
                "Increase timeout duration".to_string(),
                "Check for performance issues".to_string(),
                "Verify network connectivity".to_string(),
            ],
            _ => vec!["Check logs for more details".to_string()],
        }
    }
}
```

## Error Handling Best Practices

### 1. Use Specific Error Types

```rust
// Good: Specific error type
fn create_container(name: &str) -> Result<Container> {
    if name.is_empty() {
        return Err(CleanroomError::validation_error("Container name cannot be empty"));
    }
    
    // ... container creation logic
}

// Avoid: Generic error
fn create_container(name: &str) -> Result<Container> {
    if name.is_empty() {
        return Err(CleanroomError::internal_error("Invalid input"));
    }
    
    // ... container creation logic
}
```

### 2. Provide Context

```rust
// Good: Rich context
fn execute_command(cmd: &str) -> Result<RunResult> {
    let context = ErrorContextBuilder::new("execute_command")
        .add_info("command", cmd)
        .add_info("working_directory", std::env::current_dir()?.to_string_lossy())
        .build();
    
    match run_command(cmd) {
        Ok(result) => Ok(result),
        Err(e) => Err(e.with_context(context)),
    }
}
```

### 3. Handle Errors Gracefully

```rust
// Good: Graceful error handling
async fn run_test(test_name: &str) -> Result<()> {
    match execute_test(test_name).await {
        Ok(result) => {
            println!("Test {} passed", test_name);
            Ok(())
        }
        Err(CleanroomError::Timeout { operation, timeout }) => {
            eprintln!("Test {} timed out after {}s", test_name, timeout);
            Err(CleanroomError::timeout(operation, timeout))
        }
        Err(CleanroomError::ResourceLimitExceeded { resource, limit, current }) => {
            eprintln!("Resource limit exceeded: {} (limit: {}, current: {})", resource, limit, current);
            Err(CleanroomError::resource_limit_exceeded(resource, limit, current))
        }
        Err(e) => {
            eprintln!("Test {} failed: {}", test_name, e);
            Err(e)
        }
    }
}
```

## References

- [Rust Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [thiserror crate](https://docs.rs/thiserror/)
- [anyhow crate](https://docs.rs/anyhow/)
- [Error Handling Best Practices](https://rust-lang.github.io/api-guidelines/interoperability.html#error-types-are-meaningful-and-well-behaved-c-good-err)

## Future Considerations

- **Error Metrics**: Collect error statistics and patterns
- **Error Recovery**: Automatic error recovery strategies
- **Error Notifications**: Real-time error notifications
- **Error Analytics**: Error trend analysis and reporting
- **Custom Error Types**: User-defined error types
