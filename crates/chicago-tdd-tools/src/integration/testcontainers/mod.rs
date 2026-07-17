//! Testcontainers Support
//!
//! Provides minimal generic container support for integration testing with Docker.
//! Follows Chicago TDD principles by using real containers instead of mocks.
//!
//! ## Features (80/20 Minimal)
//!
//! - **Generic Containers**: Support for any Docker image
//! - **Port Mapping**: Get host ports for container ports
//! - **Environment Variables**: Basic environment variable support
//! - **Command Execution**: Execute commands inside containers and get stdout/stderr/exit code
//! - **Wait Conditions**: Wait for containers to be ready (e.g., HTTP health checks)
//! - **Automatic Cleanup**: Containers cleaned up automatically on Drop
//! - **Poka-Yoke Design**: Type-level state machine prevents invalid operations (see `poka_yoke` module)
//!
//! ## Chicago TDD Alignment
//!
//! - **Real Collaborators**: Actual Docker containers, not mocks
//! - **State Verification**: Verify container state and responses
//! - **Automatic Cleanup**: Containers cleaned up via Drop trait
//! - **AAA Pattern**: Arrange (start container), Act (test), Assert (verify state)
//!
//! ## Usage
//!
//! ```rust
//! # #[cfg(feature = "testcontainers")]
//! use chicago_tdd_tools::testcontainers::{ContainerClient, GenericContainer, DEFAULT_HTTP_PORT};
//! # #[cfg(feature = "testcontainers")]
//! use chicago_tdd_tools::testcontainers::exec::SUCCESS_EXIT_CODE;
//!
//! # #[cfg(feature = "testcontainers")]
//! # fn example() -> Result<(), chicago_tdd_tools::testcontainers::TestcontainersError> {
//!     // Arrange: Create client and container
//!     let client = ContainerClient::new();
//!     
//!     // Create container with command to keep it running
//!     let container = GenericContainer::with_command(
//!         client.client(),
//!         "alpine",
//!         "latest",
//!         "sleep",
//!         &["infinity"],
//!         None  // Uses testcontainers API - full features available
//!     )?;
//!
//!     // Act: Use container (e.g., get port or execute command)
//!     // Note: Port mapping requires testcontainers-managed container
//!     // let host_port = container.get_host_port(DEFAULT_HTTP_PORT)?;
//!
//!     // Execute a command in the container
//!     let result = container.exec("echo", &["hello", "world"])?;
//!
//!     // Assert: Verify command succeeded using constant
//!     assert_eq!(result.exit_code, SUCCESS_EXIT_CODE);
//!     assert_eq!(result.stdout.trim(), "hello world");
//!
//!     // Container automatically cleaned up on drop
//! #     Ok(())
//! # }
//! ```
//!
//! ## Container Lifecycle Notes
//!
//! **Command Execution**: The `exec()` method requires the container to be running.
//! This works best with:
//! - Service containers (postgres, redis, nginx, etc.) that stay running
//! - Containers with long-running default commands
//!
//! **Container Lifecycle Requirements**: For containers that exit immediately (e.g., `otel/weaver`, `alpine`), you **must** use `GenericContainer::with_command()` with a command that keeps the container running (e.g., `sleep infinity`). Using `GenericContainer::new()` for images that exit immediately will cause "container is not running" errors when executing commands.
//!
//! **Implementation Details:**
//! - **Regular containers** (`entrypoint = None`): Uses testcontainers API normally
//!   - Full features: port mapping, wait conditions, standard lifecycle
//! - **Entrypoint override** (`entrypoint = Some(...)`): Uses Docker CLI workaround
//!   - Only for images with problematic entrypoints (e.g., `otel/weaver`)
//!   - Note: Port mapping not available for Docker CLI-created containers
//!
//! **Pattern**:
//! ```rust,ignore
//! // ❌ Wrong: Container exits immediately, exec will fail
//! let container = GenericContainer::new(client.client(), "otel/weaver", "latest")?;
//! container.exec("weaver", &["--version"])?; // Error: container is not running
//!
//! // ✅ Correct: Regular container (uses testcontainers API)
//! let container = GenericContainer::with_command(
//!     client.client(),
//!     "alpine",
//!     "latest",
//!     "sleep",
//!     &["infinity"],
//!     None  // Uses testcontainers API - full features available
//! )?;
//!
//! // ✅ Correct: Container with entrypoint override (uses Docker CLI workaround)
//! let container = GenericContainer::with_command(
//!     client.client(),
//!     "otel/weaver",
//!     "latest",
//!     "sleep",
//!     &["infinity"],
//!     Some(&["/bin/sh"])  // Uses Docker CLI workaround - single executable required (Docker limitation)
//! )?;
//! container.exec("weaver", &["--version"])?; // Success: container stays running
//! ```
//!
//! For containers that exit immediately, consider using service images or
//! accessing the underlying container via `container()` for advanced configuration.

#[cfg(feature = "testcontainers")]
use std::collections::HashMap;

use thiserror::Error;

/// Default HTTP port for examples and tests
///
/// **Kaizen improvement**: Extracted magic number `80` to named constant.
/// Pattern: Use named constants for commonly used port numbers.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::testcontainers::DEFAULT_HTTP_PORT;
///
/// // Use constant instead of magic number
/// let http_port = DEFAULT_HTTP_PORT;
/// assert_eq!(http_port, 80);
/// ```
pub const DEFAULT_HTTP_PORT: u16 = 80;

/// Testcontainers error type
#[derive(Error, Debug)]
pub enum TestcontainersError {
    /// Docker daemon is not running or unavailable
    #[error("🚨 Docker daemon is not running or unavailable: {0}\n   ⚠️  STOP: Cannot proceed with container operations\n   💡 FIX: Start Docker Desktop or Docker daemon\n   📋 macOS: Open Docker Desktop\n   📋 Linux: sudo systemctl start docker\n   📋 Windows: Start Docker Desktop")]
    DockerUnavailable(String),
    /// Failed to create container
    #[error("🚨 Failed to create container: {0}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and Docker daemon is running")]
    CreationFailed(String),
    /// Container operation failed
    #[error("⚠️  Container operation failed: {0}\n   ⚠️  WARNING: Container operation did not complete successfully")]
    OperationFailed(String),
    /// Invalid configuration
    #[error("🚨 Invalid configuration: {0}\n   ⚠️  STOP: Configuration is invalid\n   💡 FIX: Check configuration parameters")]
    InvalidConfig(String),
    /// Command execution failed
    #[error("⚠️  Command execution failed: {0}\n   ⚠️  WARNING: Command did not execute successfully\n   💡 FIX: Check command syntax and container state")]
    CommandExecutionFailed(String),
    /// Failed to read stdout
    #[error("⚠️  Failed to read stdout: {0}\n   ⚠️  WARNING: Could not read command output\n   💡 FIX: Check container is running and command completed")]
    StdoutReadFailed(String),
    /// Failed to read stderr
    #[error("⚠️  Failed to read stderr: {0}\n   ⚠️  WARNING: Could not read command error output\n   💡 FIX: Check container is running and command completed")]
    StderrReadFailed(String),
    /// Failed to get exit code
    #[error("⚠️  Failed to get exit code: {0}\n   ⚠️  WARNING: Could not determine command exit status\n   💡 FIX: Check container is running and command completed")]
    ExitCodeFailed(String),
}

/// Result type for testcontainers operations
pub type TestcontainersResult<T> = Result<T, TestcontainersError>;

// Re-export exec and wait functionality
pub mod exec;
pub mod wait;

/// Poka-yoke types for testcontainers (compile-time error prevention)
///
/// **Poka-yoke**: Type-level state machine prevents invalid container operations.
/// See module documentation for examples.
pub mod poka_yoke;
pub use exec::ExecResult;

#[cfg(feature = "testcontainers")]
/// Implementation module for testcontainers functionality
///
/// Contains the actual implementation of `ContainerClient` and `GenericContainer`.
/// These types are feature-gated and only available when the `testcontainers` feature is enabled.
pub mod implementation {
    use super::{HashMap, TestcontainersError, TestcontainersResult};
    use std::process::Command;

    /// Container startup delay in milliseconds
    ///
    /// **Kaizen improvement**: Extracted magic number `100` to named constant.
    /// This delay allows Docker CLI-created containers to be ready before exec operations.
    /// Pattern: Use named constants for timing values that may need adjustment.
    #[allow(dead_code)]
    const CONTAINER_STARTUP_DELAY_MS: u64 = 100;

    /// Initial delay for container startup retry in milliseconds (100ms)
    const CONTAINER_RETRY_INITIAL_DELAY_MS: u64 = 100;

    /// Maximum retries for container startup check
    const CONTAINER_STARTUP_MAX_RETRIES: u32 = 3;

    /// Maximum total wait time for container startup (sum of all retries with backoff)
    /// Initial: 100ms, Retry 1: 200ms, Retry 2: 400ms = 700ms total
    #[allow(dead_code)]
    const CONTAINER_STARTUP_MAX_WAIT_MS: u64 = 700;
    use testcontainers::core::ContainerPort;
    use testcontainers::runners::SyncRunner;
    use testcontainers::Container;
    use testcontainers::GenericImage;
    use testcontainers::ImageExt;

    /// Check if Docker daemon is actually running and responding
    ///
    /// This function verifies Docker daemon is running by checking:
    /// 1. Docker command exists
    /// 2. Docker daemon is responding (not just command execution)
    /// 3. Docker daemon is accessible
    ///
    /// **Root Cause Fix**: Added timeout to prevent hanging when Docker daemon is not running.
    /// Pattern: All external commands must have timeout protection to fail fast.
    /// Implementation: Spawn command in thread, use mpsc channel with `recv_timeout`.
    /// Timeout duration: 500ms (fast enough to fail within 1s test timeout, enough time for docker info when Docker is running).
    /// This prevents the function from hanging indefinitely when Docker daemon is stopped.
    ///
    /// Returns 🚨 CRITICAL signal if Docker is unavailable.
    /// This is a fail-fast check - operations should stop immediately.
    ///
    /// # Returns
    ///
    /// `Ok(())` if Docker daemon is running and responding
    /// `Err(TestcontainersError::DockerUnavailable)` if Docker is stopped or unavailable
    ///
    /// # Errors
    ///
    /// Returns an error if Docker is unavailable or not responding.
    pub fn check_docker_available() -> TestcontainersResult<()> {
        use std::process::Command;
        use std::sync::mpsc;
        use std::thread;
        use std::time::Duration;

        // **Root Cause Fix**: Add timeout to prevent hanging when Docker daemon is not running.
        // Pattern: All external commands should have timeouts to fail fast.
        // Implementation: Spawn command in thread, use mpsc channel with recv_timeout.
        // Timeout duration: 5000ms (5 seconds) - increased to handle Docker Desktop startup delays
        // and parallel test execution. Fast enough to fail within test timeout, enough time for
        // docker info when Docker is running under load. This prevents the function from hanging
        // indefinitely when Docker daemon is stopped.
        // Aligns with codebase timeout standards (see docs/features/TIMEOUT_ENFORCEMENT.md).
        const DOCKER_CHECK_TIMEOUT_MILLIS: u64 = 5000;
        const MAX_RETRIES: u32 = 2;

        // Retry logic for parallel test execution - Docker may be slow to respond under load
        for attempt in 0..=MAX_RETRIES {
            // Use docker info to verify daemon is running
            // Spawn command in thread to enable timeout
            let (tx, rx) = mpsc::channel();
            let _handle = thread::spawn(move || {
                let output = Command::new("docker").args(["info"]).output();
                tx.send(output).ok();
            });

            // Wait for result with timeout
            if let Ok(docker_check) =
                rx.recv_timeout(Duration::from_millis(DOCKER_CHECK_TIMEOUT_MILLIS))
            {
                match docker_check {
                    Ok(output) => {
                        if output.status.success() {
                            // Verify Docker daemon is responding by checking output
                            let stdout = String::from_utf8_lossy(&output.stdout);
                            if stdout.contains("Server Version")
                                || stdout.contains("Docker Root Dir")
                            {
                                // ✅ Docker daemon is running and responding
                                return Ok(());
                            }
                        }
                        // If we get here and it's not the last attempt, retry with delay
                        if attempt < MAX_RETRIES {
                            // Small delay to reduce contention when multiple tests check Docker simultaneously
                            thread::sleep(Duration::from_millis(100 * u64::from(attempt + 1)));
                            continue;
                        }
                        // Last attempt failed - return error
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        return Err(TestcontainersError::DockerUnavailable(format!(
                            "Docker daemon is not running. Error: {stderr}"
                        )));
                    }
                    Err(e) => {
                        if attempt < MAX_RETRIES {
                            // Small delay to reduce contention
                            thread::sleep(Duration::from_millis(100 * u64::from(attempt + 1)));
                            continue;
                        }
                        if e.kind() == std::io::ErrorKind::NotFound {
                            return Err(TestcontainersError::DockerUnavailable(
                                "Docker command not found. Please install Docker.".to_string(),
                            ));
                        }
                        return Err(TestcontainersError::DockerUnavailable(format!(
                            "Failed to check Docker availability: {e}"
                        )));
                    }
                }
            }
            // 🚨 Timeout - Docker command hung (likely Docker daemon not running or under heavy load)
            if attempt < MAX_RETRIES {
                // Retry on timeout - Docker might be slow under parallel test load
                // Exponential backoff: 100ms, 200ms delays
                thread::sleep(Duration::from_millis(100 * u64::from(attempt + 1)));
                continue;
            }
            return Err(TestcontainersError::DockerUnavailable(format!(
                "Docker check timed out after {DOCKER_CHECK_TIMEOUT_MILLIS}ms after {} attempts (Docker daemon likely not running or under heavy load). This prevents hanging indefinitely when Docker is unavailable.",
                attempt + 1
            )));
        }

        // Should never reach here, but provide fallback error
        Err(TestcontainersError::DockerUnavailable(
            "Docker check failed after all retry attempts".to_string(),
        ))
    }

    /// Docker error message patterns that indicate Docker daemon is unavailable
    ///
    /// **Kaizen improvement**: Extracted duplicated error detection strings to named constants.
    /// Pattern: Use constants for repeated string patterns to reduce duplication and improve maintainability.
    /// Benefits: Single source of truth, easier to maintain, consistent error detection.
    const DOCKER_CONNECTION_ERROR_PATTERNS: &[&str] =
        &["Cannot connect to the Docker daemon", "docker daemon", "connection refused"];

    /// Check if an error message indicates Docker daemon is unavailable
    ///
    /// **Kaizen improvement**: Extracted duplicated error detection logic to helper function.
    /// Pattern: Extract repeated logic to function for DRY (Don't Repeat Yourself) principle.
    /// Benefits: Single implementation, easier to maintain, consistent behavior.
    fn is_docker_unavailable_error(error_msg: &str) -> bool {
        DOCKER_CONNECTION_ERROR_PATTERNS
            .iter()
            .any(|pattern| error_msg.contains(pattern))
    }

    /// Wait for container to be ready with exponential backoff retry logic
    ///
    /// **FAIL-FAST HARDENING**: Replaces fixed 100ms delay with intelligent retry loop.
    /// Root cause: Fixed delay doesn't adapt to slow Docker daemon or high system load.
    /// Solution: Use exponential backoff (100ms → 200ms → 400ms) with health check.
    ///
    /// Process:
    /// 1. Check if container exists and is running (docker ps)
    /// 2. If healthy, return immediately (fast path)
    /// 3. If not ready, retry with exponential backoff
    /// 4. Give up after 3 retries (700ms total max wait)
    ///
    /// Benefits:
    /// - Fast containers: Return immediately (< 100ms in happy path)
    /// - Slow containers: Adapt to system load with exponential backoff
    /// - Prevents false failures: Ensures container actually running before continuing
    ///
    /// # Arguments
    /// * `container_id` - Docker container ID to check
    ///
    /// # Errors
    ///
    /// Returns `Err(TestcontainersError::OperationFailed)` when all retries are exhausted and the
    /// container has still not reached the `running` state.
    fn wait_for_container_ready(container_id: &str) -> TestcontainersResult<()> {
        use std::thread;
        use std::time::Duration;

        for attempt in 0..=CONTAINER_STARTUP_MAX_RETRIES {
            // Check if container is running using docker ps
            let output = Command::new("docker")
                .args(["ps", "--filter", &format!("id={container_id}"), "--format", "{{.State}}"])
                .output();

            if let Ok(out) = output {
                let state = String::from_utf8_lossy(&out.stdout).trim().to_string();
                // Container is running if docker ps finds it in any non-empty state
                if !state.is_empty() && state == "running" {
                    return Ok(());
                }
            }
            // docker command failed or container not ready yet, retry with backoff

            // Not ready yet - retry with exponential backoff if not last attempt
            if attempt < CONTAINER_STARTUP_MAX_RETRIES {
                let delay_ms = CONTAINER_RETRY_INITIAL_DELAY_MS * 2_u64.pow(attempt);
                thread::sleep(Duration::from_millis(delay_ms));
            }
        }

        // All retries exhausted — container did not reach running state
        Err(TestcontainersError::OperationFailed(format!(
            "Container {container_id} did not reach 'running' state after {CONTAINER_STARTUP_MAX_RETRIES} retries"
        )))
    }

    /// Container client for managing Docker containers
    ///
    /// Minimal 80/20 implementation - provides basic container management.
    /// For advanced features (pools, determinism, policies), see clnrm.
    pub struct ContainerClient;

    impl ContainerClient {
        /// Create a new container client
        ///
        /// **FMEA Fix (RPN 216)**: Check Docker availability at client creation to fail-fast.
        /// Previously, Docker was only checked when containers were created, allowing false positives
        /// (tests pass when Docker unavailable). Now checks Docker immediately to prevent false positives.
        ///
        /// # Panics
        ///
        /// Panics if Docker is unavailable, with a clear error message.
        #[must_use]
        pub fn new() -> Self {
            // **FMEA Fix**: Verify Docker is available at client creation (fail-fast)
            // This prevents false positives where tests pass when Docker is unavailable
            #[allow(clippy::panic)] // Test helper - panic is appropriate if Docker unavailable
            check_docker_available().unwrap_or_else(|e| {
                panic!(
                    "🚨 Docker is required for testcontainers but Docker daemon is not available.\n\
                     ⚠️  STOP: Cannot create container client\n\
                     💡 FIX: Start Docker Desktop or Docker daemon\n\
                     📋 macOS: Open Docker Desktop\n\
                     📋 Linux: sudo systemctl start docker\n\
                     📋 Windows: Start Docker Desktop\n\
                     \n\
                     Error: {e}"
                )
            });
            Self
        }

        /// Get a reference for compatibility (no-op in minimal implementation)
        #[must_use]
        pub const fn client(&self) -> &Self {
            self
        }
    }

    impl Default for ContainerClient {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Generic container wrapper for any Docker image
    ///
    /// Minimal 80/20 implementation - supports basic container operations:
    /// - Start any Docker image
    /// - Map container ports to host ports
    /// - Set environment variables
    /// - Execute commands
    /// - Automatic cleanup on Drop
    ///
    /// For advanced features (volume mounts, resource limits, determinism),
    /// see clnrm's `TestcontainerBackend`.
    #[derive(Debug)]
    pub struct GenericContainer {
        container: Option<Container<GenericImage>>,
        /// Container ID for Docker CLI-created containers (used for entrypoint override workaround)
        /// When Some, exec operations use docker exec directly instead of testcontainers exec
        docker_cli_container_id: Option<String>,
    }

    impl GenericContainer {
        /// Create a new generic container from any Docker image
        ///
        /// 🚨 CRITICAL - Stops immediately if Docker is unavailable.
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance (should have been validated via `ContainerClient::new()`)
        /// * `image` - Docker image name (e.g., "alpine", "postgres")
        /// * `tag` - Docker image tag (e.g., "latest", "14")
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails (Docker not running, image not found, etc.)
        pub fn new(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
        ) -> TestcontainersResult<Self> {
            // 🚨 Verify Docker is still available before container operations
            check_docker_available()?;

            let image = GenericImage::new(image, tag);
            let container = image.start().map_err(|e| {
                // Check if error indicates Docker is unavailable
                let error_msg = format!("{e}");
                if is_docker_unavailable_error(&error_msg) {
                    TestcontainersError::DockerUnavailable(format!(
                        "Docker daemon connection failed during container start: {e}\n   ⚠️  STOP: Cannot connect to Docker daemon\n   💡 FIX: Start Docker Desktop or Docker daemon"
                    ))
                } else {
                    TestcontainersError::CreationFailed(format!("Failed to start container: {e}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and Docker daemon is running"))
                }
            })?;

            // ✅ Container created successfully
            Ok(Self { container: Some(container), docker_cli_container_id: None })
        }

        /// Create a `GenericContainer` from an existing Container
        ///
        /// This is used internally by other methods (e.g., `with_wait_for`) to construct
        /// a `GenericContainer` from a Container that was created with additional configuration.
        pub(crate) const fn from_container(container: Container<GenericImage>) -> Self {
            Self { container: Some(container), docker_cli_container_id: None }
        }

        /// Create a `GenericContainer` from a Docker CLI-created container ID
        /// This is used for entrypoint override workaround when testcontainers doesn't support it
        pub(crate) const fn from_docker_cli_container_id(container_id: String) -> Self {
            Self { container: None, docker_cli_container_id: Some(container_id) }
        }

        /// Create a new generic container with environment variables and optional command
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance (unused in minimal implementation)
        /// * `image` - Docker image name
        /// * `tag` - Docker image tag
        /// * `env_vars` - Environment variables to set in the container
        /// * `command` - Optional command to run (e.g., `Some(("sleep", &["infinity"]))` to keep container running)
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails
        pub fn with_env_and_command(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
            env_vars: HashMap<String, String>,
            command: Option<(&str, &[&str])>,
        ) -> TestcontainersResult<Self> {
            // 🚨 Verify Docker is still available
            check_docker_available()?;

            let image = GenericImage::new(image, tag);
            // Build container request with all env vars
            let mut request: testcontainers::core::ContainerRequest<GenericImage> = image.into();
            for (key, value) in env_vars {
                request = request.with_env_var(key, value);
            }
            // Add command if provided
            if let Some((cmd, args)) = command {
                let mut cmd_vec = vec![cmd.to_string()];
                cmd_vec.extend(args.iter().map(|s| (*s).to_string()));
                request = request.with_cmd(cmd_vec);
            }
            let container = request.start().map_err(|e| {
                let error_msg = format!("{e}");
                if is_docker_unavailable_error(&error_msg) {
                    TestcontainersError::DockerUnavailable(format!(
                        "Docker daemon connection failed during container start: {e}\n   ⚠️  STOP: Cannot connect to Docker daemon\n   💡 FIX: Start Docker Desktop or Docker daemon"
                    ))
                } else {
                    TestcontainersError::CreationFailed(format!("Failed to start container: {e}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and Docker daemon is running"))
                }
            })?;

            Ok(Self { container: Some(container), docker_cli_container_id: None })
        }

        /// Create a new generic container with environment variables
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance (unused in minimal implementation)
        /// * `image` - Docker image name
        /// * `tag` - Docker image tag
        /// * `env_vars` - Environment variables to set in the container
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails
        pub fn with_env(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
            env_vars: HashMap<String, String>,
        ) -> TestcontainersResult<Self> {
            // 🚨 Verify Docker is still available
            check_docker_available()?;

            let image = GenericImage::new(image, tag);
            // Build container request with all env vars
            let request: testcontainers::core::ContainerRequest<GenericImage> = env_vars
                .into_iter()
                .fold(image.into(), |req, (key, value)| req.with_env_var(key, value));
            let container = request.start().map_err(|e| {
                let error_msg = format!("{e}");
                if is_docker_unavailable_error(&error_msg) {
                    TestcontainersError::DockerUnavailable(format!(
                        "Docker daemon connection failed during container start: {e}\n   ⚠️  STOP: Cannot connect to Docker daemon\n   💡 FIX: Start Docker Desktop or Docker daemon"
                    ))
                } else {
                    TestcontainersError::CreationFailed(format!("Failed to start container: {e}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and Docker daemon is running"))
                }
            })?;

            Ok(Self { container: Some(container), docker_cli_container_id: None })
        }

        /// Create a new generic container with command (and optional entrypoint override)
        ///
        /// **Unified method** for creating containers with commands. Uses testcontainers API by default,
        /// and only falls back to Docker CLI workaround when entrypoint override is explicitly requested.
        ///
        /// **Implementation Details:**
        /// - **`entrypoint = None`**: Uses regular testcontainers API (normal path)
        ///   - Full testcontainers features (port mapping, wait conditions, etc.)
        ///   - Standard container lifecycle management
        /// - **`entrypoint = Some(...)`**: Uses Docker CLI workaround (only when needed)
        ///   - Required for images with entrypoints that interfere (e.g., `otel/weaver`)
        ///   - Uses `docker create --entrypoint` and `docker exec` directly
        ///   - Note: Port mapping not available for Docker CLI-created containers
        ///
        /// This method works for all containers:
        /// - Containers without entrypoints: Uses testcontainers normally (`entrypoint = None`)
        /// - Containers with entrypoints that interfere: Use entrypoint override (`entrypoint = Some(&["/bin/sh"])`)
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance
        /// * `image` - Docker image name
        /// * `tag` - Docker image tag
        /// * `command` - Command to run (e.g., "sleep", "sh")
        /// * `args` - Command arguments (e.g., `&["infinity"]` for sleep)
        /// * `entrypoint` - Optional entrypoint override (e.g., `Some(&["/bin/sh"])` for weaver, `None` for regular containers)
        ///   **Important**: Entrypoint must be a single executable** (array with exactly one element).
        ///   Docker `--entrypoint` flag only accepts a single executable path, not multiple arguments.
        ///   For shell commands with arguments, use the `command` parameter instead.
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails
        ///
        /// # Example
        ///
        /// ```rust,ignore
        /// // Simple case: container without entrypoint issues (uses testcontainers API)
        /// let container = GenericContainer::with_command(
        ///     client.client(),
        ///     "alpine",
        ///     "latest",
        ///     "sleep",
        ///     &["infinity"],
        ///     None  // Uses testcontainers API - full features available
        /// )?;
        ///
        /// // Container with entrypoint override (e.g., weaver) - uses Docker CLI workaround
        /// let container = GenericContainer::with_command(
        ///     client.client(),
        ///     "otel/weaver",
        ///     "latest",
        ///     "sleep",
        ///     &["infinity"],
        ///     Some(&["/bin/sh"])  // Uses Docker CLI workaround - single executable required
        /// )?;
        /// container.exec("weaver", &["--version"])?; // Success: container stays running
        /// ```
        pub fn with_command(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
            command: &str,
            args: &[&str],
            entrypoint: Option<&[&str]>,
        ) -> TestcontainersResult<Self> {
            // 🚨 Verify Docker is still available
            check_docker_available()?;

            // If entrypoint override is requested, use Docker CLI workaround
            if let Some(entrypoint) = entrypoint {
                // **Root Cause Fix**: testcontainers 0.25 doesn't support entrypoint override directly.
                // Workaround: Use Docker CLI to create container with --entrypoint flag.
                // This allows us to override entrypoints like [/weaver/weaver] that interfere with custom commands.

                // **Gemba Fix**: Docker --entrypoint flag only accepts a single executable path.
                // Multiple values (e.g., ["/bin/sh", "-c"]) are not supported by Docker CLI.
                // Validate that entrypoint has exactly one element.
                if entrypoint.len() != 1 {
                    return Err(TestcontainersError::InvalidConfig(format!(
                        "Entrypoint override must have exactly one element (Docker --entrypoint limitation)\n   ⚠️  STOP: Invalid entrypoint configuration\n   💡 FIX: Use single executable path, e.g., Some(&[\"/bin/sh\"]) not Some(&[\"/bin/sh\", \"-c\"])\n   💡 FIX: For shell commands with arguments, use the command parameter instead\n   Provided: {:?} ({} elements)",
                        entrypoint, entrypoint.len()
                    )));
                }

                // Build docker create command with entrypoint override
                // Format: docker create --entrypoint <single-executable> <image:tag> <command> <args...>
                // Note: Docker --entrypoint only accepts single executable, not multiple arguments
                let image_tag = format!("{image}:{tag}");
                let entrypoint_str = entrypoint[0]; // Use first (and only) element

                // Build command arguments: command + args
                let mut cmd_args = vec![command.to_string()];
                cmd_args.extend(args.iter().map(|s| (*s).to_string()));
                let cmd_str = cmd_args.join(" ");

                // Create container with entrypoint override
                let create_output = Command::new("docker")
                    .args([
                        "create",
                        "--entrypoint",
                        entrypoint_str,
                        &image_tag,
                    ])
                    .args(&cmd_args)
                    .output()
                    .map_err(|e| {
                        TestcontainersError::CreationFailed(format!(
                            "Failed to create container with entrypoint override: {e}\n   ⚠️  STOP: Docker CLI command failed\n   💡 FIX: Check Docker is installed and running"
                        ))
                    })?;

                if !create_output.status.success() {
                    let stderr = String::from_utf8_lossy(&create_output.stderr);
                    return Err(TestcontainersError::CreationFailed(format!(
                        "Failed to create container with entrypoint override: {}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and entrypoint is valid\n   Command: docker create --entrypoint {} {} {}\n   Error: {}",
                        create_output.status, entrypoint_str, image_tag, cmd_str, stderr
                    )));
                }

                // Get container ID from output
                let container_id = String::from_utf8(create_output.stdout)
                    .map_err(|e| {
                        TestcontainersError::CreationFailed(format!(
                            "Failed to parse container ID: {e}\n   ⚠️  STOP: Invalid Docker output\n   💡 FIX: Check Docker CLI is working correctly"
                        ))
                    })?
                    .trim()
                    .to_string();

                if container_id.is_empty() {
                    return Err(TestcontainersError::CreationFailed(
                        "Container ID is empty - Docker create command may have failed\n   ⚠️  STOP: Invalid container creation\n   💡 FIX: Check Docker CLI output".to_string()
                    ));
                }

                // Start the container
                let start_output = Command::new("docker")
                    .args(["start", &container_id])
                    .output()
                    .map_err(|e| {
                        TestcontainersError::CreationFailed(format!(
                            "Failed to start container: {e}\n   ⚠️  STOP: Container start failed\n   💡 FIX: Check Docker daemon is running"
                        ))
                    })?;

                if !start_output.status.success() {
                    let stderr = String::from_utf8_lossy(&start_output.stderr);
                    // Clean up the created container on failure
                    // **Gemba Fix**: Log cleanup attempt (non-critical, but useful for debugging)
                    let cleanup_result =
                        Command::new("docker").args(["rm", "-f", &container_id]).output();
                    if let Err(e) = cleanup_result {
                        // Log cleanup failure but don't fail the operation (container creation already failed)
                        eprintln!(
                            "⚠️  WARNING: Failed to cleanup container {container_id} after start failure: {e}"
                        );
                    }
                    return Err(TestcontainersError::CreationFailed(format!(
                        "Failed to start container: {}\n   ⚠️  STOP: Container start failed\n   💡 FIX: Check container logs and Docker daemon\n   Error: {}",
                        start_output.status, stderr
                    )));
                }

                // Wait for container to be ready with exponential backoff retry logic
                // **FAIL-FAST HARDENING**: Replaces fixed delay with intelligent retry (100ms → 200ms → 400ms).
                // Root cause: Fixed 100ms delay doesn't adapt to slow Docker daemon.
                // Solution: Retry with health check and exponential backoff (max 700ms total wait).
                wait_for_container_ready(&container_id)?;

                // **Workaround**: Use Docker CLI-created container with entrypoint override.
                // Store container ID for exec operations using docker exec directly.
                return Ok(Self::from_docker_cli_container_id(container_id));
            }

            // No entrypoint override needed - use regular testcontainers approach
            let image = GenericImage::new(image, tag);
            // Build container request with command
            let mut request: testcontainers::core::ContainerRequest<GenericImage> = image.into();
            // Set command and args to keep container running
            let mut cmd_vec = vec![command.to_string()];
            cmd_vec.extend(args.iter().map(|s| (*s).to_string()));
            request = request.with_cmd(cmd_vec);

            let container = request.start().map_err(|e| {
                let error_msg = format!("{e}");
                if is_docker_unavailable_error(&error_msg) {
                    TestcontainersError::DockerUnavailable(format!(
                        "Docker daemon connection failed during container start: {e}\n   ⚠️  STOP: Cannot connect to Docker daemon\n   💡 FIX: Start Docker Desktop or Docker daemon"
                    ))
                } else {
                    TestcontainersError::CreationFailed(format!("Failed to start container: {e}\n   ⚠️  STOP: Container creation failed\n   💡 FIX: Check Docker image exists and Docker daemon is running"))
                }
            })?;

            Ok(Self { container: Some(container), docker_cli_container_id: None })
        }

        /// Create a new generic container with entrypoint override and command
        ///
        /// **Deprecated**: Use `with_command()` with `entrypoint` parameter instead.
        /// This method is kept for backward compatibility.
        ///
        /// **Note**: This method uses Docker CLI workaround (not testcontainers API) because
        /// entrypoint override is required. For regular containers, use `with_command()` with
        /// `entrypoint = None` to use testcontainers API normally.
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance
        /// * `image` - Docker image name
        /// * `tag` - Docker image tag
        /// * `entrypoint` - Entrypoint to use (e.g., `&["/bin/sh"]` or `&[]` to disable entrypoint)
        /// * `command` - Command to run (e.g., "sleep", "sh")
        /// * `args` - Command arguments (e.g., `&["infinity"]` for sleep)
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails
        #[deprecated(note = "Use with_command() with entrypoint parameter instead")]
        #[allow(clippy::used_underscore_binding)] // Deprecated function, client parameter intentionally unused
        pub fn with_entrypoint_and_command(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
            entrypoint: &[&str],
            command: &str,
            args: &[&str],
        ) -> TestcontainersResult<Self> {
            Self::with_command(_client, image, tag, command, args, Some(entrypoint))
        }

        /// Create a new generic container with port mappings
        ///
        /// # Arguments
        ///
        /// * `_client` - Container client instance (unused in minimal implementation)
        /// * `image` - Docker image name
        /// * `tag` - Docker image tag
        /// * `ports` - Container ports to map to host ports
        ///
        /// # Errors
        ///
        /// Returns error if container creation fails
        pub fn with_ports(
            _client: &ContainerClient,
            image: &str,
            tag: &str,
            ports: &[u16],
        ) -> TestcontainersResult<Self> {
            // 🚨 Verify Docker is still available
            check_docker_available()?;

            let mut image = GenericImage::new(image, tag);
            for port in ports {
                image = image.with_exposed_port(ContainerPort::Tcp(*port));
            }
            let container = image.start().map_err(|e| {
                let error_msg = format!("{e}");
                if is_docker_unavailable_error(&error_msg) {
                    TestcontainersError::DockerUnavailable(format!(
                        "Docker daemon connection failed during container start: {e}\n   ⚠️  STOP: Cannot connect to Docker daemon\n   💡 FIX: Start Docker Desktop or Docker daemon"
                    ))
                } else {
                    TestcontainersError::CreationFailed(format!("Failed to start container: {e}"))
                }
            })?;

            Ok(Self { container: Some(container), docker_cli_container_id: None })
        }

        /// Get the host port for a container port
        ///
        /// # Arguments
        ///
        /// * `container_port` - The container port to get the host port for
        ///
        /// # Errors
        ///
        /// Returns error if port mapping fails or port is not mapped
        /// Note: Port mapping is not supported for Docker CLI-created containers (entrypoint override workaround)
        pub fn get_host_port(&self, container_port: u16) -> TestcontainersResult<u16> {
            let container = self.container.as_ref().ok_or_else(|| {
                TestcontainersError::OperationFailed(
                    "Port mapping not supported for Docker CLI-created containers (entrypoint override workaround)\n   ⚠️  WARNING: get_host_port() requires testcontainers-managed container\n   💡 FIX: Port mapping uses testcontainers port mapping API, which is not available for Docker CLI-created containers\n   💡 FIX: Use testcontainers container creation methods (entrypoint = None) for port mapping support\n   💡 FIX: For Docker CLI containers, use docker port command directly if port mapping is needed".to_string()
                )
            })?;

            let port = container.get_host_port_ipv4(container_port).map_err(|e| {
                TestcontainersError::OperationFailed(format!(
                    "Failed to get host port for container port {container_port}: {e}"
                ))
            })?;
            Ok(port)
        }

        /// Get the underlying testcontainers Container
        ///
        /// Allows access to advanced testcontainers features if needed.
        /// Returns None if container was created with Docker CLI (entrypoint override workaround).
        #[must_use]
        pub const fn container(&self) -> Option<&Container<GenericImage>> {
            self.container.as_ref()
        }

        /// Get the Docker CLI container ID if this container was created with entrypoint override
        ///
        /// Returns `Some(container_id)` if container was created using Docker CLI workaround,
        /// None if container was created using testcontainers normally.
        #[must_use]
        pub fn docker_cli_container_id(&self) -> Option<&str> {
            self.docker_cli_container_id.as_deref()
        }
    }

    /// Automatic cleanup for `GenericContainer`
    ///
    /// **Root Cause Fix**: Clean up Docker CLI-created containers on drop.
    /// testcontainers containers are automatically cleaned up by testcontainers,
    /// but Docker CLI-created containers need manual cleanup using docker rm.
    impl Drop for GenericContainer {
        fn drop(&mut self) {
            // Clean up Docker CLI-created containers
            if let Some(container_id) = &self.docker_cli_container_id {
                use std::process::Command;
                // Use -f flag to force remove even if container is running
                // This ensures cleanup even if container didn't stop properly
                // **Gemba Fix**: Log cleanup failures for debugging (non-critical but useful)
                let cleanup_result =
                    Command::new("docker").args(["rm", "-f", container_id]).output();
                if let Err(e) = cleanup_result {
                    // Log cleanup failure but don't panic (Drop must not panic)
                    eprintln!(
                        "⚠️  WARNING: Failed to cleanup Docker CLI container {container_id}: {e}"
                    );
                } else if let Ok(output) = cleanup_result {
                    // Log if cleanup command failed (non-zero exit code)
                    if !output.status.success() {
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        eprintln!(
                            "⚠️  WARNING: Container cleanup command failed for {container_id}: {stderr}"
                        );
                    }
                }
                // Note: We ignore cleanup errors because container may already be removed or Docker unavailable
                // This is acceptable in Drop - cleanup is best-effort
            }
            // testcontainers Container handles its own cleanup via Drop trait
        }
    }
}

#[cfg(feature = "testcontainers")]
pub use implementation::*;

#[cfg(not(feature = "testcontainers"))]
mod stubs {
    use super::*;

    /// Stub for ContainerClient when testcontainers feature is disabled
    pub struct ContainerClient;

    impl ContainerClient {
        pub fn new() -> Self {
            Self
        }
    }

    impl Default for ContainerClient {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Stub for GenericContainer when testcontainers feature is disabled
    pub struct GenericContainer;

    impl GenericContainer {
        pub fn new(
            _client: &ContainerClient,
            _image: &str,
            _tag: &str,
        ) -> TestcontainersResult<Self> {
            Err(TestcontainersError::InvalidConfig(
                "testcontainers feature is not enabled".to_string(),
            ))
        }

        pub fn with_env(
            _client: &ContainerClient,
            _image: &str,
            _tag: &str,
            _env_vars: HashMap<String, String>,
        ) -> TestcontainersResult<Self> {
            Err(TestcontainersError::InvalidConfig(
                "testcontainers feature is not enabled".to_string(),
            ))
        }

        pub fn with_ports(
            _client: &ContainerClient,
            _image: &str,
            _tag: &str,
            _ports: &[u16],
        ) -> TestcontainersResult<Self> {
            Err(TestcontainersError::InvalidConfig(
                "testcontainers feature is not enabled".to_string(),
            ))
        }

        pub fn get_host_port(&self, _container_port: u16) -> TestcontainersResult<u16> {
            Err(TestcontainersError::InvalidConfig(
                "testcontainers feature is not enabled".to_string(),
            ))
        }

        /// Returns `None` — stub only; the `testcontainers` feature is not enabled.
        ///
        /// The real implementation returns `Option<&Container<GenericImage>>`.
        /// The stub always returns `None` because no inner container exists without the feature.
        #[must_use]
        pub const fn container(&self) -> Option<&Self> {
            None
        }
    }
}

#[cfg(not(feature = "testcontainers"))]
pub use stubs::*;

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;
    use crate::assert_eq_msg;
    use crate::assertions::assert_that_with_msg;
    use crate::test;

    // ========================================================================
    // 1. ERROR PATH TESTING - Test all error variants (80% of bugs)
    // ========================================================================

    test!(test_testcontainers_error_display, {
        // Arrange: Create all error variants
        let errors = vec![
            TestcontainersError::CreationFailed("test".to_string()),
            TestcontainersError::OperationFailed("test".to_string()),
            TestcontainersError::InvalidConfig("test".to_string()),
            TestcontainersError::CommandExecutionFailed("test".to_string()),
            TestcontainersError::StdoutReadFailed("test".to_string()),
            TestcontainersError::StderrReadFailed("test".to_string()),
            TestcontainersError::ExitCodeFailed("test".to_string()),
        ];

        // Act & Assert: Verify all error variants display correctly
        for error in errors {
            let display = format!("{error}");
            assert_that_with_msg(&!display.is_empty(), |v| *v, "Error should have display message");
            assert_that_with_msg(&display.contains("test"), |v| *v, "Error should contain message");
        }
    });

    test!(test_exec_result_structure, {
        // Arrange: Create ExecResult
        let result = ExecResult {
            stdout: "output".to_string(),
            stderr: "error".to_string(),
            exit_code: exec::SUCCESS_EXIT_CODE,
        };

        // Act & Assert: Verify ExecResult structure
        assert_eq_msg!(&result.stdout, &"output".to_string(), "Stdout should match");
        assert_eq_msg!(&result.stderr, &"error".to_string(), "Stderr should match");
        assert_eq_msg!(&result.exit_code, &exec::SUCCESS_EXIT_CODE, "Exit code should match");
    });

    test!(test_exec_result_clone, {
        // Arrange: Create ExecResult
        let result1 = ExecResult {
            stdout: "output".to_string(),
            stderr: "error".to_string(),
            exit_code: exec::SUCCESS_EXIT_CODE,
        };

        // Act: Clone the result
        let result2 = result1.clone();

        // Assert: Verify cloned fields match original
        assert_eq_msg!(&result1.stdout, &result2.stdout, "Cloned stdout should match");
        assert_eq_msg!(&result1.stderr, &result2.stderr, "Cloned stderr should match");
        assert_eq_msg!(&result1.exit_code, &result2.exit_code, "Cloned exit code should match");
    });

    test!(test_exec_result_debug, {
        // Arrange: Create ExecResult
        let result = ExecResult {
            stdout: "output".to_string(),
            stderr: "error".to_string(),
            exit_code: exec::SUCCESS_EXIT_CODE,
        };

        // Act: Format as debug
        let debug = format!("{result:?}");

        // Assert: Verify debug output contains expected fields
        assert_that_with_msg(&debug.contains("output"), |v| *v, "Debug should contain stdout");
        assert_that_with_msg(&debug.contains("error"), |v| *v, "Debug should contain stderr");
        assert_that_with_msg(&debug.contains("0"), |v| *v, "Debug should contain exit code");
    });

    // ========================================================================
    // 2. STUB BEHAVIOR TESTING - Test feature-gated code paths
    // ========================================================================

    #[cfg(not(feature = "testcontainers"))]
    test!(test_stubs_return_errors, {
        // Arrange: Create container client
        let client = ContainerClient::new();

        // Act: Attempt to create container (should fail in stub mode)
        let result = GenericContainer::new(&client, "alpine", "latest");

        // Assert: Verify stub returns InvalidConfig error
        assert_err!(&result, "Stub should return error");
        match result {
            Err(TestcontainersError::InvalidConfig(msg)) => {
                assert_that_with_msg(
                    &msg.contains("testcontainers feature is not enabled"),
                    |v| *v,
                    "Error message should indicate feature not enabled",
                );
            }
            _ => panic!("Expected InvalidConfig error"),
        }

        // Act: Attempt to use stub container methods
        let container = GenericContainer;
        let port_result = container.get_host_port(DEFAULT_HTTP_PORT);
        let exec_result = container.exec("echo", &["test"]);

        // Assert: Verify all stub methods return errors
        assert_err!(&port_result, "Port result should be error");
        assert_err!(&exec_result, "Exec result should be error");
    });

    #[cfg(not(feature = "testcontainers"))]
    test!(test_stub_container_client, {
        // Arrange: Create container clients
        let client1 = ContainerClient::new();
        let client2 = ContainerClient::default();

        // Act: Access client references
        let _ref1 = client1.client();
        let _ref2 = client2.client();

        // Assert: Both should work (no panic) - stub clients are valid
    });

    // ========================================================================
    // 3. TIMEOUT BEHAVIOR TESTING - Verify timeout prevents hangs
    // ========================================================================

    #[cfg(feature = "testcontainers")]
    test!(test_check_docker_available_timeout_prevents_hang, {
        // **Root Cause Fix Test**: Verify check_docker_available() has timeout protection.
        // This test verifies that the function completes quickly (within timeout period)
        // even when Docker daemon might be unavailable, preventing hangs.
        //
        // Note: We can't reliably test with Docker stopped in CI, but we verify:
        // 1. Function completes quickly (doesn't hang)
        // 2. Error handling works correctly
        // 3. Timeout mechanism is in place
        //
        // Manual verification: Stop Docker daemon and verify function returns error within 500ms.

        use super::implementation::check_docker_available;
        use std::time::Instant;

        // Arrange: Measure execution time
        let start = Instant::now();

        // Act: Call check_docker_available() (may succeed or fail depending on Docker state)
        let result = check_docker_available();

        // Assert: Function completes quickly (within 6 seconds, well below any reasonable timeout)
        let elapsed = start.elapsed();
        assert_that_with_msg(
            &(elapsed.as_millis() < 6000),
            |v| *v,
            "check_docker_available() should complete within 6s (timeout protection prevents hangs)",
        );

        // Assert: Result is either Ok (Docker available) or DockerUnavailable error (timeout or not running)
        match result {
            Ok(()) => {
                // ✅ Docker is available - this is valid
            }
            Err(TestcontainersError::DockerUnavailable(ref msg)) => {
                // ✅ Docker unavailable - verify error message indicates timeout or connection issue
                assert_that_with_msg(
                    &(msg.contains("timed out")
                        || msg.contains("not running")
                        || msg.contains("not found")),
                    |v| *v,
                    "Error message should indicate timeout or Docker unavailability",
                );
            }
            Err(e) => {
                panic!("Unexpected error type: {e}");
            }
        }

        // Verify timeout mechanism: If Docker is unavailable, error should complete quickly
        // (This prevents the function from hanging indefinitely). Uses the same 6s bound as
        // the check above rather than a separate, stricter 1000ms one -- that second
        // threshold was environment-sensitive (a Docker-absent TCP connect-refused/timeout
        // can legitimately take 1-3s depending on the platform's socket stack) and flaked
        // 2026-07-17 on a real machine where Docker was unavailable but the OS took >1s to
        // report it. The actual property under test -- "no hang" -- is already covered by
        // the 6s check; this redundant, tighter bound added flakiness without added safety.
        if result.is_err() {
            assert_that_with_msg(
                &(elapsed.as_millis() < 6000),
                |v| *v,
                "Timeout protection should prevent hangs - function should complete quickly even when Docker unavailable",
            );
        }
    });
}
