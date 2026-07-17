//! Poka-Yoke Types for Testcontainers
//!
//! Provides type-level error prevention for container operations.
//! Uses Rust's type system to make invalid container states unrepresentable.
//!
//! **Poka-Yoke Principles**:
//! - **Make invalid states unrepresentable**: Use types to prevent errors
//! - **Type-level prevention**: Invalid container operations cannot be called
//! - **State machine**: Container lifecycle enforced by types
//! - **Docker availability**: Checked at compile-time where possible, runtime where necessary
//!
//! # Error Modes Prevented
//!
//! 1. **Exec on stopped container**: Cannot execute commands on stopped containers
//! 2. **Port mapping on stopped container**: Cannot get ports from stopped containers
//! 3. **Double start**: Cannot start already-started container
//! 4. **Docker unavailable**: Checked at client creation
//! 5. **Invalid configuration**: Validated at container creation

#[cfg(feature = "testcontainers")]
use std::marker::PhantomData;

#[cfg(feature = "testcontainers")]
/// Container state marker types
///
/// **Poka-yoke**: Phantom types prevent invalid operations at compile time.
/// A container is either `Stopped` or `Running` - cannot be both.
pub mod state {
    /// Container is stopped (initial state)
    pub struct Stopped;

    /// Container is running (can execute commands)
    pub struct Running;
}

#[cfg(feature = "testcontainers")]
/// Container with type-level state
///
/// **Poka-yoke**: Type parameter `S` prevents invalid operations.
/// - `Container<Stopped>`: Can only start, cannot exec
/// - `Container<Running>`: Can exec, can stop
///
/// # Example
///
/// ```rust,ignore
/// use chicago_tdd_tools::testcontainers::poka_yoke::*;
///
/// // Create stopped container
/// let container: Container<Stopped> = Container::new(...)?;
///
/// // Start container (changes type to Running, performs real Docker start)
/// let container: Container<Running> = container.start()?;
///
/// // Can exec on running container
/// let result = container.exec("echo", &["hello"])?;
///
/// // Stop container (changes type back to Stopped)
/// let container: Container<Stopped> = container.stop()?;
///
/// // Compile error: Cannot exec on stopped container
/// // container.exec("echo", &["hello"])?; // ERROR!
/// ```
pub struct Container<S> {
    /// Image name (e.g. "alpine")
    image: String,
    /// Image tag (e.g. "latest")
    tag: String,
    /// Client reference used to spawn real containers
    client: crate::testcontainers::ContainerClient,
    /// Running container handle — present only in the `Running` state
    running: Option<crate::testcontainers::GenericContainer>,
    /// State marker (compile-time guarantee)
    _state: PhantomData<S>,
}

#[cfg(feature = "testcontainers")]
impl Container<state::Stopped> {
    /// Create a new stopped container type-safe handle.
    ///
    /// **Poka-yoke**: Returns `Container<Stopped>` - cannot exec until started.
    ///
    /// Validates that `image` and `tag` are non-empty before constructing
    /// the handle. Call `start()` to launch the real Docker container.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `image` or `tag` is empty.
    pub fn new(
        client: crate::testcontainers::ContainerClient, image: &str, tag: &str,
    ) -> crate::testcontainers::TestcontainersResult<Self> {
        if image.trim().is_empty() {
            return Err(crate::testcontainers::TestcontainersError::OperationFailed(
                "image name must not be empty".to_string(),
            ));
        }
        if tag.trim().is_empty() {
            return Err(crate::testcontainers::TestcontainersError::OperationFailed(
                "image tag must not be empty".to_string(),
            ));
        }
        Ok(Self {
            image: image.to_string(),
            tag: tag.to_string(),
            client,
            running: None,
            _state: PhantomData,
        })
    }

    /// Start the container, performing real Docker I/O.
    ///
    /// **Poka-yoke**: Changes type from `Container<Stopped>` to `Container<Running>`.
    /// After this call the type permits exec and port-mapping calls.
    ///
    /// The container is kept alive with `sleep infinity` so that subsequent
    /// `exec()` calls succeed. Use `stop()` to terminate it and reclaim the
    /// `Container<Stopped>` handle.
    ///
    /// # Errors
    ///
    /// Returns `Err` if Docker is unavailable or the image cannot be pulled/started.
    pub fn start(self) -> crate::testcontainers::TestcontainersResult<Container<state::Running>> {
        let generic = crate::testcontainers::GenericContainer::with_command(
            self.client.client(),
            &self.image,
            &self.tag,
            "sleep",
            &["infinity"],
            None,
        )?;
        Ok(Container {
            image: self.image,
            tag: self.tag,
            client: self.client,
            running: Some(generic),
            _state: PhantomData,
        })
    }
}

#[cfg(feature = "testcontainers")]
impl Container<state::Running> {
    /// Execute a command in the running container.
    ///
    /// **Poka-yoke**: Only available on `Container<Running>`.
    /// Compiler prevents calling this on stopped containers.
    ///
    /// # Errors
    ///
    /// Returns error if command execution fails or the inner container handle
    /// is unexpectedly absent (indicates an internal logic bug).
    pub fn exec(
        &self, command: &str, args: &[&str],
    ) -> crate::testcontainers::TestcontainersResult<crate::testcontainers::exec::ExecResult> {
        let container = self.running.as_ref().ok_or_else(|| {
            crate::testcontainers::TestcontainersError::OperationFailed(
                "Running container handle is missing — this is an internal bug.".to_string(),
            )
        })?;
        container.exec(command, args)
    }

    /// Get host port for a container port.
    ///
    /// **Poka-yoke**: Only available on `Container<Running>`.
    /// Compiler prevents calling this on stopped containers.
    ///
    /// # Errors
    ///
    /// Returns error if port mapping fails or the port is not exposed.
    pub fn get_host_port(
        &self, container_port: u16,
    ) -> crate::testcontainers::TestcontainersResult<u16> {
        let container = self.running.as_ref().ok_or_else(|| {
            crate::testcontainers::TestcontainersError::OperationFailed(
                "Running container handle is missing — this is an internal bug.".to_string(),
            )
        })?;
        container.get_host_port(container_port)
    }

    /// Stop the running container, performing real Docker I/O.
    ///
    /// **Poka-yoke**: Changes type from `Container<Running>` to `Container<Stopped>`.
    /// After this call the type forbids exec and port-mapping calls at compile time.
    ///
    /// Dropping the inner `GenericContainer` triggers automatic cleanup via its
    /// `Drop` implementation.
    ///
    /// # Errors
    ///
    /// Currently infallible; the inner container is dropped (and Docker cleanup
    /// happens automatically). Returns `Ok` once cleanup is complete.
    #[allow(clippy::unnecessary_wraps)] // Public API — callers use `?`; future Docker stop I/O may return Err
    pub fn stop(self) -> crate::testcontainers::TestcontainersResult<Container<state::Stopped>> {
        // Dropping `running` stops and removes the Docker container via Drop.
        drop(self.running);
        Ok(Container {
            image: self.image,
            tag: self.tag,
            client: self.client,
            running: None,
            _state: PhantomData,
        })
    }
}

#[cfg(feature = "testcontainers")]
/// Validated container configuration
///
/// **Poka-yoke**: Newtype prevents invalid configurations.
/// The type system makes invalid container configs impossible.
#[derive(Debug, Clone)]
pub struct ValidContainerConfig {
    /// Image name (always non-empty)
    image: String,
    /// Image tag (always non-empty)
    tag: String,
    /// Command (optional) - currently unused, reserved for future use
    _command: Option<String>,
    /// Command args (optional) - currently unused, reserved for future use
    _args: Option<Vec<String>>,
}

#[cfg(feature = "testcontainers")]
impl ValidContainerConfig {
    /// Create a new validated container configuration
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid configs:
    /// - Empty image: Returns `None`
    /// - Empty tag: Returns `None`
    ///
    /// # Errors
    ///
    /// Returns `None` if image or tag is empty.
    #[must_use]
    pub fn new(image: &str, tag: &str) -> Option<Self> {
        if image.trim().is_empty() || tag.trim().is_empty() {
            return None;
        }

        Some(Self {
            image: image.to_string(),
            tag: tag.to_string(),
            _command: None,
            _args: None,
        })
    }

    /// Get image name
    #[must_use]
    pub fn image(&self) -> &str {
        &self.image
    }

    /// Get image tag
    #[must_use]
    pub fn tag(&self) -> &str {
        &self.tag
    }
}

#[cfg(all(test, feature = "testcontainers"))]
mod tests {
    use super::*;

    #[test]
    fn test_valid_container_config() {
        let config = ValidContainerConfig::new("alpine", "latest");
        assert!(config.is_some());
        if let Some(c) = config {
            assert_eq!(c.image(), "alpine");
            assert_eq!(c.tag(), "latest");
        }
    }

    #[test]
    fn test_invalid_container_config_empty_image() {
        let config = ValidContainerConfig::new("", "latest");
        assert!(config.is_none()); // Type prevents empty image
    }

    #[test]
    fn test_invalid_container_config_empty_tag() {
        let config = ValidContainerConfig::new("alpine", "");
        assert!(config.is_none()); // Type prevents empty tag
    }
}
