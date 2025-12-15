//! Testcontainers Support
//!
//! Provides generic container support for integration testing with Docker.
//! Follows Chicago TDD principles by using real containers instead of mocks.
//!
//! ## Features
//!
//! - **Generic Containers**: Support for any Docker image
//! - **HTTP Servers**: Pre-configured HTTP server containers
//! - **Static File Servers**: Containers for serving test files
//! - **Automatic Cleanup**: Integration with TestFixture for automatic cleanup
//!
//! ## Chicago TDD Alignment
//!
//! - **Real Collaborators**: Actual Docker containers, not mocks
//! - **State Verification**: Verify container state and HTTP responses
//! - **Automatic Cleanup**: Containers cleaned up via TestFixture Drop
//! - **AAA Pattern**: Arrange (start container), Act (test), Assert (verify state)

use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

/// Testcontainers error type
#[derive(Error, Debug)]
pub enum TestcontainersError {
    /// Failed to create container
    #[error("Failed to create container: {0}")]
    CreationFailed(String),
    /// Container operation failed
    #[error("Container operation failed: {0}")]
    OperationFailed(String),
    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

/// Result type for testcontainers operations
pub type TestcontainersResult<T> = Result<T, TestcontainersError>;

#[cfg(feature = "testcontainers")]
mod implementation {
    use super::*;
    use testcontainers::clients::Cli;
    use testcontainers::core::{WaitFor, RunnableImage};
    use testcontainers::images::generic::GenericImage;
    use testcontainers::Container;

    /// Container client for managing Docker containers
    pub struct ContainerClient {
        client: Cli,
    }

    impl ContainerClient {
        /// Create a new container client
        pub fn new() -> Self {
            Self {
                client: Cli::default(),
            }
        }

        /// Get the underlying Docker client
        pub fn client(&self) -> &Cli {
            &self.client
        }
    }

    impl Default for ContainerClient {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Generic container wrapper
    pub struct GenericContainer {
        container: Container<'static, GenericImage>,
        exposed_ports: Vec<u16>,
        base_url: Option<String>,
    }

    impl GenericContainer {
        /// Create a new generic container
        pub fn new(
            client: &Cli,
            image: &str,
            tag: &str,
        ) -> TestcontainersResult<Self> {
            let generic_image = GenericImage::new(image, tag);
            let container = client.run(generic_image);

            Ok(Self {
                container,
                exposed_ports: Vec::new(),
                base_url: None,
            })
        }

        /// Get the underlying container
        pub fn container(&self) -> &Container<'static, GenericImage> {
            &self.container
        }

        /// Get host port for container port
        pub fn get_host_port(&self, container_port: u16) -> u16 {
            self.container.get_host_port_ipv4(container_port)
        }

        /// Set exposed ports
        pub fn with_exposed_ports(mut self, ports: Vec<u16>) -> Self {
            self.exposed_ports = ports;
            self
        }

        /// Set base URL
        pub fn with_base_url(mut self, url: String) -> Self {
            self.base_url = Some(url);
            self
        }

        /// Get base URL
        pub fn base_url(&self) -> Option<&str> {
            self.base_url.as_deref()
        }
    }

    /// HTTP server container builder
    pub struct HttpServerContainerBuilder {
        image: String,
        tag: String,
        port: u16,
        env_vars: HashMap<String, String>,
        wait_for: Option<WaitFor>,
    }

    impl HttpServerContainerBuilder {
        /// Create a new HTTP server container builder
        pub fn new(image: &str, tag: &str) -> Self {
            Self {
                image: image.to_string(),
                tag: tag.to_string(),
                port: 80,
                env_vars: HashMap::new(),
                wait_for: None,
            }
        }

        /// Set the HTTP port
        pub fn with_port(mut self, port: u16) -> Self {
            self.port = port;
            self
        }

        /// Add environment variable
        pub fn with_env_var(mut self, key: &str, value: &str) -> Self {
            self.env_vars.insert(key.to_string(), value.to_string());
            self
        }

        /// Set wait condition
        pub fn with_wait_for(mut self, wait_for: WaitFor) -> Self {
            self.wait_for = Some(wait_for);
            self
        }

        /// Build the HTTP server container
        pub fn build(
            self,
            client: &Cli,
        ) -> TestcontainersResult<HttpServerContainer> {
            let mut image = GenericImage::new(&self.image, &self.tag)
                .with_exposed_port(self.port);

            // Add environment variables
            for (key, value) in &self.env_vars {
                image = image.with_env_var(key, value);
            }

            // Add wait condition if provided
            if let Some(wait_for) = self.wait_for {
                image = image.with_wait_for(wait_for);
            }

            let container = client.run(image);
            let host_port = container.get_host_port_ipv4(self.port);
            let base_url = format!("http://localhost:{}", host_port);

            Ok(HttpServerContainer {
                container: GenericContainer {
                    container,
                    exposed_ports: vec![self.port],
                    base_url: Some(base_url.clone()),
                },
                base_url,
            })
        }
    }

    /// HTTP server container
    pub struct HttpServerContainer {
        container: GenericContainer,
        base_url: String,
    }

    impl HttpServerContainer {
        /// Get the base URL
        pub fn base_url(&self) -> &str {
            &self.base_url
        }

        /// Get the underlying container
        pub fn container(&self) -> &GenericContainer {
            &self.container
        }

        /// Get host port
        pub fn host_port(&self) -> u16 {
            self.container.get_host_port(80)
        }
    }

    /// Static file server container builder
    pub struct StaticFileServerContainerBuilder {
        image: String,
        tag: String,
        port: u16,
        mount_path: Option<String>,
        source_path: Option<String>,
    }

    impl StaticFileServerContainerBuilder {
        /// Create a new static file server container builder
        pub fn new(image: &str, tag: &str) -> Self {
            Self {
                image: image.to_string(),
                tag: tag.to_string(),
                port: 80,
                mount_path: None,
                source_path: None,
            }
        }

        /// Set the HTTP port
        pub fn with_port(mut self, port: u16) -> Self {
            self.port = port;
            self
        }

        /// Mount a directory
        pub fn mount_directory<P: AsRef<Path>>(
            mut self,
            mount_path: &str,
            source_path: P,
        ) -> Self {
            self.mount_path = Some(mount_path.to_string());
            self.source_path = Some(
                source_path
                    .as_ref()
                    .to_string_lossy()
                    .to_string(),
            );
            self
        }

        /// Build the static file server container
        pub fn build(
            self,
            client: &Cli,
        ) -> TestcontainersResult<StaticFileServerContainer> {
            let mut image = GenericImage::new(&self.image, &self.tag)
                .with_exposed_port(self.port);

            // Mount directory if provided
            if let (Some(mount_path), Some(source_path)) =
                (&self.mount_path, &self.source_path)
            {
                image = image.with_volume(mount_path, source_path);
            }

            let container = client.run(image);
            let host_port = container.get_host_port_ipv4(self.port);
            let base_url = format!("http://localhost:{}", host_port);

            Ok(StaticFileServerContainer {
                container: GenericContainer {
                    container,
                    exposed_ports: vec![self.port],
                    base_url: Some(base_url.clone()),
                },
                base_url,
                mount_path: self.mount_path,
            })
        }
    }

    /// Static file server container
    pub struct StaticFileServerContainer {
        container: GenericContainer,
        base_url: String,
        mount_path: Option<String>,
    }

    impl StaticFileServerContainer {
        /// Get the base URL
        pub fn base_url(&self) -> &str {
            &self.base_url
        }

        /// Get the underlying container
        pub fn container(&self) -> &GenericContainer {
            &self.container
        }

        /// Get host port
        pub fn host_port(&self) -> u16 {
            self.container.get_host_port(80)
        }

        /// Get file URL for a path
        pub fn file_url(&self, path: &str) -> String {
            format!("{}/{}", self.base_url, path.trim_start_matches('/'))
        }
    }

    /// Trait for container cleanup
    pub trait ContainerCleanup: Send + Sync {
        /// Cleanup the container
        fn cleanup(&self);
    }

    impl ContainerCleanup for GenericContainer {
        fn cleanup(&self) {
            // Container is automatically cleaned up when dropped
            // This is a no-op, but provides the trait interface
        }
    }

    impl ContainerCleanup for HttpServerContainer {
        fn cleanup(&self) {
            self.container.cleanup();
        }
    }

    impl ContainerCleanup for StaticFileServerContainer {
        fn cleanup(&self) {
            self.container.cleanup();
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
}

#[cfg(not(feature = "testcontainers"))]
pub use stubs::*;















