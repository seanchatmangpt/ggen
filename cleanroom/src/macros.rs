//! Macros for reducing repetition in container implementations
//!
//! This module provides macros to eliminate repetitive patterns in container
//! implementations following core team best practices.

/// Macro to implement ContainerWrapper trait with common patterns
///
/// This macro eliminates the repetitive ContainerWrapper implementations
/// across different container types by providing sensible defaults.
///
/// # Usage
///
/// ```rust
/// impl_container_wrapper! {
///     PostgresContainer {
///         name: "postgres",
///         cpu_usage: 5.0,
///         memory_mb: 128,
///         disk_mb: 64,
///     }
/// }
/// ```
#[macro_export]
macro_rules! impl_container_wrapper {
    (
        $container_type:ident {
            name: $name:expr,
            cpu_usage: $cpu:expr,
            memory_mb: $memory:expr,
            disk_mb: $disk:expr,
        }
    ) => {
        impl ContainerWrapper for $container_type {
            fn name(&self) -> &str {
                $name
            }

            fn status(&self) -> ContainerStatus {
                // For testcontainers, the container is managed externally
                // In a production implementation, you'd check the actual container status
                ContainerStatus::Running
            }

            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn metrics(&self) -> ContainerMetrics {
                // Return current container metrics
                // In a production implementation, you'd get actual container metrics from Docker
                ContainerMetrics {
                    cpu_usage_percent: $cpu,
                    memory_usage_bytes: $memory * 1024 * 1024,
                    network_bytes_sent: 0,
                    network_bytes_received: 0,
                    disk_usage_bytes: $disk * 1024 * 1024,
                    uptime_seconds: self.start_time.elapsed().as_secs(),
                }
            }

            fn cleanup(&self) -> Result<()> {
                // Container cleanup is handled automatically by testcontainers when dropped
                Ok(())
            }
        }
    };
}

/// Macro to implement Clone trait for containers
///
/// This macro provides a standard Clone implementation that creates
/// a new container instance with the same configuration.
///
/// # Usage
///
/// ```rust
/// impl_container_clone!(PostgresContainer, |self| {
///     Self::new(
///         self.database_name.clone(),
///         self.username.clone(),
///         self.password.clone(),
///     ).expect("Failed to clone PostgresContainer")
/// });
/// ```
#[macro_export]
macro_rules! impl_container_clone {
    ($container_type:ident, $clone_fn:expr) => {
        impl Clone for $container_type {
            fn clone(&self) -> Self {
                // Note: This creates a new container instance with the same configuration
                // The actual testcontainers Container cannot be cloned, so we create a new one
                $clone_fn(self)
            }
        }
    };
}

/// Macro to create container base fields
///
/// This macro defines the common fields used across all container types.
///
/// # Usage
///
/// ```rust
/// container_base_fields! {
///     pub container: Container<Postgres>,
///     pub connection_string: String,
///     pub database_name: String,
///     pub username: String,
///     pub password: String,
/// }
/// ```
#[macro_export]
macro_rules! container_base_fields {
    ($($field:tt)*) => {
        $($field)*
        pub status: Arc<RwLock<ContainerStatus>>,
        pub metrics: Arc<RwLock<ContainerMetrics>>,
        pub policy: Policy,
        pub start_time: Instant,
    };
}

/// Macro to initialize container base fields
///
/// This macro provides the common initialization pattern for container fields.
///
/// # Usage
///
/// ```rust
/// container_base_init! {
///     status: Arc::new(RwLock::new(ContainerStatus::Starting)),
///     metrics: Arc::new(RwLock::new(ContainerMetrics::default())),
///     policy: Policy::default(),
///     start_time: Instant::now(),
/// }
/// ```
#[macro_export]
macro_rules! container_base_init {
    ($($field:ident: $value:expr),* $(,)?) => {
        $($field: $value,)*
        status: Arc::new(RwLock::new(ContainerStatus::Starting)),
        metrics: Arc::new(RwLock::new(ContainerMetrics::default())),
        policy: Policy::default(),
        start_time: Instant::now(),
    };
}
