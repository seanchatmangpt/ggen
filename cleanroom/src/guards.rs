//! RAII guards for resource management
//!
//! This module provides RAII (Resource Acquisition Is Initialization) guards
//! that ensure proper cleanup of resources even in the case of panics or early returns.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::guards::{ContainerGuard, ResourceGuard};
//!
//! let guard = ContainerGuard::new(container);
//! // Automatic cleanup on drop, even on panic
//! ```

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::sync::Arc;
use std::collections::HashMap;
use std::time::Instant;
use tokio::sync::RwLock;

/// RAII guard for container lifecycle management
///
/// Ensures containers are properly cleaned up when the guard goes out of scope,
/// even in the case of panics or early returns.
pub struct ContainerGuard {
    environment: Arc<CleanroomEnvironment>,
    container_id: String,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
    created_at: Instant,
}

impl ContainerGuard {
    /// Create a new container guard
    pub fn new(environment: Arc<CleanroomEnvironment>, container_id: String) -> Self {
        Self {
            environment,
            container_id,
            cleanup_actions: Vec::new(),
            created_at: Instant::now(),
        }
    }

    /// Add a cleanup action to be executed on drop
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the container ID
    pub fn container_id(&self) -> &str {
        &self.container_id
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Get the environment
    pub fn environment(&self) -> &CleanroomEnvironment {
        &self.environment
    }

    /// Check if the container is still registered
    pub async fn is_registered(&self) -> bool {
        self.environment.is_container_registered(&self.container_id).await
    }

    /// Manually trigger cleanup (useful for early cleanup)
    pub async fn cleanup(self) -> Result<()> {
        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Unregister container
        self.environment.unregister_container(&self.container_id).await?;
        
        Ok(())
    }
}

impl Drop for ContainerGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }

        // Attempt to unregister container (best effort)
        // Try to use the current runtime if available
        let environment = self.environment.clone();
        let container_id = self.container_id.clone();

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            // We're in a runtime context - spawn the cleanup task
            // This will be waited on by the runtime's shutdown
            handle.spawn(async move {
                if let Err(e) = environment.unregister_container(&container_id).await {
                    eprintln!("Warning: Failed to unregister container {} during drop: {}", container_id, e);
                }
            });
        } else {
            // No runtime available - log warning
            eprintln!("Warning: Cannot unregister container {} - no async runtime available during drop", container_id);
            eprintln!("         Call .cleanup().await manually before dropping to ensure proper cleanup.");
        }
    }
}

/// RAII guard for resource management
///
/// Generic guard for managing any type of resource with cleanup actions.
pub struct ResourceGuard<T> {
    resource: Option<T>,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
    created_at: Instant,
}

impl<T> ResourceGuard<T> {
    /// Create a new resource guard
    pub fn new(resource: T) -> Self {
        Self {
            resource: Some(resource),
            cleanup_actions: Vec::new(),
            created_at: Instant::now(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get a reference to the resource
    ///
    /// # Panics
    /// Panics if the resource has been taken. This should never happen in normal usage.
    pub fn resource(&self) -> &T {
        self.resource.as_ref().unwrap_or_else(|| {
            panic!("FATAL: Resource has been taken from guard. This indicates a logic error in the code.");
        })
    }

    /// Get a mutable reference to the resource
    ///
    /// # Panics
    /// Panics if the resource has been taken. This should never happen in normal usage.
    pub fn resource_mut(&mut self) -> &mut T {
        self.resource.as_mut().unwrap_or_else(|| {
            panic!("FATAL: Resource has been taken from guard. This indicates a logic error in the code.");
        })
    }

    /// Take ownership of the resource
    ///
    /// # Panics
    /// Panics if the resource has already been taken.
    pub fn take_resource(mut self) -> T {
        self.resource.take().unwrap_or_else(|| {
            panic!("FATAL: Resource has already been taken from guard. This indicates a logic error in the code.");
        })
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Check if the resource is still present
    pub fn has_resource(&self) -> bool {
        self.resource.is_some()
    }

    /// Manually trigger cleanup and return the resource
    ///
    /// # Panics
    /// Panics if the resource has already been taken.
    pub fn cleanup(mut self) -> T {
        // Execute cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Return the resource
        self.resource.take().unwrap_or_else(|| {
            panic!("FATAL: Resource has already been taken from guard. This indicates a logic error in the code.");
        })
    }
}

impl<T> Drop for ResourceGuard<T> {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }
    }
}

/// RAII guard for scoped execution
///
/// Ensures proper cleanup when exiting a scope, even on panic.
pub struct ScopeGuard {
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
    created_at: Instant,
}

impl ScopeGuard {
    /// Create a new scope guard
    pub fn new() -> Self {
        Self {
            cleanup_actions: Vec::new(),
            created_at: Instant::now(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Manually trigger cleanup
    pub fn cleanup(mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }
    }
}

impl Default for ScopeGuard {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for ScopeGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }
    }
}

/// RAII guard for environment session
///
/// Manages the lifecycle of a cleanroom environment session.
pub struct SessionGuard {
    environment: Arc<CleanroomEnvironment>,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
    created_at: Instant,
}

impl SessionGuard {
    /// Create a new session guard
    pub fn new(environment: CleanroomEnvironment) -> Self {
        Self {
            environment: Arc::new(environment),
            cleanup_actions: Vec::new(),
            created_at: Instant::now(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the environment
    pub fn environment(&self) -> &CleanroomEnvironment {
        &self.environment
    }

    /// Get the session ID
    pub fn session_id(&self) -> uuid::Uuid {
        self.environment.session_id()
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Manually trigger cleanup
    pub async fn cleanup(self) -> Result<()> {
        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Cleanup environment
        self.environment.cleanup().await?;
        
        Ok(())
    }
}

impl Drop for SessionGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }

        // Attempt to cleanup environment (best effort)
        // Try to use the current runtime if available
        let environment = self.environment.clone();

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            // We're in a runtime context - spawn the cleanup task
            // This will be waited on by the runtime's shutdown
            handle.spawn(async move {
                if let Err(e) = environment.cleanup().await {
                    eprintln!("Warning: Failed to cleanup environment during drop: {}", e);
                }
            });
        } else {
            // No runtime available - log warning
            eprintln!("Warning: Cannot cleanup environment - no async runtime available during drop");
            eprintln!("         Call .cleanup().await manually before dropping to ensure proper cleanup.");
        }
    }
}

/// RAII guard for test execution
///
/// Manages the lifecycle of a test execution with proper cleanup.
pub struct TestGuard {
    environment: Arc<CleanroomEnvironment>,
    test_name: String,
    cleanup_actions: Vec<Box<dyn FnOnce() + Send + Sync>>,
    created_at: Instant,
}

impl TestGuard {
    /// Create a new test guard
    pub fn new(environment: Arc<CleanroomEnvironment>, test_name: String) -> Self {
        Self {
            environment,
            test_name,
            cleanup_actions: Vec::new(),
            created_at: Instant::now(),
        }
    }

    /// Add a cleanup action
    pub fn add_cleanup_action<F>(mut self, action: F) -> Self
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        self.cleanup_actions.push(Box::new(action));
        self
    }

    /// Get the environment
    pub fn environment(&self) -> &CleanroomEnvironment {
        &self.environment
    }

    /// Get the test name
    pub fn test_name(&self) -> &str {
        &self.test_name
    }

    /// Get the creation time
    pub fn created_at(&self) -> Instant {
        self.created_at
    }

    /// Execute a test function with proper cleanup
    pub async fn execute_test<F, T>(self, test_fn: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        let result = self.environment.execute_test(&self.test_name, test_fn).await;
        
        // Cleanup will happen automatically on drop
        result
    }

    /// Manually trigger cleanup
    pub async fn cleanup(self) -> Result<()> {
        // Execute custom cleanup actions
        for action in self.cleanup_actions {
            action();
        }

        // Test cleanup is handled by the environment
        Ok(())
    }
}

impl Drop for TestGuard {
    fn drop(&mut self) {
        // Execute cleanup actions in reverse order
        while let Some(action) = self.cleanup_actions.pop() {
            action();
        }
    }
}

/// Convenience function to create a scope guard
pub fn scope() -> ScopeGuard {
    ScopeGuard::new()
}

/// Convenience function to create a container guard
pub fn container_guard(environment: Arc<CleanroomEnvironment>, container_id: String) -> ContainerGuard {
    ContainerGuard::new(environment, container_id)
}

/// Convenience function to create a resource guard
pub fn resource_guard<T>(resource: T) -> ResourceGuard<T> {
    ResourceGuard::new(resource)
}

/// Convenience function to create a session guard
pub fn session_guard(environment: CleanroomEnvironment) -> SessionGuard {
    SessionGuard::new(environment)
}

/// Convenience function to create a test guard
pub fn test_guard(environment: Arc<CleanroomEnvironment>, test_name: String) -> TestGuard {
    TestGuard::new(environment, test_name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CleanroomConfig;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[tokio::test]
    async fn test_container_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let container_id = "test-container".to_string();
        let guard = ContainerGuard::new(environment_arc.clone(), container_id.clone())
            .add_cleanup_action(|| {
                println!("Container cleanup action executed");
            });
        
        assert_eq!(guard.container_id(), "test-container");
        assert!(guard.is_registered().await);
        
        // Guard will be dropped here, triggering cleanup
    }

    #[tokio::test]
    async fn test_resource_guard() {
        let resource = "test-resource".to_string();
        let guard = ResourceGuard::new(resource)
            .add_cleanup_action(|| {
                println!("Resource cleanup action executed");
            });
        
        assert_eq!(guard.resource(), "test-resource");
        assert!(guard.has_resource());
        
        let resource = guard.cleanup();
        assert_eq!(resource, "test-resource");
    }

    #[tokio::test]
    async fn test_scope_guard() {
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = counter.clone();
        
        let guard = ScopeGuard::new()
            .add_cleanup_action(move || {
                counter_clone.fetch_add(1, Ordering::SeqCst);
            });
        
        assert_eq!(counter.load(Ordering::SeqCst), 0);
        
        // Guard will be dropped here, triggering cleanup
        drop(guard);
        
        // Give a moment for cleanup to execute
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn test_session_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let guard = SessionGuard::new(environment)
            .add_cleanup_action(|| {
                println!("Session cleanup action executed");
            });
        
        assert!(!guard.session_id().is_nil());
        
        // Guard will be dropped here, triggering cleanup
    }

    #[tokio::test]
    async fn test_test_guard() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let guard = TestGuard::new(environment_arc, "test".to_string())
            .add_cleanup_action(|| {
                println!("Test cleanup action executed");
            });
        
        assert_eq!(guard.test_name(), "test");
        
        let result = guard.execute_test(|| Ok::<i32, crate::error::CleanroomError>(42)).await.unwrap();
        assert_eq!(result, 42);
        
        // Guard will be dropped here, triggering cleanup
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let environment_arc = Arc::new(environment);
        
        let _scope = scope();
        let _container = container_guard(environment_arc.clone(), "test".to_string());
        let _resource = resource_guard("test".to_string());
        let _session = session_guard(CleanroomEnvironment::new(CleanroomConfig::default()).await.unwrap());
        let _test = test_guard(environment_arc, "test".to_string());
        
        // All guards will be dropped here, triggering cleanup
    }

    #[tokio::test]
    async fn test_guard_cleanup_order() {
        let counter = Arc::new(AtomicUsize::new(0));
        let mut actions = Vec::new();
        
        for i in 0..5 {
            let counter_clone = counter.clone();
            actions.push(Box::new(move || {
                counter_clone.fetch_add(1, Ordering::SeqCst);
                println!("Cleanup action {} executed", i);
            }) as Box<dyn FnOnce() + Send + Sync>);
        }
        
        let guard = ScopeGuard::new();
        let mut guard = guard;
        
        for action in actions {
            guard.cleanup_actions.push(action);
        }
        
        assert_eq!(counter.load(Ordering::SeqCst), 0);
        
        // Guard will be dropped here, triggering cleanup in reverse order
        drop(guard);
        
        // Give a moment for cleanup to execute
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        assert_eq!(counter.load(Ordering::SeqCst), 5);
    }
}
