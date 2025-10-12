//! Async executor wrapper for CleanroomEnvironment
//!
//! This module provides a high-level async executor that wraps the existing
//! CleanroomEnvironment API with better async ergonomics, timeout support,
//! and cancellation handling.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::executor::AsyncExecutor;
//! use std::time::Duration;
//!
//! let executor = AsyncExecutor::new(environment);
//! let result = executor
//!     .with_timeout(Duration::from_secs(30))
//!     .run_async(async {
//!         // Native async/await support
//!         some_async_operation().await
//!     })
//!     .await?;
//! ```

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::time::Duration;
use std::future::Future;
use tokio::time::{timeout, TimeoutError};
use tokio::sync::oneshot;
use std::sync::Arc;

/// Async executor wrapper for CleanroomEnvironment
///
/// Provides enhanced async capabilities including timeout support,
/// cancellation tokens, and better error handling.
pub struct AsyncExecutor {
    environment: Arc<CleanroomEnvironment>,
    default_timeout: Option<Duration>,
}

/// Execution context with timeout and cancellation support
pub struct ExecutionContext {
    environment: Arc<CleanroomEnvironment>,
    timeout: Option<Duration>,
    cancellation_token: Option<oneshot::Receiver<()>>,
}

/// Result of async execution
#[derive(Debug)]
pub struct AsyncResult<T> {
    /// The actual result
    pub result: T,
    /// Execution duration
    pub duration: Duration,
    /// Whether execution was cancelled
    pub cancelled: bool,
    /// Whether execution timed out
    pub timed_out: bool,
}

impl AsyncExecutor {
    /// Create a new async executor
    pub fn new(environment: CleanroomEnvironment) -> Self {
        Self {
            environment: Arc::new(environment),
            default_timeout: None,
        }
    }

    /// Create an executor with a default timeout
    pub fn with_default_timeout(environment: CleanroomEnvironment, timeout: Duration) -> Self {
        Self {
            environment: Arc::new(environment),
            default_timeout: Some(timeout),
        }
    }

    /// Create an execution context
    pub fn context(&self) -> ExecutionContext {
        ExecutionContext {
            environment: self.environment.clone(),
            timeout: self.default_timeout,
            cancellation_token: None,
        }
    }

    /// Execute an async function with timeout
    pub async fn run_async<F, T>(&self, future: F) -> Result<AsyncResult<T>>
    where
        F: Future<Output = Result<T>>,
    {
        self.context().run_async(future).await
    }

    /// Execute an async function with custom timeout
    pub async fn run_async_with_timeout<F, T>(&self, future: F, timeout_duration: Duration) -> Result<AsyncResult<T>>
    where
        F: Future<Output = Result<T>>,
    {
        self.context()
            .with_timeout(timeout_duration)
            .run_async(future)
            .await
    }

    /// Execute an async function with cancellation support
    pub async fn run_async_with_cancellation<F, T>(&self, future: F) -> Result<AsyncResult<T>>
    where
        F: Future<Output = Result<T>>,
    {
        let (tx, rx) = oneshot::channel();
        self.context()
            .with_cancellation(rx)
            .run_async(future)
            .await
    }

    /// Get the underlying environment
    pub fn environment(&self) -> &CleanroomEnvironment {
        &self.environment
    }
}

impl ExecutionContext {
    /// Set timeout for this execution
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    /// Set cancellation token for this execution
    pub fn with_cancellation(mut self, cancellation_token: oneshot::Receiver<()>) -> Self {
        self.cancellation_token = Some(cancellation_token);
        self
    }

    /// Execute an async function
    pub async fn run_async<F, T>(self, future: F) -> Result<AsyncResult<T>>
    where
        F: Future<Output = Result<T>>,
    {
        let start_time = std::time::Instant::now();
        let mut cancelled = false;
        let mut timed_out = false;

        let result = if let Some(timeout_duration) = self.timeout {
            // Execute with timeout
            match timeout(timeout_duration, self.execute_with_cancellation(future)).await {
                Ok(result) => result,
                Err(_) => {
                    timed_out = true;
                    return Err(crate::error::CleanroomError::timeout_error("Execution timed out"));
                }
            }
        } else {
            // Execute without timeout
            self.execute_with_cancellation(future).await
        };

        let duration = start_time.elapsed();

        match result {
            Ok(value) => Ok(AsyncResult {
                result: value,
                duration,
                cancelled,
                timed_out,
            }),
            Err(e) => Err(e),
        }
    }

    /// Execute with cancellation support
    async fn execute_with_cancellation<F, T>(self, future: F) -> Result<T>
    where
        F: Future<Output = Result<T>>,
    {
        if let Some(mut cancellation_token) = self.cancellation_token {
            tokio::select! {
                result = future => result,
                _ = &mut cancellation_token => {
                    Err(crate::error::CleanroomError::internal_error("Execution was cancelled"))
                }
            }
        } else {
            future.await
        }
    }

    /// Execute a test function using the environment
    pub async fn execute_test<F, T>(self, test_name: &str, test_fn: F) -> Result<AsyncResult<T>>
    where
        F: Future<Output = Result<T>>,
    {
        let wrapped_future = async {
            self.environment.execute_test(test_name, || async { test_fn.await }).await
        };

        self.run_async(wrapped_future).await
    }

    /// Execute multiple async functions concurrently
    pub async fn run_concurrent<F, T>(self, futures: Vec<F>) -> Result<Vec<AsyncResult<T>>>
    where
        F: Future<Output = Result<T>> + Send + 'static,
        T: Send + 'static,
    {
        let tasks: Vec<_> = futures
            .into_iter()
            .map(|future| {
                let ctx = ExecutionContext {
                    environment: self.environment.clone(),
                    timeout: self.timeout,
                    cancellation_token: self.cancellation_token.clone(),
                };
                tokio::spawn(async move { ctx.run_async(future).await })
            })
            .collect();

        let mut results = Vec::new();
        for task in tasks {
            match task.await {
                Ok(result) => results.push(result),
                Err(e) => return Err(crate::error::CleanroomError::internal_error(&format!("Task failed: {}", e))),
            }
        }

        Ok(results)
    }
}

impl<T> AsyncResult<T> {
    /// Check if execution was successful
    pub fn is_success(&self) -> bool {
        !self.cancelled && !self.timed_out
    }

    /// Get the result value
    pub fn into_result(self) -> T {
        self.result
    }

    /// Get execution duration
    pub fn duration(&self) -> Duration {
        self.duration
    }

    /// Check if execution was cancelled
    pub fn was_cancelled(&self) -> bool {
        self.cancelled
    }

    /// Check if execution timed out
    pub fn timed_out(&self) -> bool {
        self.timed_out
    }
}

impl From<CleanroomEnvironment> for AsyncExecutor {
    fn from(environment: CleanroomEnvironment) -> Self {
        Self::new(environment)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_async_executor_creation() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        assert!(executor.environment().session_id() != uuid::Uuid::nil());
    }

    #[tokio::test]
    async fn test_async_execution() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let result = executor.run_async(async {
            Ok::<i32, crate::error::CleanroomError>(42)
        }).await.unwrap();
        
        assert_eq!(result.result, 42);
        assert!(result.is_success());
        assert!(!result.was_cancelled());
        assert!(!result.timed_out());
    }

    #[tokio::test]
    async fn test_async_execution_with_timeout() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let result = executor.run_async_with_timeout(
            async {
                tokio::time::sleep(Duration::from_millis(100)).await;
                Ok::<i32, crate::error::CleanroomError>(42)
            },
            Duration::from_millis(200)
        ).await.unwrap();
        
        assert_eq!(result.result, 42);
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_async_execution_timeout() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let result = executor.run_async_with_timeout(
            async {
                tokio::time::sleep(Duration::from_millis(200)).await;
                Ok::<i32, crate::error::CleanroomError>(42)
            },
            Duration::from_millis(100)
        ).await;
        
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(matches!(e.kind, crate::error::ErrorKind::Timeout));
        }
    }

    #[tokio::test]
    async fn test_execution_context() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let result = executor.context()
            .with_timeout(Duration::from_millis(100))
            .run_async(async {
                Ok::<i32, crate::error::CleanroomError>(42)
            })
            .await
            .unwrap();
        
        assert_eq!(result.result, 42);
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_concurrent_execution() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let futures = vec![
            async { Ok::<i32, crate::error::CleanroomError>(1) },
            async { Ok::<i32, crate::error::CleanroomError>(2) },
            async { Ok::<i32, crate::error::CleanroomError>(3) },
        ];
        
        let results = executor.context()
            .run_concurrent(futures)
            .await
            .unwrap();
        
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].result, 1);
        assert_eq!(results[1].result, 2);
        assert_eq!(results[2].result, 3);
    }

    #[tokio::test]
    async fn test_execute_test_wrapper() {
        let config = crate::config::CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let executor = AsyncExecutor::new(environment);
        
        let result = executor.context()
            .execute_test("test", async {
                Ok::<i32, crate::error::CleanroomError>(42)
            })
            .await
            .unwrap();
        
        assert_eq!(result.result, 42);
        assert!(result.is_success());
    }
}
