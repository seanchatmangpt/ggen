//! TimedLock - RwLock with timeout guards to prevent deadlocks
//!
//! Provides a wrapper around tokio::sync::RwLock that enforces timeouts
//! on all acquire operations. When timeout is exceeded, gracefully returns
//! an error instead of deadlocking the system.

use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tokio::time::timeout;
use tracing::{error, warn};

/// Error type for timed lock operations
#[derive(Debug, Clone)]
pub enum LockError {
    /// Lock acquisition timed out - potential deadlock detected
    AcquireTimeout { component: String, timeout_ms: u64 },
    /// Lock is poisoned (should not happen with RwLock)
    Poisoned { component: String },
}

impl std::fmt::Display for LockError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LockError::AcquireTimeout {
                component,
                timeout_ms,
            } => {
                write!(
                    f,
                    "Lock acquisition timeout for {}: {}ms",
                    component, timeout_ms
                )
            }
            LockError::Poisoned { component } => {
                write!(f, "Lock poisoned for {}", component)
            }
        }
    }
}

impl std::error::Error for LockError {}

/// Configuration for timeout guard
#[derive(Debug, Clone)]
pub struct TimeoutConfig {
    /// Component name for logging
    pub component: String,
    /// Read timeout duration
    pub read_timeout: Duration,
    /// Write timeout duration
    pub write_timeout: Duration,
}

impl TimeoutConfig {
    /// Create a new timeout configuration
    pub fn new(
        component: impl Into<String>, read_timeout: Duration, write_timeout: Duration,
    ) -> Self {
        Self {
            component: component.into(),
            read_timeout,
            write_timeout,
        }
    }

    /// Sensor manager buffers (high-frequency reads)
    pub fn sensor_buffer() -> Self {
        Self {
            component: "sensor_buffer".to_string(),
            read_timeout: Duration::from_millis(500),
            write_timeout: Duration::from_millis(500),
        }
    }

    /// Policy stores (medium traffic)
    pub fn policy_store() -> Self {
        Self {
            component: "policy_store".to_string(),
            read_timeout: Duration::from_millis(1000),
            write_timeout: Duration::from_millis(1000),
        }
    }

    /// State managers (lower frequency, longer operations)
    pub fn state_manager() -> Self {
        Self {
            component: "state_manager".to_string(),
            read_timeout: Duration::from_millis(2000),
            write_timeout: Duration::from_millis(2000),
        }
    }

    /// General purpose (default)
    pub fn general_purpose() -> Self {
        Self {
            component: "general".to_string(),
            read_timeout: Duration::from_secs(5),
            write_timeout: Duration::from_secs(5),
        }
    }
}

/// TimedLock - RwLock wrapper with timeout enforcement
pub struct TimedLock<T> {
    inner: Arc<RwLock<T>>,
    config: TimeoutConfig,
    timeout_count: Arc<RwLock<usize>>,
}

impl<T> Clone for TimedLock<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
            config: self.config.clone(),
            timeout_count: Arc::clone(&self.timeout_count),
        }
    }
}

impl<T: Send + Sync> TimedLock<T> {
    /// Create a new TimedLock with custom configuration
    pub fn new(value: T, config: TimeoutConfig) -> Self {
        Self {
            inner: Arc::new(RwLock::new(value)),
            config,
            timeout_count: Arc::new(RwLock::new(0)),
        }
    }

    /// Create TimedLock with sensor buffer timeouts
    pub fn sensor_buffer(value: T) -> Self {
        Self::new(value, TimeoutConfig::sensor_buffer())
    }

    /// Create TimedLock with policy store timeouts
    pub fn policy_store(value: T) -> Self {
        Self::new(value, TimeoutConfig::policy_store())
    }

    /// Create TimedLock with state manager timeouts
    pub fn state_manager(value: T) -> Self {
        Self::new(value, TimeoutConfig::state_manager())
    }

    /// Create TimedLock with general purpose timeouts
    pub fn general_purpose(value: T) -> Self {
        Self::new(value, TimeoutConfig::general_purpose())
    }

    /// Acquire read lock with timeout
    pub async fn read(&self) -> Result<tokio::sync::RwLockReadGuard<'_, T>, LockError> {
        match timeout(self.config.read_timeout, self.inner.read()).await {
            Ok(guard) => Ok(guard),
            Err(_) => {
                let timeout_ms = self.config.read_timeout.as_millis() as u64;
                let _ = self.increment_timeout_count().await;

                warn!(
                    component = %self.config.component,
                    timeout_ms = timeout_ms,
                    "RwLock read timeout - potential deadlock detected"
                );

                Err(LockError::AcquireTimeout {
                    component: self.config.component.clone(),
                    timeout_ms,
                })
            }
        }
    }

    /// Acquire write lock with timeout
    pub async fn write(&self) -> Result<tokio::sync::RwLockWriteGuard<'_, T>, LockError> {
        match timeout(self.config.write_timeout, self.inner.write()).await {
            Ok(guard) => Ok(guard),
            Err(_) => {
                let timeout_ms = self.config.write_timeout.as_millis() as u64;
                let _ = self.increment_timeout_count().await;

                error!(
                    component = %self.config.component,
                    timeout_ms = timeout_ms,
                    "RwLock write timeout - potential deadlock detected"
                );

                Err(LockError::AcquireTimeout {
                    component: self.config.component.clone(),
                    timeout_ms,
                })
            }
        }
    }

    /// Try to acquire read lock (non-async)
    pub fn try_read(&self) -> Result<tokio::sync::RwLockReadGuard<'_, T>, LockError> {
        self.inner.try_read().map_err(|_| LockError::Poisoned {
            component: self.config.component.clone(),
        })
    }

    /// Try to acquire write lock (non-async)
    pub fn try_write(&self) -> Result<tokio::sync::RwLockWriteGuard<'_, T>, LockError> {
        self.inner.try_write().map_err(|_| LockError::Poisoned {
            component: self.config.component.clone(),
        })
    }

    /// Get the number of timeouts that have occurred
    pub async fn timeout_count(&self) -> usize {
        *self.timeout_count.read().await
    }

    /// Reset the timeout counter
    pub async fn reset_timeout_count(&self) {
        let mut count = self.timeout_count.write().await;
        *count = 0;
    }

    /// Increment timeout counter
    async fn increment_timeout_count(&self) -> Result<(), LockError> {
        match timeout(Duration::from_secs(1), self.timeout_count.write()).await {
            Ok(mut count) => {
                *count += 1;
                if *count > 10 {
                    error!(
                        component = %self.config.component,
                        timeout_count = *count,
                        "Sustained timeouts detected - potential system deadlock"
                    );
                }
                Ok(())
            }
            Err(_) => {
                error!(component = %self.config.component, "Failed to update timeout counter");
                Ok(()) // Don't fail the original operation
            }
        }
    }

    /// Get reference to the inner RwLock (use with caution - bypasses timeouts)
    pub fn inner(&self) -> &RwLock<T> {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_read_succeeds() {
        let timed_lock = TimedLock::new(42, TimeoutConfig::general_purpose());
        let value = timed_lock.read().await.unwrap();
        assert_eq!(*value, 42);
    }

    #[tokio::test]
    async fn test_write_succeeds() {
        let timed_lock = TimedLock::new(42, TimeoutConfig::general_purpose());
        {
            let mut value = timed_lock.write().await.unwrap();
            *value = 100;
        }
        let value = timed_lock.read().await.unwrap();
        assert_eq!(*value, 100);
    }

    #[tokio::test]
    async fn test_timeout_count() {
        let timed_lock = TimedLock::new(
            42,
            TimeoutConfig::new("test", Duration::from_millis(1), Duration::from_millis(1)),
        );

        // Create lock contention to trigger timeouts
        let _guard = timed_lock.write().await.unwrap();
        let result = timed_lock.read().await;

        // Should timeout since write lock is held
        assert!(result.is_err());
        assert!(timed_lock.timeout_count().await > 0);
    }
}
