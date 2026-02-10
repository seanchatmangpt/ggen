//! Work-in-progress tokens for limiting concurrent work
//!
//! Tokens represent capacity units. Dropping a token releases capacity back
//! to the pool, implementing strict WIP limits.

use std::sync::Arc;
use tokio::sync::Semaphore;

/// Token representing one unit of work-in-progress
///
/// When dropped, the token returns its permit to the semaphore,
/// allowing other work to proceed. This enforces strict WIP limits.
#[derive(Debug)]
pub struct WIPToken {
    _permit: tokio::sync::OwnedSemaphorePermit,
    metadata: TokenMetadata,
}

#[derive(Debug, Clone)]
pub struct TokenMetadata {
    pub id: u64,
    pub acquired_at: std::time::Instant,
}

impl WIPToken {
    /// Create a new WIP token with a permit
    pub(crate) fn new(permit: tokio::sync::OwnedSemaphorePermit, id: u64) -> Self {
        Self {
            _permit: permit,
            metadata: TokenMetadata {
                id,
                acquired_at: std::time::Instant::now(),
            },
        }
    }

    /// Get token metadata
    pub fn metadata(&self) -> &TokenMetadata {
        &self.metadata
    }

    /// Get time since token was acquired
    pub fn elapsed(&self) -> std::time::Duration {
        self.metadata.acquired_at.elapsed()
    }
}

/// Pool of WIP tokens with finite capacity
#[derive(Debug, Clone)]
pub struct TokenPool {
    semaphore: Arc<Semaphore>,
    capacity: usize,
    next_id: Arc<std::sync::atomic::AtomicU64>,
}

impl TokenPool {
    /// Create a new token pool with specified capacity
    ///
    /// Capacity is strictly enforced - no more than `capacity` tokens
    /// can be acquired simultaneously.
    pub fn new(capacity: usize) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(capacity)),
            capacity,
            next_id: Arc::new(std::sync::atomic::AtomicU64::new(0)),
        }
    }

    /// Acquire a token, waiting if necessary
    pub async fn acquire(&self) -> crate::Result<WIPToken> {
        let permit = self
            .semaphore
            .clone()
            .acquire_owned()
            .await
            .map_err(|_| crate::BackpressureError::ChannelClosed("semaphore closed".to_string()))?;

        let id = self.next_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Ok(WIPToken::new(permit, id))
    }

    /// Try to acquire a token without waiting
    pub fn try_acquire(&self) -> crate::Result<Option<WIPToken>> {
        match self.semaphore.clone().try_acquire_owned() {
            Ok(permit) => {
                let id = self.next_id.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Ok(Some(WIPToken::new(permit, id)))
            }
            Err(tokio::sync::TryAcquireError::NoPermits) => Ok(None),
            Err(tokio::sync::TryAcquireError::Closed) => {
                Err(crate::BackpressureError::ChannelClosed("semaphore closed".to_string()))
            }
        }
    }

    /// Get current number of available tokens
    pub fn available(&self) -> usize {
        self.semaphore.available_permits()
    }

    /// Get total capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Get current in-flight count
    pub fn in_flight(&self) -> usize {
        self.capacity - self.available()
    }

    /// Get current utilization (0.0 to 1.0)
    pub fn utilization(&self) -> f64 {
        self.in_flight() as f64 / self.capacity as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_token_pool_capacity() {
        let pool = TokenPool::new(3);

        assert_eq!(pool.capacity(), 3);
        assert_eq!(pool.available(), 3);
        assert_eq!(pool.in_flight(), 0);
        assert_eq!(pool.utilization(), 0.0);
    }

    #[tokio::test]
    async fn test_token_acquire_release() {
        let pool = TokenPool::new(2);

        let token1 = pool.acquire().await.unwrap();
        assert_eq!(pool.in_flight(), 1);
        assert_eq!(pool.available(), 1);

        let token2 = pool.acquire().await.unwrap();
        assert_eq!(pool.in_flight(), 2);
        assert_eq!(pool.available(), 0);
        assert_eq!(pool.utilization(), 1.0);

        drop(token1);
        assert_eq!(pool.in_flight(), 1);
        assert_eq!(pool.available(), 1);

        drop(token2);
        assert_eq!(pool.in_flight(), 0);
        assert_eq!(pool.available(), 2);
    }

    #[tokio::test]
    async fn test_token_try_acquire() {
        let pool = TokenPool::new(1);

        let token1 = pool.try_acquire().unwrap();
        assert!(token1.is_some());

        let token2 = pool.try_acquire().unwrap();
        assert!(token2.is_none());

        drop(token1);

        let token3 = pool.try_acquire().unwrap();
        assert!(token3.is_some());
    }

    #[tokio::test]
    async fn test_token_metadata() {
        let pool = TokenPool::new(1);
        let token = pool.acquire().await.unwrap();

        let metadata = token.metadata();
        assert_eq!(metadata.id, 0);
        assert!(token.elapsed().as_millis() < 100);
    }

    #[tokio::test]
    async fn test_strict_capacity_enforcement() {
        let pool = TokenPool::new(2);

        let _t1 = pool.acquire().await.unwrap();
        let _t2 = pool.acquire().await.unwrap();

        // Should not be able to acquire a third
        assert!(pool.try_acquire().unwrap().is_none());
    }
}
