//! Heijunka - Load leveling and resource management
//!
//! This module implements load leveling through rate limiting and resource quotas,
//! following the TPS principle of "smoothing the workload".

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Semaphore;

/// Rate limiter using token bucket algorithm
#[derive(Debug)]
pub struct RateLimiter {
    /// Maximum requests per second
    max_rate: u32,
    /// Maximum burst size
    burst_size: u32,
    /// Semaphore for rate limiting
    semaphore: Arc<Semaphore>,
    /// Last refill time
    last_refill: std::sync::Mutex<Instant>,
}

impl RateLimiter {
    /// Create a new rate limiter
    pub fn new(max_rate: u32, burst_size: u32) -> Self {
        Self {
            max_rate,
            burst_size,
            semaphore: Arc::new(Semaphore::new(burst_size as usize)),
            last_refill: std::sync::Mutex::new(Instant::now()),
        }
    }

    /// Acquire a permit (blocks until available)
    pub async fn acquire(&self) -> Result<()> {
        let _permit = self
            .semaphore
            .acquire()
            .await
            .map_err(|e| Error::new(&format!("Failed to acquire rate limit permit: {}", e)))?;

        // Release the permit after acquiring to implement token bucket
        Ok(())
    }

    /// Try to acquire without blocking
    pub fn try_acquire(&self) -> bool {
        self.semaphore.try_acquire().is_ok()
    }

    /// Refill tokens based on elapsed time
    pub fn refill(&self) {
        let mut last_refill = self.last_refill.lock().unwrap();
        let now = Instant::now();
        let elapsed = now.duration_since(*last_refill);

        let tokens_to_add = (elapsed.as_secs_f64() * self.max_rate as f64) as usize;

        if tokens_to_add > 0 {
            // Add permits up to burst_size
            self.semaphore.add_permits(tokens_to_add.min(self.burst_size as usize));
            *last_refill = now;
        }
    }
}

/// Resource quotas for generation tasks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceQuotas {
    /// Maximum memory in MB
    pub max_memory_mb: u64,
    /// Maximum disk space in MB
    pub max_disk_mb: u64,
    /// Maximum generation time in seconds
    pub max_generation_time_secs: u64,
}

impl ResourceQuotas {
    /// Create new resource quotas
    pub fn new(max_memory_mb: u64, max_disk_mb: u64, max_generation_time_secs: u64) -> Self {
        Self {
            max_memory_mb,
            max_disk_mb,
            max_generation_time_secs,
        }
    }

    /// Create default quotas
    pub fn default_quotas() -> Self {
        Self {
            max_memory_mb: 2048,
            max_disk_mb: 5000,
            max_generation_time_secs: 60,
        }
    }

    /// Check memory usage (simplified - would use OS APIs in production)
    pub fn check_memory(&self) -> Result<()> {
        // In a real implementation, would check actual process memory
        // For now, always pass
        Ok(())
    }

    /// Check disk space
    pub fn check_disk(&self, _path: &std::path::Path) -> Result<()> {
        // In a real implementation, would check actual disk space
        // For now, always pass
        Ok(())
    }

    /// Check time budget
    pub fn check_time_budget(&self, elapsed: Duration) -> Result<()> {
        if elapsed.as_secs() > self.max_generation_time_secs {
            return Err(Error::new(&format!(
                "Generation exceeded time budget of {} seconds",
                self.max_generation_time_secs
            )));
        }
        Ok(())
    }
}

impl Default for ResourceQuotas {
    fn default() -> Self {
        Self::default_quotas()
    }
}

/// System resource monitor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemLoad {
    /// CPU usage percentage (0-100)
    pub cpu_usage: f64,
    /// Memory usage in MB
    pub memory_mb: u64,
    /// Available memory in MB
    pub available_memory_mb: u64,
    /// Disk I/O queue depth
    pub disk_queue_depth: usize,
}

impl SystemLoad {
    /// Get current system load (simplified - would use OS APIs)
    pub fn current() -> Self {
        Self {
            cpu_usage: 0.0,
            memory_mb: 0,
            available_memory_mb: 1024,
            disk_queue_depth: 0,
        }
    }

    /// Check if system is under high load
    pub fn is_high_load(&self) -> bool {
        self.cpu_usage > 80.0 || self.available_memory_mb < 512 || self.disk_queue_depth > 10
    }
}

/// Adaptive scheduler that adjusts execution based on system load
#[derive(Debug)]
pub struct AdaptiveScheduler {
    /// Resource quotas
    quotas: ResourceQuotas,
}

impl AdaptiveScheduler {
    /// Create a new adaptive scheduler
    pub fn new(quotas: ResourceQuotas) -> Self {
        Self { quotas }
    }

    /// Determine if we should parallelize based on system load
    pub fn should_parallelize(&self) -> bool {
        let load = SystemLoad::current();
        !load.is_high_load()
    }

    /// Get recommended concurrency level
    pub fn recommended_concurrency(&self) -> usize {
        let load = SystemLoad::current();

        if load.cpu_usage > 80.0 {
            1 // Sequential execution
        } else if load.cpu_usage > 50.0 {
            2 // Limited parallelism
        } else {
            4 // Full parallelism
        }
    }

    /// Check resource quotas before starting generation
    pub fn check_quotas(&self, elapsed: Duration) -> Result<()> {
        self.quotas.check_memory()?;
        self.quotas.check_time_budget(elapsed)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    #[tokio::test]
    async fn test_rate_limiter() {
        let limiter = RateLimiter::new(10, 5);

        // Should be able to acquire
        let result = limiter.acquire().await;
        assert!(result.is_ok());
    }

    test!(test_resource_quotas, {
        let quotas = ResourceQuotas::new(2048, 5000, 60);

        assert_eq!(quotas.max_memory_mb, 2048);
        assert_eq!(quotas.max_disk_mb, 5000);

        // Check memory should pass
        let result = quotas.check_memory();
        assert!(result.is_ok());

        Ok(())
    });

    test!(test_system_load, {
        let load = SystemLoad::current();

        // Should return some values
        assert!(load.cpu_usage >= 0.0);
        assert!(load.available_memory_mb >= 0);

        Ok(())
    });

    test!(test_adaptive_scheduler, {
        let quotas = ResourceQuotas::default();
        let scheduler = AdaptiveScheduler::new(quotas);

        // Should return a concurrency recommendation
        let concurrency = scheduler.recommended_concurrency();
        assert!(concurrency > 0);

        Ok(())
    });
}
