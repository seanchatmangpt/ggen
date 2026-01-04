//! Work-stealing agent pool for optimal load balancing
//!
//! Implements a work-stealing scheduler that dynamically redistributes
//! work among agents for maximum throughput.

use crossbeam::deque::{Injector, Stealer, Worker};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use tracing::trace;

/// Work item for the pool
#[derive(Debug)]
pub struct WorkItem<T> {
    /// Unique work item ID
    pub id: String,
    /// The actual work payload
    pub payload: T,
    /// Priority (lower = higher priority)
    pub priority: u32,
    /// Creation timestamp
    pub created_at: std::time::Instant,
}

impl<T> WorkItem<T> {
    /// Create a new work item
    pub fn new(id: String, payload: T, priority: u32) -> Self {
        Self {
            id,
            payload,
            priority,
            created_at: std::time::Instant::now(),
        }
    }
}

/// Work-stealing agent pool
#[derive(Debug)]
pub struct WorkStealingAgentPool {
    /// Global work injector
    injector: Arc<Injector<Box<dyn FnOnce() + Send>>>,
    /// Per-worker local queues
    workers: Vec<Worker<Box<dyn FnOnce() + Send>>>,
    /// Stealers for work stealing
    stealers: Vec<Stealer<Box<dyn FnOnce() + Send>>>,
    /// Number of workers
    worker_count: usize,
    /// Active workers count
    active_workers: AtomicUsize,
    /// Pool is running
    running: AtomicBool,
    /// Pool statistics
    stats: Arc<PoolStats>,
}

/// Pool statistics
#[derive(Debug, Default)]
pub struct PoolStats {
    /// Total tasks submitted
    pub tasks_submitted: AtomicUsize,
    /// Total tasks completed
    pub tasks_completed: AtomicUsize,
    /// Total tasks stolen
    pub tasks_stolen: AtomicUsize,
    /// Current queue length
    pub queue_length: AtomicUsize,
}

impl WorkStealingAgentPool {
    /// Create a new work-stealing pool
    pub fn new(worker_count: usize) -> Self {
        let injector = Arc::new(Injector::new());
        let mut workers = Vec::with_capacity(worker_count);
        let mut stealers = Vec::with_capacity(worker_count);

        for _ in 0..worker_count {
            let worker = Worker::new_fifo();
            stealers.push(worker.stealer());
            workers.push(worker);
        }

        Self {
            injector,
            workers,
            stealers,
            worker_count,
            active_workers: AtomicUsize::new(0),
            running: AtomicBool::new(true),
            stats: Arc::new(PoolStats::default()),
        }
    }

    /// Submit work to the pool
    pub fn submit<F>(&self, work: F)
    where
        F: FnOnce() + Send + 'static,
    {
        self.injector.push(Box::new(work));
        self.stats.tasks_submitted.fetch_add(1, Ordering::Relaxed);
        self.stats.queue_length.fetch_add(1, Ordering::Relaxed);
        trace!("Work submitted to pool");
    }

    /// Try to steal work from another worker
    pub fn try_steal(&self, worker_idx: usize) -> Option<Box<dyn FnOnce() + Send>> {
        // First try the global injector
        if let crossbeam::deque::Steal::Success(work) = self.injector.steal() {
            self.stats.queue_length.fetch_sub(1, Ordering::Relaxed);
            return Some(work);
        }

        // Try stealing from other workers
        for (idx, stealer) in self.stealers.iter().enumerate() {
            if idx != worker_idx {
                if let crossbeam::deque::Steal::Success(work) = stealer.steal() {
                    self.stats.tasks_stolen.fetch_add(1, Ordering::Relaxed);
                    self.stats.queue_length.fetch_sub(1, Ordering::Relaxed);
                    return Some(work);
                }
            }
        }

        None
    }

    /// Get work for a specific worker
    pub fn get_work(&self, worker_idx: usize) -> Option<Box<dyn FnOnce() + Send>> {
        if worker_idx >= self.workers.len() {
            return None;
        }

        // First check local queue
        if let Some(work) = self.workers[worker_idx].pop() {
            self.stats.queue_length.fetch_sub(1, Ordering::Relaxed);
            return Some(work);
        }

        // Try to steal
        self.try_steal(worker_idx)
    }

    /// Record task completion
    pub fn complete_task(&self) {
        self.stats.tasks_completed.fetch_add(1, Ordering::Relaxed);
    }

    /// Get pool statistics
    pub fn statistics(&self) -> PoolStatistics {
        PoolStatistics {
            worker_count: self.worker_count,
            active_workers: self.active_workers.load(Ordering::Relaxed),
            tasks_submitted: self.stats.tasks_submitted.load(Ordering::Relaxed),
            tasks_completed: self.stats.tasks_completed.load(Ordering::Relaxed),
            tasks_stolen: self.stats.tasks_stolen.load(Ordering::Relaxed),
            queue_length: self.stats.queue_length.load(Ordering::Relaxed),
            steal_ratio: self.steal_ratio(),
        }
    }

    /// Calculate the steal ratio
    fn steal_ratio(&self) -> f64 {
        let completed = self.stats.tasks_completed.load(Ordering::Relaxed);
        let stolen = self.stats.tasks_stolen.load(Ordering::Relaxed);
        if completed > 0 {
            stolen as f64 / completed as f64
        } else {
            0.0
        }
    }

    /// Check if pool is running
    pub fn is_running(&self) -> bool {
        self.running.load(Ordering::Relaxed)
    }

    /// Stop the pool
    pub fn stop(&self) {
        self.running.store(false, Ordering::Relaxed);
    }

    /// Increment active workers
    pub fn activate_worker(&self) {
        self.active_workers.fetch_add(1, Ordering::Relaxed);
    }

    /// Decrement active workers
    pub fn deactivate_worker(&self) {
        self.active_workers.fetch_sub(1, Ordering::Relaxed);
    }
}

/// Pool statistics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PoolStatistics {
    /// Number of workers
    pub worker_count: usize,
    /// Currently active workers
    pub active_workers: usize,
    /// Total tasks submitted
    pub tasks_submitted: usize,
    /// Total tasks completed
    pub tasks_completed: usize,
    /// Total tasks stolen
    pub tasks_stolen: usize,
    /// Current queue length
    pub queue_length: usize,
    /// Ratio of stolen to completed tasks
    pub steal_ratio: f64,
}

/// Priority work queue with work stealing
#[derive(Debug)]
pub struct PriorityWorkQueue<T: Send> {
    /// Priority queues (one per priority level)
    queues: Vec<Arc<Injector<WorkItem<T>>>>,
    /// Number of priority levels
    priority_levels: usize,
    /// Stats
    stats: Arc<PoolStats>,
}

impl<T: Send + 'static> PriorityWorkQueue<T> {
    /// Create a new priority work queue
    pub fn new(priority_levels: usize) -> Self {
        let queues = (0..priority_levels)
            .map(|_| Arc::new(Injector::new()))
            .collect();

        Self {
            queues,
            priority_levels,
            stats: Arc::new(PoolStats::default()),
        }
    }

    /// Push work with priority
    pub fn push(&self, item: WorkItem<T>) {
        let priority = (item.priority as usize).min(self.priority_levels - 1);
        self.queues[priority].push(item);
        self.stats.tasks_submitted.fetch_add(1, Ordering::Relaxed);
        self.stats.queue_length.fetch_add(1, Ordering::Relaxed);
    }

    /// Pop highest priority work
    pub fn pop(&self) -> Option<WorkItem<T>> {
        // Check queues from highest to lowest priority
        for queue in &self.queues {
            if let crossbeam::deque::Steal::Success(item) = queue.steal() {
                self.stats.queue_length.fetch_sub(1, Ordering::Relaxed);
                return Some(item);
            }
        }
        None
    }

    /// Get current queue length
    pub fn len(&self) -> usize {
        self.stats.queue_length.load(Ordering::Relaxed)
    }

    /// Check if queue is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pool_creation() {
        let pool = WorkStealingAgentPool::new(4);
        assert_eq!(pool.worker_count, 4);
        assert!(pool.is_running());
    }

    #[test]
    fn test_work_submission() {
        let pool = WorkStealingAgentPool::new(4);
        pool.submit(|| println!("test work"));
        let stats = pool.statistics();
        assert_eq!(stats.tasks_submitted, 1);
        assert_eq!(stats.queue_length, 1);
    }

    #[test]
    fn test_work_stealing() {
        let pool = WorkStealingAgentPool::new(4);
        pool.submit(|| {});

        let work = pool.try_steal(0);
        assert!(work.is_some());
    }

    #[test]
    fn test_priority_queue() {
        let queue: PriorityWorkQueue<String> = PriorityWorkQueue::new(5);

        // Add items with different priorities
        queue.push(WorkItem::new("low".to_string(), "low".to_string(), 4));
        queue.push(WorkItem::new("high".to_string(), "high".to_string(), 0));
        queue.push(WorkItem::new("med".to_string(), "med".to_string(), 2));

        // High priority should come first
        let item = queue.pop().unwrap();
        assert_eq!(item.id, "high");
    }

    #[test]
    fn test_pool_statistics() {
        let pool = WorkStealingAgentPool::new(4);
        pool.submit(|| {});
        pool.submit(|| {});
        pool.complete_task();

        let stats = pool.statistics();
        assert_eq!(stats.tasks_submitted, 2);
        assert_eq!(stats.tasks_completed, 1);
    }
}
