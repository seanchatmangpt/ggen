# Backpressure and Admission Control Mechanisms

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference

## Overview

**Backpressure** and **admission control** are fundamental mechanisms for preventing system overload by ensuring that the rate of admitted work never exceeds system capacity. These patterns implement the core queueing theory constraint:

```
λ_admitted ≤ μ_capacity

Where:
- λ_admitted = arrival rate of admitted requests (requests/sec)
- μ_capacity = service rate capacity (requests/sec)
```

Violating this constraint leads to **queue explosion**, **cascade failures**, and **system collapse**. Manufacturing-grade systems enforce this inequality through multiple defensive layers.

### Key Principles

1. **Pull-Based Work Acquisition** - Workers pull work when ready (Kanban)
2. **WIP Limits** - Cap work-in-progress to prevent overload
3. **Admission Control** - Reject work before accepting it into the system
4. **Flow Control** - Propagate capacity signals upstream
5. **Observable Backpressure** - Make queue depth visible (Andon)

---

## Queueing Theory Foundation

### Little's Law

**Little's Law** is the fundamental relationship governing all queueing systems:

```
L = λ × W

Where:
- L = average number of items in system (queue + service)
- λ = average arrival rate (items/sec)
- W = average time an item spends in system (sec)
```

#### Derivation and Implications

```
L = λ × W

Rearranging:
W = L / λ          (Average latency increases with queue depth)
λ = L / W          (Throughput limited by queue capacity)

For stable systems (λ < μ):
W = 1 / (μ - λ)    (M/M/1 queue response time)

As λ → μ:
W → ∞              (Latency explodes at capacity)
```

#### Practical Application

```rust
// Little's Law in code
struct QueueMetrics {
    queue_depth: u64,           // L (items in system)
    arrival_rate: f64,          // λ (items/sec)
    avg_latency: Duration,      // W (seconds)
}

impl QueueMetrics {
    /// Verify Little's Law holds
    fn verify_littles_law(&self) -> bool {
        let computed_depth = self.arrival_rate * self.avg_latency.as_secs_f64();
        let error = (computed_depth - self.queue_depth as f64).abs();
        error < 0.01 * self.queue_depth as f64  // Within 1%
    }

    /// Predict latency from queue depth
    fn predict_latency(&self) -> Duration {
        if self.arrival_rate > 0.0 {
            Duration::from_secs_f64(self.queue_depth as f64 / self.arrival_rate)
        } else {
            Duration::ZERO
        }
    }

    /// Calculate maximum safe arrival rate
    fn max_safe_arrival_rate(&self, target_latency: Duration) -> f64 {
        self.queue_depth as f64 / target_latency.as_secs_f64()
    }
}
```

### M/M/1 Queue Model

The **M/M/1 queue** (Markovian arrivals, Markovian service, 1 server) is the canonical model:

```
System utilization:  ρ = λ / μ

Average queue length: L_q = ρ² / (1 - ρ)
Average queue time:   W_q = ρ / (μ - λ)
Average system time:  W = 1 / (μ - λ)

Stability condition: ρ < 1  ⟹  λ < μ
```

#### Performance Cliff

```
ρ (Utilization) | W (Avg Latency) | L_q (Queue Depth)
───────────────────────────────────────────────────
     0.5        |      1.0        |      0.5
     0.7        |      2.3        |      1.6
     0.8        |      4.0        |      3.2
     0.9        |      9.0        |      8.1
     0.95       |     19.0        |     18.0
     0.99       |     99.0        |     98.0
     1.0        |       ∞         |       ∞
```

**Critical Insight**: Beyond 70% utilization, latency degrades exponentially. Manufacturing-grade systems target **60-70% utilization** maximum.

```rust
/// M/M/1 queue performance model
struct MMOneQueue {
    arrival_rate: f64,    // λ
    service_rate: f64,    // μ
}

impl MMOneQueue {
    /// System utilization (ρ = λ/μ)
    fn utilization(&self) -> f64 {
        self.arrival_rate / self.service_rate
    }

    /// Average time in system (W = 1/(μ-λ))
    fn avg_system_time(&self) -> Option<Duration> {
        if self.arrival_rate >= self.service_rate {
            None  // Unstable system
        } else {
            Some(Duration::from_secs_f64(
                1.0 / (self.service_rate - self.arrival_rate)
            ))
        }
    }

    /// Average queue length (L_q = ρ²/(1-ρ))
    fn avg_queue_length(&self) -> Option<f64> {
        let rho = self.utilization();
        if rho >= 1.0 {
            None  // Unstable
        } else {
            Some(rho * rho / (1.0 - rho))
        }
    }

    /// Is system stable?
    fn is_stable(&self) -> bool {
        self.arrival_rate < self.service_rate
    }

    /// Is system healthy (< 70% utilization)?
    fn is_healthy(&self) -> bool {
        self.utilization() < 0.70
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mm1_stability() {
        // Stable system
        let queue = MMOneQueue {
            arrival_rate: 70.0,
            service_rate: 100.0,
        };
        assert!(queue.is_stable());
        assert_eq!(queue.utilization(), 0.70);
        assert_eq!(
            queue.avg_system_time().unwrap().as_secs_f64(),
            1.0 / 30.0  // 33ms
        );

        // Unstable system
        let overloaded = MMOneQueue {
            arrival_rate: 100.0,
            service_rate: 100.0,
        };
        assert!(!overloaded.is_stable());
        assert!(overloaded.avg_system_time().is_none());
    }
}
```

---

## Admission Control Algorithms

Admission control prevents system overload by **rejecting work before it enters the system**. This is preferable to accepting work and failing later.

### 1. Token Bucket Rate Limiter

Token bucket is the industry-standard admission control algorithm:

```
Algorithm:
1. Bucket holds B tokens (capacity)
2. Tokens refill at rate R (tokens/sec)
3. Each request consumes 1 token
4. If tokens available: ADMIT, else: REJECT

Properties:
- Allows bursts up to B
- Average rate limited to R
- O(1) admission decision
```

#### Implementation

```rust
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

/// Token bucket rate limiter
pub struct TokenBucket {
    capacity: f64,           // Maximum tokens (burst capacity)
    refill_rate: f64,        // Tokens per second
    tokens: Mutex<f64>,      // Current tokens
    last_refill: Mutex<Instant>,
}

impl TokenBucket {
    pub fn new(capacity: f64, refill_rate: f64) -> Self {
        Self {
            capacity,
            refill_rate,
            tokens: Mutex::new(capacity),
            last_refill: Mutex::new(Instant::now()),
        }
    }

    /// Attempt to acquire token (non-blocking)
    pub async fn try_acquire(&self) -> bool {
        self.try_acquire_tokens(1.0).await
    }

    /// Attempt to acquire N tokens
    pub async fn try_acquire_tokens(&self, n: f64) -> bool {
        let mut tokens = self.tokens.lock().await;
        let mut last = self.last_refill.lock().await;

        // Refill tokens based on elapsed time
        let now = Instant::now();
        let elapsed = now.duration_since(*last).as_secs_f64();
        let new_tokens = elapsed * self.refill_rate;
        *tokens = (*tokens + new_tokens).min(self.capacity);
        *last = now;

        // Try to consume tokens
        if *tokens >= n {
            *tokens -= n;
            true  // Admitted
        } else {
            false  // Rejected
        }
    }

    /// Block until tokens available (with timeout)
    pub async fn acquire_timeout(&self, timeout: Duration) -> Result<(), ()> {
        let deadline = Instant::now() + timeout;
        loop {
            if self.try_acquire().await {
                return Ok(());
            }
            if Instant::now() >= deadline {
                return Err(());
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    }
}

#[cfg(test)]
mod token_bucket_tests {
    use super::*;

    #[tokio::test]
    async fn test_token_bucket_burst() {
        // 100 tokens/sec, burst of 10
        let bucket = TokenBucket::new(10.0, 100.0);

        // Should allow burst of 10
        for _ in 0..10 {
            assert!(bucket.try_acquire().await);
        }

        // 11th should be rejected
        assert!(!bucket.try_acquire().await);

        // Wait for refill (100ms = 10 tokens)
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert!(bucket.try_acquire().await);
    }

    #[tokio::test]
    async fn test_token_bucket_sustained_rate() {
        let bucket = TokenBucket::new(1.0, 10.0);  // 10 req/sec
        let mut admitted = 0;

        for _ in 0..100 {
            if bucket.try_acquire().await {
                admitted += 1;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }

        // Should admit ~10 requests over 1 second
        assert!(admitted >= 8 && admitted <= 12);
    }
}
```

### 2. Leaky Bucket

Leaky bucket enforces constant output rate (traffic shaping):

```rust
use std::collections::VecDeque;

/// Leaky bucket traffic shaper
pub struct LeakyBucket<T> {
    capacity: usize,
    leak_rate: Duration,  // Time between leaks
    queue: Mutex<VecDeque<T>>,
}

impl<T> LeakyBucket<T> {
    pub fn new(capacity: usize, leak_rate: Duration) -> Self {
        Self {
            capacity,
            leak_rate,
            queue: Mutex::new(VecDeque::with_capacity(capacity)),
        }
    }

    /// Try to add item to bucket
    pub async fn try_add(&self, item: T) -> Result<(), T> {
        let mut queue = self.queue.lock().await;
        if queue.len() < self.capacity {
            queue.push_back(item);
            Ok(())
        } else {
            Err(item)  // Bucket full
        }
    }

    /// Leak (process) next item
    pub async fn leak(&self) -> Option<T> {
        let mut queue = self.queue.lock().await;
        queue.pop_front()
    }

    /// Start leaking task
    pub fn start_leaking<F>(self: Arc<Self>, mut process: F)
    where
        F: FnMut(T) + Send + 'static,
        T: Send + 'static,
    {
        tokio::spawn(async move {
            loop {
                if let Some(item) = self.leak().await {
                    process(item);
                }
                tokio::time::sleep(self.leak_rate).await;
            }
        });
    }
}
```

### 3. Adaptive Admission Control

Adaptive systems adjust admission rate based on observed performance:

```rust
use std::sync::atomic::{AtomicU64, AtomicBool, Ordering};

/// Adaptive admission controller using additive increase, multiplicative decrease (AIMD)
pub struct AdaptiveAdmissionControl {
    /// Current admission limit
    limit: AtomicU64,
    /// Minimum limit (safety floor)
    min_limit: u64,
    /// Maximum limit (capacity ceiling)
    max_limit: u64,
    /// Increase step (additive)
    increase_step: u64,
    /// Decrease factor (multiplicative)
    decrease_factor: f64,
    /// Current in-flight requests
    in_flight: AtomicU64,
    /// Is system healthy?
    healthy: AtomicBool,
}

impl AdaptiveAdmissionControl {
    pub fn new(initial_limit: u64, min_limit: u64, max_limit: u64) -> Self {
        Self {
            limit: AtomicU64::new(initial_limit),
            min_limit,
            max_limit,
            increase_step: 10,
            decrease_factor: 0.5,
            in_flight: AtomicU64::new(0),
            healthy: AtomicBool::new(true),
        }
    }

    /// Try to acquire admission token
    pub fn try_acquire(&self) -> Option<AdmissionToken> {
        let current = self.in_flight.load(Ordering::Acquire);
        let limit = self.limit.load(Ordering::Acquire);

        if current < limit {
            self.in_flight.fetch_add(1, Ordering::Release);
            Some(AdmissionToken { control: self })
        } else {
            None  // At capacity
        }
    }

    /// Release admission token
    fn release(&self) {
        self.in_flight.fetch_sub(1, Ordering::Release);
    }

    /// Signal success (increase limit)
    pub fn signal_success(&self) {
        if self.healthy.load(Ordering::Acquire) {
            // Additive increase
            let current = self.limit.load(Ordering::Acquire);
            let new_limit = (current + self.increase_step).min(self.max_limit);
            self.limit.store(new_limit, Ordering::Release);
        }
    }

    /// Signal failure (decrease limit)
    pub fn signal_failure(&self) {
        self.healthy.store(false, Ordering::Release);

        // Multiplicative decrease
        let current = self.limit.load(Ordering::Acquire);
        let new_limit = ((current as f64 * self.decrease_factor) as u64)
            .max(self.min_limit);
        self.limit.store(new_limit, Ordering::Release);
    }

    /// Reset to healthy state
    pub fn reset_healthy(&self) {
        self.healthy.store(true, Ordering::Release);
    }

    pub fn current_limit(&self) -> u64 {
        self.limit.load(Ordering::Acquire)
    }

    pub fn in_flight_count(&self) -> u64 {
        self.in_flight.load(Ordering::Acquire)
    }
}

/// RAII token that releases on drop
pub struct AdmissionToken<'a> {
    control: &'a AdaptiveAdmissionControl,
}

impl Drop for AdmissionToken<'_> {
    fn drop(&mut self) {
        self.control.release();
    }
}

#[cfg(test)]
mod adaptive_tests {
    use super::*;

    #[test]
    fn test_adaptive_increase() {
        let control = AdaptiveAdmissionControl::new(100, 10, 1000);
        assert_eq!(control.current_limit(), 100);

        control.signal_success();
        assert_eq!(control.current_limit(), 110);  // +10

        control.signal_success();
        assert_eq!(control.current_limit(), 120);  // +10
    }

    #[test]
    fn test_adaptive_decrease() {
        let control = AdaptiveAdmissionControl::new(100, 10, 1000);

        control.signal_failure();
        assert_eq!(control.current_limit(), 50);  // *0.5

        control.signal_failure();
        assert_eq!(control.current_limit(), 25);  // *0.5

        control.signal_failure();
        assert_eq!(control.current_limit(), 12);  // *0.5

        control.signal_failure();
        assert_eq!(control.current_limit(), 10);  // Floor
    }

    #[test]
    fn test_admission_token_raii() {
        let control = AdaptiveAdmissionControl::new(10, 1, 100);
        assert_eq!(control.in_flight_count(), 0);

        {
            let _token = control.try_acquire().unwrap();
            assert_eq!(control.in_flight_count(), 1);
        }  // Token dropped

        assert_eq!(control.in_flight_count(), 0);
    }
}
```

---

## WIP Tokens and Pull-Only Release

**Work-in-Progress (WIP) tokens** enforce Kanban flow control by capping concurrent work:

```
Principle: Workers PULL tokens when ready (never PUSHED)

Benefits:
1. Natural backpressure (no tokens = wait)
2. Prevents queue explosion
3. Maintains flow (Little's Law)
4. Self-regulating system
```

### Implementation with Semaphores

```rust
use tokio::sync::{Semaphore, SemaphorePermit};
use std::sync::Arc;

/// Kanban WIP limiter using semaphore tokens
pub struct WipLimiter {
    semaphore: Arc<Semaphore>,
    max_wip: usize,
}

impl WipLimiter {
    pub fn new(max_wip: usize) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(max_wip)),
            max_wip,
        }
    }

    /// Acquire WIP token (blocks until available)
    pub async fn acquire(&self) -> WipToken {
        let permit = self.semaphore.acquire().await.unwrap();
        WipToken { permit }
    }

    /// Try to acquire WIP token (non-blocking)
    pub fn try_acquire(&self) -> Option<WipToken> {
        self.semaphore.try_acquire()
            .ok()
            .map(|permit| WipToken { permit })
    }

    /// Acquire with timeout
    pub async fn acquire_timeout(&self, timeout: Duration) -> Result<WipToken, ()> {
        tokio::time::timeout(timeout, self.acquire())
            .await
            .map_err(|_| ())
    }

    pub fn available_permits(&self) -> usize {
        self.semaphore.available_permits()
    }

    pub fn max_wip(&self) -> usize {
        self.max_wip
    }
}

/// RAII WIP token (releases permit on drop)
pub struct WipToken {
    permit: SemaphorePermit<'static>,
}

impl Drop for WipToken {
    fn drop(&mut self) {
        // Permit automatically released
    }
}

/// Kanban worker with WIP limiting
pub struct KanbanWorker<T> {
    wip_limiter: Arc<WipLimiter>,
    work_queue: Arc<Mutex<VecDeque<T>>>,
}

impl<T: Send + 'static> KanbanWorker<T> {
    pub fn new(max_wip: usize) -> Self {
        Self {
            wip_limiter: Arc::new(WipLimiter::new(max_wip)),
            work_queue: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    /// Add work to queue
    pub async fn enqueue(&self, work: T) {
        let mut queue = self.work_queue.lock().await;
        queue.push_back(work);
    }

    /// Start worker loop (pull-based)
    pub fn start_worker<F>(self: Arc<Self>, mut process: F)
    where
        F: FnMut(T) -> Result<(), String> + Send + 'static,
        T: Send + 'static,
    {
        tokio::spawn(async move {
            loop {
                // 1. Acquire WIP token (PULL)
                let _token = self.wip_limiter.acquire().await;

                // 2. Pull work from queue
                let work = {
                    let mut queue = self.work_queue.lock().await;
                    queue.pop_front()
                };

                // 3. Process work
                if let Some(item) = work {
                    if let Err(e) = process(item) {
                        eprintln!("Work failed: {}", e);
                    }
                }
                // 4. Token released on drop (automatic)

                tokio::time::sleep(Duration::from_millis(10)).await;
            }
        });
    }
}

#[cfg(test)]
mod wip_tests {
    use super::*;

    #[tokio::test]
    async fn test_wip_limit_enforced() {
        let limiter = WipLimiter::new(3);

        let t1 = limiter.try_acquire().unwrap();
        let t2 = limiter.try_acquire().unwrap();
        let t3 = limiter.try_acquire().unwrap();

        // 4th should fail
        assert!(limiter.try_acquire().is_none());

        drop(t1);  // Release one
        assert!(limiter.try_acquire().is_some());
    }

    #[tokio::test]
    async fn test_wip_blocking() {
        let limiter = Arc::new(WipLimiter::new(2));

        let l1 = limiter.clone();
        let handle = tokio::spawn(async move {
            let _token = l1.acquire().await;
            tokio::time::sleep(Duration::from_millis(100)).await;
        });

        let l2 = limiter.clone();
        tokio::spawn(async move {
            let _token = l2.acquire().await;
            tokio::time::sleep(Duration::from_millis(100)).await;
        });

        // Wait for tokens to be held
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Should block until token available
        let start = Instant::now();
        let _token = limiter.acquire().await;
        let elapsed = start.elapsed();

        assert!(elapsed >= Duration::from_millis(90));
        handle.await.unwrap();
    }
}
```

### Kanban Board Pattern

```rust
use std::collections::HashMap;

/// Kanban board with column WIP limits
pub struct KanbanBoard<T> {
    columns: HashMap<String, KanbanColumn<T>>,
}

pub struct KanbanColumn<T> {
    name: String,
    wip_limit: usize,
    items: Vec<T>,
}

impl<T> KanbanBoard<T> {
    pub fn new() -> Self {
        Self {
            columns: HashMap::new(),
        }
    }

    pub fn add_column(&mut self, name: String, wip_limit: usize) {
        self.columns.insert(
            name.clone(),
            KanbanColumn {
                name,
                wip_limit,
                items: Vec::new(),
            },
        );
    }

    /// Try to move item to column (enforces WIP limit)
    pub fn move_to_column(&mut self, column: &str, item: T) -> Result<(), String> {
        let col = self.columns.get_mut(column)
            .ok_or_else(|| format!("Column not found: {}", column))?;

        if col.items.len() >= col.wip_limit {
            Err(format!("WIP limit reached for column: {}", column))
        } else {
            col.items.push(item);
            Ok(())
        }
    }

    pub fn column_wip(&self, column: &str) -> usize {
        self.columns.get(column)
            .map(|c| c.items.len())
            .unwrap_or(0)
    }

    pub fn is_column_full(&self, column: &str) -> bool {
        self.columns.get(column)
            .map(|c| c.items.len() >= c.wip_limit)
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod kanban_tests {
    use super::*;

    #[test]
    fn test_kanban_wip_limits() {
        let mut board = KanbanBoard::new();
        board.add_column("todo".to_string(), 5);
        board.add_column("in_progress".to_string(), 3);
        board.add_column("done".to_string(), 100);

        // Fill in_progress to limit
        assert!(board.move_to_column("in_progress", "task1").is_ok());
        assert!(board.move_to_column("in_progress", "task2").is_ok());
        assert!(board.move_to_column("in_progress", "task3").is_ok());

        // Should reject (WIP limit)
        assert!(board.move_to_column("in_progress", "task4").is_err());
        assert!(board.is_column_full("in_progress"));
    }
}
```

---

## Little's Law Application to Admission Control

Little's Law provides a **control theory** for admission:

```
L = λ × W

Rearranging for admission control:
λ_max = L_max / W_target

Where:
- L_max = Maximum queue depth (capacity)
- W_target = Target latency SLO
- λ_max = Maximum safe admission rate
```

### Implementation

```rust
/// Little's Law admission controller
pub struct LittlesLawController {
    max_queue_depth: f64,      // L_max
    target_latency_sec: f64,   // W_target
    current_depth: AtomicU64,  // L_current
}

impl LittlesLawController {
    pub fn new(max_queue_depth: u64, target_latency: Duration) -> Self {
        Self {
            max_queue_depth: max_queue_depth as f64,
            target_latency_sec: target_latency.as_secs_f64(),
            current_depth: AtomicU64::new(0),
        }
    }

    /// Maximum safe admission rate (λ_max = L_max / W_target)
    pub fn max_admission_rate(&self) -> f64 {
        self.max_queue_depth / self.target_latency_sec
    }

    /// Current utilization (L_current / L_max)
    pub fn utilization(&self) -> f64 {
        let current = self.current_depth.load(Ordering::Acquire) as f64;
        current / self.max_queue_depth
    }

    /// Should admit new request?
    pub fn should_admit(&self) -> bool {
        let current = self.current_depth.load(Ordering::Acquire);
        (current as f64) < self.max_queue_depth
    }

    /// Admit request (increment depth)
    pub fn admit(&self) -> Option<LittlesLawToken> {
        if self.should_admit() {
            self.current_depth.fetch_add(1, Ordering::Release);
            Some(LittlesLawToken { controller: self })
        } else {
            None
        }
    }

    /// Release request (decrement depth)
    fn release(&self) {
        self.current_depth.fetch_sub(1, Ordering::Release);
    }

    pub fn queue_depth(&self) -> u64 {
        self.current_depth.load(Ordering::Acquire)
    }
}

pub struct LittlesLawToken<'a> {
    controller: &'a LittlesLawController,
}

impl Drop for LittlesLawToken<'_> {
    fn drop(&mut self) {
        self.controller.release();
    }
}

#[cfg(test)]
mod littles_law_tests {
    use super::*;

    #[test]
    fn test_max_admission_rate() {
        // Max 100 items, target 1 second latency
        let controller = LittlesLawController::new(
            100,
            Duration::from_secs(1)
        );

        // Should allow 100 req/sec
        assert_eq!(controller.max_admission_rate(), 100.0);
    }

    #[test]
    fn test_admission_limit() {
        let controller = LittlesLawController::new(
            3,
            Duration::from_secs(1)
        );

        let t1 = controller.admit().unwrap();
        let t2 = controller.admit().unwrap();
        let t3 = controller.admit().unwrap();

        // 4th should fail
        assert!(controller.admit().is_none());
        assert_eq!(controller.utilization(), 1.0);

        drop(t1);
        assert!(controller.admit().is_some());
    }
}
```

---

## Bounded Channels for Backpressure

Rust async channels provide built-in backpressure via bounded queues:

### MPSC Bounded Channel

```rust
use tokio::sync::mpsc;

/// Producer with backpressure
pub struct BackpressureProducer<T> {
    tx: mpsc::Sender<T>,
}

impl<T> BackpressureProducer<T> {
    pub fn new(buffer_size: usize) -> (Self, mpsc::Receiver<T>) {
        let (tx, rx) = mpsc::channel(buffer_size);
        (Self { tx }, rx)
    }

    /// Send with backpressure (blocks if full)
    pub async fn send(&self, item: T) -> Result<(), mpsc::error::SendError<T>> {
        self.tx.send(item).await
    }

    /// Try to send (non-blocking, fails if full)
    pub fn try_send(&self, item: T) -> Result<(), mpsc::error::TrySendError<T>> {
        self.tx.try_send(item)
    }

    /// Send with timeout
    pub async fn send_timeout(
        &self,
        item: T,
        timeout: Duration,
    ) -> Result<(), SendTimeoutError<T>> {
        tokio::time::timeout(timeout, self.tx.send(item))
            .await
            .map_err(|_| SendTimeoutError::Timeout(item))?
            .map_err(SendTimeoutError::Disconnected)
    }
}

pub enum SendTimeoutError<T> {
    Timeout(T),
    Disconnected(mpsc::error::SendError<T>),
}

/// Consumer (pull-based)
pub async fn consumer_loop<T, F>(mut rx: mpsc::Receiver<T>, mut process: F)
where
    F: FnMut(T),
{
    while let Some(item) = rx.recv().await {
        process(item);
    }
}

#[cfg(test)]
mod channel_tests {
    use super::*;

    #[tokio::test]
    async fn test_bounded_channel_backpressure() {
        let (producer, mut rx) = BackpressureProducer::new(2);

        // Fill buffer
        producer.send(1).await.unwrap();
        producer.send(2).await.unwrap();

        // Should fail (buffer full)
        assert!(producer.try_send(3).is_err());

        // Consume one
        assert_eq!(rx.recv().await, Some(1));

        // Now should succeed
        producer.send(3).await.unwrap();
    }

    #[tokio::test]
    async fn test_send_timeout() {
        let (producer, _rx) = BackpressureProducer::new(1);

        producer.send(1).await.unwrap();

        // Should timeout (buffer full, no consumer)
        let result = producer.send_timeout(2, Duration::from_millis(50)).await;
        assert!(matches!(result, Err(SendTimeoutError::Timeout(_))));
    }
}
```

### Broadcast Channel Pattern

```rust
use tokio::sync::broadcast;

/// Broadcast backpressure with overflow strategy
pub struct BroadcastBackpressure<T: Clone> {
    tx: broadcast::Sender<T>,
}

impl<T: Clone> BroadcastBackpressure<T> {
    pub fn new(capacity: usize) -> Self {
        let (tx, _rx) = broadcast::channel(capacity);
        Self { tx }
    }

    /// Send to all subscribers
    pub fn send(&self, item: T) -> Result<usize, broadcast::error::SendError<T>> {
        self.tx.send(item)
    }

    /// Subscribe (pull-based receiver)
    pub fn subscribe(&self) -> broadcast::Receiver<T> {
        self.tx.subscribe()
    }

    pub fn receiver_count(&self) -> usize {
        self.tx.receiver_count()
    }
}

#[tokio::test]
async fn test_broadcast_backpressure() {
    let broadcast = BroadcastBackpressure::new(3);
    let mut sub1 = broadcast.subscribe();
    let mut sub2 = broadcast.subscribe();

    broadcast.send(1).unwrap();
    broadcast.send(2).unwrap();

    assert_eq!(sub1.recv().await.unwrap(), 1);
    assert_eq!(sub2.recv().await.unwrap(), 1);
}
```

---

## Metrics and Monitoring for Backpressure

### Key Metrics

```rust
use prometheus::{Histogram, IntCounter, IntGauge, Registry};

/// Backpressure metrics
pub struct BackpressureMetrics {
    // Queue depth (L in Little's Law)
    pub queue_depth: IntGauge,

    // Admission rate (λ)
    pub admission_rate: IntCounter,

    // Rejection rate
    pub rejection_rate: IntCounter,

    // Latency distribution (W in Little's Law)
    pub latency: Histogram,

    // Utilization (ρ = λ/μ)
    pub utilization: Histogram,

    // WIP tokens available
    pub wip_available: IntGauge,

    // Backpressure events
    pub backpressure_events: IntCounter,
}

impl BackpressureMetrics {
    pub fn new(registry: &Registry) -> Self {
        let queue_depth = IntGauge::new(
            "queue_depth",
            "Current queue depth (L)"
        ).unwrap();
        registry.register(Box::new(queue_depth.clone())).unwrap();

        let admission_rate = IntCounter::new(
            "admission_rate_total",
            "Total admitted requests (λ)"
        ).unwrap();
        registry.register(Box::new(admission_rate.clone())).unwrap();

        let rejection_rate = IntCounter::new(
            "rejection_rate_total",
            "Total rejected requests"
        ).unwrap();
        registry.register(Box::new(rejection_rate.clone())).unwrap();

        let latency = Histogram::with_opts(
            prometheus::HistogramOpts::new(
                "request_latency_seconds",
                "Request latency (W)"
            ).buckets(vec![0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 10.0])
        ).unwrap();
        registry.register(Box::new(latency.clone())).unwrap();

        let utilization = Histogram::with_opts(
            prometheus::HistogramOpts::new(
                "system_utilization",
                "System utilization (ρ = λ/μ)"
            ).buckets(vec![0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 1.0])
        ).unwrap();
        registry.register(Box::new(utilization.clone())).unwrap();

        let wip_available = IntGauge::new(
            "wip_tokens_available",
            "Available WIP tokens"
        ).unwrap();
        registry.register(Box::new(wip_available.clone())).unwrap();

        let backpressure_events = IntCounter::new(
            "backpressure_events_total",
            "Backpressure events triggered"
        ).unwrap();
        registry.register(Box::new(backpressure_events.clone())).unwrap();

        Self {
            queue_depth,
            admission_rate,
            rejection_rate,
            latency,
            utilization,
            wip_available,
            backpressure_events,
        }
    }

    /// Record admission attempt
    pub fn record_admission(&self, admitted: bool) {
        if admitted {
            self.admission_rate.inc();
        } else {
            self.rejection_rate.inc();
            self.backpressure_events.inc();
        }
    }

    /// Record request completion
    pub fn record_completion(&self, latency: Duration) {
        self.latency.observe(latency.as_secs_f64());
    }

    /// Update queue depth
    pub fn set_queue_depth(&self, depth: u64) {
        self.queue_depth.set(depth as i64);
    }

    /// Update utilization
    pub fn set_utilization(&self, utilization: f64) {
        self.utilization.observe(utilization);
    }

    /// Update WIP availability
    pub fn set_wip_available(&self, available: usize) {
        self.wip_available.set(available as i64);
    }
}
```

### Monitoring Dashboard Queries

```promql
# Queue depth over time (L)
queue_depth

# Admission rate (λ)
rate(admission_rate_total[1m])

# Rejection rate
rate(rejection_rate_total[1m])

# Rejection ratio
rate(rejection_rate_total[1m]) /
  (rate(admission_rate_total[1m]) + rate(rejection_rate_total[1m]))

# P50, P95, P99 latency (W)
histogram_quantile(0.50, rate(request_latency_seconds_bucket[5m]))
histogram_quantile(0.95, rate(request_latency_seconds_bucket[5m]))
histogram_quantile(0.99, rate(request_latency_seconds_bucket[5m]))

# Little's Law verification (L ≈ λ × W)
queue_depth /
  (rate(admission_rate_total[1m]) *
   histogram_quantile(0.50, rate(request_latency_seconds_bucket[5m])))
# Should be ≈ 1.0

# Utilization (ρ)
histogram_quantile(0.95, rate(system_utilization_bucket[5m]))

# Backpressure event rate
rate(backpressure_events_total[1m])

# WIP token exhaustion
(wip_tokens_total - wip_tokens_available) / wip_tokens_total
```

### Alerting Rules

```yaml
groups:
  - name: backpressure
    interval: 30s
    rules:
      # Queue depth approaching limit
      - alert: QueueDepthHigh
        expr: queue_depth > 0.8 * queue_depth_limit
        for: 2m
        annotations:
          summary: "Queue depth at 80% capacity"

      # High rejection rate
      - alert: HighRejectionRate
        expr: |
          rate(rejection_rate_total[5m]) /
          (rate(admission_rate_total[5m]) + rate(rejection_rate_total[5m]))
          > 0.05
        for: 5m
        annotations:
          summary: "Rejection rate above 5%"

      # Utilization too high (performance cliff)
      - alert: UtilizationHigh
        expr: |
          histogram_quantile(0.95, rate(system_utilization_bucket[5m]))
          > 0.75
        for: 5m
        annotations:
          summary: "System utilization above 75%"

      # Latency SLO violation
      - alert: LatencySLOViolation
        expr: |
          histogram_quantile(0.95, rate(request_latency_seconds_bucket[5m]))
          > 1.0
        for: 5m
        annotations:
          summary: "P95 latency exceeds 1s SLO"

      # Little's Law violation (system unstable)
      - alert: LittlesLawViolation
        expr: |
          abs(queue_depth /
            (rate(admission_rate_total[5m]) *
             histogram_quantile(0.50, rate(request_latency_seconds_bucket[5m])))
            - 1.0) > 0.3
        for: 10m
        annotations:
          summary: "Little's Law violation (L ≠ λW)"
```

---

## Integration Example: Complete Backpressure System

```rust
use std::sync::Arc;
use tokio::sync::mpsc;

/// Complete backpressure system
pub struct BackpressureSystem<T> {
    // Admission control
    admission: Arc<AdaptiveAdmissionControl>,

    // WIP limiting
    wip_limiter: Arc<WipLimiter>,

    // Rate limiting
    rate_limiter: Arc<TokenBucket>,

    // Work queue
    queue: mpsc::Sender<T>,

    // Metrics
    metrics: Arc<BackpressureMetrics>,
}

impl<T: Send + 'static> BackpressureSystem<T> {
    pub fn new(
        max_wip: usize,
        max_queue_depth: usize,
        rate_limit: f64,
        metrics: Arc<BackpressureMetrics>,
    ) -> (Self, mpsc::Receiver<T>) {
        let (tx, rx) = mpsc::channel(max_queue_depth);

        let system = Self {
            admission: Arc::new(AdaptiveAdmissionControl::new(
                max_wip as u64,
                (max_wip / 10) as u64,
                (max_wip * 2) as u64,
            )),
            wip_limiter: Arc::new(WipLimiter::new(max_wip)),
            rate_limiter: Arc::new(TokenBucket::new(rate_limit, rate_limit)),
            queue: tx,
            metrics,
        };

        (system, rx)
    }

    /// Submit work with full backpressure control
    pub async fn submit(&self, work: T) -> Result<(), BackpressureError<T>> {
        // 1. Rate limiting
        if !self.rate_limiter.try_acquire().await {
            self.metrics.record_admission(false);
            return Err(BackpressureError::RateLimited(work));
        }

        // 2. Admission control
        let _admission_token = self.admission.try_acquire()
            .ok_or_else(|| {
                self.metrics.record_admission(false);
                BackpressureError::AdmissionRejected(work)
            })?;

        // 3. Queue with bounded channel (backpressure)
        self.queue.send(work).await
            .map_err(|e| BackpressureError::QueueFull(e.0))?;

        self.metrics.record_admission(true);
        Ok(())
    }

    /// Try submit (non-blocking)
    pub fn try_submit(&self, work: T) -> Result<(), BackpressureError<T>> {
        // Check admission
        if self.admission.try_acquire().is_none() {
            self.metrics.record_admission(false);
            return Err(BackpressureError::AdmissionRejected(work));
        }

        // Try send
        self.queue.try_send(work)
            .map_err(|e| match e {
                mpsc::error::TrySendError::Full(w) => {
                    self.metrics.record_admission(false);
                    BackpressureError::QueueFull(w)
                }
                mpsc::error::TrySendError::Closed(w) => {
                    BackpressureError::SystemShutdown(w)
                }
            })
    }

    /// Process work from queue
    pub async fn process_loop<F>(
        self: Arc<Self>,
        mut rx: mpsc::Receiver<T>,
        mut process_fn: F,
    )
    where
        F: FnMut(T) -> Result<(), String> + Send + 'static,
    {
        while let Some(work) = rx.recv().await {
            // Acquire WIP token
            let _wip_token = self.wip_limiter.acquire().await;

            let start = Instant::now();
            match process_fn(work) {
                Ok(()) => {
                    self.admission.signal_success();
                }
                Err(e) => {
                    eprintln!("Processing error: {}", e);
                    self.admission.signal_failure();
                }
            }
            let latency = start.elapsed();
            self.metrics.record_completion(latency);
        }
    }

    /// Update metrics
    pub fn update_metrics(&self) {
        self.metrics.set_wip_available(self.wip_limiter.available_permits());
        self.metrics.set_utilization(
            1.0 - (self.wip_limiter.available_permits() as f64
                   / self.wip_limiter.max_wip() as f64)
        );
    }
}

pub enum BackpressureError<T> {
    RateLimited(T),
    AdmissionRejected(T),
    QueueFull(T),
    SystemShutdown(T),
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use prometheus::Registry;

    #[tokio::test]
    async fn test_backpressure_system() {
        let registry = Registry::new();
        let metrics = Arc::new(BackpressureMetrics::new(&registry));

        let (system, rx) = BackpressureSystem::new(
            10,    // max WIP
            100,   // queue depth
            100.0, // rate limit
            metrics.clone(),
        );
        let system = Arc::new(system);

        // Start processor
        let sys = system.clone();
        tokio::spawn(async move {
            sys.process_loop(rx, |work: u64| {
                // Simulate work
                std::thread::sleep(Duration::from_millis(10));
                Ok(())
            }).await;
        });

        // Submit work
        for i in 0..50 {
            match system.submit(i).await {
                Ok(()) => println!("Admitted: {}", i),
                Err(e) => println!("Rejected: {:?}", i),
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }

        system.update_metrics();
    }
}
```

---

## Summary

### Core Principles

1. **λ ≤ μ invariant**: Admission rate must not exceed capacity
2. **Pull-based flow**: Workers pull work (Kanban)
3. **WIP limits**: Cap concurrent work to prevent overload
4. **Little's Law**: L = λ × W governs all queueing systems
5. **Observable backpressure**: Metrics make queue state visible

### Implementation Patterns

| Pattern | Mechanism | Use Case |
|---------|-----------|----------|
| Token Bucket | Rate limiting | Enforce throughput limits |
| Leaky Bucket | Traffic shaping | Smooth bursty traffic |
| Adaptive Control | AIMD algorithm | Self-tuning admission |
| WIP Tokens | Semaphores | Kanban flow control |
| Bounded Channels | MPSC/broadcast | Natural backpressure |
| Little's Law Controller | Queue depth limiting | Latency SLO enforcement |

### Key Metrics

- **Queue depth (L)**: Work in system
- **Admission rate (λ)**: Requests/sec admitted
- **Latency (W)**: Time in system
- **Utilization (ρ)**: λ/μ ratio
- **Rejection rate**: Backpressure events
- **WIP availability**: Kanban tokens free

### Performance Targets

- **Utilization**: < 70% (avoid performance cliff)
- **Rejection rate**: < 1% under normal load
- **Latency P95**: Meet SLO with margin
- **Little's Law**: L ≈ λ × W (within 10%)

---

## References

### Queueing Theory
- Kleinrock, L. (1975). *Queueing Systems, Volume 1: Theory*
- Little, J.D.C. (1961). "A Proof for the Queuing Formula: L = λW"

### Admission Control
- Dovrolis, C. et al. (2001). "Proportional Differentiated Services"
- Welch, P.H. & Barnes, F.R.M. (2005). "Communicating Mobile Processes"

### TPS & Kanban
- Ohno, T. (1988). *Toyota Production System: Beyond Large-Scale Production*
- Anderson, D.J. (2010). *Kanban: Successful Evolutionary Change*

### Implementation Patterns
- Tokio Documentation: https://docs.rs/tokio
- Prometheus Best Practices: https://prometheus.io/docs/practices/

---

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Next Review**: 2026-03-09
