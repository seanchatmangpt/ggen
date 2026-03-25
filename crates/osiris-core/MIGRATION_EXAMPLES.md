# Migration Examples - Applying Timeout Guards

This document shows step-by-step examples of migrating each RwLock to use TimedLock.

## Pattern 1: Simple Field Migration

### circuit_breaker.rs

**Before:**
```rust
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct JidokaCircuitBreaker {
    failures: Arc<RwLock<usize>>,
    last_failure_time: Arc<RwLock<Option<Instant>>>,
    success_count: Arc<RwLock<usize>>,
    failure_history: Arc<RwLock<Vec<Instant>>>,
}

impl JidokaCircuitBreaker {
    pub fn new(failure_threshold: usize, recovery_timeout_ms: u64) -> Self {
        Self {
            failures: Arc::new(RwLock::new(0)),
            last_failure_time: Arc::new(RwLock::new(None)),
            success_count: Arc::new(RwLock::new(0)),
            failure_history: Arc::new(RwLock::new(Vec::new())),
            // ...
        }
    }

    pub async fn record_failure(&self) -> Result<()> {
        // Deadlock risk - no timeout
        let mut failures = self.failures.write().await;
        *failures += 1;
        Ok(())
    }
}
```

**After:**
```rust
use osiris_core::{TimedLock, TimeoutConfig, LockError};
use anyhow::Result;

pub struct JidokaCircuitBreaker {
    failures: TimedLock<usize>,
    last_failure_time: TimedLock<Option<Instant>>,
    success_count: TimedLock<usize>,
    failure_history: TimedLock<Vec<Instant>>,
}

impl JidokaCircuitBreaker {
    pub fn new(failure_threshold: usize, recovery_timeout_ms: u64) -> Self {
        // Use state_manager timeout for circuit breaker (lower frequency, critical state)
        let config = TimeoutConfig::state_manager();

        Self {
            failures: TimedLock::new(0, config.clone()),
            last_failure_time: TimedLock::new(None, config.clone()),
            success_count: TimedLock::new(0, config.clone()),
            failure_history: TimedLock::new(Vec::new(), config),
            // ...
        }
    }

    pub async fn record_failure(&self) -> Result<()> {
        // Graceful degradation with timeout protection
        let mut failures = self.failures.write().await
            .map_err(|e| anyhow::anyhow!("Failed to acquire lock: {}", e))?;
        *failures += 1;
        Ok(())
    }
}
```

## Pattern 2: HashMap Storage

### a2a_service.rs

**Before:**
```rust
pub struct A2AService {
    message_queue: Arc<RwLock<Vec<A2AMessage>>>,
    subscriptions: Arc<RwLock<HashMap<String, Vec<String>>>>,
    message_handlers: Arc<RwLock<HashMap<String, Box<dyn MessageHandler + Send + Sync>>>>,
}

impl A2AService {
    pub fn new() -> Self {
        Self {
            message_queue: Arc::new(RwLock::new(Vec::new())),
            subscriptions: Arc::new(RwLock::new(HashMap::new())),
            message_handlers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn send_message(&self, message: A2AMessage) -> Result<()> {
        let mut queue = self.message_queue.write().await;
        queue.push(message);
        Ok(())
    }
}
```

**After:**
```rust
use osiris_core::{TimedLock, TimeoutConfig};
use anyhow::Result;

pub struct A2AService {
    message_queue: TimedLock<Vec<A2AMessage>>,
    subscriptions: TimedLock<HashMap<String, Vec<String>>>,
    message_handlers: TimedLock<HashMap<String, Box<dyn MessageHandler + Send + Sync>>>,
}

impl A2AService {
    pub fn new() -> Self {
        // Use policy_store timeout for messaging (medium traffic)
        let config = TimeoutConfig::policy_store();

        Self {
            message_queue: TimedLock::new(Vec::new(), config.clone()),
            subscriptions: TimedLock::new(HashMap::new(), config.clone()),
            message_handlers: TimedLock::new(HashMap::new(), config),
        }
    }

    pub async fn send_message(&self, message: A2AMessage) -> Result<()> {
        let mut queue = self.message_queue.write().await
            .map_err(|e| anyhow::anyhow!("Failed to queue message: {}", e))?;
        queue.push(message);
        Ok(())
    }
}
```

## Pattern 3: Sensor Buffers (High-Frequency)

### osiris-sensors/src/lib.rs

**Before:**
```rust
pub struct SensorManager {
    sensors: Arc<RwLock<HashMap<String, SensorConfig>>>,
    data_buffer: Arc<RwLock<HashMap<String, Vec<SensorDataType>>>>,
    is_initialized: Arc<RwLock<bool>>,
}

impl SensorManager {
    pub fn new() -> Self {
        Self {
            sensors: Arc::new(RwLock::new(HashMap::new())),
            data_buffer: Arc::new(RwLock::new(HashMap::new())),
            is_initialized: Arc::new(RwLock::new(false)),
        }
    }

    pub async fn add_sensor_data(&self, sensor_id: &str, data: SensorDataType) -> Result<()> {
        let mut buffer = self.data_buffer.write().await;
        buffer.entry(sensor_id.to_string())
            .or_insert_with(Vec::new)
            .push(data);
        Ok(())
    }
}
```

**After:**
```rust
use osiris_core::{TimedLock, TimeoutConfig};
use anyhow::Result;

pub struct SensorManager {
    sensors: TimedLock<HashMap<String, SensorConfig>>,
    data_buffer: TimedLock<HashMap<String, Vec<SensorDataType>>>,
    is_initialized: TimedLock<bool>,
}

impl SensorManager {
    pub fn new() -> Self {
        // Use sensor_buffer timeout for high-frequency sensor data (500ms)
        let config = TimeoutConfig::sensor_buffer();

        Self {
            sensors: TimedLock::new(HashMap::new(), config.clone()),
            data_buffer: TimedLock::new(HashMap::new(), config.clone()),
            is_initialized: TimedLock::new(false, config),
        }
    }

    pub async fn add_sensor_data(&self, sensor_id: &str, data: SensorDataType) -> Result<()> {
        let mut buffer = self.data_buffer.write().await
            .map_err(|e| anyhow::anyhow!("Sensor buffer timeout: {}", e))?;

        buffer.entry(sensor_id.to_string())
            .or_insert_with(Vec::new)
            .push(data);
        Ok(())
    }
}
```

## Pattern 4: With Error Recovery

### osiris-tps/src/kaizen.rs

**Before:**
```rust
pub struct KaizenManager {
    improvements: Arc<RwLock<HashMap<String, KaizenImprovement>>>,
    active_cycle: Arc<RwLock<Option<OngoingCycle>>>,
    cycle_history: Arc<RwLock<Vec<CycleHistory>>>,
    improvement_suggestions: Arc<RwLock<Vec<ImprovementSuggestion>>>,
}

pub async fn record_improvement(&self, improvement: KaizenImprovement) -> Result<()> {
    let mut improvements = self.improvements.write().await;
    improvements.insert(improvement.id.clone(), improvement);
    Ok(())
}
```

**After:**
```rust
use osiris_core::{TimedLock, TimeoutConfig, DeadlockDetector};
use anyhow::Result;
use tokio::time::{sleep, Duration};

pub struct KaizenManager {
    improvements: TimedLock<HashMap<String, KaizenImprovement>>,
    active_cycle: TimedLock<Option<OngoingCycle>>,
    cycle_history: TimedLock<Vec<CycleHistory>>,
    improvement_suggestions: TimedLock<Vec<ImprovementSuggestion>>,
    detector: DeadlockDetector,
}

impl KaizenManager {
    pub fn new() -> Self {
        let config = TimeoutConfig::policy_store();
        Self {
            improvements: TimedLock::new(HashMap::new(), config.clone()),
            active_cycle: TimedLock::new(None, config.clone()),
            cycle_history: TimedLock::new(Vec::new(), config.clone()),
            improvement_suggestions: TimedLock::new(Vec::new(), config),
            detector: DeadlockDetector::new(),
        }
    }

    pub async fn record_improvement(&self, improvement: KaizenImprovement) -> Result<()> {
        // Retry pattern with exponential backoff
        let mut attempts = 0;
        loop {
            match self.improvements.write().await {
                Ok(mut improvements) => {
                    improvements.insert(improvement.id.clone(), improvement);
                    return Ok(());
                }
                Err(e) => {
                    self.detector.record_timeout("kaizen_improvements").await;
                    attempts += 1;

                    if attempts >= 3 {
                        return Err(anyhow::anyhow!("Failed to record improvement: {}", e));
                    }

                    // Exponential backoff
                    sleep(Duration::from_millis(10 * (1 << attempts))).await;
                }
            }
        }
    }
}
```

## Pattern 5: Read-Heavy Path

### osiris-domains/src/lib.rs

**Before:**
```rust
pub struct DomainManager {
    domains: Arc<RwLock<HashMap<String, LifeDomain>>>,
    workflows: Arc<RwLock<HashMap<String, Workflow>>>,
    stage: Arc<RwLock<LifecycleStage>>,
}

pub async fn get_domain(&self, domain_id: &str) -> Result<Option<LifeDomain>> {
    let domains = self.domains.read().await;
    Ok(domains.get(domain_id).cloned())
}
```

**After:**
```rust
use osiris_core::{TimedLock, TimeoutConfig};
use anyhow::Result;

pub struct DomainManager {
    domains: TimedLock<HashMap<String, LifeDomain>>,
    workflows: TimedLock<HashMap<String, Workflow>>,
    stage: TimedLock<LifecycleStage>,
}

impl DomainManager {
    pub fn new() -> Self {
        let config = TimeoutConfig::state_manager();
        Self {
            domains: TimedLock::new(HashMap::new(), config.clone()),
            workflows: TimedLock::new(HashMap::new(), config.clone()),
            stage: TimedLock::new(LifecycleStage::Assessment, config),
        }
    }

    pub async fn get_domain(&self, domain_id: &str) -> Result<Option<LifeDomain>> {
        let domains = self.domains.read().await
            .map_err(|e| anyhow::anyhow!("Failed to read domains: {}", e))?;
        Ok(domains.get(domain_id).cloned())
    }
}
```

## Pattern 6: Clone + Arc for Shared Ownership

**Before:**
```rust
pub struct SharedManager(Arc<RwLock<InnerState>>);

impl Clone for SharedManager {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}
```

**After:**
```rust
use osiris_core::TimedLock;

pub struct SharedManager {
    state: TimedLock<InnerState>,
}

impl Clone for SharedManager {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(), // TimedLock already handles Arc cloning
        }
    }
}
```

## Testing Migration

### Unit Test Pattern

**Before:**
```rust
#[tokio::test]
async fn test_record_failure() {
    let cb = JidokaCircuitBreaker::new(5, 30000);
    cb.record_failure().await.unwrap();

    let failures = cb.failures.read().await;
    assert_eq!(*failures, 1);
}
```

**After:**
```rust
#[tokio::test]
async fn test_record_failure() {
    let cb = JidokaCircuitBreaker::new(5, 30000);
    cb.record_failure().await.unwrap();

    let failures = cb.failures.read().await.unwrap();
    assert_eq!(*failures, 1);
}
```

### Timeout Test Pattern

```rust
#[tokio::test]
async fn test_timeout_handling() {
    let lock = TimedLock::new(42, TimeoutConfig::new("test", Duration::from_millis(1), Duration::from_millis(1)));

    // Hold write lock
    let _guard = lock.write().await.unwrap();

    // Attempt read should timeout
    let result = lock.read().await;
    assert!(result.is_err());
}
```

## Summary of Changes

| Aspect | Before | After |
|--------|--------|-------|
| Type | `Arc<RwLock<T>>` | `TimedLock<T>` |
| Construction | `.new(RwLock::new(val))` | `TimedLock::new(val, config)` |
| Read | `.read().await` | `.read().await?` |
| Write | `.write().await` | `.write().await?` |
| Error Handling | None | `map_err(e => anyhow!("..."))` |
| Deadlock Protection | ❌ | ✅ |
| Monitoring | ❌ | ✅ (via DeadlockDetector) |
| Memory Overhead | Baseline | +200 bytes per lock |
| Performance | Baseline | <1% overhead |

## Incremental Adoption

1. **Start with high-priority components**:
   - Circuit breaker (prevents cascade failures)
   - Sensor buffers (high contention)
   - Message queues (inter-process communication)

2. **Add error handling gradually**:
   - Use `map_err()` for simple cases
   - Add retry logic for transient failures
   - Use cached values where appropriate

3. **Integrate monitoring**:
   - Create DeadlockDetector per manager
   - Log timeout events
   - Alert on sustained patterns

4. **Test thoroughly**:
   - Add unit tests for timeout scenarios
   - Stress test with concurrent access
   - Validate error recovery paths
