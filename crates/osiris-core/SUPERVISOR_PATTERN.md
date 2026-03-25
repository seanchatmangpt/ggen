# Supervisor Pattern Implementation - Rust Edition

## Overview

This document describes the implementation of Joe Armstrong's Erlang-style supervisor pattern for Rust in the OSIRIS Core system. The pattern provides automatic component restart and recovery strategies based on failure modes.

## Architecture

### Core Components

#### 1. **Restartable Trait** (`supervisor.rs`)

```rust
#[async_trait]
pub trait Restartable: Send + Sync {
    fn id(&self) -> &str;
    async fn start(&mut self) -> Result<()>;
    async fn stop(&mut self) -> Result<()>;
    async fn is_healthy(&self) -> bool;
    fn restart_count(&self) -> u32;
}
```

All supervised components must implement this trait to support restart lifecycle.

#### 2. **RestartStrategy Enum**

Three strategies mirror Erlang/OTP's supervision model:

##### **Transient** - Restart on Abnormal Failure
- Restarts only when component fails with error
- Normal termination = no restart
- Use case: Non-critical services that can safely stop
- Example: `SensorManager`

```rust
RestartStrategy::Transient {
    max_retries: Some(3),
    backoff: ExponentialBackoff::default(),
}
```

##### **Permanent** - Always Restart
- Restarts on any termination (error or normal)
- Unlimited restart capability (configurable)
- Use case: Critical system components that must never stop
- Example: `AndonSystem` (quality control)

```rust
RestartStrategy::Permanent {
    max_retries: Some(10),
    backoff: ExponentialBackoff::default(),
}
```

##### **Temporary** - Retry Then Give Up
- Tries N times then fails permanently
- Best effort approach
- Use case: Non-critical background tasks
- Example: `KaizenCycle` (continuous improvement)

```rust
RestartStrategy::Temporary {
    max_retries: 3,
    backoff: BackoffStrategy::Fixed { delay_ms: 500 },
}
```

#### 3. **BackoffStrategy Enum**

Controls retry timing to avoid thundering herd problems:

- **None**: No delay between retries
- **Fixed**: Constant delay (e.g., 1000ms)
- **Exponential**: Delay grows exponentially (2^n * initial_delay)
  - Prevents overload during cascading failures
  - Configurable initial delay, multiplier, max delay

#### 4. **SupervisorTree**

Main supervisor managing child processes:

```rust
pub struct SupervisorTree {
    children: Arc<RwLock<HashMap<String, ChildSpec>>>,
    handles: Arc<RwLock<HashMap<String, ChildHandle>>>,
    config: SupervisorConfig,
}
```

**Supervision Strategies** (Erlang-inspired):

- **OneForOne**: Restart only failed child (default)
- **OneForAll**: Restart all children if one fails
- **RestForOne**: Restart failed child and all later children

### Implementation Status

| Component | Strategy | Restart Behavior | Status |
|-----------|----------|------------------|--------|
| **SensorManager** | Transient | Restart on error, max 3 retries | ✅ Implemented |
| **AndonSystem** | Permanent | Always restart, max 10 retries (critical) | ✅ Implemented |
| **KaizenCycle** | Temporary | Try 3 times then give up | ✅ Implemented |

## Usage Examples

### Register a Component with Supervision

```rust
// Create supervisor
let supervisor = SupervisorTree::default_tree();

// Register SensorManager with Transient strategy
supervisor.register_child(
    "sensor_1".to_string(),
    RestartStrategy::Transient {
        max_retries: Some(3),
        backoff: BackoffStrategy::Exponential {
            initial_delay_ms: 100,
            multiplier: 2.0,
            max_delay_ms: 5000,
        },
    },
).await?;
```

### Spawn a Monitored Task

```rust
supervisor.spawn_monitored(
    "sensor_manager_task",
    || {
        tokio::spawn(async {
            let mut sensor = SensorManager::new("sensor_1".to_string());
            sensor.start().await
        })
    },
    RestartStrategy::Transient {
        max_retries: None, // unlimited retries
        backoff: BackoffStrategy::default(),
    },
).await?;
```

### Monitor System Health

```rust
// Get health status
let health = supervisor.get_health().await;
println!("Total children: {}", health.total_children);
println!("Dead children: {}", health.dead_children);
println!("Is healthy: {}", health.is_healthy);

// Get specific child stats
let stats = supervisor.get_child_stats("sensor_1").await?;
println!("Restart count: {}", stats.restart_count);
println!("Last restart: {:?}", stats.last_restart);
println!("State: {}", stats.state);

// Get all children stats
let all_stats = supervisor.get_all_children_stats().await;
for stats in all_stats {
    println!("Child {}: {} restarts", stats.id, stats.restart_count);
}
```

### Emit Supervision Signals

```rust
// Create signal for supervision events
let signal = supervisor.create_signal("sensor_1", SignalLevel::Warning);
engine.emit_signal(signal).await?;
```

## Backoff Strategy Details

### Exponential Backoff Example

```
Attempt 0: 100ms    (100 * 2^0)
Attempt 1: 200ms    (100 * 2^1)
Attempt 2: 400ms    (100 * 2^2)
Attempt 3: 800ms    (100 * 2^3)
Attempt 4: 1600ms   (100 * 2^4)
Attempt 5: 3200ms   (100 * 2^5)
...
Attempt 10: min(100 * 2^10, 30000) = 30000ms (capped)
```

Benefits:
- Prevents overload during cascading failures
- Gives systems time to recover
- Scales naturally with failure intensity
- Configurable caps prevent excessive waiting

## Component Implementations

### SensorManager (Transient)

**File**: `sensor_manager.rs`

```rust
pub struct SensorManager {
    id: String,
    restart_count: u32,
    is_running: bool,
}
```

- Periodically reads sensor data
- Transient restart: only on abnormal failure
- Max 3 retries before giving up
- Useful for non-critical data collection

### AndonSystem (Permanent)

**File**: `andon_system.rs`

```rust
pub struct AndonSystem {
    id: String,
    restart_count: u32,
    is_running: bool,
    alert_threshold: f64,
}
```

- Critical quality control system
- Permanent restart: always restarts (even on clean shutdown)
- Max 10 retries (configurable)
- Cannot fail - system integrity depends on it
- Emits quality alerts and signals

### KaizenCycle (Temporary)

**File**: `kaizen_cycle.rs`

```rust
pub struct KaizenCycle {
    id: String,
    restart_count: u32,
    is_running: bool,
    improvements_found: u32,
}
```

- Continuous improvement analysis
- Temporary restart: try 3 times then give up
- Best effort approach
- Detects improvement opportunities
- Failure is acceptable (non-critical)

## Test Coverage

### Unit Tests (lib.rs)
- Backoff strategy calculations
- RestartStrategy creation and defaults
- Supervisor creation and configuration
- Child registration and statistics
- Signal creation

**Run**: `cargo test -p osiris-core --lib supervisor`

### Integration Tests (supervisor_test.rs)
- Multiple child registration
- Health monitoring
- Statistics tracking
- Supervision strategy types
- Component implementations
- Duplicate registration handling
- Nonexistent child error handling

**Run**: `cargo test -p osiris-core --test supervisor_test`

## Error Handling

The supervisor uses OSIRIS error types:

```rust
pub enum OSIRISError {
    ConfigurationError(String),
    DomainNotFound(String),
    ServiceUnavailable(String),
    Timeout(String),
    // ... other error types
}
```

## Monitoring and Observability

### Signals

All supervision events emit `OSIRISSignal`:

```rust
OSIRISSignal {
    signal_type: "supervision_event",
    message: format!("Supervision event for child: {}", id),
    level: SignalLevel::Warning,
    source: Some("supervisor".to_string()),
    target: Some(child_id.to_string()),
    // ... metadata
}
```

### Logging

Supervision loop logs all events:
- Component start/restart
- Backoff delays
- Max retry reached
- State transitions
- Health status changes

Use `RUST_LOG=debug` to see detailed supervision logs.

## Future Enhancements

1. **Supervisor Hierarchy**: Nested supervisors for tree-structured systems
2. **Dynamic Strategy Changes**: Adjust restart strategy at runtime
3. **Metrics Collection**: Prometheus metrics for restart rates
4. **Circuit Breaker Integration**: Stop restarting after N consecutive failures
5. **Graceful Shutdown**: Orderly shutdown of all children
6. **Child Groups**: Group related children for coordinated restart
7. **Predefined Strategies**: Common patterns (e.g., database connection pool)
8. **Resource Limits**: Memory/CPU limits per child
9. **Health Checks**: Periodic health verification between restarts
10. **Rollback Strategy**: Automatic rollback on cascading failures

## References

- **Erlang/OTP Supervisor**: https://www.erlang.org/doc/design_principles/sup_principle.html
- **Joe Armstrong's "Programming Erlang"**: Chapter on Supervision
- **Fault Tolerance Patterns**: https://www.akka.io/docs/akka/current/general/supervision.html

## Summary

The supervisor pattern provides:
- ✅ Automatic component restart on failure
- ✅ Configurable retry strategies (Transient/Permanent/Temporary)
- ✅ Exponential backoff to prevent cascading failures
- ✅ Health monitoring and statistics
- ✅ Signal-based event system integration
- ✅ Comprehensive test coverage
- ✅ Production-ready error handling

This implementation brings Erlang's proven fault-tolerance model to Rust, enabling self-healing systems in OSIRIS.
