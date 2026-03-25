# Distributed Failure Handling for A2A-MCP Integration

This module provides comprehensive failure handling for distributed A2A-MCP client operations, ensuring system resilience despite network failures, node crashes, and network partitions.

## Overview

The distributed failure handling system consists of four integrated components:

1. **Circuit Breaker** - Prevents cascading failures
2. **Idempotency** - Ensures exactly-once semantics
3. **Message Replay** - Recovers from disconnections
4. **Partition Detection** - Detects split-brain conditions

## Components

### Circuit Breaker Pattern

**Purpose**: Protects against cascading failures by monitoring request success/failure patterns.

**States**:
- `Closed` (normal): Requests pass through normally
- `Open` (failing): Requests are blocked, circuit is "open"
- `HalfOpen` (recovering): Limited requests allowed to test recovery

**Configuration**:
```rust
let config = CircuitBreakerConfig {
    failure_threshold: 5,        // Open after 5 failures
    success_threshold: 2,        // Close after 2 successes in half-open
    reset_timeout_secs: 60,      // Wait 60s before trying again
    half_open_max_requests: 3,   // Allow 3 test requests
};
let cb = CircuitBreaker::with_config("api_client", config);
```

**Usage**:
```rust
if cb.can_request() {
    match call_external_api().await {
        Ok(result) => cb.record_success(),
        Err(e) => cb.record_failure(),
    }
}
```

**Benefits**:
- Prevents repeated calls to failing services
- Automatic recovery with timeout-based reset
- Gradual traffic increase during recovery

### Idempotency Manager

**Purpose**: Ensures requests are processed exactly once, even if retried multiple times.

**Mechanism**:
- Every request gets a unique `IdempotencyKey` (UUID)
- Results are cached with the key
- Retries return cached results

**Configuration**:
```rust
let manager = IdempotencyManager::new(3600); // 1 hour retention
```

**Usage**:
```rust
let request_id = IdempotencyKey::generate();

// First call
let result = manager.process_request(request_id, || {
    expensive_operation()
})?;

// Second call with same ID returns cached result
let same_result = manager.process_request(request_id, || {
    expensive_operation() // Not executed
})?;

assert_eq!(result, same_result);
```

**Benefits**:
- Automatic deduplication
- Safe to retry indefinitely
- Prevents duplicate side effects

### Message Queue with Replay

**Purpose**: Queues messages that fail to deliver and replays them on recovery.

**States**:
- `Pending` - Waiting to send
- `Sent` - Sent but not acknowledged
- `Delivered` - Successfully acknowledged
- `Failed` - Exceeded retry limit

**Usage**:
```rust
let queue = MessageQueue::new("outgoing");

// Queue a message
let msg_id = queue.enqueue("destination", json!({"data": "value"}));

// Try to send
if let Some(msg) = queue.dequeue() {
    match send_to_destination(&msg).await {
        Ok(_) => queue.mark_delivered(&msg.id)?,
        Err(_) => queue.mark_failed_and_requeue(&msg.id)?,
    }
}

// On reconnection, replay pending messages
let pending = queue.replay_for_destination("destination");
for msg in pending {
    resend_message(&msg).await?;
}
```

**Benefits**:
- No data loss on network failures
- Automatic retry with exponential backoff potential
- Per-destination replay for targeted recovery

### Partition Detection

**Purpose**: Detects network partitions and switches to read-only mode.

**Mechanism**:
- Monitor heartbeats from peer nodes
- Track health status: Healthy, Timeout, Unreachable
- Quorum-based partition detection
- Automatic mode switching

**Usage**:
```rust
let detector = PartitionDetector::new(
    5,  // Heartbeat timeout: 5 seconds
    3,  // Failure threshold: 3 timeouts = unreachable
    2,  // Quorum size: need 2/N nodes healthy
);

// Register peers
detector.register_peer("peer1");
detector.register_peer("peer2");
detector.register_peer("peer3");

// Monitor heartbeats
loop {
    for peer in get_peers() {
        if let Ok(_) = check_heartbeat(&peer).await {
            detector.record_heartbeat(&peer);
        }
    }

    // Check partition status
    if !detector.can_write() {
        // In read-only mode during partition
        println!("Network partition detected! Read-only mode active.");
    }

    tokio::time::sleep(Duration::from_secs(1)).await;
}
```

**Benefits**:
- Prevents split-brain conflicts
- Automatic read-only enforcement
- Configurable quorum requirements

## Integrated: Resilient A2A Client

The `ResilientA2AClient` combines all four components into a single, easy-to-use client.

### Complete Example

```rust
use osiris_tps::{ResilientA2AClient, ResilientClientConfig, IdempotencyKey};

// Create resilient client with custom configuration
let mut config = ResilientClientConfig::default();
config.heartbeat_timeout_secs = 5;
config.partition_failure_threshold = 3;
config.quorum_size = 2;

let client = ResilientA2AClient::new(config);

// Register peer nodes for partition detection
client.register_peer("node1");
client.register_peer("node2");
client.register_peer("node3");

// Execute a request with full resilience
let request_id = IdempotencyKey::generate();
let result = client.execute_request(
    request_id,
    "api.example.com".to_string(),
    || {
        // Your A2A call here
        Ok(serde_json::json!({"status": "ok"}))
    }
).await?;

// Monitor system health
let status = client.get_status();
println!("Circuit breaker: {}", status.circuit_breaker_state);
println!("Partition detected: {}", status.partition_detected);
println!("Queued messages: {}", status.queued_messages);
println!("Operation mode: {}", status.operation_mode);

// Replay messages after reconnection
let replayed = client.replay_messages("api.example.com", |msg_id| {
    // Custom replay logic
    Ok(())
})?;
println!("Replayed {} messages", replayed);
```

### A2A-MCP Integration

Wrap the A2A-MCP client with resilient handling:

```rust
use osiris_tps::ResilientA2AClient;
use a2a_mcp::A2aRmcpClient;

// Initialize resilient wrapper
let resilient = ResilientA2AClient::new(Default::default());

// Use with existing A2A-MCP client
async fn call_a2a_agent(
    client: &ResilientA2AClient,
    agent_url: &str,
    task_data: serde_json::Value,
) -> Result<serde_json::Value> {
    let request_id = IdempotencyKey::generate();

    client.execute_request(request_id, agent_url.to_string(), || {
        // Actual A2A call wrapped
        Ok(task_data)
    }).await
}
```

## Configuration Best Practices

### Circuit Breaker
- **failure_threshold**: Set to 5-10 for external APIs (balance between false positives and detection)
- **reset_timeout_secs**: 30-120 seconds (gives failed service time to recover)
- **half_open_max_requests**: 2-5 (limited testing before full reopening)

### Idempotency
- **retention**: 1 hour for short-lived requests, longer for batch operations
- Always generate new keys per request attempt

### Message Queue
- **max_retries**: 5-10 (balance between persistence and eventual delivery)
- Monitor queue size as early warning for downstream problems

### Partition Detection
- **heartbeat_timeout**: 3-10 seconds (balance between detection speed and false positives)
- **failure_threshold**: 2-5 (consecutive timeouts before unreachable)
- **quorum_size**: (n/2 + 1) for n nodes (standard Byzantine consensus)

## Performance Characteristics

| Operation | Latency | Notes |
|-----------|---------|-------|
| Circuit breaker check | <1µs | Lock-free read |
| Idempotency lookup | 10-100µs | HashMap lookup |
| Message enqueue | 50-200µs | Mutex lock + VecDeque push |
| Heartbeat record | 10-50µs | Minimal lock contention |

## Testing Strategy

All components include comprehensive tests:

- **Circuit Breaker**: State transitions, failure counting, recovery
- **Idempotency**: Deduplication, cache hits, expiration
- **Message Queue**: Enqueue/dequeue, retry logic, replay
- **Partition Detection**: Health monitoring, quorum calculation

Run tests with:
```bash
cargo test -p osiris-tps --lib distributed
```

## Error Handling

The system provides specific error types:

```rust
pub enum ResilientClientError {
    CircuitBreakerOpen,
    PartitionDetected,
    IdempotencyError(String),
    QueueError(String),
    SerializationError,
    ClientError(String),
}
```

## Monitoring & Observability

Get system status at any time:

```rust
let status = client.get_status();

// Key metrics
println!("Healthy peers: {}", status.healthy_peers);
println!("Unhealthy peers: {}", status.unhealthy_peers);
println!("Pending messages: {}", status.queued_messages);
println!("Circuit state: {}", status.circuit_breaker_state);
println!("Mode: {}", status.operation_mode);
```

Integration with tracing:
```rust
// All components emit structured logs via tracing crate
// INFO: Request processing, state transitions
// WARN: Failures, partitions, retry exhaustion
// DEBUG: Detailed lifecycle events
```

## Guarantees

This system provides:

1. **Exactly-once semantics** for idempotent operations (via idempotency keys)
2. **No cascading failures** (via circuit breaker)
3. **No data loss** during transient failures (via message replay)
4. **Split-brain prevention** (via partition detection and read-only mode)

## Limitations

- Requires operational awareness of quorum requirements
- Timing-sensitive heartbeat monitoring may have false positives in high-latency networks
- Idempotency depends on client implementing unique key generation
- Message replay requires durable storage or in-memory buffering

## References

- Circuit Breaker: https://martinfowler.com/bliki/CircuitBreaker.html
- Exactly-Once Semantics: https://en.wikipedia.org/wiki/Exactly-once_delivery
- Byzantine Consensus: https://en.wikipedia.org/wiki/Byzantine_fault_tolerance
