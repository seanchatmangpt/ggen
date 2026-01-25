# tai-state: Cloud-Native State Machine Persistence

Production-grade state machine persistence with Google Cloud Firestore, ACID transactions, eventual consistency handling, and automatic crash recovery.

## Features

- **ACID Transactions** - Multi-document atomic operations with optimistic locking
- **Event Sourcing** - Complete audit trail and replay capability
- **Snapshots** - Periodic full state saves for fast recovery
- **Distributed Locking** - Pessimistic locking for high-contention scenarios
- **Eventual Consistency** - Conflict resolution and vector clock causality tracking
- **Crash Recovery** - Automatic state restoration from Firestore
- **Type-Safe** - Full type safety with zero `unwrap()`/`expect()` in production code

## Quick Start

```rust
use tai_state::{FirestoreStore, StateSnapshot, StateId, ChangeMetadata};
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize store
    let store = FirestoreStore::new("my-gcp-project").await?;

    // Save state
    let snapshot = StateSnapshot::new(
        StateId::new("counter".into()),
        json!({"count": 0}),
        ChangeMetadata::new("system", "init", None),
    );

    store.save_state(
        "governor1",
        "counter",
        snapshot,
        ChangeMetadata::new("system", "init", None),
    ).await?;

    // Get state with version for conflict detection
    let (state, version) = store.get_state("governor1", "counter").await?;

    // Update with optimistic locking
    let new_state = StateSnapshot::new(
        StateId::new("counter".into()),
        json!({"count": 1}),
        ChangeMetadata::new("system", "increment", Some(version)),
    );

    let new_version = store.update_state(
        "governor1",
        "counter",
        new_state,
        version,
        ChangeMetadata::new("system", "update", None),
    ).await?;

    println!("Updated to version {}", new_version);
    Ok(())
}
```

## Core Modules

### `firestore_store` - Document Store

Type-safe wrapper around Firestore with:
- Hierarchical document organization
- Version tracking for optimistic locking
- Timestamp management for causality
- Event logging for audit trails

```rust
let store = FirestoreStore::new("project-id").await?;
store.save_state("gov1", "state1", snapshot, metadata).await?;
let (state, version) = store.get_state("gov1", "state1").await?;
```

### `transactions` - ACID Operations

Multi-document transactions with:
- Read-then-write pattern
- Deadlock detection
- Exponential backoff with jitter
- Distributed locking

```rust
let manager = TransactionManager::new(store.clone());

manager.execute_transaction("gov1", |store| {
    Box::pin(async move {
        // Read, compute, write atomically
        Ok(())
    })
}).await?;
```

### `eventual_consistency` - Conflict Resolution

Handles distributed consistency with:
- Last-write-wins resolver
- Causality-aware resolver
- Vector clock support
- Read-repair pattern

```rust
let ec = EventualConsistency::new();
let resolved = ec.resolve_conflict(&state1, &state2)?;
```

### `state_machine_persister` - Event Sourcing

State machine persistence with:
- Event logging (audit trail)
- Snapshotting (fast recovery)
- Event replay (reconstruct any state)
- Crash recovery (automatic)

```rust
let persister = StateMachinePersister::with_defaults(store);
persister.log_transition("gov1", Some(&from), &to, "action", metadata).await?;
```

### `types` - Core Domain Types

- `StateSnapshot` - Complete state at a point in time
- `Event` - State transition log entry
- `ChangeMetadata` - Who/when/why changed
- `VectorClockSnapshot` - Distributed causality tracking
- `DistributedLock` - Pessimistic lock holder

## Architecture

```
┌──────────────────────────────────────────┐
│        Application Code                  │
├──────────────────────────────────────────┤
│  StateMachinePersister (Event Sourcing)  │
│  + Log transitions                       │
│  + Create snapshots                      │
│  + Replay events                         │
├──────────────────────────────────────────┤
│  TransactionManager (ACID)               │
│  + Multi-document transactions           │
│  + Deadlock detection                    │
│  + Distributed locking                   │
├──────────────────────────────────────────┤
│  EventualConsistency                     │
│  + Conflict resolution                   │
│  + Vector clock tracking                 │
│  + Read-repair                           │
├──────────────────────────────────────────┤
│  FirestoreStore (Document Store)         │
│  + Save/get/delete states                │
│  + Log events                            │
│  + Manage snapshots                      │
├──────────────────────────────────────────┤
│  Google Cloud Firestore                  │
│  (Persistent storage)                    │
└──────────────────────────────────────────┘
```

## Data Model

### State Document

```firestore
governors/{governor_id}/states/{state_id}
├── data: {...}              // JSON state
├── version: 5               // Optimistic lock counter
├── timestamp: 2024-01-20T15:30:00Z
└── metadata:
    ├── changed_by: "user-123"
    ├── reason: "transition"
    ├── parent_version: 4
    └── context: {...}
```

### Event Document

```firestore
governors/{governor_id}/events/{event_id}
├── action: "increment"
├── from_state: {...}        // Complete state before
├── to_state: {...}          // Complete state after
├── timestamp: 2024-01-20T15:30:00Z
├── causality: {             // Vector clock
│   └── node1: 5
├── metadata: {...}
```

### Snapshot Document

```firestore
governors/{governor_id}/snapshots/{snapshot_id}
├── state: {...}             // Complete state snapshot
├── timestamp: 2024-01-20T15:30:00Z
```

## Consistency Models

### Optimistic Locking

Best for low-conflict scenarios:

```rust
// Read
let (state, version) = store.get_state("gov1", "counter").await?;

// Write with version check
store.update_state("gov1", "counter", new_state, version, metadata).await?;
// Returns VersionConflict if concurrent modification detected
```

**Advantages**: No blocking, high throughput
**Disadvantages**: Retries needed on conflict

### Distributed Locking

Best for high-contention scenarios:

```rust
let lock = manager.acquire_lock("resource", "client1", Duration::from_secs(30)).await?;
// Critical section - exclusive access
manager.release_lock("resource", &lock.lock_token).await?;
```

**Advantages**: No retries, guaranteed isolation
**Disadvantages**: Blocking, potential deadlocks

### Eventual Consistency

For non-critical data:

```rust
let ec = EventualConsistency::new();
let resolved = ec.resolve_conflict(&replica1, &replica2)?;
```

**Advantages**: High availability, low latency
**Disadvantages**: Temporary inconsistency

## Recovery Patterns

### Snapshot + Event Replay

```
Recovery State = Load(Latest Snapshot) + Replay(Events after snapshot)
```

Fast recovery even with large event logs.

### Full Event Replay (No Snapshot)

```
Recovery State = Replay(All events from start)
```

Slower but more compact storage.

## Performance

### Typical Latencies

- Save state: ~50-100ms
- Get state: ~20-50ms
- Update state: ~100-200ms (includes retry overhead)
- Transaction: ~150-300ms (read + write + commit)
- Recovery: ~100-500ms (depends on event count)

### Optimization Tips

1. **Snapshot interval**: 100-500 events (tune for your workload)
2. **Batch operations**: Use `batch_get_states()` for multiple reads
3. **Cleanup**: Run event cleanup regularly
4. **Indexes**: Create Firestore indexes for queried fields

## Testing

```bash
# Run unit tests
cargo test -p tai-state --lib

# Run integration tests
cargo test -p tai-state --test '*'

# Run specific test
cargo test -p tai-state test_optimistic_locking_success -- --nocapture
```

## Production Checklist

- [ ] Firestore project created
- [ ] IAM roles configured
- [ ] Indexes created for queries
- [ ] Backup policy configured
- [ ] Monitoring/alerting set up
- [ ] Load testing completed
- [ ] Disaster recovery tested
- [ ] Documentation reviewed

## Examples

See `tests/state_integration_tests.rs` for comprehensive examples:
- Basic persistence
- Optimistic locking
- ACID transactions
- Event sourcing
- Crash recovery
- Conflict resolution
- Distributed locking

## Error Handling

All operations return `Result<T>` with detailed error types:

```rust
pub enum Error {
    Firestore(String),
    StateNotFound(String),
    VersionConflict { doc_id, expected, actual },
    TransactionFailed(String),
    DeadlockDetected(String),
    Timeout(String),
    StaleRead { context },
    ConflictResolutionFailed(String),
    EventSourcingError(String),
    // ... more
}
```

Transient errors can be retried:

```rust
if err.is_transient() {
    let backoff = err.backoff_duration();
    tokio::time::sleep(backoff).await;
}
```

## Dependencies

- **tokio**: Async runtime
- **serde**: Serialization
- **chrono**: Timestamps
- **uuid**: Unique identifiers
- **thiserror**: Error types
- **async-trait**: Async traits

## License

MIT

## Documentation

Full documentation available in `/docs/tai-state/40-state-management.md`
