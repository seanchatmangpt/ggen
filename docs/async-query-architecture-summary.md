# Async DHT Query Architecture - Executive Summary

## Problem Solved

**Current Issue**: The P2P marketplace's `search()` method starts a DHT query but immediately returns `None`, with no mechanism to collect results from network peers.

**Solution**: Channel-based async result collection system with timeout handling, peer reputation tracking, and result deduplication.

## Architecture Overview

### Core Design Pattern: Producer-Consumer with Timeout

```
Client → QueryManager → DHT Query → [Event Loop] → Results Channel → QueryHandle → Client
                                         ↓
                                   Peer Responses
```

### Key Components

| Component | Purpose | Implementation |
|-----------|---------|----------------|
| **QueryId** | Unique query identifier | `uuid::Uuid` wrapper |
| **QueryContext** | In-flight query state | Stored in `HashMap<QueryId, QueryContext>` |
| **ResultChannel** | Async result passing | `tokio::sync::mpsc::unbounded_channel` |
| **QueryHandle** | Future awaiting results | Implements `collect_results()` |
| **Event Loop** | Process DHT responses | Background tokio task |

## Architecture Decisions

### Decision 1: Channel Type
**Choice**: `tokio::sync::mpsc::unbounded_channel`
- **Why**: MPSC pattern (multiple peer producers, single consumer)
- **Trade-off**: Unbounded may use more memory, but prevents blocking peer responses
- **Alternative**: Bounded channel (rejected: could block event loop)

### Decision 2: Query-to-Future Mapping
**Choice**: `HashMap<QueryId, QueryContext>` with `RwLock`
- **Why**: O(1) lookup in event loop, concurrent reads for status checks
- **Trade-off**: Requires cleanup task for expired queries
- **Alternative**: `DashMap` (rejected: unnecessary complexity)

### Decision 3: Timeout Strategy
**Choice**: `tokio::time::timeout` wrapper around `QueryHandle`
- **Why**: Native tokio support, cancellation-safe
- **Trade-off**: Fixed timeout per query (configurable via `QueryConfig`)
- **Alternative**: `tokio::select!` with manual timer (rejected: more verbose)

### Decision 4: Result Deduplication
**Choice**: `HashSet<PackageId>` for tracking seen packages
- **Why**: O(1) duplicate detection
- **Trade-off**: Requires `PackageId` to implement `Hash` + `Eq`
- **Alternative**: `Vec::dedup()` (rejected: O(n²) complexity)

## Implementation Phases

### Phase 1: Infrastructure (30 min)
- Add `QueryId`, `QueryContext`, `QueryHandle` types
- Add `in_flight_queries` map to `P2PRegistry`
- Add `QueryConfig` to `P2PConfig`

### Phase 2: Event Processing (45 min)
- Enhance `process_events()` to handle Kademlia events
- Add channel sending logic for peer responses
- Add peer reputation updates
- Add query cleanup task

### Phase 3: Search Method (30 min)
- Rewrite `search()` to create `QueryHandle`
- Add timeout handling with `tokio::time::timeout`
- Add result deduplication and filtering
- Add metrics collection

### Phase 4: Testing (45 min)
- Unit tests for `QueryHandle` (deduplication, early exit, timeout)
- Integration test with multi-node P2P network
- Performance benchmarks (throughput, latency)

**Total Implementation Time**: ~2.5 hours

## Code Structure

### New Types (in `async_query_architecture.rs`)
```rust
pub struct QueryId(uuid::Uuid);
pub struct QueryConfig { timeout, max_results, adaptive_timeout, fan_out }
pub struct QueryHandle { query_id, result_receiver, max_results }
pub struct QueryMetrics { total_queries, successful_queries, timed_out_queries, ... }
struct QueryContext { query_id, result_sender, started_at, timeout_duration, ... }
struct QueryResult { package, peer_id, response_time }
```

### Modified Files
1. **`ggen-marketplace/src/backend/p2p.rs`**
   - Add `in_flight_queries`, `query_config`, `query_metrics` fields
   - Rewrite `process_events()` to handle Kademlia responses
   - Rewrite `search()` to use async result collection
   - Add `spawn_event_loop()` method

2. **`ggen-marketplace/src/backend/mod.rs`**
   - Export `async_query_architecture` module

3. **`ggen-marketplace/tests/integration/p2p_async_query_test.rs`** (new)
   - Integration tests for multi-peer queries

## Data Flow Sequence

### 1. Query Initiation (Client → Registry)
```rust
// Client code
let results = registry.search(&query).await?;

// Registry creates QueryHandle
let query_id = QueryId::new();
let (tx, rx) = mpsc::unbounded_channel();
in_flight_queries.insert(query_id, QueryContext { tx, ... });
swarm.behaviour_mut().kademlia.get_record(key);
return QueryHandle { rx, ... };
```

### 2. Peer Responses (DHT → Event Loop → Channel)
```rust
// Event loop processing
SwarmEvent::Behaviour(Kademlia(OutboundQueryProgressed { ... })) => {
    let package = parse_record(record)?;
    let ctx = in_flight_queries.get(&query_id)?;
    ctx.result_sender.send(QueryResult { package, peer_id, ... })?;
    record_peer_success(peer_id, response_time).await;
}
```

### 3. Result Collection (Channel → QueryHandle → Client)
```rust
// QueryHandle awaits results
while let Some(result) = rx.recv().await {
    if seen_ids.insert(result.package.id) {
        results.push(result.package);
    }
    if results.len() >= max_results { break; }
}
```

### 4. Timeout Handling
```rust
// Registry wraps with timeout
match timeout(Duration::from_secs(10), handle.collect_results()).await {
    Ok(results) => results,                    // Success
    Err(_) => handle.get_partial_results()?    // Timeout
}
```

## Error Handling

| Error | Strategy | Example |
|-------|----------|---------|
| **Peer Unreachable** | Record failure, continue | `record_peer_failure(peer_id)` |
| **Corrupt Data** | Record failure, skip result | `serde_json::from_slice()? else record_failure` |
| **Query Timeout** | Return partial results | `timeout.elapsed() → get_partial_results()` |
| **Channel Closed** | Remove from in-flight | `SendError → remove(query_id)` |

## Performance Characteristics

### Expected Metrics
- **Query Latency (p50)**: ~100ms (3-peer fan-out)
- **Query Latency (p95)**: ~500ms (includes retries)
- **Timeout Duration**: 10s (configurable)
- **Max Results**: 20 (configurable)
- **Deduplication**: O(n) with HashSet
- **Memory per Query**: ~1KB + result size

### Scalability
- **Concurrent Queries**: Limited by tokio runtime (10k+)
- **Peers per Query**: Fan-out factor (default 3)
- **Results per Query**: Max results setting (default 20)

## Testing Strategy

### Unit Tests
```rust
test_query_handle_deduplication()  // Verify unique results
test_query_handle_early_exit()      // Stop at max_results
test_query_handle_timeout()         // Handle timeout gracefully
```

### Integration Tests
```rust
test_async_query_multiple_peers()   // 3-node network
test_peer_reputation_update()       // Verify reputation tracking
test_concurrent_queries()           // Multiple queries in parallel
```

### Performance Tests
```rust
bench_query_throughput()            // Queries per second
bench_query_latency()               // p50, p95, p99 latency
bench_memory_usage()                // Under load
```

## Configuration Examples

### High-Throughput (Batch Processing)
```rust
QueryConfig {
    default_timeout: Duration::from_secs(5),
    max_results: 50,
    adaptive_timeout: true,
    fan_out: 5,
}
```

### Low-Latency (Interactive UI)
```rust
QueryConfig {
    default_timeout: Duration::from_secs(2),
    max_results: 10,
    adaptive_timeout: false,
    fan_out: 1,
}
```

### Production (Balanced)
```rust
QueryConfig {
    default_timeout: Duration::from_secs(10),
    max_results: 20,
    adaptive_timeout: true,
    fan_out: 3,
}
```

## Monitoring

### Metrics to Track
```rust
let metrics = registry.query_metrics();
println!("Total queries: {}", metrics.total_queries.load());
println!("Success rate: {:.2}%",
    100.0 * metrics.successful_queries.load() as f64 / metrics.total_queries.load() as f64
);
println!("Avg latency: {}ms", metrics.avg_query_duration_ms.load());
println!("In-flight: {}", metrics.in_flight_count.load());
```

### Tracing Integration
```rust
#[instrument(skip(self), fields(query_id = %query_id))]
async fn search_dht_async(&self, query: &Query) -> Result<Vec<Package>> {
    Span::current().record("peer_count", peer_count);
    Span::current().record("result_count", results.len());
}
```

## Migration Checklist

- [ ] Add `async_query_architecture.rs` module
- [ ] Add fields to `P2PRegistry` struct
- [ ] Update `P2PRegistry::new()` constructor
- [ ] Implement `process_events()` event handling
- [ ] Implement `handle_kademlia_event()` method
- [ ] Implement `cleanup_expired_queries()` method
- [ ] Rewrite `search()` to use `QueryHandle`
- [ ] Add `spawn_event_loop()` method
- [ ] Write unit tests for `QueryHandle`
- [ ] Write integration test for multi-peer queries
- [ ] Add performance benchmarks
- [ ] Update documentation
- [ ] Deploy and monitor metrics

## Future Enhancements

### 1. Streaming Results (v2.5.0)
Return `Stream<Package>` instead of `Vec<Package>` for better UX on slow networks.

### 2. Query Caching (v2.6.0)
Cache recent query results with TTL-based invalidation.

### 3. Adaptive Fan-Out (v2.7.0)
Dynamically adjust fan-out based on network conditions and peer reputation.

### 4. Query Federation (v3.0.0)
Combine results from multiple DHT networks and centralized registries.

## Success Criteria

✅ **Functional**
- Queries return results from remote peers
- Timeout returns partial results gracefully
- Peer reputation updates correctly

✅ **Performance**
- p50 latency < 200ms
- p95 latency < 1s
- Throughput > 100 queries/sec

✅ **Reliability**
- No memory leaks from in-flight queries
- No deadlocks in event processing
- Graceful degradation on network issues

## References

- **Architecture Design**: `async_query_architecture.rs` (detailed comments)
- **Sequence Diagram**: `async-query-sequence-diagram.md` (visual flow)
- **Implementation Guide**: `async-query-implementation-guide.md` (step-by-step)
- **libp2p Kademlia**: https://docs.rs/libp2p-kad/
- **tokio Channels**: https://docs.rs/tokio/latest/tokio/sync/mpsc/

## Contact

For questions or issues:
1. Check implementation guide for troubleshooting
2. Review sequence diagram for data flow
3. Inspect `async_query_architecture.rs` comments
4. File issue on GitHub with metrics and logs
