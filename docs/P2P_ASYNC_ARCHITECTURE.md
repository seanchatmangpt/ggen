# P2P Async Query Architecture Design

**Agent**: system-architect (async-architect)
**Date**: 2025-11-02
**Target**: High-performance async P2P queries

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    P2P Registry (Arc<RwLock>)               │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │   Swarm      │  │ Query Queue  │  │ Event Loop   │    │
│  │ (libp2p)     │  │ (mpsc)       │  │ (tokio)      │    │
│  └──────────────┘  └──────────────┘  └──────────────┘    │
│         │                  │                  │            │
│         └──────────────────┴──────────────────┘            │
│                            │                               │
└────────────────────────────┼───────────────────────────────┘
                             │
                ┌────────────┴────────────┐
                │                         │
         ┌──────▼──────┐           ┌─────▼──────┐
         │ Kademlia    │           │ Gossipsub  │
         │ DHT Queries │           │ PubSub     │
         └─────────────┘           └────────────┘
```

## Core Components

### 1. Event Loop (Primary)

**Location**: `process_events()`

**Pattern**: Infinite loop with tokio::select!

```rust
pub async fn process_events(&self) -> Result<()> {
    let mut swarm = self.swarm.write().await;
    let mut interval = tokio::time::interval(Duration::from_millis(100));

    loop {
        tokio::select! {
            // Process swarm events
            event = swarm.select_next_some() => {
                self.handle_swarm_event(event).await?;
            }

            // Periodic maintenance
            _ = interval.tick() => {
                self.maintenance_tick().await?;
            }

            // Graceful shutdown
            _ = self.shutdown_signal.notified() => {
                break;
            }
        }
    }

    Ok(())
}
```

**Key Decisions**:
- Use `select_next_some()` instead of `now_or_never()` ✅
- Separate handler methods for each event type ✅
- Periodic maintenance for cache cleanup, peer pruning ✅
- Graceful shutdown via shutdown_signal ✅

---

### 2. Query Response Channel Pattern

**Problem**: DHT queries are async - response comes later via event

**Solution**: Pending query map + mpsc channels

```rust
type QueryId = kad::QueryId;
type ResponseChannel = tokio::sync::oneshot::Sender<Result<Package>>;

pub struct P2PRegistry {
    // ... existing fields ...

    /// Pending DHT queries awaiting responses
    pending_queries: Arc<RwLock<HashMap<QueryId, ResponseChannel>>>,
}
```

**Flow**:
1. `query_dht_parallel()` creates oneshot channel
2. Stores (QueryId → Sender) in pending_queries map
3. Initiates Kademlia get_record() query
4. Returns receiver.await with timeout
5. Event loop receives KademliaEvent::GetRecordResult
6. Looks up sender in pending_queries
7. Sends result through channel
8. query_dht_parallel() receives result and returns

```rust
async fn query_dht_parallel(&self, package_id: &PackageId, fan_out: usize)
    -> Result<Option<Package>>
{
    let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
    let (tx, rx) = tokio::sync::oneshot::channel();

    // Initiate query and get QueryId
    let query_id = {
        let mut swarm = self.swarm.write().await;
        swarm.behaviour_mut().kademlia.get_record(key)
    };

    // Register callback
    self.pending_queries.write().await.insert(query_id, tx);

    // Wait for response with timeout
    match tokio::time::timeout(Duration::from_secs(30), rx).await {
        Ok(Ok(result)) => Ok(Some(result)),
        Ok(Err(_)) => Ok(None), // Channel closed
        Err(_) => {
            // Timeout - cleanup
            self.pending_queries.write().await.remove(&query_id);
            Ok(None)
        }
    }
}
```

---

### 3. Event Handler Architecture

**Pattern**: Dispatcher → Specific Handlers

```rust
#[instrument(skip(self, event))]
async fn handle_swarm_event(&self, event: SwarmEvent<P2PBehaviourEvent>)
    -> Result<()>
{
    match event {
        SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(event)) => {
            self.handle_kademlia_event(event).await
        }
        SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(event)) => {
            self.handle_gossipsub_event(event).await
        }
        SwarmEvent::Behaviour(P2PBehaviourEvent::Identify(event)) => {
            self.handle_identify_event(event).await
        }
        SwarmEvent::ConnectionEstablished { peer_id, .. } => {
            self.handle_connection_established(peer_id).await
        }
        SwarmEvent::ConnectionClosed { peer_id, cause, .. } => {
            self.handle_connection_closed(peer_id, cause).await
        }
        _ => Ok(())
    }
}
```

#### Kademlia Event Handler

```rust
#[instrument(skip(self, event), fields(event_type = ?event))]
async fn handle_kademlia_event(&self, event: kad::Event) -> Result<()> {
    match event {
        kad::Event::OutboundQueryProgressed {
            id,
            result: kad::QueryResult::GetRecord(Ok(record)),
            ..
        } => {
            // DHT query succeeded
            self.handle_get_record_success(id, record).await
        }

        kad::Event::OutboundQueryProgressed {
            id,
            result: kad::QueryResult::GetRecord(Err(error)),
            ..
        } => {
            // DHT query failed
            self.handle_get_record_failure(id, error).await
        }

        kad::Event::RoutingUpdated { peer, .. } => {
            // DHT routing table updated
            info!("Routing table updated with peer {}", peer);
            Ok(())
        }

        _ => Ok(())
    }
}

async fn handle_get_record_success(
    &self,
    query_id: kad::QueryId,
    record: kad::GetRecordOk
) -> Result<()> {
    // Deserialize package
    let package: Package = serde_json::from_slice(&record.record.value)
        .map_err(|e| MarketplaceError::serialization_error(e))?;

    // Find pending query and send response
    if let Some(tx) = self.pending_queries.write().await.remove(&query_id) {
        let _ = tx.send(Ok(package));
    }

    Ok(())
}
```

#### Gossipsub Event Handler

```rust
#[instrument(skip(self, event))]
async fn handle_gossipsub_event(&self, event: gossipsub::Event)
    -> Result<()>
{
    match event {
        gossipsub::Event::Message {
            propagation_source,
            message,
            ..
        } => {
            // Parse package announcement
            let package: Package = serde_json::from_slice(&message.data)
                .map_err(|e| MarketplaceError::serialization_error(e))?;

            info!("Received package announcement: {} from {}",
                package.id, propagation_source);

            // Store in discovered_packages
            self.discovered_packages
                .write()
                .await
                .entry(package.id.clone())
                .or_insert_with(HashSet::new)
                .insert(propagation_source);

            // Update peer reputation (successful announcement)
            self.record_peer_success(propagation_source, None).await;

            Ok(())
        }

        gossipsub::Event::Subscribed { peer_id, topic } => {
            info!("Peer {} subscribed to topic {}", peer_id, topic);
            Ok(())
        }

        _ => Ok(())
    }
}
```

---

### 4. Parallel Fan-Out Strategy

**Goal**: Query multiple peers concurrently for faster results

**Implementation**:

```rust
async fn query_dht_parallel(&self, package_id: &PackageId, fan_out: usize)
    -> Result<Option<Package>>
{
    if fan_out <= 1 {
        return self.query_dht_single(package_id).await;
    }

    // Get best peers from routing table
    let best_peers = self.select_best_peers(0.5, fan_out, None).await;

    if best_peers.is_empty() {
        return self.query_dht_single(package_id).await;
    }

    // Create channels for each query
    let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
    let (tx, mut rx) = tokio::sync::mpsc::channel(fan_out);

    // Launch parallel queries
    for (peer_id, _score) in best_peers {
        let registry = self.clone(); // Clone Arc
        let key_clone = key.clone();
        let tx_clone = tx.clone();
        let package_id_clone = package_id.clone();

        tokio::spawn(async move {
            match registry.query_peer_directly(&peer_id, &key_clone).await {
                Ok(Some(package)) => {
                    let _ = tx_clone.send(Ok(package)).await;
                }
                Ok(None) => {} // No result from this peer
                Err(e) => {
                    let _ = tx_clone.send(Err(e)).await;
                }
            }
        });
    }

    drop(tx); // Close sender so rx knows when all queries are done

    // Collect first successful result with timeout
    match tokio::time::timeout(Duration::from_secs(10), rx.recv()).await {
        Ok(Some(Ok(package))) => Ok(Some(package)),
        Ok(Some(Err(_))) | Ok(None) | Err(_) => Ok(None)
    }
}
```

---

### 5. Graceful Shutdown Pattern

```rust
impl P2PRegistry {
    pub async fn shutdown(&self) -> Result<()> {
        // Signal event loop to stop
        self.shutdown_signal.notify_waiters();

        // Close all pending queries
        let pending = self.pending_queries.write().await;
        for (_id, tx) in pending.drain() {
            let _ = tx.send(Err(MarketplaceError::network_error("Shutdown")));
        }

        // Persist state
        self.persist_state().await?;

        Ok(())
    }
}
```

---

## Performance Targets

| Metric | Target | Notes |
|--------|--------|-------|
| DHT query latency (single) | <500ms | Single peer query |
| DHT query latency (fan-out=3) | <200ms | Parallel queries |
| Gossipsub message propagation | <100ms | Network-dependent |
| Event processing throughput | >1000 events/sec | Event loop capacity |
| Memory overhead | <50MB | Per 10K packages |

---

## Error Handling Strategy

1. **Network Errors**: Retry with exponential backoff
2. **Timeout Errors**: Return None, don't block forever
3. **Serialization Errors**: Log + skip malformed messages
4. **Lock Poisoning**: Panic (unrecoverable)
5. **Query Failures**: Try alternate peers

---

## OTEL Integration Points

```rust
#[instrument(
    skip(self, event),
    fields(
        event_type = ?event,
        peer_count = tracing::field::Empty,
        query_latency_ms = tracing::field::Empty
    )
)]
async fn handle_kademlia_event(&self, event: kad::Event) -> Result<()> {
    let start = Instant::now();
    let span = Span::current();

    // ... processing ...

    span.record("query_latency_ms", start.elapsed().as_millis() as u64);
    span.record("peer_count", self.peer_reputation.read().await.len());
}
```

---

## Testing Strategy

### Unit Tests
- Event handler logic
- Query result parsing
- Reputation calculations

### Integration Tests
- Multi-node P2P network
- DHT query/response flow
- Gossipsub message propagation
- Peer discovery

### Performance Tests
- Query latency benchmarks
- Fan-out effectiveness
- Cache hit rates

---

## Implementation Phases

### Phase 1: Event Loop (30 min)
- Replace now_or_never() with select_next_some()
- Add event dispatcher
- Add basic handlers

### Phase 2: Query Response (1 hour)
- Add pending_queries map
- Implement channel-based result collection
- Add timeout handling

### Phase 3: Event Handlers (1 hour)
- Implement Kademlia handler
- Implement Gossipsub handler
- Implement Identify handler

### Phase 4: OTEL (30 min)
- Add spans to all handlers
- Add metrics counters
- Add error tracking

---

**Architecture Design Complete**: Ready for implementation
