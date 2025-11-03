# libp2p Async DHT Query Result Collection - Research Report

**Research Date:** 2025-11-02
**Target:** rust-libp2p v0.54 (ggen-marketplace)
**Focus:** Production-ready patterns for async DHT queries with QueryId tracking

---

## Executive Summary

### Current Problem
The ggen-marketplace P2P backend starts DHT queries but never collects results:
- `query_dht_parallel()` calls `kademlia.get_record()` which returns `QueryId`
- No mechanism exists to wait for `OutboundQueryProgressed` events
- All DHT queries return `Ok(None)` immediately, breaking package discovery

### Required Solution
Implement a **message-passing event loop architecture** with:
1. Dedicated tokio task owning the Swarm
2. `HashMap<QueryId, oneshot::Sender>` for query result routing
3. `tokio::select!` for concurrent event and command processing
4. Proper timeout handling with `tokio::time::timeout`

---

## Industry Best Practices

### 1. Event Loop Architecture (rust-libp2p Pattern)

**Pattern Source:** rust-libp2p examples (file-sharing, ipfs-kad), Substrate sc-network

**Key Principle:**
> "Do as little as possible in the libp2p actor - only handle interaction with libp2p, maintain response channels (typically one-shots), call methods on behaviours when receiving commands, and match events from the Swarm to those commands."

**Architecture:**

```rust
use tokio::sync::{mpsc, oneshot};
use std::collections::HashMap;
use libp2p::kad::QueryId;

/// Commands sent to the P2P swarm task
pub enum SwarmCommand {
    QueryDHT {
        package_id: PackageId,
        response: oneshot::Sender<Option<Package>>,
    },
    PublishPackage {
        package: Package,
        response: oneshot::Sender<Result<()>>,
    },
    Bootstrap {
        response: oneshot::Sender<Result<()>>,
    },
    Shutdown,
}

/// The event loop task structure
pub struct SwarmEventLoop {
    swarm: Swarm<P2PBehaviour>,
    command_rx: mpsc::Receiver<SwarmCommand>,

    // Query tracking - maps QueryId to result channels
    pending_dht_queries: HashMap<QueryId, oneshot::Sender<Option<Package>>>,
    pending_bootstrap: HashMap<QueryId, oneshot::Sender<Result<()>>>,
}

impl SwarmEventLoop {
    pub async fn run(mut self) {
        loop {
            tokio::select! {
                // Process swarm events (network activity)
                event = self.swarm.select_next_some() => {
                    if let Err(e) = self.handle_event(event).await {
                        tracing::error!("Event handling failed: {}", e);
                    }
                }

                // Process commands from application
                Some(command) = self.command_rx.recv() => {
                    if let SwarmCommand::Shutdown = command {
                        break;
                    }
                    if let Err(e) = self.handle_command(command).await {
                        tracing::error!("Command handling failed: {}", e);
                    }
                }

                // Graceful shutdown on dropped sender
                else => break,
            }
        }
    }
}
```

**Why This Works:**
- ✅ Swarm stays in single-threaded context (no Send/Sync issues)
- ✅ Commands can be sent from any thread via mpsc
- ✅ QueryId naturally maps to oneshot for result delivery
- ✅ `tokio::select!` ensures non-blocking concurrent processing

---

### 2. QueryId Tracking Pattern

**Pattern Source:** rust-libp2p file-sharing example

**Implementation:**

```rust
impl SwarmEventLoop {
    async fn handle_command(&mut self, command: SwarmCommand) -> Result<()> {
        match command {
            SwarmCommand::QueryDHT { package_id, response } => {
                let key = kad::RecordKey::new(&package_id.to_string().as_bytes());

                // Start the DHT query - returns QueryId
                let query_id = self.swarm
                    .behaviour_mut()
                    .kademlia
                    .get_record(key);

                // Store the response channel, indexed by QueryId
                self.pending_dht_queries.insert(query_id, response);

                tracing::debug!("Started DHT query {}", query_id);
                Ok(())
            }

            SwarmCommand::Bootstrap { response } => {
                let query_id = self.swarm
                    .behaviour_mut()
                    .kademlia
                    .bootstrap()?;

                self.pending_bootstrap.insert(query_id, response);
                Ok(())
            }

            // ... other commands
        }
    }
}
```

**Key Points:**
- `get_record()` returns `QueryId` immediately (non-blocking)
- Store `oneshot::Sender` in HashMap with QueryId as key
- Query executes in background as swarm processes network events

---

### 3. Event Processing with Result Routing

**Pattern Source:** rust-libp2p ipfs-kad example, Substrate

**Implementation:**

```rust
impl SwarmEventLoop {
    async fn handle_event(&mut self, event: SwarmEvent<P2PBehaviourEvent>) -> Result<()> {
        match event {
            SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => {
                self.handle_kademlia_event(kad_event).await
            }

            SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(gossip_event)) => {
                self.handle_gossipsub_event(gossip_event).await
            }

            SwarmEvent::ConnectionEstablished { peer_id, .. } => {
                tracing::info!("Connected to peer: {}", peer_id);
                Ok(())
            }

            SwarmEvent::ConnectionClosed { peer_id, cause, .. } => {
                tracing::warn!("Disconnected from {}: {:?}", peer_id, cause);
                Ok(())
            }

            _ => Ok(()),
        }
    }

    async fn handle_kademlia_event(&mut self, event: kad::Event) -> Result<()> {
        use kad::QueryResult;

        match event {
            // The critical event for DHT query results
            kad::Event::OutboundQueryProgressed { id, result, .. } => {
                match result {
                    // Successful record retrieval
                    QueryResult::GetRecord(Ok(kad::GetRecordOk::FoundRecord(record))) => {
                        if let Some(sender) = self.pending_dht_queries.remove(&id) {
                            let package: Option<Package> = serde_json::from_slice(&record.record.value)
                                .ok();

                            // Send result back through oneshot
                            let _ = sender.send(package);
                            tracing::debug!("DHT query {} completed successfully", id);
                        }
                    }

                    // Query timed out - send None
                    QueryResult::GetRecord(Err(kad::GetRecordError::Timeout)) => {
                        if let Some(sender) = self.pending_dht_queries.remove(&id) {
                            let _ = sender.send(None);
                            tracing::warn!("DHT query {} timed out", id);
                        }
                    }

                    // Record not found - send None
                    QueryResult::GetRecord(Err(_)) => {
                        if let Some(sender) = self.pending_dht_queries.remove(&id) {
                            let _ = sender.send(None);
                        }
                    }

                    // Bootstrap completed
                    QueryResult::Bootstrap(Ok(_)) => {
                        if let Some(sender) = self.pending_bootstrap.remove(&id) {
                            let _ = sender.send(Ok(()));
                        }
                    }

                    QueryResult::Bootstrap(Err(e)) => {
                        if let Some(sender) = self.pending_bootstrap.remove(&id) {
                            let _ = sender.send(Err(MarketplaceError::network_error(e)));
                        }
                    }

                    _ => {}
                }
                Ok(())
            }

            _ => Ok(()),
        }
    }
}
```

**Critical Pattern:**
```rust
// 1. Extract QueryId from event
kad::Event::OutboundQueryProgressed { id, result, .. }

// 2. Remove pending channel from HashMap (query complete)
if let Some(sender) = self.pending_dht_queries.remove(&id) {
    // 3. Send result through oneshot
    let _ = sender.send(result);
}
```

---

### 4. Application-Side Usage Pattern

**Implementation:**

```rust
pub struct P2PRegistry {
    command_tx: mpsc::Sender<SwarmCommand>,
    peer_id: PeerId,
    // ... other fields NOT including Swarm
}

#[async_trait]
impl Registry for P2PRegistry {
    async fn get_package(&self, package_id: &PackageId) -> Result<Option<Package>> {
        // Create oneshot channel for result
        let (response_tx, response_rx) = oneshot::channel();

        // Send command to swarm task
        self.command_tx
            .send(SwarmCommand::QueryDHT {
                package_id: package_id.clone(),
                response: response_tx,
            })
            .await
            .map_err(|_| MarketplaceError::network_error("Swarm task died"))?;

        // Wait for result with timeout
        let result = tokio::time::timeout(
            Duration::from_secs(30),
            response_rx
        ).await;

        match result {
            Ok(Ok(package)) => Ok(package),
            Ok(Err(_)) => Err(MarketplaceError::network_error("Query cancelled")),
            Err(_) => Err(MarketplaceError::network_error("Query timeout")),
        }
    }
}
```

**Key Benefits:**
- ✅ No `Arc<RwLock<Swarm>>` - solves Send/Sync issues
- ✅ Clean async/await interface for users
- ✅ Built-in timeout handling
- ✅ Swarm stays in dedicated task

---

### 5. Timeout Handling (Multiple Strategies)

**Strategy 1: Application-Level Timeout (Recommended)**
```rust
// In application code
let result = tokio::time::timeout(
    Duration::from_secs(30),
    response_rx
).await?;
```

**Strategy 2: Event Loop Timeout Tracking**
```rust
struct PendingQuery {
    sender: oneshot::Sender<Option<Package>>,
    started_at: Instant,
}

// In event loop
async fn cleanup_stale_queries(&mut self) {
    let now = Instant::now();
    let timeout = Duration::from_secs(60);

    self.pending_dht_queries.retain(|query_id, pending| {
        if now.duration_since(pending.started_at) > timeout {
            let _ = pending.sender.send(None);
            tracing::warn!("Cleaned up stale query: {}", query_id);
            false
        } else {
            true
        }
    });
}

// Run cleanup periodically in tokio::select!
tokio::select! {
    // ... existing branches
    _ = tokio::time::sleep(Duration::from_secs(10)) => {
        self.cleanup_stale_queries().await;
    }
}
```

---

## Production Examples Analysis

### Substrate sc-network Pattern

**Source:** Polkadot/Substrate networking layer

**Key Insights:**
- Uses dedicated `NetworkWorker` task owning swarm
- Commands sent via bounded mpsc channels
- Query results routed through oneshot channels
- Extensive use of `tokio::select!` with multiple branches
- Custom `NotificationService` for protocol-specific messages

**Architecture Similarity:**
```
Application Code → mpsc::Sender<Command> →
    EventLoop (owns Swarm) →
    oneshot::Receiver<Result> → Application Code
```

### rust-libp2p File Sharing Example

**Source:** `examples/file-sharing/src/network.rs`

**Direct Quotes from Code:**
- "Completed query to be previously pending"
- "The oneshot sender transmits results back to the awaiting client"
- "This decoupling allows the event loop to remain responsive"

**HashMap Usage:**
```rust
pending_start_providing: HashMap<kad::QueryId, oneshot::Sender<()>>
pending_get_providers: HashMap<kad::QueryId, oneshot::Sender<HashSet<PeerId>>>
```

---

## Common Pitfalls & Anti-Patterns

### ❌ Anti-Pattern 1: `Arc<RwLock<Swarm>>`
```rust
// DON'T DO THIS
pub struct P2PRegistry {
    swarm: Arc<RwLock<Swarm<P2PBehaviour>>>,  // Swarm is NOT Send
}

#[async_trait]
impl Registry for P2PRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        let mut swarm = self.swarm.write().await;  // Fails Send bound
        // ...
    }
}
```

**Problems:**
- `Swarm` is not `Send` or `Sync`
- Cannot use in async trait methods
- Locks held across await points cause deadlocks

**Fix:** Use message-passing architecture (see above)

---

### ❌ Anti-Pattern 2: `.now_or_never()` Event Processing
```rust
// DON'T DO THIS
async fn process_events(&self) {
    let mut swarm = self.swarm.write().await;
    if let Some(event) = swarm.next().now_or_never() {  // Never blocks!
        // Process event
    }
}
```

**Problems:**
- `.now_or_never()` returns immediately if no event ready
- Network events never get processed
- DHT queries started but results ignored

**Fix:** Use dedicated task with `.await` or `tokio::select!`

---

### ❌ Anti-Pattern 3: Ignoring QueryId
```rust
// DON'T DO THIS
async fn query_dht(&self, key: &str) -> Option<Package> {
    let mut swarm = self.swarm.write().await;
    swarm.behaviour_mut().kademlia.get_record(key);  // Returns QueryId

    // Oops! No way to get result
    Ok(None)
}
```

**Problems:**
- Query starts but no mechanism to receive result
- QueryId discarded immediately
- Always returns None

**Fix:** Store QueryId → oneshot mapping, process events

---

### ❌ Anti-Pattern 4: Synchronous Query Assumptions
```rust
// DON'T DO THIS
async fn query_dht(&self, key: &str) -> Option<Package> {
    self.start_query(key).await;

    // Can't wait for result here - wrong architecture!
    thread::sleep(Duration::from_secs(5));  // Terrible!

    self.check_result(key).await
}
```

**Problems:**
- DHT queries are inherently async
- Blocking waits waste resources
- No guarantee of result within timeout

**Fix:** Use event-driven architecture with channels

---

## Recommended Implementation Plan

### Phase 1: Create Command Enum and Event Loop Structure
```rust
// Step 1: Define commands (2-3 hours)
pub enum SwarmCommand {
    QueryDHT { ... },
    PublishPackage { ... },
    Bootstrap { ... },
    Subscribe { ... },
    Shutdown,
}

// Step 2: Create event loop struct (1-2 hours)
pub struct SwarmEventLoop {
    swarm: Swarm<P2PBehaviour>,
    command_rx: mpsc::Receiver<SwarmCommand>,
    pending_dht_queries: HashMap<QueryId, oneshot::Sender<Option<Package>>>,
    // ... other pending trackers
}

// Step 3: Implement basic event loop (2-3 hours)
impl SwarmEventLoop {
    pub async fn run(mut self) {
        loop {
            tokio::select! {
                event = self.swarm.select_next_some() => { ... }
                Some(command) = self.command_rx.recv() => { ... }
            }
        }
    }
}
```

### Phase 2: Refactor P2PRegistry
```rust
// Step 4: Remove Arc<RwLock<Swarm>> (1 hour)
pub struct P2PRegistry {
    command_tx: mpsc::Sender<SwarmCommand>,  // Replace swarm
    peer_id: PeerId,
    // ... keep other fields
}

// Step 5: Spawn event loop in constructor (1 hour)
impl P2PRegistry {
    pub async fn new(config: P2PConfig) -> Result<Self> {
        let (command_tx, command_rx) = mpsc::channel(100);

        let event_loop = SwarmEventLoop::new(config, command_rx).await?;
        tokio::spawn(async move {
            event_loop.run().await;
        });

        Ok(Self { command_tx, ... })
    }
}
```

### Phase 3: Implement Query Handlers
```rust
// Step 6: Command handling (3-4 hours)
async fn handle_command(&mut self, command: SwarmCommand) -> Result<()> {
    match command {
        SwarmCommand::QueryDHT { package_id, response } => {
            let query_id = self.swarm.behaviour_mut().kademlia.get_record(...);
            self.pending_dht_queries.insert(query_id, response);
        }
        // ... other commands
    }
}

// Step 7: Event handling (3-4 hours)
async fn handle_event(&mut self, event: SwarmEvent) -> Result<()> {
    match event {
        SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => {
            match kad_event {
                kad::Event::OutboundQueryProgressed { id, result, .. } => {
                    // Route result to pending channel
                }
            }
        }
    }
}
```

### Phase 4: Update Registry Methods
```rust
// Step 8: Refactor each method (2-3 hours)
#[async_trait]
impl Registry for P2PRegistry {
    async fn get_package(&self, id: &PackageId) -> Result<Option<Package>> {
        let (tx, rx) = oneshot::channel();

        self.command_tx.send(SwarmCommand::QueryDHT {
            package_id: id.clone(),
            response: tx,
        }).await?;

        tokio::time::timeout(Duration::from_secs(30), rx).await??
    }
}
```

### Phase 5: Testing & Validation
```rust
// Step 9: Unit tests (4-5 hours)
#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn test_dht_query_success() {
        // Test successful query with mock swarm
    }

    #[tokio::test]
    async fn test_dht_query_timeout() {
        // Test timeout handling
    }
}

// Step 10: Integration tests (4-5 hours)
#[tokio::test]
async fn test_real_swarm_query() {
    // Test with actual libp2p swarm
}
```

**Total Estimated Time:** 23-30 hours (3-4 days)

---

## Code References

### External Sources
1. **rust-libp2p examples:** https://github.com/libp2p/rust-libp2p/tree/master/examples
   - `file-sharing/src/network.rs` - Complete message-passing pattern
   - `ipfs-kad/src/main.rs` - Kademlia event handling

2. **Substrate sc-network:** https://github.com/paritytech/substrate/tree/master/client/network
   - Production-grade event loop architecture
   - Multi-protocol handling

3. **libp2p documentation:** https://docs.rs/libp2p-kad/latest
   - QueryId API
   - Event types reference

### Internal References
- `/Users/sac/ggen/docs/P2P_IMPLEMENTATION_GAPS.json` - Gap analysis
- `/Users/sac/ggen/docs/validation/FIX_GUIDE.md` - Fix recommendations
- `/Users/sac/ggen/ggen-marketplace/src/backend/p2p.rs` - Current implementation

---

## Dependencies Required

```toml
[dependencies]
# Already present in ggen-marketplace/Cargo.toml
libp2p = { version = "0.54", features = ["tcp", "noise", "yamux", "gossipsub", "kad", "identify", "tokio", "macros"] }
tokio = { version = "1.35", features = ["full"] }
futures = "0.3"
tracing = "0.1"
serde_json = "1.0"
```

No additional dependencies needed! ✅

---

## Success Criteria

### Functional Requirements
- ✅ DHT queries return actual results, not always None
- ✅ Gossipsub messages processed into discovered_packages
- ✅ Bootstrap connects to bootstrap nodes
- ✅ Peer discovery populates routing table

### Non-Functional Requirements
- ✅ Zero compilation errors
- ✅ Zero clippy warnings
- ✅ 90%+ test coverage for new code
- ✅ No deadlocks under concurrent load
- ✅ Query latency < 200ms (p95)

### Validation Commands
```bash
# Compilation
cargo build -p ggen-marketplace --features p2p

# Tests
cargo test -p ggen-marketplace --features p2p -- --nocapture

# Clippy
cargo clippy -p ggen-marketplace --features p2p -- -D warnings

# Benchmarks
cargo bench --bench p2p_dht_benchmark
```

---

## Conclusion

The solution is **well-established** in the rust-libp2p ecosystem:

1. **Message-Passing Architecture** - Industry standard pattern
2. **QueryId → oneshot Mapping** - Used in Substrate, file-sharing example
3. **tokio::select! Event Loop** - Best practice for async coordination
4. **Dedicated Swarm Task** - Solves Send/Sync constraints

**Recommendation:** Follow the file-sharing example pattern exactly - it's battle-tested and matches our use case perfectly.

**Next Steps:**
1. Implement SwarmEventLoop with command enum
2. Refactor P2PRegistry to use message passing
3. Test with real libp2p swarms
4. Validate against benchmarks

**Estimated Delivery:** 3-4 days of focused implementation work.
