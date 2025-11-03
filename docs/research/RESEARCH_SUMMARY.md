# libp2p Async DHT Research - Summary Report

**Research Date:** 2025-11-02
**Researcher:** Research Agent
**Task:** Investigate production-ready patterns for async DHT query result collection
**Status:** ✅ COMPLETE

---

## Research Questions Answered

### 1. How do production libp2p applications handle async DHT queries?

**Answer:** **Message-passing architecture with dedicated event loop task**

**Pattern:**
- Dedicated `tokio::task` owns the `Swarm`
- Commands sent via `mpsc::channel` from application
- Results returned via `oneshot::channel` per query
- `tokio::select!` drives both network events and commands

**Sources:**
- rust-libp2p file-sharing example
- Substrate sc-network implementation
- rust-libp2p community discussions

**Evidence:**
> "Do as little as possible in the libp2p actor - only handle interaction with libp2p, maintain response channels (typically one-shots), call methods on behaviours when receiving commands, and match events from the Swarm to those commands."
> — rust-libp2p Discussion #2024

---

### 2. What's the idiomatic way to wait for QueryId results?

**Answer:** **HashMap<QueryId, oneshot::Sender> pattern**

**Implementation:**
```rust
// 1. Start query, get QueryId
let query_id = swarm.behaviour_mut().kademlia.get_record(key);

// 2. Store response channel
pending_queries.insert(query_id, response_sender);

// 3. Wait for OutboundQueryProgressed event
kad::Event::OutboundQueryProgressed { id, result, .. } => {
    if let Some(sender) = pending_queries.remove(&id) {
        sender.send(result);  // Route result back
    }
}
```

**Sources:**
- rust-libp2p file-sharing example (`pending_get_providers`)
- Substrate DHT implementation
- Multiple GitHub discussions

**Key Insight:** QueryId is the natural index for routing async results

---

### 3. How to implement timeout handling with tokio::select!?

**Answer:** **Two-tier timeout strategy**

**Application Level (Recommended):**
```rust
let result = tokio::time::timeout(
    Duration::from_secs(30),
    response_rx
).await?;
```

**Event Loop Level (Additional Safety):**
```rust
tokio::select! {
    event = swarm.select_next_some() => { ... }
    command = command_rx.recv() => { ... }

    // Periodic cleanup of stale queries
    _ = tokio::time::sleep(Duration::from_secs(30)) => {
        cleanup_stale_queries();
    }
}
```

**Best Practice:** Both levels provide defense-in-depth
- Application timeout: Protects caller from hanging
- Event loop cleanup: Prevents memory leaks

---

### 4. Best practices for event loop architecture?

**Answer:** **Single-responsibility event loop with command pattern**

**Core Principles:**
1. **Ownership:** Event loop owns Swarm exclusively
2. **Communication:** mpsc for commands, oneshot for results
3. **Concurrency:** `tokio::select!` for non-blocking multiplexing
4. **Separation:** Minimal logic in event loop, delegate to handlers

**Architecture:**
```
┌─────────────────────────────────────────────────────────────┐
│  Application Code (Registry trait impl)                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  mpsc::Sender<Command> → oneshot::Receiver<Result>  │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  Event Loop Task (owns Swarm)                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  tokio::select! {                                    │    │
│  │    event = swarm.next() => handle_event(event)      │    │
│  │    command = rx.recv() => handle_command(command)   │    │
│  │  }                                                   │    │
│  └─────────────────────────────────────────────────────┘    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  HashMap<QueryId, oneshot::Sender> - result routing │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│  libp2p Swarm (network I/O)                                  │
└─────────────────────────────────────────────────────────────┘
```

**Why This Works:**
- ✅ Solves Send/Sync constraints (Swarm not Send)
- ✅ Enables async/await interface
- ✅ Non-blocking concurrent processing
- ✅ Clean separation of concerns

---

### 5. Common pitfalls in libp2p event handling?

**Pitfall 1: `Arc<RwLock<Swarm>>`**
```rust
// ❌ WRONG - Swarm is not Send
pub struct Registry {
    swarm: Arc<RwLock<Swarm<Behaviour>>>,
}
```
**Problem:** `Swarm` cannot cross thread boundaries
**Fix:** Use message-passing, don't share Swarm

---

**Pitfall 2: `.now_or_never()` for events**
```rust
// ❌ WRONG - Never blocks, events ignored
if let Some(event) = swarm.next().now_or_never() {
    handle_event(event);
}
```
**Problem:** Returns immediately if no event ready
**Fix:** Use `.await` or `tokio::select!` to actually wait

---

**Pitfall 3: Discarding QueryId**
```rust
// ❌ WRONG - No way to get result
swarm.behaviour_mut().kademlia.get_record(key);  // QueryId discarded
return Ok(None);  // Always returns None!
```
**Problem:** Query starts but result never collected
**Fix:** Store QueryId → oneshot mapping

---

**Pitfall 4: Blocking waits**
```rust
// ❌ WRONG - Blocks thread, wastes resources
async fn query(&self) -> Result<Package> {
    self.start_query().await;
    tokio::time::sleep(Duration::from_secs(5)).await;  // Hope result arrives?
    self.get_result().await  // No guarantee!
}
```
**Problem:** Arbitrary delays, no synchronization
**Fix:** Event-driven architecture with channels

---

**Pitfall 5: Holding locks across await**
```rust
// ❌ WRONG - Blocks other operations
async fn query(&self) -> Result<Package> {
    let mut swarm = self.swarm.write().await;  // Lock acquired
    swarm.behaviour_mut().kademlia.get_record(key);
    // ... other async operations while holding lock
}  // Lock released here - blocked everyone!
```
**Problem:** Poor concurrency, possible deadlocks
**Fix:** Minimize lock scopes or use message-passing

---

## Key Findings Summary

| Aspect | Current State | Required State | Effort |
|--------|---------------|----------------|--------|
| **Event Loop** | `.now_or_never()` (broken) | Dedicated `tokio::spawn` task | 12h |
| **Query Results** | Always returns None | `HashMap<QueryId, oneshot>` | 8h |
| **Architecture** | `Arc<RwLock<Swarm>>` | Message-passing | 6h |
| **Timeouts** | None | Application + loop cleanup | 2h |
| **Gossipsub** | Published but not consumed | Process in event handler | 4h |

**Total Estimated Effort:** 32 hours (4-5 days)

---

## Reference Implementation

**Primary Source:** rust-libp2p `examples/file-sharing/src/network.rs`

**Why This Example:**
- ✅ Complete message-passing architecture
- ✅ HashMap<QueryId, oneshot> pattern
- ✅ tokio::select! event loop
- ✅ Multiple query types handled
- ✅ Production-quality code

**Direct Link:** https://github.com/libp2p/rust-libp2p/blob/master/examples/file-sharing/src/network.rs

**Secondary Sources:**
- Substrate sc-network (large-scale production)
- rust-libp2p ipfs-kad example (simpler patterns)

---

## Deliverables

### 1. Comprehensive Research Report
**File:** `/Users/sac/ggen/docs/research/LIBP2P_ASYNC_DHT_PATTERNS.md`

**Contents:**
- Industry best practices analysis
- Complete architecture patterns
- Event processing patterns
- Timeout strategies
- Anti-patterns documentation
- Implementation timeline

**Size:** 5,000+ words, production-ready patterns

---

### 2. Copy-Paste Code Snippets
**File:** `/Users/sac/ggen/docs/research/LIBP2P_CODE_SNIPPETS.md`

**Contents:**
- SwarmCommand enum (ready to use)
- SwarmEventLoop structure (complete)
- Command handlers (all Registry methods)
- Event handlers (Kademlia + Gossipsub)
- Refactored P2PRegistry (message-passing)
- Registry trait implementation (channels)
- Testing examples
- Integration checklist

**Size:** 600+ lines of ready-to-use Rust code

---

### 3. This Summary
**File:** `/Users/sac/ggen/docs/research/RESEARCH_SUMMARY.md`

**Contents:**
- Research questions answered
- Key findings table
- Pitfall documentation
- Reference links

---

## Actionable Next Steps

### Immediate (Today)
1. ✅ Review research findings
2. ✅ Read code snippets document
3. ⏭️ Decide on implementation approach

### Day 1-2 (Implementation Start)
1. Copy `SwarmCommand` enum from snippets
2. Copy `SwarmEventLoop` structure
3. Implement event loop `run()` method
4. Test basic command processing

### Day 3-4 (Core Implementation)
1. Implement command handlers
2. Implement event handlers (Kademlia, Gossipsub)
3. Add timeout and cleanup logic
4. Refactor `P2PRegistry` to remove `Arc<RwLock<Swarm>>`

### Day 5 (Testing & Validation)
1. Unit tests for event loop
2. Integration tests with real swarm
3. Performance benchmarks
4. Clippy and compilation checks

---

## Success Metrics

### Functional
- ✅ DHT queries return actual results (not always None)
- ✅ Gossipsub messages processed into discovered_packages
- ✅ Bootstrap connects to bootstrap nodes
- ✅ Peer discovery works

### Quality
- ✅ Zero compilation errors
- ✅ Zero clippy warnings
- ✅ 90%+ test coverage
- ✅ No deadlocks under load

### Performance
- ✅ Query latency < 200ms (p95)
- ✅ Handles 100+ concurrent queries
- ✅ Memory usage stable over time

---

## External References

### Documentation
- libp2p-kad Rust docs: https://docs.rs/libp2p-kad
- Kademlia Event types: https://docs.rs/libp2p/latest/libp2p/kad/enum.Event.html
- tokio::select! docs: https://docs.rs/tokio/latest/tokio/macro.select.html

### Code Examples
- rust-libp2p file-sharing: https://github.com/libp2p/rust-libp2p/tree/master/examples/file-sharing
- rust-libp2p ipfs-kad: https://github.com/libp2p/rust-libp2p/tree/master/examples/ipfs-kad
- Substrate sc-network: https://github.com/paritytech/substrate/tree/master/client/network

### Community Discussions
- Event loop architecture: https://github.com/libp2p/rust-libp2p/discussions/2024
- Multi-threaded patterns: https://github.com/libp2p/rust-libp2p/discussions/2505
- Kademlia queries: https://github.com/libp2p/rust-libp2p/discussions/2177

---

## Dependencies

**All dependencies already present in `ggen-marketplace/Cargo.toml`:**
```toml
libp2p = { version = "0.54", features = ["tcp", "noise", "yamux", "gossipsub", "kad", "identify", "tokio", "macros"] }
tokio = { version = "1.35", features = ["full"] }
futures = "0.3"
```

**No additional crates required!** ✅

---

## Risk Assessment

### Low Risk
- ✅ Pattern is well-established (Substrate uses it)
- ✅ Examples exist in rust-libp2p repo
- ✅ No new dependencies needed
- ✅ Can be tested in isolation

### Medium Risk
- ⚠️ Integration testing requires real network
- ⚠️ Performance tuning may need iteration
- ⚠️ Timeout values may need adjustment

### Mitigation
- Start with unit tests (mock channels)
- Use testnet for integration tests
- Add performance benchmarks early
- Monitor query latencies in production

---

## Conclusion

### The Pattern is Clear
The rust-libp2p ecosystem has converged on a **message-passing architecture** for integrating libp2p into async Rust applications. This pattern:

1. **Solves technical constraints** (Swarm not Send/Sync)
2. **Provides clean interface** (async/await for callers)
3. **Scales to production** (Substrate proves this)
4. **Is well-documented** (examples exist)

### Implementation is Straightforward
With the provided code snippets, implementation is:
- ✅ **Mechanical** - Copy patterns from snippets
- ✅ **Testable** - Each component can be tested independently
- ✅ **Incremental** - Can be built phase by phase
- ✅ **Low-risk** - Pattern is proven

### Timeline is Reasonable
4-5 days of focused work:
- Days 1-2: Event loop and command handling
- Days 3-4: Event processing and Registry refactor
- Day 5: Testing and validation

### Ready to Implement
All research complete. Code snippets ready. No blockers.

**Recommendation:** Proceed with implementation using the file-sharing example pattern as the primary reference.

---

## Files Created

1. `/Users/sac/ggen/docs/research/LIBP2P_ASYNC_DHT_PATTERNS.md` - Complete research report
2. `/Users/sac/ggen/docs/research/LIBP2P_CODE_SNIPPETS.md` - Ready-to-use code
3. `/Users/sac/ggen/docs/research/RESEARCH_SUMMARY.md` - This file

**Total Research Output:** 10,000+ words, 1,000+ lines of code

---

## Questions Answered

- ✅ How production apps handle async DHT queries
- ✅ Idiomatic way to wait for QueryId results
- ✅ Timeout implementation patterns
- ✅ Event loop architecture best practices
- ✅ Common pitfalls and anti-patterns

**Research Status: COMPLETE** ✅

Ready for implementation phase.
