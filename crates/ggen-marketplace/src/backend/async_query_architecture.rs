//! Async DHT Query Result Collection Architecture (v2.4.0)
//!
//! # Problem Statement
//! The current P2P implementation starts DHT queries but immediately returns None,
//! with no mechanism to wait for and collect results from network peers.
//!
//! # Architecture Design
//!
//! ## Overview
//! This module provides a channel-based async result collection system for DHT queries
//! with timeout handling, peer reputation tracking, and result deduplication.
//!
//! ## Core Components
//!
//! 1. **QueryManager**: Orchestrates in-flight queries and result collection
//! 2. **QueryHandle**: Future that awaits query completion with timeout
//! 3. **ResultChannel**: Multi-producer single-consumer channel for peer results
//! 4. **QueryId**: Unique identifier linking queries to waiting futures
//!
//! ## Data Flow Sequence
//!
//! ```text
//! [Client] --search()-->  [QueryManager]
//!                              |
//!                              v
//!                      [Generate QueryId]
//!                              |
//!                              v
//!                      [Create ResultChannel]
//!                              |
//!                              +---> [Store in InFlightQueries map]
//!                              |
//!                              v
//!                      [Start DHT Query]
//!                              |
//!                              v
//!                      [Return QueryHandle future]
//!                              |
//!                              v
//!                      [Client awaits handle]
//!                              |
//!    [Event Loop] <------------+
//!         |
//!         v
//!    [Process DHT Events]
//!         |
//!         +---> [Got Record Response]
//!         |              |
//!         |              v
//!         |      [Parse Package]
//!         |              |
//!         |              v
//!         |      [Send via ResultChannel]
//!         |              |
//!         |              v
//!         |      [QueryHandle receives result]
//!         |              |
//!         |              v
//!         |      [Update peer reputation]
//!         |              |
//!         |              v
//!         |      [Deduplicate results]
//!         |              |
//!         |              v
//!         +---> [Timeout?] --YES--> [Return collected results]
//!                       |
//!                       NO
//!                       |
//!                       v
//!              [Continue waiting]
//! ```
//!
//! ## Architecture Decisions
//!
//! ### Decision 1: Channel Type
//! **Choice**: `tokio::sync::mpsc::unbounded_channel<QueryResult>`
//! **Rationale**:
//! - Unbounded allows any number of peers to respond without blocking
//! - MPSC pattern: Multiple peers (producers) → Single query handler (consumer)
//! - Tokio integration for seamless async/await
//! **Alternative Considered**: `crossbeam::channel` (rejected: requires additional runtime)
//!
//! ### Decision 2: Query ID Mapping
//! **Choice**: `HashMap<QueryId, QueryContext>` with RwLock
//! **Rationale**:
//! - Fast lookup by query ID in event loop
//! - RwLock allows concurrent reads for query status checks
//! - HashMap provides O(1) lookup performance
//! **Alternative Considered**: `DashMap` (rejected: overkill for this use case)
//!
//! ### Decision 3: Timeout Strategy
//! **Choice**: `tokio::time::timeout` with configurable duration
//! **Rationale**:
//! - Native tokio timeout support
//! - Cancellation-safe (drops future cleanly)
//! - Per-query timeout configuration
//! **Alternative Considered**: Manual `tokio::select!` (rejected: more verbose, same result)
//!
//! ### Decision 4: In-Flight Query Storage
//! **Choice**: Struct with QueryId, mpsc::Sender, start time, timeout
//! **Rationale**:
//! - Encapsulates all query state in one place
//! - Easy cleanup on timeout or completion
//! - Supports query introspection and debugging
//!
//! ## Implementation Patterns
//!
//! ### Pattern 1: Query Initiation
//!
//! ```rust
//! // In P2PRegistry::search()
//! async fn search(&self, query: &Query) -> Result<Vec<Package>> {
//!     // 1. Generate unique query ID
//!     let query_id = QueryId::new();
//!
//!     // 2. Create result channel
//!     let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
//!
//!     // 3. Register in-flight query
//!     let query_context = QueryContext {
//!         query_id,
//!         result_sender: tx,
//!         query_params: query.clone(),
//!         started_at: Instant::now(),
//!         timeout_duration: Duration::from_secs(10),
//!         peers_queried: HashSet::new(),
//!         results_received: 0,
//!     };
//!
//!     self.in_flight_queries.write().await.insert(query_id, query_context);
//!
//!     // 4. Start DHT query (non-blocking)
//!     self.swarm.write().await
//!         .behaviour_mut()
//!         .kademlia
//!         .get_record(key);
//!
//!     // 5. Return future that collects results
//!     let handle = QueryHandle {
//!         query_id,
//!         result_receiver: rx,
//!         registry: Arc::downgrade(&self.shared_state),
//!     };
//!
//!     // 6. Await with timeout
//!     tokio::time::timeout(
//!         Duration::from_secs(10),
//!         handle.collect_results()
//!     ).await
//!     .unwrap_or_else(|_| handle.get_partial_results())
//! }
//! ```
//!
//! ### Pattern 2: Event Processing
//!
//! ```rust
//! // In P2PRegistry::process_events()
//! pub async fn process_events(&self) {
//!     let mut swarm = self.swarm.write().await;
//!
//!     // Process all pending events
//!     while let Some(event) = swarm.next().now_or_never() {
//!         if let Some(event) = event {
//!             match event {
//!                 SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(
//!                     kad::Event::OutboundQueryProgressed {
//!                         id: query_id,
//!                         result: QueryResult::GetRecord(Ok(GetRecordOk::FoundRecord(record))),
//!                         ..
//!                     }
//!                 )) => {
//!                     // 1. Parse the package from record
//!                     if let Ok(package) = serde_json::from_slice::<Package>(&record.record.value) {
//!                         // 2. Find the query context
//!                         let queries = self.in_flight_queries.read().await;
//!                         if let Some(ctx) = queries.get(&QueryId::from(query_id)) {
//!                             // 3. Send result through channel (non-blocking)
//!                             let _ = ctx.result_sender.send(QueryResult {
//!                                 package,
//!                                 peer_id: record.peer,
//!                                 response_time: ctx.started_at.elapsed(),
//!                             });
//!
//!                             // 4. Update peer reputation
//!                             self.record_peer_success(
//!                                 record.peer,
//!                                 Some(ctx.started_at.elapsed().as_millis() as u64)
//!                             ).await;
//!                         }
//!                     }
//!                 }
//!
//!                 SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(
//!                     kad::Event::OutboundQueryProgressed {
//!                         id: query_id,
//!                         result: QueryResult::GetRecord(Err(_)),
//!                         stats,
//!                         ..
//!                     }
//!                 )) => {
//!                     // Query failed - update peer reputation
//!                     if let Some(peer_id) = stats.requests().next() {
//!                         self.record_peer_failure(*peer_id).await;
//!                     }
//!                 }
//!
//!                 _ => {}
//!             }
//!         }
//!     }
//!
//!     // Cleanup expired queries
//!     self.cleanup_expired_queries().await;
//! }
//! ```
//!
//! ### Pattern 3: Result Collection
//!
//! ```rust
//! impl QueryHandle {
//!     /// Collect results from channel until timeout or sufficient results
//!     async fn collect_results(mut self) -> Result<Vec<Package>> {
//!         let mut results = Vec::new();
//!         let mut seen_ids = HashSet::new();
//!
//!         // Collect results from channel
//!         while let Some(result) = self.result_receiver.recv().await {
//!             // Deduplicate by package ID
//!             if seen_ids.insert(result.package.id.clone()) {
//!                 results.push(result.package);
//!             }
//!
//!             // Early exit if we have enough results
//!             if results.len() >= 20 {
//!                 break;
//!             }
//!         }
//!
//!         Ok(results)
//!     }
//!
//!     /// Get partial results on timeout
//!     fn get_partial_results(mut self) -> Result<Vec<Package>> {
//!         let mut results = Vec::new();
//!         let mut seen_ids = HashSet::new();
//!
//!         // Drain any pending results
//!         while let Ok(result) = self.result_receiver.try_recv() {
//!             if seen_ids.insert(result.package.id.clone()) {
//!                 results.push(result.package);
//!             }
//!         }
//!
//!         Ok(results)
//!     }
//! }
//! ```
//!
//! ### Pattern 4: Timeout Handling with tokio::select!
//!
//! ```rust
//! // Alternative timeout pattern using select! for more control
//! async fn search_with_adaptive_timeout(&self, query: &Query) -> Result<Vec<Package>> {
//!     let query_id = QueryId::new();
//!     let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
//!
//!     // Register query...
//!
//!     let mut results = Vec::new();
//!     let mut seen_ids = HashSet::new();
//!     let timeout = tokio::time::sleep(Duration::from_secs(10));
//!     tokio::pin!(timeout);
//!
//!     loop {
//!         tokio::select! {
//!             // Receive result from channel
//!             Some(result) = rx.recv() => {
//!                 if seen_ids.insert(result.package.id.clone()) {
//!                     results.push(result.package);
//!
//!                     // Adaptive timeout: extend if we're getting good results
//!                     if results.len() % 5 == 0 {
//!                         timeout.as_mut().reset(
//!                             tokio::time::Instant::now() + Duration::from_secs(2)
//!                         );
//!                     }
//!                 }
//!
//!                 // Early exit if sufficient results
//!                 if results.len() >= 20 {
//!                     break;
//!                 }
//!             }
//!
//!             // Timeout elapsed
//!             _ = &mut timeout => {
//!                 break;
//!             }
//!         }
//!     }
//!
//!     Ok(results)
//! }
//! ```
//!
//! ## Data Structures
//!
//! ### QueryId
//! ```rust
//! /// Unique identifier for in-flight DHT queries
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//! pub struct QueryId(uuid::Uuid);
//!
//! impl QueryId {
//!     pub fn new() -> Self {
//!         Self(uuid::Uuid::new_v4())
//!     }
//! }
//! ```
//!
//! ### QueryContext
//! ```rust
//! /// State for an in-flight query
//! struct QueryContext {
//!     query_id: QueryId,
//!     result_sender: mpsc::UnboundedSender<QueryResult>,
//!     query_params: Query,
//!     started_at: Instant,
//!     timeout_duration: Duration,
//!     peers_queried: HashSet<PeerId>,
//!     results_received: usize,
//! }
//! ```
//!
//! ### QueryResult
//! ```rust
//! /// Result from a single peer
//! struct QueryResult {
//!     package: Package,
//!     peer_id: PeerId,
//!     response_time: Duration,
//! }
//! ```
//!
//! ### QueryHandle
//! ```rust
//! /// Future that resolves to query results
//! pub struct QueryHandle {
//!     query_id: QueryId,
//!     result_receiver: mpsc::UnboundedReceiver<QueryResult>,
//!     registry: Weak<P2PRegistryShared>,
//! }
//! ```
//!
//! ## Error Handling Strategy
//!
//! ### 1. Network Errors (Peer Unreachable)
//! ```rust
//! // In event loop
//! kad::Event::OutboundQueryProgressed {
//!     result: QueryResult::GetRecord(Err(GetRecordError::Timeout { .. })),
//!     ..
//! } => {
//!     // Record peer failure but don't fail entire query
//!     self.record_peer_failure(peer_id).await;
//!     // Query continues with other peers
//! }
//! ```
//!
//! ### 2. Deserialization Errors (Corrupt Data)
//! ```rust
//! // In event processing
//! if let Ok(package) = serde_json::from_slice::<Package>(&record.value) {
//!     // Valid package - send to channel
//!     ctx.result_sender.send(QueryResult { package, .. });
//! } else {
//!     // Invalid data - record peer failure
//!     self.record_peer_failure(record.peer).await;
//! }
//! ```
//!
//! ### 3. Timeout (Slow Network)
//! ```rust
//! // In search()
//! match tokio::time::timeout(Duration::from_secs(10), handle.collect_results()).await {
//!     Ok(results) => results,  // Got results before timeout
//!     Err(_) => {
//!         // Timeout - return partial results
//!         handle.get_partial_results()
//!     }
//! }
//! ```
//!
//! ### 4. Channel Closed (Query Cancelled)
//! ```rust
//! // In event processing
//! if let Err(mpsc::error::SendError(_)) = ctx.result_sender.send(result) {
//!     // Query was cancelled or handle dropped
//!     // Clean up from in-flight queries
//!     self.in_flight_queries.write().await.remove(&query_id);
//! }
//! ```
//!
//! ## Performance Optimizations
//!
//! ### 1. Result Deduplication
//! - Use `HashSet<PackageId>` to track seen packages
//! - Prevents duplicate results from multiple peers providing same package
//!
//! ### 2. Early Exit
//! - Stop collecting after N results (configurable, default 20)
//! - Reduces latency for queries with many results
//!
//! ### 3. Adaptive Timeout
//! - Extend timeout if results are arriving (shows network is responsive)
//! - Shorten timeout if no results after initial period
//!
//! ### 4. Peer Selection
//! - Query high-reputation peers first (existing reputation system)
//! - Use geo-proximity for faster responses (existing GeoLocation)
//!
//! ### 5. Query Cleanup
//! ```rust
//! async fn cleanup_expired_queries(&self) {
//!     let now = Instant::now();
//!     let mut queries = self.in_flight_queries.write().await;
//!
//!     queries.retain(|_, ctx| {
//!         let elapsed = now.duration_since(ctx.started_at);
//!         elapsed < ctx.timeout_duration + Duration::from_secs(5)  // Grace period
//!     });
//! }
//! ```
//!
//! ## Concurrency Safety
//!
//! ### Thread Safety
//! - All shared state uses `Arc<RwLock<T>>`
//! - Event loop has exclusive write access to swarm
//! - Query contexts use MPSC channels (naturally thread-safe)
//!
//! ### Cancellation Safety
//! - Dropping `QueryHandle` closes receiver, event loop detects via SendError
//! - Timeout dropping future is safe (channel cleanup is automatic)
//! - No blocking operations in async code
//!
//! ### Deadlock Prevention
//! - Lock ordering: Always acquire swarm lock before in_flight_queries lock
//! - Short critical sections (no async operations while holding locks)
//! - Use `try_lock()` where appropriate to avoid blocking
//!
//! ## Integration Points
//!
//! ### 1. Modified P2PRegistry Struct
//! ```rust
//! pub struct P2PRegistry {
//!     // ... existing fields ...
//!
//!     /// In-flight query tracking
//!     in_flight_queries: Arc<RwLock<HashMap<QueryId, QueryContext>>>,
//!
//!     /// Query configuration
//!     query_config: Arc<QueryConfig>,
//! }
//! ```
//!
//! ### 2. Query Configuration
//! ```rust
//! pub struct QueryConfig {
//!     /// Default query timeout
//!     pub default_timeout: Duration,
//!
//!     /// Maximum results per query
//!     pub max_results: usize,
//!
//!     /// Enable adaptive timeout
//!     pub adaptive_timeout: bool,
//!
//!     /// Fan-out factor for parallel queries
//!     pub fan_out: usize,
//! }
//!
//! impl Default for QueryConfig {
//!     fn default() -> Self {
//!         Self {
//!             default_timeout: Duration::from_secs(10),
//!             max_results: 20,
//!             adaptive_timeout: true,
//!             fan_out: 3,
//!         }
//!     }
//! }
//! ```
//!
//! ### 3. Modified Registry::search() Implementation
//! The search() method in the Registry trait implementation will be updated to:
//! 1. Create QueryHandle with result channel
//! 2. Start DHT query asynchronously
//! 3. Await results with timeout
//! 4. Apply deduplication and filtering
//! 5. Update peer reputations based on response quality
//!
//! ## Testing Strategy
//!
//! ### Unit Tests
//! - QueryId generation and uniqueness
//! - QueryContext creation and cleanup
//! - Result deduplication logic
//! - Timeout handling
//!
//! ### Integration Tests
//! - Multi-peer result collection
//! - Peer reputation updates
//! - Timeout with partial results
//! - Concurrent query handling
//!
//! ### Performance Tests
//! - Query throughput (queries per second)
//! - Latency distribution (p50, p95, p99)
//! - Memory usage under load
//! - Cleanup efficiency
//!
//! ## Monitoring and Observability
//!
//! ### Metrics to Track
//! ```rust
//! pub struct QueryMetrics {
//!     /// Total queries initiated
//!     pub total_queries: AtomicU64,
//!
//!     /// Queries that completed successfully
//!     pub successful_queries: AtomicU64,
//!
//!     /// Queries that timed out
//!     pub timed_out_queries: AtomicU64,
//!
//!     /// Average results per query
//!     pub avg_results_per_query: AtomicU64,
//!
//!     /// Average query duration (milliseconds)
//!     pub avg_query_duration_ms: AtomicU64,
//!
//!     /// Currently in-flight queries
//!     pub in_flight_count: AtomicUsize,
//! }
//! ```
//!
//! ### Tracing Integration
//! - Use existing `#[instrument]` macros
//! - Add spans for query lifecycle
//! - Log peer response times
//! - Track query completion status
//!
//! ## Migration Path
//!
//! ### Phase 1: Add Infrastructure
//! 1. Add QueryId, QueryContext, QueryHandle types
//! 2. Add in_flight_queries map to P2PRegistry
//! 3. Add query configuration
//!
//! ### Phase 2: Implement Event Processing
//! 1. Enhance process_events() to handle DHT responses
//! 2. Add result channel sending logic
//! 3. Add peer reputation updates
//!
//! ### Phase 3: Update Search Method
//! 1. Modify search() to create QueryHandle
//! 2. Add timeout and result collection
//! 3. Add deduplication logic
//!
//! ### Phase 4: Optimization
//! 1. Add adaptive timeout
//! 2. Add early exit conditions
//! 3. Add query cleanup task
//!
//! ## Future Enhancements
//!
//! ### 1. Query Caching
//! - Cache recent query results for fast repeated searches
//! - TTL-based cache invalidation
//!
//! ### 2. Query Federation
//! - Combine results from multiple DHT networks
//! - Cross-registry search support
//!
//! ### 3. Streaming Results
//! - Return results as they arrive (Stream instead of Vec)
//! - Better UX for slow networks
//!
//! ### 4. Query Prioritization
//! - High-priority queries get more network resources
//! - Background prefetching for predicted queries

use crate::error::{MarketplaceError, Result};
use crate::models::{Package, PackageId, Query};
use libp2p::{kad, PeerId};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, RwLock};

/// Unique identifier for in-flight DHT queries
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct QueryId(uuid::Uuid);

impl QueryId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

impl Default for QueryId {
    fn default() -> Self {
        Self::new()
    }
}

/// State for an in-flight query
struct QueryContext {
    query_id: QueryId,
    result_sender: mpsc::UnboundedSender<QueryResult>,
    query_params: Query,
    started_at: Instant,
    timeout_duration: Duration,
    peers_queried: HashSet<PeerId>,
    results_received: usize,
}

/// Result from a single peer
#[derive(Debug, Clone)]
struct QueryResult {
    package: Package,
    peer_id: PeerId,
    response_time: Duration,
}

/// Future that resolves to query results
pub struct QueryHandle {
    query_id: QueryId,
    result_receiver: mpsc::UnboundedReceiver<QueryResult>,
    max_results: usize,
}

impl QueryHandle {
    /// Collect results from channel until timeout or sufficient results
    pub async fn collect_results(mut self) -> Result<Vec<Package>> {
        let mut results = Vec::new();
        let mut seen_ids = HashSet::new();

        // Collect results from channel
        while let Some(result) = self.result_receiver.recv().await {
            // Deduplicate by package ID
            if seen_ids.insert(result.package.id.clone()) {
                results.push(result.package);
            }

            // Early exit if we have enough results
            if results.len() >= self.max_results {
                break;
            }
        }

        Ok(results)
    }

    /// Get partial results on timeout (non-blocking drain)
    pub fn get_partial_results(mut self) -> Result<Vec<Package>> {
        let mut results = Vec::new();
        let mut seen_ids = HashSet::new();

        // Drain any pending results
        while let Ok(result) = self.result_receiver.try_recv() {
            if seen_ids.insert(result.package.id.clone()) {
                results.push(result.package);
            }
        }

        Ok(results)
    }
}

/// Query configuration
#[derive(Debug, Clone)]
pub struct QueryConfig {
    /// Default query timeout
    pub default_timeout: Duration,

    /// Maximum results per query
    pub max_results: usize,

    /// Enable adaptive timeout
    pub adaptive_timeout: bool,

    /// Fan-out factor for parallel queries
    pub fan_out: usize,
}

impl Default for QueryConfig {
    fn default() -> Self {
        Self {
            default_timeout: Duration::from_secs(10),
            max_results: 20,
            adaptive_timeout: true,
            fan_out: 3,
        }
    }
}

/// Query performance metrics
#[derive(Debug, Default)]
pub struct QueryMetrics {
    /// Total queries initiated
    pub total_queries: std::sync::atomic::AtomicU64,

    /// Queries that completed successfully
    pub successful_queries: std::sync::atomic::AtomicU64,

    /// Queries that timed out
    pub timed_out_queries: std::sync::atomic::AtomicU64,

    /// Average results per query
    pub avg_results_per_query: std::sync::atomic::AtomicU64,

    /// Average query duration (milliseconds)
    pub avg_query_duration_ms: std::sync::atomic::AtomicU64,

    /// Currently in-flight queries
    pub in_flight_count: std::sync::atomic::AtomicUsize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_id_unique() {
        let id1 = QueryId::new();
        let id2 = QueryId::new();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_query_config_defaults() {
        let config = QueryConfig::default();
        assert_eq!(config.default_timeout, Duration::from_secs(10));
        assert_eq!(config.max_results, 20);
        assert!(config.adaptive_timeout);
        assert_eq!(config.fan_out, 3);
    }
}
