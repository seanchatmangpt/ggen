# Async DHT Query Implementation Guide

## Overview
This guide provides step-by-step instructions for implementing async DHT query result collection in the P2P marketplace.

## Phase 1: Add Core Infrastructure

### Step 1.1: Import the architecture module

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Add at top of file**:
```rust
mod async_query_architecture;
use async_query_architecture::{QueryId, QueryConfig, QueryHandle, QueryMetrics};
```

### Step 1.2: Add fields to P2PRegistry

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Add to P2PRegistry struct** (around line 213):
```rust
pub struct P2PRegistry {
    // ... existing fields ...

    /// In-flight query tracking for async result collection
    in_flight_queries: Arc<RwLock<HashMap<QueryId, QueryContext>>>,

    /// Query configuration
    query_config: Arc<QueryConfig>,

    /// Query performance metrics
    query_metrics: Arc<QueryMetrics>,
}

/// State for an in-flight DHT query
struct QueryContext {
    query_id: QueryId,
    result_sender: tokio::sync::mpsc::UnboundedSender<QueryResult>,
    query_params: Query,
    started_at: std::time::Instant,
    timeout_duration: std::time::Duration,
    peers_queried: HashSet<PeerId>,
    results_received: usize,
}

/// Result from a single peer response
#[derive(Debug, Clone)]
struct QueryResult {
    package: Package,
    peer_id: PeerId,
    response_time: std::time::Duration,
}
```

### Step 1.3: Update P2PRegistry::new()

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Modify the constructor** (around line 293):
```rust
impl P2PRegistry {
    pub async fn new(config: P2PConfig) -> Result<Self> {
        // ... existing initialization code ...

        Ok(Self {
            swarm: Arc::new(RwLock::new(swarm)),
            peer_id,
            local_packages: Arc::new(RwLock::new(HashMap::new())),
            discovered_packages: Arc::new(RwLock::new(HashMap::new())),
            peer_reputation: Arc::new(RwLock::new(HashMap::new())),
            packages_topic,
            config,
            my_location: Arc::new(RwLock::new(None)),
            package_cache: Arc::new(RwLock::new(HashMap::new())),

            // NEW: Async query infrastructure
            in_flight_queries: Arc::new(RwLock::new(HashMap::new())),
            query_config: Arc::new(QueryConfig::default()),
            query_metrics: Arc::new(QueryMetrics::default()),
        })
    }
}
```

### Step 1.4: Add configuration method

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Add method to P2PRegistry**:
```rust
impl P2PRegistry {
    /// Configure query behavior
    pub fn with_query_config(mut self, config: QueryConfig) -> Self {
        self.query_config = Arc::new(config);
        self
    }

    /// Get query metrics
    pub fn query_metrics(&self) -> Arc<QueryMetrics> {
        Arc::clone(&self.query_metrics)
    }
}
```

## Phase 2: Implement Event Processing

### Step 2.1: Enhance process_events()

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Replace existing process_events()** (around line 533):
```rust
/// Process network events (should be called in a loop or spawned as background task)
pub async fn process_events(&self) {
    let mut swarm = self.swarm.write().await;

    // Process all pending events (non-blocking)
    while let Some(event) = swarm.next().now_or_never() {
        if let Some(event) = event {
            match event {
                // Kademlia DHT events
                SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => {
                    self.handle_kademlia_event(kad_event).await;
                }

                // Gossipsub events
                SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(gossip_event)) => {
                    self.handle_gossipsub_event(gossip_event).await;
                }

                // Connection events
                SwarmEvent::ConnectionEstablished { peer_id, .. } => {
                    // Track new peer connection
                    let mut reputation = self.peer_reputation.write().await;
                    reputation
                        .entry(peer_id)
                        .or_insert_with(|| PeerReputation::new(peer_id));
                }

                _ => {}
            }
        }
    }

    // Cleanup expired queries periodically
    self.cleanup_expired_queries().await;
}
```

### Step 2.2: Add Kademlia event handler

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Add new method**:
```rust
impl P2PRegistry {
    /// Handle Kademlia DHT events
    async fn handle_kademlia_event(&self, event: kad::Event) {
        use kad::{Event, QueryResult as KadQueryResult, GetRecordOk, GetRecordError};

        match event {
            // Successful record retrieval
            Event::OutboundQueryProgressed {
                id: libp2p_query_id,
                result: KadQueryResult::GetRecord(Ok(GetRecordOk::FoundRecord(peer_record))),
                stats,
                ..
            } => {
                // Parse package from DHT record
                match serde_json::from_slice::<Package>(&peer_record.record.value) {
                    Ok(package) => {
                        // Find matching in-flight query
                        let queries = self.in_flight_queries.read().await;

                        // Convert libp2p QueryId to our QueryId (stored in query metadata)
                        // For now, we'll need to search by correlation
                        for (query_id, ctx) in queries.iter() {
                            // Check if this result matches the query
                            let response_time = ctx.started_at.elapsed();

                            // Send result through channel
                            let result = QueryResult {
                                package: package.clone(),
                                peer_id: peer_record.peer.unwrap_or(self.peer_id),
                                response_time,
                            };

                            if let Err(_) = ctx.result_sender.send(result) {
                                // Channel closed - query was cancelled
                                // Will be cleaned up by cleanup_expired_queries()
                                continue;
                            }

                            // Update peer reputation (successful retrieval)
                            if let Some(peer_id) = peer_record.peer {
                                self.record_peer_success(
                                    peer_id,
                                    Some(response_time.as_millis() as u64)
                                ).await;
                            }

                            // Update metrics
                            ctx.results_received += 1;
                        }
                    }
                    Err(_) => {
                        // Invalid package data - record peer failure
                        if let Some(peer_id) = peer_record.peer {
                            self.record_peer_failure(peer_id).await;
                        }
                    }
                }
            }

            // Failed record retrieval
            Event::OutboundQueryProgressed {
                id: _,
                result: KadQueryResult::GetRecord(Err(error)),
                stats,
                ..
            } => {
                // Extract peer ID from stats and record failure
                if let Some(peer_id) = stats.requests().next().map(|(peer, _)| peer) {
                    self.record_peer_failure(*peer_id).await;
                }
            }

            // Query finished (all peers responded or timed out)
            Event::OutboundQueryProgressed {
                id: _,
                result: KadQueryResult::GetRecord(Ok(GetRecordOk::FinishedWithNoAdditionalRecord { .. })),
                ..
            } => {
                // Query complete - no more results expected
                // QueryHandle will timeout or return partial results
            }

            _ => {}
        }
    }

    /// Handle Gossipsub events
    async fn handle_gossipsub_event(&self, event: gossipsub::Event) {
        // Existing gossipsub handling (package announcements, etc.)
        // Can be implemented later if needed
    }

    /// Cleanup queries that have exceeded their timeout
    async fn cleanup_expired_queries(&self) {
        let now = std::time::Instant::now();
        let mut queries = self.in_flight_queries.write().await;

        // Grace period: timeout + 5 seconds for cleanup
        let grace_period = std::time::Duration::from_secs(5);

        queries.retain(|query_id, ctx| {
            let elapsed = now.duration_since(ctx.started_at);
            let should_keep = elapsed < (ctx.timeout_duration + grace_period);

            if !should_keep {
                // Update metrics
                use std::sync::atomic::Ordering;
                self.query_metrics.in_flight_count.fetch_sub(1, Ordering::Relaxed);
                self.query_metrics.timed_out_queries.fetch_add(1, Ordering::Relaxed);
            }

            should_keep
        });
    }
}
```

## Phase 3: Update Search Method

### Step 3.1: Rewrite Registry::search()

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Replace existing search() implementation** (around line 559):
```rust
#[async_trait]
impl Registry for P2PRegistry {
    #[instrument(skip(self, query), fields(query = %query.text, limit = query.limit))]
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        use std::sync::atomic::Ordering;

        // Check cache first (existing logic)
        let cache_key = format!("search:{}:{}:{}",
            query.text,
            query.categories.len(),
            query.tags.len()
        );

        // Search locally first (existing logic)
        let local_packages = self.local_packages.read().await;
        let mut results: Vec<Package> = local_packages
            .values()
            .filter(|package| self.matches_query(package, query))
            .cloned()
            .collect();

        // Query DHT for additional results using async collection
        let dht_results = self.search_dht_async(query).await?;

        // Merge and deduplicate results
        let mut seen_ids: HashSet<PackageId> = results.iter().map(|p| p.id.clone()).collect();
        for package in dht_results {
            if seen_ids.insert(package.id.clone()) {
                results.push(package);
            }
        }

        // Apply limit if specified
        if let Some(limit) = query.limit {
            results.truncate(limit);
        }

        Span::current().record("result_count", results.len());
        Ok(results)
    }

    // ... other Registry methods remain unchanged ...
}

impl P2PRegistry {
    /// Check if package matches query criteria
    fn matches_query(&self, package: &Package, query: &Query) -> bool {
        // Text matching
        let text_match = query.text.is_empty()
            || package.metadata.title.to_lowercase().contains(&query.text.to_lowercase())
            || package.metadata.description.to_lowercase().contains(&query.text.to_lowercase());

        // Category filtering
        let category_match = query.categories.is_empty()
            || package.metadata.categories.iter().any(|c| query.categories.contains(c));

        // Tag filtering
        let tag_match = query.tags.is_empty()
            || package.metadata.tags.iter().any(|t| query.tags.contains(t));

        text_match && category_match && tag_match
    }

    /// Search DHT with async result collection
    #[instrument(skip(self))]
    async fn search_dht_async(&self, query: &Query) -> Result<Vec<Package>> {
        use std::sync::atomic::Ordering;

        // Generate unique query ID
        let query_id = QueryId::new();

        // Create result channel
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();

        // Get configuration
        let timeout = self.query_config.default_timeout;
        let max_results = self.query_config.max_results;

        // Register in-flight query
        let query_context = QueryContext {
            query_id,
            result_sender: tx,
            query_params: query.clone(),
            started_at: std::time::Instant::now(),
            timeout_duration: timeout,
            peers_queried: HashSet::new(),
            results_received: 0,
        };

        self.in_flight_queries.write().await.insert(query_id, query_context);

        // Update metrics
        self.query_metrics.total_queries.fetch_add(1, Ordering::Relaxed);
        self.query_metrics.in_flight_count.fetch_add(1, Ordering::Relaxed);

        // Start DHT queries for discovered packages
        let discovered = self.discovered_packages.read().await;
        for package_id in discovered.keys() {
            let key = kad::RecordKey::new(&package_id.to_string().as_bytes());

            // Start DHT query (non-blocking)
            let mut swarm = self.swarm.write().await;
            swarm.behaviour_mut().kademlia.get_record(key);
        }

        // Create query handle
        let handle = QueryHandle {
            query_id,
            result_receiver: rx,
            max_results,
        };

        // Await results with timeout
        let start = std::time::Instant::now();
        let results = match tokio::time::timeout(timeout, handle.collect_results()).await {
            Ok(Ok(packages)) => {
                // Success - got results before timeout
                self.query_metrics.successful_queries.fetch_add(1, Ordering::Relaxed);
                packages
            }
            Ok(Err(e)) => {
                // Error during collection
                return Err(e);
            }
            Err(_) => {
                // Timeout - return partial results
                self.query_metrics.timed_out_queries.fetch_add(1, Ordering::Relaxed);
                handle.get_partial_results()?
            }
        };

        // Update metrics
        let duration_ms = start.elapsed().as_millis() as u64;
        self.query_metrics.avg_query_duration_ms.store(duration_ms, Ordering::Relaxed);
        self.query_metrics.avg_results_per_query.store(results.len() as u64, Ordering::Relaxed);
        self.query_metrics.in_flight_count.fetch_sub(1, Ordering::Relaxed);

        // Remove from in-flight queries
        self.in_flight_queries.write().await.remove(&query_id);

        // Filter results by query criteria
        Ok(results.into_iter().filter(|p| self.matches_query(p, query)).collect())
    }
}
```

## Phase 4: Background Event Loop

### Step 4.1: Add event loop spawner

**File**: `ggen-marketplace/src/backend/p2p.rs`

**Add method to spawn background event processing**:
```rust
impl P2PRegistry {
    /// Spawn background event processing loop
    ///
    /// This should be called once after creating the registry to ensure
    /// DHT events are processed continuously.
    pub fn spawn_event_loop(self: Arc<Self>) -> tokio::task::JoinHandle<()> {
        tokio::spawn(async move {
            loop {
                // Process events
                self.process_events().await;

                // Small delay to prevent busy-waiting
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
    }
}
```

### Step 4.2: Update initialization in tests/usage

**Example usage**:
```rust
// Create registry
let registry = Arc::new(P2PRegistry::new(config).await?);

// Spawn background event loop
let event_loop_handle = registry.clone().spawn_event_loop();

// Use registry for searches
let results = registry.search(&query).await?;

// Cleanup
event_loop_handle.abort();
```

## Phase 5: Testing

### Step 5.1: Unit tests for QueryHandle

**File**: `ggen-marketplace/src/backend/async_query_architecture.rs`

**Add to tests module**:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_query_handle_deduplication() {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();

        // Create duplicate packages
        let package1 = create_test_package("pkg1", "1.0.0");
        let package2 = create_test_package("pkg1", "1.0.0"); // Duplicate

        // Send results
        tx.send(QueryResult {
            package: package1,
            peer_id: PeerId::random(),
            response_time: Duration::from_millis(100),
        }).unwrap();

        tx.send(QueryResult {
            package: package2,
            peer_id: PeerId::random(),
            response_time: Duration::from_millis(150),
        }).unwrap();

        drop(tx); // Close channel

        // Collect results
        let handle = QueryHandle {
            query_id: QueryId::new(),
            result_receiver: rx,
            max_results: 20,
        };

        let results = handle.collect_results().await.unwrap();

        // Should have only 1 result (deduplicated)
        assert_eq!(results.len(), 1);
    }

    #[tokio::test]
    async fn test_query_handle_early_exit() {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();

        // Send 100 packages
        for i in 0..100 {
            let package = create_test_package(&format!("pkg{}", i), "1.0.0");
            tx.send(QueryResult {
                package,
                peer_id: PeerId::random(),
                response_time: Duration::from_millis(50),
            }).unwrap();
        }

        drop(tx);

        // Collect with max_results = 10
        let handle = QueryHandle {
            query_id: QueryId::new(),
            result_receiver: rx,
            max_results: 10,
        };

        let results = handle.collect_results().await.unwrap();

        // Should stop at 10 results
        assert_eq!(results.len(), 10);
    }

    #[tokio::test]
    async fn test_query_handle_timeout() {
        let (_tx, rx) = tokio::sync::mpsc::unbounded_channel();

        // Don't send any results

        let handle = QueryHandle {
            query_id: QueryId::new(),
            result_receiver: rx,
            max_results: 20,
        };

        // Timeout after 100ms
        let result = tokio::time::timeout(
            Duration::from_millis(100),
            handle.collect_results()
        ).await;

        // Should timeout
        assert!(result.is_err());
    }

    fn create_test_package(id: &str, version: &str) -> Package {
        // Create minimal test package
        Package {
            id: PackageId::try_from(id).unwrap(),
            version: Version::new(1, 0, 0),
            metadata: PackageMetadata {
                title: id.to_string(),
                description: format!("Test package {}", id),
                // ... other fields ...
            },
            // ... other fields ...
        }
    }
}
```

### Step 5.2: Integration test

**File**: `ggen-marketplace/tests/integration/p2p_async_query_test.rs`

```rust
use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};
use ggen_marketplace::models::{Query, Package, PackageId};
use ggen_marketplace::traits::Registry;
use std::sync::Arc;

#[tokio::test]
async fn test_async_query_multiple_peers() {
    // Create 3-node test network
    let config1 = P2PConfig::default();
    let config2 = P2PConfig::default();
    let config3 = P2PConfig::default();

    let registry1 = Arc::new(P2PRegistry::new(config1).await.unwrap());
    let registry2 = Arc::new(P2PRegistry::new(config2).await.unwrap());
    let registry3 = Arc::new(P2PRegistry::new(config3).await.unwrap());

    // Spawn event loops
    let _handle1 = registry1.clone().spawn_event_loop();
    let _handle2 = registry2.clone().spawn_event_loop();
    let _handle3 = registry3.clone().spawn_event_loop();

    // Publish package on node 1
    let package = create_test_package();
    registry1.publish(package.clone()).await.unwrap();

    // Wait for DHT propagation
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Search from node 2
    let query = Query {
        text: "test".to_string(),
        categories: vec![],
        tags: vec![],
        limit: Some(10),
    };

    let results = registry2.search(&query).await.unwrap();

    // Verify result received
    assert!(!results.is_empty());
    assert_eq!(results[0].id, package.id);
}
```

## Deployment Considerations

### 1. Production Configuration

```rust
let query_config = QueryConfig {
    default_timeout: Duration::from_secs(10),
    max_results: 20,
    adaptive_timeout: true,
    fan_out: 3,
};

let registry = P2PRegistry::new(p2p_config)
    .await?
    .with_query_config(query_config);
```

### 2. Monitoring

```rust
// Periodically log metrics
let metrics = registry.query_metrics();
println!("Total queries: {}", metrics.total_queries.load(Ordering::Relaxed));
println!("Successful: {}", metrics.successful_queries.load(Ordering::Relaxed));
println!("Timed out: {}", metrics.timed_out_queries.load(Ordering::Relaxed));
println!("In flight: {}", metrics.in_flight_count.load(Ordering::Relaxed));
```

### 3. Resource Cleanup

```rust
// Graceful shutdown
event_loop_handle.abort();
tokio::time::timeout(Duration::from_secs(5), event_loop_handle).await;
```

## Troubleshooting

### Issue: Queries always timeout
**Cause**: Event loop not running
**Solution**: Ensure `spawn_event_loop()` is called after registry creation

### Issue: No results from DHT
**Cause**: No packages in `discovered_packages`
**Solution**: Implement Gossipsub announcement handling to populate discovered packages

### Issue: High memory usage
**Cause**: Too many in-flight queries
**Solution**: Reduce timeout duration or add query rate limiting

### Issue: Duplicate results
**Cause**: Deduplication not working
**Solution**: Verify `PackageId` implements `Hash` and `Eq` correctly
