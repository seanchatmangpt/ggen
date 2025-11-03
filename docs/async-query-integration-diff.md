# Async DHT Query Integration - Code Diffs

## Overview
This document shows the exact code changes needed to integrate async query collection into the existing P2P registry.

## File: `ggen-marketplace/src/backend/p2p.rs`

### Change 1: Add imports (after line 24)

```diff
 use tokio::sync::RwLock;
 use tracing::{info_span, instrument, Span};
+
+// Async query architecture imports
+mod async_query_architecture;
+use async_query_architecture::{QueryId, QueryConfig, QueryHandle, QueryMetrics};
```

### Change 2: Add QueryContext and QueryResult types (after line 195)

```diff
 }

+/// State for an in-flight DHT query
+struct QueryContext {
+    query_id: QueryId,
+    result_sender: tokio::sync::mpsc::UnboundedSender<QueryResult>,
+    query_params: Query,
+    started_at: std::time::Instant,
+    timeout_duration: std::time::Duration,
+    peers_queried: std::collections::HashSet<PeerId>,
+    results_received: usize,
+}
+
+/// Result from a single peer response
+#[derive(Debug, Clone)]
+struct QueryResult {
+    package: Package,
+    peer_id: PeerId,
+    response_time: std::time::Duration,
+}
+
 /// P2P Registry implementation
 pub struct P2PRegistry {
```

### Change 3: Add fields to P2PRegistry struct (after line 217)

```diff
     /// Multi-tier cache for hot packages (v2.4.0)
     package_cache: Arc<RwLock<HashMap<PackageId, (Package, std::time::Instant)>>>,
+    /// In-flight query tracking for async result collection (v2.4.0)
+    in_flight_queries: Arc<RwLock<HashMap<QueryId, QueryContext>>>,
+    /// Query configuration
+    query_config: Arc<QueryConfig>,
+    /// Query performance metrics
+    query_metrics: Arc<QueryMetrics>,
 }
```

### Change 4: Update P2PRegistry::new() (after line 293)

```diff
             config,
             my_location: Arc::new(RwLock::new(None)),
             package_cache: Arc::new(RwLock::new(HashMap::new())),
+            // Async query infrastructure (v2.4.0)
+            in_flight_queries: Arc::new(RwLock::new(HashMap::new())),
+            query_config: Arc::new(QueryConfig::default()),
+            query_metrics: Arc::new(QueryMetrics::default()),
         })
     }
 }
```

### Change 5: Add configuration methods (after line 330)

```diff
     }

+    /// Configure query behavior (v2.4.0)
+    pub fn with_query_config(mut self, config: QueryConfig) -> Self {
+        self.query_config = Arc::new(config);
+        self
+    }
+
+    /// Get query metrics (v2.4.0)
+    pub fn query_metrics(&self) -> Arc<QueryMetrics> {
+        Arc::clone(&self.query_metrics)
+    }
+
     /// Announce a package to the network
     async fn announce_package(&self, package: &Package) -> Result<()> {
```

### Change 6: Replace process_events() implementation (replace lines 533-554)

```diff
-    /// Process network events (should be called in a loop)
+    /// Process network events (should be called in a loop or spawned as background task)
     pub async fn process_events(&self) {
         let mut swarm = self.swarm.write().await;

-        if let Some(event) = swarm.next().now_or_never() {
+        // Process all pending events (non-blocking)
+        while let Some(event) = swarm.next().now_or_never() {
             if let Some(event) = event {
                 match event {
-                    SwarmEvent::Behaviour(event) => {
-                        // Handle behavior events
-                        // In real implementation, process Kademlia, Gossipsub, and Identify events
+                    // Kademlia DHT events
+                    SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => {
+                        drop(swarm); // Release lock before async call
+                        self.handle_kademlia_event(kad_event).await;
+                        swarm = self.swarm.write().await; // Re-acquire
+                    }
+
+                    // Gossipsub events
+                    SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(gossip_event)) => {
+                        drop(swarm);
+                        self.handle_gossipsub_event(gossip_event).await;
+                        swarm = self.swarm.write().await;
                     }
+
+                    // Connection events
                     SwarmEvent::ConnectionEstablished { peer_id, .. } => {
                         // Track new peer connection
-                        let mut reputation = self.peer_reputation.write().await;
-                        reputation
-                            .entry(peer_id)
-                            .or_insert_with(|| PeerReputation::new(peer_id));
+                        drop(swarm);
+                        {
+                            let mut reputation = self.peer_reputation.write().await;
+                            reputation
+                                .entry(peer_id)
+                                .or_insert_with(|| PeerReputation::new(peer_id));
+                        }
+                        swarm = self.swarm.write().await;
                     }
+
                     _ => {}
                 }
             }
         }
+
+        drop(swarm); // Release lock before cleanup
+
+        // Cleanup expired queries periodically
+        self.cleanup_expired_queries().await;
     }
 }
```

### Change 7: Add new methods for event handling (after process_events)

```diff
+    /// Handle Kademlia DHT events (v2.4.0)
+    async fn handle_kademlia_event(&self, event: kad::Event) {
+        use kad::{Event, QueryResult as KadQueryResult, GetRecordOk, GetRecordError};
+
+        match event {
+            // Successful record retrieval
+            Event::OutboundQueryProgressed {
+                id: _libp2p_query_id,
+                result: KadQueryResult::GetRecord(Ok(GetRecordOk::FoundRecord(peer_record))),
+                ..
+            } => {
+                // Parse package from DHT record
+                match serde_json::from_slice::<Package>(&peer_record.record.value) {
+                    Ok(package) => {
+                        // Find matching in-flight queries
+                        let queries = self.in_flight_queries.read().await;
+
+                        // Send result to all matching queries
+                        for (_query_id, ctx) in queries.iter() {
+                            let response_time = ctx.started_at.elapsed();
+
+                            // Send result through channel
+                            let result = QueryResult {
+                                package: package.clone(),
+                                peer_id: peer_record.peer.unwrap_or(self.peer_id),
+                                response_time,
+                            };
+
+                            if let Err(_) = ctx.result_sender.send(result) {
+                                // Channel closed - query was cancelled
+                                continue;
+                            }
+                        }
+
+                        // Update peer reputation (successful retrieval)
+                        if let Some(peer_id) = peer_record.peer {
+                            let response_time = ctx.started_at.elapsed();
+                            self.record_peer_success(
+                                peer_id,
+                                Some(response_time.as_millis() as u64)
+                            ).await;
+                        }
+                    }
+                    Err(_) => {
+                        // Invalid package data - record peer failure
+                        if let Some(peer_id) = peer_record.peer {
+                            self.record_peer_failure(peer_id).await;
+                        }
+                    }
+                }
+            }
+
+            // Failed record retrieval
+            Event::OutboundQueryProgressed {
+                result: KadQueryResult::GetRecord(Err(_error)),
+                stats,
+                ..
+            } => {
+                // Extract peer ID from stats and record failure
+                if let Some((peer_id, _)) = stats.requests().next() {
+                    self.record_peer_failure(*peer_id).await;
+                }
+            }
+
+            _ => {}
+        }
+    }
+
+    /// Handle Gossipsub events (v2.4.0)
+    async fn handle_gossipsub_event(&self, _event: gossipsub::Event) {
+        // Can be implemented later for package announcements
+    }
+
+    /// Cleanup queries that have exceeded their timeout (v2.4.0)
+    async fn cleanup_expired_queries(&self) {
+        use std::sync::atomic::Ordering;
+
+        let now = std::time::Instant::now();
+        let mut queries = self.in_flight_queries.write().await;
+
+        // Grace period: timeout + 5 seconds for cleanup
+        let grace_period = std::time::Duration::from_secs(5);
+
+        queries.retain(|_query_id, ctx| {
+            let elapsed = now.duration_since(ctx.started_at);
+            let should_keep = elapsed < (ctx.timeout_duration + grace_period);
+
+            if !should_keep {
+                // Update metrics
+                self.query_metrics.in_flight_count.fetch_sub(1, Ordering::Relaxed);
+                self.query_metrics.timed_out_queries.fetch_add(1, Ordering::Relaxed);
+            }
+
+            should_keep
+        });
+    }
+
+    /// Spawn background event processing loop (v2.4.0)
+    pub fn spawn_event_loop(self: Arc<Self>) -> tokio::task::JoinHandle<()> {
+        tokio::spawn(async move {
+            loop {
+                // Process events
+                self.process_events().await;
+
+                // Small delay to prevent busy-waiting
+                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
+            }
+        })
+    }
+
 #[async_trait]
```

### Change 8: Replace Registry::search() implementation (replace lines 559-656)

```diff
 impl Registry for P2PRegistry {
     #[instrument(skip(self, query), fields(query = %query.text, limit = query.limit))]
     async fn search(&self, query: &Query) -> Result<Vec<Package>> {
-        // Check cache first (v2.4.0 multi-tier cache)
-        let cache_key = format!("search:{}:{}:{}",
-            query.text,
-            query.categories.len(),
-            query.tags.len()
-        );
-
         // Search locally first
         let local_packages = self.local_packages.read().await;
-        let mut results: Vec<Package> = local_packages
+        let local_results: Vec<Package> = local_packages
             .values()
-            .filter(|package| {
-                // Simple text matching
-                let text_match = query.text.is_empty()
-                    || package.metadata.title.to_lowercase().contains(&query.text.to_lowercase())
-                    || package
-                        .metadata
-                        .description
-                        .to_lowercase()
-                        .contains(&query.text.to_lowercase());
-
-                // Category filtering
-                let category_match = query.categories.is_empty()
-                    || package
-                        .metadata
-                        .categories
-                        .iter()
-                        .any(|c| query.categories.contains(c));
-
-                // Tag filtering
-                let tag_match = query.tags.is_empty()
-                    || package
-                        .metadata
-                        .tags
-                        .iter()
-                        .any(|t| query.tags.contains(t));
-
-                text_match && category_match && tag_match
-            })
+            .filter(|package| self.matches_query(package, query))
             .cloned()
             .collect();
+        drop(local_packages); // Release lock

-        // Query DHT for additional results using parallel fan-out (v2.4.0)
-        let discovered = self.discovered_packages.read().await;
-        let my_location = self.get_location().await;
-
-        // Use adaptive peer selection for best results
-        let best_peers = self.select_best_peers(0.5, 10, my_location.as_ref()).await;
-
-        // Parallel queries to multiple peers (fan-out = 3 by default)
-        let fan_out = (best_peers.len().min(3)).max(1);
-
-        for package_id in discovered.keys() {
-            // Use parallel DHT query with fan-out strategy
-            if let Ok(Some(package)) = self.query_dht_parallel(package_id, fan_out).await {
-                // Apply same filters as local search
-                let text_match = query.text.is_empty()
-                    || package.metadata.title.to_lowercase().contains(&query.text.to_lowercase())
-                    || package
-                        .metadata
-                        .description
-                        .to_lowercase()
-                        .contains(&query.text.to_lowercase());
-
-                let category_match = query.categories.is_empty()
-                    || package
-                        .metadata
-                        .categories
-                        .iter()
-                        .any(|c| query.categories.contains(c));
-
-                let tag_match = query.tags.is_empty()
-                    || package
-                        .metadata
-                        .tags
-                        .iter()
-                        .any(|t| query.tags.contains(t));
-
-                if text_match && category_match && tag_match {
-                    // Avoid duplicates
-                    if !results.iter().any(|p| p.id == package.id) {
-                        results.push(package);
-                    }
-                }
-            }
+        // Query DHT for additional results using async collection (v2.4.0)
+        let dht_results = self.search_dht_async(query).await?;
+
+        // Merge and deduplicate results
+        let mut results = local_results;
+        let mut seen_ids: HashSet<PackageId> = results.iter().map(|p| p.id.clone()).collect();
+        for package in dht_results {
+            if seen_ids.insert(package.id.clone()) {
+                results.push(package);
+            }
         }

         // Apply limit if specified
         if let Some(limit) = query.limit {
             results.truncate(limit);
         }

         Span::current().record("result_count", results.len());
-
         Ok(results)
     }

     // ... other Registry methods remain unchanged ...
 }
```

### Change 9: Add helper methods (after Registry implementation)

```diff
 #[async_trait]
 impl Registry for P2PRegistry {
     // ... existing methods ...
 }
+
+// Private helper methods for P2PRegistry
+impl P2PRegistry {
+    /// Check if package matches query criteria (v2.4.0)
+    fn matches_query(&self, package: &Package, query: &Query) -> bool {
+        // Text matching
+        let text_match = query.text.is_empty()
+            || package.metadata.title.to_lowercase().contains(&query.text.to_lowercase())
+            || package.metadata.description.to_lowercase().contains(&query.text.to_lowercase());
+
+        // Category filtering
+        let category_match = query.categories.is_empty()
+            || package.metadata.categories.iter().any(|c| query.categories.contains(c));
+
+        // Tag filtering
+        let tag_match = query.tags.is_empty()
+            || package.metadata.tags.iter().any(|t| query.tags.contains(t));
+
+        text_match && category_match && tag_match
+    }
+
+    /// Search DHT with async result collection (v2.4.0)
+    #[instrument(skip(self))]
+    async fn search_dht_async(&self, query: &Query) -> Result<Vec<Package>> {
+        use std::sync::atomic::Ordering;
+
+        // Generate unique query ID
+        let query_id = QueryId::new();
+
+        // Create result channel
+        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
+
+        // Get configuration
+        let timeout = self.query_config.default_timeout;
+        let max_results = self.query_config.max_results;
+
+        // Register in-flight query
+        let query_context = QueryContext {
+            query_id,
+            result_sender: tx,
+            query_params: query.clone(),
+            started_at: std::time::Instant::now(),
+            timeout_duration: timeout,
+            peers_queried: HashSet::new(),
+            results_received: 0,
+        };
+
+        self.in_flight_queries.write().await.insert(query_id, query_context);
+
+        // Update metrics
+        self.query_metrics.total_queries.fetch_add(1, Ordering::Relaxed);
+        self.query_metrics.in_flight_count.fetch_add(1, Ordering::Relaxed);
+
+        // Start DHT queries for discovered packages
+        let discovered = self.discovered_packages.read().await;
+        let package_ids: Vec<PackageId> = discovered.keys().cloned().collect();
+        drop(discovered);
+
+        for package_id in package_ids {
+            let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
+            let mut swarm = self.swarm.write().await;
+            swarm.behaviour_mut().kademlia.get_record(key);
+        }
+
+        // Create query handle
+        let handle = QueryHandle {
+            query_id,
+            result_receiver: rx,
+            max_results,
+        };
+
+        // Await results with timeout
+        let start = std::time::Instant::now();
+        let results = match tokio::time::timeout(timeout, handle.collect_results()).await {
+            Ok(Ok(packages)) => {
+                self.query_metrics.successful_queries.fetch_add(1, Ordering::Relaxed);
+                packages
+            }
+            Ok(Err(e)) => {
+                return Err(e);
+            }
+            Err(_) => {
+                self.query_metrics.timed_out_queries.fetch_add(1, Ordering::Relaxed);
+                handle.get_partial_results()?
+            }
+        };
+
+        // Update metrics
+        let duration_ms = start.elapsed().as_millis() as u64;
+        self.query_metrics.avg_query_duration_ms.store(duration_ms, Ordering::Relaxed);
+        self.query_metrics.avg_results_per_query.store(results.len() as u64, Ordering::Relaxed);
+        self.query_metrics.in_flight_count.fetch_sub(1, Ordering::Relaxed);
+
+        // Remove from in-flight queries
+        self.in_flight_queries.write().await.remove(&query_id);
+
+        // Filter results by query criteria
+        Ok(results.into_iter().filter(|p| self.matches_query(p, query)).collect())
+    }
+}

 #[cfg(test)]
 mod tests {
```

## Summary of Changes

### Files Modified
1. **`ggen-marketplace/src/backend/p2p.rs`** - Core implementation
   - Added 9 sections of changes
   - ~300 lines added
   - ~80 lines modified

### Files Created
1. **`ggen-marketplace/src/backend/async_query_architecture.rs`** - Architecture module
2. **`docs/async-query-sequence-diagram.md`** - Visual documentation
3. **`docs/async-query-implementation-guide.md`** - Step-by-step guide
4. **`docs/async-query-architecture-summary.md`** - Executive summary

### Breaking Changes
None - All changes are backward compatible. Existing code continues to work.

### New Public API
```rust
// Configuration
impl P2PRegistry {
    pub fn with_query_config(self, config: QueryConfig) -> Self;
    pub fn query_metrics(&self) -> Arc<QueryMetrics>;
    pub fn spawn_event_loop(self: Arc<Self>) -> tokio::task::JoinHandle<()>;
}

// Types
pub struct QueryConfig { ... }
pub struct QueryMetrics { ... }
```

### Migration Steps
1. Add the new `async_query_architecture.rs` file
2. Apply all 9 diffs to `p2p.rs` in order
3. Update usage code to spawn event loop:
   ```rust
   let registry = Arc::new(P2PRegistry::new(config).await?);
   let _event_loop = registry.clone().spawn_event_loop();
   ```
4. Run tests to verify functionality
5. Monitor metrics in production

### Testing Checklist
- [ ] Unit tests pass: `cargo test --lib p2p`
- [ ] Integration tests pass: `cargo test --test '*p2p*'`
- [ ] Benchmarks run: `cargo bench marketplace`
- [ ] No memory leaks (valgrind or similar)
- [ ] Metrics are updated correctly
- [ ] Event loop processes events
- [ ] Queries return results from DHT
- [ ] Timeout handling works
