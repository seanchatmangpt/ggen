<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Advanced Caching Strategies with TAI-Cache](#advanced-caching-strategies-with-tai-cache)
  - [Overview](#overview)
    - [Core Components](#core-components)
  - [Architecture](#architecture)
  - [Caching Strategies](#caching-strategies)
    - [1. Time-Based Invalidation (TTL)](#1-time-based-invalidation-ttl)
    - [2. Event-Based Invalidation](#2-event-based-invalidation)
    - [3. Dependency-Based Invalidation](#3-dependency-based-invalidation)
    - [4. Cascading Invalidation](#4-cascading-invalidation)
  - [Cache Patterns](#cache-patterns)
    - [1. Write-Through Caching](#1-write-through-caching)
    - [2. Write-Behind Caching (Write-Back)](#2-write-behind-caching-write-back)
    - [3. Read-Through Caching](#3-read-through-caching)
    - [4. Lazy-Load Caching](#4-lazy-load-caching)
  - [Multi-Tier Caching Architecture](#multi-tier-caching-architecture)
    - [Three-Tier Cache Hierarchy](#three-tier-cache-hierarchy)
    - [Cache Hit Flow](#cache-hit-flow)
    - [Cache Warming](#cache-warming)
  - [Cache Coherency in Distributed Systems](#cache-coherency-in-distributed-systems)
    - [Multi-Instance Cache Consistency](#multi-instance-cache-consistency)
  - [Metrics & Monitoring](#metrics--monitoring)
    - [Key Metrics](#key-metrics)
    - [Collecting Metrics](#collecting-metrics)
    - [SLO Monitoring](#slo-monitoring)
  - [Redis vs Memcached](#redis-vs-memcached)
    - [Redis](#redis)
    - [Memcached](#memcached)
    - [Comparison Table](#comparison-table)
  - [Best Practices](#best-practices)
    - [1. Cache Key Design](#1-cache-key-design)
    - [2. TTL Strategy](#2-ttl-strategy)
    - [3. Cache Size Management](#3-cache-size-management)
    - [4. Monitoring Strategy](#4-monitoring-strategy)
    - [5. Failure Handling](#5-failure-handling)
    - [6. Avoid Cache Stampede](#6-avoid-cache-stampede)
  - [Common Patterns](#common-patterns)
    - [Pattern 1: Cache-Aside (Lazy Loading)](#pattern-1-cache-aside-lazy-loading)
    - [Pattern 2: Write-Through](#pattern-2-write-through)
    - [Pattern 3: Cache Invalidation on Write](#pattern-3-cache-invalidation-on-write)
  - [Performance Tuning](#performance-tuning)
    - [1. Connection Pool Sizing](#1-connection-pool-sizing)
    - [2. Batch Operations](#2-batch-operations)
    - [3. Compression for Large Values](#3-compression-for-large-values)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Advanced Caching Strategies with TAI-Cache

## Overview

TAI-Cache is an enterprise-grade caching library that provides multiple caching backends, advanced invalidation patterns, distributed caching strategies, and comprehensive metrics collection. It enables building high-performance systems with intelligent cache management.

### Core Components

- **Redis Cache**: High-performance distributed cache with async operations and connection pooling
- **Memcached Cache**: Simple, fast key-value caching for stateless systems
- **Multi-Tier Caching**: Combination of in-memory (L1), Redis (L2), and persistent storage (L3)
- **Cache Invalidation**: Time-based, event-based, dependency tracking, and cascading patterns
- **Distributed Caching**: Write-through, write-behind, and read-through strategies
- **Metrics & Monitoring**: Hit/miss rates, latency tracking, SLO enforcement

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  Application Layer                           │
└─────────────────────────────────────────────────────────────┘
                            │
         ┌──────────────────┼──────────────────┐
         │                  │                  │
    ┌────▼─────┐       ┌────▼─────┐      ┌────▼─────┐
    │ L1 Cache │       │ L2 Cache │      │ L3 Cache │
    │(In-Memory)       │ (Redis)  │      │(Storage) │
    │                  │          │      │          │
    └────┬─────┘       └────┬─────┘      └────┬─────┘
         │                  │                  │
         └──────────────────┼──────────────────┘
                            │
        ┌───────────────────┴───────────────────┐
        │                                       │
    ┌───▼──────────────┐          ┌────▼──────────────┐
    │ Invalidation     │          │ Metrics           │
    │ Manager          │          │ Collector         │
    └────────────────────┘         └──────────────────┘
```

## Caching Strategies

### 1. Time-Based Invalidation (TTL)

The simplest and most common invalidation strategy. Cache entries expire after a fixed duration.

**Use Cases:**
- Session data (15-30 minutes TTL)
- User preferences (1 hour TTL)
- API responses (5-15 minutes TTL)
- Product catalogs (24 hours TTL)

**Implementation:**
```rust
use tai_cache::RedisCache;
use std::time::Duration;

let cache = RedisCache::new("redis://127.0.0.1:6379").await?;

// Cache with 1 hour TTL
cache.set("user:123:profile", &user_profile, Some(Duration::from_secs(3600))).await?;

// Check TTL remaining
let ttl = cache.ttl("user:123:profile").await?;
if ttl > 0 {
    println!("Cache expires in {} seconds", ttl);
}
```

**Advantages:**
- Simple to implement
- Predictable memory usage
- No manual invalidation needed

**Disadvantages:**
- Stale data may be served
- Requires tuning TTL values
- No event-driven updates

### 2. Event-Based Invalidation

Cache entries are invalidated when underlying data changes.

**Use Cases:**
- Real-time data (user updates)
- Database changes
- Configuration updates
- Derived data (aggregations)

**Implementation:**
```rust
use tai_cache::{InvalidationManager, InvalidationStrategy};

let manager = InvalidationManager::new();

// Register cache with event-based strategy
manager.register("user:123:profile", InvalidationStrategy::EventBased);

// When user updates profile
let invalidated = manager.invalidate("user:123:profile");
println!("Invalidated {} keys", invalidated.len());
```

**Advantages:**
- No stale data
- Immediate consistency
- Precise invalidation

**Disadvantages:**
- Requires event infrastructure
- Complex implementation
- Higher operational overhead

### 3. Dependency-Based Invalidation

Cache entries track dependencies on other cache entries. When a dependency changes, dependents are automatically invalidated.

**Use Cases:**
- Derived data (aggregations from base data)
- Composite objects
- Multi-level hierarchies

**Implementation:**
```rust
use tai_cache::DependencyGraph;

let graph = DependencyGraph::new();

// Define dependencies
// User profile page depends on user data and settings
graph.add_dependency("page:user:123", "user:123:data");
graph.add_dependency("page:user:123", "user:123:settings");

// When user data changes
let cascade_targets = graph.get_cascade_targets("user:123:data");
// cascade_targets = ["page:user:123"]

// Invalidate all affected caches
for key in cascade_targets {
    cache.delete(&key).await?;
}
```

**Advantages:**
- Automatic cascade invalidation
- Maintains consistency automatically
- Clear dependency relationships

**Disadvantages:**
- More complex to set up
- Graph traversal overhead
- Requires careful design

### 4. Cascading Invalidation

Invalidating one key automatically invalidates all dependent keys.

**Use Cases:**
- Multi-level cache hierarchies
- Derived metrics from base metrics
- Complex object graphs

**Example Dependency Chain:**
```
user:1:data (base)
    ↓
user:1:profile (depends on data)
    ↓
page:dashboard (depends on profile)
```

Invalidating `user:1:data` cascades to `user:1:profile` and `page:dashboard`.

**Implementation:**
```rust
let graph = DependencyGraph::new();

graph.add_dependency("user:1:profile", "user:1:data");
graph.add_dependency("page:dashboard", "user:1:profile");

// Single invalidation cascades through chain
let all_affected = graph.get_cascade_targets("user:1:data");
// Returns: ["user:1:profile", "page:dashboard"]
```

## Cache Patterns

### 1. Write-Through Caching

Data is written to all cache tiers synchronously before returning to the caller.

```
Write Request
    ↓
    ├─→ Update L1 (In-Memory)
    ├─→ Update L2 (Redis)
    ├─→ Update L3 (Persistent Storage)
    ↓
Return Success
```

**Advantages:**
- Strong consistency guarantee
- Read always gets latest data
- Simple to understand

**Disadvantages:**
- Slow write operations (wait for all tiers)
- Blocking operations
- Higher latency

**Implementation:**
```rust
use tai_cache::{MultiTierCache, WriteStrategy, ReadStrategy};

let cache = MultiTierCache::new(
    WriteStrategy::WriteThrough,
    ReadStrategy::ReadThrough,
);

// Write synchronously to all tiers
cache.set_tiered("key", &value, ttl).await?;
```

### 2. Write-Behind Caching (Write-Back)

Data is written to cache immediately; background process flushes to persistent storage.

```
Write Request
    ↓
    └─→ Update Cache (return immediately)
            ↓
        Background Flush to Storage
```

**Advantages:**
- Fast write operations
- Non-blocking
- Reduced latency
- Batch writes to storage

**Disadvantages:**
- Eventual consistency
- Data loss risk if cache fails
- Complex flush logic

**Implementation:**
```rust
let cache = MultiTierCache::new(
    WriteStrategy::WriteBehind {
        flush_interval: Duration::from_secs(60)
    },
    ReadStrategy::ReadThrough,
);

// Write returns immediately
cache.set_tiered("key", &value, None).await?;

// Background flush happens periodically
let flushed = cache.flush_pending().await?;
println!("Flushed {} pending writes", flushed);
```

### 3. Read-Through Caching

Application reads from cache; on miss, cache reads from source and populates itself.

```
Read Request
    ↓
Check L1 (In-Memory) ──→ HIT: Return
    ↓ (MISS)
Check L2 (Redis) ──→ HIT: Populate L1, Return
    ↓ (MISS)
Check L3 (Storage) ──→ HIT: Populate L1/L2, Return
    ↓ (MISS)
Application Logic ──→ Populate all tiers, Return
```

**Advantages:**
- Automatic cache population
- Transparent caching
- Simplest application code

**Disadvantages:**
- First request is slow (cold start)
- Application needs cache-aware logic
- Harder to control what's cached

### 4. Lazy-Load Caching

Application is responsible for checking cache and populating on miss.

```
Application: Check Cache
    ↓
HIT: Use value
MISS: Load from source, Update cache, Use value
```

## Multi-Tier Caching Architecture

### Three-Tier Cache Hierarchy

```
┌──────────────────────────┐
│ L1: In-Memory Cache      │
│ - Fastest                │
│ - Smallest (MB-scale)    │
│ - Single instance only   │
├──────────────────────────┤
│ L2: Redis (Distributed)  │
│ - Medium speed           │
│ - Medium size (GB-scale) │
│ - All instances share    │
├──────────────────────────┤
│ L3: Persistent Storage   │
│ - Slowest                │
│ - Largest (TB-scale)     │
│ - Durable                │
└──────────────────────────┘
```

### Cache Hit Flow

1. **L1 Hit** (99% of requests): Return in <1µs
2. **L1 Miss, L2 Hit** (0.9% of requests): Return from Redis (~5-10µs)
3. **L1/L2 Miss, L3 Hit** (0.09% of requests): Load from storage (~100-500µs)
4. **Complete Miss** (0.01% of requests): Compute fresh (~1-10ms+)

### Cache Warming

Pre-load hot data at startup to avoid cold start penalties.

```rust
let warmer = CacheWarmer::new();

// Add hot data
warmer.add_data("config:app".into(), serde_json::json!({
    "timeout": 30,
    "retries": 3,
})).await;

warmer.add_data("product:top-10".into(), serde_json::json!({
    "id": "123",
    "name": "Popular Product",
})).await;

// Warm cache at startup
let data = warmer.get_data().await;
let count = cache.warm_cache(data).await?;
println!("Pre-loaded {} hot entries", count);
```

## Cache Coherency in Distributed Systems

### Multi-Instance Cache Consistency

Keeping caches synchronized across multiple application instances.

```
Instance A          Instance B          Instance C
  L1 Cache           L1 Cache             L1 Cache
    ↓                  ↓                    ↓
  Redis (Shared L2 Cache)
    ↓
Invalidation Broadcast
```

**Implementation:**
```rust
use tai_cache::CacheCoherence;

let coherence = CacheCoherence::new();

// Update on Instance A
coherence.update_local_version().await;
let version = coherence.get_local_version().await;

// Broadcast to Instance B and C
broadcaster.broadcast("cache:updates", &format!("version:{}", version))?;

// Instance B receives broadcast
let receiver = broadcaster.subscribe("cache:updates").await;
if let Ok(msg) = tokio::time::timeout(
    Duration::from_secs(1),
    receiver.recv()
).await {
    if let Ok(version_update) = msg {
        coherence.update_remote_version("instance-b", version);
    }
}
```

## Metrics & Monitoring

### Key Metrics

1. **Hit Rate** = Hits / Total Requests
   - Target: ≥80%
   - Indicates cache effectiveness

2. **Miss Rate** = Misses / Total Requests
   - Target: ≤20%
   - High misses indicate poor cache strategy

3. **Eviction Rate** = Evictions / Total Requests
   - Target: ≤5%
   - High evictions indicate cache too small

4. **Hit Latency** = Time for cache hit (avg)
   - Target: ≤100µs
   - Should be <1% of miss latency

5. **Miss Latency** = Time for cache miss (avg)
   - Target: ≤1000µs
   - Includes source fetch time

### Collecting Metrics

```rust
use tai_cache::MetricsCollector;
use std::time::Instant;

let metrics = MetricsCollector::new();

// Record cache hit with latency
let start = Instant::now();
let value = cache.get("key").await?;
let latency_us = start.elapsed().as_micros() as u64;

if value.is_some() {
    metrics.record_hit(latency_us);
} else {
    // Fetch from source
    let start = Instant::now();
    let fresh = fetch_from_source().await?;
    let miss_latency_us = start.elapsed().as_micros() as u64;

    metrics.record_miss(miss_latency_us);
    cache.set("key", &fresh, Some(Duration::from_secs(3600))).await?;
}

// Get snapshot
let snapshot = metrics.snapshot();
println!("Hit Rate: {:.2}%", snapshot.hit_rate());
println!("Avg Hit Latency: {}µs", snapshot.avg_hit_latency_us);
println!("Avg Miss Latency: {}µs", snapshot.avg_miss_latency_us);
```

### SLO Monitoring

```rust
use tai_cache::SloChecker;

let slo = SloChecker::new(
    80.0,    // min hit rate %
    100,     // max hit latency µs
    1000,    // max miss latency µs
    5.0,     // max eviction rate %
);

let snapshot = metrics.snapshot();
let result = slo.check(&snapshot);

if result.passed {
    println!("✓ All SLOs met");
} else {
    for violation in result.violations {
        println!("✗ SLO violation: {}", violation);
    }
}
```

## Redis vs Memcached

### Redis

**Strengths:**
- Complex data structures (lists, sets, sorted sets, hashes)
- Built-in expiration (TTL)
- Persistence options
- Pub/Sub for cache invalidation
- Single-threaded predictable performance
- Rich command set

**Best For:**
- Complex caching needs
- Session management
- Leaderboards/rankings
- Rate limiting
- Message queues
- Event streaming

**Performance:**
- ~5-10µs latency for simple ops
- Throughput: 100k-1M ops/sec

### Memcached

**Strengths:**
- Simple key-value only
- Ultra-fast (single-threaded, minimal overhead)
- Memory-efficient
- No persistence (stateless)
- Well-understood and reliable

**Best For:**
- Simple string caching
- Stateless systems
- API response caching
- Database query result caching
- High-throughput, low-latency systems

**Performance:**
- ~1-5µs latency
- Throughput: 1M-10M ops/sec

### Comparison Table

| Feature | Redis | Memcached |
|---------|-------|-----------|
| Data Types | Complex | Strings only |
| Persistence | Optional | None |
| Replication | Yes | No |
| Cluster | Yes | No |
| TTL | Built-in | Built-in |
| Latency | ~5-10µs | ~1-5µs |
| Memory Overhead | ~10% | ~5% |
| Learning Curve | Medium | Low |
| Transactions | Yes | No |
| Pub/Sub | Yes | No |

## Best Practices

### 1. Cache Key Design

Use hierarchical, descriptive keys:
```
Good:
- user:123:profile
- user:123:settings
- product:456:details
- product:456:inventory

Bad:
- u123p
- user-profile-123
- random-key-xyz
```

### 2. TTL Strategy

- **Short-lived data** (1-5 min): User input, search results
- **Medium-lived** (15-60 min): User preferences, API responses
- **Long-lived** (1-24 hours): Product catalogs, configuration
- **Very long-lived** (7+ days): Reference data, lookup tables

### 3. Cache Size Management

- **L1 (In-Memory)**: 100MB-1GB (single instance)
- **L2 (Redis)**: 5-50GB (distributed)
- **L3 (Storage)**: Unlimited

### 4. Monitoring Strategy

- Monitor hit rate (target: >80%)
- Track eviction rate (target: <5%)
- Measure latency (target: <100µs for hits)
- Alert on SLO violations

### 5. Failure Handling

Always have fallback to source on cache miss:
```rust
async fn get_user(id: u64) -> Result<User> {
    // Try cache
    if let Ok(Some(user)) = cache.get(&format!("user:{}", id)).await {
        return Ok(user);
    }

    // Fallback to database
    let user = db.get_user(id).await?;

    // Update cache (fire and forget)
    let _ = cache.set(&format!("user:{}", id), &user, Some(Duration::from_secs(3600))).await;

    Ok(user)
}
```

### 6. Avoid Cache Stampede

Use probabilistic early expiration (TTL jitter):
```rust
let base_ttl = Duration::from_secs(3600); // 1 hour
let jitter = Duration::from_secs(rand::random::<u64>() % 300); // 0-5 min
let actual_ttl = base_ttl + jitter;

cache.set("key", &value, Some(actual_ttl)).await?;
```

## Common Patterns

### Pattern 1: Cache-Aside (Lazy Loading)

```rust
async fn get_user(id: u64) -> Result<User> {
    let key = format!("user:{}", id);

    // Check cache
    match cache.get(&key).await? {
        Some(user) => return Ok(user),
        None => {}
    }

    // Load from source
    let user = db.get_user(id).await?;

    // Update cache
    cache.set(&key, &user, Some(Duration::from_secs(3600))).await?;

    Ok(user)
}
```

### Pattern 2: Write-Through

```rust
async fn update_user(id: u64, user: &User) -> Result<()> {
    let key = format!("user:{}", id);

    // Update database
    db.update_user(id, user).await?;

    // Update cache
    cache.set(&key, user, Some(Duration::from_secs(3600))).await?;

    Ok(())
}
```

### Pattern 3: Cache Invalidation on Write

```rust
async fn delete_user(id: u64) -> Result<()> {
    let key = format!("user:{}", id);

    // Delete from database
    db.delete_user(id).await?;

    // Invalidate cache
    cache.delete(&key).await?;

    // Cascade invalidation for dependent caches
    cache.delete(&format!("page:user:{}", id)).await?;
    cache.delete(&format!("user:{}:profile", id)).await?;

    Ok(())
}
```

## Performance Tuning

### 1. Connection Pool Sizing

```rust
// For high throughput: 2-4x number of cores
let pool_size = num_cpus::get() * 4;
let cache = RedisCache::with_pool_size("redis://...", pool_size).await?;
```

### 2. Batch Operations

```rust
// Instead of:
for key in keys {
    cache.delete(key).await?;
}

// Use:
cache.delete_many(&keys).await?;
```

### 3. Compression for Large Values

```rust
// For large objects, compress before caching
let compressed = compress(&large_object)?;
cache.set("large_key", &compressed, ttl).await?;

// On retrieval, decompress
let compressed: Bytes = cache.get("large_key").await?;
let original = decompress(compressed)?;
```

## Conclusion

TAI-Cache provides a complete caching solution with:
- Multiple backends (Redis, Memcached, in-memory)
- Advanced invalidation strategies
- Multi-tier caching for optimal performance
- Comprehensive metrics and SLO monitoring
- Production-ready error handling and resilience

Use these patterns and strategies to build fast, scalable systems that maintain consistency and reliability under load.
