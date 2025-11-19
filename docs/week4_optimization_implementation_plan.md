# Week 4 Performance Optimization Implementation Plan

## Executive Summary

**Objective:** Refine Week 3 optimizations to achieve A+ grade (92+/100)
**Current State:** A- (88/100)
**Target:** A+ (92/100) - requires +4 points
**Timeline:** 5 days (Days 1-5 of Week 4)

---

## Optimization 1: Lockfile Dependency Resolution

### Current Implementation

**File:** `crates/ggen-core/src/lockfile.rs`

**Week 3 Optimizations:**
- ✅ Parallel manifest loading with Rayon (lines 431-447)
- ✅ LRU cache (1000 entries) for dependencies (lines 98-99)
- ✅ Single-pack fast path (lines 419-423)

**Performance:**
- Single pack: ~20-30ms (current)
- 10 packs: ~100-150ms (current)
- Cache hit rate: ~70-75%

### Week 4 Refinements

#### 1.1 Connection Pooling for Manifest Downloads

**Goal:** Reduce network latency by reusing HTTP connections

**Implementation:**
```rust
// Add to lockfile.rs
use reqwest::Client;
use std::sync::Arc;

pub struct LockfileManager {
    // ... existing fields ...
    http_client: Arc<Client>,
}

impl LockfileManager {
    pub fn new(base_dir: impl AsRef<Path>) -> Self {
        let http_client = Arc::new(
            Client::builder()
                .pool_max_idle_per_host(10)
                .pool_idle_timeout(Duration::from_secs(30))
                .build()
                .unwrap()
        );

        Self {
            // ... existing initialization ...
            http_client,
        }
    }

    fn fetch_manifest(&self, url: &str) -> Result<Manifest> {
        // Use self.http_client instead of creating new client each time
        self.http_client.get(url).send()?.json()
    }
}
```

**Expected Improvement:** 20-30% reduction in network overhead

#### 1.2 Manifest Pre-Fetching (Parallel Loading)

**Goal:** Start downloading manifests before they're needed

**Implementation:**
```rust
use futures::future::join_all;
use tokio::runtime::Runtime;

impl LockfileManager {
    pub async fn prefetch_manifests(&self, pack_ids: &[String]) -> Result<HashMap<String, Manifest>> {
        let futures: Vec<_> = pack_ids
            .iter()
            .map(|id| self.fetch_manifest_async(id))
            .collect();

        let results = join_all(futures).await;

        // Convert results to HashMap
        results.into_iter()
            .filter_map(|r| r.ok())
            .collect()
    }

    pub fn upsert_bulk_optimized(&self, packs: &[(String, String, String, String)]) -> Result<()> {
        // Pre-fetch all manifests in parallel
        let pack_ids: Vec<_> = packs.iter().map(|(id, _, _, _)| id.clone()).collect();
        let manifests = Runtime::new()?.block_on(self.prefetch_manifests(&pack_ids))?;

        // Now process with manifests already cached
        // ... rest of upsert_bulk logic ...
    }
}
```

**Expected Improvement:** 40-60% faster for bulk operations

#### 1.3 Cache Key Optimization

**Goal:** Reduce string allocations in cache key generation

**Implementation:**
```rust
use ahash::AHashMap;

// Replace HashMap with AHashMap for faster hashing
type DepCache = Arc<Mutex<LruCache<u64, Option<Vec<String>>>>>;

impl LockfileManager {
    fn cache_key(pack_id: &str, version: &str) -> u64 {
        // Use ahash for fast hashing, avoid string allocation
        use std::hash::{Hash, Hasher};
        use ahash::AHasher;

        let mut hasher = AHasher::default();
        pack_id.hash(&mut hasher);
        version.hash(&mut hasher);
        hasher.finish()
    }

    fn resolve_dependencies(&self, pack_id: &str, version: &str, source: &str) -> Result<Vec<String>> {
        let key = Self::cache_key(pack_id, version);

        // Check cache
        {
            let mut cache = self.dep_cache.lock().unwrap();
            if let Some(cached) = cache.get(&key) {
                return Ok(cached.clone().unwrap_or_default());
            }
        }

        // ... rest of resolution logic ...
    }
}
```

**Expected Improvement:** 10-15% reduction in allocation overhead

### Performance Targets

| Metric | Current | Target | Strategy |
|--------|---------|--------|----------|
| Single pack | 20-30ms | <10ms | Connection pooling + fast path |
| 10 packs | 100-150ms | <50ms | Pre-fetching + parallel loading |
| Cache hit rate | 70-75% | >85% | Better cache key design |

---

## Optimization 2: RDF Query Optimization

### Current Implementation

**File:** `crates/ggen-core/src/rdf/query.rs`

**Week 3 Optimizations:**
- ✅ Query result caching with version tracking (lines 96-127)
- ✅ Predicate indexing for pattern matching (lines 181-208)
- ✅ Automatic cache invalidation (lines 158-163)

**Performance:**
- Cold queries: ~10-20ms
- Cached queries: ~5-8ms (needs improvement)
- Cache hit rate: ~60-70%

### Week 4 Refinements

#### 2.1 Cache Size Tuning

**Goal:** Find optimal cache size based on memory usage

**Implementation:**
```rust
use std::sync::atomic::{AtomicUsize, Ordering};

impl QueryCache {
    /// Auto-tune cache capacity based on hit rate and memory
    pub fn auto_tune(&self) -> Result<usize> {
        let stats = self.stats();
        let hit_rate = stats.cache_size as f64 / stats.cache_capacity as f64;

        let new_capacity = if hit_rate > 0.9 {
            // High hit rate, increase capacity
            (stats.cache_capacity as f64 * 1.5) as usize
        } else if hit_rate < 0.5 {
            // Low hit rate, decrease capacity
            (stats.cache_capacity as f64 * 0.75) as usize
        } else {
            stats.cache_capacity
        };

        // Apply new capacity
        let mut cache = self.cache.lock().unwrap();
        cache.resize(NonZeroUsize::new(new_capacity).unwrap());

        Ok(new_capacity)
    }
}
```

**Expected Improvement:** 15-25% better cache efficiency

#### 2.2 Optimized LRU Eviction Policy

**Goal:** Keep hot queries in cache longer

**Implementation:**
```rust
use std::time::{Instant, Duration};

#[derive(Debug, Clone)]
struct CachedResult {
    data: String,
    version: u64,
    access_count: AtomicUsize,
    last_access: Instant,
}

impl QueryCache {
    /// Custom eviction policy: evict based on access count + age
    pub fn evict_smart(&self) {
        let mut cache = self.cache.lock().unwrap();

        // Find least valuable entry (low access count + old)
        let mut to_evict = None;
        let mut lowest_score = f64::MAX;

        for (key, result) in cache.iter() {
            let age = result.last_access.elapsed().as_secs() as f64;
            let access_count = result.access_count.load(Ordering::Relaxed) as f64;

            // Score: lower is worse (old + rarely accessed)
            let score = access_count / (age + 1.0);

            if score < lowest_score {
                lowest_score = score;
                to_evict = Some(key.clone());
            }
        }

        if let Some(key) = to_evict {
            cache.pop(&key);
        }
    }
}
```

**Expected Improvement:** 20-30% better retention of hot queries

#### 2.3 Predicate Index Building Optimization

**Goal:** Faster index building with parallel processing

**Implementation:**
```rust
use rayon::prelude::*;

impl QueryCache {
    pub fn build_predicate_index_parallel(&self, store: &Store, predicates: &[&str]) -> Result<()> {
        // Build indexes in parallel
        let results: Result<Vec<_>> = predicates
            .par_iter()
            .map(|predicate| {
                let query = format!(
                    "SELECT ?s ?o WHERE {{ ?s <{}> ?o }}",
                    predicate
                );
                let results = self.execute_query(store, &query)?;
                Ok((*predicate, results))
            })
            .collect();

        let indexes = results?;

        // Update index atomically
        let mut index = self.predicate_index.lock().unwrap();
        for (predicate, results) in indexes {
            let parsed: Vec<HashMap<String, String>> = serde_json::from_str(&results)?;
            let entries: Vec<(String, String)> = parsed
                .into_iter()
                .filter_map(|mut row| {
                    let s = row.remove("s")?;
                    let o = row.remove("o")?;
                    Some((s, o))
                })
                .collect();
            index.insert(predicate.to_string(), entries);
        }

        Ok(())
    }
}
```

**Expected Improvement:** 50-70% faster index building

### Performance Targets

| Metric | Current | Target | Strategy |
|--------|---------|--------|----------|
| Cached queries | 5-8ms | <1ms | Better eviction policy |
| Cache hit rate | 60-70% | >80% | Auto-tuning capacity |
| Index build time | ~50ms | <20ms | Parallel building |
| Memory overhead | ~8MB | <5MB | Optimized storage |

---

## Optimization 3: Template Processing

### Current Implementation

**File:** `crates/ggen-core/src/template_cache.rs`

**Week 3 Optimizations:**
- ✅ Frontmatter caching (YAML parsing) (lines 184-227)
- ✅ Tera template caching (lines 228-241)
- ✅ Thread-safe Arc sharing (lines 86-106)

**Performance:**
- Frontmatter parse: ~2-3ms (first time), ~0.5ms (cached)
- Tera compile: ~5-10ms (first time), ~1ms (cached)
- Memory per cache entry: ~2KB

### Week 4 Refinements

#### 3.1 Lazy Tera Engine Loading

**Goal:** Only create Tera engine when actually needed

**Implementation:**
```rust
use once_cell::sync::Lazy;

pub struct TemplateCache {
    // ... existing fields ...
    tera: Arc<Mutex<Option<tera::Tera>>>,
}

impl TemplateCache {
    pub fn get_tera(&self) -> Arc<Mutex<tera::Tera>> {
        let mut tera = self.tera.lock().unwrap();

        if tera.is_none() {
            // Lazy initialization only when first needed
            let mut t = tera::Tera::default();
            crate::register::register_all(&mut t);
            *tera = Some(t);
        }

        Arc::clone(&self.tera)
    }

    pub fn render_cached(&self, template_key: &str, context: &Context) -> Result<String> {
        // Get or lazy-load Tera
        let tera = self.get_tera();
        let tera = tera.lock().unwrap();

        // Render with cached template
        tera.as_ref()
            .unwrap()
            .render(template_key, context)
            .map_err(|e| Error::with_context("Template render failed", &e.to_string()))
    }
}
```

**Expected Improvement:** 15-25% faster startup, 10% memory reduction

#### 3.2 Reduced Arc Allocations

**Goal:** Minimize Arc cloning overhead

**Implementation:**
```rust
use parking_lot::RwLock; // Faster than std::sync::RwLock

pub struct TemplateCache {
    frontmatter: Arc<RwLock<LruCache<String, YamlValue>>>, // Use RwLock for read-heavy workload
    tera_templates: Arc<RwLock<LruCache<String, String>>>,
}

impl TemplateCache {
    pub fn get_or_parse_frontmatter(&self, content: &str, key: &str) -> Result<&YamlValue> {
        // Try read lock first (fast path)
        {
            let cache = self.frontmatter.read();
            if let Some(cached) = cache.peek(key) {
                return Ok(cached);
            }
        }

        // Write lock only if needed (slow path)
        let mut cache = self.frontmatter.write();

        // Double-check after acquiring write lock
        if let Some(cached) = cache.get(key) {
            return Ok(cached);
        }

        // Parse and cache
        let matter = gray_matter::Matter::<gray_matter::engine::YAML>::new();
        let parsed = matter.parse(content);
        let value = parsed.data.ok_or_else(|| Error::new("No frontmatter found"))?;

        cache.put(key.to_string(), value);
        Ok(cache.get(key).unwrap())
    }
}
```

**Expected Improvement:** 20-30% reduction in lock contention

#### 3.3 Memory Optimization

**Goal:** Reduce memory footprint per cache entry

**Implementation:**
```rust
use serde_yaml::Value as YamlValue;
use compact_str::CompactString; // Use CompactString for small strings

#[derive(Clone)]
struct CompactYamlValue {
    // Store common small strings inline
    to: CompactString,
    description: CompactString,
    variables: Vec<CompactString>,
}

impl TemplateCache {
    fn parse_frontmatter_compact(&self, content: &str) -> Result<CompactYamlValue> {
        let matter = gray_matter::Matter::<gray_matter::engine::YAML>::new();
        let parsed = matter.parse(content);

        let value = parsed.data.ok_or_else(|| Error::new("No frontmatter found"))?;

        // Extract only necessary fields, use CompactString
        let to = value.get("to")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .into();

        let description = value.get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .into();

        let variables = value.get("variables")
            .and_then(|v| v.as_sequence())
            .map(|seq| seq.iter()
                .filter_map(|v| v.as_str().map(|s| s.into()))
                .collect())
            .unwrap_or_default();

        Ok(CompactYamlValue {
            to,
            description,
            variables,
        })
    }
}
```

**Expected Improvement:** 20-30% memory reduction

### Performance Targets

| Metric | Current | Target | Strategy |
|--------|---------|--------|----------|
| Frontmatter caching | ~0.5ms | ~0.3ms | RwLock, reduced cloning |
| Tera caching | ~1ms | ~0.6ms | Lazy loading, optimization |
| Memory per entry | ~2KB | ~1.4KB | CompactString, selective fields |
| Cold startup | ~50ms | ~30ms | Lazy Tera initialization |

---

## Combined Realistic Workflow Benchmarks

### Scenario 1: Single Template Render
**Target:** <2ms (currently ~3-4ms)

### Scenario 2: Bulk Template Generation (100 templates)
**Target:** <200ms (currently ~300-400ms)

### Scenario 3: Lockfile Operations (5, 10, 20 packs)
**Target:** 5=<5ms, 10=<25ms, 20=<45ms

### Scenario 4: SPARQL Queries (Mixed repeated/new)
**Target:** 80% cache hit rate, <1ms cached, <10ms uncached

---

## Performance Grade Impact

### Current Grade: A- (88/100)

**Breakdown:**
- Compilation: 100% (30 pts)
- Testing: 50% (12.5 pts)
- Code Quality: 96% (14.4 pts)
- Security: 82% (12.3 pts)
- **Performance: 88% (8.8 pts)** ← Focus
- Architecture: 60% (3 pts)

### Target Grade: A+ (92/100)

**Required Improvements:**
- +4 points overall
- Performance: 88% → 92% (+0.4 pts on performance metric)
- Translates to: **20%+ improvement on critical paths**

**Critical Path Improvements:**
1. Lockfile operations: 50-80% faster → +0.15 pts
2. RDF queries (cached): 50-90% faster → +0.15 pts
3. Template processing: 20-40% faster → +0.10 pts
4. **Total: +0.40 pts → 92.4% (A+)**

---

## Implementation Timeline

### Day 1-2: Profiling & Benchmarking
- [x] Run comprehensive benchmarks on Week 3 code
- [ ] Measure current performance (establish baselines)
- [ ] Identify remaining bottlenecks
- [ ] Document before-state in performance report

### Day 3-4: Optimization & Tuning
- [ ] **Lockfile refinements:**
  - [ ] Connection pooling implementation
  - [ ] Manifest pre-fetching
  - [ ] Cache key optimization
- [ ] **RDF query tuning:**
  - [ ] Auto-tuning implementation
  - [ ] Smart eviction policy
  - [ ] Parallel index building
- [ ] **Template optimizations:**
  - [ ] Lazy Tera engine
  - [ ] RwLock migration
  - [ ] Memory optimization

### Day 5: Final Validation & Report
- [ ] Run final comprehensive benchmark
- [ ] Verify all operations meet targets
- [ ] Generate performance comparison report
- [ ] Update grade: A- (88) → A+ (92+)

---

## Success Criteria

- [ ] Lockfile: <10ms single, <50ms bulk (10 packs), >85% hit rate
- [ ] RDF: <1ms cached, >80% hit rate, <5MB memory
- [ ] Templates: 20-40% faster, 20% memory reduction
- [ ] Overall grade: A+ (92+/100)
- [ ] All benchmarks automated and reproducible
- [ ] Performance gains verified with before/after metrics

---

## Monitoring & Validation

### Automated Benchmarks
- `cargo bench --bench week4_optimization_benchmark`
- `./scripts/week4_performance_profiler.sh`

### Performance Dashboard
- `./scripts/performance_dashboard.sh`
- Tracks SLA compliance in real-time

### CI/CD Integration
- Pre-merge performance regression tests
- Automated benchmark runs on main branch
- Performance trend tracking over time

---

*Document created: 2025-11-18*
*Target completion: Week 4, Day 5*
*Goal: A- (88/100) → A+ (92/100)*
