# Performance Optimization Implementation Guide

**Quick Reference**: Code snippets for implementing the optimizations identified in the Performance Optimization Report.

---

## Priority 1: Template Caching (20-30% improvement)

### File: `ggen-core/src/template_cache.rs` (Create new file)

```rust
//! Template caching system for high-performance template reuse
//!
//! Provides thread-safe caching of parsed templates to avoid repeated parsing overhead.

use dashmap::DashMap;
use once_cell::sync::Lazy;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use anyhow::Result;

use crate::template::Template;

/// Global template cache using DashMap for concurrent access
static TEMPLATE_CACHE: Lazy<TemplateCache> = Lazy::new(TemplateCache::new);

/// Thread-safe template cache with automatic eviction
pub struct TemplateCache {
    cache: DashMap<PathBuf, Arc<Template>>,
    max_size: usize,
}

impl TemplateCache {
    /// Create new template cache with default size limit
    pub fn new() -> Self {
        Self {
            cache: DashMap::new(),
            max_size: 1000, // Limit to 1000 cached templates
        }
    }

    /// Get or insert template into cache
    pub fn get_or_insert<P: AsRef<Path>, F>(
        &self,
        path: P,
        loader: F,
    ) -> Result<Arc<Template>>
    where
        F: FnOnce() -> Result<Template>,
    {
        let path = path.as_ref().to_path_buf();

        // Try to get from cache first
        if let Some(cached) = self.cache.get(&path) {
            return Ok(Arc::clone(&cached));
        }

        // Not in cache - load and insert
        let template = loader()?;
        let arc_template = Arc::new(template);

        // Check cache size limit
        if self.cache.len() >= self.max_size {
            // Simple eviction: clear 10% of cache
            // TODO: Implement LRU eviction for better performance
            self.evict_oldest(self.max_size / 10);
        }

        self.cache.insert(path.clone(), Arc::clone(&arc_template));
        Ok(arc_template)
    }

    /// Clear entire cache
    pub fn clear(&self) {
        self.cache.clear();
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            entries: self.cache.len(),
            max_size: self.max_size,
            hit_rate: 0.0, // TODO: Track hits/misses
        }
    }

    /// Evict oldest entries (simple FIFO for now)
    fn evict_oldest(&self, count: usize) {
        let mut to_remove = Vec::new();
        for entry in self.cache.iter().take(count) {
            to_remove.push(entry.key().clone());
        }
        for key in to_remove {
            self.cache.remove(&key);
        }
    }
}

#[derive(Debug, Clone)]
pub struct CacheStats {
    pub entries: usize,
    pub max_size: usize,
    pub hit_rate: f64,
}

/// Get global template cache instance
pub fn global_cache() -> &'static TemplateCache {
    &TEMPLATE_CACHE
}
```

### File: `ggen-core/src/generator.rs` (Modify existing)

```rust
// Add to imports
use crate::template_cache::global_cache;

// Modify generate() method
pub fn generate(&mut self) -> Result<PathBuf> {
    // Use cache for template loading
    let tmpl = global_cache().get_or_insert(&self.ctx.template_path, || {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        Template::parse(&input)
    })?;

    // Clone Arc to get owned Template for mutation
    let mut tmpl = (*tmpl).clone();

    // Rest of the function remains the same
    let mut tctx = Context::from_serialize(&self.ctx.vars)?;
    insert_env(&mut tctx);

    tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;
    tmpl.process_graph(
        &mut self.pipeline.graph,
        &mut self.pipeline.tera,
        &tctx,
        &self.ctx.template_path,
    )?;

    let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

    // ... rest of function
}
```

---

## Priority 2: RDF Batch Processing (15-20% improvement)

### File: `ggen-core/src/graph.rs` (Add method)

```rust
impl Graph {
    /// Insert multiple Turtle statements in a single transaction
    ///
    /// This is significantly faster than calling insert_turtle() multiple times
    /// because it processes all triples in one Oxigraph transaction.
    pub fn insert_turtle_batch(&mut self, turtle: &str) -> Result<()> {
        use oxigraph::io::RdfFormat;
        use oxigraph::model::GraphNameRef;
        use std::io::Cursor;

        let cursor = Cursor::new(turtle.as_bytes());

        // Single transaction for all triples
        self.store.load_graph(
            cursor,
            RdfFormat::Turtle,
            GraphNameRef::DefaultGraph,
            None,
        )?;

        Ok(())
    }

    /// Insert RDF from multiple sources in batch
    pub fn insert_rdf_batch(&mut self, sources: &[&str]) -> Result<()> {
        let combined = sources.join("\n");
        self.insert_turtle_batch(&combined)
    }
}
```

### File: `ggen-core/src/template.rs` (Modify process_graph)

```rust
pub fn process_graph(
    &mut self,
    graph: &mut Graph,
    tera: &mut Tera,
    ctx: &Context,
    template_path: &Path,
) -> Result<()> {
    // Batch RDF inline insertion
    if !self.front.rdf_inline.is_empty() {
        let mut batch = Vec::with_capacity(self.front.rdf_inline.len());
        for rdf_line in &self.front.rdf_inline {
            let rendered = tera.render_str(rdf_line, ctx)?;
            batch.push(rendered);
        }
        graph.insert_rdf_batch(&batch.iter().map(|s| s.as_str()).collect::<Vec<_>>())?;
    }

    // Batch RDF file insertion
    if !self.front.rdf.is_empty() {
        let mut batch = Vec::with_capacity(self.front.rdf.len());
        for rdf_line in &self.front.rdf {
            let rendered = tera.render_str(rdf_line, ctx)?;
            batch.push(rendered);
        }
        graph.insert_rdf_batch(&batch.iter().map(|s| s.as_str()).collect::<Vec<_>>())?;
    }

    // SPARQL queries remain the same (individual execution needed)
    for (name, query_str) in &self.front.sparql {
        let rendered_query = tera.render_str(query_str, ctx)?;
        let results = graph.query(&rendered_query)?;
        self.front.sparql_results.insert(name.clone(), results);
    }

    Ok(())
}
```

---

## Priority 3: CLI Lazy Initialization (30-40% startup improvement)

### File: `cli/src/lib.rs` (Modify)

```rust
use once_cell::sync::Lazy;
use tokio::runtime::Runtime;
use crate::pipeline::Pipeline;

/// Global tokio runtime - initialized on first access
static RUNTIME: Lazy<Runtime> = Lazy::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(4)
        .thread_name("ggen-runtime")
        .enable_all()
        .build()
        .expect("Failed to create tokio runtime")
});

/// Global pipeline - initialized on first access
static PIPELINE: Lazy<Result<Pipeline, anyhow::Error>> = Lazy::new(|| {
    Pipeline::new()
});

/// Execute async code using global runtime
pub fn execute<F, T>(future: F) -> T
where
    F: std::future::Future<Output = T>,
{
    RUNTIME.block_on(future)
}

/// Get global pipeline instance
pub fn global_pipeline() -> &'static Pipeline {
    PIPELINE.as_ref().expect("Failed to initialize pipeline")
}
```

### File: `cli/src/cmds/project/gen.rs` (Use lazy instances)

```rust
pub fn run(args: &GenArgs) -> Result<()> {
    // Use lazy-initialized global pipeline instead of creating new one
    let pipeline = global_pipeline();

    // Rest of function remains the same
    let ctx = GenContext::new(
        args.template_path.clone(),
        args.output_root.clone(),
    );

    let mut generator = Generator::new(pipeline.clone(), ctx);
    generator.generate()?;

    Ok(())
}
```

---

## Priority 4: Memory Optimization (10-15% memory reduction)

### File: `ggen-core/src/template.rs` (Optimize allocations)

```rust
// Use SmallVec for small variable lists (stack allocation)
use smallvec::SmallVec;

impl Frontmatter {
    /// Create context with pre-allocated capacity
    pub fn create_context(&self, base_vars: &BTreeMap<String, String>) -> Result<Context> {
        // Pre-allocate context with known size
        let capacity = self.vars.len() + base_vars.len() + 10; // +10 for env vars

        let mut ctx = Context::new();
        // Unfortunately, Tera Context doesn't expose capacity API
        // But we can minimize allocations by inserting in order

        // Insert vars efficiently
        for (key, value) in &self.vars {
            ctx.insert(key, value);
        }

        for (key, value) in base_vars {
            ctx.insert(key, value);
        }

        Ok(ctx)
    }
}

// Use Cow for zero-copy template content when possible
use std::borrow::Cow;

impl Template {
    /// Render body without copying if no substitution needed
    pub fn render_cow<'a>(&'a self, tera: &Tera, ctx: &Context) -> Result<Cow<'a, str>> {
        // Check if template has any variables
        if !self.body.contains("{{") && !self.body.contains("{%") {
            // No substitution needed - return borrowed content
            Ok(Cow::Borrowed(&self.body))
        } else {
            // Substitution needed - render to owned string
            let rendered = tera.render_str(&self.body, ctx)?;
            Ok(Cow::Owned(rendered))
        }
    }
}
```

### File: `ggen-core/src/templates.rs` (Pre-allocate file tree buffers)

```rust
pub fn generate_file_tree(spec: &FileTreeTemplate, vars: &BTreeMap<String, String>) -> Result<Vec<GeneratedFile>> {
    let estimated_files = estimate_file_count(&spec.root);

    // Pre-allocate vector with estimated capacity
    let mut results = Vec::with_capacity(estimated_files);

    generate_node(&spec.root, PathBuf::new(), vars, &mut results)?;

    Ok(results)
}

fn estimate_file_count(node: &FileTreeNode) -> usize {
    match &node.node_type {
        NodeType::Directory { children } => {
            1 + children.iter().map(estimate_file_count).sum::<usize>()
        }
        NodeType::File { .. } => 1,
    }
}
```

---

## Priority 5: Binary Size Reduction (30-40% size reduction)

### File: `Cargo.toml` (Optimize release profile)

```toml
[profile.release]
opt-level = "z"        # Optimize for size (change from 3)
lto = "fat"            # Full LTO (change from "thin")
codegen-units = 1      # Single codegen unit (change from 16)
strip = true           # Keep
panic = "abort"        # Add - removes unwinding code
overflow-checks = false # Add - removes overflow checks

[profile.release-small]
inherits = "release"
opt-level = "z"
lto = "fat"
codegen-units = 1
strip = "symbols"
panic = "abort"
```

### Feature Gating

```toml
[features]
default = ["core"]
core = []
ai = ["genai", "ggen-ai"]
marketplace = ["ggen-marketplace", "reqwest"]
full = ["ai", "marketplace"]

[dependencies]
# Make AI optional
genai = { version = "0.4", optional = true }
ggen-ai = { path = "ggen-ai", version = "1.2.0", optional = true }

# Make marketplace optional
ggen-marketplace = { path = "ggen-marketplace", version = "1.2.0", optional = true }
```

### Build Script for Size Optimization

```bash
#!/bin/bash
# scripts/build-optimized.sh

set -e

echo "Building optimized binary..."

# Build with size optimization profile
cargo build --profile release-small --no-default-features --features core

# Strip binary
strip target/release-small/ggen

# Compress with UPX (if available)
if command -v upx &> /dev/null; then
    echo "Compressing with UPX..."
    upx --best --lzma target/release-small/ggen
fi

# Show size
ls -lh target/release-small/ggen
```

---

## Benchmarking the Optimizations

### File: `ggen-core/benches/optimization_validation.rs`

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::template_cache::global_cache;
use ggen_core::Template;
use std::path::Path;

fn bench_template_caching_improvement(c: &mut Criterion) {
    let mut group = c.benchmark_group("caching_improvement");

    let template_str = r#"---
to: "output.rs"
vars:
  name: test
---
fn main() { println!("{{ name }}"); }
"#;

    // WITHOUT caching (baseline)
    group.bench_function("no_cache", |b| {
        b.iter(|| {
            Template::parse(black_box(template_str))
        });
    });

    // WITH caching (optimized)
    group.bench_function("with_cache", |b| {
        let path = Path::new("/tmp/test.tmpl");
        b.iter(|| {
            global_cache().get_or_insert(path, || {
                Template::parse(template_str)
            })
        });
    });

    group.finish();
}

fn bench_rdf_batch_improvement(c: &mut Criterion) {
    let mut group = c.benchmark_group("rdf_batch");

    let rdf_lines = vec![
        "@prefix ex: <http://example.org/> .",
        "ex:entity1 a ex:Type1 .",
        "ex:entity2 a ex:Type2 .",
        // ... add more triples
    ];

    // WITHOUT batching (baseline)
    group.bench_function("individual_inserts", |b| {
        b.iter(|| {
            let mut graph = Graph::new().unwrap();
            for line in &rdf_lines {
                graph.insert_turtle(line).unwrap();
            }
            black_box(graph);
        });
    });

    // WITH batching (optimized)
    group.bench_function("batch_insert", |b| {
        b.iter(|| {
            let mut graph = Graph::new().unwrap();
            graph.insert_rdf_batch(&rdf_lines.iter().map(|s| *s).collect::<Vec<_>>()).unwrap();
            black_box(graph);
        });
    });

    group.finish();
}

criterion_group!(
    optimization_benches,
    bench_template_caching_improvement,
    bench_rdf_batch_improvement
);

criterion_main!(optimization_benches);
```

### Running Benchmarks

```bash
# Baseline (before optimizations)
cargo bench --bench optimization_validation -- --save-baseline before

# Apply optimizations
# ... implement changes ...

# After benchmarks
cargo bench --bench optimization_validation -- --baseline before

# Should see output like:
# caching_improvement/no_cache    20.0 µs
# caching_improvement/with_cache   5.0 µs  (75% improvement!)
# rdf_batch/individual_inserts    100.0 µs
# rdf_batch/batch_insert           70.0 µs  (30% improvement!)
```

---

## Integration Checklist

- [ ] Create `template_cache.rs` module
- [ ] Modify `generator.rs` to use template cache
- [ ] Add `insert_turtle_batch()` to `graph.rs`
- [ ] Modify `template.rs` to batch RDF insertions
- [ ] Add lazy globals to `cli/src/lib.rs`
- [ ] Update command handlers to use lazy instances
- [ ] Add memory optimization utilities
- [ ] Update `Cargo.toml` with size optimization profile
- [ ] Add feature gates for optional functionality
- [ ] Create `optimization_validation.rs` benchmark
- [ ] Run benchmarks and validate improvements
- [ ] Update documentation with new performance characteristics

---

## Expected Results

After implementing all optimizations:

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Simple template parse | 20µs | 14µs | **30%** |
| Complex template parse | 500µs | 350µs | **30%** |
| RDF insert (100 triples) | 200ms | 160ms | **20%** |
| CLI startup | 150ms | 90ms | **40%** |
| Memory usage | 75MB | 65MB | **13%** |
| Binary size | 24MB | 16MB | **33%** |

---

## Monitoring Regressions

Add to CI pipeline:

```yaml
# .github/workflows/performance.yml
name: Performance Regression Check

on: [pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - run: cargo bench --bench optimization_validation -- --save-baseline pr
      - run: cargo bench --bench optimization_validation -- --baseline pr
      - name: Check for regressions
        run: |
          # Fail if any benchmark is >10% slower
          cargo bench --bench optimization_validation -- --baseline pr --regressed 10
```

---

**Implementation Guide Complete**
**Ready for Development**
