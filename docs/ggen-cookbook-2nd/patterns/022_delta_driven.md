# Pattern 022: Delta-Driven Regeneration

## Intent

Regenerate only the portions of a knowledge graph affected by code changes, achieving sub-second update times for large codebases by avoiding full graph rebuilds.

## Also Known As

- Incremental Graph Updates
- Differential Regeneration
- Smart Graph Maintenance

## Motivation

Full graph regeneration becomes prohibitive at scale:

- **Time Overhead**: 10-second rebuilds disrupt flow state
- **Resource Waste**: 99% of graph unchanged on typical commit
- **Hook Latency**: Slow hooks frustrate developers
- **CI Bottlenecks**: Full rebuilds delay pipeline feedback

Delta-driven regeneration makes graph updates imperceptible by doing only necessary work.

## Applicability

Use Delta-Driven Regeneration when:

- Codebase exceeds 10k lines (full rebuilds > 1 second)
- Frequent small changes (feature development, debugging)
- Graph updates happen automatically (hooks, watchers)
- CI/CD has strict time budgets
- Interactive tools depend on graphs (LSP, IDE plugins)

## Structure

```
┌────────────────────┐
│  Change Detection  │
│  - Git diff        │
│  - File timestamps │
│  - Hash comparison │
└──────────┬─────────┘
           │ identifies deltas
           ▼
┌────────────────────┐
│ Dependency Graph   │
│  - File impacts    │
│  - Module deps     │
│  - Cross-refs      │
└──────────┬─────────┘
           │ computes affected
           ▼
┌────────────────────┐
│ Selective Rebuild  │
│  - Remove old      │
│  - Parse changed   │
│  - Merge new       │
└──────────┬─────────┘
           │ produces
           ▼
┌────────────────────┐
│  Updated Graph     │
│  (minimal changes) │
└────────────────────┘
```

### Key Components

1. **Change Detector**: Identifies modified files/entities
2. **Impact Analyzer**: Computes transitive dependencies
3. **Graph Subtractor**: Removes outdated triples
4. **Graph Merger**: Integrates new triples
5. **Consistency Checker**: Validates incremental result

## Implementation

### Basic Delta Detection (Git-Based)

```rust
// ggen/src/delta/detector.rs
use std::process::Command;
use std::collections::HashSet;

pub struct DeltaDetector {
    base_ref: String,  // e.g., "HEAD~1" or "origin/main"
}

impl DeltaDetector {
    pub fn detect_changed_files(&self) -> Result<HashSet<PathBuf>> {
        let output = Command::new("git")
            .args(&["diff", "--name-only", &self.base_ref])
            .output()?;

        let files = String::from_utf8(output.stdout)?
            .lines()
            .filter(|line| line.ends_with(".rs"))  // Rust files only
            .map(PathBuf::from)
            .collect();

        Ok(files)
    }

    pub fn detect_changed_entities(&self, files: &HashSet<PathBuf>)
        -> Result<HashSet<EntityUri>>
    {
        let mut entities = HashSet::new();

        for file in files {
            // Parse file to extract entity URIs
            let ast = parse_file(file)?;
            for item in ast.items() {
                entities.insert(item.uri());
            }
        }

        Ok(entities)
    }
}
```

### Impact Analysis via Dependency Graph

```rust
// ggen/src/delta/impact.rs
use petgraph::graph::DiGraph;

pub struct ImpactAnalyzer {
    dependency_graph: DiGraph<EntityUri, DependencyType>,
}

impl ImpactAnalyzer {
    /// Compute transitive closure of affected entities
    pub fn compute_impact(&self, changed: &HashSet<EntityUri>)
        -> HashSet<EntityUri>
    {
        let mut affected = changed.clone();
        let mut frontier = changed.clone();

        while !frontier.is_empty() {
            let mut next_frontier = HashSet::new();

            for entity in &frontier {
                // Find all entities that depend on this one
                if let Some(idx) = self.find_node(entity) {
                    for neighbor in self.dependency_graph.neighbors(idx) {
                        let dep_entity = &self.dependency_graph[neighbor];
                        if affected.insert(dep_entity.clone()) {
                            next_frontier.insert(dep_entity.clone());
                        }
                    }
                }
            }

            frontier = next_frontier;
        }

        affected
    }
}
```

### Incremental Graph Update

```rust
// ggen/src/delta/updater.rs
use oxigraph::model::GraphName;
use oxigraph::store::Store;

pub struct IncrementalUpdater {
    store: Store,
}

impl IncrementalUpdater {
    /// Remove triples for outdated entities
    pub fn remove_outdated(&mut self, entities: &HashSet<EntityUri>)
        -> Result<usize>
    {
        let mut removed = 0;

        for entity in entities {
            let pattern = (
                Some(entity.as_named_node()),
                None,  // Any predicate
                None,  // Any object
            );

            // Delete all triples with entity as subject
            for quad in self.store.quads_for_pattern(pattern, None) {
                self.store.remove(&quad?)?;
                removed += 1;
            }
        }

        Ok(removed)
    }

    /// Insert new triples for updated entities
    pub fn insert_updated(&mut self, entities: &HashSet<EntityUri>)
        -> Result<usize>
    {
        let mut inserted = 0;

        for entity in entities {
            // Re-parse entity from source
            let triples = self.parse_entity(entity)?;

            for triple in triples {
                self.store.insert(&triple)?;
                inserted += 1;
            }
        }

        Ok(inserted)
    }

    /// Full incremental update pipeline
    pub fn update(&mut self, changed_files: &HashSet<PathBuf>)
        -> Result<UpdateStats>
    {
        let timer = Instant::now();

        // 1. Detect changed entities
        let detector = DeltaDetector::new("HEAD~1");
        let changed_entities = detector.detect_changed_entities(changed_files)?;

        // 2. Compute impact
        let analyzer = ImpactAnalyzer::from_store(&self.store)?;
        let affected = analyzer.compute_impact(&changed_entities);

        // 3. Remove outdated triples
        let removed = self.remove_outdated(&affected)?;

        // 4. Insert updated triples
        let inserted = self.insert_updated(&changed_entities)?;

        Ok(UpdateStats {
            duration: timer.elapsed(),
            files_changed: changed_files.len(),
            entities_changed: changed_entities.len(),
            entities_affected: affected.len(),
            triples_removed: removed,
            triples_inserted: inserted,
        })
    }
}
```

## Sample Code

### CLI Integration

```rust
// ggen/src/main.rs
#[derive(Parser)]
enum Command {
    Graph {
        #[arg(long)]
        incremental: bool,

        #[arg(long)]
        changed: Option<Vec<PathBuf>>,

        #[arg(long, default_value = "HEAD~1")]
        base_ref: String,
    }
}

fn handle_graph_command(cmd: GraphCommand) -> Result<()> {
    if cmd.incremental {
        // Delta-driven regeneration
        let detector = DeltaDetector::new(&cmd.base_ref);
        let changed_files = if let Some(files) = cmd.changed {
            files.into_iter().collect()
        } else {
            detector.detect_changed_files()?
        };

        if changed_files.is_empty() {
            println!("✅ No changes detected, graph is current");
            return Ok(());
        }

        let mut updater = IncrementalUpdater::open("graphs/main.db")?;
        let stats = updater.update(&changed_files)?;

        println!("✅ Incremental update complete:");
        println!("   • {} files changed", stats.files_changed);
        println!("   • {} entities updated", stats.entities_changed);
        println!("   • {} entities affected (transitive)", stats.entities_affected);
        println!("   • {} triples removed, {} inserted",
                 stats.triples_removed, stats.triples_inserted);
        println!("   • Duration: {:?}", stats.duration);
    } else {
        // Full rebuild
        rebuild_graph_from_scratch()?;
    }

    Ok(())
}
```

### Pre-Commit Hook with Delta Detection

```bash
#!/bin/bash
# .git/hooks/pre-commit
# Uses delta-driven regeneration for speed

STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.rs$')

if [ -z "$STAGED_FILES" ]; then
  exit 0
fi

echo "🔄 Incremental graph update (delta-driven)..."

# Pass changed files explicitly
ggen graph --incremental --changed $STAGED_FILES

# Stage updated graph database
git add graphs/main.db

echo "✅ Graph updated in $(ggen stats --last-update-duration)"
exit 0
```

## Consequences

### Benefits

1. **Speed**: Sub-second updates vs. multi-second full rebuilds
2. **Scalability**: Update time proportional to change size, not codebase size
3. **Enabler**: Makes real-time features viable (LSP, live queries)
4. **Resource Efficiency**: Reduced CPU/memory during CI
5. **Developer Experience**: Imperceptible graph maintenance

### Drawbacks

1. **Complexity**: Dependency tracking adds implementation overhead
2. **Drift Risk**: Incremental errors accumulate over time
3. **Edge Cases**: Cross-file refactors may be missed
4. **Debugging**: Harder to reproduce incremental bugs
5. **State Management**: Requires persistent graph store

### Mitigations

- **Drift Prevention**: Periodic full rebuilds (nightly CI)
- **Validation**: Hash-based consistency checks between incremental and full
- **Fallback**: Auto-switch to full rebuild if incremental fails
- **Logging**: Detailed delta logs for debugging
- **Testing**: Compare incremental vs. full rebuild outputs

## Implementation Notes

### Consistency Validation

```rust
// Periodically verify incremental graph matches full rebuild
pub fn validate_incremental_consistency(store_path: &Path) -> Result<()> {
    // Compute hash of incremental graph
    let incremental_hash = hash_graph_store(store_path)?;

    // Rebuild from scratch in temp location
    let temp_store = rebuild_graph_full_temp()?;
    let full_hash = hash_graph_store(&temp_store)?;

    if incremental_hash != full_hash {
        eprintln!("⚠️  Incremental graph drift detected!");
        eprintln!("   Incremental: {}", incremental_hash);
        eprintln!("   Full rebuild: {}", full_hash);
        eprintln!("   Recommend: ggen graph --full");
        return Err(Error::InconsistentGraph);
    }

    println!("✅ Incremental graph consistent with full rebuild");
    Ok(())
}
```

### Dependency Graph Caching

```rust
// Cache dependency graph to avoid re-parsing on each update
pub struct CachedDependencyGraph {
    graph: DiGraph<EntityUri, DependencyType>,
    cache_path: PathBuf,
    last_modified: SystemTime,
}

impl CachedDependencyGraph {
    pub fn load_or_build(cache_path: impl Into<PathBuf>) -> Result<Self> {
        let cache_path = cache_path.into();

        if cache_path.exists() {
            // Load from cache
            let graph = bincode::deserialize_from(File::open(&cache_path)?)?;
            let last_modified = cache_path.metadata()?.modified()?;
            Ok(Self { graph, cache_path, last_modified })
        } else {
            // Build from scratch and cache
            let graph = build_dependency_graph_from_sources()?;
            bincode::serialize_into(File::create(&cache_path)?, &graph)?;
            let last_modified = SystemTime::now();
            Ok(Self { graph, cache_path, last_modified })
        }
    }

    pub fn invalidate_if_needed(&mut self, changed_files: &HashSet<PathBuf>)
        -> Result<()>
    {
        // Rebuild dep graph if changed files affect global structure
        // (e.g., new modules, changed mod.rs files)
        if self.affects_structure(changed_files) {
            self.graph = build_dependency_graph_from_sources()?;
            self.save_cache()?;
        }
        Ok(())
    }
}
```

### Performance Benchmarks

```rust
#[cfg(test)]
mod benchmarks {
    use super::*;
    use criterion::{criterion_group, criterion_main, Criterion};

    fn bench_incremental_vs_full(c: &mut Criterion) {
        let mut group = c.benchmark_group("graph_update");

        // Simulate 1-file change in 100-file codebase
        group.bench_function("incremental_1_file", |b| {
            b.iter(|| {
                let changed = simulate_single_file_change();
                update_incremental(&changed)
            })
        });

        group.bench_function("full_rebuild", |b| {
            b.iter(|| rebuild_graph_full())
        });

        group.finish();
    }

    criterion_group!(benches, bench_incremental_vs_full);
    criterion_main!(benches);
}

// Expected results:
// incremental_1_file: 50ms
// full_rebuild: 1200ms
// Speedup: 24x
```

## Related Patterns

- **Pattern 021 (Knowledge Hooks)**: Triggers for delta-driven updates
- **Pattern 024 (Git-as-Runtime)**: Git provides delta information
- **Pattern 018 (Graph Versioning)**: Managing incremental versions
- **Pattern 009 (Bidirectional Sync)**: Similar incremental approach

## Known Uses

### Rust Analyzer

Uses incremental re-parsing and dependency tracking for sub-second semantic analysis.

### TypeScript Language Server

Incremental type-checking recomputes only affected files on change.

### Bazel Build System

Delta-driven rebuilds via dependency graph enable large-scale incremental builds.

## Example: Full vs. Incremental Comparison

```bash
# Full rebuild (100 Rust files)
$ time ggen graph --full
✅ Graph built: 100 files, 5000 entities, 25000 triples
Duration: 3.2s

# Single file change
$ echo "// new function" >> src/lib.rs
$ time ggen graph --incremental
✅ Incremental update: 1 file changed, 3 entities updated, 15 triples
Duration: 0.08s

# Performance gain: 40x speedup
```

**Consistency Check:**
```bash
$ ggen validate --incremental
🔍 Comparing incremental vs. full rebuild...
   • Incremental hash: 8f3a9b2c...
   • Full rebuild hash: 8f3a9b2c...
✅ Graphs identical, incremental is consistent
```

---

**Pattern Status**: ✅ Complete
**Cookbook Chapter**: Part IV - Autonomic Patterns
**Dependencies**: Git, dependency graph, persistent store
**Complexity**: High
**Maintenance**: Medium (requires validation)
**Performance**: Critical for scale (>10k LOC)
