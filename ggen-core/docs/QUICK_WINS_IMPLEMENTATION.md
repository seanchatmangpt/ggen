# Quick Wins Implementation Guide

**Target**: 40-60% performance improvement in 4-8 hours
**Date**: 2025-10-11

---

## Quick Win #1: SHA256 Caching (1 hour, 50-90% gain)

### Problem
`calculate_sha256()` is called multiple times for the same directory:
- On cache validation (line 63)
- On cache loading (line 132)
- On listing cached packs (line 195)

### Implementation

**File**: `/src/cache.rs`

```rust
use std::time::SystemTime;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Local cache manager for gpacks
#[derive(Debug, Clone)]
pub struct CacheManager {
    cache_dir: PathBuf,
    // NEW: SHA256 cache with timestamp validation
    sha256_cache: Arc<Mutex<HashMap<PathBuf, (SystemTime, String)>>>,
}

impl CacheManager {
    /// Create a new cache manager
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .context("Failed to find cache directory")?
            .join("ggen")
            .join("gpacks");

        fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

        Ok(Self {
            cache_dir,
            sha256_cache: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    /// Create a cache manager with custom directory (for testing)
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&cache_dir).context("Failed to create cache directory")?;

        Ok(Self {
            cache_dir,
            sha256_cache: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    /// Calculate SHA256 hash of a directory (with caching)
    fn calculate_sha256(&self, dir: &Path) -> Result<String> {
        // Check cache first
        if let Ok(cache) = self.sha256_cache.lock() {
            if let Some((cached_time, cached_hash)) = cache.get(dir) {
                // Verify directory hasn't been modified
                if let Ok(metadata) = fs::metadata(dir) {
                    if let Ok(modified) = metadata.modified() {
                        if modified <= *cached_time {
                            // Cache is still valid
                            return Ok(cached_hash.clone());
                        }
                    }
                }
            }
        }

        // Cache miss or invalidated - calculate hash
        let hash = self.calculate_sha256_uncached(dir)?;

        // Update cache
        if let Ok(mut cache) = self.sha256_cache.lock() {
            if let Ok(metadata) = fs::metadata(dir) {
                if let Ok(modified) = metadata.modified() {
                    cache.insert(dir.to_path_buf(), (modified, hash.clone()));
                }
            }
        }

        Ok(hash)
    }

    /// Calculate SHA256 without caching (private implementation)
    fn calculate_sha256_uncached(&self, dir: &Path) -> Result<String> {
        let mut hasher = Sha256::new();

        // Walk directory and hash all files
        for entry in walkdir::WalkDir::new(dir) {
            let entry = entry.context("Failed to read directory entry")?;
            let path = entry.path();

            if path.is_file() {
                let content = fs::read(path).context("Failed to read file for hashing")?;
                hasher.update(&content);
            }
        }

        Ok(format!("{:x}", hasher.finalize()))
    }

    /// Clear SHA256 cache (useful for testing)
    #[cfg(test)]
    pub fn clear_sha256_cache(&self) {
        if let Ok(mut cache) = self.sha256_cache.lock() {
            cache.clear();
        }
    }
}
```

### Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_caching() {
        let temp_dir = TempDir::new().unwrap();
        let test_dir = temp_dir.path().join("test");
        fs::create_dir_all(&test_dir).unwrap();
        fs::write(test_dir.join("file.txt"), "content").unwrap();

        let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf()).unwrap();

        // First call - cache miss
        let hash1 = cache_manager.calculate_sha256(&test_dir).unwrap();

        // Second call - should hit cache
        let hash2 = cache_manager.calculate_sha256(&test_dir).unwrap();

        assert_eq!(hash1, hash2);

        // Modify file
        fs::write(test_dir.join("file.txt"), "new content").unwrap();

        // Third call - cache should be invalidated
        let hash3 = cache_manager.calculate_sha256(&test_dir).unwrap();

        assert_ne!(hash2, hash3);
    }
}
```

### Expected Result
- First `calculate_sha256`: 200ms
- Subsequent calls: 1-5ms (99% faster)
- Overall speedup: 50-90% for operations that hash multiple times

---

## Quick Win #2: Batch State Persistence (1 hour, 30-50% gain)

### Problem
State is loaded and saved on every phase execution:
```rust
let mut state = load_state(&ctx.state_path)?;  // Disk read
state.record_run(...);
state.add_cache_key(...);
save_state(&ctx.state_path, &state)?;          // Disk write
```

For 5 phases: 5 reads + 5 writes = **10 disk operations**

### Implementation

**File**: `/src/lifecycle/exec.rs`

```rust
use std::sync::{Arc, Mutex};

/// Execution context for lifecycle phases (thread-safe)
pub struct Context {
    pub root: PathBuf,
    pub make: Arc<Make>,
    pub state_path: PathBuf,
    pub env: Vec<(String, String)>,
    hook_guard: Arc<Mutex<HashSet<String>>>,
    // NEW: In-memory state with deferred persistence
    state: Arc<Mutex<LifecycleState>>,
}

impl Context {
    /// Create new context with loaded state
    pub fn new(
        root: PathBuf,
        make: Arc<Make>,
        state_path: PathBuf,
        env: Vec<(String, String)>,
    ) -> Result<Self> {
        // Load state once at creation
        let state = load_state(&state_path)?;

        Ok(Self {
            root,
            make,
            state_path,
            env,
            hook_guard: Arc::new(Mutex::new(HashSet::new())),
            state: Arc::new(Mutex::new(state)),
        })
    }

    /// Persist state to disk (call explicitly when needed)
    pub fn persist_state(&self) -> Result<()> {
        let state = self.state.lock()
            .map_err(|_| LifecycleError::MutexPoisoned {
                phase: "state_persistence".to_string(),
            })?;

        save_state(&self.state_path, &*state)
    }
}

/// Internal phase execution (called after recursion check)
fn run_phase_internal(ctx: &Context, phase_name: &str) -> Result<()> {
    let phase = ctx
        .make
        .lifecycle
        .get(phase_name)
        .ok_or_else(|| LifecycleError::phase_not_found(phase_name))?;

    let cmds = phase.commands();
    if cmds.is_empty() {
        println!("⚠️  Phase '{}' has no commands", phase_name);
        return Ok(());
    }

    // Run before hooks
    run_before_hooks(ctx, phase_name)?;

    // Generate cache key
    let key = cache_key(phase_name, &cmds, &ctx.env, &[]);

    // Execute phase commands
    let started = current_time_ms();
    let timer = Instant::now();

    println!("▶️  Running phase: {}", phase_name);
    for cmd in &cmds {
        execute_command(cmd, &ctx.root, &ctx.env)?;
    }

    let duration = timer.elapsed().as_millis();
    println!("✅ Phase '{}' completed in {}ms", phase_name, duration);

    // Update in-memory state (NO disk I/O here!)
    {
        let mut state = ctx.state.lock()
            .map_err(|_| LifecycleError::MutexPoisoned {
                phase: phase_name.to_string(),
            })?;

        state.record_run(phase_name.to_string(), started, duration, true);
        state.add_cache_key(phase_name.to_string(), key);
    }

    // Run after hooks
    run_after_hooks(ctx, phase_name)?;

    Ok(())
}

/// Run a pipeline of phases sequentially
pub fn run_pipeline(ctx: &Context, phases: &[String]) -> Result<()> {
    // Execute all phases...
    if let Some(workspaces) = &ctx.make.workspace {
        let parallel = phases
            .first()
            .and_then(|p| ctx.make.lifecycle.get(p))
            .and_then(|ph| ph.parallel)
            .unwrap_or(false);

        if parallel {
            use rayon::prelude::*;

            let results: Vec<Result<()>> = workspaces
                .par_iter()
                .map(|(ws_name, workspace)| {
                    println!("\n📦 Workspace: {}", ws_name);
                    let ws_path = ctx.root.join(&workspace.path);
                    let ws_make_path = ws_path.join("make.toml");

                    let ws_make = if ws_make_path.exists() {
                        Arc::new(load_make(&ws_make_path)?)
                    } else {
                        Arc::clone(&ctx.make)
                    };

                    let ws_state_path = ws_path.join(".ggen/state.json");

                    let ws_ctx = Context::new(ws_path, ws_make, ws_state_path, ctx.env.clone())?;

                    for phase in phases {
                        run_phase(&ws_ctx, phase)?;
                    }

                    // Persist state after all phases complete
                    ws_ctx.persist_state()?;

                    Ok(())
                })
                .collect();

            for result in results {
                result?;
            }
        } else {
            for (ws_name, workspace) in workspaces {
                println!("\n📦 Workspace: {}", ws_name);
                let ws_path = ctx.root.join(&workspace.path);
                let ws_make_path = ws_path.join("make.toml");

                let ws_make = if ws_make_path.exists() {
                    Arc::new(load_make(&ws_make_path)?)
                } else {
                    Arc::clone(&ctx.make)
                };

                let ws_state_path = ws_path.join(".ggen/state.json");

                let ws_ctx = Context::new(ws_path, ws_make, ws_state_path, ctx.env.clone())?;

                for phase in phases {
                    run_phase(&ws_ctx, phase)?;
                }

                // Persist state after all phases complete
                ws_ctx.persist_state()?;
            }
        }
    } else {
        // No workspaces, run directly
        for phase in phases {
            run_phase(ctx, phase)?;
        }

        // Persist state once at the end
        ctx.persist_state()?;
    }

    Ok(())
}
```

### Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deferred_state_persistence() {
        let temp_dir = TempDir::new().unwrap();
        let state_path = temp_dir.path().join("state.json");

        let ctx = Context::new(
            temp_dir.path().to_path_buf(),
            Arc::new(make),
            state_path.clone(),
            vec![],
        ).unwrap();

        // State file should not exist yet
        assert!(!state_path.exists());

        // Run phases (state in memory only)
        run_phase(&ctx, "init").unwrap();
        run_phase(&ctx, "build").unwrap();

        // State file still should not exist
        assert!(!state_path.exists());

        // Explicitly persist
        ctx.persist_state().unwrap();

        // Now state file should exist
        assert!(state_path.exists());

        // Verify state contains both phases
        let state = load_state(&state_path).unwrap();
        assert_eq!(state.phase_history.len(), 2);
    }
}
```

### Expected Result
- 5-phase pipeline: 10 disk operations → 2 disk operations (80% reduction)
- Speedup: 30-50% for I/O-bound workloads

---

## Quick Win #3: Workspace Config Memoization (1 hour, 60-80% gain)

### Problem
Workspace `make.toml` files are loaded repeatedly in both parallel and sequential paths.

### Implementation

**File**: `/src/lifecycle/exec.rs`

```rust
use std::collections::HashMap;

/// Pre-load all workspace configurations
fn load_workspace_configs(
    ctx: &Context,
) -> Result<HashMap<String, Arc<Make>>> {
    let mut configs = HashMap::new();

    if let Some(workspaces) = &ctx.make.workspace {
        for (ws_name, workspace) in workspaces {
            let ws_path = ctx.root.join(&workspace.path);
            let ws_make_path = ws_path.join("make.toml");

            let ws_make = if ws_make_path.exists() {
                Arc::new(load_make(&ws_make_path)?)
            } else {
                Arc::clone(&ctx.make)
            };

            configs.insert(ws_name.clone(), ws_make);
        }
    }

    Ok(configs)
}

/// Run a pipeline of phases sequentially
pub fn run_pipeline(ctx: &Context, phases: &[String]) -> Result<()> {
    if let Some(workspaces) = &ctx.make.workspace {
        // Pre-load all workspace configs ONCE
        let ws_configs = load_workspace_configs(ctx)?;

        let parallel = phases
            .first()
            .and_then(|p| ctx.make.lifecycle.get(p))
            .and_then(|ph| ph.parallel)
            .unwrap_or(false);

        if parallel {
            use rayon::prelude::*;

            let results: Vec<Result<()>> = workspaces
                .par_iter()
                .map(|(ws_name, workspace)| {
                    println!("\n📦 Workspace: {}", ws_name);
                    let ws_path = ctx.root.join(&workspace.path);
                    let ws_state_path = ws_path.join(".ggen/state.json");

                    // Reuse pre-loaded config
                    let ws_make = Arc::clone(
                        ws_configs.get(ws_name)
                            .expect("Workspace config should be pre-loaded")
                    );

                    let ws_ctx = Context::new(ws_path, ws_make, ws_state_path, ctx.env.clone())?;

                    for phase in phases {
                        run_phase(&ws_ctx, phase)?;
                    }

                    ws_ctx.persist_state()?;
                    Ok(())
                })
                .collect();

            for result in results {
                result?;
            }
        } else {
            // Sequential execution
            for (ws_name, workspace) in workspaces {
                println!("\n📦 Workspace: {}", ws_name);
                let ws_path = ctx.root.join(&workspace.path);
                let ws_state_path = ws_path.join(".ggen/state.json");

                // Reuse pre-loaded config
                let ws_make = Arc::clone(
                    ws_configs.get(ws_name)
                        .expect("Workspace config should be pre-loaded")
                );

                let ws_ctx = Context::new(ws_path, ws_make, ws_state_path, ctx.env.clone())?;

                for phase in phases {
                    run_phase(&ws_ctx, phase)?;
                }

                ws_ctx.persist_state()?;
            }
        }
    } else {
        // No workspaces
        for phase in phases {
            run_phase(ctx, phase)?;
        }

        ctx.persist_state()?;
    }

    Ok(())
}
```

### Expected Result
- 10 workspaces: 10 loads → 1 load (90% reduction)
- Speedup: 60-80% for multi-workspace pipelines

---

## Quick Win #4: String Clone Reduction (2 hours, 15-25% gain)

### Problem
Multiple unnecessary string allocations:
- `phase.commands()` clones entire Vec
- `phase_name.to_string()` on every state operation

### Implementation

**File**: `/src/lifecycle/model.rs`

```rust
impl Phase {
    /// Get commands for this phase (returns slice to avoid cloning)
    pub fn commands(&self) -> &[String] {
        if let Some(cmd) = &self.command {
            std::slice::from_ref(cmd)
        } else if let Some(cmds) = &self.commands {
            cmds.as_slice()
        } else {
            &[]
        }
    }
}
```

**File**: `/src/lifecycle/state.rs`

```rust
impl LifecycleState {
    /// Add a new run record (avoids allocation if already owns String)
    pub fn record_run(
        &mut self,
        phase: impl Into<String>,
        started_ms: u128,
        duration_ms: u128,
        success: bool,
    ) {
        let phase_str = phase.into();
        self.phase_history.push(RunRecord {
            phase: phase_str.clone(),
            started_ms,
            duration_ms,
            success,
        });
        self.last_phase = Some(phase_str);
    }

    /// Add a cache key (avoids allocation if already owns String)
    pub fn add_cache_key(&mut self, phase: impl Into<String>, key: impl Into<String>) {
        self.cache_keys.push(CacheKey {
            phase: phase.into(),
            key: key.into(),
        });
    }
}
```

### Expected Result
- 15-25% reduction in string allocations
- Lower memory pressure

---

## Implementation Checklist

- [ ] **SHA256 Caching**
  - [ ] Add `sha256_cache` field to `CacheManager`
  - [ ] Implement `calculate_sha256_uncached()`
  - [ ] Add cache lookup in `calculate_sha256()`
  - [ ] Write tests for cache invalidation
  - [ ] Benchmark before/after

- [ ] **Batch State Persistence**
  - [ ] Add `state` field to `Context`
  - [ ] Load state in `Context::new()`
  - [ ] Remove `save_state()` from `run_phase_internal()`
  - [ ] Add `ctx.persist_state()` at end of `run_pipeline()`
  - [ ] Write tests for deferred persistence
  - [ ] Benchmark before/after

- [ ] **Workspace Config Memoization**
  - [ ] Implement `load_workspace_configs()`
  - [ ] Call once before workspace iteration
  - [ ] Reuse configs in parallel/sequential paths
  - [ ] Write tests for config sharing
  - [ ] Benchmark before/after

- [ ] **String Clone Reduction**
  - [ ] Change `Phase::commands()` to return `&[String]`
  - [ ] Update callers to use slice
  - [ ] Add `Into<String>` to state methods
  - [ ] Profile memory allocations before/after

---

## Benchmarking

Create `/benches/lifecycle_perf.rs`:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::lifecycle::*;

fn benchmark_sha256(c: &mut Criterion) {
    let temp_dir = TempDir::new().unwrap();
    let cache_manager = CacheManager::with_dir(temp_dir.path().to_path_buf()).unwrap();

    c.bench_function("sha256_first_call", |b| {
        b.iter(|| {
            cache_manager.clear_sha256_cache();
            cache_manager.calculate_sha256(black_box(&temp_dir.path())).unwrap()
        })
    });

    c.bench_function("sha256_cached_call", |b| {
        b.iter(|| {
            cache_manager.calculate_sha256(black_box(&temp_dir.path())).unwrap()
        })
    });
}

fn benchmark_state_persistence(c: &mut Criterion) {
    c.bench_function("state_save_load", |b| {
        let temp_dir = TempDir::new().unwrap();
        let state_path = temp_dir.path().join("state.json");

        b.iter(|| {
            let mut state = load_state(&state_path).unwrap();
            state.record_run("test".to_string(), 0, 100, true);
            save_state(&state_path, &state).unwrap();
        })
    });
}

criterion_group!(benches, benchmark_sha256, benchmark_state_persistence);
criterion_main!(benches);
```

Run benchmarks:
```bash
cargo bench --bench lifecycle_perf
```

---

## Expected Timeline

| Task | Time | Cumulative Gain |
|------|------|-----------------|
| SHA256 caching | 1h | 50-90% |
| Batch state saves | 1h | 60-95% |
| Workspace memoization | 1h | 70-98% |
| String clones | 2h | 75-99% |

**Total Time**: 4-5 hours
**Total Gain**: 40-60% overall performance improvement
