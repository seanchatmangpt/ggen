# Quickstart Guide: ggen v5.0.0 Sync Command

**Feature**: Unified sync command
**Date**: 2025-12-17
**Audience**: Developers implementing v5 sync

## Overview

This quickstart provides the essential commands and workflows for implementing and testing ggen v5.0.0's unified sync command. It covers the 20% of operations that handle 80% of use cases.

---

## 1. Prerequisites

**Required**:
- Rust 1.75+ installed
- ggen workspace cloned
- Branch: `001-v5-release`

**Verify setup**:
```bash
rustc --version  # Should be >= 1.75.0
cargo make check  # Should complete in <5s
```

---

## 2. Quick Command Reference

### Core Development Commands

```bash
# Fast feedback loop (<20s total)
cargo make check       # Compilation check (<5s)
cargo make test-unit   # Unit tests only (<16s)
cargo make lint        # Clippy strict rules

# Full validation (<60s total)
cargo make test        # All 1,168+ tests
cargo make pre-commit  # Format + lint + tests
cargo make ci          # Full CI pipeline

# Performance
cargo make bench       # Run criterion benchmarks
cargo make slo-check   # Verify SLO compliance
```

### Sync Command Usage

```bash
# Basic sync (full regeneration)
ggen sync --manifest ggen.toml

# Validate manifest without generation
ggen sync --validate-only --manifest ggen.toml

# Dry run (preview changes)
ggen sync --dry-run --manifest ggen.toml

# Generate with audit trail
ggen sync --audit --manifest ggen.toml

# Verbose output for debugging
ggen sync --verbose --manifest ggen.toml
```

---

## 3. Essential File Locations

### Source Code

```
crates/
├── ggen-cli/src/cmds/sync.rs           # Layer 3: CLI verb
├── ggen-core/src/codegen/
│   ├── executor.rs                     # Layer 2: SyncExecutor
│   ├── pipeline.rs                     # Layer 1: GenerationPipeline
│   └── mod.rs                          # Module exports
├── ggen-core/src/manifest/
│   ├── parser.rs                       # Manifest TOML parsing
│   └── validator.rs                    # Schema validation
├── ggen-core/src/rdf/
│   └── query.rs                        # QueryCache (LRU)
└── ggen-core/src/templates/
    ├── generator.rs                    # FileTreeGenerator (Tera)
    └── tera_env.rs                     # Tera environment setup
```

### Configuration

```
ggen.toml                               # Manifest (workspace root)
crates/*/ggen.toml                      # Per-crate manifests
.ggen/sync-state.json                   # Incremental sync metadata
.ggen/cache/sparql/                     # Query result cache
```

### Tests

```
crates/ggen-core/tests/
├── sync_pipeline_test.rs               # Integration tests
└── fixtures/                           # Test data
```

---

## 4. Chicago TDD Workflow (Red-Green-Refactor)

### Step 1: Write Failing Test (RED)

```rust
// crates/ggen-core/tests/sync_pipeline_test.rs
#[test]
fn test_sync_generates_code_from_ontology() {
    // Arrange
    let manifest = create_test_manifest();
    let executor = SyncExecutor::new(manifest).unwrap();

    // Act
    let result = executor.execute_full_sync();

    // Assert
    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.files_written, 1);
    assert!(Path::new("target/test_output/generated.rs").exists());
}
```

**Run test**:
```bash
cargo make test-unit  # Should FAIL (test compiles but assertion fails)
```

### Step 2: Implement Minimum Code (GREEN)

```rust
// crates/ggen-core/src/codegen/executor.rs
impl SyncExecutor {
    pub fn execute_full_sync(&self) -> Result<SyncOutput, SyncError> {
        // Load ontology
        let store = self.load_ontology()?;

        // Execute inference (CONSTRUCT)
        self.execute_inference(&store)?;

        // Extract data (SELECT)
        let context = self.extract_data(&store)?;

        // Render templates
        let artifacts = self.render_templates(&context)?;

        // Write files
        self.write_files(&artifacts)?;

        Ok(SyncOutput {
            success: true,
            files_written: artifacts.len(),
            ..Default::default()
        })
    }
}
```

**Run test**:
```bash
cargo make test-unit  # Should PASS (green)
```

### Step 3: Refactor (Keep Tests Green)

```rust
// Extract stages into pipeline
impl SyncExecutor {
    pub fn execute_full_sync(&self) -> Result<SyncOutput, SyncError> {
        let pipeline = GenerationPipeline::new(&self.manifest, &self.base_dir)?;
        pipeline.run()
    }
}
```

**Verify refactoring**:
```bash
cargo make test-unit  # Still PASS (green maintained)
```

---

## 5. Common Development Tasks

### Task: Add New Pipeline Stage

1. **Write test** (RED):
```rust
#[test]
fn test_new_stage_execution() {
    let pipeline = GenerationPipeline::new(...);
    let result = pipeline.run_new_stage();
    assert!(result.is_ok());
}
```

2. **Implement stage** (GREEN):
```rust
impl GenerationPipeline {
    fn run_new_stage(&self) -> Result<(), Error> {
        // Implementation
        Ok(())
    }
}
```

3. **Integrate into pipeline** (Refactor):
```rust
pub fn run(&self) -> Result<SyncOutput, Error> {
    self.load_ontology()?;
    self.run_new_stage()?;  // Add here
    self.execute_inference()?;
    // ...
}
```

---

### Task: Add Query Cache Optimization

1. **Benchmark baseline**:
```bash
cargo bench -- baseline_no_cache
# Record mean time (e.g., 45.2ms)
```

2. **Implement cache**:
```rust
// crates/ggen-core/src/rdf/query.rs
impl QueryCache {
    pub fn execute_cached(&mut self, query: &str) -> Result<Vec<QuerySolution>, Error> {
        let key = self.compute_key(query);
        if let Some(cached) = self.cache.get(&key) {
            return Ok(cached.clone());  // Cache hit
        }

        let results = self.execute_query(query)?;
        self.cache.put(key, results.clone());
        Ok(results)
    }
}
```

3. **Benchmark optimized**:
```bash
cargo bench -- baseline_with_cache
# Verify speedup (e.g., 4.5ms = 10x faster)
```

---

### Task: Handle New Error Type

1. **Add error variant**:
```rust
// crates/ggen-core/src/error.rs
pub enum SyncError {
    // ... existing variants
    NewErrorType { message: String, context: String },
}
```

2. **Test error propagation**:
```rust
#[test]
fn test_new_error_propagates() {
    let result = function_that_fails();
    assert!(matches!(result, Err(SyncError::NewErrorType { .. })));
}
```

3. **Map to exit code**:
```rust
impl From<&SyncError> for i32 {
    fn from(error: &SyncError) -> i32 {
        match error {
            SyncError::NewErrorType { .. } => 7,  // New exit code
            // ...
        }
    }
}
```

---

## 6. Debugging Checklist

### Sync Fails to Execute

1. ☑ Check Andon signals: `cargo make check` (look for RED/YELLOW)
2. ☑ Verify manifest syntax: `ggen sync --validate-only`
3. ☑ Check file permissions: `ls -la ontology/*.ttl`
4. ☑ Run with verbose: `ggen sync --verbose`
5. ☑ Check logs: `cat .ggen/sync.log`

### Tests Failing

1. ☑ Run single test: `cargo test test_name -- --nocapture`
2. ☑ Check for Andon signals: `cargo make lint`
3. ☑ Verify test data: `ls -la crates/ggen-core/tests/fixtures/`
4. ☑ Check for unwrap violations: `grep -r "\.unwrap()" crates/ggen-core/src/`
5. ☑ Run with backtrace: `RUST_BACKTRACE=1 cargo test`

### Performance Regression

1. ☑ Run benchmarks: `cargo make bench`
2. ☑ Compare to baseline: `./scripts/compare_benchmarks.sh target/v4_baseline.json target/v5_current.json`
3. ☑ Profile with flamegraph: `cargo flamegraph --bin ggen -- sync --manifest ggen.toml`
4. ☑ Check cache hit rate: Look for "cache_hits" in audit.json

---

## 7. Validation Before Commit

**MANDATORY checks** (pre-commit hook runs these):

```bash
# 1. Compilation check (RED signal)
cargo make check  # Must pass (<5s)

# 2. Format check (YELLOW signal)
cargo fmt -- --check  # Must be formatted

# 3. Lint check (YELLOW signal)
cargo make lint  # Must pass Clippy

# 4. Test check (RED signal)
cargo make test-unit  # Must pass all unit tests

# 5. Integration tests (if changed pipeline)
cargo make test  # Must pass all 1,168+ tests
```

**Cannot commit if**:
- Compiler errors (RED)
- Test failures (RED)
- Unwrap in production code (see exemption in constitution)
- Missing type annotations

---

## 8. Common Patterns

### Pattern: Add SPARQL Query Support

```rust
// 1. Parse query from manifest
let query_source = match &rule.query {
    QuerySource::Inline(q) => q.clone(),
    QuerySource::File(p) => fs::read_to_string(p)?,
};

// 2. Execute with cache
let results = self.query_cache
    .execute_cached(&query_source)
    .map_err(|e| SyncError::Sparql { message: e.to_string() })?;

// 3. Convert to JSON context
let json_results: Value = results.iter()
    .map(|solution| solution_to_json(solution))
    .collect();
```

### Pattern: Handle File Generation Mode

```rust
match mode {
    GenerationMode::Create => {
        if path.exists() {
            return Err(IoError::FileExists(path.clone()));
        }
        fs::write(path, content)?;
    }
    GenerationMode::Overwrite => {
        fs::write(path, content)?;
    }
    GenerationMode::Merge => {
        let existing = if path.exists() {
            Some(fs::read_to_string(path)?)
        } else {
            None
        };
        let merged = merge_manual_regions(existing, content)?;
        fs::write(path, merged)?;
    }
}
```

### Pattern: Report Andon Signals

```rust
// In pipeline execution
if let Err(e) = stage_result {
    eprintln!("🔴 RED SIGNAL: {}", e);  // Stop the line!
    return Err(e);
}

// In warning conditions
if duration > slo_threshold * 0.9 {
    eprintln!("🟡 YELLOW SIGNAL: Approaching SLO ({:.2}ms)", duration);
}

// Success
println!("🟢 GREEN SIGNAL: Stage completed ({:.2}ms)", duration);
```

---

## 9. Key Files to Review

**Before implementing**:
1. `specs/001-v5-release/spec.md` - Requirements
2. `specs/001-v5-release/plan.md` - Architecture
3. `specs/001-v5-release/data-model.md` - Entity definitions
4. `specs/001-v5-release/contracts/sync-pipeline-api.md` - API contracts

**During implementation**:
5. `crates/ggen-core/src/codegen/pipeline.rs` - Main orchestration
6. `crates/ggen-core/src/manifest/parser.rs` - Config parsing
7. `crates/ggen-core/src/rdf/query.rs` - SPARQL caching

**For testing**:
8. `crates/ggen-core/tests/sync_pipeline_test.rs` - Integration tests
9. `benches/sync_scaling.rs` - Performance benchmarks

---

## 10. Next Steps

After reading this quickstart:

1. ☑ **Verify environment**: Run `cargo make check` (should pass)
2. ☑ **Review architecture**: Read `specs/001-v5-release/plan.md`
3. ☑ **Run existing tests**: Run `cargo make test`
4. ☑ **Write first test**: Add test for new feature (RED)
5. ☑ **Implement feature**: Make test pass (GREEN)
6. ☑ **Refactor**: Clean up while keeping tests green
7. ☑ **Benchmark**: Validate performance with `cargo make bench`
8. ☑ **Commit**: Ensure all Andon signals GREEN

---

## Summary

This quickstart covers:
- ✅ Essential commands (`cargo make check/test/lint/bench`)
- ✅ File locations (source, config, tests)
- ✅ TDD workflow (RED → GREEN → Refactor)
- ✅ Common tasks (add stage, optimize, debug)
- ✅ Validation checklist (pre-commit gates)
- ✅ Code patterns (SPARQL, file modes, Andon signals)

**Remember**: ggen v5 sync is **85% complete**. Focus on enhancement, not greenfield development.

**Constitutional principles**: Crate-first, Chicago TDD, cargo make protocol, type-first thinking, Andon signals, Result<T,E> error handling.

**Good luck! 🚀**
