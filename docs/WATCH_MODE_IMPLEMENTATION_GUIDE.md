# Watch Mode Implementation Guide

## How to Use These Tests for TDD

This guide shows how to use the London TDD tests to drive watch mode implementation.

## Prerequisites

1. Fix library compilation errors:
   ```bash
   # The library currently has errors in conventions/planner.rs
   # Fix those first, then verify tests compile:
   cargo test --test conventions_test --no-run
   ```

2. Once tests compile, you're ready to implement!

## TDD Cycle Example: First Test

Let's implement `test_watch_detects_rdf_file_changes`:

### Step 1: RED - See the Test Fail

```bash
# Run the specific test
cargo test test_watch_detects_rdf_file_changes -- --nocapture

# Expected output:
# test test_watch_detects_rdf_file_changes ... FAILED
# thread panicked at 'not yet implemented: WatchService::start() does not exist'
```

### Step 2: Remove Panic, Uncomment Assertions

In `watch_tests.rs`, find the test and:

```rust
// BEFORE:
panic!("not yet implemented: WatchService::start() does not exist");

// AFTER:
let watch_service = WatchService::new(&fixture.project_root, mock_watcher, mock_graph, mock_regen)?;
watch_service.start()?;

// Simulate RDF file change
std::fs::write(&rdf_file, "<rdf:RDF>NEW DATA</rdf:RDF>")?;

// Wait for watch event processing
tokio::time::sleep(Duration::from_millis(200)).await;

// ASSERT: Verify event was detected
assert!(*event_triggered.lock().unwrap(), "RDF file change should trigger event");
```

### Step 3: Run Test Again - Should Fail with Compilation Error

```bash
cargo test test_watch_detects_rdf_file_changes

# Expected: compilation error - WatchService doesn't exist
```

### Step 4: GREEN - Create Minimum Implementation

Create `/Users/sac/ggen/cli/src/domain/watch/mod.rs`:

```rust
pub mod service;
pub mod watcher;
pub mod graph;
pub mod regeneration;
pub mod filter;
pub mod errors;
pub mod events;

pub use service::WatchService;
pub use errors::*;
pub use events::*;
```

Create `/Users/sac/ggen/cli/src/domain/watch/errors.rs`:

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum WatchError {
    NotifyError(String),
    InvalidPath(String),
    AlreadyWatching,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegenerationError {
    TemplateNotFound(String),
    RenderFailed(String),
    IoError(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum GraphError {
    ParseError(String),
    InvalidGraph(String),
}

impl std::error::Error for WatchError {}
impl std::error::Error for RegenerationError {}
impl std::error::Error for GraphError {}

impl std::fmt::Display for WatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotifyError(msg) => write!(f, "Watch error: {}", msg),
            Self::InvalidPath(msg) => write!(f, "Invalid path: {}", msg),
            Self::AlreadyWatching => write!(f, "Already watching"),
        }
    }
}

// Similar Display impls for other error types...
```

Create `/Users/sac/ggen/cli/src/domain/watch/events.rs`:

```rust
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum WatchEvent {
    Created(PathBuf),
    Modified(PathBuf),
    Deleted(PathBuf),
    Renamed { from: PathBuf, to: PathBuf },
}
```

Create `/Users/sac/ggen/cli/src/domain/watch/service.rs`:

```rust
use std::path::Path;
use super::errors::*;

pub struct WatchService {
    // Fields to be determined by tests
}

impl WatchService {
    pub fn new<P: AsRef<Path>>(
        project_root: P,
        watcher: impl FileWatcher,
        graph: impl DependencyGraph,
        regeneration: impl RegenerationService,
    ) -> Result<Self, WatchError> {
        // Minimum implementation to pass first test
        Ok(Self {})
    }

    pub fn start(&self) -> Result<(), WatchError> {
        // Minimum implementation
        Ok(())
    }
}
```

### Step 5: Run Test - Should Pass!

```bash
cargo test test_watch_detects_rdf_file_changes

# Expected:
# test test_watch_detects_rdf_file_changes ... ok
```

### Step 6: REFACTOR - Improve Implementation

Now that the test passes, you can refactor:

- Add proper struct fields
- Implement actual file watching logic
- Add error handling
- Improve code organization

**KEY RULE**: Keep the test passing while refactoring!

## Implementation Order

Implement tests in this order for best results:

### Phase 1: Foundation (Error & Event Types)

1. Create error types (`errors.rs`)
2. Create event types (`events.rs`)
3. Run helper tests:
   - `test_event_filter_patterns`
   - `test_dependency_graph_build`

### Phase 2: Core Components

4. **EventFilter** → Test: `test_watch_ignores_generated_directory`
   ```rust
   // cli/src/domain/watch/filter.rs
   pub struct EventFilter {
       patterns: Vec<String>,
   }

   impl EventFilter {
       pub fn new() -> Self { /* ... */ }
       pub fn should_ignore<P: AsRef<Path>>(&self, path: P) -> bool { /* ... */ }
       pub fn add_ignore_pattern(&mut self, pattern: &str) { /* ... */ }
       pub fn is_generated_directory<P: AsRef<Path>>(&self, path: P) -> bool { /* ... */ }
   }
   ```

5. **DependencyGraph** → Test: `test_watch_triggers_affected_templates_only`
   ```rust
   // cli/src/domain/watch/graph.rs
   pub struct DependencyGraph {
       dependents: HashMap<PathBuf, Vec<PathBuf>>,
   }

   impl DependencyGraph {
       pub fn new() -> Self { /* ... */ }
       pub fn build_graph<P: AsRef<Path>>(&mut self, root: P) -> Result<(), GraphError> { /* ... */ }
       pub fn get_dependents<P: AsRef<Path>>(&self, path: P) -> Vec<PathBuf> { /* ... */ }
       pub fn is_rdf_file<P: AsRef<Path>>(&self, path: P) -> bool { /* ... */ }
       pub fn is_template_file<P: AsRef<Path>>(&self, path: P) -> bool { /* ... */ }
       pub fn is_query_file<P: AsRef<Path>>(&self, path: P) -> bool { /* ... */ }
   }
   ```

6. **RegenerationService** → Tests:
   - `test_watch_detects_template_file_changes`
   - `test_watch_detects_rdf_file_changes`
   ```rust
   // cli/src/domain/watch/regeneration.rs
   pub struct RegenerationService {
       graph: Arc<DependencyGraph>,
   }

   impl RegenerationService {
       pub fn new(graph: Arc<DependencyGraph>) -> Self { /* ... */ }
       pub fn regenerate_template<P: AsRef<Path>>(&self, path: P) -> Result<(), RegenerationError> { /* ... */ }
       pub fn regenerate_affected<P: AsRef<Path>>(&self, path: P) -> Result<Vec<PathBuf>, RegenerationError> { /* ... */ }
       pub fn get_affected_templates<P: AsRef<Path>>(&self, path: P) -> Vec<PathBuf> { /* ... */ }
   }
   ```

### Phase 3: File Watching

7. **FileWatcher** → Tests:
   - `test_watch_respects_debounce_period`
   - `test_watch_handles_rapid_sequential_changes`
   ```rust
   // cli/src/domain/watch/watcher.rs
   use notify::{Watcher, RecursiveMode, recommended_watcher};

   pub struct FileWatcher {
       watcher: Box<dyn Watcher>,
       debounce: Duration,
   }

   impl FileWatcher {
       pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, WatchError> { /* ... */ }
       pub fn watch(&mut self) -> Result<(), WatchError> { /* ... */ }
       pub fn set_debounce(&mut self, duration: Duration) { /* ... */ }
       pub fn on_change<F>(&mut self, callback: F)
       where F: Fn(WatchEvent) + Send + 'static { /* ... */ }
       pub fn stop(&mut self) -> Result<(), WatchError> { /* ... */ }
   }
   ```

8. **Event Handling** → Tests:
   - `test_watch_handles_file_deletion`
   - `test_watch_handles_new_file_creation`
   - `test_watch_handles_directory_creation`

### Phase 4: Integration

9. **WatchService** → Test: `integration_test_watch_full_workflow`
   ```rust
   // cli/src/domain/watch/service.rs
   pub struct WatchService {
       watcher: FileWatcher,
       graph: Arc<DependencyGraph>,
       regeneration: RegenerationService,
       filter: EventFilter,
   }

   impl WatchService {
       pub fn new<P: AsRef<Path>>(root: P) -> Result<Self, WatchError> { /* ... */ }
       pub fn start(&self) -> Result<(), WatchError> { /* ... */ }
       pub fn stop(&self) -> Result<(), WatchError> { /* ... */ }
   }
   ```

### Phase 5: CLI & Performance

10. **CLI Command** → Manual testing
    ```rust
    // cli/src/commands/template/watch.rs
    pub async fn run(args: WatchArgs) -> anyhow::Result<()> {
        let service = WatchService::new(&args.project_root)?;
        println!("Watching for changes... (Press Ctrl+C to stop)");
        service.start()?;
        Ok(())
    }
    ```

11. **Performance** → Test: `perf_test_watch_handles_high_frequency_changes`

## Tips for Success

### 1. Follow the Red-Green-Refactor Cycle

```
RED: Write test → See it fail
GREEN: Write minimum code to pass
REFACTOR: Improve code while keeping tests green
REPEAT: Move to next test
```

### 2. Use Mocks to Define Interfaces

The mock interfaces in the tests show you exactly what your real code needs to implement:

```rust
// Mock shows the contract:
mock_graph
    .expect_is_rdf_file()
    .with(eq(rdf_file.clone()))
    .returning(|_| true);

// Your implementation must satisfy it:
impl DependencyGraph {
    pub fn is_rdf_file<P: AsRef<Path>>(&self, path: P) -> bool {
        path.as_ref()
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| matches!(e, "ttl" | "rdf" | "nt"))
            .unwrap_or(false)
    }
}
```

### 3. One Test at a Time

Don't implement everything at once. Focus on making one test pass, then move to the next.

### 4. Tests Guide Design

If a test is hard to implement, it might be telling you to refactor. The tests define good design.

### 5. Integration Tests Last

Run integration tests (`#[ignore]` tests) only after all unit tests pass.

## Example Session

```bash
# Start with first test
cargo test test_watch_detects_rdf_file_changes -- --nocapture
# RED: Test fails

# Implement minimum WatchService
# Edit cli/src/domain/watch/service.rs

cargo test test_watch_detects_rdf_file_changes
# GREEN: Test passes

# Refactor if needed
# GREEN: Test still passes

# Move to next test
cargo test test_watch_detects_template_file_changes
# RED: Test fails

# Implement template detection
# GREEN: Test passes

# Continue with remaining tests...
```

## When You're Done

All tests should pass:

```bash
cargo test --test conventions_test

# Expected output:
# test test_watch_detects_rdf_file_changes ... ok
# test test_watch_detects_template_file_changes ... ok
# test test_watch_detects_query_file_changes ... ok
# test test_watch_triggers_affected_templates_only ... ok
# test test_watch_handles_rapid_sequential_changes ... ok
# test test_watch_respects_debounce_period ... ok
# test test_watch_handles_file_deletion ... ok
# test test_watch_handles_new_file_creation ... ok
# test test_watch_handles_directory_creation ... ok
# test test_watch_ignores_generated_directory ... ok
# test test_event_filter_patterns ... ok
# test test_dependency_graph_build ... ok
#
# test result: ok. 12 passed; 0 failed; 0 ignored
```

Then run integration tests:

```bash
cargo test --test conventions_test -- --ignored

# test integration_test_watch_full_workflow ... ok
# test perf_test_watch_handles_high_frequency_changes ... ok
```

## Final CLI Test

```bash
# Create test project
ggen project init test-watch
cd test-watch

# Create RDF and template
echo '<rdf:RDF></rdf:RDF>' > rdf/data.ttl
echo '{{ data }}' > templates/output.md

# Start watching
ggen template watch

# In another terminal, modify RDF
echo '<rdf:RDF>NEW</rdf:RDF>' > rdf/data.ttl

# Check that template was regenerated
cat generated/output.md  # Should show new data
```

## Resources

- **Test File**: `/Users/sac/ggen/cli/tests/conventions/watch_tests.rs`
- **Documentation**: `/Users/sac/ggen/docs/WATCH_MODE_TDD_TESTS.md`
- **Summary**: `/Users/sac/ggen/docs/WATCH_MODE_TESTS_SUMMARY.md`

## Questions?

The tests are self-documenting. If you're unsure what a component should do, look at its tests:

- What methods should `DependencyGraph` have? → Look at `MockDependencyGraph`
- How should debouncing work? → Look at `test_watch_handles_rapid_sequential_changes`
- What should be ignored? → Look at `test_watch_ignores_generated_directory`

The tests tell you everything you need to know!
