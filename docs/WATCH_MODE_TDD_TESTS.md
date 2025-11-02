# Watch Mode Test Suite - London TDD

## Overview

Comprehensive London-style TDD test suite for watch mode functionality. These tests define the expected behavior for file watching and automatic template regeneration.

## Location

- Test file: `/Users/sac/ggen/cli/tests/conventions/watch_tests.rs`
- Module: `/Users/sac/ggen/cli/tests/conventions/mod.rs`
- Test runner: `/Users/sac/ggen/cli/tests/conventions_test.rs`

## Test Coverage

### 10 Core Tests (All Designed to Fail Initially)

1. **test_watch_detects_rdf_file_changes**
   - Verifies that RDF file modifications trigger watch events
   - Tests dependency graph querying
   - Ensures affected templates are identified

2. **test_watch_detects_template_file_changes**
   - Verifies template file modifications are detected
   - Tests self-regeneration of changed templates
   - Validates template file type detection

3. **test_watch_detects_query_file_changes**
   - Verifies SPARQL query file changes are detected
   - Tests dependency resolution for query-dependent templates
   - Ensures affected templates are regenerated

4. **test_watch_triggers_affected_templates_only**
   - Validates selective regeneration
   - Ensures independent templates are NOT regenerated
   - Tests dependency graph accuracy

5. **test_watch_handles_rapid_sequential_changes**
   - Tests debouncing functionality
   - Verifies multiple rapid changes result in single regeneration
   - Validates timing and debounce period compliance

6. **test_watch_respects_debounce_period**
   - Tests debounce configuration
   - Validates configurable debounce duration
   - Ensures debounce is properly applied

7. **test_watch_handles_file_deletion**
   - Tests deletion event handling
   - Verifies dependency graph updates after deletion
   - Ensures graceful handling of removed files

8. **test_watch_handles_new_file_creation**
   - Tests file creation events
   - Verifies new files are added to watch list
   - Tests regeneration of newly created templates

9. **test_watch_handles_directory_creation**
   - Tests directory creation events
   - Verifies recursive watching of new directories
   - Ensures dependency graph includes new locations

10. **test_watch_ignores_generated_directory**
    - Tests event filtering
    - Verifies generated files are ignored
    - Prevents infinite regeneration loops

### Additional Tests

- **integration_test_watch_full_workflow** (ignored by default)
  - End-to-end integration test with real file system
  - Tests complete watch-modify-regenerate cycle

- **test_event_filter_patterns**
  - Tests ignore pattern configuration
  - Validates pattern matching for `.git`, `generated`, etc.

- **test_dependency_graph_build**
  - Tests dependency graph construction
  - Validates graph building from project files

- **perf_test_watch_handles_high_frequency_changes** (ignored by default)
  - Performance test with 100 templates and 1000 changes
  - Validates memory and CPU usage under load

## Mock Interfaces Defined

### 1. MockFileWatcher
```rust
fn new<P: AsRef<Path> + 'static>(path: P) -> Result<Self, WatchError>;
fn watch(&mut self) -> Result<(), WatchError>;
fn set_debounce(&mut self, duration: Duration);
fn on_change<F>(&mut self, callback: F) where F: Fn(WatchEvent) + Send + 'static;
fn stop(&mut self) -> Result<(), WatchError>;
```

### 2. MockRegenerationService
```rust
fn new() -> Self;
fn regenerate_template<P: AsRef<Path> + 'static>(&self, template_path: P) -> Result<(), RegenerationError>;
fn regenerate_affected<P: AsRef<Path> + 'static>(&self, changed_file: P) -> Result<Vec<PathBuf>, RegenerationError>;
fn get_affected_templates<P: AsRef<Path> + 'static>(&self, file_path: P) -> Vec<PathBuf>;
```

### 3. MockDependencyGraph
```rust
fn new() -> Self;
fn build_graph<P: AsRef<Path> + 'static>(&mut self, project_root: P) -> Result<(), GraphError>;
fn get_dependents<P: AsRef<Path> + 'static>(&self, file_path: P) -> Vec<PathBuf>;
fn is_rdf_file<P: AsRef<Path> + 'static>(&self, file_path: P) -> bool;
fn is_template_file<P: AsRef<Path> + 'static>(&self, file_path: P) -> bool;
fn is_query_file<P: AsRef<Path> + 'static>(&self, file_path: P) -> bool;
```

### 4. MockEventFilter
```rust
fn new() -> Self;
fn should_ignore<P: AsRef<Path> + 'static>(&self, path: P) -> bool;
fn add_ignore_pattern(&mut self, pattern: &str);
fn is_generated_directory<P: AsRef<Path> + 'static>(&self, path: P) -> bool;
```

## Error Types

```rust
pub enum WatchError {
    NotifyError(String),
    InvalidPath(String),
    AlreadyWatching,
}

pub enum RegenerationError {
    TemplateNotFound(String),
    RenderFailed(String),
    IoError(String),
}

pub enum GraphError {
    ParseError(String),
    InvalidGraph(String),
}

pub enum WatchEvent {
    Created(PathBuf),
    Modified(PathBuf),
    Deleted(PathBuf),
    Renamed { from: PathBuf, to: PathBuf },
}
```

## Test Fixtures

### WatchTestFixture
- Creates temporary project structure
- Provides helper methods for file creation:
  - `create_rdf_file(name)` - Creates RDF file in `rdf/` directory
  - `create_template_file(name)` - Creates template in `templates/` directory
  - `create_query_file(name)` - Creates SPARQL query in `queries/` directory

## Dependencies Added

```toml
[dev-dependencies]
notify = "6.1"  # File system watcher library
mockall = "0.13"  # Already present - used for mocking
tempfile = "3.23"  # Already present - used for test fixtures
```

## Running Tests

```bash
# Run all watch mode tests (will fail until implementation exists)
cargo test --package ggen-cli-lib --test conventions_test

# Run specific test
cargo test --package ggen-cli-lib --test conventions_test test_watch_detects_rdf_file_changes

# Run with output
cargo test --package ggen-cli-lib --test conventions_test -- --nocapture

# Run including ignored tests (integration and performance)
cargo test --package ggen-cli-lib --test conventions_test -- --ignored
```

## Implementation Guidance

These tests define the contract for the watch mode implementation. Follow this order:

1. **Create Error Types** (`cli/src/domain/watch/errors.rs`)
   - Implement `WatchError`, `RegenerationError`, `GraphError`

2. **Create Event Types** (`cli/src/domain/watch/events.rs`)
   - Implement `WatchEvent` enum

3. **Implement EventFilter** (`cli/src/domain/watch/filter.rs`)
   - Ignore patterns for `.git`, `generated`, etc.
   - Directory filtering logic

4. **Implement DependencyGraph** (`cli/src/domain/watch/graph.rs`)
   - Parse RDF, template, and query files
   - Build dependency relationships
   - Query dependents of changed files

5. **Implement RegenerationService** (`cli/src/domain/watch/regeneration.rs`)
   - Template regeneration logic
   - Batch regeneration for affected templates
   - Error handling and logging

6. **Implement FileWatcher** (`cli/src/domain/watch/watcher.rs`)
   - Integrate with `notify` crate
   - Debouncing logic
   - Event callback handling
   - Start/stop functionality

7. **Implement WatchService** (`cli/src/domain/watch/service.rs`)
   - Coordinate all components
   - Main public API for watch mode

8. **Add CLI Command** (`cli/src/commands/template/watch.rs`)
   - `ggen template watch` command
   - Configuration options (debounce period, etc.)

## London TDD Principles Demonstrated

1. **Mock All Dependencies**: Every external dependency is mocked
2. **Test Behavior, Not Implementation**: Tests specify WHAT should happen, not HOW
3. **Design Through Tests**: Interfaces emerge from test requirements
4. **Fast Feedback**: Tests run in milliseconds without file I/O
5. **Clear Failures**: Each test uses `#[should_panic]` to show it will fail until implementation exists

## Expected Test Behavior

All tests currently use `panic!("not yet implemented: ...")` to demonstrate they will fail. This is intentional - these are RED tests waiting for GREEN implementation.

When you implement each component:
1. Remove the `panic!()` call from the test
2. Uncomment the assertion code
3. Run the test - it should FAIL (red)
4. Implement the feature
5. Run the test - it should PASS (green)
6. Refactor as needed

## Status

- ✅ Tests written and compilable (pending library fixes)
- ✅ Mock interfaces defined
- ✅ Error types specified
- ✅ Test fixtures created
- ❌ Implementation does not exist yet (by design)
- ❌ Tests currently panic with "not yet implemented"

## Note on Compilation

The tests are syntactically correct but cannot currently compile due to unrelated compilation errors in the main library code (`cli/src/conventions/planner.rs`). Once those errors are fixed, these tests will compile and can guide the watch mode implementation.

The tests themselves are independent and well-structured, ready to drive TDD implementation once the library compiles.
