# ADR-001: Backend Abstraction Design

## Status
Accepted

## Context

The Cleanroom Testing Framework needs to support multiple execution environments:
- Local system execution (for lightweight testing)
- Docker containers (for full isolation)
- Podman containers (for rootless alternatives)
- Potential future backends (Kubernetes, cloud services, etc.)

We need a design that:
1. Provides a unified API across all backends
2. Allows easy addition of new backends
3. Minimizes code duplication
4. Maintains performance across implementations
5. Handles backend-specific features gracefully

## Decision

Implement a **trait-based backend abstraction** with the following design:

### Core Abstraction

```rust
pub trait Backend {
    fn name(&self) -> &str;
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult>;
    fn is_available(&self) -> bool;
    fn supports_feature(&self, feature: BackendFeature) -> bool;
}
```

### Key Design Elements

1. **Unified Command Structure**: `Cmd` type encapsulates all execution parameters
   - Binary/executable path
   - Arguments
   - Environment variables
   - Working directory
   - Policy constraints

2. **Auto-Detection**: `AutoBackend` automatically selects the best available backend
   ```rust
   pub struct AutoBackend;
   impl AutoBackend {
       pub fn detect() -> Result<Box<dyn Backend>> {
           // Priority: Docker > Podman > Local
       }
   }
   ```

3. **Feature Flags**: Backends declare capabilities
   - Network isolation
   - Filesystem isolation
   - Resource limiting
   - Deterministic execution

4. **Standardized Result**: All backends return `RunResult` with:
   - Exit code
   - stdout/stderr
   - Execution duration
   - Metadata (backend used, etc.)

### Implementation Strategy

- Start with `TestcontainerBackend` as the primary implementation
- Use testcontainers-rs 0.25 for Docker/Podman support
- Implement `LocalBackend` for non-containerized execution
- Future backends implement the same trait

## Consequences

### Positive

- **Unified API**: Users write code once, works across backends
- **Easy Testing**: Can mock backends for unit testing
- **Extensibility**: New backends add minimal overhead
- **Feature Detection**: Graceful degradation when features unavailable
- **Performance**: Backend-specific optimizations possible

### Negative

- **Abstraction Tax**: Some overhead from trait dispatch
- **Lowest Common Denominator**: Must design for least capable backend
- **Backend-Specific Features**: Advanced features may not be universally available
- **Complexity**: Additional layer between user and execution

### Neutral

- **Trait Objects**: Dynamic dispatch has minor performance cost
- **Backend Selection**: Requires runtime detection logic
- **Error Handling**: Must handle backend-specific errors generically

## Alternatives Considered

### 1. Compile-Time Backend Selection

Use feature flags to select backend at compile time.

**Rejected because:**
- Forces users to choose at build time
- Can't switch backends at runtime
- Makes testing multiple backends difficult
- Less flexible for end users

### 2. Monolithic Implementation

Implement all backends in a single type with runtime checks.

**Rejected because:**
- Tight coupling between backends
- Harder to test individual backends
- Difficult to maintain
- Can't easily add third-party backends

### 3. Macro-Based Code Generation

Generate backend implementations from declarative specifications.

**Rejected because:**
- Increases build complexity
- Harder to debug
- Less flexible for complex logic
- Steeper learning curve

### 4. Plugin System

Dynamic loading of backend implementations.

**Rejected because:**
- Adds significant complexity
- Security concerns with dynamic loading
- Complicates deployment
- Not necessary for current scope

## Implementation Details

### Backend Priority

```rust
impl AutoBackend {
    pub fn detect() -> Result<Box<dyn Backend>> {
        if TestcontainerBackend::is_available() {
            Ok(Box::new(TestcontainerBackend::new("alpine:latest")?))
        } else if LocalBackend::is_available() {
            Ok(Box::new(LocalBackend::new()))
        } else {
            Err(Error::no_backend_available())
        }
    }
}
```

### Error Handling

All backends must convert their errors to `CleanroomError`:

```rust
impl Backend for TestcontainerBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        self.execute(&cmd)
            .map_err(|e| CleanroomError::backend_error(e))
    }
}
```

### Feature Detection

```rust
pub enum BackendFeature {
    NetworkIsolation,
    FilesystemIsolation,
    ResourceLimits,
    DeterministicExecution,
}

// Usage
if backend.supports_feature(BackendFeature::NetworkIsolation) {
    // Apply network isolation
}
```

## References

- [testcontainers-rs documentation](https://docs.rs/testcontainers/)
- [Rust trait objects](https://doc.rust-lang.org/book/ch17-02-trait-objects.html)
- [Backend abstraction patterns in Cargo](https://github.com/rust-lang/cargo/tree/master/src/cargo/sources)
- Similar patterns: Tokio's Runtime, Diesel's Connection trait

## Future Considerations

- **Kubernetes Backend**: For distributed test execution
- **Cloud Provider Backends**: AWS Fargate, GCP Cloud Run
- **Wasm Backend**: For browser-based testing
- **SSH Backend**: For remote execution
- **Custom Backends**: User-defined backend implementations

