# Cleanroom Design Philosophy

## Architecture Overview

Cleanroom follows a layered architecture that separates concerns while maintaining a unified API:

```
┌─────────────────┐    ┌─────────────────┐
│   High-level    │    │   Scenario DSL  │
│      API        │    │   & Assertions  │
└─────────────────┘    └─────────────────┘
         │                       │
         └───────────────────────┼───────────────────────┘
                                 │
┌─────────────────────────────────────────────────────────────────┐
│                      Runtime Controllers                         │
│  TimeController │ RngController │ FsController │ NetController  │
└─────────────────────────────────────────────────────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┼───────────────────────┘
                                 │                       │
┌─────────────────────────────────────────────────────────────────┐
│                          Backends                               │
│   AutoBackend   │   DockerBackend   │  PodmanBackend  │ Local  │
└─────────────────────────────────────────────────────────────────┘
```

## Design Principles

### 1. Zero-Cost Abstractions

- Minimal runtime overhead when determinism features aren't used
- Compile-time feature flags for optional functionality
- Efficient trait objects for backend abstraction

### 2. Type-Level Safety

- Typed error enums prevent error handling mistakes
- Const generics for template parameter shapes (future)
- Compile-time validation of policy constraints

### 3. Deterministic by Default

- Secure defaults that enforce determinism
- Explicit opt-in for non-deterministic features
- Clear separation between deterministic and system surfaces

### 4. Backend Parity

All backends implement identical semantics:
- Same security constraints
- Same resource limits
- Same determinism guarantees (where possible)

## Runtime Controllers

Runtime controllers abstract deterministic surfaces:

### TimeController
- `TimeProfile::{Frozen(u64), Monotonic, System}`
- Frozen time provides stable timestamps across runs
- Monotonic time advances predictably for testing timing logic

### RngController
- `RngProfile::Seed(u64)` for reproducible randomness
- System RNG for true randomness when needed
- Environment variable injection for subprocess control

### FsController
- `FsProfile::{ReadOnly, Writable}`
- Read-only rootfs with tmpfs workdir for isolation
- Bind mounts for test binaries and fixtures

### NetController
- `NetProfile::{Offline, Limited, Open}`
- Network namespace isolation
- iptables rules for local backend limitations

### ProcController
- `ProcProfile::{Standard, Isolated, Strict}`
- UID/GID mapping, capability dropping
- Seccomp filters and resource limits

## Backend Implementation

### Trait Design

```rust
pub trait Backend: Send + Sync {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult>;
    fn name(&self) -> &str;
}
```

### Command Structure

```rust
pub struct Cmd {
    pub bin: String,
    pub args: Vec<String>,
    pub env: Vec<(String, String)>,
    pub workdir: Option<String>,
    pub timeout_ms: Option<u64>,
}
```

### Result Structure

```rust
pub struct RunResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
    pub duration_ms: u64,
}
```

## Scenario DSL

Fluent API for building test scenarios:

```rust
impl Scenario {
    pub fn step<I, S>(self, label: String, args: I) -> Self
    pub fn concurrent(mut self) -> Self
    pub fn policy(self, policy: Policy) -> Self
    pub fn determinism(self, time: TimeProfile, rng: RngProfile) -> Self
    pub fn backend(self, backend: Box<dyn Backend>) -> Self
    pub fn services(self, services: Vec<Service>) -> Self
    pub fn run(self) -> Result<PlanResult>
}
```

## Error Model

Hierarchical error types for precise error handling:

```rust
pub enum CleanroomError {
    Backend(BackendError),
    Policy(PolicyError),
    Coverage(CoverageError),
    Scenario(ScenarioError),
    Service(ServiceError),
    Io(std::io::Error),
    Time(std::time::SystemTimeError),
}
```

Each error type provides context and actionable messages.

## Configuration Layering

Priority order: test options → `[package.metadata.cleanroom]` → environment → defaults

```toml
[package.metadata.cleanroom]
bin = "target/debug/mycli"
image = "rust:1-slim"
timeout_ms = 30000
net = "offline"
seed = 42
```

Environment variables (`CLEANROOM_*`) override config file settings.

## Security Model

### Container Security

All container backends enforce:
- Non-root execution (`--user=1000:1000`)
- Capability dropping (`--cap-drop=ALL`)
- Read-only root filesystem (`--read-only`)
- Tmpfs workdir (`--tmpfs=/workdir:rw,nodev,nosuid,size=100m`)
- Network isolation (`--network=none`)
- Resource limits (`--memory=256m`, `--cpus=0.5`, `--pids-limit=128`)

### Environment Redaction

Sensitive environment variables are automatically redacted:
- `*_KEY`, `*_TOKEN`, `*_SECRET`, `*_PASSWORD`
- `AWS_*`, `GITHUB_*`, `GITLAB_*`, `DOCKER_*`, `KUBE_*`

## Performance Optimizations

### Caching
- Template compilation results cached
- Query execution plans cached
- Coverage data deduplication

### Streaming
- Large RDF graphs processed in chunks
- Coverage data streamed from containers
- Logs processed incrementally

### Memory Management
- Bounded readers for large inputs
- Efficient string interning for identifiers
- Memory-mapped files for large templates

## Testing Strategy

### Unit Tests
- Error formatting and parsing
- Configuration layering
- Path remapping algorithms

### Integration Tests
- Scenario execution across all backends
- Determinism surface validation
- Service fixture lifecycle
- Coverage collection and merging

### Property Tests
- Template rendering determinism
- Policy serialization round-trips
- Error message normalization

### Fuzzing
- RDF parser robustness
- Template syntax edge cases
- Configuration file parsing

## Future Enhancements

### Advanced Features
- Proc-macro for compile-time template validation
- Const generics for type-safe template parameters
- Async runtime for concurrent scenario execution

### Ecosystem Integration
- VS Code extension for live template preview
- CI/CD pipeline integration
- IDE support for RDF schema validation

### Performance
- SIMD-accelerated RDF parsing
- GPU-accelerated template rendering (research)
- Zero-copy deserialization paths
