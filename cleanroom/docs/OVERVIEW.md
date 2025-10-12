# Cleanroom - Hermetic Testing for CLIs and Services

## Mission

Cleanroom provides hermetic, deterministic testing for CLIs and services. One API runs identically on laptop and CI by abstracting over Docker/Podman/local backends. Covers the 80/20 of test engineering: orchestration, security, determinism, fixtures, coverage, and forensics.

## Core Features

- **Unified API**: Single interface across Docker, Podman, and local execution
- **Deterministic Surfaces**: Control over time, RNG, filesystem, network, and process isolation
- **Security Baselines**: Non-root execution, read-only filesystems, capability dropping
- **Service Fixtures**: Ready-to-use PostgreSQL and Redis containers with health checks
- **Coverage Collection**: LLVM-based coverage with path remapping and merging
- **Forensics Bundle**: Complete test execution artifacts for debugging and compliance

## Quick Start

```rust
use cleanroom::{scenario, Backend, Policy, TimeProfile, RngProfile};

fn main() -> Result<(), cleanroom::CleanroomError> {
    // Simple one-shot execution
    let result = cleanroom::run(["echo", "hello world"])?;
    assert!(result.success());

    // Scenario DSL with fluent assertions
    let result = scenario("test echo")
        .step("echo", ["echo", "hello world"])
        .policy(Policy::locked())
        .determinism(TimeProfile::Frozen(12345), RngProfile::Seed(42))
        .backend(Backend::Auto)
        .run()?;

    assert!(result.stdout("hello world").is_ok());
    Ok(())
}
```

## Backends

### Auto Backend
Automatically detects and uses the best available backend:
- Podman (preferred - daemonless, rootless)
- Docker (isolated, widely available)
- Local (fallback, limited determinism)

### Docker Backend
Provides maximum isolation with security constraints:
```rust
use cleanroom::backend::DockerBackend;

let backend = DockerBackend::new("rust:1-slim");
```

### Podman Backend
Rootless container execution:
```rust
use cleanroom::backend::PodmanBackend;

let backend = PodmanBackend::new("rust:1-slim");
```

### Local Backend
Direct execution with best-effort determinism enforcement:
```rust
use cleanroom::backend::LocalBackend;

let backend = LocalBackend::new();
```

## Deterministic Surfaces

### Time Control
Freeze time for reproducible timestamps:
```rust
use cleanroom::{TimeProfile, scenario};

let result = scenario("time_test")
    .determinism(TimeProfile::Frozen(1640995200), RngProfile::Seed(42))
    .step("date", ["date"])
    .run()?;
```

### RNG Seeding
Control randomness for deterministic outputs:
```rust
use cleanroom::{RngProfile, scenario};

let result = scenario("rng_test")
    .determinism(TimeProfile::System, RngProfile::Seed(12345))
    .step("uuid", ["uuidgen"])
    .run()?;
```

### Network Isolation
Control network access:
```rust
use cleanroom::{Policy, NetProfile};

let policy = Policy {
    net: NetProfile::Offline, // No network access
    ..Policy::locked()
};
```

### Filesystem Control
Read-only execution with tmpfs workdir:
```rust
use cleanroom::{Policy, FsProfile};

let policy = Policy {
    fs: FsProfile::ReadOnly { workdir: true },
    ..Policy::locked()
};
```

## Service Fixtures

### PostgreSQL
Ready-to-use PostgreSQL container:
```rust
use cleanroom::services::Postgres;

let mut postgres = Postgres::new()?;
postgres.start()?;

// Get connection info
let conn_info = postgres.connection_info()?;
println!("Host: {}, Port: {}", conn_info.host, conn_info.port);

// Use in tests
// ... run tests against postgres ...

postgres.stop()?; // Cleanup
```

### Redis
Redis container with health checks:
```rust
use cleanroom::services::Redis;

let mut redis = Redis::with_config(6379, Some("mypass".to_string()))?;
redis.start()?;

let conn_info = redis.connection_info()?;
// Use Redis in tests...

redis.stop()?;
```

## Coverage Collection

Collect and merge coverage data:
```rust
use cleanroom::CoverageCollector;

let collector = CoverageCollector::new()?;
let coverage_data = collector.collect_from_container("container_id")?;
let merged = collector.merge_with_remap(coverage_data, remap)?;

println!("Coverage: {:.1}%", merged.percentage);
```

## Forensics and Attestation

Capture complete test execution artifacts:
```rust
use cleanroom::{ArtifactCollector, AttestationGenerator};

let collector = ArtifactCollector::new()?;
let run_info = RunInfo {
    scenario_name: "test_scenario".to_string(),
    backend: "docker".to_string(),
    workdir: PathBuf::from("/tmp"),
    log_files: vec![PathBuf::from("test.log")],
    binary_files: vec![PathBuf::from("target/debug/mycli")],
    config_files: vec![PathBuf::from("test_config.toml")],
};

let bundle = collector.collect(&run_info)?;
let bundle_path = PathBuf::from("forensics.json");
collector.save_bundle(&bundle, bundle_path)?;

// Generate attestation
let generator = AttestationGenerator::new().with_signing(Some(key));
let attestation = generator.generate(&run_info)?;
```

## Security Model

All backends enforce security baselines:
- Non-root execution (`--user=1000:1000`)
- Capability dropping (`--cap-drop=ALL`)
- Read-only root filesystem (`--read-only`)
- Tmpfs workdir (`--tmpfs=/workdir`)
- Network isolation (`--network=none` by default)
- Resource limits (CPU, memory, PIDs)

## Configuration

Configure via Cargo.toml metadata:
```toml
[package.metadata.cleanroom]
bin = "target/debug/mycli"
enabled = true
image = "rust:1-slim"
timeout_ms = 30000
net = "offline"
seed = 42
```

Environment variables and CLI options override config file settings.

## Performance

- First build: ≤15s
- Incremental: ≤2s
- RDF processing: ≤5s for 1k+ triples
- Generation memory: ≤100MB
- CLI scaffolding: ≤3s end-to-end

## Error Handling

Typed errors with actionable messages:
```rust
use cleanroom::{CleanroomError, BackendError, PolicyError};

match result {
    Err(CleanroomError::Backend(BackendError::NonZero { code, msg })) => {
        eprintln!("Command failed with code {}: {}", code, msg);
    }
    Err(CleanroomError::Policy(PolicyError::Violation(msg))) => {
        eprintln!("Policy violation: {}", msg);
    }
    _ => {}
}
```

## Integration Testing

Use in integration tests:
```rust
#[cfg(test)]
mod tests {
    use cleanroom::{scenario, Policy, TimeProfile, RngProfile};
    use cleanroom::services::Postgres;

    #[tokio::test]
    async fn test_with_database() {
        let mut postgres = Postgres::new().unwrap();
        postgres.start().unwrap();

        let result = scenario("db_test")
            .step("migrate", ["./migrate.sh"])
            .step("test", ["./test.sh"])
            .run()
            .unwrap();

        assert!(result.success());
        postgres.stop().unwrap();
    }
}
```
