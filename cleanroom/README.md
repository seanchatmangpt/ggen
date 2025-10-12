# Cleanroom: Hermetic, Deterministic Testing for CLIs and Services

[![Crates.io](https://img.shields.io/crates/v/ggen-cleanroom.svg)](https://crates.io/crates/ggen-cleanroom)
[![Documentation](https://docs.rs/ggen-cleanroom/badge.svg)](https://docs.rs/ggen-cleanroom)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Cleanroom provides hermetic, deterministic testing for CLIs and services by abstracting over Docker/Podman/local backends. It covers the 80/20 of test engineering: orchestration, security, determinism, fixtures, coverage, and forensics.

## Features

- **Hermetic Execution**: Complete isolation from host system using containers
- **Deterministic Results**: Reproducible test execution with fixed seeds and ti```gherkin
# features/01_unified_execution.feature
@core @unified @auto
Feature: Unified execution behaves identically local and in containers
  Background:
    Given a fixture project "cli_smoke" with binary "target/debug/cli_smoke"
    And cleanroom is configured with defaults

  Scenario Outline: Help prints with identical semantics across backends
    Given backend "<backend>" is available or test is skipped
    When I run the binary with args ["--help"] using backend "<backend>"
    Then the exit code is 0
    And stdout contains "USAGE"
    And stderr is empty
    And execution is hermetic
    And mounts are deterministic
    And clock is normalized

    Examples:
      | backend  |
      | auto     |
      | local    |
      | docker   |
      | podman   |

  Scenario: Exit codes propagate
    When I run the binary with args ["exit", "--code", "7"] using backend "auto"
    Then the exit code is 7
    And stdout is empty
    And stderr contains "exiting with 7"
```

```gherkin
# features/02_backend_autodetect.feature
@core @detect
Feature: Backend auto-detection and override
  Background:
    Given a fixture project "cli_smoke"
    And docker is "maybe" available
    And podman is "maybe" available

  Scenario: Auto chooses docker if available
    Given environment CLEANROOM_BACKEND is unset
    When I request backend "auto"
    Then the resolved backend is one of ["docker","podman","local"]
    And preference order is docker > podman > local given availability

  Scenario: Env override forces local
    Given environment CLEANROOM_BACKEND="local"
    When I request backend "auto"
    Then the resolved backend is "local"
```

```gherkin
# features/03_config_precedence.feature
@core @config
Feature: Config precedence and validation
  Background:
    Given a fixture project "cli_smoke"
    And Cargo.toml contains:
      """
      [package.metadata.cleanroom]
      timeout_ms = 10000
      env.FROM_TOML = "1"
      """

  Scenario: Test options override toml and env
    Given environment CLEANROOM_TIMEOUT_MS="20000"
    And I pass --timeout-ms=5000 to run()
    When I run ["--help"]
    Then effective timeout is 5000
    And env contains "FROM_TOML=1"

  Scenario: Env overrides defaults but not explicit test options
    Given environment CLEANROOM_TIMEOUT_MS="15000"
    When I run ["--help"]
    Then effective timeout is 15000
```

```gherkin
# features/04_scenario_dsl.feature
@dsl
Feature: Scenario DSL for multi-step workflows
  Background:
    Given a fixture project "cli_workflow"

  Scenario: Sequential steps succeed
    When I define scenario "Build process"
      | step         | args                  | expect  |
      | version      | ["--version"]         | success |
      | build prod   | ["build","--prod"]    | success |
    And I execute the scenario on backend "auto"
    Then all steps succeeded
    And aggregated duration <= 30000 ms
    And step order is deterministic

  Scenario: Concurrent steps with deterministic aggregation
    When I define concurrent scenario "Parallel smoke"
      | step     | args             | expect  |
      | help1    | ["--help"]       | success |
      | help2    | ["--help"]       | success |
    And I execute the scenario on backend "auto"
    Then all steps succeeded
    And logs are order-stable by (start_ts, step_name)
```

```gherkin
# features/05_assertions.feature
@assert
Feature: Fluent assertions on output, JSON, and timing
  Background:
    Given a fixture project "cli_json"

  Scenario: JSON assertion with predicate
    When I run ["json","--emit","{\"ok\":true,\"n\":3}"] using "auto"
    Then stdout is valid json
    And stdout.json at "$.ok" is true
    And stdout.json at "$.n" equals 3
    And duration <= 1000 ms

  Scenario: Regex assertions
    When I run ["--help"] using "local"
    Then stdout matches /USAGE(?s).*OPTIONS/
```

```gherkin
# features/06_snapshots.feature
@snapshots
Feature: Snapshots with redaction and stability
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Help snapshot is stable across backends
    When I run ["--help"] using "docker"
    And I snapshot stdout as "help-output"
    Then snapshot matches on "local" backend too
    And paths and timestamps are normalized
```

```gherkin
# features/07_coverage.feature
@coverage
Feature: Coverage artifacts collection and host merge
  Background:
    Given a fixture project "cli_cov" instrumented for llvm-cov

  Scenario: Container-produced profraw is merged on host
    When I run ["cover","--work"] using "docker"
    Then a .profraw file exists in artifacts
    And cleanroom merges coverage into "target/coverage/coverage.json"
    And remapped paths point to host sources
```

```gherkin
# features/08_security_defaults.feature
@security
Feature: Secure-by-default policy
  Background:
    Given a fixture project "cli_net"

  Scenario: Network is disabled by default
    When I run ["fetch","http://example.org"] using "docker"
    Then the exit code is nonzero
    And stderr contains "network disabled"
    And capabilities dropped include ALL
    And process runs as non-root

  Scenario: Opt-in permissive policy
    Given I enable policy "permissive" explicitly
    When I run ["fetch","http://example.org"] using "docker"
    Then the exit code is 0
```

```gherkin
# features/09_services.feature
@services
Feature: Side services with health gates
  Background:
    Given a fixture project "cli_pg"

  Scenario: Postgres service with ready gate
    Given services:
      | name     | image              | port | health               |
      | postgres | postgres:16-alpine | 5432 | pg_isready -q        |
    When I run ["migrate"] using "docker"
    Then exit code is 0
    And service "postgres" logs contain "database system is ready"
```

```gherkin
# features/10_redaction.feature
@redact
Feature: Secret redaction in logs and artifacts
  Background:
    Given a fixture project "cli_secret"
    And environment SECRET_TOKEN="s3cr3t-ABC"

  Scenario: Secrets do not leak
    When I run ["echo-env","SECRET_TOKEN"] using "auto"
    Then stdout does not contain "s3cr3t-ABC"
    And stdout contains "[REDACTED]"
    And artifacts contain no unredacted secrets
```

```gherkin
# features/11_determinism.feature
@determinism
Feature: Deterministic outputs with seeded RNG and stable mounts
  Background:
    Given a fixture project "cli_rng"

  Scenario Outline: Same seed yields identical outputs
    Given seed "<seed>"
    When I run ["rand","--count","5"] using "auto"
    Then stdout equals the previous stdout for the same seed
    And artifact hashes match

    Examples:
      | seed |
      | 42   |
      | 1337 |

  Scenario: Different seeds yield different outputs
    Given seed "1"
    When I run ["rand","--count","5"] using "auto"
    And I run ["rand","--count","5"] using "auto" with seed "2"
    Then stdout differs
```

```gherkin
# features/12_errors.feature
@errors
Feature: Structured errors with context, no panics in library code
  Background:
    Given a fixture project "cli_fail"

  Scenario: Timeouts produce typed errors
    Given timeout is 50 ms
    When I run ["sleep","--ms","200"] using "local"
    Then an error "Timeout" is returned
    And error context includes { "args": ["sleep","--ms","200"], "timeout_ms": 50 }

  Scenario: Engine unavailable is a skipped test, not a failure
    Given docker is unavailable
    When I run ["--help"] using "docker"
    Then the scenario is skipped with reason "engine unavailable"

  Scenario: No panics in library code
    When I run ["panic"] using "auto"
    Then exit code is nonzero
    And error kind is "ProcessExit"
```

```gherkin
# features/13_windows_local.feature
@windows @local
Feature: Windows support via local backend
  Background:
    Given the OS is Windows

  Scenario: Local backend works without container engine
    Given docker is unavailable
    When I run ["--help"] using "local"
    Then exit code is 0
```

```gherkin
# features/14_concurrency.feature
@concurrency
Feature: Concurrent steps with isolation and bounded output
  Background:
    Given a fixture project "cli_workflow"

  Scenario: Two steps run concurrently without interference
    When I define concurrent scenario "dual-help"
      | step   | args       | expect  | max_output |
      | help1  | ["--help"] | success | 64KB       |
      | help2  | ["--help"] | success | 64KB       |
    And I execute the scenario on backend "auto"
    Then both steps succeeded
    And total duration < 2x single-step duration
    And per-step output <= max_output
```

```gherkin
# features/15_api_stability.feature
@api @doc
Feature: Public API stability and docs build
  Scenario: Docs build without private panics
    When I build documentation with --document-private-items
    Then build succeeds
    And crate root has forbid(unsafe_code)
    And clippy passes with -D warnings
```

```gherkin
# features/16_freeze_outputs.feature
@snapshots @idempotent
Feature: Idempotent results across repeated runs
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Re-running produces identical artifacts
    When I run ["--help"] using "auto"
    And I rerun ["--help"] using "auto"
    Then artifact digests are identical
    And stdout equals previous stdout
```

```gherkin
# features/17_policy_typestate.feature
@policy @compile
Feature: Typestate-enforced policy elevation
  Scenario: Elevation requires explicit permissive type
    Given a test that calls "enable_network()" under Restricted policy
    When I compile the test
    Then compilation fails with a type error mentioning "Permissive"
    And when I change policy to Permissive
    Then compilation succeeds
```

```gherkin
# features/18_cli_integration.feature
@cli @tools
Feature: Cargo integration via package.metadata.cleanroom
  Background:
    Given Cargo.toml contains:
      """
      [package.metadata.cleanroom]
      bin = "target/debug/cli_smoke"
      enabled = true
      """

  Scenario: One-flag CI enablement uses container backend automatically
    Given CI environment is detected
    When I run ["--help"] with backend "auto"
    Then resolved backend is not "local" if a container engine is available
    And exit code is 0
```

```gherkin
# features/19_service_teardown.feature
@services @teardown
Feature: Idempotent teardown of services and workspaces
  Background:
    Given a fixture project "cli_pg"

  Scenario: Teardown always succeeds
    Given a postgres service is running
    When I teardown services
    And I teardown services again
    Then both operations succeed
    And no containers remain with label "cleanroom"
```

```gherkin
# features/20_artifact_contract.feature
@artifacts
Feature: Artifact capture, structure, and size limits
  Background:
    Given a fixture project "cli_artifacts"

  Scenario: Logs and files captured under run id
    When I run ["produce-artifacts"] using "auto"
    Then an artifact directory exists "artifacts/<run_id>/"
    And contains files:
      | path               |
      | stdout.log         |
      | stderr.log         |
      | config.json        |
    And total size <= 10MB by default
```

```gherkin
# features/21_engine_matrix.feature
@matrix
Feature: Matrix behavior across docker and podman
  Background:
    Given docker is "maybe" available
    And podman is "maybe" available

  Scenario: Same scenario passes on both engines
    When I execute scenario "help" on engines ["docker","podman"]
    Then results are equivalent modulo engine metadata
```

```gherkin
# features/22_tracing.feature
@tracing
Feature: Structured tracing for runs
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Trace includes spans for prepare/run/teardown
    When I run ["--help"] with tracing enabled
    Then a trace file exists
    And spans include "prepare","run","teardown"
    And each span has duration and status
```

```gherkin
# features/23_skip_logic.feature
@skip
Feature: Skip semantics for missing engines or capabilities
  Scenario: Missing podman skips podman scenarios only
    Given podman is unavailable
    When I run the full feature suite
    Then podman-tagged scenarios are skipped
    And other scenarios run
```

```gherkin
# features/24_io_limits.feature
@limits
Feature: Output and resource limits
  Background:
    Given a fixture project "cli_spam"

  Scenario: Output is truncated with indicator
    Given max_output is 4KB
    When I run ["spam","--bytes","16384"] using "auto"
    Then captured stdout size is 4096 bytes
    And stdout ends with "[TRUNCATED]"
```

```gherkin
# features/25_workdir_isolation.feature
@isolation
Feature: Ephemeral workspace and read-only rootfs
  Background:
    Given a fixture project "cli_fs"

  Scenario: Writes land in tmpfs workdir
    When I run ["write","/tmp/testfile"] using "docker"
    Then file "/tmp/testfile" exists inside container
    And no writes occurred on host source directory
```

```gherkin
# features/26_time_budget.feature
@timing
Feature: Time budgets per step and scenario
  Background:
    Given a fixture project "cli_sleep"

  Scenario: Per-step timeout enforced
    Given step timeout is 100 ms
    When I run scenario with a step ["sleep","--ms","300"] using "auto"
    Then that step fails with timeout
    And subsequent steps still run if configured "continue_on_fail"
```

```gherkin
# features/27_json_report.feature
@report
Feature: Machine-readable run reports
  Background:
    Given a fixture project "cli_smoke"

  Scenario: Report contains normalized schema
    When I run ["--help"] using "auto" with report enabled
    Then "artifacts/<run_id>/report.json" exists
    And $.backend in report is one of ["local","docker","podman"]
    And $.steps[0].name == "run --help"
    And $.steps[0].exit_code == 0
```
me
- **Multiple Backends**: Docker, Podman, and local execution support
- **Security Policies**: Network isolation, filesystem constraints, and resource limits
- **Service Fixtures**: Containerized PostgreSQL, Redis, and other services
- **Coverage Collection**: Comprehensive test coverage analysis
- **Artifact Collection**: Forensics bundle creation for debugging
- **Attestation**: Cryptographic attestation of test execution
- **Performance Monitoring**: Resource usage tracking and SLO validation

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
ggen-cleanroom = "0.1"
```

Basic usage:

```rust
use cleanroom::{run, assertions::Assert};

// Simple command execution
let result = run(["echo", "hello world"])?;
result.success().stdout("hello world");

// Scenario-based testing
use cleanroom::scenario;

let result = scenario("test scenario")
    .step("echo hello", ["echo", "hello"])
    .step("echo world", ["echo", "world"])
    .run()?;

result.success();
assert_eq!(result.steps.len(), 2);
```

## Architecture

### Core Components

- **Backends**: Docker, Podman, and local execution engines
- **Policies**: Security and resource constraint enforcement
- **Services**: Containerized service fixtures (PostgreSQL, Redis)
- **Coverage**: Test coverage collection and analysis
- **Artifacts**: Forensics bundle creation and storage
- **Attestation**: Cryptographic attestation and provenance

### Backend Abstraction

```rust
use cleanroom::backend::{Backend, AutoBackend};

// Auto-detect best available backend
let backend = AutoBackend::detect()?;

// Or use specific backend
use cleanroom::backend::{DockerBackend, PodmanBackend};

let docker_backend = DockerBackend::new("rust:1-slim")?;
let podman_backend = PodmanBackend::new("rust:1-slim")?;
```

### Security Policies

```rust
use cleanroom::policy::{Policy, NetProfile, FsProfile, ProcProfile};

// Locked-down policy (maximum security)
let policy = Policy::locked();

// Custom policy
let policy = Policy {
    net: NetProfile::Limited { allowed_ports: vec![80, 443] },
    fs: FsProfile::ReadOnly { workdir: "workdir".to_string() },
    proc: ProcProfile::Strict,
    limits: ResourceLimits::strict(),
};
```

### Service Fixtures

```rust
use cleanroom::services::{ServiceManager, ServiceBuilder};

let mut manager = ServiceManager::new();

// Add PostgreSQL service
let postgres = ServiceBuilder::new("postgres")
    .with_config("port", "5433")
    .with_config("database", "testdb")
    .build()?;
manager.add_service(postgres);

// Add Redis service
let redis = ServiceBuilder::new("redis")
    .with_config("port", "6380")
    .build()?;
manager.add_service(redis);

// Start all services
manager.start_all()?;

// Use services in tests
let postgres_service = manager.get_service_by_name("postgres").unwrap();
let conn_info = postgres_service.connection_info()?;
println!("PostgreSQL URL: {}", conn_info.postgres_url());

// Stop all services
manager.stop_all()?;
```

### Coverage Collection

```rust
use cleanroom::coverage::CoverageCollector;

let collector = CoverageCollector::new()?
    .with_format(CoverageFormat::Profraw)
    .with_system_libraries(false);

// Collect coverage from local execution
let coverage = collector.collect_local(&binary_path, &["arg1", "arg2"])?;

// Convert to different format
let lcov_data = collector.convert_format(&coverage, CoverageFormat::Lcov)?;

// Save coverage data
collector.save_coverage(&coverage, Path::new("coverage.profraw"))?;
```

### Artifact Collection

```rust
use cleanroom::artifacts::ArtifactCollector;

let collector = ArtifactCollector::new()?
    .with_redaction(true);

let run_info = RunInfo {
    scenario_name: "test_scenario".to_string(),
    backend: "docker".to_string(),
    workdir: PathBuf::from("/tmp"),
    log_files: vec![],
    binary_files: vec![],
    config_files: vec![],
    coverage_files: vec![],
    container_id: None,
    policy: Policy::default(),
    image_digests: HashMap::new(),
    environment: HashMap::new(),
};

let bundle = collector.collect(&run_info)?;
collector.save_bundle(&bundle, PathBuf::from("forensics.json"))?;
```

### Attestation

```rust
use cleanroom::attest::AttestationGenerator;

let generator = AttestationGenerator::new()
    .with_signing(Some("signing_key.pem".to_string()));

let run_info = RunInfo {
    scenario_name: "test_scenario".to_string(),
    backend: "docker".to_string(),
    image_digests: HashMap::new(),
    policy: "locked".to_string(),
    environment: HashMap::new(),
};

let attestation = generator.generate(&run_info, Some((100, 150)))?;
let verified = generator.verify(&attestation)?;
assert!(verified);
```

## Configuration

### Environment Variables

```bash
# Backend selection
export CLEANROOM_DEFAULT_BACKEND=docker

# Policy selection
export CLEANROOM_DEFAULT_POLICY=locked

# Working directory
export CLEANROOM_WORKDIR=/tmp/cleanroom

# Artifact collection
export CLEANROOM_ARTIFACTS_ENABLED=true
export CLEANROOM_ARTIFACTS_OUTPUT_DIR=/tmp/artifacts

# Performance settings
export CLEANROOM_DEFAULT_TIMEOUT_SECS=300
export CLEANROOM_MAX_CONCURRENT=4
```

### Configuration File

```toml
# cleanroom.toml
default_backend = "docker"
default_policy = "locked"
workdir = "/tmp/cleanroom"

[artifacts]
enabled = true
output_dir = "/tmp/artifacts"
redact_sensitive = true
max_size_bytes = 104857600  # 100MB
retention = "KeepDays(7)"

[coverage]
enabled = true
format = "profraw"
include_system = false

[security]
attestation_enabled = true
signing_enabled = false
sensitive_env_patterns = ["KEY", "TOKEN", "SECRET", "PASSWORD"]

[performance]
default_timeout_secs = 300
max_concurrent = 4
memory_limit_bytes = 536870912  # 512MB
cpu_limit = 0.5

[backends.docker]
settings = { "network_mode" = "bridge" }
image = { name = "rust", tag = "1-slim", pull_latest = true }
resources = { cpu_limit = 0.5, memory_limit = 536870912 }

[backends.podman]
settings = { "rootless" = true }
image = { name = "rust", tag = "1-slim", pull_latest = true }
resources = { cpu_limit = 0.5, memory_limit = 536870912 }
```

## Advanced Usage

### Custom Backend

```rust
use cleanroom::backend::Backend;

struct CustomBackend;

impl Backend for CustomBackend {
    fn run_cmd(&self, cmd: Cmd) -> Result<RunResult> {
        // Custom execution logic
        Ok(RunResult {
            exit_code: 0,
            stdout: "custom output".to_string(),
            stderr: String::new(),
            duration_ms: 100,
        })
    }

    fn name(&self) -> &str {
        "custom"
    }
}

let backend = Box::new(CustomBackend);
let result = scenario("custom test")
    .backend(backend)
    .step("test", ["custom", "command"])
    .run()?;
```

### Custom Service

```rust
use cleanroom::services::Service;

struct CustomService {
    started: bool,
}

impl Service for CustomService {
    fn name(&self) -> &str {
        "custom"
    }

    fn health_check(&self) -> Result<bool> {
        Ok(self.started)
    }

    fn connection_info(&self) -> Result<ConnectionInfo> {
        Ok(ConnectionInfo::new("localhost", 8080))
    }

    fn start(&mut self) -> Result<()> {
        self.started = true;
        Ok(())
    }

    fn stop(&mut self) -> Result<()> {
        self.started = false;
        Ok(())
    }
}
```

### Scenario DSL

```rust
use cleanroom::scenario;

// Sequential execution
let result = scenario("sequential test")
    .step("setup", ["mkdir", "-p", "testdir"])
    .step("build", ["cargo", "build"])
    .step("test", ["cargo", "test"])
    .policy(Policy::locked())
    .run()?;

// Concurrent execution
let result = scenario("concurrent test")
    .step("task1", ["echo", "task1"])
    .step("task2", ["echo", "task2"])
    .step("task3", ["echo", "task3"])
    .concurrent()
    .run()?;

// With services
let mut postgres = Postgres::new()?;
let result = scenario("database test")
    .services(vec![Box::new(postgres)])
    .step("migrate", ["cargo", "run", "--", "migrate"])
    .step("test", ["cargo", "test"])
    .run()?;
```

### Fluent Assertions

```rust
use cleanroom::assertions::Assert;

let result = run(["cargo", "test"])?;

// Chained assertions
result
    .success()
    .stdout("test result")
    .stderr_regex(r"warning: .*")
    .duration_le(5000)
    .no_output_over(1024 * 1024); // 1MB limit

// JSON output validation
let result = run(["cargo", "test", "--", "--format", "json"])?;
result.success().stdout_json();
```

## Performance and SLOs

Cleanroom is designed to meet strict performance requirements:

- **First build**: ≤ 15 seconds
- **Incremental build**: ≤ 2 seconds
- **RDF processing**: ≤ 5 seconds for 1k+ triples
- **Generation memory**: ≤ 100MB
- **CLI scaffolding**: ≤ 3 seconds end-to-end
- **100% reproducible outputs**

### Performance Monitoring

```rust
use cleanroom::runtime::RuntimeBuilder;

let mut runtime = RuntimeBuilder::new(vec!["cargo".to_string(), "build".to_string()])
    .timeout(Duration::from_secs(15))
    .build()?;

let output = runtime.execute()?;

// Check SLO compliance
assert!(output.duration_ms <= 15000, "Build exceeded 15s SLO");
assert!(output.output_size() <= 1024 * 1024, "Output exceeded 1MB limit");
```

## Security

### Network Isolation

```rust
use cleanroom::policy::NetProfile;

// Offline mode (no network access)
let policy = Policy {
    net: NetProfile::Offline,
    ..Policy::default()
};

// Limited network (specific ports only)
let policy = Policy {
    net: NetProfile::Limited { allowed_ports: vec![80, 443] },
    ..Policy::default()
};
```

### Filesystem Constraints

```rust
use cleanroom::policy::FsProfile;

// Read-only filesystem
let policy = Policy {
    fs: FsProfile::ReadOnly { workdir: "workdir".to_string() },
    ..Policy::default()
};

// Writable workdir only
let policy = Policy {
    fs: FsProfile::Writable { workdir: "workdir".to_string() },
    ..Policy::default()
};
```

### Resource Limits

```rust
use cleanroom::policy::ResourceLimits;

// Strict limits for production
let limits = ResourceLimits::strict();
// CPU: 5 minutes, Memory: 512MB, File: 100MB, Processes: 1

// Custom limits
let limits = ResourceLimits {
    cpu_time_secs: Some(60),
    memory_bytes: Some(256 * 1024 * 1024), // 256MB
    file_size_bytes: Some(50 * 1024 * 1024), // 50MB
    process_count: Some(4),
};
```

## Testing

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_execution() {
        let result = run(["echo", "hello"]).unwrap();
        assert!(result.success());
        assert!(result.stdout.contains("hello"));
    }

    #[test]
    fn test_policy_validation() {
        let policy = Policy::locked();
        assert!(policy.validate().is_ok());
        assert!(!policy.allows_network());
        assert!(!policy.allows_writes());
    }
}
```

### Integration Tests

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_postgres_service() {
        let mut postgres = Postgres::new().unwrap();
        postgres.start().unwrap();
        
        assert!(postgres.health_check().unwrap());
        let conn_info = postgres.connection_info().unwrap();
        assert_eq!(conn_info.port, 5432);
        
        postgres.stop().unwrap();
    }

    #[test]
    fn test_scenario_with_services() {
        let mut postgres = Postgres::new().unwrap();
        let result = scenario("database test")
            .services(vec![Box::new(postgres)])
            .step("connect", ["psql", "-c", "SELECT 1"])
            .run()
            .unwrap();
        
        assert!(result.success());
    }
}
```

### Property Tests

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_command_execution_properties(
        cmd in "[a-zA-Z0-9]+",
        args in prop::collection::vec("[a-zA-Z0-9]+", 0..5)
    ) {
        let mut cmd_args = vec![cmd];
        cmd_args.extend(args);
        
        let result = run(cmd_args.iter().map(|s| s.as_str()));
        // Property: execution should not panic
        let _ = result;
    }
}
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Cleanroom Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Install Docker
        run: |
          sudo apt-get update
          sudo apt-get install -y docker.io
          sudo systemctl start docker
          sudo usermod -aG docker $USER
      
      - name: Run Cleanroom Tests
        run: |
          cargo make test-cleanroom
      
      - name: Upload Artifacts
        uses: actions/upload-artifact@v3
        if: failure()
        with:
          name: cleanroom-artifacts
          path: cleanroom-artifacts/
```

### GitLab CI

```yaml
stages:
  - test

variables:
  DOCKER_DRIVER: overlay2

test:
  stage: test
  image: docker:latest
  services:
    - docker:dind
  before_script:
    - docker info
  script:
    - cargo make test-cleanroom
  artifacts:
    when: on_failure
    paths:
      - cleanroom-artifacts/
    expire_in: 1 week
```

## Troubleshooting

### Common Issues

1. **Docker/Podman not available**
   ```bash
   # Install Docker
   sudo apt-get install docker.io
   sudo systemctl start docker
   sudo usermod -aG docker $USER
   
   # Install Podman
   sudo apt-get install podman
   ```

2. **Permission denied**
   ```bash
   # Add user to docker group
   sudo usermod -aG docker $USER
   newgrp docker
   
   # Or use rootless Podman
   podman --version
   ```

3. **Container startup timeout**
   ```rust
   // Increase startup timeout
   let postgres = Postgres::new()?
       .with_startup_timeout(Duration::from_secs(60));
   ```

4. **Resource limit exceeded**
   ```rust
   // Use more permissive policy
   let policy = Policy::permissive();
   
   // Or adjust specific limits
   let mut policy = Policy::default();
   policy.limits.memory_bytes = Some(1024 * 1024 * 1024); // 1GB
   ```

### Debugging

Enable debug logging:

```bash
export RUST_LOG=cleanroom=debug
cargo test
```

Collect forensics bundle:

```rust
use cleanroom::artifacts::ArtifactCollector;

let collector = ArtifactCollector::new()?;
let bundle = collector.collect(&run_info)?;
collector.save_bundle(&bundle, PathBuf::from("debug.json"))?;
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

### Development Setup

```bash
# Clone repository
git clone https://github.com/ggen/ggen.git
cd ggen/cleanroom

# Install dependencies
cargo make install-deps

# Run tests
cargo make test

# Run linting
cargo make lint

# Run benchmarks
cargo make bench
```

### Code Style

- Follow Rust formatting with `cargo fmt`
- Use clippy for linting: `cargo clippy`
- Write comprehensive tests
- Document all public APIs
- Use `cargo make` for all development workflows

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [testcontainers-rs](https://github.com/testcontainers/testcontainers-rs) for container testing inspiration
- [assert_cmd](https://github.com/assert-rs/assert_cmd) for command assertion patterns
- [tempfile](https://github.com/Stebalien/tempfile) for temporary file handling
- [bollard](https://github.com/fussybeaver/bollard) for Docker API integration

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for a list of changes.

## Roadmap

- [ ] Kubernetes backend support
- [ ] Multi-cloud testing capabilities
- [ ] Chaos engineering scenarios
- [ ] Performance benchmarking suite
- [ ] Security vulnerability scanning
- [ ] Advanced attestation features
- [ ] Web UI for test results
- [ ] Integration with popular CI/CD platforms
