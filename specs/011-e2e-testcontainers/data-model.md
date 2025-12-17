# Data Model: End-to-End Testing with Testcontainers

**Feature**: 011-e2e-testcontainers | **Date**: 2025-12-16
**Status**: Phase 1 Complete

## Entity Relationship Overview

```
┌─────────────┐       ┌──────────────┐       ┌────────────┐
│  Platform   │──────▶│ TestRunner   │──────▶│ TestResult │
└─────────────┘       └──────────────┘       └────────────┘
                             │
                             ▼
┌─────────────┐       ┌──────────────┐       ┌────────────┐
│ TestFixture │──────▶│ TestExecution│──────▶│ GoldenFile │
└─────────────┘       └──────────────┘       └────────────┘
                             │
                             ▼
                      ┌──────────────┐
                      │ ContainerCtx │
                      └──────────────┘
```

## Core Entities

### 1. Platform

Represents a target operating system and architecture combination.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Human-readable name (e.g., "Linux x86_64") |
| `os` | `Os` | Operating system enum |
| `arch` | `Arch` | Architecture enum |
| `docker_available` | `bool` | Whether Docker is available |

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Os {
    Linux,
    Darwin,
    Windows,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arch {
    X86_64,
    Aarch64,
}

#[derive(Debug, Clone)]
pub struct Platform {
    pub name: String,
    pub os: Os,
    pub arch: Arch,
    pub docker_available: bool,
}

impl Platform {
    pub fn current() -> Result<Self, PlatformError>;
    pub fn supports_testcontainers(&self) -> bool;
    pub fn target_triple(&self) -> &'static str;
}
```

### 2. TestFixture

A test project with ontology, templates, and expected output.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Fixture identifier (e.g., "thesis-gen") |
| `path` | `PathBuf` | Path to fixture directory |
| `ontology_files` | `Vec<PathBuf>` | TTL/RDF ontology files |
| `template_files` | `Vec<PathBuf>` | Tera template files |
| `ggen_toml` | `PathBuf` | ggen.toml manifest |
| `golden_dir` | `PathBuf` | Expected output directory |

```rust
#[derive(Debug, Clone)]
pub struct TestFixture {
    pub name: String,
    pub path: PathBuf,
    pub ontology_files: Vec<PathBuf>,
    pub template_files: Vec<PathBuf>,
    pub ggen_toml: PathBuf,
    pub golden_dir: PathBuf,
}

impl TestFixture {
    pub fn load(path: impl AsRef<Path>) -> Result<Self, FixtureError>;
    pub fn copy_to_temp(&self) -> Result<TempDir, FixtureError>;
    pub fn golden_files(&self) -> Result<Vec<GoldenFile>, FixtureError>;
}
```

### 3. GoldenFile

Expected output file for comparison.

| Field | Type | Description |
|-------|------|-------------|
| `relative_path` | `PathBuf` | Path relative to output directory |
| `content` | `String` | Expected file content |
| `checksum` | `String` | SHA256 checksum |

```rust
#[derive(Debug, Clone)]
pub struct GoldenFile {
    pub relative_path: PathBuf,
    pub content: String,
    pub checksum: String,
}

impl GoldenFile {
    pub fn load(golden_dir: &Path, relative_path: &Path) -> Result<Self, GoldenError>;
    pub fn compare(&self, actual: &str) -> Result<(), GoldenMismatch>;
    pub fn update(&mut self, actual: &str) -> Result<(), GoldenError>;
}

#[derive(Debug)]
pub struct GoldenMismatch {
    pub file: PathBuf,
    pub expected: String,
    pub actual: String,
    pub diff: String,
}
```

### 4. ContainerConfig

Configuration for testcontainer instances.

| Field | Type | Description |
|-------|------|-------------|
| `image` | `String` | Docker image name |
| `tag` | `String` | Image tag |
| `env_vars` | `HashMap<String, String>` | Environment variables |
| `volumes` | `Vec<VolumeMount>` | Volume mounts |
| `startup_timeout` | `Duration` | Max wait for container ready |
| `command` | `Option<Vec<String>>` | Override entrypoint |

```rust
#[derive(Debug, Clone)]
pub struct ContainerConfig {
    pub image: String,
    pub tag: String,
    pub env_vars: HashMap<String, String>,
    pub volumes: Vec<VolumeMount>,
    pub startup_timeout: Duration,
    pub command: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct VolumeMount {
    pub host_path: PathBuf,
    pub container_path: PathBuf,
    pub read_only: bool,
}

impl Default for ContainerConfig {
    fn default() -> Self {
        Self {
            image: "ubuntu".to_string(),
            tag: "22.04".to_string(),
            env_vars: HashMap::new(),
            volumes: Vec::new(),
            startup_timeout: Duration::from_secs(120),
            command: None,
        }
    }
}
```

### 5. TestRunner

Orchestrates test execution across platforms.

| Field | Type | Description |
|-------|------|-------------|
| `platform` | `Platform` | Target platform |
| `container_config` | `Option<ContainerConfig>` | Container settings (if Docker) |
| `timeout` | `Duration` | Max test duration |
| `retry_count` | `u32` | Retry attempts on failure |

```rust
#[derive(Debug)]
pub struct TestRunner {
    pub platform: Platform,
    pub container_config: Option<ContainerConfig>,
    pub timeout: Duration,
    pub retry_count: u32,
}

impl TestRunner {
    pub fn new(platform: Platform) -> Self;
    pub fn with_container(self, config: ContainerConfig) -> Self;

    pub async fn run(&self, fixture: &TestFixture) -> Result<TestResult, RunnerError>;
    pub async fn run_ggen_sync(&self, project_dir: &Path) -> Result<SyncOutput, RunnerError>;
}
```

### 6. TestExecution

A single test run instance.

| Field | Type | Description |
|-------|------|-------------|
| `id` | `Uuid` | Unique execution ID |
| `fixture` | `String` | Fixture name |
| `platform` | `Platform` | Target platform |
| `started_at` | `DateTime<Utc>` | Execution start time |
| `ended_at` | `Option<DateTime<Utc>>` | Execution end time |
| `container_id` | `Option<String>` | Docker container ID |

```rust
#[derive(Debug)]
pub struct TestExecution {
    pub id: Uuid,
    pub fixture: String,
    pub platform: Platform,
    pub started_at: DateTime<Utc>,
    pub ended_at: Option<DateTime<Utc>>,
    pub container_id: Option<String>,
}

impl TestExecution {
    pub fn new(fixture: &str, platform: Platform) -> Self;
    pub fn duration(&self) -> Option<Duration>;
}
```

### 7. TestResult

Outcome of a test execution.

| Field | Type | Description |
|-------|------|-------------|
| `execution` | `TestExecution` | Execution metadata |
| `status` | `TestStatus` | Pass/Fail/Skip |
| `generated_files` | `Vec<PathBuf>` | Files created by ggen sync |
| `mismatches` | `Vec<GoldenMismatch>` | Golden file differences |
| `logs` | `String` | Captured stdout/stderr |
| `error` | `Option<String>` | Error message if failed |

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestStatus {
    Passed,
    Failed,
    Skipped,
    TimedOut,
}

#[derive(Debug)]
pub struct TestResult {
    pub execution: TestExecution,
    pub status: TestStatus,
    pub generated_files: Vec<PathBuf>,
    pub mismatches: Vec<GoldenMismatch>,
    pub logs: String,
    pub error: Option<String>,
}

impl TestResult {
    pub fn passed(execution: TestExecution, files: Vec<PathBuf>) -> Self;
    pub fn failed(execution: TestExecution, error: String, logs: String) -> Self;

    pub fn is_success(&self) -> bool;
    pub fn to_junit_xml(&self) -> String;
}
```

### 8. CrossPlatformComparison

Compares results across multiple platforms.

| Field | Type | Description |
|-------|------|-------------|
| `results` | `HashMap<Platform, TestResult>` | Results per platform |
| `reference_platform` | `Platform` | Baseline for comparison |
| `differences` | `Vec<PlatformDifference>` | Cross-platform diffs |

```rust
#[derive(Debug)]
pub struct CrossPlatformComparison {
    pub results: HashMap<Platform, TestResult>,
    pub reference_platform: Platform,
    pub differences: Vec<PlatformDifference>,
}

#[derive(Debug)]
pub struct PlatformDifference {
    pub file: PathBuf,
    pub platform_a: Platform,
    pub platform_b: Platform,
    pub diff: String,
}

impl CrossPlatformComparison {
    pub fn new(reference: Platform) -> Self;
    pub fn add_result(&mut self, platform: Platform, result: TestResult);
    pub fn compute_differences(&mut self) -> Result<(), ComparisonError>;
    pub fn is_deterministic(&self) -> bool;
}
```

## Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum E2EError {
    #[error("Platform detection failed: {0}")]
    PlatformError(#[from] PlatformError),

    #[error("Fixture error: {0}")]
    FixtureError(#[from] FixtureError),

    #[error("Golden file error: {0}")]
    GoldenError(#[from] GoldenError),

    #[error("Container error: {0}")]
    ContainerError(#[from] ContainerError),

    #[error("Runner error: {0}")]
    RunnerError(#[from] RunnerError),

    #[error("Test timed out after {0:?}")]
    Timeout(Duration),
}

#[derive(Debug, thiserror::Error)]
pub enum PlatformError {
    #[error("Unsupported OS: {0}")]
    UnsupportedOs(String),

    #[error("Docker not available")]
    DockerUnavailable,
}

#[derive(Debug, thiserror::Error)]
pub enum FixtureError {
    #[error("Fixture not found: {0}")]
    NotFound(PathBuf),

    #[error("Invalid ggen.toml: {0}")]
    InvalidManifest(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum GoldenError {
    #[error("Golden file not found: {0}")]
    NotFound(PathBuf),

    #[error("Checksum mismatch: expected {expected}, got {actual}")]
    ChecksumMismatch { expected: String, actual: String },

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum ContainerError {
    #[error("Failed to start container: {0}")]
    StartFailed(String),

    #[error("Container exited with code {0}")]
    ExitCode(i32),

    #[error("Failed to mount volume: {0}")]
    VolumeMountFailed(String),
}

#[derive(Debug, thiserror::Error)]
pub enum RunnerError {
    #[error("ggen sync failed: {0}")]
    SyncFailed(String),

    #[error("Container error: {0}")]
    Container(#[from] ContainerError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}
```

## Module Organization

```
crates/ggen-e2e/src/
├── lib.rs                 # Public API exports
├── platform.rs            # Platform, Os, Arch
├── fixture.rs             # TestFixture
├── golden.rs              # GoldenFile, GoldenMismatch
├── container.rs           # ContainerConfig, VolumeMount
├── runner.rs              # TestRunner, TestExecution
├── result.rs              # TestResult, TestStatus
├── comparison.rs          # CrossPlatformComparison
└── error.rs               # All error types
```

## Trait Abstractions

```rust
/// Trait for executing ggen in different environments
#[async_trait]
pub trait GgenExecutor: Send + Sync {
    async fn execute(&self, project_dir: &Path) -> Result<SyncOutput, RunnerError>;
    fn platform(&self) -> &Platform;
}

/// Native executor (macOS)
pub struct NativeExecutor {
    platform: Platform,
    ggen_path: PathBuf,
}

/// Container executor (Linux via testcontainers)
pub struct ContainerExecutor {
    platform: Platform,
    config: ContainerConfig,
}

impl GgenExecutor for NativeExecutor { ... }
impl GgenExecutor for ContainerExecutor { ... }
```

## State Transitions

```
TestExecution States:
┌──────────┐     ┌───────────┐     ┌──────────┐
│ Created  │────▶│  Running  │────▶│ Passed   │
└──────────┘     └───────────┘     └──────────┘
                       │
                       ├──────────▶ Failed
                       │
                       ├──────────▶ TimedOut
                       │
                       └──────────▶ Skipped
```

## Relationships Summary

| Entity A | Relationship | Entity B |
|----------|--------------|----------|
| Platform | determines | TestRunner behavior |
| TestFixture | contains | GoldenFile (many) |
| TestRunner | creates | TestExecution |
| TestExecution | produces | TestResult |
| TestResult | contains | GoldenMismatch (many) |
| CrossPlatformComparison | aggregates | TestResult (many) |
