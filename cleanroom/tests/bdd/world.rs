use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;
use serde_json::Value;

/// World state for Cleanroom BDD tests
///
/// Maintains state across scenario steps including:
/// - Temporary directories for isolated test runs
/// - Command outputs and exit codes
/// - Captured file contents and artifacts
/// - Backend configuration and state
/// - Policy and constraint settings
/// - Service management and health checks
/// - Coverage and attestation data
#[derive(Debug, Default, cucumber::World)]
pub struct CleanroomWorld {
    /// Temporary directory for test isolation (kept alive for test duration)
    #[allow(dead_code)]
    pub temp_dir: Option<TempDir>,

    /// Project directory path (usually temp_dir.path())
    pub project_dir: PathBuf,

    /// Last command output
    pub last_output: Option<Output>,

    /// Last command exit code
    pub last_exit_code: Option<i32>,

    /// Captured file contents by path
    pub captured_files: HashMap<String, String>,

    /// Captured artifacts for verification
    pub captured_artifacts: HashMap<String, Vec<u8>>,

    /// Captured output hashes for determinism testing
    pub captured_hashes: Vec<String>,

    /// Current backend type being used
    pub current_backend: Option<String>,

    /// Policy settings for current test
    pub policy_settings: HashMap<String, String>,

    /// Network constraints
    pub network_constraints: Vec<String>,

    /// Filesystem constraints
    pub filesystem_constraints: Vec<String>,

    /// Mock server for external service testing
    #[allow(dead_code)]
    pub mock_server: Option<mockito::Server>,

    /// Fixture project information
    pub fixture_project: Option<String>,

    /// Binary path for the fixture project
    pub binary_path: Option<String>,

    /// Environment variables
    pub environment: HashMap<String, String>,

    /// Available backends
    pub available_backends: Vec<String>,

    /// Docker availability
    pub docker_available: Option<bool>,

    /// Podman availability
    pub podman_available: Option<bool>,

    /// Scenario definitions
    pub scenarios: HashMap<String, ScenarioDefinition>,

    /// Current scenario being executed
    pub current_scenario: Option<String>,

    /// Scenario execution results
    pub scenario_results: HashMap<String, ScenarioResult>,

    /// Snapshots for comparison
    pub snapshots: HashMap<String, String>,

    /// Coverage data
    pub coverage_data: HashMap<String, Value>,

    /// Service definitions
    pub services: HashMap<String, ServiceDefinition>,

    /// Service health status
    pub service_health: HashMap<String, bool>,

    /// Redaction patterns
    pub redaction_patterns: Vec<String>,

    /// RNG seed for deterministic testing
    pub rng_seed: Option<u64>,

    /// Timeout settings
    pub timeout_ms: Option<u64>,

    /// Step timeout settings
    pub step_timeout_ms: Option<u64>,

    /// Output limits
    pub max_output_bytes: Option<usize>,

    /// Trace data
    pub trace_data: Vec<TraceSpan>,

    /// Run ID for artifact organization
    pub run_id: Option<String>,

    /// Artifact directory
    pub artifact_dir: Option<PathBuf>,

    /// JSON report data
    pub json_report: Option<Value>,

    /// Skip reasons
    pub skip_reasons: HashMap<String, String>,

    /// Engine matrix results
    pub engine_results: HashMap<String, EngineResult>,

    /// Typestate policy level
    pub policy_typestate: Option<PolicyTypestate>,
}

/// Scenario definition for DSL
#[derive(Debug, Clone)]
pub struct ScenarioDefinition {
    pub name: String,
    pub steps: Vec<ScenarioStep>,
    pub concurrent: bool,
    pub continue_on_fail: bool,
}

/// Individual step in a scenario
#[derive(Debug, Clone)]
pub struct ScenarioStep {
    pub name: String,
    pub args: Vec<String>,
    pub expect: String,
    pub max_output: Option<usize>,
}

/// Scenario execution result
#[derive(Debug, Clone)]
pub struct ScenarioResult {
    pub scenario_name: String,
    pub steps_succeeded: Vec<String>,
    pub steps_failed: Vec<String>,
    pub total_duration_ms: u64,
    pub aggregated_duration_ms: u64,
}

/// Service definition
#[derive(Debug, Clone)]
pub struct ServiceDefinition {
    pub name: String,
    pub image: String,
    pub port: Option<u16>,
    pub health_check: Option<String>,
}

/// Trace span for structured logging
#[derive(Debug, Clone)]
pub struct TraceSpan {
    pub name: String,
    pub duration_ms: u64,
    pub status: String,
}

/// Engine result for matrix testing
#[derive(Debug, Clone)]
pub struct EngineResult {
    pub engine: String,
    pub success: bool,
    pub duration_ms: u64,
    pub metadata: HashMap<String, String>,
}

/// Policy typestate levels
#[derive(Debug, Clone, PartialEq)]
pub enum PolicyTypestate {
    Restricted,
    Permissive,
}

impl CleanroomWorld {
    /// Create a new world with a temporary directory
    #[allow(dead_code)]
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().to_path_buf();
        let run_id = uuid::Uuid::new_v4().to_string();
        let artifact_dir = project_dir.join("artifacts").join(&run_id);

        Self {
            temp_dir: Some(temp_dir),
            project_dir,
            run_id: Some(run_id),
            artifact_dir: Some(artifact_dir),
            ..Default::default()
        }
    }

    /// Get the project directory path
    #[allow(dead_code)]
    pub fn project_dir(&self) -> &PathBuf {
        &self.project_dir
    }

    /// Set current backend type
    pub fn set_backend(&mut self, backend: String) {
        self.current_backend = Some(backend);
    }

    /// Capture file content for later comparison
    pub fn capture_file(&mut self, path: &str, content: String) {
        self.captured_files.insert(path.to_string(), content);
    }

    /// Capture binary artifact for later verification
    pub fn capture_artifact(&mut self, path: &str, content: Vec<u8>) {
        self.captured_artifacts.insert(path.to_string(), content);
    }

    /// Capture output hash for determinism testing
    pub fn capture_hash(&mut self, hash: String) {
        self.captured_hashes.push(hash);
    }

    /// Get last captured hash
    #[allow(dead_code)]
    pub fn last_hash(&self) -> Option<&String> {
        self.captured_hashes.last()
    }

    /// Check if last command succeeded
    pub fn last_command_succeeded(&self) -> bool {
        self.last_exit_code == Some(0)
    }

    /// Get last command stdout as string
    pub fn last_stdout(&self) -> String {
        self.last_output
            .as_ref()
            .map(|output| String::from_utf8_lossy(&output.stdout).to_string())
            .unwrap_or_default()
    }

    /// Get last command stderr as string
    pub fn last_stderr(&self) -> String {
        self.last_output
            .as_ref()
            .map(|output| String::from_utf8_lossy(&output.stderr).to_string())
            .unwrap_or_default()
    }

    /// Set policy constraint
    pub fn set_policy(&mut self, key: String, value: String) {
        self.policy_settings.insert(key, value);
    }

    /// Add network constraint
    pub fn add_network_constraint(&mut self, constraint: String) {
        self.network_constraints.push(constraint);
    }

    /// Add filesystem constraint
    pub fn add_filesystem_constraint(&mut self, constraint: String) {
        self.filesystem_constraints.push(constraint);
    }

    /// Setup mock HTTP server for external service testing
    pub fn setup_mock_server(&mut self) -> String {
        let mut server = mockito::Server::new();
        let server_url = server.url();
        
        // Keep server alive for test duration
        self.mock_server = Some(server);
        server_url
    }

    /// Set fixture project
    pub fn set_fixture_project(&mut self, project: String, binary: String) {
        self.fixture_project = Some(project);
        self.binary_path = Some(binary);
    }

    /// Set environment variable
    pub fn set_env(&mut self, key: String, value: String) {
        self.environment.insert(key, value);
    }

    /// Unset environment variable
    pub fn unset_env(&mut self, key: String) {
        self.environment.remove(&key);
    }

    /// Check if backend is available
    pub fn is_backend_available(&self, backend: &str) -> bool {
        match backend {
            "docker" => self.docker_available.unwrap_or(false),
            "podman" => self.podman_available.unwrap_or(false),
            "local" => true,
            "auto" => self.docker_available.unwrap_or(false) || self.podman_available.unwrap_or(false) || true,
            _ => false,
        }
    }

    /// Add scenario definition
    pub fn add_scenario(&mut self, scenario: ScenarioDefinition) {
        self.scenarios.insert(scenario.name.clone(), scenario);
    }

    /// Set current scenario
    pub fn set_current_scenario(&mut self, name: String) {
        self.current_scenario = Some(name);
    }

    /// Add snapshot
    pub fn add_snapshot(&mut self, name: String, content: String) {
        self.snapshots.insert(name, content);
    }

    /// Get snapshot
    pub fn get_snapshot(&self, name: &str) -> Option<&String> {
        self.snapshots.get(name)
    }

    /// Add service definition
    pub fn add_service(&mut self, service: ServiceDefinition) {
        self.services.insert(service.name.clone(), service);
    }

    /// Set service health
    pub fn set_service_health(&mut self, name: String, healthy: bool) {
        self.service_health.insert(name, healthy);
    }

    /// Add redaction pattern
    pub fn add_redaction_pattern(&mut self, pattern: String) {
        self.redaction_patterns.push(pattern);
    }

    /// Set RNG seed
    pub fn set_rng_seed(&mut self, seed: u64) {
        self.rng_seed = Some(seed);
    }

    /// Set timeout
    pub fn set_timeout(&mut self, timeout_ms: u64) {
        self.timeout_ms = Some(timeout_ms);
    }

    /// Set step timeout
    pub fn set_step_timeout(&mut self, timeout_ms: u64) {
        self.step_timeout_ms = Some(timeout_ms);
    }

    /// Set output limit
    pub fn set_max_output(&mut self, max_bytes: usize) {
        self.max_output_bytes = Some(max_bytes);
    }

    /// Add trace span
    pub fn add_trace_span(&mut self, span: TraceSpan) {
        self.trace_data.push(span);
    }

    /// Set policy typestate
    pub fn set_policy_typestate(&mut self, typestate: PolicyTypestate) {
        self.policy_typestate = Some(typestate);
    }

    /// Add engine result
    pub fn add_engine_result(&mut self, result: EngineResult) {
        self.engine_results.insert(result.engine.clone(), result);
    }

    /// Set skip reason
    pub fn set_skip_reason(&mut self, scenario: String, reason: String) {
        self.skip_reasons.insert(scenario, reason);
    }

    /// Set JSON report
    pub fn set_json_report(&mut self, report: Value) {
        self.json_report = Some(report);
    }

    /// Debug dump of world state
    pub fn debug_dump(&self) {
        eprintln!("=== CLEANROOM WORLD STATE ===");
        eprintln!("Project dir: {}", self.project_dir.display());
        eprintln!("Run ID: {:?}", self.run_id);
        eprintln!("Backend: {:?}", self.current_backend);
        eprintln!("Last exit code: {:?}", self.last_exit_code);
        eprintln!("Policy settings: {:?}", self.policy_settings);
        eprintln!("Network constraints: {:?}", self.network_constraints);
        eprintln!("Filesystem constraints: {:?}", self.filesystem_constraints);
        eprintln!("Fixture project: {:?}", self.fixture_project);
        eprintln!("Binary path: {:?}", self.binary_path);
        eprintln!("Available backends: {:?}", self.available_backends);
        eprintln!("Docker available: {:?}", self.docker_available);
        eprintln!("Podman available: {:?}", self.podman_available);
        eprintln!("Current scenario: {:?}", self.current_scenario);
        eprintln!("Scenarios: {:?}", self.scenarios.keys());
        eprintln!("Snapshots: {:?}", self.snapshots.keys());
        eprintln!("Services: {:?}", self.services.keys());
        eprintln!("RNG seed: {:?}", self.rng_seed);
        eprintln!("Timeout: {:?}", self.timeout_ms);
        eprintln!("Step timeout: {:?}", self.step_timeout_ms);
        eprintln!("Max output: {:?}", self.max_output_bytes);
        eprintln!("Trace spans: {}", self.trace_data.len());
        eprintln!("Captured files: {:?}", self.captured_files.keys());
        eprintln!("Captured artifacts: {:?}", self.captured_artifacts.keys());
        eprintln!("Captured hashes: {}", self.captured_hashes.len());
        eprintln!("=============================");
    }
}

// World trait is automatically implemented by the derive macro
