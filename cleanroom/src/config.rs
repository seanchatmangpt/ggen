//! Configuration for cleanroom testing environments
//!
//! Source-of-truth merge order: test opts → `[package.metadata.cleanroom]` → env → defaults.
//! Reads `Cargo.toml` via `camino` + `toml` and validates paths, timeouts, env.

use crate::error::{CleanroomError, Result};
use camino::{Utf8Path, Utf8PathBuf};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;

/// Configuration for cleanroom testing environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomConfig {
    /// Test timeout duration in milliseconds
    pub timeout_ms: u64,

    /// Working directory for test execution
    pub workdir: Option<String>,

    /// Environment variables to set
    pub env: HashMap<String, String>,

    /// Network policy: "offline", "limited", or "open"
    pub net: String,

    /// Filesystem policy: "readonly" or "writable"
    pub fs: String,

    /// Process policy: "isolated" or "standard"
    pub proc: String,

    /// Enable performance benchmarking
    pub enable_benchmarking: bool,

    /// Enable detailed logging
    pub enable_logging: bool,

    /// Test concurrency level
    pub concurrency: usize,

    /// Backend preference: "auto", "docker", "podman", or "local"
    pub backend: String,

    /// Enable coverage collection
    pub enable_coverage: bool,

    /// Coverage output directory
    pub coverage_dir: Option<String>,

    /// Enable service fixtures
    pub enable_services: bool,
}

impl CleanroomConfig {
    /// Load configuration with source-of-truth precedence
    pub fn load() -> Result<Self> {
        // 1. Start with defaults
        let mut config = Self::default();

        // 2. Read from Cargo.toml [package.metadata.cleanroom]
        if let Ok(cargo_toml) = Self::load_from_cargo_toml() {
            config = Self::merge(config, cargo_toml);
        }

        // 3. Override from environment variables
        config = Self::apply_env_overrides(config)?;

        // 4. Validate configuration
        config.validate()?;

        Ok(config)
    }

    /// Load configuration from Cargo.toml metadata
    fn load_from_cargo_toml() -> Result<Self> {
        let manifest_dir = env::var("CARGO_MANIFEST_DIR").map_err(|_| {
            CleanroomError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "CARGO_MANIFEST_DIR not set",
            ))
        })?;

        let cargo_toml_path = Utf8Path::new(&manifest_dir).join("Cargo.toml");

        if !cargo_toml_path.exists() {
            return Ok(Self::default());
        }

        let content = std::fs::read_to_string(cargo_toml_path.as_std_path())
            .map_err(|e| CleanroomError::Io(e))?;

        let cargo_toml: CargoToml = toml::from_str(&content).map_err(|e| {
            CleanroomError::Policy(crate::error::PolicyError::Violation(format!(
                "invalid Cargo.toml: {}",
                e
            )))
        })?;

        if let Some(cleanroom_metadata) = cargo_toml.package.metadata.and_then(|m| m.cleanroom) {
            Ok(cleanroom_metadata)
        } else {
            Ok(Self::default())
        }
    }

    /// Apply environment variable overrides
    fn apply_env_overrides(mut config: Self) -> Result<Self> {
        if let Ok(timeout) = env::var("CLEANROOM_TIMEOUT_MS") {
            config.timeout_ms = timeout.parse().map_err(|_| {
                CleanroomError::Policy(crate::error::PolicyError::Violation(
                    "invalid CLEANROOM_TIMEOUT_MS value".to_string(),
                ))
            })?;
        }

        if let Ok(workdir) = env::var("CLEANROOM_WORKDIR") {
            config.workdir = Some(Utf8PathBuf::from(workdir));
        }

        if let Ok(net) = env::var("CLEANROOM_NET") {
            config.net = net;
        }

        if let Ok(fs) = env::var("CLEANROOM_FS") {
            config.fs = fs;
        }

        if let Ok(proc) = env::var("CLEANROOM_PROC") {
            config.proc = proc;
        }

        if let Ok(backend) = env::var("CLEANROOM_BACKEND") {
            config.backend = backend;
        }

        if let Ok(coverage) = env::var("CLEANROOM_COVERAGE") {
            config.enable_coverage = coverage.parse().map_err(|_| {
                CleanroomError::Policy(crate::error::PolicyError::Violation(
                    "invalid CLEANROOM_COVERAGE value".to_string(),
                ))
            })?;
        }

        if let Ok(coverage_dir) = env::var("CLEANROOM_COVERAGE_DIR") {
            config.coverage_dir = Some(Utf8PathBuf::from(coverage_dir));
        }

        if let Ok(services) = env::var("CLEANROOM_SERVICES") {
            config.enable_services = services.parse().map_err(|_| {
                CleanroomError::Policy(crate::error::PolicyError::Violation(
                    "invalid CLEANROOM_SERVICES value".to_string(),
                ))
            })?;
        }

        Ok(config)
    }

    /// Validate configuration values
    fn validate(&self) -> Result<()> {
        if self.timeout_ms == 0 {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation("timeout_ms must be > 0".to_string()),
            ));
        }

        if self.concurrency == 0 {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation("concurrency must be > 0".to_string()),
            ));
        }

        if !["offline", "limited", "open"].contains(&self.net.as_str()) {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation(
                    "net must be 'offline', 'limited', or 'open'".to_string(),
                ),
            ));
        }

        if !["readonly", "writable"].contains(&self.fs.as_str()) {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation(
                    "fs must be 'readonly' or 'writable'".to_string(),
                ),
            ));
        }

        if !["isolated", "standard"].contains(&self.proc.as_str()) {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation(
                    "proc must be 'isolated' or 'standard'".to_string(),
                ),
            ));
        }

        if !["auto", "docker", "podman", "local"].contains(&self.backend.as_str()) {
            return Err(CleanroomError::Policy(
                crate::error::PolicyError::Violation(
                    "backend must be 'auto', 'docker', 'podman', or 'local'".to_string(),
                ),
            ));
        }

        Ok(())
    }

    /// Merge two configurations (later overrides earlier)
    fn merge(mut base: Self, override_: Self) -> Self {
        if override_.timeout_ms != Self::default().timeout_ms {
            base.timeout_ms = override_.timeout_ms;
        }
        if override_.workdir.is_some() {
            base.workdir = override_.workdir;
        }
        if !override_.env.is_empty() {
            base.env.extend(override_.env);
        }
        if override_.net != Self::default().net {
            base.net = override_.net;
        }
        if override_.fs != Self::default().fs {
            base.fs = override_.fs;
        }
        if override_.proc != Self::default().proc {
            base.proc = override_.proc;
        }
        if override_.enable_benchmarking != Self::default().enable_benchmarking {
            base.enable_benchmarking = override_.enable_benchmarking;
        }
        if override_.enable_logging != Self::default().enable_logging {
            base.enable_logging = override_.enable_logging;
        }
        if override_.concurrency != Self::default().concurrency {
            base.concurrency = override_.concurrency;
        }
        if override_.backend != Self::default().backend {
            base.backend = override_.backend;
        }
        if override_.enable_coverage != Self::default().enable_coverage {
            base.enable_coverage = override_.enable_coverage;
        }
        if override_.coverage_dir.is_some() {
            base.coverage_dir = override_.coverage_dir;
        }
        if override_.enable_services != Self::default().enable_services {
            base.enable_services = override_.enable_services;
        }
        base
    }
}

impl Default for CleanroomConfig {
    fn default() -> Self {
        Self {
            timeout_ms: 30000,
            workdir: None,
            env: HashMap::new(),
            net: "offline".to_string(),
            fs: "readonly".to_string(),
            proc: "isolated".to_string(),
            enable_benchmarking: true,
            enable_logging: true,
            concurrency: 4,
            backend: "auto".to_string(),
            enable_coverage: false,
            coverage_dir: None,
            enable_services: false,
        }
    }
}

/// Cargo.toml structure for parsing metadata
#[derive(Deserialize)]
struct CargoToml {
    package: Package,
}

#[derive(Deserialize)]
struct Package {
    metadata: Option<Metadata>,
}

#[derive(Deserialize)]
struct Metadata {
    cleanroom: Option<CleanroomConfig>,
}

/// Builder for cleanroom configuration
pub struct CleanroomConfigBuilder {
    config: CleanroomConfig,
}

impl CleanroomConfigBuilder {
    /// Create a new config builder
    pub fn new() -> Self {
        Self {
            config: CleanroomConfig::default(),
        }
    }

    /// Set timeout duration
    pub fn timeout_ms(mut self, ms: u64) -> Self {
        self.config.timeout_ms = ms;
        self
    }

    /// Set working directory
    pub fn workdir(mut self, dir: impl Into<Utf8PathBuf>) -> Self {
        self.config.workdir = Some(dir.into());
        self
    }

    /// Add environment variable
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config.env.insert(key.into(), value.into());
        self
    }

    /// Set network policy
    pub fn net(mut self, policy: impl Into<String>) -> Self {
        self.config.net = policy.into();
        self
    }

    /// Set filesystem policy
    pub fn fs(mut self, policy: impl Into<String>) -> Self {
        self.config.fs = policy.into();
        self
    }

    /// Set process policy
    pub fn proc(mut self, policy: impl Into<String>) -> Self {
        self.config.proc = policy.into();
        self
    }

    /// Enable or disable benchmarking
    pub fn benchmarking(mut self, enabled: bool) -> Self {
        self.config.enable_benchmarking = enabled;
        self
    }

    /// Enable or disable logging
    pub fn logging(mut self, enabled: bool) -> Self {
        self.config.enable_logging = enabled;
        self
    }

    /// Set concurrency level
    pub fn concurrency(mut self, level: usize) -> Self {
        self.config.concurrency = level;
        self
    }

    /// Set backend preference
    pub fn backend(mut self, backend: impl Into<String>) -> Self {
        self.config.backend = backend.into();
        self
    }

    /// Enable or disable coverage
    pub fn coverage(mut self, enabled: bool) -> Self {
        self.config.enable_coverage = enabled;
        self
    }

    /// Set coverage output directory
    pub fn coverage_dir(mut self, dir: impl Into<Utf8PathBuf>) -> Self {
        self.config.coverage_dir = Some(dir.into());
        self
    }

    /// Enable or disable services
    pub fn services(mut self, enabled: bool) -> Self {
        self.config.enable_services = enabled;
        self
    }

    /// Build the configuration
    pub fn build(self) -> CleanroomConfig {
        self.config
    }
}

impl Default for CleanroomConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}
