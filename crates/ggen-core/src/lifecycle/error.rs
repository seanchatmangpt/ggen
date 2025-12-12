//! Error types for lifecycle management
//!
//! This module provides rich, context-aware error types for all lifecycle operations,
//! following Rust best practices with thiserror.

use std::path::PathBuf;

/// Main error type for lifecycle operations
#[derive(Debug, thiserror::Error)]
pub enum LifecycleError {
    /// Error loading or parsing make.toml configuration
    #[error("Failed to load configuration from {path}: {source}")]
    ConfigLoad {
        path: PathBuf,
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Error parsing make.toml TOML format
    #[error("Failed to parse TOML configuration at {path}: {source}")]
    ConfigParse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    /// Phase not found in configuration
    #[error("Phase '{phase}' not found in configuration")]
    PhaseNotFound { phase: String },

    /// Phase has no commands defined
    #[error("Phase '{phase}' has no commands defined")]
    NoCommands { phase: String },

    /// Command execution failure
    #[error("Command failed in phase '{phase}': {command}\n  Exit code: {exit_code}\n  Stderr: {stderr}")]
    CommandFailed {
        phase: String,
        command: String,
        exit_code: i32,
        stderr: String,
    },

    /// Command spawn failure
    #[error("Failed to spawn command in phase '{phase}': {command}")]
    CommandSpawn {
        phase: String,
        command: String,
        #[source]
        source: std::io::Error,
    },

    /// Hook recursion detected (circular dependency)
    #[error("Hook recursion detected: phase '{phase}' called recursively through chain: {}", chain.join(" -> "))]
    HookRecursion { phase: String, chain: Vec<String> },

    /// Hook execution failed
    #[error("Hook failed for phase '{phase}': {hook_phase}")]
    HookFailed {
        phase: String,
        hook_phase: String,
        #[source]
        source: Box<LifecycleError>,
    },

    /// State file load error
    #[error("Failed to load state from {path}: {source}")]
    StateLoad {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// State file parse error
    #[error("Failed to parse state JSON from {path}: {source}")]
    StateParse {
        path: PathBuf,
        #[source]
        source: serde_json::Error,
    },

    /// State file save error
    #[error("Failed to save state to {path}: {source}")]
    StateSave {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Cache directory error
    #[error("Invalid cache path: {path}")]
    InvalidCachePath { path: PathBuf },

    /// Cache creation error
    #[error("Failed to create cache directory for phase '{phase}': {source}")]
    CacheCreate {
        phase: String,
        #[source]
        source: std::io::Error,
    },

    /// File I/O error with context
    #[error("File I/O error at {path}: {source}")]
    FileIo {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Directory creation error
    #[error("Failed to create directory {path}: {source}")]
    DirectoryCreate {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Make.toml load error
    #[error("Failed to load make.toml from {path}: {source}")]
    MakeTomlLoad {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Make.toml parse error
    #[error("Failed to parse make.toml at {path}: {source}")]
    MakeTomlParse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    /// Workspace not found
    #[error("Workspace '{workspace}' not found in configuration")]
    WorkspaceNotFound { workspace: String },

    /// Workspace path error
    #[error("Invalid workspace path '{workspace}': {path}")]
    WorkspacePath { workspace: String, path: PathBuf },

    /// Parallel execution error
    #[error("Parallel execution failed for workspace '{workspace}': {source}")]
    ParallelExecution {
        workspace: String,
        #[source]
        source: Box<LifecycleError>,
    },

    /// Environment variable error
    #[error("Invalid environment variable: {key}={value}")]
    InvalidEnv { key: String, value: String },

    /// Mutex poisoned error (should never happen in normal operation)
    #[error("Internal error: mutex poisoned in phase '{phase}'")]
    MutexPoisoned { phase: String },

    /// Generic I/O error
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Dependency cycle detected
    #[error("Circular dependency detected in phase dependencies: {phases}")]
    DependencyCycle { phases: String },

    /// Generic error for compatibility
    #[error("{0}")]
    Other(String),
}

impl LifecycleError {
    /// Create a config load error
    pub fn config_load(
        path: impl Into<PathBuf>, source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self::ConfigLoad {
            path: path.into(),
            source: Box::new(source),
        }
    }

    /// Create a config parse error
    pub fn config_parse(path: impl Into<PathBuf>, source: toml::de::Error) -> Self {
        Self::ConfigParse {
            path: path.into(),
            source,
        }
    }

    /// Create a phase not found error
    pub fn phase_not_found(phase: impl Into<String>) -> Self {
        Self::PhaseNotFound {
            phase: phase.into(),
        }
    }

    /// Create a command failed error
    pub fn command_failed(
        phase: impl Into<String>, command: impl Into<String>, exit_code: i32,
        stderr: impl Into<String>,
    ) -> Self {
        Self::CommandFailed {
            phase: phase.into(),
            command: command.into(),
            exit_code,
            stderr: stderr.into(),
        }
    }

    /// Create a command spawn error
    pub fn command_spawn(
        phase: impl Into<String>, command: impl Into<String>, source: std::io::Error,
    ) -> Self {
        Self::CommandSpawn {
            phase: phase.into(),
            command: command.into(),
            source,
        }
    }

    /// Create a hook recursion error with call chain
    pub fn hook_recursion_with_chain(phase: impl Into<String>, chain: Vec<String>) -> Self {
        Self::HookRecursion {
            phase: phase.into(),
            chain,
        }
    }

    /// Create a simple hook recursion error (for backward compatibility)
    pub fn hook_recursion(phase: impl Into<String>) -> Self {
        let phase_str = phase.into();
        Self::HookRecursion {
            phase: phase_str.clone(),
            chain: vec![phase_str],
        }
    }

    /// Create a state load error
    pub fn state_load(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::StateLoad {
            path: path.into(),
            source,
        }
    }

    /// Create a state parse error
    pub fn state_parse(path: impl Into<PathBuf>, source: serde_json::Error) -> Self {
        Self::StateParse {
            path: path.into(),
            source,
        }
    }

    /// Create a state save error
    pub fn state_save(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::StateSave {
            path: path.into(),
            source,
        }
    }

    /// Create an invalid cache path error
    pub fn invalid_cache_path(path: impl Into<PathBuf>) -> Self {
        Self::InvalidCachePath { path: path.into() }
    }

    /// Create a cache create error
    pub fn cache_create(phase: impl Into<String>, source: std::io::Error) -> Self {
        Self::CacheCreate {
            phase: phase.into(),
            source,
        }
    }

    /// Create a file I/O error
    pub fn file_io(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::FileIo {
            path: path.into(),
            source,
        }
    }

    /// Create a directory creation error
    pub fn directory_create(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::DirectoryCreate {
            path: path.into(),
            source,
        }
    }

    /// Create a make.toml load error
    pub fn make_toml_load(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::MakeTomlLoad {
            path: path.into(),
            source,
        }
    }

    /// Create a make.toml parse error
    pub fn make_toml_parse(path: impl Into<PathBuf>, source: toml::de::Error) -> Self {
        Self::MakeTomlParse {
            path: path.into(),
            source,
        }
    }

    /// Create a parallel execution error
    pub fn parallel_execution(workspace: impl Into<String>, source: LifecycleError) -> Self {
        Self::ParallelExecution {
            workspace: workspace.into(),
            source: Box::new(source),
        }
    }

    /// Create a dependency cycle error
    pub fn dependency_cycle(phases: impl Into<String>) -> Self {
        Self::DependencyCycle {
            phases: phases.into(),
        }
    }
}

/// Result type alias for lifecycle operations
pub type Result<T> = std::result::Result<T, LifecycleError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_phase_not_found_error() {
        let err = LifecycleError::phase_not_found("build");
        assert_eq!(err.to_string(), "Phase 'build' not found in configuration");
    }

    #[test]
    fn test_command_failed_error() {
        let err = LifecycleError::command_failed("test", "cargo test", 101, "test failed");
        let msg = err.to_string();
        assert!(msg.contains("Command failed"));
        assert!(msg.contains("Exit code: 101"));
        assert!(msg.contains("test failed"));
    }

    #[test]
    fn test_hook_recursion_error() {
        let err = LifecycleError::hook_recursion("build");
        let msg = err.to_string();
        assert!(msg.contains("Hook recursion detected"));
        assert!(msg.contains("build"));
    }

    #[test]
    fn test_hook_recursion_with_chain() {
        let chain = vec![
            "init".to_string(),
            "setup".to_string(),
            "build".to_string(),
            "init".to_string(),
        ];
        let err = LifecycleError::hook_recursion_with_chain("init", chain);
        let msg = err.to_string();
        assert!(msg.contains("init -> setup -> build -> init"));
    }

    #[test]
    fn test_state_errors() {
        let path = PathBuf::from("/tmp/state.json");

        // Test state load error
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let err = LifecycleError::state_load(&path, io_err);
        assert!(err.to_string().contains("Failed to load state"));

        // Test state save error
        let io_err = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "denied");
        let err = LifecycleError::state_save(&path, io_err);
        assert!(err.to_string().contains("Failed to save state"));
    }

    #[test]
    fn test_make_toml_errors() {
        let path = PathBuf::from("/tmp/make.toml");

        // Test load error
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let err = LifecycleError::make_toml_load(&path, io_err);
        assert!(err.to_string().contains("Failed to load make.toml"));
        assert!(err.to_string().contains("/tmp/make.toml"));
    }

    #[test]
    fn test_cache_errors() {
        let path = PathBuf::from("/tmp/cache");

        // Test invalid cache path
        let err = LifecycleError::invalid_cache_path(&path);
        assert!(err.to_string().contains("Invalid cache path"));

        // Test cache create error
        let io_err = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "denied");
        let err = LifecycleError::cache_create("build", io_err);
        assert!(err.to_string().contains("Failed to create cache directory"));
        assert!(err.to_string().contains("build"));
    }

    #[test]
    fn test_config_load_error() {
        let path = PathBuf::from("/tmp/make.toml");
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let err = LifecycleError::config_load(&path, Box::new(io_err));
        assert!(err.to_string().contains("Failed to load configuration"));
        assert!(err.to_string().contains("/tmp/make.toml"));
    }

    #[test]
    fn test_config_parse_error() {
        let path = PathBuf::from("/tmp/make.toml");
        let parse_err = toml::de::Error::custom("EOF");
        let err = LifecycleError::config_parse(&path, parse_err);
        assert!(err.to_string().contains("Failed to parse TOML"));
        assert!(err.to_string().contains("/tmp/make.toml"));
    }

    #[test]
    fn test_command_spawn_error() {
        let io_err = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "permission denied");
        let err = LifecycleError::command_spawn("build", "cargo build", io_err);
        assert!(err.to_string().contains("Failed to spawn command"));
        assert!(err.to_string().contains("build"));
        assert!(err.to_string().contains("cargo build"));
    }

    #[test]
    fn test_parallel_execution_error() {
        let inner_err = LifecycleError::phase_not_found("test");
        let err = LifecycleError::parallel_execution("workspace1", inner_err);
        assert!(err.to_string().contains("Parallel execution failed"));
        assert!(err.to_string().contains("workspace1"));
    }

    #[test]
    fn test_dependency_cycle_error() {
        let err = LifecycleError::dependency_cycle("init -> setup -> build -> init");
        assert!(err.to_string().contains("Dependency cycle detected"));
        assert!(err.to_string().contains("init -> setup -> build -> init"));
    }

    #[test]
    fn test_error_downcasting() {
        let err = LifecycleError::phase_not_found("build");
        let err_ref: &dyn std::error::Error = &err;
        assert!(err_ref.source().is_none());

        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "not found");
        let err = LifecycleError::state_load("/tmp/state.json", io_err);
        let err_ref: &dyn std::error::Error = &err;
        assert!(err_ref.source().is_some());
    }

    #[test]
    fn test_error_display() {
        let lifecycle_err = LifecycleError::phase_not_found("test");
        let err_str = lifecycle_err.to_string();
        assert!(err_str.contains("Phase 'test' not found"));
    }
}
