//! Cleanroom: Hermetic, deterministic testing for CLIs and services.
//!
//! One API runs identically on laptop and CI by abstracting over Docker/Podman/local backends.
//! Covers the 80/20 of test engineering: orchestration, security, determinism, fixtures, coverage, and forensics.

#![forbid(unsafe_code)]
#![warn(missing_docs)]

pub mod prelude;
pub mod error;
pub mod config;
pub mod policy;
pub mod backend;
pub mod runtime;
pub mod scenario;
pub mod assertions;
pub mod coverage;
pub mod services;
pub mod attest;
pub mod artifacts;

// Re-exports for convenience
pub use backend::{Backend, AutoBackend};
pub use config::{CleanroomConfig, CleanroomConfigBuilder};
pub use error::{CleanroomError, Result};
pub use policy::{
    FsProfile, NetProfile, Policy, ProcProfile, ResourceLimits, RngProfile, TimeProfile,
};
// TODO: Implement Runner and RunnerConfig
pub use scenario::{scenario, RunResult, Scenario};

/// Run a single command with auto-detected backend.
///
/// This is the simplest entry point for running a command in a hermetic environment.
///
/// # Example
///
/// ```no_run
/// use cleanroom::{run, assertions::Assert};
///
/// let result = run(["echo", "hello world"])?;
/// assert!(result.success());
/// assert_eq!(result.stdout.trim(), "hello world");
/// # Ok::<(), cleanroom::CleanroomError>(())
/// ```
pub fn run<I, S>(args: I) -> Result<scenario::RunResult>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let backend = AutoBackend::detect()?;
    let args_vec: Vec<String> = args.into_iter().map(|s| s.as_ref().to_string()).collect();

    if args_vec.is_empty() {
        return Err(crate::error::BackendError::Runtime("no command provided".into()).into());
    }

    let cmd = crate::backend::Cmd::new(&args_vec[0]).args(&args_vec[1..]);
    let backend_result = backend.run_cmd(cmd)?;

    Ok(scenario::RunResult {
        exit_code: backend_result.exit_code,
        stdout: backend_result.stdout,
        stderr: backend_result.stderr,
        duration_ms: backend_result.duration_ms,
        steps: Vec::new(),
        redacted_env: Vec::new(),
        backend: backend.name().to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_echo() {
        let result = run(["echo", "hello"]);
        assert!(result.is_ok());

        let run_result = result.unwrap();
        assert_eq!(run_result.exit_code, 0);
        assert!(run_result.stdout.contains("hello"));
    }

    #[test]
    fn test_scenario_creation() {
        let _s = scenario("test scenario");
        // Just verify it compiles
    }
}