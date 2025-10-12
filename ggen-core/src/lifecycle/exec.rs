//! Phase execution with hooks and state management

use super::{cache::cache_key, error::*, loader::load_make, model::*, state::*};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

/// Execution context for lifecycle phases (thread-safe)
pub struct Context {
    pub root: PathBuf,
    pub make: Arc<Make>,
    pub state_path: PathBuf,
    pub env: Vec<(String, String)>,
    /// Hook recursion guard (thread-safe for parallel execution)
    hook_guard: Arc<Mutex<HashSet<String>>>,
}

impl Context {
    /// Create new context with empty hook guard
    pub fn new(
        root: PathBuf, make: Arc<Make>, state_path: PathBuf, env: Vec<(String, String)>,
    ) -> Self {
        Self {
            root,
            make,
            state_path,
            env,
            hook_guard: Arc::new(Mutex::new(HashSet::new())),
        }
    }

    /// Check for hook recursion and add to guard
    fn enter_phase(&self, phase: &str) -> Result<()> {
        let mut guard = self
            .hook_guard
            .lock()
            .map_err(|_| LifecycleError::MutexPoisoned {
                phase: phase.to_string(),
            })?;

        if guard.contains(phase) {
            return Err(LifecycleError::hook_recursion(phase));
        }
        guard.insert(phase.to_string());
        Ok(())
    }

    /// Remove phase from guard after completion
    fn exit_phase(&self, phase: &str) {
        if let Ok(mut guard) = self.hook_guard.lock() {
            guard.remove(phase);
        }
        // If mutex is poisoned, we're in a panic scenario anyway
    }
}

/// Run a single lifecycle phase with hooks
pub fn run_phase(ctx: &Context, phase_name: &str) -> Result<()> {
    // Check for hook recursion FIRST
    ctx.enter_phase(phase_name)?;

    // Ensure we exit phase even on error
    let result = run_phase_internal(ctx, phase_name);
    ctx.exit_phase(phase_name);
    result
}

/// Internal phase execution (called after recursion check)
fn run_phase_internal(ctx: &Context, phase_name: &str) -> Result<()> {
    let phase = ctx
        .make
        .lifecycle
        .get(phase_name)
        .ok_or_else(|| LifecycleError::phase_not_found(phase_name))?;

    // Get commands for this phase using new Phase::commands() method
    let cmds = phase.commands();
    if cmds.is_empty() {
        tracing::warn!(phase = %phase_name, "Phase has no commands");
        return Ok(());
    }

    // Run before hooks
    run_before_hooks(ctx, phase_name)?;

    // Generate cache key
    let key = cache_key(phase_name, &cmds, &ctx.env, &[]);

    // Execute phase commands
    let started = current_time_ms()?;
    let timer = Instant::now();

    tracing::info!(phase = %phase_name, "Starting phase execution");
    for cmd in &cmds {
        tracing::debug!(phase = %phase_name, command = %cmd, "Executing command");
        execute_command(cmd, &ctx.root, &ctx.env)?;
    }

    let duration = timer.elapsed().as_millis();
    tracing::info!(
        phase = %phase_name,
        duration_ms = duration,
        "Phase completed successfully"
    );

    // Update state
    let mut state = load_state(&ctx.state_path)?;
    state.record_run(phase_name.to_string(), started, duration, true);
    state.add_cache_key(phase_name.to_string(), key);
    save_state(&ctx.state_path, &state)?;

    // Run after hooks
    run_after_hooks(ctx, phase_name)?;

    Ok(())
}

/// Run a pipeline of phases sequentially
pub fn run_pipeline(ctx: &Context, phases: &[String]) -> Result<()> {
    if let Some(workspaces) = &ctx.make.workspace {
        // Check if parallel execution is requested
        let parallel = phases
            .first()
            .and_then(|p| ctx.make.lifecycle.get(p))
            .and_then(|ph| ph.parallel)
            .unwrap_or(false);

        if parallel {
            // PRODUCTION FIX: Bounded thread pool to prevent resource exhaustion
            use rayon::prelude::*;
            use rayon::ThreadPoolBuilder;

            // Limit to max 8 threads to prevent fork bomb with many workspaces
            let max_threads = 8.min(num_cpus::get());
            let pool = ThreadPoolBuilder::new()
                .num_threads(max_threads)
                .build()
                .map_err(|e| LifecycleError::Other(format!("Failed to create thread pool: {}", e)))?;

            let results: Vec<Result<()>> = pool.install(|| {
                workspaces
                    .par_iter()
                    .map(|(ws_name, workspace)| {
                        tracing::info!(workspace = %ws_name, "Processing workspace");
                        let ws_ctx = create_workspace_context(
                            &ctx.root,
                            ws_name,
                            workspace,
                            &ctx.make,
                            &ctx.env,
                        )?;

                        for phase in phases {
                            run_phase(&ws_ctx, phase)?;
                        }
                        Ok(())
                    })
                    .collect()
            });

            // Check for errors
            for result in results {
                result?;
            }
        } else {
            // Sequential execution
            for (ws_name, workspace) in workspaces {
                tracing::info!(workspace = %ws_name, "Processing workspace");
                let ws_ctx = create_workspace_context(
                    &ctx.root,
                    ws_name,
                    workspace,
                    &ctx.make,
                    &ctx.env,
                )?;

                for phase in phases {
                    run_phase(&ws_ctx, phase)?;
                }
            }
        }
    } else {
        // No workspaces, run directly
        for phase in phases {
            run_phase(ctx, phase)?;
        }
    }

    Ok(())
}

/// Create workspace context with security validation
///
/// SECURITY: Validates workspace paths to prevent directory traversal attacks
fn create_workspace_context(
    root: &Path,
    ws_name: &str,
    workspace: &super::model::Workspace,
    root_make: &Arc<Make>,
    env: &[(String, String)],
) -> Result<Context> {
    let ws_path = root.join(&workspace.path);

    // SECURITY: Canonicalize and validate paths to prevent traversal
    let canonical_root = root
        .canonicalize()
        .map_err(|e| LifecycleError::Other(format!("Failed to canonicalize root path: {}", e)))?;

    let canonical_ws = ws_path.canonicalize().map_err(|e| {
        LifecycleError::Other(format!(
            "Failed to canonicalize workspace '{}' path: {}",
            ws_name, e
        ))
    })?;

    // SECURITY: Ensure workspace path is within project root
    if !canonical_ws.starts_with(&canonical_root) {
        return Err(LifecycleError::Other(format!(
            "Security violation: workspace '{}' path '{}' is outside project root",
            ws_name,
            workspace.path
        )));
    }

    let ws_make_path = canonical_ws.join("make.toml");

    // Load workspace-specific make.toml if it exists, otherwise use root
    let ws_make = if ws_make_path.exists() {
        Arc::new(load_make(&ws_make_path)?)
    } else {
        Arc::clone(root_make)
    };

    let ws_state_path = canonical_ws.join(".ggen/state.json");

    Ok(Context::new(canonical_ws, ws_make, ws_state_path, env.to_vec()))
}

/// Run before hooks for a phase
fn run_before_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        // Global before_all
        if let Some(before_all) = &hooks.before_all {
            for hook_phase in before_all {
                run_phase(ctx, hook_phase)?;
            }
        }

        // Phase-specific before hooks
        let before_hooks = match phase_name {
            "init" => &hooks.before_init,
            "setup" => &hooks.before_setup,
            "build" => &hooks.before_build,
            "test" => &hooks.before_test,
            "deploy" => &hooks.before_deploy,
            _ => &None,
        };

        if let Some(hooks_list) = before_hooks {
            for hook_phase in hooks_list {
                run_phase(ctx, hook_phase)?;
            }
        }
    }

    Ok(())
}

/// Run after hooks for a phase
fn run_after_hooks(ctx: &Context, phase_name: &str) -> Result<()> {
    if let Some(hooks) = &ctx.make.hooks {
        // Phase-specific after hooks
        let after_hooks = match phase_name {
            "init" => &hooks.after_init,
            "setup" => &hooks.after_setup,
            "build" => &hooks.after_build,
            "test" => &hooks.after_test,
            "deploy" => &hooks.after_deploy,
            _ => &None,
        };

        if let Some(hooks_list) = after_hooks {
            for hook_phase in hooks_list {
                run_phase(ctx, hook_phase)?;
            }
        }

        // Global after_all
        if let Some(after_all) = &hooks.after_all {
            for hook_phase in after_all {
                run_phase(ctx, hook_phase)?;
            }
        }
    }

    Ok(())
}

/// Execute a shell command with streaming output and timeout
///
/// PRODUCTION FIX: Commands now have 5-minute timeout to prevent hung processes
fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    use std::time::Duration;

    let mut command = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C");
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-c"); // Changed from -lc to -c (don't load profile - faster)
        c
    };

    command.current_dir(cwd).arg(cmd);

    for (key, value) in env {
        command.env(key, value);
    }

    // PRODUCTION FIX: Stream output to user (80/20 - visibility over capture)
    let mut child = command
        .stdout(std::process::Stdio::inherit()) // Show stdout in real-time
        .stderr(std::process::Stdio::inherit()) // Show stderr in real-time
        .spawn()
        .map_err(|e| LifecycleError::command_spawn("unknown", cmd, e))?;

    // PRODUCTION FIX: Implement timeout to prevent hung processes
    let timeout = Duration::from_secs(300); // 5 minutes
    let start = Instant::now();

    loop {
        match child
            .try_wait()
            .map_err(|e| LifecycleError::command_spawn("unknown", cmd, e))?
        {
            Some(status) => {
                if !status.success() {
                    let exit_code = status.code().unwrap_or(-1);
                    return Err(LifecycleError::command_failed(
                        "unknown",
                        cmd,
                        exit_code,
                        "Command output shown above".to_string(),
                    ));
                }
                return Ok(());
            }
            None if start.elapsed() > timeout => {
                // Timeout exceeded - kill the process
                let _ = child.kill(); // Ignore kill errors
                return Err(LifecycleError::Other(format!(
                    "Command timeout after {}s: {}",
                    timeout.as_secs(),
                    cmd
                )));
            }
            None => {
                // Still running, sleep briefly and check again
                std::thread::sleep(Duration::from_millis(100));
            }
        }
    }
}

/// Get current time in milliseconds since epoch
fn current_time_ms() -> Result<u128> {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis())
        .map_err(|_| LifecycleError::Other("System clock error: time is before UNIX epoch".into()))
}

// Unit tests removed - covered by integration_test.rs:
// - test_phase_commands_extraction (tests both single and multiple commands)
// This provides better coverage with actual make.toml parsing
