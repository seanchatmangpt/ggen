//! Phase execution with hooks and state management

use super::{cache::cache_key, error::*, model::*, state::*};
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
    pub fn new(root: PathBuf, make: Arc<Make>, state_path: PathBuf, env: Vec<(String, String)>) -> Self {
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
        let mut guard = self.hook_guard.lock()
            .map_err(|_| LifecycleError::MutexPoisoned { phase: phase.to_string() })?;

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
    let phase = ctx.make.lifecycle.get(phase_name)
        .ok_or_else(|| LifecycleError::phase_not_found(phase_name))?;

    // Get commands for this phase using new Phase::commands() method
    let cmds = phase.commands();
    if cmds.is_empty() {
        println!("⚠️  Phase '{}' has no commands", phase_name);
        return Ok(());
    }

    // Run before hooks
    run_before_hooks(ctx, phase_name)?;

    // Generate cache key
    let key = cache_key(phase_name, &cmds, &ctx.env, &[]);

    // Execute phase commands
    let started = current_time_ms();
    let timer = Instant::now();

    println!("▶️  Running phase: {}", phase_name);
    for cmd in &cmds {
        execute_command(cmd, &ctx.root, &ctx.env)?;
    }

    let duration = timer.elapsed().as_millis();
    println!("✅ Phase '{}' completed in {}ms", phase_name, duration);

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
        let parallel = phases.first()
            .and_then(|p| ctx.make.lifecycle.get(p))
            .and_then(|ph| ph.parallel)
            .unwrap_or(false);

        if parallel {
            // Parallel execution using rayon
            use rayon::prelude::*;

            let results: Vec<Result<()>> = workspaces
                .par_iter()
                .map(|(ws_name, workspace)| {
                    println!("\n📦 Workspace: {}", ws_name);
                    let ws_path = ctx.root.join(&workspace.path);
                    let ws_state_path = ws_path.join(".ggen/state.json");

                    let ws_ctx = Context::new(
                        ws_path,
                        Arc::clone(&ctx.make),
                        ws_state_path,
                        ctx.env.clone(),
                    );

                    for phase in phases {
                        run_phase(&ws_ctx, phase)?;
                    }
                    Ok(())
                })
                .collect();

            // Check for errors
            for result in results {
                result?;
            }
        } else {
            // Sequential execution
            for (ws_name, workspace) in workspaces {
                println!("\n📦 Workspace: {}", ws_name);
                let ws_path = ctx.root.join(&workspace.path);
                let ws_state_path = ws_path.join(".ggen/state.json");

                let ws_ctx = Context::new(
                    ws_path,
                    Arc::clone(&ctx.make),
                    ws_state_path,
                    ctx.env.clone(),
                );

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

/// Execute a shell command
fn execute_command(cmd: &str, cwd: &Path, env: &[(String, String)]) -> Result<()> {
    let mut command = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.arg("/C");
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-lc");
        c
    };

    command.current_dir(cwd).arg(cmd);

    for (key, value) in env {
        command.env(key, value);
    }

    let output = command.output()
        .map_err(|e| LifecycleError::command_spawn("unknown", cmd, e))?;

    if !output.status.success() {
        let exit_code = output.status.code().unwrap_or(-1);
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(LifecycleError::command_failed("unknown", cmd, exit_code, stderr));
    }

    Ok(())
}

/// Get current time in milliseconds since epoch
fn current_time_ms() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("System time is before UNIX epoch - invalid system clock")
        .as_millis()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_phase_commands_single() {
        let phase = Phase {
            command: Some("echo hello".to_string()),
            commands: None,
            description: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        };

        let cmds = phase.commands();
        assert_eq!(cmds, vec!["echo hello"]);
    }

    #[test]
    fn test_phase_commands_multiple() {
        let phase = Phase {
            command: None,
            commands: Some(vec!["echo 1".to_string(), "echo 2".to_string()]),
            description: None,
            watch: None,
            port: None,
            outputs: None,
            cache: None,
            workspaces: None,
            parallel: None,
        };

        let cmds = phase.commands();
        assert_eq!(cmds, vec!["echo 1", "echo 2"]);
    }
}
