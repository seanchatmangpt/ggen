//! Lifecycle command implementation
//!
//! Provides CLI interface for universal lifecycle management through make.toml

use clap::Parser;
use ggen_core::lifecycle::{load_make, load_state, Context, run_phase, run_pipeline};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug, Clone)]
#[command(name = "lifecycle", about = "Universal lifecycle management")]
pub struct LifecycleArgs {
    #[command(subcommand)]
    pub command: LifecycleCmd,
}

#[derive(Parser, Debug, Clone)]
pub enum LifecycleCmd {
    /// List all available lifecycle phases
    #[command(name = "list")]
    List {
        /// Path to project root (defaults to current directory)
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Show details of a specific phase
    #[command(name = "show")]
    Show {
        /// Phase name to show
        phase: String,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Run a single lifecycle phase
    #[command(name = "run")]
    Run {
        /// Phase name to run
        phase: String,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Environment name (development, staging, production)
        #[arg(long, short = 'e')]
        env: Option<String>,
    },

    /// Run multiple phases in sequence (pipeline)
    #[command(name = "pipeline")]
    Pipeline {
        /// Phase names to run in order
        phases: Vec<String>,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Environment name
        #[arg(long, short = 'e')]
        env: Option<String>,
    },
}

pub async fn run(args: LifecycleArgs) -> ggen_utils::error::Result<()> {
    use LifecycleCmd::*;

    match args.command {
        List { root } => list_phases(&root),
        Show { phase, root } => show_phase(&root, &phase),
        Run { phase, root, env } => run_single_phase(&root, &phase, env),
        Pipeline { phases, root, env } => run_phase_pipeline(&root, &phases, env),
    }
}

/// List all available phases from make.toml
fn list_phases(root: &Path) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");

    if !make_path.exists() {
        println!("❌ No make.toml found in {}", root.display());
        println!("   Initialize a project with 'ggen lifecycle init'");
        return Ok(());
    }

    let make = load_make(&make_path)?;

    println!("📋 Available lifecycle phases in {}:", root.display());
    println!();

    let phase_names = make.phase_names();
    if phase_names.is_empty() {
        println!("   (no phases defined)");
    } else {
        for phase_name in phase_names {
            if let Some(phase) = make.lifecycle.get(&phase_name) {
                let desc = phase.description.as_deref().unwrap_or("(no description)");
                println!("  • {} - {}", phase_name, desc);
            }
        }
    }

    // Show last executed phase
    let state_path = root.join(".ggen/state.json");
    if state_path.exists() {
        let state = load_state(&state_path);
        if let Some(last) = state.last_phase {
            println!();
            println!("🔄 Last executed: {}", last);
        }
    }

    Ok(())
}

/// Show details of a specific phase
fn show_phase(root: &Path, phase_name: &str) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = load_make(&make_path)?;

    let phase = make.lifecycle.get(phase_name)
        .ok_or_else(|| anyhow::anyhow!("Phase '{}' not found in make.toml", phase_name))?;

    println!("📦 Phase: {}", phase_name);
    println!();

    if let Some(desc) = &phase.description {
        println!("Description: {}", desc);
    }

    // Commands
    let cmds = make.phase_commands(phase_name);
    if !cmds.is_empty() {
        println!();
        println!("Commands:");
        for cmd in cmds {
            println!("  $ {}", cmd);
        }
    }

    // Metadata
    if let Some(watch) = phase.watch {
        println!();
        println!("Watch mode: {}", watch);
    }

    if let Some(port) = phase.port {
        println!("Port: {}", port);
    }

    if let Some(cache) = phase.cache {
        println!("Caching: {}", cache);
    }

    // Hooks
    if let Some(hooks) = &make.hooks {
        let mut has_hooks = false;

        let before = match phase_name {
            "init" => &hooks.before_init,
            "setup" => &hooks.before_setup,
            "build" => &hooks.before_build,
            "test" => &hooks.before_test,
            "deploy" => &hooks.before_deploy,
            _ => &None,
        };

        let after = match phase_name {
            "init" => &hooks.after_init,
            "setup" => &hooks.after_setup,
            "build" => &hooks.after_build,
            "test" => &hooks.after_test,
            "deploy" => &hooks.after_deploy,
            _ => &None,
        };

        if let Some(before_hooks) = before {
            if !before_hooks.is_empty() {
                println!();
                println!("Before hooks:");
                for h in before_hooks {
                    println!("  → {}", h);
                }
                has_hooks = true;
            }
        }

        if let Some(after_hooks) = after {
            if !after_hooks.is_empty() {
                if !has_hooks {
                    println!();
                }
                println!("After hooks:");
                for h in after_hooks {
                    println!("  → {}", h);
                }
            }
        }
    }

    Ok(())
}

/// Run a single phase
fn run_single_phase(root: &Path, phase_name: &str, env: Option<String>) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = std::sync::Arc::new(load_make(&make_path)?);

    let env_vars = build_env(env);
    let state_path = root.join(".ggen/state.json");

    let ctx = Context::new(root.to_path_buf(), make, state_path, env_vars);

    run_phase(&ctx, phase_name)?;

    Ok(())
}

/// Run a pipeline of phases
fn run_phase_pipeline(root: &Path, phases: &[String], env: Option<String>) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = std::sync::Arc::new(load_make(&make_path)?);

    let env_vars = build_env(env);
    let state_path = root.join(".ggen/state.json");

    let ctx = Context::new(root.to_path_buf(), make, state_path, env_vars);

    run_pipeline(&ctx, phases)?;

    println!();
    println!("✅ Pipeline completed: {}", phases.join(" → "));

    Ok(())
}

/// Build environment variables
fn build_env(env: Option<String>) -> Vec<(String, String)> {
    env.map(|e| vec![("GGEN_ENV".to_string(), e)])
        .unwrap_or_default()
}
