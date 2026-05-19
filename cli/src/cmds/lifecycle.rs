//! Lifecycle commands - clap-noun-verb auto-discovery
//!
//! Universal lifecycle management for cross-framework development workflows.
//! Commands for managing make.toml-based lifecycle phases.

use clap::{Args, Subcommand};
use ggen_core::lifecycle::{load_make, load_state, run_phase, run_pipeline, Context};
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Lifecycle command arguments
#[derive(Debug, Args)]
pub struct LifecycleArgs {
    #[command(subcommand)]
    pub command: LifecycleCmd,

    /// Path to make.toml file
    #[arg(short = 'f', long, default_value = "make.toml")]
    pub config: PathBuf,

    /// Environment (development, staging, production)
    #[arg(short = 'e', long, default_value = "development")]
    pub env: String,

    /// Root directory (defaults to current directory)
    #[arg(short = 'r', long)]
    pub root: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
pub enum LifecycleCmd {
    /// List all available lifecycle phases
    List,
    /// Run a specific lifecycle phase
    Run(RunArgs),
    /// Run multiple phases in sequence (pipeline)
    Pipeline(PipelineArgs),
    /// Show details of a specific phase
    Show(ShowArgs),
    /// Show current lifecycle state
    State,
}

/// Run command arguments
#[derive(Debug, Args)]
pub struct RunArgs {
    /// Phase name to run
    pub phase: String,

    /// Force re-run (ignore cache)
    #[arg(long)]
    pub force: bool,

    /// Dry run (show what would execute)
    #[arg(long)]
    pub dry_run: bool,
}

/// Pipeline command arguments
#[derive(Debug, Args)]
pub struct PipelineArgs {
    /// Phase names to run in sequence
    pub phases: Vec<String>,

    /// Force re-run (ignore cache)
    #[arg(long)]
    pub force: bool,

    /// Stop on first error
    #[arg(long)]
    pub fail_fast: bool,
}

/// Show command arguments
#[derive(Debug, Args)]
pub struct ShowArgs {
    /// Phase name to show
    pub phase: String,
}

impl LifecycleArgs {
    /// Create lifecycle context from args
    fn create_context(&self) -> Result<Context> {
        let root = self.root.clone().unwrap_or_else(|| {
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
        });

        // Build full path to make.toml
        let make_path = if self.config.is_absolute() {
            self.config.clone()
        } else {
            root.join(&self.config)
        };

        let make = load_make(&make_path)
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;
        let state_path = root.join(".ggen").join("state.json");

        // Convert env string to Vec<(String, String)> format
        let env_vars: Vec<(String, String)> = vec![("ENV".to_string(), self.env.clone())];

        Ok(Context::new(
            root,
            std::sync::Arc::new(make),
            state_path,
            env_vars,
        ))
    }

    /// Execute lifecycle command
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            LifecycleCmd::List => self.list_phases(),
            LifecycleCmd::Run(args) => self.run_phase(args),
            LifecycleCmd::Pipeline(args) => self.run_pipeline(args),
            LifecycleCmd::Show(args) => self.show_phase(args),
            LifecycleCmd::State => self.show_state(),
        }
    }

    /// List all available phases
    fn list_phases(&self) -> Result<()> {
        let ctx = self.create_context()?;

        println!("Available lifecycle phases:\n");

        if ctx.make.lifecycle.is_empty() {
            println!("  No phases defined in make.toml");
            return Ok(());
        }

        for (name, phase) in &ctx.make.lifecycle {
            println!("  {} - {}", name, phase.description.as_deref().unwrap_or("(no description)"));
            
            if let Some(ref commands) = phase.commands {
                println!("    Commands: {}", commands.join(", "));
            } else if let Some(ref cmd) = phase.command {
                println!("    Command: {}", cmd);
            }

            if phase.watch.unwrap_or(false) {
                println!("    Watch mode: enabled");
            }

            if phase.cache.unwrap_or(false) {
                println!("    Caching: enabled");
            }

            println!();
        }

        Ok(())
    }

    /// Run a specific phase
    fn run_phase(&self, args: &RunArgs) -> Result<()> {
        let ctx = self.create_context()?;

        if args.dry_run {
            println!("Dry run: Would execute phase '{}'", args.phase);
            return Ok(());
        }

        println!("Running phase: {}", args.phase);
        run_phase(&ctx, &args.phase)
            .map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;
        println!("✅ Phase '{}' completed successfully", args.phase);

        Ok(())
    }

    /// Run multiple phases in pipeline
    fn run_pipeline(&self, args: &PipelineArgs) -> Result<()> {
        let ctx = self.create_context()?;

        if args.phases.is_empty() {
            return Err(ggen_utils::error::Error::new("No phases specified for pipeline"));
        }

        println!("Running pipeline: {}", args.phases.join(" → "));

        match run_pipeline(&ctx, &args.phases) {
            Ok(_) => {
                println!("✅ Pipeline completed successfully");
                Ok(())
            }
            Err(e) => {
                let err = ggen_utils::error::Error::new(&e.to_string());
                if args.fail_fast {
                    eprintln!("❌ Pipeline failed at: {}", err);
                    Err(err)
                } else {
                    eprintln!("⚠️  Pipeline had errors: {}", err);
                    Ok(())
                }
            }
        }
    }

    /// Show details of a specific phase
    fn show_phase(&self, args: &ShowArgs) -> Result<()> {
        let ctx = self.create_context()?;

        let phase = ctx.make.lifecycle.get(&args.phase)
            .ok_or_else(|| ggen_utils::error::Error::new(&format!("Phase '{}' not found", args.phase)))?;

        println!("Phase: {}\n", args.phase);

        if let Some(ref desc) = phase.description {
            println!("Description: {}", desc);
        }

        if let Some(ref commands) = phase.commands {
            println!("\nCommands:");
            for (i, cmd) in commands.iter().enumerate() {
                println!("  {}. {}", i + 1, cmd);
            }
        } else if let Some(ref cmd) = phase.command {
            println!("\nCommand: {}", cmd);
        }

        if phase.watch.unwrap_or(false) {
            println!("\nWatch mode: enabled");
        }

        if let Some(port) = phase.port {
            println!("\nPort: {}", port);
        }

        if phase.cache.unwrap_or(false) {
            println!("\nCaching: enabled");
        }

        if let Some(ref outputs) = phase.outputs {
            println!("\nOutputs: {}", outputs.join(", "));
        }

        // Show state information
        let state = load_state(&ctx.state_path)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load state: {}", e)))?;
        if let Some(last_run) = state.last_run(&args.phase) {
            println!("\nLast run:");
            println!("  Success: {}", last_run.success);
            println!("  Duration: {}ms", last_run.duration_ms);
        }

        Ok(())
    }

    /// Show current lifecycle state
    fn show_state(&self) -> Result<()> {
        let ctx = self.create_context()?;
        let state = load_state(&ctx.state_path)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load state: {}", e)))?;

        println!("Lifecycle State:\n");
        println!("  Project: {}", ctx.make.project.name);
        if let Some(ref version) = ctx.make.project.version {
            println!("  Version: {}", version);
        }
        println!("  Environment: {}", self.env);

        if let Some(ref last_phase) = state.last_phase {
            println!("  Last phase run: {}", last_phase);
        }

        if !state.phase_history.is_empty() {
            println!("\n  Phase history:");
            for run in &state.phase_history {
                let status = if run.success { "✅" } else { "❌" };
                println!("    {} {} - {}ms", status, run.phase, run.duration_ms);
            }
        }

        Ok(())
    }
}

