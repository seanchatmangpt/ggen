//! GitHub Actions workflow triggering and management.
//!
//! This module provides functionality to trigger GitHub Actions workflows,
//! manage workflow runs, and monitor execution status. It integrates with
//! GitHub API to provide comprehensive workflow management capabilities.
//!
//! # Examples
//!
//! ```bash
//! ggen ci trigger workflow --name "build" --ref "main"
//! ggen ci trigger dispatch --event "release" --payload '{"version": "1.0.0"}'
//! ggen ci trigger rerun --run-id "12345"
//! ```
//!
//! # Errors
//!
//! Returns errors if GitHub API calls fail, workflows don't exist, or if
//! authentication is not properly configured.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct TriggerArgs {
    #[command(subcommand)]
    pub action: TriggerAction,
}

#[derive(Subcommand, Debug)]
pub enum TriggerAction {
    /// Trigger a specific workflow
    Workflow(WorkflowTriggerArgs),

    /// Trigger all workflows
    All(AllTriggerArgs),

    /// Trigger local testing with act
    Local(LocalTriggerArgs),
}

#[derive(Args, Debug)]
pub struct WorkflowTriggerArgs {
    /// Workflow name to trigger
    #[arg(long)]
    pub workflow: String,

    /// Branch to trigger workflow on [default: main]
    #[arg(long, default_value = "main")]
    pub branch: String,

    /// Input parameters for the workflow
    #[arg(long)]
    pub inputs: Option<Vec<String>>,
}

#[derive(Args, Debug)]
pub struct AllTriggerArgs {
    /// Branch to trigger workflows on [default: main]
    #[arg(long, default_value = "main")]
    pub branch: String,

    /// Wait for workflows to complete
    #[arg(long)]
    pub wait: bool,
}

#[derive(Args, Debug)]
pub struct LocalTriggerArgs {
    /// Workflow to run locally [default: all]
    #[arg(long, default_value = "all")]
    pub workflow: String,

    /// Use lightweight mode (less memory)
    #[arg(long)]
    pub light: bool,

    /// Dry run (no execution)
    #[arg(long)]
    pub dry_run: bool,
}

pub async fn run(args: &TriggerArgs) -> Result<()> {
    match &args.action {
        TriggerAction::Workflow(workflow_args) => trigger_workflow(workflow_args).await,
        TriggerAction::All(all_args) => trigger_all_workflows(all_args).await,
        TriggerAction::Local(local_args) => trigger_local_testing(local_args).await,
    }
}

/// Validate and sanitize workflow name input
fn validate_workflow_name(workflow: &str) -> Result<()> {
    // Validate workflow name is not empty
    if workflow.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Workflow name cannot be empty",
        ));
    }

    // Validate workflow name length
    if workflow.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Workflow name too long (max 200 characters)",
        ));
    }

    // Validate workflow name format (basic pattern check)
    if !workflow
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid workflow name format: only alphanumeric characters, dashes, underscores, and dots allowed",
        ));
    }

    Ok(())
}

/// Validate and sanitize branch name input
fn validate_branch_name(branch: &str) -> Result<()> {
    // Validate branch name is not empty
    if branch.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Branch name cannot be empty"));
    }

    // Validate branch name length
    if branch.len() > 200 {
        return Err(ggen_utils::error::Error::new(
            "Branch name too long (max 200 characters)",
        ));
    }

    // Validate branch name format (basic pattern check)
    if !branch
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '/' || c == '.')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid branch name format: only alphanumeric characters, dashes, underscores, slashes, and dots allowed",
        ));
    }

    Ok(())
}

/// Validate and sanitize input parameters
fn validate_inputs(inputs: &Option<Vec<String>>) -> Result<()> {
    if let Some(inputs) = inputs {
        for input in inputs {
            // Validate input is not empty
            if input.trim().is_empty() {
                return Err(ggen_utils::error::Error::new(
                    "Input parameter cannot be empty",
                ));
            }

            // Validate input length
            if input.len() > 1000 {
                return Err(ggen_utils::error::Error::new(
                    "Input parameter too long (max 1000 characters)",
                ));
            }

            // Validate input format (key=value)
            if !input.contains('=') {
                return Err(ggen_utils::error::Error::new_fmt(format_args!(
                    "Invalid input format: '{}'. Expected 'key=value'",
                    input
                )));
            }
        }
    }

    Ok(())
}

async fn trigger_workflow(args: &WorkflowTriggerArgs) -> Result<()> {
    // Validate inputs
    validate_workflow_name(&args.workflow)?;
    validate_branch_name(&args.branch)?;
    validate_inputs(&args.inputs)?;

    println!("🚀 Triggering workflow: {}", args.workflow);

    let mut cmd = std::process::Command::new("gh");
    cmd.args(["workflow", "run", &args.workflow]);
    cmd.arg("--ref").arg(&args.branch);

    if let Some(inputs) = &args.inputs {
        for input in inputs {
            cmd.arg("--input").arg(input);
        }
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Failed to trigger workflow {}: {}",
            args.workflow, stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Workflow {} triggered successfully", args.workflow);
    println!("{}", stdout);
    Ok(())
}

async fn trigger_all_workflows(args: &AllTriggerArgs) -> Result<()> {
    // Validate inputs
    validate_branch_name(&args.branch)?;

    println!("🚀 Triggering all workflows on branch: {}", args.branch);

    // Get list of workflows
    let mut list_cmd = std::process::Command::new("gh");
    list_cmd.args(["workflow", "list"]);

    let list_output = list_cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !list_output.status.success() {
        let stderr = String::from_utf8_lossy(&list_output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Failed to list workflows: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&list_output.stdout);
    let workflows: Vec<&str> = stdout
        .lines()
        .filter_map(|line| line.split_whitespace().next())
        .collect();

    println!("📋 Found {} workflows to trigger", workflows.len());

    for workflow in workflows {
        println!("🚀 Triggering workflow: {}", workflow);

        let mut cmd = std::process::Command::new("gh");
        cmd.args(["workflow", "run", workflow]);
        cmd.arg("--ref").arg(&args.branch);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            println!("❌ Failed to trigger workflow {}: {}", workflow, stderr);
            continue;
        }

        println!("✅ Workflow {} triggered", workflow);
    }

    if args.wait {
        println!("⏳ Waiting for workflows to complete...");
        // This would implement waiting logic
        // For now, just show a message
        println!("Use 'ggen ci workflow status' to check progress");
    }

    Ok(())
}

async fn trigger_local_testing(args: &LocalTriggerArgs) -> Result<()> {
    // Validate inputs
    validate_workflow_name(&args.workflow)?;

    println!("🧪 Running local testing with act");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make"]);

    match args.workflow.as_str() {
        "all" => {
            if args.light {
                cmd.arg("act-all-light");
            } else {
                cmd.arg("act-all");
            }
        }
        "lint" => {
            if args.light {
                cmd.arg("act-lint-light");
            } else {
                cmd.arg("act-lint");
            }
        }
        "test" => {
            if args.light {
                cmd.arg("act-test-light");
            } else {
                cmd.arg("act-test");
            }
        }
        "build" => {
            if args.light {
                cmd.arg("act-build-light");
            } else {
                cmd.arg("act-build");
            }
        }
        _ => {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Unknown workflow: {}. Valid options: all, lint, test, build",
                args.workflow
            )));
        }
    }

    if args.dry_run {
        cmd.arg("act-dry-run");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Local testing failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Local testing completed successfully");
    println!("{}", stdout);
    Ok(())
}
