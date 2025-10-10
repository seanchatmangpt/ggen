use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct ReleaseArgs {
    #[command(subcommand)]
    pub action: ReleaseAction,
}

#[derive(Subcommand, Debug)]
pub enum ReleaseAction {
    /// Run release workflows locally with act
    Run(RunArgs),

    /// Run release workflows with retry logic
    Retry(RetryArgs),

    /// Run release workflows with metrics collection
    Metrics(MetricsArgs),

    /// Run release workflows with custom timeout
    Timeout(TimeoutArgs),

    /// Show what would be executed without running
    DryRun(DryRunArgs),
}

#[derive(Args, Debug)]
pub struct RunArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,

    /// Enable debug output
    #[arg(long)]
    pub debug: bool,
}

#[derive(Args, Debug)]
pub struct RetryArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Maximum retry attempts [default: 3]
    #[arg(long, default_value = "3")]
    pub max_retries: u8,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug)]
pub struct MetricsArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug)]
pub struct TimeoutArgs {
    /// Workflow timeout in seconds [default: 1800]
    #[arg(long, default_value = "1800")]
    pub timeout: u32,

    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug)]
pub struct DryRunArgs {
    /// Run only the release workflow
    #[arg(long)]
    pub workflow_only: bool,

    /// Run only the homebrew-release workflow
    #[arg(long)]
    pub homebrew_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Enable verbose output
    #[arg(long)]
    pub verbose: bool,
}

pub async fn run(args: &ReleaseArgs) -> Result<()> {
    match &args.action {
        ReleaseAction::Run(run_args) => run_release_workflows(run_args).await,
        ReleaseAction::Retry(retry_args) => run_release_with_retry(retry_args).await,
        ReleaseAction::Metrics(metrics_args) => run_release_with_metrics(metrics_args).await,
        ReleaseAction::Timeout(timeout_args) => run_release_with_timeout(timeout_args).await,
        ReleaseAction::DryRun(dry_run_args) => run_release_dry_run(dry_run_args).await,
    }
}

async fn run_release_workflows(args: &RunArgs) -> Result<()> {
    println!("🚀 Running release workflows locally with act");

    let mut cmd = std::process::Command::new("./scripts/act-release.sh");

    if args.workflow_only {
        cmd.arg("--workflow-only");
    }

    if args.homebrew_only {
        cmd.arg("--homebrew-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.debug {
        cmd.env("DEBUG", "true");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !args.json {
        println!("✅ Release workflows completed successfully");
    }
    println!("{}", stdout);
    Ok(())
}

async fn run_release_with_retry(args: &RetryArgs) -> Result<()> {
    println!("🔄 Running release workflows with retry logic");

    let mut cmd = std::process::Command::new("./scripts/act-release.sh");
    cmd.arg("--retry-failed");
    cmd.arg("--max-retries").arg(args.max_retries.to_string());

    if args.workflow_only {
        cmd.arg("--workflow-only");
    }

    if args.homebrew_only {
        cmd.arg("--homebrew-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with retry failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !args.json {
        println!("✅ Release workflows with retry completed successfully");
    }
    println!("{}", stdout);
    Ok(())
}

async fn run_release_with_metrics(args: &MetricsArgs) -> Result<()> {
    println!("📊 Running release workflows with metrics collection");

    let mut cmd = std::process::Command::new("./scripts/act-release.sh");
    cmd.arg("--metrics");

    if args.workflow_only {
        cmd.arg("--workflow-only");
    }

    if args.homebrew_only {
        cmd.arg("--homebrew-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with metrics failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !args.json {
        println!("✅ Release workflows with metrics completed successfully");
    }
    println!("{}", stdout);
    Ok(())
}

async fn run_release_with_timeout(args: &TimeoutArgs) -> Result<()> {
    println!("⏱️ Running release workflows with custom timeout");

    let mut cmd = std::process::Command::new("./scripts/act-release.sh");
    cmd.arg("--timeout").arg(args.timeout.to_string());

    if args.workflow_only {
        cmd.arg("--workflow-only");
    }

    if args.homebrew_only {
        cmd.arg("--homebrew-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows with timeout failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !args.json {
        println!("✅ Release workflows with timeout completed successfully");
    }
    println!("{}", stdout);
    Ok(())
}

async fn run_release_dry_run(args: &DryRunArgs) -> Result<()> {
    println!("🔍 Showing what would be executed (dry run)");

    let mut cmd = std::process::Command::new("./scripts/act-release.sh");
    cmd.arg("--dry-run");

    if args.workflow_only {
        cmd.arg("--workflow-only");
    }

    if args.homebrew_only {
        cmd.arg("--homebrew-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Release workflows dry run failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}
