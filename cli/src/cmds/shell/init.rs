use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct InitArgs {
    #[command(subcommand)]
    pub action: InitAction,
}

#[derive(Subcommand, Debug)]
pub enum InitAction {
    /// Initialize shell integration
    Shell(ShellInitArgs),

    /// Initialize project configuration
    Project(ProjectInitArgs),

    /// Initialize development environment
    Dev(DevInitArgs),
}

#[derive(Args, Debug)]
pub struct ShellInitArgs {
    /// Shell type (bash, zsh, fish, powershell)
    #[arg(long)]
    pub shell: String,

    /// Configuration file path [default: auto-detect]
    #[arg(long)]
    pub config: Option<String>,

    /// Force initialization even if already configured
    #[arg(long)]
    pub force: bool,
}

#[derive(Args, Debug)]
pub struct ProjectInitArgs {
    /// Project name
    #[arg(long)]
    pub name: Option<String>,

    /// Project template to use
    #[arg(long)]
    pub template: Option<String>,

    /// Initialize in current directory
    #[arg(long)]
    pub here: bool,
}

#[derive(Args, Debug)]
pub struct DevInitArgs {
    /// Development tool to initialize
    #[arg(long)]
    pub tool: Option<String>,

    /// Configuration file path
    #[arg(long)]
    pub config: Option<String>,
}

pub async fn run(args: &InitArgs) -> Result<()> {
    match &args.action {
        InitAction::Shell(shell_args) => init_shell(shell_args).await,
        InitAction::Project(project_args) => init_project(project_args).await,
        InitAction::Dev(dev_args) => init_dev(dev_args).await,
    }
}

async fn init_shell(args: &ShellInitArgs) -> Result<()> {
    println!("Initializing shell integration for {}", args.shell);

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "shell-init"]);

    cmd.arg("--shell").arg(&args.shell);

    if let Some(config) = &args.config {
        cmd.arg("--config").arg(config);
    }

    if args.force {
        cmd.arg("--force");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Shell initialization failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Shell integration initialized successfully");
    println!("{}", stdout);
    Ok(())
}

async fn init_project(args: &ProjectInitArgs) -> Result<()> {
    println!("Initializing project");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-init"]);

    if let Some(name) = &args.name {
        cmd.arg("--name").arg(name);
    }

    if let Some(template) = &args.template {
        cmd.arg("--template").arg(template);
    }

    if args.here {
        cmd.arg("--here");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Project initialization failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Project initialized successfully");
    println!("{}", stdout);
    Ok(())
}

async fn init_dev(args: &DevInitArgs) -> Result<()> {
    println!("Initializing development environment");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "dev-init"]);

    if let Some(tool) = &args.tool {
        cmd.arg("--tool").arg(tool);
    }

    if let Some(config) = &args.config {
        cmd.arg("--config").arg(config);
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Development environment initialization failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Development environment initialized successfully");
    println!("{}", stdout);
    Ok(())
}
