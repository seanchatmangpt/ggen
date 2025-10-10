//! Shell completion script generation and installation.
//!
//! This module provides functionality to generate and install shell completion
//! scripts for various shells (bash, zsh, fish, powershell). It supports
//! generating completion scripts and installing them to the appropriate
//! shell configuration directories.
//!
//! # Examples
//!
//! ```bash
//! ggen shell completion generate --shell bash --output ~/.bash_completion.d/ggen
//! ggen shell completion install --shell zsh --force
//! ggen shell completion list
//! ```
//!
//! # Errors
//!
//! Returns errors if the shell type is unsupported, file operations fail,
//! or if the completion script generation fails.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait CompletionGenerator {
    fn generate(&self, shell: &str, output: Option<&str>) -> Result<CompletionResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait CompletionInstaller {
    fn install(&self, shell: &str, force: bool) -> Result<InstallResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait ShellLister {
    fn list(&self) -> Result<ShellListResult>;
}

#[derive(Debug, Clone)]
pub struct CompletionResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct InstallResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct ShellListResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Args, Debug)]
pub struct CompletionArgs {
    #[command(subcommand)]
    pub action: CompletionAction,
}

#[derive(Subcommand, Debug)]
pub enum CompletionAction {
    /// Generate completion script for a shell
    Generate(GenerateArgs),

    /// Install completion script
    Install(InstallArgs),

    /// List supported shells
    List,
}

#[derive(Args, Debug)]
pub struct GenerateArgs {
    /// Shell type (bash, zsh, fish, powershell)
    #[arg(long)]
    pub shell: String,

    /// Output file path [default: stdout]
    #[arg(long)]
    pub output: Option<String>,
}

#[derive(Args, Debug)]
pub struct InstallArgs {
    /// Shell type (bash, zsh, fish, powershell)
    #[arg(long)]
    pub shell: String,

    /// Force installation even if already installed
    #[arg(long)]
    pub force: bool,
}

pub async fn run(args: &CompletionArgs) -> Result<()> {
    let generator = CargoMakeCompletionGenerator;
    let installer = CargoMakeCompletionInstaller;
    let lister = CargoMakeShellLister;
    
    run_with_deps(args, &generator, &installer, &lister).await
}

pub async fn run_with_deps(
    args: &CompletionArgs,
    generator: &dyn CompletionGenerator,
    installer: &dyn CompletionInstaller,
    lister: &dyn ShellLister,
) -> Result<()> {
    match &args.action {
        CompletionAction::Generate(generate_args) => generate_completion_with_deps(generate_args, generator).await,
        CompletionAction::Install(install_args) => install_completion_with_deps(install_args, installer).await,
        CompletionAction::List => list_shells_with_deps(lister).await,
    }
}

async fn generate_completion_with_deps(args: &GenerateArgs, generator: &dyn CompletionGenerator) -> Result<()> {
    println!("Generating completion script for {}", args.shell);

    let result = generator.generate(&args.shell, args.output.as_deref())?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Completion generation failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn generate_completion(args: &GenerateArgs) -> Result<()> {
    let generator = CargoMakeCompletionGenerator;
    generate_completion_with_deps(args, &generator).await
}

async fn install_completion_with_deps(args: &InstallArgs, installer: &dyn CompletionInstaller) -> Result<()> {
    println!("Installing completion script for {}", args.shell);

    let result = installer.install(&args.shell, args.force)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Completion installation failed: {}",
            result.stderr
        )));
    }

    println!("✅ Completion script installed successfully");
    println!("{}", result.stdout);
    Ok(())
}

async fn install_completion(args: &InstallArgs) -> Result<()> {
    let installer = CargoMakeCompletionInstaller;
    install_completion_with_deps(args, &installer).await
}

async fn list_shells_with_deps(lister: &dyn ShellLister) -> Result<()> {
    println!("Listing supported shells");

    let result = lister.list()?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Shell listing failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn list_shells() -> Result<()> {
    let lister = CargoMakeShellLister;
    list_shells_with_deps(&lister).await
}
