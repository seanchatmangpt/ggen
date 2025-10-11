//! Shell and project initialization utilities.
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should streamline the onboarding experience by automating
//! shell integration, project setup, and development environment configuration,
//! reducing friction for new users and ensuring consistent setups.
//!
//! ## RESPONSIBILITIES
//! 1. **Shell Integration**: Should configure shell RC files for completions/aliases
//! 2. **Project Init**: Should scaffold new projects from templates
//! 3. **Dev Environment**: Should configure tools (git, editor, linters)
//! 4. **Cross-Platform**: Should support bash, zsh, fish, powershell
//! 5. **Safe Updates**: Should backup configs before modification
//!
//! ## CONSTRAINTS
//! - Must detect shell type automatically when possible
//! - Must validate config file paths before writing
//! - Must support force flag for re-initialization
//! - Must provide clear help messages for all options
//! - Must preserve user's existing shell configurations
//!
//! ## DEPENDENCIES
//! - Cargo make: Should delegate to makefile tasks
//! - Initializer traits: Should be mockable for testing
//! - Filesystem: Should safely modify config files
//! - Templates: Should access project scaffolding templates
//!
//! ## ERROR HANDLING STRATEGY
//! - Unsupported shell → List supported shells, suggest manual config
//! - Config file missing → Offer to create it
//! - Permission denied → Suggest running with appropriate permissions
//! - Template not found → List available templates
//! - Init already done → Suggest --force flag
//!
//! ## TESTING STRATEGY
//! - Mock all three initializer traits separately
//! - Test each shell type with mock configs
//! - Test force flag behavior
//! - Test error paths (missing files, permissions)
//! - Test integration between subcommands
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Implement actual shell config modification (currently delegates to cargo make)
//! - [P1] Add auto-detection of shell type
//! - [P1] Create backup before modifying configs
//! - [P1] Add project template selection UI
//! - [P2] Support custom template repositories
//!
//! # Examples
//!
//! ```bash
//! ggen shell init shell --shell zsh --force
//! ggen shell init project --name "my-project" --template "rust-cli"
//! ggen shell init dev --all
//! ```
//!
//! # Errors
//!
//! Returns errors if initialization fails, configuration files can't be created,
//! or if the specified shell or template is not supported.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait ShellInitializer {
    fn init_shell(&self, shell: &str, config: Option<String>, force: bool) -> Result<InitResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait ProjectInitializer {
    fn init_project(
        &self, name: Option<String>, template: Option<String>, here: bool,
    ) -> Result<InitResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait DevInitializer {
    fn init_dev(&self, all: bool) -> Result<InitResult>;
}

#[derive(Debug, Clone)]
pub struct InitResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

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
    #[arg(long, help = "Shell type: bash, zsh, fish, or powershell")]
    pub shell: String,

    /// Configuration file path [default: auto-detect]
    #[arg(long, help = "Path to shell configuration file (auto-detected if not provided)")]
    pub config: Option<String>,

    /// Force initialization even if already configured
    #[arg(long, help = "Force re-initialization even if already configured")]
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
    let shell_init = CargoMakeShellInitializer;
    let project_init = CargoMakeProjectInitializer;
    let dev_init = CargoMakeDevInitializer;

    run_with_deps(args, &shell_init, &project_init, &dev_init).await
}

pub async fn run_with_deps(
    args: &InitArgs, shell_init: &dyn ShellInitializer, project_init: &dyn ProjectInitializer,
    dev_init: &dyn DevInitializer,
) -> Result<()> {
    match &args.action {
        InitAction::Shell(shell_args) => init_shell_with_deps(shell_args, shell_init).await,
        InitAction::Project(project_args) => {
            init_project_with_deps(project_args, project_init).await
        }
        InitAction::Dev(dev_args) => init_dev_with_deps(dev_args, dev_init).await,
    }
}

async fn init_shell_with_deps(
    args: &ShellInitArgs, shell_init: &dyn ShellInitializer,
) -> Result<()> {
    println!("Initializing shell integration for {}", args.shell);

    let result = shell_init.init_shell(&args.shell, args.config.clone(), args.force)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Shell initialization failed: {}",
            result.stderr
        )));
    }

    println!("✅ Shell integration initialized successfully");
    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn init_shell(args: &ShellInitArgs) -> Result<()> {
    let shell_init = CargoMakeShellInitializer;
    init_shell_with_deps(args, &shell_init).await
}

async fn init_project_with_deps(
    args: &ProjectInitArgs, project_init: &dyn ProjectInitializer,
) -> Result<()> {
    println!("Initializing project");

    let result = project_init.init_project(args.name.clone(), args.template.clone(), args.here)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Project initialization failed: {}",
            result.stderr
        )));
    }

    println!("✅ Project initialized successfully");
    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn init_project(args: &ProjectInitArgs) -> Result<()> {
    let project_init = CargoMakeProjectInitializer;
    init_project_with_deps(args, &project_init).await
}

async fn init_dev_with_deps(_args: &DevInitArgs, dev_init: &dyn DevInitializer) -> Result<()> {
    println!("Initializing development environment");

    let result = dev_init.init_dev(false)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Development environment initialization failed: {}",
            result.stderr
        )));
    }

    println!("✅ Development environment initialized successfully");
    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn init_dev(args: &DevInitArgs) -> Result<()> {
    let dev_init = CargoMakeDevInitializer;
    init_dev_with_deps(args, &dev_init).await
}

// Concrete implementations for production use
pub struct CargoMakeShellInitializer;

impl ShellInitializer for CargoMakeShellInitializer {
    fn init_shell(&self, shell: &str, config: Option<String>, force: bool) -> Result<InitResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "shell-init"]);

        cmd.arg("--shell").arg(shell);

        if let Some(config) = config {
            cmd.arg("--config").arg(config);
        }

        if force {
            cmd.arg("--force");
        }

        let output = cmd.output()?;
        Ok(InitResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakeProjectInitializer;

impl ProjectInitializer for CargoMakeProjectInitializer {
    fn init_project(
        &self, name: Option<String>, template: Option<String>, here: bool,
    ) -> Result<InitResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "project-init"]);

        if let Some(name) = name {
            cmd.arg("--name").arg(name);
        }

        if let Some(template) = template {
            cmd.arg("--template").arg(template);
        }

        if here {
            cmd.arg("--here");
        }

        let output = cmd.output()?;
        Ok(InitResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakeDevInitializer;

impl DevInitializer for CargoMakeDevInitializer {
    fn init_dev(&self, all: bool) -> Result<InitResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "dev-init"]);

        if all {
            cmd.arg("--all");
        }

        let output = cmd.output()?;
        Ok(InitResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_init_shell_calls_initializer() {
        let mut mock = MockShellInitializer::new();
        mock.expect_init_shell()
            .with(eq("zsh"), eq(None::<String>), eq(false))
            .times(1)
            .returning(|_, _, _| {
                Ok(InitResult {
                    stdout: "Shell initialized".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ShellInitArgs {
            shell: "zsh".to_string(),
            config: None,
            force: false,
        };
        let result = init_shell_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_init_shell_with_config() {
        let mut mock = MockShellInitializer::new();
        mock.expect_init_shell()
            .with(eq("bash"), eq(Some("/tmp/bashrc".to_string())), eq(true))
            .times(1)
            .returning(|_, _, _| {
                Ok(InitResult {
                    stdout: "Shell initialized".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ShellInitArgs {
            shell: "bash".to_string(),
            config: Some("/tmp/bashrc".to_string()),
            force: true,
        };
        let result = init_shell_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_init_project_calls_initializer() {
        let mut mock = MockProjectInitializer::new();
        mock.expect_init_project()
            .with(
                eq(Some("my-project".to_string())),
                eq(Some("rust-cli".to_string())),
                eq(false),
            )
            .times(1)
            .returning(|_, _, _| {
                Ok(InitResult {
                    stdout: "Project initialized".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = ProjectInitArgs {
            name: Some("my-project".to_string()),
            template: Some("rust-cli".to_string()),
            here: false,
        };
        let result = init_project_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_init_dev_calls_initializer() {
        let mut mock = MockDevInitializer::new();
        mock.expect_init_dev()
            .with(eq(false))
            .times(1)
            .returning(|_| {
                Ok(InitResult {
                    stdout: "Dev environment initialized".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = DevInitArgs {
            tool: None,
            config: None,
        };
        let result = init_dev_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock_shell_init = MockShellInitializer::new();
        mock_shell_init
            .expect_init_shell()
            .with(eq("fish"), eq(None::<String>), eq(false))
            .times(1)
            .returning(|_, _, _| {
                Ok(InitResult {
                    stdout: "Shell initialized".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let mock_project_init = MockProjectInitializer::new();
        let mock_dev_init = MockDevInitializer::new();

        let args = InitArgs {
            action: InitAction::Shell(ShellInitArgs {
                shell: "fish".to_string(),
                config: None,
                force: false,
            }),
        };

        let result =
            run_with_deps(&args, &mock_shell_init, &mock_project_init, &mock_dev_init).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_shell_init_args_defaults() {
        let args = ShellInitArgs {
            shell: "zsh".to_string(),
            config: None,
            force: false,
        };
        assert_eq!(args.shell, "zsh");
        assert!(args.config.is_none());
        assert!(!args.force);
    }

    #[test]
    fn test_project_init_args_defaults() {
        let args = ProjectInitArgs {
            name: None,
            template: None,
            here: false,
        };
        assert!(args.name.is_none());
        assert!(args.template.is_none());
        assert!(!args.here);
    }

    #[test]
    fn test_dev_init_args_defaults() {
        let args = DevInitArgs {
            tool: None,
            config: None,
        };
        assert!(args.tool.is_none());
        assert!(args.config.is_none());
    }
}
