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
    fn generate(&self, shell: &str, output: Option<String>) -> Result<CompletionResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait CompletionInstaller {
    fn install(&self, shell: &str, force: bool) -> Result<InstallResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait ShellLister {
    fn list(&self) -> Result<ShellListResult>;
}

/// Result of shell completion generation
#[derive(Debug, Clone)]
pub struct CompletionResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

/// Result of shell completion installation
#[derive(Debug, Clone)]
pub struct InstallResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

/// Result of listing available shells
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

// Concrete implementations for production use
pub struct CargoMakeCompletionGenerator;

impl CompletionGenerator for CargoMakeCompletionGenerator {
    fn generate(&self, shell: &str, output: Option<String>) -> Result<CompletionResult> {
        // Use clap_complete to generate completions directly
        use clap_complete::{generate, Shell};
        
        let shell_type = match shell {
            "bash" => Shell::Bash,
            "zsh" => Shell::Zsh,
            "fish" => Shell::Fish,
            "powershell" => Shell::PowerShell,
            _ => {
                return Ok(CompletionResult {
                    stdout: "".to_string(),
                    stderr: format!("Unsupported shell: {}", shell),
                    success: false,
                });
            }
        };

        // Get the CLI app from the main binary
        let mut app = crate::build_cli();
        
        let mut buf = Vec::new();
        generate(shell_type, &mut app, "ggen", &mut buf);
        
        let completion_script = String::from_utf8_lossy(&buf).to_string();
        
        // Write to output file if specified
        if let Some(output_path) = output {
            std::fs::write(&output_path, &completion_script)?;
        }

        Ok(CompletionResult {
            stdout: completion_script,
            stderr: "".to_string(),
            success: true,
        })
    }
}

pub struct CargoMakeCompletionInstaller;

impl CompletionInstaller for CargoMakeCompletionInstaller {
    fn install(&self, shell: &str, force: bool) -> Result<InstallResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "completions-install"]);

        cmd.arg("--shell").arg(shell);

        if force {
            cmd.arg("--force");
        }

        let output = cmd.output()?;
        Ok(InstallResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakeShellLister;

impl ShellLister for CargoMakeShellLister {
    fn list(&self) -> Result<ShellListResult> {
        let shells_info = "Supported shells:\n  bash      - Bash shell completion\n  zsh       - Zsh shell completion\n  fish      - Fish shell completion\n  powershell - PowerShell completion\n";
        Ok(ShellListResult {
            stdout: shells_info.to_string(),
            stderr: "".to_string(),
            success: true,
        })
    }
}

pub async fn run(args: &CompletionArgs) -> Result<()> {
    let generator = CargoMakeCompletionGenerator;
    let installer = CargoMakeCompletionInstaller;
    let lister = CargoMakeShellLister;

    run_with_deps(args, &generator, &installer, &lister).await
}

pub async fn run_with_deps(
    args: &CompletionArgs, generator: &dyn CompletionGenerator,
    installer: &dyn CompletionInstaller, lister: &dyn ShellLister,
) -> Result<()> {
    match &args.action {
        CompletionAction::Generate(generate_args) => {
            generate_completion_with_deps(generate_args, generator).await
        }
        CompletionAction::Install(install_args) => {
            install_completion_with_deps(install_args, installer).await
        }
        CompletionAction::List => list_shells_with_deps(lister).await,
    }
}

async fn generate_completion_with_deps(
    args: &GenerateArgs, generator: &dyn CompletionGenerator,
) -> Result<()> {
    println!("Generating completion script for {}", args.shell);

    let result = generator.generate(&args.shell, args.output.clone())?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Completion generation failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

#[allow(dead_code)]
async fn generate_completion(args: &GenerateArgs) -> Result<()> {
    let generator = CargoMakeCompletionGenerator;
    generate_completion_with_deps(args, &generator).await
}

async fn install_completion_with_deps(
    args: &InstallArgs, installer: &dyn CompletionInstaller,
) -> Result<()> {
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

#[allow(dead_code)]
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

#[allow(dead_code)]
async fn list_shells() -> Result<()> {
    let lister = CargoMakeShellLister;
    list_shells_with_deps(&lister).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_generate_calls_generator() {
        let mut mock = MockCompletionGenerator::new();
        mock.expect_generate()
            .with(eq("bash"), eq(None::<String>))
            .times(1)
            .returning(|_, _| {
                Ok(CompletionResult {
                    stdout: "Completion script".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = GenerateArgs {
            shell: "bash".to_string(),
            output: None,
        };
        let result = generate_completion_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_generate_with_output_file() {
        let mut mock = MockCompletionGenerator::new();
        mock.expect_generate()
            .with(eq("zsh"), eq(Some("/tmp/ggen.zsh".to_string())))
            .times(1)
            .returning(|_, _| {
                Ok(CompletionResult {
                    stdout: "Completion script".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = GenerateArgs {
            shell: "zsh".to_string(),
            output: Some("/tmp/ggen.zsh".to_string()),
        };
        let result = generate_completion_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_install_calls_installer() {
        let mut mock = MockCompletionInstaller::new();
        mock.expect_install()
            .with(eq("fish"), eq(false))
            .times(1)
            .returning(|_, _| {
                Ok(InstallResult {
                    stdout: "Installation complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = InstallArgs {
            shell: "fish".to_string(),
            force: false,
        };
        let result = install_completion_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_install_with_force() {
        let mut mock = MockCompletionInstaller::new();
        mock.expect_install()
            .with(eq("powershell"), eq(true))
            .times(1)
            .returning(|_, _| {
                Ok(InstallResult {
                    stdout: "Force installation complete".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let args = InstallArgs {
            shell: "powershell".to_string(),
            force: true,
        };
        let result = install_completion_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_calls_lister() {
        let mut mock = MockShellLister::new();
        mock.expect_list().times(1).returning(|| {
            Ok(ShellListResult {
                stdout: "Supported shells:\n  bash\n  zsh\n".to_string(),
                stderr: "".to_string(),
                success: true,
            })
        });

        let result = list_shells_with_deps(&mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock_generator = MockCompletionGenerator::new();
        mock_generator
            .expect_generate()
            .with(eq("bash"), eq(None::<String>))
            .times(1)
            .returning(|_, _| {
                Ok(CompletionResult {
                    stdout: "Completion script".to_string(),
                    stderr: "".to_string(),
                    success: true,
                })
            });

        let mock_installer = MockCompletionInstaller::new();
        let mock_lister = MockShellLister::new();

        let args = CompletionArgs {
            action: CompletionAction::Generate(GenerateArgs {
                shell: "bash".to_string(),
                output: None,
            }),
        };

        let result = run_with_deps(&args, &mock_generator, &mock_installer, &mock_lister).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_generate_args_defaults() {
        let args = GenerateArgs {
            shell: "bash".to_string(),
            output: None,
        };
        assert_eq!(args.shell, "bash");
        assert!(args.output.is_none());
    }

    #[test]
    fn test_install_args_defaults() {
        let args = InstallArgs {
            shell: "zsh".to_string(),
            force: false,
        };
        assert_eq!(args.shell, "zsh");
        assert!(!args.force);
    }
}
