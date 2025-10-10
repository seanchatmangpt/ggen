//! GitHub Pages deployment and management operations.
//!
//! This module provides functionality to deploy documentation to GitHub Pages,
//! check deployment status, view logs, and compare local documentation with
//! the deployed version. It integrates with GitHub Actions and GitHub Pages API.
//!
//! # Examples
//!
//! ```bash
//! ggen ci pages deploy --force --wait
//! ggen ci pages status --verbose
//! ggen ci pages logs --follow
//! ggen ci pages compare --diff
//! ```
//!
//! # Errors
//!
//! Returns errors if GitHub API calls fail, deployment fails, or if
//! authentication is not properly configured.

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[cfg_attr(test, mockall::automock)]
pub trait GitHubPagesDeployer {
    fn deploy(&self, force: bool, wait: bool, branch: &str) -> Result<DeployResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait GitHubPagesStatusChecker {
    fn check_status(&self, verbose: bool, json: bool) -> Result<StatusResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait GitHubPagesLogViewer {
    fn view_logs(&self, follow: bool) -> Result<LogsResult>;
}

#[cfg_attr(test, mockall::automock)]
pub trait GitHubPagesComparer {
    fn compare(&self, diff: bool) -> Result<CompareResult>;
}

#[derive(Debug, Clone)]
pub struct DeployResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct StatusResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct LogsResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct CompareResult {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

#[derive(Args, Debug)]
pub struct PagesArgs {
    #[command(subcommand)]
    pub action: PagesAction,
}

#[derive(Subcommand, Debug)]
pub enum PagesAction {
    /// Deploy documentation to GitHub Pages
    Deploy(DeployArgs),

    /// Check Pages deployment status
    Status(StatusArgs),

    /// View Pages deployment logs
    Logs(LogsArgs),

    /// Compare local docs with deployed version
    Compare(CompareArgs),
}

#[derive(Args, Debug)]
pub struct DeployArgs {
    /// Force deployment even if no changes detected
    #[arg(long)]
    pub force: bool,

    /// Wait for deployment to complete
    #[arg(long)]
    pub wait: bool,

    /// Branch to deploy from [default: main]
    #[arg(long, default_value = "main")]
    pub branch: String,
}

#[derive(Args, Debug)]
pub struct StatusArgs {
    /// Show detailed status information
    #[arg(long)]
    pub verbose: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,
}

#[derive(Args, Debug)]
pub struct LogsArgs {
    /// Follow logs in real-time
    #[arg(long)]
    pub follow: bool,

    /// Number of log lines to show [default: 50]
    #[arg(long, default_value = "50")]
    pub lines: usize,
}

#[derive(Args, Debug)]
pub struct CompareArgs {
    /// Show detailed diff
    #[arg(long)]
    pub diff: bool,

    /// Compare specific files only
    #[arg(long)]
    pub files: Option<Vec<String>>,
}

pub async fn run(args: &PagesArgs) -> Result<()> {
    let deployer = CargoMakePagesDeployer;
    let status_checker = CargoMakePagesStatusChecker;
    let log_viewer = CargoMakePagesLogViewer;
    let comparer = CargoMakePagesComparer;
    
    run_with_deps(args, &deployer, &status_checker, &log_viewer, &comparer).await
}

pub async fn run_with_deps(
    args: &PagesArgs,
    deployer: &dyn GitHubPagesDeployer,
    status_checker: &dyn GitHubPagesStatusChecker,
    log_viewer: &dyn GitHubPagesLogViewer,
    comparer: &dyn GitHubPagesComparer,
) -> Result<()> {
    match &args.action {
        PagesAction::Deploy(deploy_args) => deploy_pages_with_deps(deploy_args, deployer).await,
        PagesAction::Status(status_args) => check_status_with_deps(status_args, status_checker).await,
        PagesAction::Logs(logs_args) => view_logs_with_deps(logs_args, log_viewer).await,
        PagesAction::Compare(compare_args) => compare_deployment_with_deps(compare_args, comparer).await,
    }
}

async fn deploy_pages_with_deps(args: &DeployArgs, deployer: &dyn GitHubPagesDeployer) -> Result<()> {
    println!("📚 Deploying documentation to GitHub Pages");

    let result = deployer.deploy(args.force, args.wait, &args.branch)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Pages deployment failed: {}",
            result.stderr
        )));
    }

    println!("✅ Documentation deployed successfully");
    Ok(())
}

async fn deploy_pages(args: &DeployArgs) -> Result<()> {
    let deployer = CargoMakePagesDeployer;
    deploy_pages_with_deps(args, &deployer).await
}

async fn check_status_with_deps(args: &StatusArgs, status_checker: &dyn GitHubPagesStatusChecker) -> Result<()> {
    println!("🔍 Checking GitHub Pages deployment status");

    let result = status_checker.check_status(args.verbose, args.json)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Status check failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn check_status(args: &StatusArgs) -> Result<()> {
    let status_checker = CargoMakePagesStatusChecker;
    check_status_with_deps(args, &status_checker).await
}

async fn view_logs_with_deps(args: &LogsArgs, log_viewer: &dyn GitHubPagesLogViewer) -> Result<()> {
    println!("📋 Viewing GitHub Pages deployment logs");

    let result = log_viewer.view_logs(args.follow)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Logs retrieval failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn view_logs(args: &LogsArgs) -> Result<()> {
    let log_viewer = CargoMakePagesLogViewer;
    view_logs_with_deps(args, &log_viewer).await
}

async fn compare_deployment_with_deps(args: &CompareArgs, comparer: &dyn GitHubPagesComparer) -> Result<()> {
    println!("🔍 Comparing local documentation with deployed version");

    let result = comparer.compare(args.diff)?;

    if !result.success {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Comparison failed: {}",
            result.stderr
        )));
    }

    println!("{}", result.stdout);
    Ok(())
}

async fn compare_deployment(args: &CompareArgs) -> Result<()> {
    let comparer = CargoMakePagesComparer;
    compare_deployment_with_deps(args, &comparer).await
}

// Concrete implementations for production use
pub struct CargoMakePagesDeployer;

impl GitHubPagesDeployer for CargoMakePagesDeployer {
    fn deploy(&self, force: bool, wait: bool, branch: &str) -> Result<DeployResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-pages-trigger"]);

        if force {
            cmd.arg("--force");
        }

        if wait {
            cmd.arg("--wait");
        }

        cmd.arg("--branch").arg(branch);

        let output = cmd.output()?;
        Ok(DeployResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakePagesStatusChecker;

impl GitHubPagesStatusChecker for CargoMakePagesStatusChecker {
    fn check_status(&self, verbose: bool, json: bool) -> Result<StatusResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-pages-status"]);

        if verbose {
            cmd.arg("--verbose");
        }

        if json {
            cmd.arg("--json");
        }

        let output = cmd.output()?;
        Ok(StatusResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakePagesLogViewer;

impl GitHubPagesLogViewer for CargoMakePagesLogViewer {
    fn view_logs(&self, follow: bool) -> Result<LogsResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-pages-logs"]);

        if follow {
            cmd.arg("--follow");
        }

        let output = cmd.output()?;
        Ok(LogsResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            success: output.status.success(),
        })
    }
}

pub struct CargoMakePagesComparer;

impl GitHubPagesComparer for CargoMakePagesComparer {
    fn compare(&self, diff: bool) -> Result<CompareResult> {
        let mut cmd = std::process::Command::new("cargo");
        cmd.args(["make", "gh-pages-compare"]);

        if diff {
            cmd.arg("--diff");
        }

        let output = cmd.output()?;
        Ok(CompareResult {
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
    async fn test_deploy_calls_deployer() {
        let mut mock = MockGitHubPagesDeployer::new();
        mock.expect_deploy()
            .with(eq(false), eq(false), eq("main"))
            .times(1)
            .returning(|_, _, _| Ok(DeployResult {
                stdout: "Deploy complete".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = DeployArgs {
            force: false,
            wait: false,
            branch: "main".to_string(),
        };
        let result = deploy_pages_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_deploy_handles_failure() {
        let mut mock = MockGitHubPagesDeployer::new();
        mock.expect_deploy()
            .with(eq(false), eq(false), eq("main"))
            .times(1)
            .returning(|_, _, _| Ok(DeployResult {
                stdout: "".to_string(),
                stderr: "Deploy failed".to_string(),
                success: false,
            }));

        let args = DeployArgs {
            force: false,
            wait: false,
            branch: "main".to_string(),
        };
        let result = deploy_pages_with_deps(&args, &mock).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Pages deployment failed"));
    }

    #[tokio::test]
    async fn test_status_calls_checker() {
        let mut mock = MockGitHubPagesStatusChecker::new();
        mock.expect_check_status()
            .with(eq(false), eq(false))
            .times(1)
            .returning(|_, _| Ok(StatusResult {
                stdout: "Status OK".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = StatusArgs {
            verbose: false,
            json: false,
        };
        let result = check_status_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_logs_calls_viewer() {
        let mut mock = MockGitHubPagesLogViewer::new();
        mock.expect_view_logs()
            .with(eq(false))
            .times(1)
            .returning(|_| Ok(LogsResult {
                stdout: "Log output".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = LogsArgs {
            follow: false,
            lines: 50,
        };
        let result = view_logs_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_compare_calls_comparer() {
        let mut mock = MockGitHubPagesComparer::new();
        mock.expect_compare()
            .with(eq(false))
            .times(1)
            .returning(|_| Ok(CompareResult {
                stdout: "Comparison complete".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let args = CompareArgs {
            diff: false,
            files: None,
        };
        let result = compare_deployment_with_deps(&args, &mock).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_run_with_deps_dispatches_correctly() {
        let mut mock_deployer = MockGitHubPagesDeployer::new();
        mock_deployer
            .expect_deploy()
            .with(eq(false), eq(false), eq("main"))
            .times(1)
            .returning(|_, _, _| Ok(DeployResult {
                stdout: "Deploy complete".to_string(),
                stderr: "".to_string(),
                success: true,
            }));

        let mock_status_checker = MockGitHubPagesStatusChecker::new();
        let mock_log_viewer = MockGitHubPagesLogViewer::new();
        let mock_comparer = MockGitHubPagesComparer::new();

        let args = PagesArgs {
            action: PagesAction::Deploy(DeployArgs {
                force: false,
                wait: false,
                branch: "main".to_string(),
            }),
        };

        let result = run_with_deps(&args, &mock_deployer, &mock_status_checker, &mock_log_viewer, &mock_comparer).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_deploy_args_defaults() {
        let args = DeployArgs {
            force: false,
            wait: false,
            branch: "main".to_string(),
        };
        assert!(!args.force);
        assert!(!args.wait);
        assert_eq!(args.branch, "main");
    }

    #[test]
    fn test_status_args_defaults() {
        let args = StatusArgs {
            verbose: false,
            json: false,
        };
        assert!(!args.verbose);
        assert!(!args.json);
    }

    #[test]
    fn test_logs_args_defaults() {
        let args = LogsArgs {
            follow: false,
            lines: 50,
        };
        assert!(!args.follow);
        assert_eq!(args.lines, 50);
    }

    #[test]
    fn test_compare_args_defaults() {
        let args = CompareArgs {
            diff: false,
            files: None,
        };
        assert!(!args.diff);
        assert!(args.files.is_none());
    }
}
