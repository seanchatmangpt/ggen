use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct SecurityArgs {
    #[command(subcommand)]
    pub action: SecurityAction,
}

#[derive(Subcommand, Debug)]
pub enum SecurityAction {
    /// Scan for security vulnerabilities
    Scan(ScanArgs),

    /// Check dependencies for known vulnerabilities
    Dependencies(DependenciesArgs),

    /// Audit configuration files
    Config(ConfigArgs),
}

#[derive(Args, Debug)]
pub struct ScanArgs {
    /// Directory to scan [default: current directory]
    #[arg(long, default_value = ".")]
    pub path: String,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Show detailed information
    #[arg(long)]
    pub verbose: bool,

    /// Fix automatically where possible
    #[arg(long)]
    pub fix: bool,
}

#[derive(Args, Debug)]
pub struct DependenciesArgs {
    /// Check only direct dependencies
    #[arg(long)]
    pub direct_only: bool,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Update vulnerable dependencies
    #[arg(long)]
    pub update: bool,
}

#[derive(Args, Debug)]
pub struct ConfigArgs {
    /// Configuration file to audit
    #[arg(long)]
    pub file: Option<String>,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Fix configuration issues
    #[arg(long)]
    pub fix: bool,
}

pub async fn run(args: &SecurityArgs) -> Result<()> {
    match &args.action {
        SecurityAction::Scan(scan_args) => scan_security(scan_args).await,
        SecurityAction::Dependencies(deps_args) => check_dependencies(deps_args).await,
        SecurityAction::Config(config_args) => audit_config(config_args).await,
    }
}

async fn scan_security(args: &ScanArgs) -> Result<()> {
    println!("🔒 Scanning for security vulnerabilities");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit"]);

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    if args.fix {
        cmd.arg("--fix");
    }

    cmd.arg("--path").arg(&args.path);

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Security scan failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn check_dependencies(args: &DependenciesArgs) -> Result<()> {
    println!("📦 Checking dependencies for security vulnerabilities");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit-deps"]);

    if args.direct_only {
        cmd.arg("--direct-only");
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.update {
        cmd.arg("--update");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Dependency security check failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn audit_config(args: &ConfigArgs) -> Result<()> {
    println!("⚙️ Auditing configuration files for security issues");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "audit-config"]);

    if let Some(file) = &args.file {
        cmd.arg("--file").arg(file);
    }

    if args.json {
        cmd.arg("--json");
    }

    if args.fix {
        cmd.arg("--fix");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Configuration audit failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}
