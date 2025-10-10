use clap::{Args, Subcommand};
use ggen_utils::error::Result;
// CLI output only - no library logging

#[derive(Args, Debug)]
pub struct PerformanceArgs {
    #[command(subcommand)]
    pub action: PerformanceAction,
}

#[derive(Subcommand, Debug)]
pub enum PerformanceAction {
    /// Benchmark performance characteristics
    Benchmark(BenchmarkArgs),

    /// Profile memory usage
    Memory(MemoryArgs),

    /// Check performance SLOs
    Slo(SloArgs),
}

#[derive(Args, Debug)]
pub struct BenchmarkArgs {
    /// Number of iterations [default: 100]
    #[arg(long, default_value = "100")]
    pub iterations: usize,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Show detailed metrics
    #[arg(long)]
    pub verbose: bool,
}

#[derive(Args, Debug)]
pub struct MemoryArgs {
    /// Memory limit in MB [default: 100]
    #[arg(long, default_value = "100")]
    pub limit: usize,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Show memory allocation details
    #[arg(long)]
    pub detailed: bool,
}

#[derive(Args, Debug)]
pub struct SloArgs {
    /// Check specific SLO [default: all]
    #[arg(long, default_value = "all")]
    pub slo: String,

    /// Output in JSON format
    #[arg(long)]
    pub json: bool,

    /// Show detailed SLO information
    #[arg(long)]
    pub verbose: bool,
}

pub async fn run(args: &PerformanceArgs) -> Result<()> {
    match &args.action {
        PerformanceAction::Benchmark(benchmark_args) => run_benchmark(benchmark_args).await,
        PerformanceAction::Memory(memory_args) => check_memory(memory_args).await,
        PerformanceAction::Slo(slo_args) => check_slo(slo_args).await,
    }
}

async fn run_benchmark(args: &BenchmarkArgs) -> Result<()> {
    println!("⚡ Running performance benchmarks");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "bench"]);

    cmd.arg("--iterations").arg(args.iterations.to_string());

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Benchmark failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn check_memory(args: &MemoryArgs) -> Result<()> {
    println!("💾 Checking memory usage");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "profile"]);

    cmd.arg("--memory-limit").arg(args.limit.to_string());

    if args.json {
        cmd.arg("--json");
    }

    if args.detailed {
        cmd.arg("--detailed");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "Memory check failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}

async fn check_slo(args: &SloArgs) -> Result<()> {
    println!("📊 Checking performance SLOs");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "slo-check"]);

    cmd.arg("--slo").arg(&args.slo);

    if args.json {
        cmd.arg("--json");
    }

    if args.verbose {
        cmd.arg("--verbose");
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new(&format!(
            "SLO check failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("{}", stdout);
    Ok(())
}
