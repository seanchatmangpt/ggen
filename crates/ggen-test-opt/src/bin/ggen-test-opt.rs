// ggen-test-opt CLI - Test optimization and selection tooling
//
// Command-line interface for test value scoring, Pareto selection,
// metadata collection, and performance budget enforcement.

use clap::{Parser, Subcommand};
use ggen_test_opt::{MetadataCollector, OptResult, ParetoSelector, TestValueScorer};
use std::path::PathBuf;

/// ggen-test-opt - Test optimization and selection tooling
#[derive(Debug, Parser)]
#[command(name = "ggen-test-opt")]
#[command(version = "0.1.0")]
#[command(about = "Test optimization and selection for ggen")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

/// Available commands
#[derive(Debug, Subcommand)]
enum Commands {
    /// Run 80/20 Pareto selection to optimize test suite
    Optimize {
        /// Target number of tests to select (default: 200)
        #[arg(long, default_value = "200")]
        target_count: usize,

        /// Minimum bug detection rate (default: 0.80 = 80%)
        #[arg(long, default_value = "0.80")]
        min_detection_rate: f64,

        /// Path to test metadata directory (default: .ggen/test-metadata)
        #[arg(long, default_value = ".ggen/test-metadata")]
        metadata_dir: PathBuf,

        /// Path to output optimized suite manifest
        #[arg(long, default_value = ".ggen/test-metadata/optimized-suite.json")]
        output: PathBuf,
    },

    /// Update test execution metadata from latest test runs
    MetadataUpdate {
        /// Path to cargo-nextest JSON report
        #[arg(long)]
        nextest_json: Option<PathBuf>,

        /// Path to cargo-tarpaulin JSON report
        #[arg(long)]
        tarpaulin_json: Option<PathBuf>,

        /// Path to metadata storage directory (default: .ggen/test-metadata)
        #[arg(long, default_value = ".ggen/test-metadata")]
        metadata_dir: PathBuf,

        /// Test results to update failure history (JSON format)
        #[arg(long)]
        test_results: Option<PathBuf>,
    },

    /// Validate performance budgets for unit and integration tests
    BudgetCheck {
        /// Unit test budget in milliseconds (default: 1000ms)
        #[arg(long, default_value = "1000")]
        unit_budget: u64,

        /// Integration test budget in milliseconds (default: 10000ms)
        #[arg(long, default_value = "10000")]
        integration_budget: u64,

        /// Path to test metadata directory (default: .ggen/test-metadata)
        #[arg(long, default_value = ".ggen/test-metadata")]
        metadata_dir: PathBuf,

        /// Path to cargo-nextest JSON report with execution times
        #[arg(long)]
        nextest_json: Option<PathBuf>,
    },
}

fn main() -> OptResult<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Optimize {
            target_count,
            min_detection_rate,
            metadata_dir,
            output,
        } => {
            execute_optimize(target_count, min_detection_rate, metadata_dir, output)?;
        }

        Commands::MetadataUpdate {
            nextest_json,
            tarpaulin_json,
            metadata_dir,
            test_results,
        } => {
            execute_metadata_update(nextest_json, tarpaulin_json, metadata_dir, test_results)?;
        }

        Commands::BudgetCheck {
            unit_budget,
            integration_budget,
            metadata_dir,
            nextest_json,
        } => {
            let exit_code =
                execute_budget_check(unit_budget, integration_budget, metadata_dir, nextest_json)?;
            std::process::exit(exit_code);
        }
    }

    Ok(())
}

// Execute optimize command - 80/20 Pareto selection
fn execute_optimize(
    target_count: usize, min_detection_rate: f64, metadata_dir: PathBuf, output: PathBuf,
) -> OptResult<()> {
    println!("🚀 Starting test suite optimization...");
    println!("   Target: {} tests", target_count);
    println!("   Min bug detection: {:.1}%", min_detection_rate * 100.0);
    println!();

    // Initialize metadata collector
    let collector = MetadataCollector::new(&metadata_dir);

    // Collect execution times (if available)
    let nextest_path = metadata_dir.join("nextest-results.json");
    let execution_times = if nextest_path.exists() {
        println!("📊 Collecting execution times from nextest...");
        collector.collect_execution_times(&nextest_path)?
    } else {
        println!("⚠️  No nextest results found, skipping execution times");
        Default::default()
    };

    // Collect coverage data (if available)
    let tarpaulin_path = metadata_dir.join("tarpaulin-coverage.json");
    let coverage_data = if tarpaulin_path.exists() {
        println!("📊 Collecting coverage data from tarpaulin...");
        collector.collect_coverage_data(&tarpaulin_path)?
    } else {
        println!("⚠️  No tarpaulin results found, skipping coverage");
        Default::default()
    };

    // Collect failure history
    println!("📊 Collecting failure history...");
    let failure_history = collector.collect_failure_history()?;

    // Calculate test value scores
    println!("🧮 Calculating test value scores...");
    let scorer = TestValueScorer::new();
    let mut scores = Vec::new();

    // Calculate component scores for each test
    for (test_id, (test_type, exec_time)) in execution_times.iter() {
        let (lines_covered, total_lines) = coverage_data.get(test_id).copied().unwrap_or((0, 100));
        let (failures, total_runs) = failure_history.get(test_id).copied().unwrap_or((0, 1));

        // Calculate component scores (0-100 scale)
        let failure_freq = scorer.calculate_failure_freq_score(failures, total_runs);
        let coverage = scorer.calculate_coverage_score(lines_covered, total_lines);
        let speed = scorer.calculate_speed_score(*exec_time, 1000); // Assume 1s max
        let criticality = scorer.calculate_criticality_score(&[test_id.as_str().to_string()]);
        let budget_penalty = scorer.calculate_budget_penalty(
            *exec_time,
            match test_type {
                ggen_test_opt::TestType::Unit => 1000,
                ggen_test_opt::TestType::Integration => 10000,
            },
        );

        // Calculate composite score
        let score = scorer.calculate_composite_score(
            test_id.clone(),
            failure_freq,
            coverage,
            speed,
            criticality,
            budget_penalty,
        );

        scores.push(score);
    }

    println!("   Scored {} tests", scores.len());
    println!();

    // Execute Pareto selection
    println!("🎯 Executing 80/20 Pareto selection...");
    let selector = ParetoSelector::with_config(min_detection_rate, target_count);
    let result = selector.execute_selection(scores)?;

    // Display results
    println!("✅ Optimization complete!");
    println!();
    println!("📊 Results:");
    println!(
        "   Selected tests: {} / {}",
        result.selected_count, result.total_tests
    );
    println!(
        "   Reduction: {:.1}%",
        (1.0 - result.selected_count as f64 / result.total_tests as f64) * 100.0
    );
    println!(
        "   Bug detection rate: {:.1}%",
        result.bug_detection_rate * 100.0
    );
    println!();

    // Save optimized suite manifest (simplified version without full serialization)
    println!("💾 Saving optimized suite to {}...", output.display());
    let summary = format!(
        "Optimized suite: {}/{} tests selected ({:.1}% reduction), {:.1}% bug detection",
        result.selected_count,
        result.total_tests,
        (1.0 - result.selected_count as f64 / result.total_tests as f64) * 100.0,
        result.bug_detection_rate * 100.0
    );
    std::fs::create_dir_all(output.parent().unwrap())?;
    std::fs::write(&output, summary)?;

    println!("✅ Done!");

    Ok(())
}

// Execute metadata-update command
fn execute_metadata_update(
    nextest_json: Option<PathBuf>, tarpaulin_json: Option<PathBuf>, metadata_dir: PathBuf,
    test_results: Option<PathBuf>,
) -> OptResult<()> {
    println!("🔄 Updating test metadata...");
    println!();

    let collector = MetadataCollector::new(&metadata_dir);

    // Update execution times if nextest JSON provided
    if let Some(nextest_path) = nextest_json {
        println!(
            "📊 Collecting execution times from {}...",
            nextest_path.display()
        );
        let execution_times = collector.collect_execution_times(&nextest_path)?;
        println!(
            "   Collected {} test execution times",
            execution_times.len()
        );
        println!();
    }

    // Update coverage data if tarpaulin JSON provided
    if let Some(tarpaulin_path) = tarpaulin_json {
        println!(
            "📊 Collecting coverage data from {}...",
            tarpaulin_path.display()
        );
        let coverage_data = collector.collect_coverage_data(&tarpaulin_path)?;
        println!("   Collected coverage for {} tests", coverage_data.len());
        println!();
    }

    // Update failure history if test results provided
    if let Some(results_path) = test_results {
        println!(
            "📊 Updating failure history from {}...",
            results_path.display()
        );
        let results_json = std::fs::read_to_string(&results_path)?;
        let test_results = serde_json::from_str(&results_json)?;
        collector.update_failure_history(&test_results)?;
        println!("   Updated failure history");
        println!();
    }

    println!("✅ Metadata update complete!");

    Ok(())
}

// Execute budget-check command
fn execute_budget_check(
    unit_budget: u64, integration_budget: u64, metadata_dir: PathBuf, nextest_json: Option<PathBuf>,
) -> OptResult<i32> {
    println!("⏱️  Checking performance budgets...");
    println!("   Unit budget: {}ms", unit_budget);
    println!("   Integration budget: {}ms", integration_budget);
    println!();

    let collector = MetadataCollector::new(&metadata_dir);

    // Load execution times
    let nextest_path = nextest_json.unwrap_or_else(|| metadata_dir.join("execution-times.json"));
    let execution_times = collector.collect_execution_times(&nextest_path)?;

    // Calculate totals by test type
    let mut unit_total: u64 = 0;
    let mut integration_total: u64 = 0;

    for (_test_id, (test_type, exec_time)) in execution_times.iter() {
        match test_type {
            ggen_test_opt::TestType::Unit => unit_total += exec_time,
            ggen_test_opt::TestType::Integration => integration_total += exec_time,
        }
    }

    // Check totals
    let unit_margin = ((unit_budget as f64 - unit_total as f64) / unit_budget as f64) * 100.0;
    let integration_margin = ((integration_budget as f64 - integration_total as f64)
        / integration_budget as f64)
        * 100.0;

    println!("📊 Budget Status:");
    println!(
        "   Unit: {}ms / {}ms ({:.1}% margin)",
        unit_total, unit_budget, unit_margin
    );
    println!(
        "   Integration: {}ms / {}ms ({:.1}% margin)",
        integration_total, integration_budget, integration_margin
    );
    println!();

    // Check for violations
    if unit_total > unit_budget {
        println!(
            "🔴 CRITICAL: Total unit test time {}ms exceeds budget {}ms",
            unit_total, unit_budget
        );
        return Ok(2);
    }

    if integration_total > integration_budget {
        println!(
            "🔴 CRITICAL: Total integration test time {}ms exceeds budget {}ms",
            integration_total, integration_budget
        );
        return Ok(2);
    }

    println!("✅ All budgets met!");
    Ok(0)
}
