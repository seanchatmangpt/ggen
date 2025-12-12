//! Test quality audit and optimization commands
//!
//! Provides CLI commands for test quality analysis and optimization:
//! - `ggen test audit` - Run comprehensive test quality audit
//! - `ggen test optimize` - Select optimal subset of tests using Pareto principle
//! - `ggen test budget-check` - Validate performance budgets

use clap::Parser;
use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ggen_test_audit::{
    AssertionAnalyzer, FalsePositiveDetector, MutationAnalyzer, ReportGenerator,
};
use std::path::PathBuf;
use std::str::FromStr;

fn exec_err(msg: impl Into<String>) -> NounVerbError {
    NounVerbError::execution_error(msg.into())
}

fn parse_args_from_str<T: Parser>(s: &str, verb: &str) -> std::result::Result<T, NounVerbError> {
    let mut parts = vec!["ggen", "test", verb];
    parts.extend(s.split_whitespace());
    T::try_parse_from(parts).map_err(|e| exec_err(e.to_string()))
}

/// Test quality audit command
///
/// Runs comprehensive test quality analysis:
/// - Mutation testing via cargo-mutants (target: ≥80% kill rate)
/// - Assertion strength analysis (detect weak assertions)
/// - False positive detection (tests passing when code is broken)
/// - Critical path coverage validation (RDF, ontology, code gen, ggen.toml)
///
/// # Example
/// ```bash
/// ggen test audit --fail-on-threshold --output-format json
/// ```
#[derive(Parser, Debug)]
pub struct AuditArgs {
    /// Exit with code 2 if mutation kill rate < 80%
    #[clap(long)]
    pub fail_on_threshold: bool,

    /// Output format: json, markdown, both
    #[clap(long, default_value = "both")]
    pub output_format: String,

    /// Output directory for reports
    #[clap(long, default_value = ".ggen/test-metadata")]
    pub output_dir: PathBuf,

    /// Workspace root directory
    #[clap(long, default_value = ".")]
    pub workspace_root: PathBuf,

    /// Test directories to analyze
    #[clap(long, default_value = "tests")]
    pub test_dirs: Vec<PathBuf>,
}

impl FromStr for AuditArgs {
    type Err = NounVerbError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        parse_args_from_str(s, "audit")
    }
}

/// Run test quality audit
///
/// # Errors
/// Returns error if:
/// - Mutation testing fails
/// - Test analysis fails
/// - Report generation fails
/// - Kill rate below threshold (with --fail-on-threshold)
#[verb("audit", "test")]
pub fn audit(args: AuditArgs) -> Result<()> {
    println!("🔍 Running Test Quality Audit...\n");

    // 1. Mutation Testing
    println!("📊 Step 1/4: Running mutation testing...");
    let mut mutation_analyzer = MutationAnalyzer::new(&args.workspace_root, &args.output_dir)
        .map_err(|e| exec_err(format!("Mutation analyzer init failed: {}", e)))?;

    mutation_analyzer.add_critical_paths();
    let mutation_results = mutation_analyzer
        .run_mutations()
        .map_err(|e| exec_err(format!("Mutation testing failed: {}", e)))?;

    let kill_rate = mutation_analyzer.calculate_kill_rate(&mutation_results);
    println!(
        "  ✓ Mutation kill rate: {:.1}% ({}/{})",
        kill_rate * 100.0,
        mutation_results
            .iter()
            .filter(|r| !r.mutant_survived)
            .count(),
        mutation_results.len()
    );

    // 2. Assertion Strength Analysis
    println!("\n📝 Step 2/4: Analyzing assertion strength...");
    let assertion_analyzer = AssertionAnalyzer::new(args.test_dirs.clone());
    let assertions = assertion_analyzer
        .analyze_all_tests()
        .map_err(|e| exec_err(format!("Assertion analysis failed: {}", e)))?;

    let weak_count = assertions
        .iter()
        .filter(|a| {
            matches!(
                a.assertion_strength,
                ggen_test_audit::AssertionStrength::Weak
            )
        })
        .count();

    println!(
        "  ✓ Analyzed {} tests ({} weak assertions)",
        assertions.len(),
        weak_count
    );

    // 3. False Positive Detection
    println!("\n🐛 Step 3/4: Detecting false positives...");
    let false_positive_detector = FalsePositiveDetector::new(args.test_dirs.clone());
    let false_positive_report = false_positive_detector
        .generate_report(&assertions)
        .map_err(|e| exec_err(format!("False positive detection failed: {}", e)))?;

    println!(
        "  ✓ Found {} false positives",
        false_positive_report.execution_only_tests.len()
            + false_positive_report.ggen_toml_issues.len()
    );

    if !false_positive_report.ggen_toml_issues.is_empty() {
        println!(
            "  ⚠️  CRITICAL: {} ggen.toml test issues detected",
            false_positive_report.ggen_toml_issues.len()
        );
    }

    // 4. Report Generation
    println!("\n📄 Step 4/4: Generating quality report...");
    let report_generator = ReportGenerator::new(&args.output_dir)
        .map_err(|e| exec_err(format!("Report generator init failed: {}", e)))?;

    let quality_report = report_generator.generate_quality_report(
        &mutation_results,
        &assertions,
        &false_positive_report,
    );

    // Export based on format
    match args.output_format.as_str() {
        "json" => {
            let path = report_generator
                .export_json(&quality_report)
                .map_err(|e| exec_err(format!("JSON export failed: {}", e)))?;
            println!("  ✓ JSON report: {}", path.display());
        }
        "markdown" => {
            let path = report_generator
                .export_markdown(&quality_report)
                .map_err(|e| exec_err(format!("Markdown export failed: {}", e)))?;
            println!("  ✓ Markdown report: {}", path.display());
        }
        "both" => {
            let json_path = report_generator
                .export_json(&quality_report)
                .map_err(|e| exec_err(format!("JSON export failed: {}", e)))?;
            let md_path = report_generator
                .export_markdown(&quality_report)
                .map_err(|e| exec_err(format!("Markdown export failed: {}", e)))?;
            println!("  ✓ JSON report: {}", json_path.display());
            println!("  ✓ Markdown report: {}", md_path.display());
        }
        _ => {
            return Err(exec_err(format!(
                "Invalid output format: {}. Use json, markdown, or both",
                args.output_format
            )));
        }
    }

    // Print summary
    println!("\n📊 Summary:");
    println!(
        "  - Mutation kill rate: {:.1}% (target: 80%)",
        kill_rate * 100.0
    );
    println!("  - Weak assertions: {}", weak_count);
    println!(
        "  - False positives: {}",
        false_positive_report.execution_only_tests.len()
            + false_positive_report.ggen_toml_issues.len()
    );
    println!(
        "  - Critical path gaps: {}",
        false_positive_report.critical_path_gaps.len()
    );

    // Print recommendations
    if !quality_report.recommendations.is_empty() {
        println!("\n💡 Recommendations:");
        for (i, rec) in quality_report.recommendations.iter().enumerate().take(5) {
            println!(
                "  {}. [{:?}] {}: {}",
                i + 1,
                rec.priority,
                rec.category,
                rec.issue
            );
        }
    }

    // Check threshold and exit
    if args.fail_on_threshold && kill_rate < 0.80 {
        eprintln!(
            "\n❌ FAIL: Mutation kill rate {:.1}% below threshold 80%",
            kill_rate * 100.0
        );
        std::process::exit(2);
    }

    println!("\n✅ Test quality audit complete!");
    Ok(())
}

/// Placeholder for test optimization command (Feature 004 Phase 5)
///
/// Will implement 80/20 Pareto test selection:
/// - Calculate test value scores
/// - Select top 200 tests (from 1,178 total)
/// - Maintain 80%+ bug detection rate
/// - Meet performance budgets (unit ≤1s, integration ≤10s)
#[derive(Parser, Debug)]
pub struct OptimizeArgs {
    /// Output directory
    #[clap(long, default_value = ".ggen/test-metadata")]
    pub output_dir: PathBuf,
}

impl FromStr for OptimizeArgs {
    type Err = NounVerbError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        parse_args_from_str(s, "optimize")
    }
}

#[verb("optimize", "test")]
pub fn optimize(_args: OptimizeArgs) -> Result<()> {
    println!("⚡ Test optimization (Pareto 80/20 selection)");
    println!("📋 Status: Not implemented yet (Phase 5)");
    println!("🎯 Target: Select 200 high-value tests from 1,178 total");
    println!("🚀 Expected benefits: 83% reduction, <11s total runtime");
    Ok(())
}

/// Placeholder for budget check command (Feature 004 Phase 6)
///
/// Will validate performance budgets:
/// - Unit tests: ≤1s total
/// - Integration tests: ≤10s total
/// - Combined: ≤11s total
#[derive(Parser, Debug)]
pub struct BudgetCheckArgs {
    /// Fail if budgets exceeded
    #[clap(long)]
    pub fail_on_violation: bool,
}

impl FromStr for BudgetCheckArgs {
    type Err = NounVerbError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        parse_args_from_str(s, "budget-check")
    }
}

#[verb("budget-check", "test")]
pub fn budget_check(_args: BudgetCheckArgs) -> Result<()> {
    println!("⏱️  Performance budget validation");
    println!("📋 Status: Not implemented yet (Phase 6)");
    println!("🎯 Budgets: unit ≤1s, integration ≤10s, combined ≤11s");
    Ok(())
}
