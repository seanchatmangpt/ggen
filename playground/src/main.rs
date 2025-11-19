//! HTF CLI: Hyper-Thesis Framework Command Interface
//!
//! Commands:
//! - `htf schedule` - Plan chapters using Λ-scheduling
//! - `htf profile` - Show Π-profile coverage analysis
//! - `htf check` - Validate against Q-invariants with Γ-checker
//! - `htf add` - Add a new Δ-shard
//! - `htf list` - List all shards
//! - `htf export` - Export thesis in various formats

#![allow(non_upper_case_globals)]

use clap::{Parser, Subcommand};
use clap_noun_verb::Result as NounVerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::HashMap;

mod checker;
mod error;
mod models;
mod ontology;
mod profiler;
mod scheduler;

use error::Result;
use models::*;

// ============================================================================
// Output Types (Serializable for JSON/YAML)
// ============================================================================

#[derive(Serialize)]
struct ScheduleOutput {
    thesis_id: String,
    chapters: usize,
    total_shards: usize,
    total_words: usize,
    chapters_detail: Vec<ChapterDetail>,
}

#[derive(Serialize)]
struct ChapterDetail {
    number: usize,
    title: String,
    shards: usize,
    words: usize,
    families: Vec<String>,
}

#[derive(Serialize)]
struct ProfileOutput {
    thesis_id: String,
    total_words: usize,
    total_shards: usize,
    coverage: HashMap<String, f32>,
    report: String,
}

#[derive(Serialize)]
struct CheckOutput {
    is_valid: bool,
    passed: Vec<String>,
    failed: Vec<String>,
    drift: Vec<String>,
    recommendations: Vec<String>,
}

#[derive(Serialize)]
struct AddOutput {
    shard_id: String,
    family: String,
    status: String,
}

#[derive(Serialize)]
struct ListOutput {
    shards: Vec<ShardDetail>,
    total: usize,
}

#[derive(Serialize)]
struct ShardDetail {
    id: String,
    name: String,
    family: String,
    status: String,
    words: usize,
}

// ============================================================================
// Sample Data (for demonstration)
// ============================================================================

fn sample_shards() -> Vec<DeltaShard> {
    vec![
        DeltaShard {
            id: "intro-1".to_string(),
            name: "Introduction".to_string(),
            family: ShardFamily::Intro,
            content: "This thesis explores the intersection of formal methods and machine learning. ".repeat(100),
            status: ShardStatus::InProgress,
            dependencies: vec![],
        },
        DeltaShard {
            id: "problem-1".to_string(),
            name: "Problem Statement".to_string(),
            family: ShardFamily::Problem,
            content: "The fundamental challenge is bridging the gap between symbolic reasoning and neural learning. ".repeat(80),
            status: ShardStatus::InProgress,
            dependencies: vec!["intro-1".to_string()],
        },
        DeltaShard {
            id: "gap-1".to_string(),
            name: "Research Gap".to_string(),
            family: ShardFamily::Gap,
            content: "Existing work lacks integration between formal verification and deep learning. ".repeat(80),
            status: ShardStatus::Draft,
            dependencies: vec!["problem-1".to_string()],
        },
        DeltaShard {
            id: "method-1".to_string(),
            name: "Research Methodology".to_string(),
            family: ShardFamily::Method,
            content: "We employ a mixed-methods approach combining formal analysis with empirical evaluation. ".repeat(100),
            status: ShardStatus::InProgress,
            dependencies: vec!["gap-1".to_string()],
        },
        DeltaShard {
            id: "artifact-1".to_string(),
            name: "Research Artifact".to_string(),
            family: ShardFamily::Artifact,
            content: "A framework combining Coq verification with PyTorch neural networks. ".repeat(90),
            status: ShardStatus::InProgress,
            dependencies: vec!["method-1".to_string()],
        },
        DeltaShard {
            id: "eval-1".to_string(),
            name: "Evaluation".to_string(),
            family: ShardFamily::Evaluation,
            content: "Benchmarks show 95% improvement in verification time with acceptable loss in accuracy. ".repeat(85),
            status: ShardStatus::Draft,
            dependencies: vec!["artifact-1".to_string()],
        },
    ]
}

// ============================================================================
// CLI Verbs (using clap-noun-verb pattern)
// ============================================================================

/// Schedule chapters from shards using Λ-ordering
#[verb]
fn schedule(chapter_size: Option<usize>) -> NounVerbResult<ScheduleOutput> {
    let shards = sample_shards();
    let chapter_size = chapter_size.unwrap_or(2000);

    let plan = scheduler::schedule_chapters(shards.clone(), chapter_size).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Scheduling failed: {}", e))
    })?;

    let chapters_detail = plan
        .chapters
        .iter()
        .map(|ch| ChapterDetail {
            number: ch.number,
            title: ch.title.clone(),
            shards: ch.shards.len(),
            words: ch.estimated_words,
            families: ch.families.iter().map(|f| format!("{:?}", f)).collect(),
        })
        .collect();

    let total_words: usize = plan.chapters.iter().map(|ch| ch.estimated_words).sum();

    Ok(ScheduleOutput {
        thesis_id: plan.thesis_id,
        chapters: plan.chapters.len(),
        total_shards: shards.len(),
        total_words,
        chapters_detail,
    })
}

/// Show Π-profile coverage analysis
#[verb]
fn profile() -> NounVerbResult<ProfileOutput> {
    let shards = sample_shards();

    let profile = profiler::profile_thesis(shards.clone()).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Profiling failed: {}", e))
    })?;

    let coverage: HashMap<String, f32> = profile
        .coverage
        .iter()
        .map(|(f, v)| (format!("{:?}", f), *v))
        .collect();

    let report = profiler::generate_coverage_report(&profile);

    Ok(ProfileOutput {
        thesis_id: profile.thesis_id,
        total_words: profile.total_words,
        total_shards: profile.shards.len(),
        coverage,
        report,
    })
}

/// Validate thesis against Q-invariants
#[verb]
fn check() -> NounVerbResult<CheckOutput> {
    let shards = sample_shards();

    let result = checker::check_thesis(shards).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Checking failed: {}", e))
    })?;

    Ok(CheckOutput {
        is_valid: result.is_valid,
        passed: result.invariants_passed,
        failed: result.invariants_failed,
        drift: result.drift_detected,
        recommendations: result.recommendations,
    })
}

/// Add a new Δ-shard
#[verb]
fn add(_name: String, family: String) -> NounVerbResult<AddOutput> {
    let shard_id = format!("{}-{}", family.to_lowercase(), uuid::Uuid::new_v4());

    Ok(AddOutput {
        shard_id,
        family,
        status: "Draft".to_string(),
    })
}

/// List all shards
#[verb]
fn list() -> NounVerbResult<ListOutput> {
    let shards = sample_shards();

    let details = shards
        .iter()
        .map(|s| ShardDetail {
            id: s.id.clone(),
            name: s.name.clone(),
            family: format!("{:?}", s.family),
            status: format!("{:?}", s.status),
            words: s.content.split_whitespace().count(),
        })
        .collect();

    Ok(ListOutput {
        shards: details,
        total: shards.len(),
    })
}

/// Export thesis
#[verb]
fn export(format: Option<String>) -> NounVerbResult<String> {
    let format = format.as_deref().unwrap_or("json");
    Ok(format!("Exporting thesis in {} format...", format))
}

// ============================================================================
// Main CLI
// ============================================================================

#[derive(Parser)]
#[command(name = "htf")]
#[command(about = "Hyper-Thesis Framework: RDF-backed thesis planning", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Plan chapters using Λ-scheduling
    Schedule {
        /// Target chapter size in words
        #[arg(short, long)]
        chapter_size: Option<usize>,
    },
    /// Show Π-profile coverage analysis
    Profile,
    /// Validate against Q-invariants
    Check,
    /// Add a new Δ-shard
    Add {
        /// Shard name
        name: String,
        /// Shard family
        family: String,
    },
    /// List all shards
    List,
    /// Export thesis
    Export {
        /// Export format
        #[arg(short, long)]
        format: Option<String>,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Schedule { chapter_size } => {
            let output = schedule(chapter_size)?;
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok::<(), anyhow::Error>(())
        }
        Commands::Profile => {
            let output = profile()?;
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        Commands::Check => {
            let output = check()?;
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        Commands::Add { name, family } => {
            let output = add(name, family)?;
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        Commands::List => {
            let output = list()?;
            println!("{}", serde_json::to_string_pretty(&output)?);
            Ok(())
        }
        Commands::Export { format } => {
            let output = export(format)?;
            println!("{}", output);
            Ok(())
        }
    };

    result
}
