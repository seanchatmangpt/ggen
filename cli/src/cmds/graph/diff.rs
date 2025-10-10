use clap::Args;
use ggen_core::delta::{GraphDelta, ImpactAnalyzer};
use ggen_core::graph::Graph;
use ggen_utils::error::Result;
use std::path::PathBuf;

#[derive(Args, Debug)]
pub struct DiffArgs {
    /// Path to baseline RDF file
    #[arg(short, long)]
    pub baseline: PathBuf,

    /// Path to current RDF file
    #[arg(short, long)]
    pub current: PathBuf,

    /// Output format (human, json, hash)
    #[arg(short, long, default_value = "human")]
    pub format: String,

    /// Filter results to specific IRIs
    #[arg(short, long)]
    pub filter: Vec<String>,

    /// Show affected templates
    #[arg(short = 't', long)]
    pub show_templates: bool,

    /// Template paths to analyze for impacts
    #[arg(short = 'T', long)]
    pub template_paths: Vec<PathBuf>,
}

pub async fn run(args: &DiffArgs) -> Result<()> {
    // Load baseline graph
    let baseline_graph = Graph::new()?;
    baseline_graph.load_path(&args.baseline)?;

    // Load current graph
    let current_graph = Graph::new()?;
    current_graph.load_path(&args.current)?;

    // Compute delta
    let delta = GraphDelta::new(&baseline_graph, &current_graph)?;

    // Apply filters if specified
    let delta = if !args.filter.is_empty() {
        delta.filter_by_iris(&args.filter)
    } else {
        delta
    };

    // Output based on format
    match args.format.as_str() {
        "human" => print_human(&delta)?,
        "json" => println!("{}", serde_json::to_string_pretty(&delta)?),
        "hash" => println!("{}", delta.baseline_hash.as_ref().unwrap_or(&String::new())),
        _ => {
            return Err(ggen_utils::error::Error::new(&format!(
                "Unknown format: {}",
                args.format
            )))
        }
    }

    // Show template impacts if requested
    if args.show_templates && !args.template_paths.is_empty() {
        let mut analyzer = ImpactAnalyzer::new();
        let impacts = analyzer.analyze_impacts(
            &delta,
            &args
                .template_paths
                .iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>(),
            &baseline_graph,
        )?;

        println!("\nTemplate Impacts:");
        for impact in impacts {
            if impact.is_confident(0.1) {
                println!(
                    "  {}: {:.2} - {}",
                    impact.template_path, impact.confidence, impact.reason
                );
            }
        }
    }

    Ok(())
}

fn print_human(delta: &GraphDelta) -> Result<()> {
    if delta.is_empty() {
        println!("No changes detected between graphs.");
        return Ok(());
    }

    println!("Graph Delta Summary:");
    println!(
        "  Baseline hash: {}",
        delta.baseline_hash.as_deref().unwrap_or("unknown")
    );
    println!(
        "  Current hash:  {}",
        delta.current_hash.as_deref().unwrap_or("unknown")
    );
    println!(
        "  Computed at:   {}",
        delta.computed_at.format("%Y-%m-%d %H:%M:%S UTC")
    );

    let counts = delta.counts();
    println!("  Changes:");
    println!(
        "    Additions:     {}",
        counts.get("additions").unwrap_or(&0)
    );
    println!(
        "    Deletions:     {}",
        counts.get("deletions").unwrap_or(&0)
    );
    println!(
        "    Modifications: {}",
        counts.get("modifications").unwrap_or(&0)
    );

    if !delta.deltas.is_empty() {
        println!("\nDetailed Changes:");
        for (i, delta_change) in delta.deltas.iter().enumerate() {
            println!("  {}. {}", i + 1, delta_change);
        }
    }

    Ok(())
}
