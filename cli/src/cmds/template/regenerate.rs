use clap::Args;
use ggen_core::graph::Graph;
use ggen_core::merge::{MergeStrategy, RegionAwareMerger};
use ggen_core::snapshot::SnapshotManager;
use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

#[derive(Args, Debug)]
pub struct RegenerateArgs {
    /// Template paths to regenerate
    #[arg(required = true)]
    pub templates: Vec<PathBuf>,

    /// Baseline snapshot name
    #[arg(short, long)]
    pub baseline: Option<String>,

    /// Output directory (defaults to current directory)
    #[arg(short, long)]
    pub output_dir: Option<PathBuf>,

    /// Merge strategy for conflicts
    #[arg(short, long, default_value = "generated-wins")]
    pub merge: String,

    /// Dry run - show what would be regenerated
    #[arg(short = 'n', long)]
    pub dry_run: bool,

    /// Show diff of changes
    #[arg(short = 'd', long)]
    pub show_diff: bool,

    /// Filter to specific IRIs
    #[arg(short = 'f', long)]
    pub filter: Vec<String>,

    /// Snapshot directory
    #[arg(short = 'S', long, default_value = ".ggen/snapshots")]
    pub snapshot_dir: PathBuf,
}

pub async fn run(args: &RegenerateArgs) -> Result<()> {
    let merge_strategy = parse_merge_strategy(&args.merge)?;

    // Load baseline snapshot if provided
    let baseline_snapshot = if let Some(baseline_name) = &args.baseline {
        let manager = SnapshotManager::new(args.snapshot_dir.clone())?;
        Some(manager.load(baseline_name)?)
    } else {
        None
    };

    // Load current graph
    let _current_graph = Graph::new()?;
    // Would need to load current graph state - simplified for now

    // For each template, determine if it needs regeneration
    for template_path in &args.templates {
        if !template_path.exists() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Template not found: {}",
                template_path.display()
            )));
        }

        // Analyze if this template is affected by changes
        let needs_regeneration = if let Some(ref snapshot) = baseline_snapshot {
            // Compare with snapshot
            let template_snapshot = snapshot.find_template(template_path);
            let template_content = std::fs::read_to_string(template_path)?;
            let current_hash = calculate_template_hash(&template_content);

            if let Some(ts) = template_snapshot {
                ts.hash != current_hash
            } else {
                true // New template
            }
        } else {
            true // No baseline, regenerate everything
        };

        if needs_regeneration {
            if args.dry_run {
                println!("Would regenerate: {}", template_path.display());
                if args.show_diff {
                    println!("  (diff would be shown here)");
                }
            } else {
                regenerate_template(template_path, &merge_strategy, args).await?;
            }
        } else {
            println!("No changes needed for: {}", template_path.display());
        }
    }

    Ok(())
}

async fn regenerate_template(
    template_path: &Path, strategy: &MergeStrategy, args: &RegenerateArgs,
) -> Result<()> {
    // Read template content
    let template_content = std::fs::read_to_string(template_path)?;

    // Generate new content (simplified - would use actual generation pipeline)
    let generated_content = generate_from_template(&template_content)?;

    // Determine output path
    let output_path = args
        .output_dir
        .as_ref()
        .map(|dir| {
            let file_name = template_path
                .file_name()
                .ok_or_else(|| ggen_utils::error::Error::new("Template path has no file name"))?;
            Ok::<PathBuf, ggen_utils::error::Error>(dir.join(file_name))
        })
        .transpose()?
        .unwrap_or_else(|| {
            let file_stem = template_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("output");
            PathBuf::from(file_stem).with_extension("rs")
        });

    if output_path.exists() {
        // File exists - need to merge
        let existing_content = std::fs::read_to_string(&output_path)?;

        let merger = RegionAwareMerger::new(strategy.clone());
        // Create a dummy snapshot for now
        let dummy_snapshot = ggen_core::snapshot::FileSnapshot {
            path: output_path.clone(),
            hash: String::new(),
            size: 0,
            modified_at: chrono::Utc::now(),
            generated_regions: vec![],
            manual_regions: vec![],
        };
        let merge_result = merger.merge_with_regions(
            &existing_content,
            &generated_content,
            &existing_content,
            &dummy_snapshot,
            &output_path,
        )?;

        if merge_result.has_conflicts {
            println!("⚠️  Conflicts detected in {}:", output_path.display());
            for conflict in &merge_result.conflicts {
                println!("  {}: {}", conflict.conflict_type, conflict.description);
            }

            if matches!(strategy, MergeStrategy::FailOnConflict) {
                return Err(ggen_utils::error::Error::new("Merge conflicts detected"));
            }
        }

        std::fs::write(&output_path, &merge_result.content)?;
        println!("✓ Regenerated {} with merge", output_path.display());
    } else {
        // New file - just write generated content
        std::fs::write(&output_path, &generated_content)?;
        println!("✓ Generated new file: {}", output_path.display());
    }

    Ok(())
}

fn parse_merge_strategy(strategy: &str) -> Result<MergeStrategy> {
    match strategy {
        "generated-wins" => Ok(MergeStrategy::GeneratedWins),
        "manual-wins" => Ok(MergeStrategy::ManualWins),
        "interactive" => Ok(MergeStrategy::Interactive),
        "fail-on-conflict" => Ok(MergeStrategy::FailOnConflict),
        _ => Err(ggen_utils::error::Error::new(&format!(
            "Unknown merge strategy: {}",
            strategy
        ))),
    }
}

fn calculate_template_hash(content: &str) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("sha256:{:x}", hasher.finalize())
}

fn generate_from_template(_content: &str) -> Result<String> {
    // Simplified template generation
    // Real implementation would use the template engine
    Ok("// Generated content\npub struct Example {}\n".to_string())
}
