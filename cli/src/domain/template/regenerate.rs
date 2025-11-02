//! Template regeneration domain logic

use clap::Args;
use ggen_core::{MergeStrategy, RegionAwareMerger};
use ggen_utils::error::Result;
use std::fs;
use std::path::{Path, PathBuf};

/// Parse merge strategy from string
pub fn parse_merge_strategy(strategy: &str) -> Result<MergeStrategy> {
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

/// Regenerate template with merge support
pub fn regenerate_with_merge(
    _template_path: &Path,
    output_path: &Path,
    generated_content: &str,
    strategy: &MergeStrategy,
) -> Result<()> {
    if output_path.exists() {
        // File exists - need to merge
        let existing_content = fs::read_to_string(output_path).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read existing file: {}", e))
        })?;

        let merger = RegionAwareMerger::new(strategy.clone());

        // Create a dummy snapshot for merge
        let dummy_snapshot = ggen_core::snapshot::FileSnapshot {
            path: output_path.to_path_buf(),
            hash: String::new(),
            size: 0,
            modified_at: chrono::Utc::now(),
            generated_regions: vec![],
            manual_regions: vec![],
        };

        let merge_result = merger
            .merge_with_regions(
                &existing_content,
                generated_content,
                &existing_content,
                &dummy_snapshot,
                output_path,
            )
            .map_err(|e| ggen_utils::error::Error::new(&format!("Merge failed: {}", e)))?;

        if merge_result.has_conflicts {
            if matches!(strategy, MergeStrategy::FailOnConflict) {
                return Err(ggen_utils::error::Error::new("Merge conflicts detected"));
            }
        }

        fs::write(output_path, &merge_result.content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write merged content: {}", e))
        })?;
    } else {
        // New file - just write generated content
        fs::write(output_path, generated_content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write file: {}", e))
        })?;
    }

    Ok(())
}

/// Calculate SHA256 hash of content
pub fn calculate_hash(content: &str) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("sha256:{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_parse_merge_strategies() {
        assert!(matches!(
            parse_merge_strategy("generated-wins").unwrap(),
            MergeStrategy::GeneratedWins
        ));
        assert!(matches!(
            parse_merge_strategy("manual-wins").unwrap(),
            MergeStrategy::ManualWins
        ));
        assert!(matches!(
            parse_merge_strategy("interactive").unwrap(),
            MergeStrategy::Interactive
        ));
        assert!(matches!(
            parse_merge_strategy("fail-on-conflict").unwrap(),
            MergeStrategy::FailOnConflict
        ));
        assert!(parse_merge_strategy("invalid").is_err());
    }

    #[test]
    fn test_regenerate_new_file() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.rs");

        let content = "fn main() {}";
        let strategy = MergeStrategy::GeneratedWins;

        regenerate_with_merge(&template_path, &output_path, content, &strategy).unwrap();

        assert!(output_path.exists());
        assert_eq!(fs::read_to_string(&output_path).unwrap(), content);
    }

    #[test]
    fn test_calculate_hash() {
        let hash1 = calculate_hash("content1");
        let hash2 = calculate_hash("content2");
        let hash3 = calculate_hash("content1");

        assert_ne!(hash1, hash2);
        assert_eq!(hash1, hash3);
        assert!(hash1.starts_with("sha256:"));
    }
}

/// CLI Arguments for regenerate command
#[derive(Debug, Clone, Args)]
pub struct RegenerateArgs {
    /// Template file path
    #[arg(short = 't', long)]
    pub template: PathBuf,

    /// Output file path
    #[arg(short = 'o', long)]
    pub output: PathBuf,

    /// Merge strategy (generated-wins, manual-wins, interactive, fail-on-conflict)
    #[arg(short = 's', long, default_value = "manual-wins")]
    pub strategy: String,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub var: Vec<String>,
}

/// CLI run function - bridges sync CLI to async domain logic
pub fn run(args: &RegenerateArgs) -> ggen_utils::error::Result<()> {
    crate::runtime::execute(async move {
        // Parse merge strategy
        let strategy = parse_merge_strategy(&args.strategy)?;

        // TODO: Generate content from template with variables
        let generated_content = "// Generated content placeholder\n";

        // Regenerate with merge
        regenerate_with_merge(&args.template, &args.output, generated_content, &strategy)?;

        println!("✅ Regenerated template with {} strategy", args.strategy);
        println!("📄 Output: {}", args.output.display());

        Ok(())
    })
}
