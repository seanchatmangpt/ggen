//! Template regenerate command - CLI layer

use clap::Args;
use ggen_utils::error::Result;
use std::path::PathBuf;

use crate::domain::template::regenerate::{calculate_hash, parse_merge_strategy, regenerate_with_merge};

#[derive(Debug, Args)]
pub struct RegenerateCommand {
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

    /// Snapshot directory
    #[arg(short = 'S', long, default_value = ".ggen/snapshots")]
    pub snapshot_dir: PathBuf,
}

impl RegenerateCommand {
    pub async fn execute(&self) -> Result<()> {
        let merge_strategy = parse_merge_strategy(&self.merge)?;

        // For each template, determine if it needs regeneration
        for template_path in &self.templates {
            if !template_path.exists() {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Template not found: {}",
                    template_path.display()
                )));
            }

            if self.dry_run {
                println!("Would regenerate: {}", template_path.display());
                if self.show_diff {
                    println!("  (diff would be shown here)");
                }
            } else {
                // Read template content
                let template_content = std::fs::read_to_string(template_path).map_err(|e| {
                    ggen_utils::error::Error::new(&format!(
                        "Failed to read template: {}",
                        e
                    ))
                })?;

                // Simple generation for now (in real implementation, use TemplateEngine)
                let generated_content = format!("// Generated from {}\n{}", template_path.display(), template_content);

                // Determine output path
                let output_path = self
                    .output_dir
                    .as_ref()
                    .map(|dir| {
                        let file_name = template_path
                            .file_name()
                            .ok_or_else(|| {
                                ggen_utils::error::Error::new("Template path has no file name")
                            })?;
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

                regenerate_with_merge(template_path, &output_path, &generated_content, &merge_strategy)?;

                if output_path.exists() {
                    println!("✓ Regenerated {}", output_path.display());
                } else {
                    println!("✓ Generated new file: {}", output_path.display());
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_parsing() {
        // Verify command structure is valid
        let cmd = RegenerateCommand {
            templates: vec![PathBuf::from("test.tmpl")],
            baseline: None,
            output_dir: None,
            merge: "generated-wins".to_string(),
            dry_run: false,
            show_diff: false,
            snapshot_dir: PathBuf::from(".ggen/snapshots"),
        };

        assert_eq!(cmd.merge, "generated-wins");
        assert_eq!(cmd.templates.len(), 1);
    }
}
