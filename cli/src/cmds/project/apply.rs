use clap::Args;
use ggen_utils::error::Result;
use std::fs;
use std::path::{Component, Path};

#[derive(Args, Debug)]
pub struct ApplyArgs {
    /// Path to the plan file to apply
    pub plan_file: String,

    /// Apply changes without prompting for confirmation
    #[arg(short = 'y', long = "yes")]
    pub auto_confirm: bool,

    /// Perform dry-run to show what would be applied
    #[arg(long)]
    pub dry_run: bool,
}

/// Validate path to prevent directory traversal attacks
fn validate_path(path: &Path) -> Result<()> {
    if path.components().any(|c| matches!(c, Component::ParentDir)) {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }
    Ok(())
}

/// Main entry point for `ggen project apply`
pub async fn run(args: &ApplyArgs) -> Result<()> {
    println!("Applying generation plan");

    // Validate plan file exists and path is safe
    let plan_path = Path::new(&args.plan_file);
    validate_path(plan_path)?;

    if !plan_path.exists() {
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Plan file not found: {}",
            args.plan_file
        )));
    }

    // Read and parse plan file
    let content = fs::read_to_string(plan_path).map_err(ggen_utils::error::Error::from)?;

    let plan: GenerationPlan = match plan_path.extension().and_then(|ext| ext.to_str()) {
        Some("json") => serde_json::from_str(&content).map_err(ggen_utils::error::Error::from)?,
        Some("yaml") | Some("yml") => {
            serde_yaml::from_str(&content).map_err(ggen_utils::error::Error::from)?
        }
        Some("toml") => toml::from_str(&content).map_err(ggen_utils::error::Error::from)?,
        _ => {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Unsupported plan file format. Supported: .json, .yaml, .yml, .toml"
            )))
        }
    };

    // Show plan summary
    println!("📋 Plan Summary:");
    println!("  Template: {}", plan.template_ref);
    println!("  Variables: {}", plan.variables.len());
    println!(
        "  Created: {}",
        plan.timestamp.format("%Y-%m-%d %H:%M:%S UTC")
    );

    if args.dry_run {
        println!("🔍 Dry run mode - no changes will be applied");
        println!("Would apply plan with {} variables", plan.variables.len());
        return Ok(());
    }

    // Confirm before applying
    if !args.auto_confirm {
        println!("\n⚠️  This will apply the generation plan to your project.");
        println!("Continue? [y/N]: ");

        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .map_err(ggen_utils::error::Error::from)?;

        if !input.trim().to_lowercase().starts_with('y') {
            println!("Plan application cancelled by user");
            return Ok(());
        }
    }

    // Apply the plan using cargo make
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(["make", "project-gen"]);
    cmd.arg("--template").arg(&plan.template_ref);

    for (key, value) in &plan.variables {
        cmd.arg("--var").arg(format!("{}={}", key, value));
    }

    let output = cmd.output().map_err(ggen_utils::error::Error::from)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ggen_utils::error::Error::new_fmt(format_args!(
            "Plan application failed: {}",
            stderr
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("✅ Generation plan applied successfully");
    println!("{}", stdout);

    Ok(())
}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct GenerationPlan {
    template_ref: String,
    variables: std::collections::HashMap<String, String>,
    timestamp: chrono::DateTime<chrono::Utc>,
    format: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_apply_args_parsing() {
        let args = ApplyArgs {
            plan_file: "changes.plan".to_string(),
            auto_confirm: true,
            dry_run: false,
        };

        assert_eq!(args.plan_file, "changes.plan");
        assert!(args.auto_confirm);
    }
}
