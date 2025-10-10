use clap::Args;
use ggen_utils::error::Result;
use std::fs;
use std::path::{Component, Path};

#[derive(Args, Debug)]
pub struct PlanArgs {
    /// Template reference (e.g., "gpack-id:path/to.tmpl" or "local/path.tmpl")
    pub template_ref: String,

    /// Variables to pass to the template (key=value format)
    #[arg(short = 'v', long = "var")]
    pub vars: Vec<String>,

    /// Output file for the generated plan
    #[arg(short = 'o', long = "output")]
    pub output: Option<String>,

    /// Output format (json, yaml, toml)
    #[arg(long, default_value = "json")]
    pub format: String,
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

/// Main entry point for `ggen project plan`
pub async fn run(args: &PlanArgs) -> Result<()> {
    println!("Creating generation plan");

    // Validate template reference
    if args.template_ref.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Parse variables
    let mut variables = std::collections::HashMap::new();
    for var in &args.vars {
        if let Some((key, value)) = var.split_once('=') {
            variables.insert(key.to_string(), value.to_string());
        } else {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: {}. Expected key=value",
                var
            )));
        }
    }

    // Create plan structure
    let plan = GenerationPlan {
        template_ref: args.template_ref.clone(),
        variables,
        timestamp: chrono::Utc::now(),
        format: args.format.clone(),
    };

    // Determine output path
    let output_path = match &args.output {
        Some(path) => Path::new(path).to_path_buf(),
        None => Path::new("ggen-plan").with_extension(&args.format),
    };

    // Serialize plan
    let content = match args.format.as_str() {
        "json" => serde_json::to_string_pretty(&plan).map_err(ggen_utils::error::Error::from)?,
        "yaml" => serde_yaml::to_string(&plan).map_err(ggen_utils::error::Error::from)?,
        "toml" => toml::to_string_pretty(&plan).map_err(|e| {
            ggen_utils::error::Error::new_fmt(format_args!("TOML serialization failed: {}", e))
        })?,
        _ => {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Unsupported format: {}. Supported: json, yaml, toml",
                args.format
            )))
        }
    };

    // Validate output path and write plan file
    validate_path(&output_path)?;
    fs::write(&output_path, content).map_err(ggen_utils::error::Error::from)?;

    println!("✅ Generation plan created: {}", output_path.display());
    println!("Plan saved to: {}", output_path.display());

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
    async fn test_plan_args_parsing() {
        let args = PlanArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec!["key=value".to_string()],
            output: Some("plan.json".to_string()),
            format: "json".to_string(),
        };

        assert_eq!(args.template_ref, "test.tmpl");
        assert_eq!(args.format, "json");
    }
}
