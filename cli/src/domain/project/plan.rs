//! Generation plan domain logic
//!
//! Chicago TDD: Pure business logic with REAL plan creation

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Component, Path};

/// Plan creation result
#[derive(Debug, Clone)]
pub struct PlanResult {
    pub output_path: String,
    pub variables_count: usize,
}

/// Generation plan structure
#[derive(Serialize, Deserialize, Debug)]
pub struct GenerationPlan {
    pub template_ref: String,
    pub variables: HashMap<String, String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub format: String,
}

/// Validate path to prevent directory traversal
fn validate_path(path: &Path) -> Result<()> {
    if path.components().any(|c| matches!(c, Component::ParentDir)) {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }
    Ok(())
}

/// Parse variables from key=value format
fn parse_variables(vars: &[String]) -> Result<HashMap<String, String>> {
    let mut variables = HashMap::new();

    for var in vars {
        if let Some((key, value)) = var.split_once('=') {
            variables.insert(key.to_string(), value.to_string());
        } else {
            return Err(ggen_utils::error::Error::new_fmt(format_args!(
                "Invalid variable format: {}. Expected key=value",
                var
            )));
        }
    }

    Ok(variables)
}

/// Create generation plan (Chicago TDD: REAL implementation)
pub fn create_plan(args: &crate::cmds::project::PlanArgs) -> Result<PlanResult> {
    // Validate template reference
    if args.template_ref.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Parse variables
    let variables = parse_variables(&args.vars)?;

    // Create plan structure
    let plan = GenerationPlan {
        template_ref: args.template_ref.clone(),
        variables: variables.clone(),
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
        "json" => serde_json::to_string_pretty(&plan)
            .map_err(ggen_utils::error::Error::from)?,
        "yaml" => serde_yaml::to_string(&plan)
            .map_err(ggen_utils::error::Error::from)?,
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

    // Validate and write plan file
    validate_path(&output_path)?;
    fs::write(&output_path, content)
        .map_err(ggen_utils::error::Error::from)?;

    Ok(PlanResult {
        output_path: output_path.display().to_string(),
        variables_count: variables.len(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_parse_variables_valid() {
        let vars = vec!["key=value".to_string(), "name=test".to_string()];
        let result = parse_variables(&vars).unwrap();

        assert_eq!(result.get("key"), Some(&"value".to_string()));
        assert_eq!(result.get("name"), Some(&"test".to_string()));
    }

    #[test]
    fn test_parse_variables_invalid() {
        let vars = vec!["invalid".to_string()];
        let result = parse_variables(&vars);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Expected key=value"));
    }

    #[test]
    fn test_validate_path_safe() {
        let path = Path::new("safe/path/file.json");
        assert!(validate_path(path).is_ok());
    }

    #[test]
    fn test_validate_path_traversal() {
        let path = Path::new("../../../etc/passwd");
        let result = validate_path(path);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Path traversal"));
    }

    #[test]
    fn test_create_plan_json() -> Result<()> {
        let temp_dir = tempdir().unwrap();
        let output_path = temp_dir.path().join("plan.json");

        let args = crate::commands::project::plan::PlanArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec!["key=value".to_string()],
            output: Some(output_path.to_string_lossy().to_string()),
            format: "json".to_string(),
        };

        let result = create_plan(&args)?;

        assert!(Path::new(&result.output_path).exists());
        assert_eq!(result.variables_count, 1);

        Ok(())
    }
}
