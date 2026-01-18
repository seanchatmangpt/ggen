//! Template generation lifecycle phase
//!
//! This module adds template generation as a lifecycle phase,
//! allowing templates to be part of standard project workflows.

use crate::lifecycle::{Context, LifecycleError, Result};
use std::collections::HashMap;
use std::path::Path;

/// Template generation phase configuration
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TemplatePhaseConfig {
    /// Template file to generate from
    pub template: String,

    /// Output directory
    pub output_dir: Option<String>,

    /// Variables to pass to template
    pub variables: HashMap<String, String>,

    /// Run in interactive mode
    pub interactive: bool,

    /// Force overwrite existing files
    pub force: bool,

    /// Run post-generation hooks
    pub post_hooks: Vec<String>,
}

impl Default for TemplatePhaseConfig {
    fn default() -> Self {
        Self {
            template: String::new(),
            output_dir: None,
            variables: HashMap::new(),
            interactive: false,
            force: false,
            post_hooks: Vec::new(),
        }
    }
}

/// Execute template generation phase
pub async fn execute_template_phase(
    config: &TemplatePhaseConfig,
    context: &Context,
) -> Result<()> {
    use ggen_template::template_tree::TemplateTree;
    use ggen_template::rdf_metadata::MetadataStore;

    log::info!("🔧 Template Generation Phase");
    log::info!("   Template: {}", config.template);

    // Resolve template path
    let template_path = if Path::new(&config.template).is_absolute() {
        config.template.clone()
    } else {
        context
            .project_root
            .join(&config.template)
            .to_string_lossy()
            .to_string()
    };

    // Load template
    let tree = TemplateTree::from_file(Path::new(&template_path)).map_err(|e| {
        LifecycleError::ExecutionError(format!("Failed to load template: {}", e))
    })?;

    // Resolve output directory
    let output_dir = if let Some(dir) = &config.output_dir {
        context.project_root.join(dir)
    } else {
        context.project_root.clone()
    };

    // Validate variables
    validate_template_variables(&tree, &config.variables)?;

    // Check for conflicts unless force is set
    if !config.force {
        check_file_conflicts(&tree, &output_dir, &config.variables)?;
    }

    // Generate files
    log::info!("   Generating files to: {}", output_dir.display());
    let metadata_store = MetadataStore::new();
    let count = tree
        .generate(&output_dir, &config.variables, &metadata_store)
        .map_err(|e| LifecycleError::ExecutionError(format!("Generation failed: {}", e)))?;

    log::info!("   ✅ Generated {} files", count);

    // Run post-generation hooks
    if !config.post_hooks.is_empty() {
        log::info!("   Running post-generation hooks...");
        for hook in &config.post_hooks {
            run_post_hook(hook, &output_dir).await?;
        }
    }

    Ok(())
}

/// Validate template variables are provided
fn validate_template_variables(
    tree: &ggen_template::template_tree::TemplateTree,
    variables: &HashMap<String, String>,
) -> Result<()> {
    let required_vars = tree.required_variables();
    let mut missing = Vec::new();

    for var in required_vars {
        if !var.optional && !variables.contains_key(&var.name) && var.default.is_none() {
            missing.push(var.name.clone());
        }
    }

    if !missing.is_empty() {
        return Err(LifecycleError::ConfigError(format!(
            "Missing required template variables: {}",
            missing.join(", ")
        )));
    }

    Ok(())
}

/// Check for file conflicts
fn check_file_conflicts(
    tree: &ggen_template::template_tree::TemplateTree,
    output_dir: &Path,
    variables: &HashMap<String, String>,
) -> Result<()> {
    let files = tree.preview_files(output_dir, variables).map_err(|e| {
        LifecycleError::ExecutionError(format!("Failed to preview files: {}", e))
    })?;

    let mut conflicts = Vec::new();
    for file in files {
        if file.exists() {
            conflicts.push(file.to_string_lossy().to_string());
        }
    }

    if !conflicts.is_empty() {
        return Err(LifecycleError::ExecutionError(format!(
            "Files already exist (use force=true to overwrite): {}",
            conflicts.join(", ")
        )));
    }

    Ok(())
}

/// Run post-generation hook
async fn run_post_hook(command: &str, working_dir: &Path) -> Result<()> {
    use tokio::process::Command;

    log::info!("      Hook: {}", command);

    let output = Command::new("sh")
        .arg("-c")
        .arg(command)
        .current_dir(working_dir)
        .output()
        .await
        .map_err(|e| LifecycleError::ExecutionError(format!("Hook execution failed: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LifecycleError::ExecutionError(format!(
            "Hook failed: {}",
            stderr
        )));
    }

    log::info!("      ✅ Hook completed");
    Ok(())
}

/// Add template phase to lifecycle configuration
pub fn register_template_phase(make: &mut crate::lifecycle::Make) {
    use crate::lifecycle::Phase;

    // Add template-generate phase if not present
    if !make.phases.iter().any(|p| p.name == "template-generate") {
        make.phases.push(Phase {
            name: "template-generate".to_string(),
            description: Some("Generate files from templates".to_string()),
            depends_on: vec![],
            commands: vec![],
            parallel: false,
            continue_on_error: false,
            env: HashMap::new(),
            workdir: None,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = TemplatePhaseConfig::default();
        assert!(config.template.is_empty());
        assert!(!config.interactive);
        assert!(!config.force);
        assert!(config.post_hooks.is_empty());
    }

    #[test]
    fn test_config_with_variables() {
        let mut config = TemplatePhaseConfig::default();
        config.variables.insert("name".to_string(), "test".to_string());
        config
            .variables
            .insert("version".to_string(), "1.0.0".to_string());

        assert_eq!(config.variables.len(), 2);
        assert_eq!(config.variables.get("name").unwrap(), "test");
    }
}
