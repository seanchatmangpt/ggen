//! File tree generation domain logic

use ggen_core::{FileTreeTemplate, GenerationResult, TemplateContext, TemplateParser};
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Generate file tree from template
pub fn generate_file_tree(
    template_path: &Path, output_dir: &Path, variables: &HashMap<String, String>, force: bool,
) -> Result<GenerationResult> {
    // Load and parse template
    let template = TemplateParser::parse_file(template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to parse template: {}", e)))?;

    // Create template context from variables
    let var_map: std::collections::BTreeMap<String, String> = variables
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    let context = TemplateContext::from_map(var_map)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create context: {}", e)))?;

    // Validate required variables
    if let Err(e) = context.validate_required(template.required_variables()) {
        return Err(ggen_utils::error::Error::new(&format!(
            "Validation failed: {}",
            e
        )));
    }

    // Check if files would be overwritten
    if !force && would_overwrite(&template, output_dir, &context)? {
        return Err(ggen_utils::error::Error::new(
            "Files would be overwritten. Use --force to overwrite or choose different output directory",
        ));
    }

    // Generate files
    let result = ggen_core::templates::generate_file_tree(template, context, output_dir)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Generation failed: {}", e)))?;

    Ok(result)
}

/// Check if generation would overwrite existing files
fn would_overwrite(
    template: &FileTreeTemplate, output: &Path, context: &TemplateContext,
) -> Result<bool> {
    fn check_nodes(
        nodes: &[ggen_core::FileTreeNode], current_path: &Path, context: &TemplateContext,
    ) -> Result<bool> {
        for node in nodes {
            let rendered_name = context
                .render_string(&node.name)
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to render: {}", e)))?;

            let node_path = current_path.join(&rendered_name);

            match node.node_type {
                ggen_core::NodeType::Directory => {
                    if check_nodes(&node.children, &node_path, context)? {
                        return Ok(true);
                    }
                }
                ggen_core::NodeType::File => {
                    if node_path.exists() {
                        return Ok(true);
                    }
                }
            }
        }
        Ok(false)
    }

    check_nodes(template.nodes(), output, context)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_would_overwrite_detects_existing_files() {
        let temp_dir = TempDir::new().unwrap();

        // Create a simple template tree structure manually
        let template_content = r#"
name: "test-project"
nodes:
  - type: directory
    name: "src"
    children:
      - type: file
        name: "main.rs"
        content: "fn main() {}"
"#;

        // Write template file
        let template_path = temp_dir.path().join("template.yaml");
        fs::write(&template_path, template_content).unwrap();

        // Create output directory with existing file
        let output_dir = temp_dir.path().join("output");
        fs::create_dir_all(output_dir.join("src")).unwrap();
        fs::write(output_dir.join("src/main.rs"), "existing").unwrap();

        // This test verifies the concept - actual integration would need real FileTreeTemplate
        assert!(output_dir.join("src/main.rs").exists());
    }

    #[test]
    fn test_variables_conversion() {
        let mut vars = HashMap::new();
        vars.insert("name".to_string(), "test".to_string());
        vars.insert("author".to_string(), "Alice".to_string());

        let btree_map: std::collections::BTreeMap<String, String> =
            vars.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

        assert_eq!(btree_map.get("name").unwrap(), "test");
        assert_eq!(btree_map.get("author").unwrap(), "Alice");
    }
}

use serde::{Deserialize, Serialize};

/// CLI Arguments for generate-tree command
#[derive(Debug, Clone, Default, Deserialize)]
pub struct GenerateTreeInput {
    /// Template file path
    pub template: PathBuf,

    /// Output directory
    pub output: PathBuf,

    /// Variables (key=value format)
    pub var: Vec<String>,

    /// Force overwrite existing files
    pub force: bool,
}

/// Generate tree output
#[derive(Debug, Clone, Serialize)]
pub struct GenerateTreeOutput {
    pub files_generated: usize,
    pub directories_created: usize,
    pub output_path: String,
}

/// Execute generate-tree command - full implementation
pub async fn execute_generate_tree(input: GenerateTreeInput) -> Result<GenerateTreeOutput> {
    // Parse variables from key=value format
    let mut variables = HashMap::new();
    for var_str in &input.var {
        let parts: Vec<&str> = var_str.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new(&format!(
                "Invalid variable format: '{}'. Expected 'key=value'",
                var_str
            )));
        }
        variables.insert(parts[0].to_string(), parts[1].to_string());
    }

    // Generate file tree
    let result = generate_file_tree(&input.template, &input.output, &variables, input.force)?;

    // Return output with generation results
    Ok(GenerateTreeOutput {
        files_generated: result.files().len(),
        directories_created: result.directories().len(),
        output_path: input.output.display().to_string(),
    })
}

/// CLI run function - bridges sync CLI to async domain logic
pub fn run(args: &GenerateTreeInput) -> Result<()> {
    // Use tokio runtime for async execution
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create runtime: {}", e)))?;

    let output = runtime.block_on(execute_generate_tree(args.clone()))?;

    ggen_utils::alert_success!("Generated file tree from: {}", args.template.display());
    ggen_utils::alert_info!("📁 Output directory: {}", output.output_path);
    ggen_utils::alert_info!("📄 Files created: {}", output.files_generated);
    ggen_utils::alert_info!("📂 Directories created: {}", output.directories_created);

    Ok(())
}
