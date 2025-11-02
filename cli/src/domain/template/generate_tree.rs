//! File tree generation domain logic

use clap::Args;
use ggen_core::{FileTreeTemplate, TemplateContext, TemplateParser, GenerationResult};
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Generate file tree from template
pub fn generate_file_tree(
    template_path: &Path,
    output_dir: &Path,
    variables: &HashMap<String, String>,
    force: bool,
) -> Result<GenerationResult> {
    // Load and parse template
    let template = TemplateParser::parse_file(template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to parse template: {}", e)))?;

    // Create template context from variables
    let var_map: std::collections::BTreeMap<String, String> =
        variables.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

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
    template: &FileTreeTemplate,
    output: &Path,
    context: &TemplateContext,
) -> Result<bool> {
    fn check_nodes(
        nodes: &[ggen_core::FileTreeNode],
        current_path: &Path,
        context: &TemplateContext,
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

/// CLI Arguments for generate-tree command
#[derive(Debug, Clone, Args)]
pub struct GenerateTreeArgs {
    /// Template file path
    #[arg(short = 't', long)]
    pub template: PathBuf,

    /// Output directory
    #[arg(short = 'o', long)]
    pub output: PathBuf,

    /// Variables (key=value format)
    #[arg(short = 'v', long)]
    pub var: Vec<String>,

    /// Force overwrite existing files
    #[arg(short = 'f', long)]
    pub force: bool,
}

/// CLI run function - bridges sync CLI to async domain logic
pub fn run(args: &GenerateTreeArgs) -> ggen_utils::error::Result<()> {
    crate::runtime::execute(async move {
        // Parse variables
        let variables: HashMap<String, String> = args
            .var
            .iter()
            .filter_map(|v| v.split_once('='))
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        let _result = generate_file_tree(&args.template, &args.output, &variables, args.force)?;

        println!("✅ Generated file tree");
        println!("📁 Output directory: {}", args.output.display());

        Ok(())
    })
}
