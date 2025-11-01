use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Args, Debug)]
pub struct GenerateTreeArgs {
    /// Template file path (YAML template tree specification)
    #[arg(short, long)]
    pub template: String,

    /// Output directory for generated files
    #[arg(short, long, default_value = ".")]
    pub output: PathBuf,

    /// Template variables (key=value format)
    #[arg(short, long, value_parser = parse_key_val)]
    pub var: Vec<(String, String)>,

    /// Interactive mode - prompt for missing variables
    #[arg(short, long)]
    pub interactive: bool,

    /// Dry run - show what would be generated without creating files
    #[arg(long)]
    pub dry_run: bool,

    /// Force overwrite existing files
    #[arg(short, long)]
    pub force: bool,
}

fn parse_key_val(s: &str) -> Result<(String, String)> {
    let pos = s.find('=').ok_or_else(|| {
        ggen_utils::error::Error::new("Invalid KEY=VALUE format: no '=' found")
    })?;
    Ok((s[..pos].to_string(), s[pos + 1..].to_string()))
}

pub async fn run(args: &GenerateTreeArgs) -> Result<()> {
    use ggen_core::{FileTreeTemplate, TemplateContext, TemplateParser};
    use std::collections::BTreeMap;

    println!("📦 Generating file tree from template: {}", args.template);

    // Load template tree
    let template_path = Path::new(&args.template);
    if !template_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template not found: {}",
            args.template
        )));
    }

    let template = TemplateParser::parse_file(template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load template: {}", e)))?;

    // Collect variables
    let mut variables: HashMap<String, String> = args.var.iter().cloned().collect();

    // Interactive variable collection
    if args.interactive {
        collect_interactive_variables(&template, &mut variables)?;
    }

    // Create template context
    let var_map: BTreeMap<String, String> = variables.iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    let context = TemplateContext::from_map(var_map)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create context: {}", e)))?;

    // Validate required variables
    if let Err(e) = context.validate_required(template.required_variables()) {
        return Err(ggen_utils::error::Error::new(&format!("Validation failed: {}", e)));
    }

    // Preview generation
    if args.dry_run {
        println!("\n🔍 Dry run - files that would be generated:");
        preview_generation(&template, &args.output, &context)?;
        return Ok(());
    }

    // Check for existing files
    if !args.force && would_overwrite(&template, &args.output, &context)? {
        return Err(ggen_utils::error::Error::new(
            "Files would be overwritten. Use --force to overwrite or choose different output directory",
        ));
    }

    // Generate files
    println!("\n📝 Generating files...");
    let result = ggen_core::templates::generate_file_tree(template, context, &args.output)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Generation failed: {}", e)))?;

    println!("\n✅ Successfully generated {} files and {} directories",
        result.files().len(), result.directories().len());
    println!("📂 Output directory: {}", args.output.display());

    Ok(())
}

fn collect_interactive_variables(
    template: &ggen_core::FileTreeTemplate,
    variables: &mut HashMap<String, String>,
) -> Result<()> {
    use std::io::{self, Write};

    println!("\n🔧 Interactive variable collection");
    println!("Press Enter to use default values (if available)\n");

    for var_name in template.required_variables() {
        if variables.contains_key(var_name) {
            continue; // Already provided via --var
        }

        print!("  {}", var_name);

        if let Some(default) = template.defaults().get(var_name) {
            print!(" [{}]", default);
        }
        print!(": ");
        io::stdout()
            .flush()
            .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read input: {}", e)))?;

        let value = input.trim();
        if value.is_empty() {
            if let Some(default) = template.defaults().get(var_name) {
                variables.insert(var_name.clone(), default.clone());
            }
        } else {
            variables.insert(var_name.clone(), value.to_string());
        }
    }

    Ok(())
}


fn preview_generation(
    template: &ggen_core::FileTreeTemplate,
    output: &Path,
    context: &ggen_core::TemplateContext,
) -> Result<()> {
    // Preview what would be generated
    fn preview_nodes(
        nodes: &[ggen_core::FileTreeNode],
        current_path: &Path,
        context: &ggen_core::TemplateContext,
        files: &mut Vec<PathBuf>,
    ) -> Result<()> {
        for node in nodes {
            let rendered_name = context.render_string(&node.name)
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to render: {}", e)))?;

            let node_path = current_path.join(&rendered_name);

            match node.node_type {
                ggen_core::NodeType::Directory => {
                    preview_nodes(&node.children, &node_path, context, files)?;
                }
                ggen_core::NodeType::File => {
                    files.push(node_path);
                }
            }
        }
        Ok(())
    }

    let mut files = Vec::new();
    preview_nodes(template.nodes(), output, context, &mut files)?;

    for (i, file_path) in files.iter().enumerate() {
        println!("  {}. {}", i + 1, file_path.display());
    }

    println!("\nTotal: {} files", files.len());
    Ok(())
}

fn would_overwrite(
    template: &ggen_core::FileTreeTemplate,
    output: &Path,
    context: &ggen_core::TemplateContext,
) -> Result<bool> {
    fn check_nodes(
        nodes: &[ggen_core::FileTreeNode],
        current_path: &Path,
        context: &ggen_core::TemplateContext,
    ) -> Result<bool> {
        for node in nodes {
            let rendered_name = context.render_string(&node.name)
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

    #[test]
    fn test_parse_key_val_valid() {
        assert_eq!(
            parse_key_val("name=value").unwrap(),
            ("name".to_string(), "value".to_string())
        );
        assert_eq!(
            parse_key_val("port=8080").unwrap(),
            ("port".to_string(), "8080".to_string())
        );
    }

    #[test]
    fn test_parse_key_val_with_equals_in_value() {
        assert_eq!(
            parse_key_val("url=http://example.com?foo=bar").unwrap(),
            ("url".to_string(), "http://example.com?foo=bar".to_string())
        );
    }

    #[test]
    fn test_parse_key_val_invalid() {
        assert!(parse_key_val("invalid").is_err());
        assert!(parse_key_val("").is_err());
    }
}
