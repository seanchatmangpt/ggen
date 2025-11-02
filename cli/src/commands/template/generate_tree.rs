//! Template generate-tree command - CLI layer

use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

use crate::domain::template::generate_tree::generate_file_tree;

#[derive(Debug, Args)]
pub struct GenerateTreeCommand {
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

impl GenerateTreeCommand {
    pub async fn execute(&self) -> Result<()> {
        println!("📦 Generating file tree from template: {}", self.template);

        let template_path = std::path::Path::new(&self.template);
        if !template_path.exists() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Template not found: {}",
                self.template
            )));
        }

        // Collect variables
        let variables: HashMap<String, String> = self.var.iter().cloned().collect();

        if self.dry_run {
            println!("\n🔍 Dry run - would generate file tree from: {}", self.template);
            println!("Variables: {:?}", variables);
            println!("Output directory: {}", self.output.display());
            return Ok(());
        }

        // Generate file tree
        let result = generate_file_tree(template_path, &self.output, &variables, self.force)?;

        println!(
            "\n✅ Successfully generated {} files and {} directories",
            result.files().len(),
            result.directories().len()
        );
        println!("📂 Output directory: {}", self.output.display());

        Ok(())
    }
}

fn parse_key_val(s: &str) -> Result<(String, String)> {
    let pos = s
        .find('=')
        .ok_or_else(|| ggen_utils::error::Error::new("Invalid KEY=VALUE format: no '=' found"))?;
    Ok((s[..pos].to_string(), s[pos + 1..].to_string()))
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
    fn test_parse_key_val_with_equals() {
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
