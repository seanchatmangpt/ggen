//! Template show domain logic

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone)]
pub struct TemplateMetadata {
    pub name: String,
    pub path: String,
    pub description: Option<String>,
    pub variables: Vec<String>,
    pub output_path: Option<String>,
    pub rdf_sources: Vec<String>,
    pub sparql_queries: HashMap<String, String>,
    pub determinism_seed: Option<u64>,
}

/// Show template metadata
pub fn show_template_metadata(template_ref: &str) -> Result<TemplateMetadata> {
    // Determine template path
    let template_path = if template_ref.starts_with("gpack:") {
        return Err(ggen_utils::error::Error::new(
            "gpack templates not yet supported",
        ));
    } else if template_ref.contains('/') {
        template_ref.to_string()
    } else {
        format!("templates/{}", template_ref)
    };

    // Check if template exists
    let path = std::path::Path::new(&template_path);
    if !path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template not found: {}",
            template_path
        )));
    }

    // Read template content
    let content = fs::read_to_string(&template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))?;

    // Parse template metadata
    parse_template_metadata(&content, &template_path)
}

/// Parse template metadata from content
fn parse_template_metadata(content: &str, path: &str) -> Result<TemplateMetadata> {
    // Extract filename from path
    let name = std::path::Path::new(path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string();

    // Look for YAML frontmatter
    if content.starts_with("---\n") {
        if let Some(end_pos) = content.find("\n---\n") {
            let frontmatter = &content[4..end_pos];
            return parse_yaml_frontmatter(frontmatter, &name, path);
        }
    }

    // Fallback: basic metadata extraction
    Ok(TemplateMetadata {
        name,
        path: path.to_string(),
        description: None,
        variables: extract_variables_from_content(content),
        output_path: None,
        rdf_sources: vec![],
        sparql_queries: HashMap::new(),
        determinism_seed: None,
    })
}

/// Parse YAML frontmatter
fn parse_yaml_frontmatter(frontmatter: &str, name: &str, path: &str) -> Result<TemplateMetadata> {
    let mut metadata = TemplateMetadata {
        name: name.to_string(),
        path: path.to_string(),
        description: None,
        variables: vec![],
        output_path: None,
        rdf_sources: vec![],
        sparql_queries: HashMap::new(),
        determinism_seed: None,
    };

    // Simple YAML parsing for our specific format
    for line in frontmatter.lines() {
        let line = line.trim();
        if let Some(stripped) = line.strip_prefix("to:") {
            metadata.output_path = Some(stripped.trim().to_string());
        } else if line.starts_with("vars:") {
            continue;
        } else if line.starts_with("rdf:") {
            continue;
        } else if line.starts_with("sparql:") {
            continue;
        } else if line.starts_with("determinism:") {
            continue;
        } else if let Some(stripped) = line.strip_prefix("- ") {
            let var = stripped.trim();
            if !var.is_empty() {
                metadata.variables.push(var.to_string());
            }
        } else if line.contains(':') && !line.starts_with("  ") {
            if let Some((key, value)) = line.split_once(':') {
                let key = key.trim();
                let value = value.trim();

                if key == "seed" {
                    if let Ok(seed) = value.parse::<u64>() {
                        metadata.determinism_seed = Some(seed);
                    }
                }
            }
        }
    }

    Ok(metadata)
}

/// Extract variables from template content using simple pattern matching
fn extract_variables_from_content(content: &str) -> Vec<String> {
    let mut variables = Vec::new();

    // Look for {{ variable }} patterns
    let re = regex::Regex::new(r"\{\{\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\}\}").unwrap();
    for cap in re.captures_iter(content) {
        if let Some(var) = cap.get(1) {
            let var_name = var.as_str().to_string();
            if !variables.contains(&var_name) {
                variables.push(var_name);
            }
        }
    }

    variables
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_variables() {
        let content = "Hello {{ name }}, your age is {{ age }}. {{ name }} again.";
        let vars = extract_variables_from_content(content);

        assert_eq!(vars.len(), 2);
        assert!(vars.contains(&"name".to_string()));
        assert!(vars.contains(&"age".to_string()));
    }

    #[test]
    fn test_parse_yaml_frontmatter() {
        let frontmatter = r#"to: output.txt
vars:
  - name
  - age
seed: 42"#;

        let metadata = parse_yaml_frontmatter(frontmatter, "test", "test.tmpl").unwrap();

        assert_eq!(metadata.name, "test");
        assert_eq!(metadata.output_path, Some("output.txt".to_string()));
        assert_eq!(metadata.variables.len(), 2);
        assert_eq!(metadata.determinism_seed, Some(42));
    }

    #[test]
    fn test_parse_metadata_with_frontmatter() {
        let content = r#"---
to: {{ name }}.txt
vars:
  - name
---
Hello {{ name }}!"#;

        let metadata = parse_template_metadata(content, "test.tmpl").unwrap();

        assert_eq!(metadata.name, "test.tmpl");
        assert_eq!(metadata.output_path, Some("{{ name }}.txt".to_string()));
    }

    #[test]
    fn test_parse_metadata_without_frontmatter() {
        let content = "Hello {{ name }}!";
        let metadata = parse_template_metadata(content, "test.tmpl").unwrap();

        assert_eq!(metadata.name, "test.tmpl");
        assert_eq!(metadata.variables.len(), 1);
        assert!(metadata.variables.contains(&"name".to_string()));
    }
}

/// CLI Arguments for show command
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ShowInput {
    /// Template name or path
    pub template: String,
}

/// Show template output
#[derive(Debug, Clone, Serialize)]
pub struct ShowOutput {
    pub name: String,
    pub path: String,
    pub description: Option<String>,
    pub variables: Vec<String>,
    pub output_path: Option<String>,
    pub rdf_sources: Vec<String>,
    pub sparql_queries: HashMap<String, String>,
    pub determinism_seed: Option<u64>,
}

impl From<TemplateMetadata> for ShowOutput {
    fn from(metadata: TemplateMetadata) -> Self {
        Self {
            name: metadata.name,
            path: metadata.path,
            description: metadata.description,
            variables: metadata.variables,
            output_path: metadata.output_path,
            rdf_sources: metadata.rdf_sources,
            sparql_queries: metadata.sparql_queries,
            determinism_seed: metadata.determinism_seed,
        }
    }
}

/// Execute show command - returns structured output
pub fn execute_show(input: ShowInput) -> Result<ShowOutput> {
    let metadata = show_template_metadata(&input.template)?;
    Ok(ShowOutput::from(metadata))
}

/// CLI run function - bridges sync CLI to async domain logic
pub fn run(args: &ShowInput) -> Result<()> {
    let output = execute_show(args.clone())?;

    println!("📋 Template: {}", output.name);
    println!("📍 Path: {}", output.path);

    if let Some(desc) = &output.description {
        println!("📝 Description: {}", desc);
    }

    if let Some(output_path) = &output.output_path {
        println!("📂 Output path: {}", output_path);
    }

    if !output.variables.is_empty() {
        println!("\n🔧 Variables ({}):", output.variables.len());
        for var in &output.variables {
            println!("  • {}", var);
        }
    }

    if !output.rdf_sources.is_empty() {
        println!("\n🔗 RDF Sources ({}):", output.rdf_sources.len());
        for source in &output.rdf_sources {
            println!("  • {}", source);
        }
    }

    if !output.sparql_queries.is_empty() {
        println!("\n🔍 SPARQL Queries ({}):", output.sparql_queries.len());
        for (name, query) in &output.sparql_queries {
            println!("  • {}: {}", name, query);
        }
    }

    if let Some(seed) = output.determinism_seed {
        println!("\n🎲 Determinism seed: {}", seed);
    }

    Ok(())
}
