use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;

#[derive(Args, Debug)]
pub struct ShowArgs {
    /// Template reference (local path or gpack:template)
    pub template_ref: String,
}

#[cfg_attr(test, mockall::automock)]
pub trait TemplateMetadataFetcher {
    fn fetch_metadata(&self, template_ref: &str) -> Result<TemplateMetadata>;
}

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

/// Validate and sanitize template reference input
fn validate_template_ref(template_ref: &str) -> Result<()> {
    // Validate template reference is not empty
    if template_ref.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Validate template reference length
    if template_ref.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }

    // Basic path traversal protection
    if template_ref.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: template reference cannot contain '..'",
        ));
    }

    // Validate template reference format (basic pattern check)
    if !template_ref
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == ':' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template reference format: only alphanumeric characters, dots, slashes, colons, dashes, and underscores allowed",
        ));
    }

    Ok(())
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
        if line.starts_with("to:") {
            metadata.output_path = Some(line[3..].trim().to_string());
        } else if line.starts_with("vars:") {
            // Variables are on subsequent lines
            continue;
        } else if line.starts_with("rdf:") {
            // RDF sources are on subsequent lines
            continue;
        } else if line.starts_with("sparql:") {
            // SPARQL queries are on subsequent lines
            continue;
        } else if line.starts_with("determinism:") {
            // Determinism config is on subsequent lines
            continue;
        } else if line.starts_with("- ") {
            // This is a list item - could be variable, RDF source, etc.
            // For now, treat as variable
            let var = line[2..].trim();
            if !var.is_empty() {
                metadata.variables.push(var.to_string());
            }
        } else if line.contains(":") && !line.starts_with("  ") {
            // This is a key-value pair
            if let Some((key, value)) = line.split_once(':') {
                let key = key.trim();
                let value = value.trim();

                match key {
                    "seed" => {
                        if let Ok(seed) = value.parse::<u64>() {
                            metadata.determinism_seed = Some(seed);
                        }
                    }
                    _ => {}
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

pub async fn run(args: &ShowArgs) -> Result<()> {
    // Validate input
    validate_template_ref(&args.template_ref)?;

    println!("📄 Template Information:");

    // Determine template path
    let template_path = if args.template_ref.starts_with("gpack:") {
        return Err(ggen_utils::error::Error::new(
            "gpack templates not yet supported",
        ));
    } else if args.template_ref.contains('/') {
        args.template_ref.clone()
    } else {
        format!("templates/{}", args.template_ref)
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
    let content = std::fs::read_to_string(&template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))?;

    // Parse template metadata
    let metadata = parse_template_metadata(&content, &template_path)?;

    // Display metadata
    println!("  Name: {}", metadata.name);
    println!("  Path: {}", metadata.path);

    if let Some(desc) = metadata.description {
        println!("  Description: {}", desc);
    }

    if let Some(output) = metadata.output_path {
        println!("  Output: {}", output);
    }

    if !metadata.variables.is_empty() {
        println!("  Variables:");
        for var in metadata.variables {
            println!("    - {}", var);
        }
    }

    if !metadata.rdf_sources.is_empty() {
        println!("  RDF Sources:");
        for source in metadata.rdf_sources {
            println!("    - {}", source);
        }
    }

    if !metadata.sparql_queries.is_empty() {
        println!("  SPARQL Queries:");
        for (name, _) in metadata.sparql_queries {
            println!("    - {}", name);
        }
    }

    if let Some(seed) = metadata.determinism_seed {
        println!("  Determinism Seed: {}", seed);
    }

    Ok(())
}

pub async fn run_with_deps(args: &ShowArgs, fetcher: &dyn TemplateMetadataFetcher) -> Result<()> {
    // Validate input
    validate_template_ref(&args.template_ref)?;

    // Show progress for metadata fetching
    println!("🔍 Fetching template metadata...");

    let metadata = fetcher.fetch_metadata(&args.template_ref)?;

    println!("📄 Template Information:");
    println!("  Name: {}", metadata.name);
    println!("  Path: {}", metadata.path);

    if let Some(desc) = metadata.description {
        println!("  Description: {}", desc);
    }

    if let Some(output) = metadata.output_path {
        println!("  Output: {}", output);
    }

    if !metadata.variables.is_empty() {
        println!("  Variables:");
        for var in metadata.variables {
            println!("    - {}", var);
        }
    }

    if !metadata.rdf_sources.is_empty() {
        println!("  RDF Sources:");
        for source in metadata.rdf_sources {
            println!("    - {}", source);
        }
    }

    if !metadata.sparql_queries.is_empty() {
        println!("  SPARQL Queries:");
        for (name, _) in metadata.sparql_queries {
            println!("    - {}", name);
        }
    }

    if let Some(seed) = metadata.determinism_seed {
        println!("  Determinism Seed: {}", seed);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_show_displays_metadata() {
        let mut mock_fetcher = MockTemplateMetadataFetcher::new();
        mock_fetcher
            .expect_fetch_metadata()
            .with(eq(String::from("hello.tmpl")))
            .times(1)
            .returning(|_| {
                Ok(TemplateMetadata {
                    name: "hello.tmpl".to_string(),
                    path: "templates/hello.tmpl".to_string(),
                    description: Some("A simple hello world template".to_string()),
                    variables: vec!["name".to_string(), "greeting".to_string()],
                    output_path: Some("{{ name }}.txt".to_string()),
                    rdf_sources: vec![],
                    sparql_queries: HashMap::new(),
                    determinism_seed: Some(42),
                })
            });

        let args = ShowArgs {
            template_ref: "hello.tmpl".to_string(),
        };

        let result = run_with_deps(&args, &mock_fetcher).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_show_with_rdf_and_sparql() {
        let mut mock_fetcher = MockTemplateMetadataFetcher::new();
        mock_fetcher
            .expect_fetch_metadata()
            .times(1)
            .returning(|_| {
                let mut queries = HashMap::new();
                queries.insert(
                    "get_classes".to_string(),
                    "SELECT ?class WHERE { ?class a owl:Class }".to_string(),
                );

                Ok(TemplateMetadata {
                    name: "ontology.tmpl".to_string(),
                    path: "templates/ontology.tmpl".to_string(),
                    description: None,
                    variables: vec![],
                    output_path: None,
                    rdf_sources: vec!["schema.ttl".to_string()],
                    sparql_queries: queries,
                    determinism_seed: None,
                })
            });

        let args = ShowArgs {
            template_ref: "ontology.tmpl".to_string(),
        };

        let result = run_with_deps(&args, &mock_fetcher).await;
        assert!(result.is_ok());
    }
}
