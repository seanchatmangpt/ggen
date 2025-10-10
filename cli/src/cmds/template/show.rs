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

pub async fn run(args: &ShowArgs) -> Result<()> {
    // Validate input
    validate_template_ref(&args.template_ref)?;

    println!("🚧 Placeholder: template show");
    println!("  Template: {}", args.template_ref.trim());
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
