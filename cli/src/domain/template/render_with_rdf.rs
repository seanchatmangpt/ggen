//! Domain logic for template rendering with RDF integration
//!
//! This module provides the v2 API for rendering templates with RDF/SPARQL support,
//! maintaining backward compatibility with v1 template rendering.

use ggen_core::{Graph, Template};
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tera::Context;

/// Options for rendering templates with RDF support
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RenderWithRdfOptions {
    /// Path to the template file
    pub template_path: PathBuf,

    /// RDF/TTL files to load for context
    pub rdf_files: Vec<PathBuf>,

    /// Output path for the rendered template
    pub output_path: PathBuf,

    /// Additional variables for template context
    pub variables: BTreeMap<String, String>,

    /// Force overwrite existing output
    pub force_overwrite: bool,

    /// Use preprocessor for template processing
    pub use_preprocessor: bool,
}

impl RenderWithRdfOptions {
    pub fn new(template_path: PathBuf, output_path: PathBuf) -> Self {
        Self {
            template_path,
            output_path,
            rdf_files: Vec::new(),
            variables: BTreeMap::new(),
            force_overwrite: false,
            use_preprocessor: false,
        }
    }

    pub fn with_rdf_file(mut self, rdf_file: PathBuf) -> Self {
        self.rdf_files.push(rdf_file);
        self
    }

    pub fn with_rdf_files(mut self, rdf_files: Vec<PathBuf>) -> Self {
        self.rdf_files.extend(rdf_files);
        self
    }

    pub fn with_var(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.variables.insert(key.into(), value.into());
        self
    }

    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self {
        self.variables.extend(vars);
        self
    }

    pub fn force(mut self) -> Self {
        self.force_overwrite = true;
        self
    }

    pub fn with_preprocessor(mut self, enabled: bool) -> Self {
        self.use_preprocessor = enabled;
        self
    }
}

/// Result of template rendering with RDF
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderWithRdfResult {
    /// Path to the generated output file
    pub output_path: PathBuf,

    /// Number of bytes written
    pub bytes_written: usize,

    /// Template path used
    pub template_path: PathBuf,

    /// Number of RDF files loaded
    pub rdf_files_loaded: usize,

    /// Number of SPARQL queries executed
    pub sparql_queries_executed: usize,

    /// Variables used in rendering
    pub variables_used: usize,
}

/// Render a template with RDF data integration
///
/// This is the v2 API that integrates RDF/SPARQL with template rendering.
/// It maintains backward compatibility - if no RDF files are provided,
/// it behaves like the v1 API.
pub fn render_with_rdf(options: &RenderWithRdfOptions) -> Result<RenderWithRdfResult> {
    // Validate template exists
    if !options.template_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template not found: {}",
            options.template_path.display()
        )));
    }

    // Check if output exists and we're not forcing
    if options.output_path.exists() && !options.force_overwrite {
        return Err(ggen_utils::error::Error::new(&format!(
            "Output file already exists: {}. Use --force to overwrite.",
            options.output_path.display()
        )));
    }

    // Prepare output directory
    if let Some(parent) = options.output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create output directory: {}", e))
        })?;
    }

    // Read template content
    let template_content = std::fs::read_to_string(&options.template_path).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read template: {}", e))
    })?;

    // Parse template with or without preprocessor
    let output_dir = options
        .output_path
        .parent()
        .unwrap_or_else(|| Path::new("."));

    let mut template = if options.use_preprocessor {
        Template::parse_with_preprocessor(&template_content, &options.template_path, output_dir)
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to parse template: {}", e))
            })?
    } else {
        Template::parse(&template_content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to parse template: {}", e))
        })?
    };

    // Create Tera instance with filters
    let mut tera = ggen_core::tera_env::build_tera_minimal().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create Tera instance: {}", e))
    })?;

    // Build context from variables
    let mut context = Context::new();
    for (key, value) in &options.variables {
        context.insert(key, value);
    }

    // Count SPARQL queries from frontmatter
    let sparql_count = template.front.sparql.len();

    // Render with RDF if files provided, otherwise use v1 API
    let rendered_content = if !options.rdf_files.is_empty() {
        // Create RDF graph
        let mut graph = Graph::new().map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create RDF graph: {}", e))
        })?;

        // Render using v2 RDF integration
        template
            .render_with_rdf(
                options.rdf_files.clone(),
                &mut graph,
                &mut tera,
                &context,
                &options.template_path,
            )
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Template rendering failed: {}", e))
            })?
    } else {
        // Render frontmatter first
        template.render_frontmatter(&mut tera, &context).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to render frontmatter: {}", e))
        })?;

        // Process graph with inline RDF and SPARQL
        let mut graph = Graph::new().map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create RDF graph: {}", e))
        })?;

        template
            .process_graph(&mut graph, &mut tera, &context, &options.template_path)
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to process graph: {}", e))
            })?;

        // Render body using v1 API (backward compatible)
        template.render(&mut tera, &context).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Template rendering failed: {}", e))
        })?
    };

    // Write output
    std::fs::write(&options.output_path, &rendered_content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to write output: {}", e))
    })?;

    Ok(RenderWithRdfResult {
        output_path: options.output_path.clone(),
        bytes_written: rendered_content.len(),
        template_path: options.template_path.clone(),
        rdf_files_loaded: options.rdf_files.len(),
        sparql_queries_executed: sparql_count,
        variables_used: options.variables.len(),
    })
}

/// Generate a template from RDF metadata
///
/// This function takes RDF/TTL files describing template metadata
/// and generates a template file from them.
pub fn generate_from_rdf(
    rdf_files: Vec<PathBuf>,
    output_template_path: PathBuf,
) -> Result<PathBuf> {
    // Create RDF graph
    let graph = Graph::new().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create RDF graph: {}", e))
    })?;

    // Load RDF files
    for rdf_file in &rdf_files {
        let ttl_content = std::fs::read_to_string(rdf_file).map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to read RDF file '{}': {}",
                rdf_file.display(),
                e
            ))
        })?;

        graph.insert_turtle(&ttl_content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to parse RDF: {}", e))
        })?;
    }

    // Query for template metadata
    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?template ?name ?description ?to
        WHERE {
            ?template a ggen:Template ;
                ggen:templateName ?name .
            OPTIONAL { ?template ggen:templateDescription ?description }
            OPTIONAL { ?template ggen:outputPath ?to }
        }
        LIMIT 1
    "#;

    let results = graph.query_cached(query).map_err(|e| {
        ggen_utils::error::Error::new(&format!("SPARQL query failed: {}", e))
    })?;

    // Extract template metadata
    let (name, description, to) = match results {
        ggen_core::graph::CachedResult::Solutions(rows) if !rows.is_empty() => {
            let row = &rows[0];
            let name = row
                .get("name")
                .map(|s| s.trim_matches('"').to_string())
                .unwrap_or_else(|| "Generated Template".to_string());
            let description = row
                .get("description")
                .map(|s| s.trim_matches('"').to_string());
            let to = row.get("to").map(|s| s.trim_matches('"').to_string());
            (name, description, to)
        }
        _ => {
            return Err(ggen_utils::error::Error::new(
                "No template metadata found in RDF files",
            ))
        }
    };

    // Generate template content
    let mut template_content = String::from("---\n");

    if let Some(to_path) = to {
        template_content.push_str(&format!("to: \"{}\"\n", to_path));
    }

    template_content.push_str("---\n");

    if let Some(desc) = description {
        template_content.push_str(&format!("# {}\n", desc));
    }

    template_content.push_str(&format!("# Template: {}\n", name));
    template_content.push_str("# Generated from RDF metadata\n\n");
    template_content.push_str("Hello from {{ name }}!\n");

    // Write template file
    std::fs::write(&output_template_path, template_content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to write template: {}", e))
    })?;

    Ok(output_template_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_render_with_rdf_options_builder() {
        let options = RenderWithRdfOptions::new(
            PathBuf::from("template.tmpl"),
            PathBuf::from("output.txt"),
        )
        .with_rdf_file(PathBuf::from("data.ttl"))
        .with_var("name", "test")
        .force()
        .with_preprocessor(true);

        assert_eq!(options.rdf_files.len(), 1);
        assert_eq!(options.variables.len(), 1);
        assert!(options.force_overwrite);
        assert!(options.use_preprocessor);
    }

    #[test]
    fn test_render_with_rdf_backward_compatible() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.txt");

        // Create a simple v1-style template (no RDF)
        fs::write(
            &template_path,
            r#"---
to: "output.txt"
---
Hello, {{ name }}!"#,
        )
        .unwrap();

        let options = RenderWithRdfOptions::new(template_path, output_path.clone())
            .with_var("name", "World");

        let result = render_with_rdf(&options).unwrap();

        assert_eq!(result.output_path, output_path);
        assert_eq!(result.rdf_files_loaded, 0);
        assert!(result.bytes_written > 0);

        let content = fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("Hello, World!"));
    }

    #[test]
    fn test_render_with_inline_rdf() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.txt");

        // Create template with inline RDF
        fs::write(
            &template_path,
            r#"---
to: "output.txt"
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ; ex:age 30 ."
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person }"
---
Found {{ sparql_results.people | length }} person(s)"#,
        )
        .unwrap();

        let options = RenderWithRdfOptions::new(template_path, output_path.clone());

        let result = render_with_rdf(&options).unwrap();

        assert_eq!(result.sparql_queries_executed, 1);

        let content = fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("Found 1 person(s)"));
    }

    #[test]
    fn test_render_template_not_found() {
        let options = RenderWithRdfOptions::new(
            PathBuf::from("/nonexistent/template.tmpl"),
            PathBuf::from("output.txt"),
        );

        let result = render_with_rdf(&options);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Template not found"));
    }

    #[test]
    fn test_render_prevents_overwrite_without_force() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.txt");

        fs::write(&template_path, "---\nto: \"output.txt\"\n---\nContent").unwrap();
        fs::write(&output_path, "Existing").unwrap();

        let options = RenderWithRdfOptions::new(template_path, output_path);

        let result = render_with_rdf(&options);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("already exists"));
    }

    #[test]
    fn test_render_with_preprocessor() {
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("template.tmpl");
        let output_path = temp_dir.path().join("output.txt");

        fs::write(
            &template_path,
            r#"---
to: "output.txt"
---
Hello {{ name }}!"#,
        )
        .unwrap();

        let options = RenderWithRdfOptions::new(template_path, output_path.clone())
            .with_var("name", "Preprocessor")
            .with_preprocessor(true);

        let result = render_with_rdf(&options);
        assert!(result.is_ok());
    }

    #[test]
    fn test_generate_from_rdf() {
        let temp_dir = TempDir::new().unwrap();
        let rdf_file = temp_dir.path().join("metadata.ttl");
        let output_template = temp_dir.path().join("generated.tmpl");

        // Create RDF metadata
        let rdf_content = r#"
@prefix ggen: <http://ggen.dev/ontology#> .

<http://example.org/template1> a ggen:Template ;
    ggen:templateName "My Template" ;
    ggen:templateDescription "A generated template" ;
    ggen:outputPath "output.txt" .
"#;
        fs::write(&rdf_file, rdf_content).unwrap();

        let result = generate_from_rdf(vec![rdf_file], output_template.clone());
        assert!(result.is_ok());
        assert!(output_template.exists());

        let content = fs::read_to_string(&output_template).unwrap();
        assert!(content.contains("to: \"output.txt\""));
        assert!(content.contains("# A generated template"));
        assert!(content.contains("# Template: My Template"));
    }
}
