//! Domain logic for template rendering with RDF integration
//!
//! This module provides the v2 API for rendering templates with RDF/SPARQL support,
//! maintaining backward compatibility with v1 template rendering.

use ggen_core::{Graph, Template};
use ggen_utils::error::Result;
use regex::Regex;
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
    /// Path to the generated output file (or directory if multi-file)
    pub output_path: PathBuf,

    /// Number of bytes written (total if multi-file)
    pub bytes_written: usize,

    /// Template path used
    pub template_path: PathBuf,

    /// Number of RDF files loaded
    pub rdf_files_loaded: usize,

    /// Number of SPARQL queries executed
    pub sparql_queries_executed: usize,

    /// Variables used in rendering
    pub variables_used: usize,

    /// Number of files created (for multi-file generation)
    pub files_created: usize,
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
    // For multi-file output, we check the base directory, not the specific file
    // (File marker detection happens after rendering, so we can't check individual files yet)
    if options.output_path.exists() && !options.force_overwrite {
        return Err(ggen_utils::error::Error::new(&format!(
            "Output file already exists: {}. Use force_overwrite to overwrite.",
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
    let template_content = std::fs::read_to_string(&options.template_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))?;

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

    // Create RDF graph
    let mut graph = Graph::new().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create RDF graph: {}", e))
    })?;

    // Render frontmatter first - this populates template.front from raw_frontmatter
    template
        .render_frontmatter(&mut tera, &context)
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to render frontmatter: {}", e))
        })?;

    // Count SPARQL queries from frontmatter AFTER render_frontmatter populates it
    let sparql_count = template.front.sparql.len();

    // Determine base output directory for resolving frontmatter `to` paths
    let base_output_dir = if options.output_path.is_dir() {
        options.output_path.clone()
    } else {
        options
            .output_path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| {
                std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."))
            })
    };

    // Resolve final output path from frontmatter `to` if present
    // This honors the template's declared output path (e.g., `to: "ggen.toml"`)
    let resolved_output_path = if let Some(to_path) = &template.front.to {
        // Render the `to` path through Tera to resolve any template variables
        let rendered_to = tera.render_str(to_path, &context).map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to render frontmatter 'to' path '{}': {}",
                to_path, e
            ))
        })?;

        // Join with base output directory
        let joined_path = base_output_dir.join(&rendered_to);

        // Security: Prevent path traversal attacks by ensuring the resolved path
        // stays within base_output_dir. This prevents paths like "../../../etc/passwd"
        // from escaping the output directory.
        // We normalize the path and check that all components stay within base_output_dir
        let normalized = joined_path.components().collect::<Vec<_>>();
        let base_components = base_output_dir.components().collect::<Vec<_>>();

        // Check that normalized path starts with base components
        if normalized.len() < base_components.len() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Output path '{}' would escape output root '{}'",
                rendered_to,
                base_output_dir.display()
            )));
        }

        for (i, component) in base_components.iter().enumerate() {
            if normalized.get(i) != Some(component) {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Output path '{}' would escape output root '{}'",
                    rendered_to,
                    base_output_dir.display()
                )));
            }
        }

        joined_path
    } else {
        // No frontmatter `to` - use the provided output_path
        options.output_path.clone()
    };

    // Re-check overwrite guard for resolved path (if different from original)
    if resolved_output_path != options.output_path
        && resolved_output_path.exists()
        && !options.force_overwrite
    {
        return Err(ggen_utils::error::Error::new(&format!(
            "Output file already exists: {}. Use force_overwrite to overwrite.",
            resolved_output_path.display()
        )));
    }

    // Render using v2 RDF integration
    // If RDF files provided via CLI, use those (take precedence)
    // Otherwise, use frontmatter rdf: field (filesystem-routed)
    let rendered_content = if !options.rdf_files.is_empty() {
        // Use CLI-provided RDF files (explicit)
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
    } else if !template.front.rdf.is_empty() {
        // Use frontmatter rdf: field (filesystem-routed)
        // Process graph will load RDF files from frontmatter
        template
            .process_graph(&mut graph, &mut tera, &context, &options.template_path)
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to process graph: {}", e))
            })?;

        // Render body with SPARQL results available
        template.render(&mut tera, &context).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Template rendering failed: {}", e))
        })?
    } else {
        // No RDF files - backward compatible v1 API
        template
            .process_graph(&mut graph, &mut tera, &context, &options.template_path)
            .map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to process graph: {}", e))
            })?;

        template.render(&mut tera, &context).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Template rendering failed: {}", e))
        })?
    };

    // Check for file markers and split if present
    let (files_created, total_bytes, output_path) = if rendered_content.contains("{# FILE:") {
        // Multi-file generation mode
        // Use base_output_dir for file markers
        // File markers contain paths relative to project root (e.g., "crates/clnrm-v2-generated/...")
        let files = split_file_markers(&rendered_content, &base_output_dir)?;

        for (file_path, content) in &files {
            if let Some(parent) = file_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    ggen_utils::error::Error::new(&format!(
                        "Failed to create output directory for {}: {}",
                        file_path.display(),
                        e
                    ))
                })?;
            }

            std::fs::write(file_path, content).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to write file {}: {}",
                    file_path.display(),
                    e
                ))
            })?;
        }

        let total_bytes: usize = files.iter().map(|(_, content)| content.len()).sum();
        (files.len(), total_bytes, base_output_dir.to_path_buf())
    } else {
        // Single file output - use resolved path (honors frontmatter `to`)
        // Ensure parent directory exists
        if let Some(parent) = resolved_output_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to create output directory for {}: {}",
                    resolved_output_path.display(),
                    e
                ))
            })?;
        }

        std::fs::write(&resolved_output_path, &rendered_content).map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to write output to {}: {}",
                resolved_output_path.display(),
                e
            ))
        })?;

        (1, rendered_content.len(), resolved_output_path)
    };

    // Count RDF files loaded (from CLI args or frontmatter)
    let rdf_files_loaded = if !options.rdf_files.is_empty() {
        // RDF files provided via CLI/API
        options.rdf_files.len()
    } else if !template.front.rdf.is_empty() {
        // RDF files loaded from frontmatter (filesystem-routed)
        template.front.rdf.len()
    } else {
        // No RDF files
        0
    };

    Ok(RenderWithRdfResult {
        output_path,
        bytes_written: total_bytes,
        template_path: options.template_path.clone(),
        rdf_files_loaded,
        sparql_queries_executed: sparql_count,
        variables_used: options.variables.len(),
        files_created,
    })
}

/// Generate a template from RDF metadata
///
/// This function takes RDF/TTL files describing template metadata
/// and generates a template file from them.
pub fn generate_from_rdf(
    rdf_files: Vec<PathBuf>, output_template_path: PathBuf,
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

        graph
            .insert_turtle(&ttl_content)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to parse RDF: {}", e)))?;
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

    let results = graph
        .query_cached(query)
        .map_err(|e| ggen_utils::error::Error::new(&format!("SPARQL query failed: {}", e)))?;

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
    std::fs::write(&output_template_path, template_content)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to write template: {}", e)))?;

    Ok(output_template_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_render_with_rdf_options_builder() {
        let options =
            RenderWithRdfOptions::new(PathBuf::from("template.tmpl"), PathBuf::from("output.txt"))
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

        let options =
            RenderWithRdfOptions::new(template_path, output_path.clone()).with_var("name", "World");

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
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person }"
---
Found {{ sparql_results.people | length }} person(s)"#,
        )
        .unwrap();

        let options = RenderWithRdfOptions::new(template_path, output_path.clone());

        let result = render_with_rdf(&options).unwrap();

        // Verify output was written
        assert!(result.bytes_written > 0);

        // Verify the SPARQL query was executed by checking output content
        let content = fs::read_to_string(&output_path).unwrap();
        assert!(
            content.contains("Found 1 person(s)"),
            "Expected output to contain 'Found 1 person(s)' but got: {}",
            content
        );
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
        assert!(result.unwrap_err().to_string().contains("already exists"));
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

    #[test]
    fn test_split_file_markers() {
        let content = r#"Some content before
{# FILE: file1.txt #}
Content for file 1
Line 2

{# FILE: subdir/file2.txt #}
Content for file 2
"#;
        let base_dir = std::path::Path::new("/tmp/test");
        let files = split_file_markers(content, base_dir).unwrap();

        assert_eq!(files.len(), 2);
        assert_eq!(files[0].0, base_dir.join("file1.txt"));
        assert_eq!(files[0].1.trim(), "Content for file 1\nLine 2");
        assert_eq!(files[1].0, base_dir.join("subdir/file2.txt"));
        assert_eq!(files[1].1.trim(), "Content for file 2");
    }

    #[test]
    fn test_render_honors_frontmatter_to_for_ggen_toml() {
        // Regression test: Verify that frontmatter `to: "ggen.toml"` is honored
        // even when a placeholder output path is provided (CLI-style usage)
        let temp_dir = TempDir::new().unwrap();
        let template_path = temp_dir.path().join("ggen.toml.tmpl");
        let placeholder_output = temp_dir.path().join("placeholder");

        // Create template with frontmatter `to: "ggen.toml"` and template variables
        fs::write(
            &template_path,
            r#"---
to: "ggen.toml"
---
[project]
name = "{{ project_name }}"
version = "{{ project_version }}"
description = "{{ project_description }}"
"#,
        )
        .unwrap();

        // Use placeholder output path (simulating CLI usage)
        let options = RenderWithRdfOptions::new(template_path, placeholder_output.clone())
            .with_var("project_name", "test-project")
            .with_var("project_version", "1.0.0")
            .with_var("project_description", "A test project");

        let result = render_with_rdf(&options).unwrap();

        // Verify file was written to resolved location (not placeholder)
        let resolved_path = temp_dir.path().join("ggen.toml");
        assert!(
            resolved_path.exists(),
            "File should be written to resolved path: {}",
            resolved_path.display()
        );
        assert_eq!(
            result.output_path, resolved_path,
            "Result should report resolved path"
        );

        // Verify placeholder was NOT created
        assert!(
            !placeholder_output.exists(),
            "Placeholder path should not exist: {}",
            placeholder_output.display()
        );

        // Verify content was rendered correctly with variables
        let content = fs::read_to_string(&resolved_path).unwrap();
        assert!(
            content.contains("name = \"test-project\""),
            "Content should contain rendered project_name"
        );
        assert!(
            content.contains("version = \"1.0.0\""),
            "Content should contain rendered project_version"
        );
        assert!(
            content.contains("description = \"A test project\""),
            "Content should contain rendered project_description"
        );
        assert!(
            content.contains("[project]"),
            "Content should contain project section"
        );

        // Verify bytes written
        assert!(result.bytes_written > 0, "Should report bytes written");
        assert_eq!(result.files_created, 1, "Should report 1 file created");
    }
}

/// Split rendered content by file markers
///
/// Parses `{# FILE: path/to/file.ext #}` markers and returns a vector of
/// (file_path, content) tuples. Content before the first marker is discarded.
fn split_file_markers(content: &str, base_dir: &Path) -> Result<Vec<(PathBuf, String)>> {
    let file_marker_re = Regex::new(r"\{#\s*FILE:\s*([^\s#]+)\s*#\}").map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to compile file marker regex: {}", e))
    })?;

    let mut files = Vec::new();
    let mut current_file: Option<PathBuf> = None;
    let mut current_content = String::new();

    for line in content.lines() {
        if let Some(captures) = file_marker_re.captures(line) {
            // Write previous file if exists
            if let Some(path) = current_file.take() {
                files.push((path, current_content.trim_end().to_string()));
            }

            // Start new file
            let file_path = captures.get(1).unwrap().as_str();
            let full_path = base_dir.join(file_path);
            current_file = Some(full_path);
            current_content.clear();
        } else if current_file.is_some() {
            // Accumulate content for current file
            current_content.push_str(line);
            current_content.push('\n');
        }
        // Content before first marker is ignored
    }

    // Write last file if exists
    if let Some(path) = current_file {
        files.push((path, current_content.trim_end().to_string()));
    }

    Ok(files)
}
