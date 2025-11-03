use anyhow::{Context, Result};
use glob::glob;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Project conventions discovered from the file system
#[derive(Debug, Clone)]
pub struct ProjectConventions {
    /// RDF files discovered in domain/ directory
    pub rdf_files: Vec<PathBuf>,
    /// Base directory for RDF files (for watching)
    pub rdf_dir: PathBuf,
    /// Templates discovered in templates/ directory (name -> path)
    pub templates: HashMap<String, PathBuf>,
    /// Base directory for templates (for watching)
    pub templates_dir: PathBuf,
    /// SPARQL queries discovered in queries/ directory (name -> content)
    pub queries: HashMap<String, String>,
    /// Output directory for generated code
    pub output_dir: PathBuf,
    /// Convention preset name (e.g., "clap-noun-verb", "default")
    pub preset: String,
}

/// Convention overrides from .ggen/conventions.toml
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct ConventionOverrides {
    #[serde(default)]
    rdf: RdfOverrides,
    #[serde(default)]
    templates: TemplatesOverrides,
    #[serde(default)]
    queries: QueriesOverrides,
    #[serde(default)]
    output: OutputOverrides,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct RdfOverrides {
    #[serde(default)]
    patterns: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct TemplatesOverrides {
    #[serde(default)]
    patterns: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct QueriesOverrides {
    #[serde(default)]
    patterns: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
struct OutputOverrides {
    #[serde(default)]
    dir: Option<String>,
}

/// Convention resolver that discovers project structure
pub struct ConventionResolver {
    project_root: PathBuf,
}

impl ConventionResolver {
    /// Create a new convention resolver for the given project root
    pub fn new(project_root: impl Into<PathBuf>) -> Self {
        Self {
            project_root: project_root.into(),
        }
    }

    /// Discover project conventions by scanning the file system
    pub fn discover(&self) -> Result<ProjectConventions> {
        // Load overrides if they exist
        let overrides = self.load_overrides()?;

        // Discover RDF files
        let rdf_files = self.discover_rdf(&overrides)?;

        // Discover templates
        let templates = self.discover_templates(&overrides)?;

        // Discover queries
        let queries = self.discover_queries(&overrides)?;

        // Resolve output directory
        let output_dir = self.resolve_output_dir(&overrides);

        Ok(ProjectConventions {
            rdf_files,
            rdf_dir: self.project_root.join("domain"),
            templates,
            templates_dir: self.project_root.join("templates"),
            queries,
            output_dir,
            preset: "clap-noun-verb".to_string(), // Default preset
        })
    }

    /// Load convention overrides from .ggen/conventions.toml if it exists
    fn load_overrides(&self) -> Result<ConventionOverrides> {
        let override_path = self.project_root.join(".ggen").join("conventions.toml");

        if override_path.exists() {
            let content = std::fs::read_to_string(&override_path)
                .context("Failed to read conventions.toml")?;
            let overrides: ConventionOverrides =
                toml::from_str(&content).context("Failed to parse conventions.toml")?;
            Ok(overrides)
        } else {
            Ok(ConventionOverrides::default())
        }
    }

    /// Discover RDF files in domain/ directory
    fn discover_rdf(&self, overrides: &ConventionOverrides) -> Result<Vec<PathBuf>> {
        let patterns = if overrides.rdf.patterns.is_empty() {
            vec!["domain/**/*.ttl".to_string()]
        } else {
            overrides.rdf.patterns.clone()
        };

        let mut files = Vec::new();
        for pattern in patterns {
            let full_pattern = self.project_root.join(&pattern);
            for entry in glob(&full_pattern.to_string_lossy())? {
                files.push(entry?);
            }
        }

        // Sort alphabetically for deterministic order
        files.sort();

        Ok(files)
    }

    /// Discover templates in templates/ directory
    fn discover_templates(&self, overrides: &ConventionOverrides) -> Result<HashMap<String, PathBuf>> {
        let patterns = if overrides.templates.patterns.is_empty() {
            vec!["templates/**/*.tmpl".to_string()]
        } else {
            overrides.templates.patterns.clone()
        };

        let mut templates = HashMap::new();
        let templates_base = self.project_root.join("templates");

        for pattern in patterns {
            let full_pattern = self.project_root.join(&pattern);
            for entry in glob(&full_pattern.to_string_lossy())? {
                let path = entry?;

                // Convert nested path to template name
                // e.g., templates/api/user.tmpl -> api/user
                let name = if let Ok(rel_path) = path.strip_prefix(&templates_base) {
                    rel_path
                        .with_extension("") // Remove .tmpl extension
                        .to_string_lossy()
                        .to_string()
                } else {
                    // Fallback: use file stem
                    path.file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string()
                };

                templates.insert(name, path);
            }
        }

        Ok(templates)
    }

    /// Discover SPARQL queries in queries/ directory
    fn discover_queries(&self, overrides: &ConventionOverrides) -> Result<HashMap<String, String>> {
        let patterns = if overrides.queries.patterns.is_empty() {
            vec!["queries/**/*.sparql".to_string()]
        } else {
            overrides.queries.patterns.clone()
        };

        let mut queries = HashMap::new();
        let queries_base = self.project_root.join("queries");

        for pattern in patterns {
            let full_pattern = self.project_root.join(&pattern);
            for entry in glob(&full_pattern.to_string_lossy())? {
                let path = entry?;

                // Read query content
                let content = std::fs::read_to_string(&path)
                    .with_context(|| format!("Failed to read query file: {:?}", path))?;

                // Convert nested path to query name
                // e.g., queries/user/find.sparql -> user/find
                let name = if let Ok(rel_path) = path.strip_prefix(&queries_base) {
                    rel_path
                        .with_extension("") // Remove .sparql extension
                        .to_string_lossy()
                        .to_string()
                } else {
                    // Fallback: use file stem
                    path.file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string()
                };

                queries.insert(name, content);
            }
        }

        Ok(queries)
    }

    /// Resolve output directory (default: generated/)
    fn resolve_output_dir(&self, overrides: &ConventionOverrides) -> PathBuf {
        if let Some(ref dir) = overrides.output.dir {
            self.project_root.join(dir)
        } else {
            self.project_root.join("generated")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn setup_test_project() -> TempDir {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create directory structure
        fs::create_dir_all(root.join("domain")).unwrap();
        fs::create_dir_all(root.join("templates/api")).unwrap();
        fs::create_dir_all(root.join("queries/user")).unwrap();

        // Create RDF files
        fs::write(root.join("domain/user.ttl"), "@prefix ex: <http://example.org/> .").unwrap();
        fs::write(root.join("domain/order.ttl"), "@prefix ex: <http://example.org/> .").unwrap();

        // Create template files
        fs::write(root.join("templates/main.tmpl"), "Hello {{ name }}").unwrap();
        fs::write(root.join("templates/api/user.tmpl"), "User API").unwrap();

        // Create query files
        fs::write(
            root.join("queries/user/find.sparql"),
            "SELECT * WHERE { ?s ?p ?o }",
        )
        .unwrap();

        temp_dir
    }

    #[test]
    fn test_new() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = ConventionResolver::new(temp_dir.path());

        assert_eq!(resolver.project_root, temp_dir.path());
    }

    #[test]
    fn test_discover_rdf_files() {
        let temp_dir = setup_test_project();
        let resolver = ConventionResolver::new(temp_dir.path());

        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.rdf_files.len(), 2);
        assert!(conventions
            .rdf_files
            .iter()
            .any(|p| p.ends_with("user.ttl")));
        assert!(conventions
            .rdf_files
            .iter()
            .any(|p| p.ends_with("order.ttl")));

        // Verify alphabetical order
        assert!(conventions.rdf_files[0].ends_with("order.ttl"));
        assert!(conventions.rdf_files[1].ends_with("user.ttl"));
    }

    #[test]
    fn test_discover_templates() {
        let temp_dir = setup_test_project();
        let resolver = ConventionResolver::new(temp_dir.path());

        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.templates.len(), 2);
        assert!(conventions.templates.contains_key("main"));
        assert!(conventions.templates.contains_key("api/user"));

        let main_path = &conventions.templates["main"];
        assert!(main_path.ends_with("templates/main.tmpl"));
    }

    #[test]
    fn test_discover_queries() {
        let temp_dir = setup_test_project();
        let resolver = ConventionResolver::new(temp_dir.path());

        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.queries.len(), 1);
        assert!(conventions.queries.contains_key("user/find"));

        let query = &conventions.queries["user/find"];
        assert!(query.contains("SELECT * WHERE"));
    }

    #[test]
    fn test_resolve_output_dir_default() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = ConventionResolver::new(temp_dir.path());

        let conventions = resolver.discover().unwrap();

        assert_eq!(
            conventions.output_dir,
            temp_dir.path().join("generated")
        );
    }

    #[test]
    fn test_resolve_output_dir_override() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create .ggen directory and conventions.toml
        fs::create_dir_all(root.join(".ggen")).unwrap();
        fs::write(
            root.join(".ggen/conventions.toml"),
            r#"
[output]
dir = "build/generated"
"#,
        )
        .unwrap();

        let resolver = ConventionResolver::new(root);
        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.output_dir, root.join("build/generated"));
    }

    #[test]
    fn test_override_rdf_patterns() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create custom RDF location
        fs::create_dir_all(root.join("ontology")).unwrap();
        fs::write(
            root.join("ontology/custom.ttl"),
            "@prefix ex: <http://example.org/> .",
        )
        .unwrap();

        // Create override config
        fs::create_dir_all(root.join(".ggen")).unwrap();
        fs::write(
            root.join(".ggen/conventions.toml"),
            r#"
[rdf]
patterns = ["ontology/**/*.ttl"]
"#,
        )
        .unwrap();

        let resolver = ConventionResolver::new(root);
        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.rdf_files.len(), 1);
        assert!(conventions.rdf_files[0].ends_with("custom.ttl"));
    }

    #[test]
    fn test_override_template_patterns() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create custom template location
        fs::create_dir_all(root.join("views")).unwrap();
        fs::write(root.join("views/page.tmpl"), "Page template").unwrap();

        // Create override config
        fs::create_dir_all(root.join(".ggen")).unwrap();
        fs::write(
            root.join(".ggen/conventions.toml"),
            r#"
[templates]
patterns = ["views/**/*.tmpl"]
"#,
        )
        .unwrap();

        let resolver = ConventionResolver::new(root);
        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.templates.len(), 1);
        // The template name will be just "page" since views is not recognized as templates base
        assert!(conventions.templates.values().any(|p| p.ends_with("page.tmpl")));
    }

    #[test]
    fn test_override_query_patterns() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create custom query location
        fs::create_dir_all(root.join("sparql")).unwrap();
        fs::write(
            root.join("sparql/select.sparql"),
            "SELECT * WHERE { ?s ?p ?o }",
        )
        .unwrap();

        // Create override config
        fs::create_dir_all(root.join(".ggen")).unwrap();
        fs::write(
            root.join(".ggen/conventions.toml"),
            r#"
[queries]
patterns = ["sparql/**/*.sparql"]
"#,
        )
        .unwrap();

        let resolver = ConventionResolver::new(root);
        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.queries.len(), 1);
        assert!(conventions
            .queries
            .values()
            .any(|c| c.contains("SELECT * WHERE")));
    }

    #[test]
    fn test_empty_project() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = ConventionResolver::new(temp_dir.path());

        let conventions = resolver.discover().unwrap();

        assert!(conventions.rdf_files.is_empty());
        assert!(conventions.templates.is_empty());
        assert!(conventions.queries.is_empty());
        assert_eq!(
            conventions.output_dir,
            temp_dir.path().join("generated")
        );
    }

    #[test]
    fn test_nested_template_names() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create nested template structure
        fs::create_dir_all(root.join("templates/api/v1")).unwrap();
        fs::write(root.join("templates/api/v1/user.tmpl"), "User API").unwrap();

        let resolver = ConventionResolver::new(root);
        let conventions = resolver.discover().unwrap();

        assert_eq!(conventions.templates.len(), 1);
        assert!(conventions.templates.contains_key("api/v1/user"));
    }

    #[test]
    fn test_load_overrides_invalid_toml() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create invalid TOML
        fs::create_dir_all(root.join(".ggen")).unwrap();
        fs::write(
            root.join(".ggen/conventions.toml"),
            "invalid toml content [[[",
        )
        .unwrap();

        let resolver = ConventionResolver::new(root);

        // Should return an error
        assert!(resolver.discover().is_err());
    }
}
