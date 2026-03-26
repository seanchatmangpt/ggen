//! Generic Rule<Q, T> system for composable code generation
//!
//! Implements type-safe, zero-cost abstractions for YAWL Java code generation rules.
//! All 10 rules (JPA entities, repositories, DTOs, etc.) use the same Rule<Q, T> pattern.

use crate::error::Result;
use std::collections::HashMap;
use std::path::PathBuf;

/// Trait for SPARQL query execution
///
/// Abstracts the execution of SPARQL SELECT queries and binding extraction.
/// Implementors return bindings (variable name → value) for template rendering.
pub trait Queryable: Send + Sync {
    /// Execute the query and return a vector of binding maps
    ///
    /// Each map represents one result row from the SPARQL SELECT query.
    /// Keys are SPARQL variable names (e.g., "?className", "?fieldName").
    /// Values are RDF term strings (IRIs, literals, etc.).
    fn execute(&self) -> Result<Vec<HashMap<String, String>>>;

    /// Query name for logging/auditing
    fn name(&self) -> &str;

    /// Optional: query source for debugging (SPARQL text)
    fn source(&self) -> Option<&str> {
        None
    }
}

/// Trait for Tera template rendering
///
/// Abstracts template rendering with context bindings.
/// Implementors transform SPARQL bindings into rendered output.
pub trait Renderable: Send + Sync {
    /// Render template with given context bindings
    ///
    /// # Arguments
    /// * `bindings` - SPARQL variable bindings (e.g., {"className": "YWorkItem", ...})
    ///
    /// # Returns
    /// Rendered file content (typically Java source code)
    fn render(&self, bindings: &HashMap<String, String>) -> Result<String>;

    /// Template name for logging/auditing
    fn name(&self) -> &str;

    /// Optional: template source for debugging
    fn source(&self) -> Option<&str> {
        None
    }
}

/// Represents a single generated file
#[derive(Debug, Clone)]
pub struct GeneratedFile {
    /// Relative file path (may contain {{ }} for variable substitution)
    pub path: PathBuf,
    /// File content
    pub content: String,
    /// Source rule name
    pub source_rule: String,
    /// Content hash for determinism verification
    pub content_hash: String,
}

impl GeneratedFile {
    /// Create a new generated file with content hash
    pub fn new(path: PathBuf, content: String, source_rule: String) -> Self {
        use sha2::{Sha256, Digest};

        let mut hasher = Sha256::new();
        hasher.update(&content);
        let content_hash = format!("{:x}", hasher.finalize());

        Self {
            path,
            content,
            source_rule,
            content_hash,
        }
    }

    /// Render the file path with bindings
    pub fn render_path(&self, bindings: &HashMap<String, String>) -> Result<PathBuf> {
        let path_str = self.path.to_string_lossy().to_string();

        // Simple template variable substitution ({{ varName }} → value)
        let rendered = bindings.iter().fold(path_str, |acc, (key, value)| {
            acc.replace(&format!("{{{{{}}}}}", key), value)
        });

        Ok(PathBuf::from(rendered))
    }
}

/// Generation mode for output files
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenerationMode {
    /// Overwrite existing file
    Overwrite,
    /// Append to existing file
    Append,
    /// Skip if file exists
    SkipIfExists,
}

/// A composable code generation rule
///
/// Combines a SPARQL query (Q) and a Tera template (T) to generate files.
/// This is the core abstraction: all 10 YAWL Java generation rules use this.
///
/// # Type Parameters
/// * `Q` - Query type implementing `Queryable` (SPARQL SELECT execution)
/// * `T` - Template type implementing `Renderable` (Tera template rendering)
#[derive(Debug)]
pub struct Rule<Q: Queryable, T: Renderable> {
    /// Rule name (e.g., "jpa-entities", "repositories")
    name: String,
    /// SPARQL query executor
    query: Q,
    /// Tera template renderer
    template: T,
    /// Output file path pattern (may contain {{ }} for variable substitution)
    output_file: PathBuf,
    /// Generation mode (Overwrite, Append, SkipIfExists)
    mode: GenerationMode,
}

impl<Q: Queryable, T: Renderable> Rule<Q, T> {
    /// Create a new rule
    pub fn new(
        name: impl Into<String>,
        query: Q,
        template: T,
        output_file: impl Into<PathBuf>,
        mode: GenerationMode,
    ) -> Self {
        Self {
            name: name.into(),
            query,
            template,
            output_file: output_file.into(),
            mode,
        }
    }

    /// Execute the rule and generate files
    ///
    /// # Process
    /// 1. Execute SPARQL query to get bindings
    /// 2. For each binding set:
    ///    - Render template with bindings → content
    ///    - Render output file path with bindings → actual path
    ///    - Create GeneratedFile
    /// 3. Return all generated files
    pub fn execute(&self) -> Result<Vec<GeneratedFile>> {
        // Execute query to get all binding sets
        let binding_sets = self.query.execute()?;

        // Render template for each binding set
        let files = binding_sets
            .iter()
            .map(|bindings| {
                // Render template content
                let content = self.template.render(bindings)?;

                // Render output file path with variable substitution
                let path = self.output_file.to_string_lossy().to_string();
                let rendered_path = bindings.iter().fold(path, |acc, (key, value)| {
                    acc.replace(&format!("{{{{{}}}}}", key), value)
                });

                Ok(GeneratedFile::new(
                    PathBuf::from(rendered_path),
                    content,
                    self.name.clone(),
                ))
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(files)
    }

    /// Get rule metadata
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn mode(&self) -> GenerationMode {
        self.mode
    }

    pub fn output_pattern(&self) -> &PathBuf {
        &self.output_file
    }
}

/// Execution record for a rule
#[derive(Debug, Clone)]
pub struct ExecutedRuleRecord {
    /// Rule name
    pub name: String,
    /// Files generated
    pub files_count: usize,
    /// Execution duration (milliseconds)
    pub duration_ms: u128,
    /// Whether execution succeeded
    pub success: bool,
    /// Error message if failed
    pub error: Option<String>,
}

/// Orchestrator for multiple rules
///
/// Runs rules sequentially and tracks execution state.
/// Enables parallel rule execution with proper batching.
#[allow(dead_code)]
pub struct RuleSet {
    rules: Vec<(String, Box<dyn std::any::Any>)>,
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockQuery {
        name: String,
    }

    impl Queryable for MockQuery {
        fn execute(&self) -> Result<Vec<HashMap<String, String>>> {
            let mut bindings = HashMap::new();
            bindings.insert("className".to_string(), "TestClass".to_string());
            Ok(vec![bindings])
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    struct MockTemplate {
        name: String,
    }

    impl Renderable for MockTemplate {
        fn render(&self, bindings: &HashMap<String, String>) -> Result<String> {
            Ok(format!(
                "public class {} {{\n}}\n",
                bindings.get("className").unwrap_or(&"Unknown".to_string())
            ))
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    #[test]
    fn test_rule_creation() {
        let query = MockQuery {
            name: "test-query".to_string(),
        };
        let template = MockTemplate {
            name: "test-template".to_string(),
        };

        let rule = Rule::new(
            "test-rule",
            query,
            template,
            "generated/Test.java",
            GenerationMode::Overwrite,
        );

        assert_eq!(rule.name(), "test-rule");
        assert_eq!(rule.mode(), GenerationMode::Overwrite);
    }

    #[test]
    fn test_generated_file_determinism() {
        let content1 = "public class Test {}";
        let file1 = GeneratedFile::new(
            PathBuf::from("Test.java"),
            content1.to_string(),
            "rule1".to_string(),
        );

        let file2 = GeneratedFile::new(
            PathBuf::from("Test.java"),
            content1.to_string(),
            "rule1".to_string(),
        );

        // Same content → same hash
        assert_eq!(file1.content_hash, file2.content_hash);
    }
}
