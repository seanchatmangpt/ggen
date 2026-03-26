//! Core rule abstraction for composable code generation

use crate::{GeneratedFile, GenerationMode, Queryable, Renderable, Result};
use std::path::PathBuf;

/// A composable code generation rule
///
/// Combines a query (Q) and a template (T) to generate files.
/// This is the core abstraction: all code generation rules use this pattern.
///
/// # Type Parameters
/// * `Q` - Query type implementing `Queryable`
/// * `T` - Template type implementing `Renderable`
#[derive(Debug)]
pub struct Rule<Q: Queryable, T: Renderable> {
    /// Rule name (e.g., "jpa-entities", "repositories")
    name: String,
    /// Query executor
    query: Q,
    /// Template renderer
    template: T,
    /// Output file path pattern (may contain {{ }} for variable substitution)
    output_file: PathBuf,
    /// Generation mode (Overwrite, Append, SkipIfExists)
    mode: GenerationMode,
}

impl<Q: Queryable, T: Renderable> Rule<Q, T> {
    /// Create a new rule
    pub fn new(
        name: impl Into<String>, query: Q, template: T, output_file: impl Into<PathBuf>,
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
    /// 1. Execute query to get bindings
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

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
