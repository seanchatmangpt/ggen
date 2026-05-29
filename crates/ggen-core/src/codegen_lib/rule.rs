//! Core rule abstraction for composable code generation

use crate::codegen_lib::{GeneratedFile, GenerationMode, Queryable, Renderable, Result};
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

    // Chicago TDD: Real inline query implementation (not a test double)
    struct InlineQuery {
        name: String,
        bindings: Vec<HashMap<String, String>>,
    }

    impl Queryable for InlineQuery {
        fn execute(&self) -> Result<Vec<HashMap<String, String>>> {
            // Real query execution that returns actual bindings
            Ok(self.bindings.clone())
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    // Chicago TDD: Real template implementation using actual Tera-like rendering
    struct InlineTemplate {
        name: String,
        template_str: String,
    }

    impl Renderable for InlineTemplate {
        fn render(&self, bindings: &HashMap<String, String>) -> Result<String> {
            // Real template rendering - substitute bindings into template
            let mut result = self.template_str.clone();
            for (key, value) in bindings {
                let placeholder = format!("{{{{{}}}}}", key);
                result = result.replace(&placeholder, value);
            }
            Ok(result)
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    #[test]
    fn test_rule_creation_with_real_query_and_template() {
        // Chicago TDD: Use real query and template implementations
        let mut bindings = HashMap::new();
        bindings.insert("className".to_string(), "TestClass".to_string());

        let query = InlineQuery {
            name: "test-query".to_string(),
            bindings: vec![bindings],
        };

        let template = InlineTemplate {
            name: "test-template".to_string(),
            template_str: "public class {{className}} {\n}\n".to_string(),
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
    fn test_query_execution_returns_bindings() {
        // Chicago TDD: Verify real query execution produces correct bindings
        let mut bindings = HashMap::new();
        bindings.insert("className".to_string(), "TestClass".to_string());
        bindings.insert("packageName".to_string(), "com.example".to_string());

        let query = InlineQuery {
            name: "test-query".to_string(),
            bindings: vec![bindings],
        };

        let results = query.execute().expect("Query should execute");
        assert_eq!(results.len(), 1, "Should return one binding set");
        assert_eq!(
            results[0].get("className").unwrap(),
            "TestClass",
            "Binding should contain className"
        );
        assert_eq!(
            results[0].get("packageName").unwrap(),
            "com.example",
            "Binding should contain packageName"
        );
    }

    #[test]
    fn test_template_rendering_with_real_bindings() {
        // Chicago TDD: Verify real template rendering produces correct output
        let mut bindings = HashMap::new();
        bindings.insert("className".to_string(), "TestClass".to_string());

        let template = InlineTemplate {
            name: "test-template".to_string(),
            template_str: "public class {{className}} {\n}\n".to_string(),
        };

        let rendered = template.render(&bindings).expect("Template should render");
        assert!(
            rendered.contains("TestClass"),
            "Rendered output should contain className"
        );
        assert!(
            rendered.contains("public class"),
            "Rendered output should contain template structure"
        );
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

        // Chicago TDD: Verify observable property - same content produces same hash
        assert_eq!(
            file1.content_hash, file2.content_hash,
            "Same content must produce identical hash"
        );
    }
}
