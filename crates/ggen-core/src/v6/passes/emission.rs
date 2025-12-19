//! μ₃: Emission Pass
//!
//! Performs bindings → files transformation using Tera templates.
//! Renders extracted data into source code files.

use crate::v6::guard::{GuardAction, GuardSet, GuardViolation};
use crate::v6::pass::{Pass, PassContext, PassResult, PassType};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::time::Instant;

/// An emission rule that produces files from templates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmissionRule {
    /// Rule name for auditing
    pub name: String,

    /// Path to Tera template file (relative to base_path)
    pub template_path: PathBuf,

    /// Output file pattern (supports Tera syntax for dynamic paths)
    pub output_pattern: String,

    /// Binding key to use as context (from extraction)
    pub binding_key: String,

    /// Whether to iterate over binding array items
    pub iterate: bool,

    /// Skip if binding is empty
    pub skip_empty: bool,

    /// Description for documentation
    pub description: Option<String>,
}

/// μ₃: Emission pass implementation
#[derive(Debug, Clone)]
pub struct EmissionPass {
    /// Rules to execute
    rules: Vec<EmissionRule>,

    /// Guards to apply to outputs
    guards: GuardSet,
}

impl EmissionPass {
    /// Create a new emission pass
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            guards: GuardSet::default_v6(),
        }
    }

    /// Add an emission rule
    pub fn add_rule(&mut self, rule: EmissionRule) {
        self.rules.push(rule);
    }

    /// Create with a set of rules
    pub fn with_rules(mut self, rules: Vec<EmissionRule>) -> Self {
        self.rules = rules;
        self
    }

    /// Set custom guards
    pub fn with_guards(mut self, guards: GuardSet) -> Self {
        self.guards = guards;
        self
    }

    /// Render a single file from template and context
    fn render_file(
        &self, ctx: &PassContext<'_>, rule: &EmissionRule, template_content: &str,
        item_context: &serde_json::Value,
    ) -> Result<(PathBuf, String)> {
        // Create Tera instance
        let mut tera = tera::Tera::default();
        tera.add_raw_template("template", template_content)
            .map_err(|e| Error::new(&format!("Template parse error in '{}': {}", rule.name, e)))?;

        // Build context
        let mut context = tera::Context::new();

        // Add project metadata
        context.insert("project_name", &ctx.project_name);
        context.insert("project_version", &ctx.project_version);

        // Add all bindings
        for (key, value) in &ctx.bindings {
            context.insert(key, value);
        }

        // Add current item if iterating
        if rule.iterate {
            context.insert("item", item_context);

            // Also flatten item properties to top level for convenience
            if let Some(obj) = item_context.as_object() {
                for (key, value) in obj {
                    context.insert(key, value);
                }
            }
        }

        // Render template
        let content = tera.render("template", &context).map_err(|e| {
            Error::new(&format!(
                "Template render error in '{}': {}",
                rule.name, e
            ))
        })?;

        // Render output path
        let output_path_str = tera
            .render_str(&rule.output_pattern, &context)
            .map_err(|e| {
                Error::new(&format!(
                    "Output path template error in '{}': {}",
                    rule.name, e
                ))
            })?;

        let output_path = PathBuf::from(output_path_str.trim());

        Ok((output_path, content))
    }

    /// Execute a single emission rule
    fn execute_rule(
        &self, ctx: &mut PassContext<'_>, rule: &EmissionRule,
    ) -> Result<Vec<PathBuf>> {
        let mut generated_files = Vec::new();

        // Load template content
        let template_path = ctx.base_path.join(&rule.template_path);
        let template_content = std::fs::read_to_string(&template_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read template '{}': {}",
                template_path.display(),
                e
            ))
        })?;

        // Get binding value
        let binding = ctx.bindings.get(&rule.binding_key).cloned();

        if rule.iterate {
            // Iterate over array items
            if let Some(serde_json::Value::Array(items)) = &binding {
                if items.is_empty() && rule.skip_empty {
                    return Ok(generated_files);
                }

                for item in items {
                    let (output_path, content) =
                        self.render_file(ctx, rule, &template_content, item)?;

                    // Apply guards
                    let violations = self.guards.check(&output_path, &content);
                    self.handle_violations(&violations)?;

                    // Write file
                    let full_output_path = ctx.output_dir.join(&output_path);
                    self.write_output(&full_output_path, &content)?;

                    generated_files.push(output_path);
                }
            }
        } else {
            // Single file from entire binding
            let context_value = binding.unwrap_or(serde_json::Value::Null);

            if context_value.is_null() && rule.skip_empty {
                return Ok(generated_files);
            }

            let (output_path, content) =
                self.render_file(ctx, rule, &template_content, &context_value)?;

            // Apply guards
            let violations = self.guards.check(&output_path, &content);
            self.handle_violations(&violations)?;

            // Write file
            let full_output_path = ctx.output_dir.join(&output_path);
            self.write_output(&full_output_path, &content)?;

            generated_files.push(output_path);
        }

        Ok(generated_files)
    }

    /// Handle guard violations
    fn handle_violations(&self, violations: &[GuardViolation]) -> Result<()> {
        for violation in violations {
            match violation.action {
                GuardAction::Reject => {
                    return Err(Error::new(&format!(
                        "Guard '{}' violation: {}",
                        violation.guard_name, violation.message
                    )));
                }
                GuardAction::Warn => {
                    eprintln!("WARNING [{}]: {}", violation.guard_name, violation.message);
                }
                GuardAction::RequireApproval => {
                    return Err(Error::new(&format!(
                        "Guard '{}' requires approval: {}",
                        violation.guard_name, violation.message
                    )));
                }
            }
        }
        Ok(())
    }

    /// Write output file with parent directory creation
    fn write_output(&self, path: &std::path::Path, content: &str) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!(
                    "Failed to create directory '{}': {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        std::fs::write(path, content).map_err(|e| {
            Error::new(&format!(
                "Failed to write file '{}': {}",
                path.display(),
                e
            ))
        })
    }
}

impl Default for EmissionPass {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for EmissionPass {
    fn pass_type(&self) -> PassType {
        PassType::Emission
    }

    fn name(&self) -> &str {
        "μ₃:emission"
    }

    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();
        let mut all_generated = Vec::new();

        for rule in &self.rules {
            let generated = self.execute_rule(ctx, rule)?;
            all_generated.extend(generated);
        }

        // Update context with generated files
        ctx.generated_files.extend(all_generated.clone());

        let duration = start.elapsed();
        Ok(PassResult::success()
            .with_files(all_generated)
            .with_duration(duration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use tempfile::TempDir;

    #[test]
    fn test_emission_pass_empty() {
        let graph = Graph::new().unwrap();
        let pass = EmissionPass::new();

        let temp_dir = TempDir::new().unwrap();
        let mut ctx = PassContext::new(
            &graph,
            temp_dir.path().to_path_buf(),
            temp_dir.path().join("output"),
        );

        let result = pass.execute(&mut ctx).unwrap();
        assert!(result.success);
    }

    #[test]
    fn test_emission_single_file() {
        let graph = Graph::new().unwrap();
        let temp_dir = TempDir::new().unwrap();

        // Create template file
        let template_dir = temp_dir.path().join("templates");
        std::fs::create_dir_all(&template_dir).unwrap();
        std::fs::write(
            template_dir.join("domain.ttl.tera"),
            "@prefix ex: <http://example.org/> .\n# Generated for {{ project_name }}\n",
        )
        .unwrap();

        // Create output dir (use generated subdirectory to pass guards)
        let output_dir = temp_dir.path().join("ontology").join("generated");
        std::fs::create_dir_all(&output_dir).unwrap();

        let mut pass = EmissionPass::new();
        // Disable guards for testing
        pass.guards = GuardSet::new();

        pass.add_rule(EmissionRule {
            name: "generate-ontology".to_string(),
            template_path: PathBuf::from("templates/domain.ttl.tera"),
            output_pattern: "domain.ttl".to_string(),
            binding_key: "data".to_string(),
            iterate: false,
            skip_empty: false,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone())
            .with_project("TestProject".to_string(), "1.0.0".to_string());

        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert_eq!(result.files_generated.len(), 1);
        assert!(output_dir.join("domain.ttl").exists());
    }

    #[test]
    fn test_emission_with_iteration() {
        let graph = Graph::new().unwrap();
        let temp_dir = TempDir::new().unwrap();

        // Create template
        let template_dir = temp_dir.path().join("templates");
        std::fs::create_dir_all(&template_dir).unwrap();
        std::fs::write(
            template_dir.join("entity.rs.tera"),
            "pub struct {{ name }} {}",
        )
        .unwrap();

        let output_dir = temp_dir.path().join("output");

        let mut pass = EmissionPass::new();
        pass.guards = GuardSet::new(); // Disable guards for testing

        pass.add_rule(EmissionRule {
            name: "generate-entities".to_string(),
            template_path: PathBuf::from("templates/entity.rs.tera"),
            output_pattern: "{{ name | lower }}.rs".to_string(),
            binding_key: "entities".to_string(),
            iterate: true,
            skip_empty: true,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());

        // Add bindings
        ctx.bindings.insert(
            "entities".to_string(),
            serde_json::json!([
                {"name": "User"},
                {"name": "Order"}
            ]),
        );

        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert_eq!(result.files_generated.len(), 2);
    }
}
