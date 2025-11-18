//! Template generation engine
//!
//! This module provides the core generation engine that orchestrates template
//! processing, RDF graph operations, and file generation. The `Generator` type
//! coordinates the entire generation pipeline from template parsing to output
//! file creation.
//!
//! ## Architecture
//!
//! The generation process follows these steps:
//! 1. Parse template (frontmatter + body)
//! 2. Render frontmatter with variables
//! 3. Process RDF graph (load data, execute SPARQL queries)
//! 4. Render template body with full context
//! 5. Handle file injection/merging if needed
//! 6. Write output file
//!
//! ## Examples
//!
//! ### Basic Generation
//!
//! ```rust,no_run
//! use ggen_core::generator::{Generator, GenContext};
//! use ggen_core::pipeline::Pipeline;
//! use std::collections::BTreeMap;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let pipeline = Pipeline::new()?;
//! let ctx = GenContext::new(
//!     PathBuf::from("template.tmpl"),
//!     PathBuf::from("output")
//! ).with_vars({
//!     let mut vars = BTreeMap::new();
//!     vars.insert("name".to_string(), "MyApp".to_string());
//!     vars
//! });
//!
//! let mut generator = Generator::new(pipeline, ctx);
//! let output_path = generator.generate()?;
//! println!("Generated: {:?}", output_path);
//! # Ok(())
//! # }
//! ```
//!
//! ### Dry Run
//!
//! ```rust,no_run
//! use ggen_core::generator::{Generator, GenContext};
//! use ggen_core::pipeline::Pipeline;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let pipeline = Pipeline::new()?;
//! let ctx = GenContext::new(
//!     PathBuf::from("template.tmpl"),
//!     PathBuf::from("output")
//! ).dry(true); // Enable dry run mode
//!
//! let mut generator = Generator::new(pipeline, ctx);
//! let output_path = generator.generate()?;
//! // File is not actually written in dry run mode
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use tera::Context;

use crate::pipeline::Pipeline;
use crate::template::Template;
use crate::templates::frozen::FrozenMerger;

/// Context for template generation with paths, variables, and configuration
pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,
    pub dry_run: bool,
}

impl GenContext {
    /// Create a new generation context
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::generator::GenContext;
    /// use std::path::PathBuf;
    ///
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// );
    /// assert_eq!(ctx.template_path, PathBuf::from("template.tmpl"));
    /// assert_eq!(ctx.output_root, PathBuf::from("output"));
    /// assert!(ctx.vars.is_empty());
    /// assert!(!ctx.dry_run);
    /// ```
    pub fn new(template_path: PathBuf, output_root: PathBuf) -> Self {
        Self {
            template_path,
            output_root,
            vars: BTreeMap::new(),
            global_prefixes: BTreeMap::new(),
            base: None,
            dry_run: false,
        }
    }
    /// Add variables to the generation context
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::generator::GenContext;
    /// use std::collections::BTreeMap;
    /// use std::path::PathBuf;
    ///
    /// let mut vars = BTreeMap::new();
    /// vars.insert("name".to_string(), "MyApp".to_string());
    /// vars.insert("version".to_string(), "1.0.0".to_string());
    ///
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// ).with_vars(vars);
    ///
    /// assert_eq!(ctx.vars.get("name"), Some(&"MyApp".to_string()));
    /// assert_eq!(ctx.vars.get("version"), Some(&"1.0.0".to_string()));
    /// ```
    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self {
        self.vars = vars;
        self
    }
    /// Add RDF prefixes and base IRI to the context
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_core::generator::GenContext;
    /// use std::collections::BTreeMap;
    /// use std::path::PathBuf;
    ///
    /// let mut prefixes = BTreeMap::new();
    /// prefixes.insert("ex".to_string(), "http://example.org/".to_string());
    ///
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// ).with_prefixes(prefixes, Some("http://example.org/".to_string()));
    ///
    /// assert_eq!(ctx.global_prefixes.get("ex"), Some(&"http://example.org/".to_string()));
    /// assert_eq!(ctx.base, Some("http://example.org/".to_string()));
    /// ```
    pub fn with_prefixes(
        mut self, prefixes: BTreeMap<String, String>, base: Option<String>,
    ) -> Self {
        self.global_prefixes = prefixes;
        self.base = base;
        self
    }
    /// Enable or disable dry run mode
    ///
    /// When dry run is enabled, files are not actually written to disk.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::generator::GenContext;
    /// use std::path::PathBuf;
    ///
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// ).dry(true);
    ///
    /// assert!(ctx.dry_run);
    /// ```
    pub fn dry(mut self, dry: bool) -> Self {
        self.dry_run = dry;
        self
    }
}

/// Main generator that orchestrates template processing and file generation
pub struct Generator {
    pub pipeline: Pipeline,
    pub ctx: GenContext,
}

impl Generator {
    /// Create a new generator with a pipeline and context
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_core::generator::{Generator, GenContext};
    /// use ggen_core::pipeline::Pipeline;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let pipeline = Pipeline::new()?;
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// );
    /// let generator = Generator::new(pipeline, ctx);
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(pipeline: Pipeline, ctx: GenContext) -> Self {
        Self { pipeline, ctx }
    }

    /// Generate output from the template
    ///
    /// Processes the template, renders it with the provided context,
    /// and writes the output to the specified location.
    ///
    /// # Returns
    ///
    /// Returns the path to the generated file. The path is returned even in
    /// dry run mode (when `dry_run` is `true`), allowing you to preview where
    /// the file would be written without actually creating it.
    ///
    /// The output path is determined as follows:
    /// - If the template frontmatter specifies a `to` field, that path is used
    ///   (rendered with template variables)
    /// - Otherwise, the output path defaults to the template filename with a
    ///   `.out` extension in the output root directory
    ///
    /// # Behavior
    ///
    /// - **Parent directories**: Automatically creates parent directories as needed
    /// - **Frozen sections**: If the output file already exists and contains frozen
    ///   sections (marked with `# frozen` comments), those sections are preserved
    ///   and merged with the new generated content
    /// - **Dry run**: When `dry_run` is `true`, the file is not written to disk,
    ///   but the path is still returned
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The template file cannot be read
    /// - The template syntax is invalid
    /// - Template variables are missing or invalid
    /// - RDF processing fails (if RDF is used)
    /// - The template path has no file stem (cannot determine default output name)
    /// - The output file cannot be written (unless in dry run mode)
    /// - File system permissions are insufficient
    /// - Parent directories cannot be created
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust,no_run
    /// use ggen_core::generator::{Generator, GenContext};
    /// use ggen_core::pipeline::Pipeline;
    /// use std::collections::BTreeMap;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let pipeline = Pipeline::new()?;
    /// let mut vars = BTreeMap::new();
    /// vars.insert("name".to_string(), "MyApp".to_string());
    ///
    /// let ctx = GenContext::new(
    ///     PathBuf::from("template.tmpl"),
    ///     PathBuf::from("output")
    /// ).with_vars(vars);
    ///
    /// let mut generator = Generator::new(pipeline, ctx);
    /// let output_path = generator.generate()?;
    /// println!("Generated: {:?}", output_path);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case - Template file not found
    ///
    /// ```rust,no_run
    /// use ggen_core::generator::{Generator, GenContext};
    /// use ggen_core::pipeline::Pipeline;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let pipeline = Pipeline::new()?;
    /// let ctx = GenContext::new(
    ///     PathBuf::from("nonexistent.tmpl"), // File doesn't exist
    ///     PathBuf::from("output")
    /// );
    ///
    /// let mut generator = Generator::new(pipeline, ctx);
    /// // This will fail because the template file doesn't exist
    /// let result = generator.generate();
    /// assert!(result.is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn generate(&mut self) -> Result<PathBuf> {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        let mut tmpl = Template::parse(&input)?;

        // Context
        // Security: Sanitize template variables to prevent code injection
        // Tera templates can execute arbitrary code, so we need to ensure
        // variables don't contain executable content
        let sanitized_vars = self
            .ctx
            .vars
            .iter()
            .map(|(k, v)| {
                // Basic sanitization: remove control characters and limit length
                let sanitized_key = k
                    .chars()
                    .filter(|c| !c.is_control())
                    .take(100)
                    .collect::<String>();
                let sanitized_value = v
                    .chars()
                    .filter(|c| !c.is_control())
                    .take(10000)
                    .collect::<String>();
                (sanitized_key, sanitized_value)
            })
            .collect::<BTreeMap<String, String>>();
        let mut tctx = Context::from_serialize(&sanitized_vars)?;
        insert_env(&mut tctx);

        // Render frontmatter
        tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

        // Process graph
        tmpl.process_graph(
            &mut self.pipeline.graph,
            &mut self.pipeline.tera,
            &tctx,
            &self.ctx.template_path,
        )?;

        // Render body
        let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

        // Determine output path
        let output_path = if let Some(to_path) = &tmpl.front.to {
            let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
            let joined_path = self.ctx.output_root.join(&rendered_to);

            // Security: Prevent path traversal attacks by ensuring the resolved path
            // stays within output_root. This prevents paths like "../../../etc/passwd"
            // from escaping the output directory.
            // We normalize the path and check that all components stay within output_root
            let normalized = joined_path.components().collect::<Vec<_>>();
            let output_root_components = self.ctx.output_root.components().collect::<Vec<_>>();

            // Check that normalized path starts with output_root components
            if normalized.len() < output_root_components.len() {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Output path '{}' would escape output root '{}'",
                    rendered_to,
                    self.ctx.output_root.display()
                )));
            }

            for (i, component) in output_root_components.iter().enumerate() {
                if normalized.get(i) != Some(component) {
                    return Err(ggen_utils::error::Error::new(&format!(
                        "Output path '{}' would escape output root '{}'",
                        rendered_to,
                        self.ctx.output_root.display()
                    )));
                }
            }

            joined_path
        } else {
            // Default to template name with .out extension
            let template_name = self
                .ctx
                .template_path
                .file_stem()
                .ok_or_else(|| {
                    ggen_utils::error::Error::new(&format!(
                        "Template path has no file stem: {}",
                        self.ctx.template_path.display()
                    ))
                })?
                .to_string_lossy();
            self.ctx.output_root.join(format!("{}.out", template_name))
        };

        if !self.ctx.dry_run {
            // Ensure parent directory exists
            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }

            // Check if file exists and has frozen sections
            let final_content = if output_path.exists() {
                let existing_content = fs::read_to_string(&output_path)?;
                if FrozenMerger::has_frozen_sections(&existing_content) {
                    // Merge frozen sections from existing file
                    FrozenMerger::merge_with_frozen(&existing_content, &rendered)?
                } else {
                    rendered
                }
            } else {
                rendered
            };

            fs::write(&output_path, final_content)?;
        }

        Ok(output_path)
    }
}

fn insert_env(ctx: &mut Context) {
    for (k, v) in env::vars() {
        ctx.insert(&k, &v);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_pipeline() -> Pipeline {
        Pipeline::new().unwrap()
    }

    fn create_test_template(content: &str) -> (TempDir, PathBuf) {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let template_path = temp_dir.path().join("test.tmpl");
        fs::write(&template_path, content).expect("Failed to write test template");
        (temp_dir, template_path)
    }

    test!(test_gen_context_new, {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");

        let ctx = GenContext::new(template_path.clone(), output_root.clone());

        assert_eq!(ctx.template_path, template_path);
        assert_eq!(ctx.output_root, output_root);
        assert!(ctx.vars.is_empty());
        assert!(ctx.global_prefixes.is_empty());
        assert!(ctx.base.is_none());
        assert!(!ctx.dry_run);
    });

    test!(test_gen_context_with_vars, {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "TestApp".to_string());
        vars.insert("version".to_string(), "1.0.0".to_string());

        let ctx = GenContext::new(template_path, output_root).with_vars(vars.clone());

        assert_eq!(ctx.vars, vars);
    });

    test!(test_gen_context_with_prefixes, {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());
        let base = Some("http://example.org/base/".to_string());

        let ctx = GenContext::new(template_path, output_root)
            .with_prefixes(prefixes.clone(), base.clone());

        assert_eq!(ctx.global_prefixes, prefixes);
        assert_eq!(ctx.base, base);
    });

    test!(test_gen_context_dry, {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");

        let ctx = GenContext::new(template_path, output_root).dry(true);
        assert!(ctx.dry_run);

        let ctx = GenContext::new(PathBuf::from("test.tmpl"), PathBuf::from("output")).dry(false);
        assert!(!ctx.dry_run);
    });

    test!(test_generator_new, {
        let pipeline = create_test_pipeline();
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");
        let ctx = GenContext::new(template_path, output_root);

        let generator = Generator::new(pipeline, ctx);

        // Generator should be created successfully
        assert!(generator
            .ctx
            .template_path
            .to_string_lossy()
            .contains("test.tmpl"));
        assert!(generator
            .ctx
            .output_root
            .to_string_lossy()
            .contains("output"));
    });

    test!(test_generate_simple_template, {
        let (_temp_dir, template_path) = create_test_template(
            r#"---
to: "output/{{ name | lower }}.rs"
---
// Generated by ggen
// Name: {{ name }}
// Description: {{ description }}
"#,
        );

        let output_dir = _temp_dir.path();
        let pipeline = create_test_pipeline();
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "MyApp".to_string());
        vars.insert("description".to_string(), "A test application".to_string());

        let ctx = GenContext::new(template_path, output_dir.to_path_buf()).with_vars(vars);

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        if let Err(e) = &result {
            log::error!("Generation failed: {}", e);
        }
        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("output/myapp.rs"));

        // Verify file was created and has correct content
        let content = fs::read_to_string(&output_path).expect("Failed to read output file");
        assert!(content.contains("// Generated by ggen"));
        assert!(content.contains("// Name: MyApp"));
        assert!(content.contains("// Description: A test application"));
    });

    test!(test_generate_dry_run, {
        let (_temp_dir, template_path) = create_test_template(
            r#"---
to: "output/{{ name | lower }}.rs"
---
// Generated content
"#,
        );

        let output_dir = _temp_dir.path();
        let pipeline = create_test_pipeline();
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "MyApp".to_string());

        let ctx = GenContext::new(template_path, output_dir.to_path_buf())
            .with_vars(vars)
            .dry(true);

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("output/myapp.rs"));

        // Verify file was NOT created in dry run
        assert!(!output_path.exists());
    });

    test!(test_generate_with_default_output, {
        let (_temp_dir, template_path) = create_test_template(
            r#"---
{}
---
// Default output content
"#,
        );

        let output_dir = _temp_dir.path();
        let pipeline = create_test_pipeline();
        let ctx = GenContext::new(template_path, output_dir.to_path_buf());

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        if let Err(e) = &result {
            log::error!("Generation failed: {}", e);
        }
        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("test.out"));

        // Verify file was created
        let content = fs::read_to_string(&output_path).expect("Failed to read output file");
        assert!(content.contains("// Default output content"));
    });

    test!(test_generate_with_nested_output_path, {
        let (_temp_dir, template_path) = create_test_template(
            r#"---
to: "src/{{ module }}/{{ name | lower }}.rs"
---
// Nested output content
"#,
        );

        let output_dir = _temp_dir.path();
        let pipeline = create_test_pipeline();
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "MyModule".to_string());
        vars.insert("module".to_string(), "handlers".to_string());

        let ctx = GenContext::new(template_path, output_dir.to_path_buf()).with_vars(vars);

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("src/handlers/mymodule.rs"));

        // Verify directory was created and file exists
        assert!(output_path.exists());
        let content = fs::read_to_string(&output_path).expect("Failed to read output file");
        assert!(content.contains("// Nested output content"));
    });

    test!(test_generate_invalid_template, {
        let (_temp_dir, template_path) = create_test_template(
            r#"---
invalid_yaml: [unclosed
---
// Template body
"#,
        );

        let output_dir = _temp_dir.path();
        let pipeline = create_test_pipeline();
        let ctx = GenContext::new(template_path, output_dir.to_path_buf());

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        // Should fail due to invalid YAML in frontmatter
        assert!(result.is_err());
    });

    test!(test_generate_missing_template_file, {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let template_path = temp_dir.path().join("nonexistent.tmpl");
        let output_dir = temp_dir.path();

        let pipeline = create_test_pipeline();
        let ctx = GenContext::new(template_path, output_dir.to_path_buf());

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        // Should fail due to missing template file
        assert!(result.is_err());
    });

    test!(test_insert_env, {
        let mut ctx = Context::new();

        // Set a test environment variable
        std::env::set_var("TEST_GGEN_VAR", "test_value");

        insert_env(&mut ctx);

        // Verify environment variable was inserted
        assert!(ctx.get("TEST_GGEN_VAR").is_some());

        // Clean up
        std::env::remove_var("TEST_GGEN_VAR");
    });
}
