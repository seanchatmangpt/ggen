use anyhow::Result;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use tera::Context;

use crate::pipeline::Pipeline;
use crate::template::Template;

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
    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self {
        self.vars = vars;
        self
    }
    pub fn with_prefixes(
        mut self, prefixes: BTreeMap<String, String>, base: Option<String>,
    ) -> Self {
        self.global_prefixes = prefixes;
        self.base = base;
        self
    }
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
    pub fn new(pipeline: Pipeline, ctx: GenContext) -> Self {
        Self { pipeline, ctx }
    }

    pub fn generate(&mut self) -> Result<PathBuf> {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        let mut tmpl = Template::parse(&input)?;

        // Context
        let mut tctx = Context::from_serialize(&self.ctx.vars)?;
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
            self.ctx.output_root.join(rendered_to)
        } else {
            // Default to template name with .out extension
            let template_name = self
                .ctx
                .template_path
                .file_stem()
                .ok_or_else(|| anyhow::anyhow!("Template path has no file stem: {}", self.ctx.template_path.display()))?
                .to_string_lossy();
            self.ctx.output_root.join(format!("{}.out", template_name))
        };

        if !self.ctx.dry_run {
            // Ensure parent directory exists
            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&output_path, rendered)?;
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

    #[test]
    fn test_gen_context_new() {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");

        let ctx = GenContext::new(template_path.clone(), output_root.clone());

        assert_eq!(ctx.template_path, template_path);
        assert_eq!(ctx.output_root, output_root);
        assert!(ctx.vars.is_empty());
        assert!(ctx.global_prefixes.is_empty());
        assert!(ctx.base.is_none());
        assert!(!ctx.dry_run);
    }

    #[test]
    fn test_gen_context_with_vars() {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");
        let mut vars = BTreeMap::new();
        vars.insert("name".to_string(), "TestApp".to_string());
        vars.insert("version".to_string(), "1.0.0".to_string());

        let ctx = GenContext::new(template_path, output_root).with_vars(vars.clone());

        assert_eq!(ctx.vars, vars);
    }

    #[test]
    fn test_gen_context_with_prefixes() {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");
        let mut prefixes = BTreeMap::new();
        prefixes.insert("ex".to_string(), "http://example.org/".to_string());
        let base = Some("http://example.org/base/".to_string());

        let ctx = GenContext::new(template_path, output_root)
            .with_prefixes(prefixes.clone(), base.clone());

        assert_eq!(ctx.global_prefixes, prefixes);
        assert_eq!(ctx.base, base);
    }

    #[test]
    fn test_gen_context_dry() {
        let template_path = PathBuf::from("test.tmpl");
        let output_root = PathBuf::from("output");

        let ctx = GenContext::new(template_path, output_root).dry(true);
        assert!(ctx.dry_run);

        let ctx = GenContext::new(PathBuf::from("test.tmpl"), PathBuf::from("output")).dry(false);
        assert!(!ctx.dry_run);
    }

    #[test]
    fn test_generator_new() {
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
    }

    #[test]
    fn test_generate_simple_template() {
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
            eprintln!("Generation failed: {}", e);
        }
        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("output/myapp.rs"));

        // Verify file was created and has correct content
        let content = fs::read_to_string(&output_path).expect("Failed to read output file");
        assert!(content.contains("// Generated by ggen"));
        assert!(content.contains("// Name: MyApp"));
        assert!(content.contains("// Description: A test application"));
    }

    #[test]
    fn test_generate_dry_run() {
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
    }

    #[test]
    fn test_generate_with_default_output() {
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
            eprintln!("Generation failed: {}", e);
        }
        assert!(result.is_ok());
        let output_path = result.unwrap();
        assert_eq!(output_path, output_dir.join("test.out"));

        // Verify file was created
        let content = fs::read_to_string(&output_path).expect("Failed to read output file");
        assert!(content.contains("// Default output content"));
    }

    #[test]
    fn test_generate_with_nested_output_path() {
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
    }

    #[test]
    fn test_generate_invalid_template() {
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
    }

    #[test]
    fn test_generate_missing_template_file() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let template_path = temp_dir.path().join("nonexistent.tmpl");
        let output_dir = temp_dir.path();

        let pipeline = create_test_pipeline();
        let ctx = GenContext::new(template_path, output_dir.to_path_buf());

        let mut generator = Generator::new(pipeline, ctx);
        let result = generator.generate();

        // Should fail due to missing template file
        assert!(result.is_err());
    }

    #[test]
    fn test_insert_env() {
        let mut ctx = Context::new();

        // Set a test environment variable
        std::env::set_var("TEST_GGEN_VAR", "test_value");

        insert_env(&mut ctx);

        // Verify environment variable was inserted
        assert!(ctx.get("TEST_GGEN_VAR").is_some());

        // Clean up
        std::env::remove_var("TEST_GGEN_VAR");
    }
}
