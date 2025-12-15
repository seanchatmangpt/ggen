//! Template rendering for CLI project generation
//!
//! This module provides template rendering functionality for generating CLI projects
//! from RDF-based project definitions. It uses Tera templates to render project files
//! including Cargo.toml, main.rs, command.rs, and lib.rs.
//!
//! ## Features
//!
//! - **Template Loading**: Load templates from directory with glob support
//! - **Context Building**: Build Tera context from CliProject structure
//! - **File Rendering**: Render individual templates or entire project structure
//! - **Output Management**: Write rendered files to output directory
//!
//! ## Examples
//!
//! ### Rendering a Project
//!
//! ```rust,ignore
//! use ggen_ai::rdf::template::TemplateRenderer;
//! use ggen_ai::rdf::types::CliProject;
//! use std::path::Path;
//!
//! let template_dir = Path::new("templates");
//! let renderer = TemplateRenderer::new(template_dir)?;
//!
//! let project = CliProject::default(); // Load from RDF
//! let output_dir = Path::new("output");
//!
//! renderer.render_all(&project, output_dir)?;
//! ```

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};
use tera::{Context, Tera};

use crate::rdf::types::CliProject;

pub struct TemplateRenderer {
    tera: Tera,
}

impl TemplateRenderer {
    pub fn new(template_dir: &Path) -> Result<Self> {
        let pattern = format!("{}/**/*.tmpl", template_dir.display());
        let tera = Tera::new(&pattern)?;

        Ok(Self { tera })
    }

    pub fn build_context(&self, project: &CliProject) -> Context {
        let mut context = Context::new();
        context.insert("project", project);
        context
    }

    pub fn render_file(&self, template: &str, context: &Context) -> Result<String> {
        Ok(self.tera.render(template, context)?)
    }

    pub fn render_all(&self, project: &CliProject, output_dir: &Path) -> Result<()> {
        let context = self.build_context(project);

        // Render Cargo.toml
        self.render_and_write("Cargo.toml.tmpl", &context, output_dir.join("Cargo.toml"))?;

        // Render main.rs
        std::fs::create_dir_all(output_dir.join("src"))?;
        self.render_and_write("main.rs.tmpl", &context, output_dir.join("src/main.rs"))?;

        // Render command.rs
        self.render_and_write(
            "command.rs.tmpl",
            &context,
            output_dir.join("src/command.rs"),
        )?;

        // Render lib.rs
        self.render_and_write("lib.rs.tmpl", &context, output_dir.join("src/lib.rs"))?;

        // Render cmds/mod.rs
        std::fs::create_dir_all(output_dir.join("src/cmds"))?;
        self.render_and_write(
            "cmds/mod.rs.tmpl",
            &context,
            output_dir.join("src/cmds/mod.rs"),
        )?;

        // Render each noun module
        for noun in &project.nouns {
            let noun_dir = output_dir.join("src/cmds").join(&noun.name);
            std::fs::create_dir_all(&noun_dir)?;

            // Render noun/mod.rs
            let mut noun_context = context.clone();
            noun_context.insert("noun", noun);
            self.render_and_write(
                "cmds/noun_mod.rs.tmpl",
                &noun_context,
                noun_dir.join("mod.rs"),
            )?;

            // Render each verb
            for verb in &noun.verbs {
                let mut verb_context = noun_context.clone();
                verb_context.insert("verb", verb);
                self.render_and_write(
                    "cmds/verb.rs.tmpl",
                    &verb_context,
                    noun_dir.join(format!("{}.rs", verb.name)),
                )?;
            }
        }

        // Render tests
        std::fs::create_dir_all(output_dir.join("tests"))?;
        self.render_and_write(
            "tests/integration.rs.tmpl",
            &context,
            output_dir.join("tests/integration_test.rs"),
        )?;

        // Render README
        self.render_and_write("README.md.tmpl", &context, output_dir.join("README.md"))?;

        // Copy .gitignore
        self.render_and_write("gitignore.tmpl", &context, output_dir.join(".gitignore"))?;

        Ok(())
    }

    fn render_and_write(
        &self, template: &str, context: &Context, output_path: PathBuf,
    ) -> Result<()> {
        let content = self.render_file(template, context)?;
        std::fs::write(output_path, content)?;
        Ok(())
    }
}
