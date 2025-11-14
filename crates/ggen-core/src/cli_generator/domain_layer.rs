//! Domain layer generator for 2026 best practices
//!
//! This module generates the domain layer with function stubs
//! that the CLI layer can reference.

use crate::cli_generator::types::{CliProject, Noun, Verb};
use anyhow::{Context as _, Result};
use std::path::Path;
use tera::{Context, Tera};

/// Domain layer generator
pub struct DomainLayerGenerator {
    tera: Tera,
}

impl DomainLayerGenerator {
    /// Create a new domain layer generator
    pub fn new(template_dir: &Path) -> Result<Self> {
        let pattern = format!("{}/**/*.tmpl", template_dir.display());
        let tera = Tera::new(&pattern).with_context(|| {
            format!("Failed to load templates from: {}", template_dir.display())
        })?;

        Ok(Self { tera })
    }

    /// Generate domain layer
    ///
    /// Creates:
    /// - Domain crate Cargo.toml
    /// - Domain crate lib.rs
    /// - Domain modules ({domain}/mod.rs, {domain}/{verb}.rs)
    pub fn generate(&self, project: &CliProject, output_dir: &Path) -> Result<()> {
        let core_crate = project.domain_crate.as_ref().ok_or_else(|| {
            anyhow::anyhow!("domain_crate is required for domain layer generation")
        })?;
        let core_dir = output_dir.join("crates").join(core_crate);
        let core_src = core_dir.join("src");

        std::fs::create_dir_all(&core_src).context("Failed to create domain src directory")?;

        let mut context = Context::new();
        context.insert("project_name", &project.name);
        context.insert("core_crate", core_crate);
        context.insert("version", &project.version);
        context.insert("edition", &project.edition);
        context.insert("license", &project.license);
        context.insert("authors", &project.authors);

        // Generate domain crate Cargo.toml
        self.render_template(
            "cli/core-crate/Cargo.toml.tmpl",
            &context,
            &core_dir.join("Cargo.toml"),
        )?;

        // Generate lib.rs
        // First, determine domain modules from nouns
        let domain_modules: Vec<String> = project.nouns.iter().map(|n| n.name.clone()).collect();
        context.insert("domain_modules", &domain_modules);

        // For now, use first noun as domain module in lib.rs template
        if let Some(first_noun) = project.nouns.first() {
            context.insert("domain_module", &first_noun.name);
        }

        self.render_template(
            "cli/core-crate/src/lib.rs.tmpl",
            &context,
            &core_src.join("lib.rs"),
        )?;

        // Generate domain modules for each noun
        for noun in &project.nouns {
            self.generate_domain_module(noun, project, &core_src, &context)?;
        }

        Ok(())
    }

    fn generate_domain_module(
        &self, noun: &Noun, project: &CliProject, core_src: &Path, base_context: &Context,
    ) -> Result<()> {
        let domain_dir = core_src.join(&noun.name);
        std::fs::create_dir_all(&domain_dir)?;

        let mut context = base_context.clone();
        context.insert("domain_module", &noun.name);

        // Generate domain module mod.rs
        if let Some(first_verb) = noun.verbs.first() {
            context.insert("verb", &first_verb.name);
            self.render_template(
                "cli/core-crate/src/domain/mod.rs.tmpl",
                &context,
                &domain_dir.join("mod.rs"),
            )?;
        }

        // Generate verb functions
        for verb in &noun.verbs {
            self.generate_verb_function(verb, noun, project, &domain_dir, &context)?;
        }

        Ok(())
    }

    fn generate_verb_function(
        &self, verb: &Verb, noun: &Noun, _project: &CliProject, domain_dir: &Path,
        base_context: &Context,
    ) -> Result<()> {
        let mut context = base_context.clone();
        context.insert("verb", &verb.name);
        context.insert("domain_module", &noun.name);

        self.render_template(
            "cli/core-crate/src/domain/verb.rs.tmpl",
            &context,
            &domain_dir.join(format!("{}.rs", verb.name)),
        )?;

        Ok(())
    }

    fn render_template(&self, template: &str, context: &Context, output: &Path) -> Result<()> {
        let content = self
            .tera
            .render(template, context)
            .with_context(|| format!("Failed to render template: {}", template))?;

        if let Some(parent) = output.parent() {
            std::fs::create_dir_all(parent)?;
        }

        std::fs::write(output, content)
            .with_context(|| format!("Failed to write: {}", output.display()))?;

        Ok(())
    }
}
