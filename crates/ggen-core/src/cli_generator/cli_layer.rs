//! CLI layer generator for 2026 best practices
//!
//! This module generates the CLI layer using clap-noun-verb v3.3.0
//! with domain function references.

use crate::cli_generator::types::{CliProject, Noun, Verb};
use anyhow::{Context as _, Result};
use std::path::Path;
use tera::{Context, Tera};

/// CLI layer generator
pub struct CliLayerGenerator {
    tera: Tera,
}

impl CliLayerGenerator {
    /// Create a new CLI layer generator
    pub fn new(template_dir: &Path) -> Result<Self> {
        let pattern = format!("{}/**/*.tmpl", template_dir.display());
        let tera = Tera::new(&pattern).with_context(|| {
            format!("Failed to load templates from: {}", template_dir.display())
        })?;

        Ok(Self { tera })
    }

    /// Generate CLI layer
    ///
    /// Creates:
    /// - CLI crate Cargo.toml
    /// - CLI crate main.rs, lib.rs, runtime.rs
    /// - CLI commands (cmds/mod.rs, cmds/{noun}/mod.rs, cmds/{noun}/{verb}.rs)
    pub fn generate(&self, project: &CliProject, output_dir: &Path) -> Result<()> {
        let cli_crate = project
            .cli_crate
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("CLI crate name is required but was not provided"))?;
        let core_crate = project
            .domain_crate
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Domain crate name is required but was not provided"))?;
        let cli_dir = output_dir.join("crates").join(cli_crate);
        let cli_src = cli_dir.join("src");

        std::fs::create_dir_all(&cli_src).context("Failed to create CLI src directory")?;

        let mut context = Context::new();
        context.insert("project_name", &project.name);
        context.insert("cli_crate", cli_crate);
        context.insert("core_crate", core_crate);
        context.insert("version", &project.version);
        context.insert("edition", &project.edition);
        context.insert("license", &project.license);
        context.insert("authors", &project.authors);
        context.insert(
            "nouns",
            &project.nouns.iter().map(|n| &n.name).collect::<Vec<_>>(),
        );

        // Generate CLI crate Cargo.toml
        self.render_template(
            "cli/cli-crate/Cargo.toml.tmpl",
            &context,
            &cli_dir.join("Cargo.toml"),
        )?;

        // Generate main.rs
        self.render_template(
            "cli/cli-crate/src/main.rs.tmpl",
            &context,
            &cli_src.join("main.rs"),
        )?;

        // Generate lib.rs
        self.render_template(
            "cli/cli-crate/src/lib.rs.tmpl",
            &context,
            &cli_src.join("lib.rs"),
        )?;

        // Generate runtime.rs
        self.render_template(
            "cli/cli-crate/src/runtime.rs.tmpl",
            &context,
            &cli_src.join("runtime.rs"),
        )?;

        // Generate cmds/mod.rs
        let cmds_dir = cli_src.join("cmds");
        std::fs::create_dir_all(&cmds_dir)?;
        self.render_template(
            "cli/cli-crate/src/cmds/mod.rs.tmpl",
            &context,
            &cmds_dir.join("mod.rs"),
        )?;

        // Generate noun modules
        for noun in &project.nouns {
            self.generate_noun(noun, project, &cmds_dir, &context)?;
        }

        Ok(())
    }

    fn generate_noun(
        &self, noun: &Noun, project: &CliProject, cmds_dir: &Path, base_context: &Context,
    ) -> Result<()> {
        let noun_dir = cmds_dir.join(&noun.name);
        std::fs::create_dir_all(&noun_dir)?;

        let mut context = base_context.clone();
        context.insert("noun", &noun.name);
        context.insert(
            "verbs",
            &noun.verbs.iter().map(|v| &v.name).collect::<Vec<_>>(),
        );

        // Generate noun/mod.rs
        self.render_template(
            "cli/cli-crate/src/cmds/noun/mod.rs.tmpl",
            &context,
            &noun_dir.join("mod.rs"),
        )?;

        // Generate verb files
        for verb in &noun.verbs {
            self.generate_verb(verb, noun, project, &noun_dir, &context)?;
        }

        Ok(())
    }

    fn generate_verb(
        &self, verb: &Verb, noun: &Noun, project: &CliProject, noun_dir: &Path,
        base_context: &Context,
    ) -> Result<()> {
        let mut context = base_context.clone();
        context.insert("verb", &verb.name);
        context.insert("noun", &noun.name);

        // Extract domain function path
        let core_crate_name = project
            .domain_crate
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("Domain crate name is required but was not provided"))?;
        let domain_function = verb.domain_function.as_ref().cloned().unwrap_or_else(|| {
            // Default: core_crate::noun::verb
            let core_crate = core_crate_name.replace("-", "_");
            format!("{}::{}::{}", core_crate, noun.name, verb.name)
        });
        context.insert("domain_function", &domain_function);
        context.insert("core_crate", core_crate_name);

        self.render_template(
            "cli/cli-crate/src/cmds/noun/verb.rs.tmpl",
            &context,
            &noun_dir.join(format!("{}.rs", verb.name)),
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
