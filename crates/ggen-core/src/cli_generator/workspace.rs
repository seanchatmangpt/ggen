//! Workspace generator for 2026 CLI projects
//!
//! This module generates the workspace structure with separate CLI and domain crates.

use anyhow::{Context as _, Result};
use crate::cli_generator::types::CliProject;
use std::path::{Path, PathBuf};
use tera::{Context, Tera};

/// Workspace generator for creating workspace structure
pub struct WorkspaceGenerator {
    tera: Tera,
}

impl WorkspaceGenerator {
    /// Create a new workspace generator
    pub fn new(template_dir: &Path) -> Result<Self> {
        let pattern = format!("{}/**/*.tmpl", template_dir.display());
        let tera = Tera::new(&pattern).with_context(|| {
            format!("Failed to load templates from: {}", template_dir.display())
        })?;
        
        Ok(Self { tera })
    }
    
    /// Generate workspace structure
    ///
    /// Creates:
    /// - Workspace root Cargo.toml
    /// - CLI crate directory structure
    /// - Domain crate directory structure
    pub fn generate(&self, project: &CliProject, output_dir: &Path) -> Result<()> {
        let mut context = Context::new();
        context.insert("project_name", &project.name);
        context.insert("cli_crate", &project.cli_crate.as_ref().unwrap());
        context.insert("core_crate", &project.domain_crate.as_ref().unwrap());
        context.insert("version", &project.version);
        context.insert("edition", &project.edition);
        context.insert("license", &project.license);
        context.insert("authors", &project.authors);
        context.insert("resolver", &project.resolver);
        context.insert("project", project);
        
        // Generate workspace root Cargo.toml
        let workspace_cargo = output_dir.join("Cargo.toml");
        self.render_template("cli/workspace/Cargo.toml.tmpl", &context, &workspace_cargo)
            .context("Failed to generate workspace Cargo.toml")?;
        
        // Create crates directory
        let crates_dir = output_dir.join("crates");
        std::fs::create_dir_all(&crates_dir)
            .context("Failed to create crates directory")?;
        
        Ok(())
    }
    
    fn render_template(&self, template: &str, context: &Context, output: &Path) -> Result<()> {
        let content = self.tera.render(template, context)
            .with_context(|| format!("Failed to render template: {}", template))?;
        
        // Create parent directory if needed
        if let Some(parent) = output.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
        }
        
        std::fs::write(output, content)
            .with_context(|| format!("Failed to write file: {}", output.display()))?;
        
        Ok(())
    }
}

