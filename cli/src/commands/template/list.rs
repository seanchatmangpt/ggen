//! Template list command - CLI layer

use clap::Args;
use ggen_utils::error::Result;
use std::path::PathBuf;

use crate::domain::template::{list_templates, ListFilters, TemplateSource};

#[derive(Debug, Args)]
pub struct ListCommand {
    /// Filter by pattern (glob)
    #[arg(long)]
    pub pattern: Option<String>,

    /// Show only local templates
    #[arg(long)]
    pub local: bool,

    /// Show only gpack templates
    #[arg(long)]
    pub gpack: bool,

    /// Templates directory (defaults to ./templates)
    #[arg(long, default_value = "templates")]
    pub templates_dir: PathBuf,
}

impl ListCommand {
    pub fn execute(&self) -> Result<()> {
        crate::runtime::execute(async {
            // Validate pattern input
            validate_pattern(&self.pattern)?;

            println!("📄 Listing templates...");

            let filters = ListFilters {
                pattern: self.pattern.clone(),
                local_only: self.local,
                gpack_only: self.gpack,
            };

            let templates = list_templates(&self.templates_dir, &filters)
                .map_err(|e| e.to_string())?;

            if templates.is_empty() {
                println!("ℹ️  No templates found");
                return Ok(());
            }

            println!("📄 Available Templates:");
            for template in templates {
                match template.source {
                    TemplateSource::Local => {
                        println!("  📄 {} (local)", template.name);
                    }
                    TemplateSource::Gpack(ref gpack_id) => {
                        println!("  📦 {} ({})", template.name, gpack_id);
                    }
                }
                if let Some(desc) = template.description {
                    println!("     {}", desc);
                }
            }

            Ok(())
        })
    }
}

/// Validate and sanitize pattern input
fn validate_pattern(pattern: &Option<String>) -> std::result::Result<(), String> {
    if let Some(pattern) = pattern {
        if pattern.trim().is_empty() {
            return Err("Pattern cannot be empty".to_string());
        }

        if pattern.len() > 200 {
            return Err("Pattern too long (max 200 characters)".to_string());
        }

        if pattern.contains("..") {
            return Err("Path traversal detected: pattern cannot contain '..'".to_string());
        }

        if !pattern.chars().all(|c| {
            c.is_alphanumeric()
                || c == '.'
                || c == '*'
                || c == '?'
                || c == '['
                || c == ']'
                || c == '-'
                || c == '_'
        }) {
            return Err("Invalid pattern format: only alphanumeric characters, dots, wildcards, brackets, dashes, and underscores allowed".to_string());
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_pattern_valid() {
        assert!(validate_pattern(&Some("rust*".to_string())).is_ok());
        assert!(validate_pattern(&Some("test_[0-9]".to_string())).is_ok());
        assert!(validate_pattern(&None).is_ok());
    }

    #[test]
    fn test_validate_pattern_invalid() {
        assert!(validate_pattern(&Some("".to_string())).is_err());
        assert!(validate_pattern(&Some("a".repeat(201))).is_err());
        assert!(validate_pattern(&Some("../etc".to_string())).is_err());
        assert!(validate_pattern(&Some("test@#$".to_string())).is_err());
    }
}
