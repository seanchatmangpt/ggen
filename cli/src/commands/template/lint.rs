//! Template lint command - CLI layer

use clap::Args;
use ggen_utils::error::Result;

use crate::domain::template::lint::{lint_template, LintOptions};

#[derive(Debug, Args)]
pub struct LintCommand {
    /// Template reference (local path or gpack:template)
    pub template_ref: String,

    /// Check SPARQL query syntax
    #[arg(long)]
    pub sparql: bool,

    /// Validate against RDF schema
    #[arg(long)]
    pub schema: bool,
}

impl LintCommand {
    pub async fn execute(&self) -> Result<()> {
        // Validate input
        validate_template_ref(&self.template_ref)?;

        println!("🔍 Linting template...");

        let options = LintOptions {
            check_sparql: self.sparql,
            check_schema: self.schema,
        };

        let report = lint_template(&self.template_ref, &options)?;

        if !report.errors.is_empty() {
            println!("❌ Errors:");
            for error in &report.errors {
                if let Some(line) = error.line {
                    println!("  Line {}: {}", line, error.message);
                } else {
                    println!("  {}", error.message);
                }
            }
        }

        if !report.warnings.is_empty() {
            println!("⚠️  Warnings:");
            for warning in &report.warnings {
                if let Some(line) = warning.line {
                    println!("  Line {}: {}", line, warning.message);
                } else {
                    println!("  {}", warning.message);
                }
            }
        }

        if !report.has_errors() && !report.has_warnings() {
            println!("✅ No issues found");
        }

        if report.has_errors() {
            Err(ggen_utils::error::Error::new("Template has errors"))
        } else {
            Ok(())
        }
    }
}

/// Validate and sanitize template reference input
fn validate_template_ref(template_ref: &str) -> Result<()> {
    if template_ref.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    if template_ref.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }

    if template_ref.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: template reference cannot contain '..'",
        ));
    }

    if !template_ref
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == ':' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template reference format: only alphanumeric characters, dots, slashes, colons, dashes, and underscores allowed",
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_template_ref_valid() {
        assert!(validate_template_ref("hello.tmpl").is_ok());
        assert!(validate_template_ref("path/to/template.tmpl").is_ok());
        assert!(validate_template_ref("gpack:my-template").is_ok());
    }

    #[test]
    fn test_validate_template_ref_invalid() {
        assert!(validate_template_ref("").is_err());
        assert!(validate_template_ref(&"a".repeat(501)).is_err());
        assert!(validate_template_ref("../etc/passwd").is_err());
        assert!(validate_template_ref("bad@chars#").is_err());
    }
}
