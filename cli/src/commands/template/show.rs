//! Template show command - CLI layer

use clap::Args;
use ggen_utils::error::Result;

use crate::domain::template::show::show_template_metadata;

#[derive(Debug, Args)]
pub struct ShowCommand {
    /// Template reference (local path or gpack:template)
    pub template_ref: String,
}

impl ShowCommand {
    pub async fn execute(&self) -> Result<()> {
        // Validate input
        validate_template_ref(&self.template_ref)?;

        println!("📄 Template Information:");

        let metadata = show_template_metadata(&self.template_ref)?;

        // Display metadata
        println!("  Name: {}", metadata.name);
        println!("  Path: {}", metadata.path);

        if let Some(desc) = metadata.description {
            println!("  Description: {}", desc);
        }

        if let Some(output) = metadata.output_path {
            println!("  Output: {}", output);
        }

        if !metadata.variables.is_empty() {
            println!("  Variables:");
            for var in metadata.variables {
                println!("    - {}", var);
            }
        }

        if !metadata.rdf_sources.is_empty() {
            println!("  RDF Sources:");
            for source in metadata.rdf_sources {
                println!("    - {}", source);
            }
        }

        if !metadata.sparql_queries.is_empty() {
            println!("  SPARQL Queries:");
            for (name, _) in metadata.sparql_queries {
                println!("    - {}", name);
            }
        }

        if let Some(seed) = metadata.determinism_seed {
            println!("  Determinism Seed: {}", seed);
        }

        Ok(())
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
