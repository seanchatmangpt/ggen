//! Template new command - CLI layer

use clap::Args;
use ggen_utils::error::Result;
use std::path::PathBuf;

use crate::domain::template::{generate_template_content, TemplateService};

#[derive(Debug, Args)]
pub struct NewCommand {
    /// Template name
    pub name: String,

    /// Template type (rust, python, typescript, etc.)
    #[arg(long)]
    pub template_type: Option<String>,

    /// Interactive wizard mode
    #[arg(long)]
    pub interactive: bool,

    /// Templates directory (defaults to ./templates)
    #[arg(long, default_value = "templates")]
    pub templates_dir: PathBuf,
}

impl NewCommand {
    pub async fn execute(&self) -> Result<()> {
        // Validate input
        validate_template_input(&self.name, &self.template_type)?;

        println!("🔧 Creating new template...");

        // Create service
        let service = TemplateService::new(self.templates_dir.clone());

        // Determine template type
        let template_type = self.template_type.as_deref().unwrap_or("generic");

        // Generate template content
        let template_content = generate_template_content(&self.name, template_type)?;

        // Write template
        let template_path = service.write_template(&self.name, &template_content)?;

        println!(
            "✅ Created template '{}' at {}",
            self.name,
            template_path.display()
        );
        println!("📝 Template type: {}", template_type);

        if self.interactive {
            println!("💡 Tip: Edit the template file to customize variables and content");
        }

        Ok(())
    }
}

/// Validate and sanitize template input
fn validate_template_input(name: &str, template_type: &Option<String>) -> Result<()> {
    // Validate template name
    if name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template name cannot be empty",
        ));
    }

    if name.len() > 100 {
        return Err(ggen_utils::error::Error::new(
            "Template name too long (max 100 characters)",
        ));
    }

    if !name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template name format: only alphanumeric characters, dashes, and underscores allowed",
        ));
    }

    // Validate template type if provided
    if let Some(template_type) = template_type {
        if template_type.trim().is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Template type cannot be empty",
            ));
        }

        if template_type.len() > 50 {
            return Err(ggen_utils::error::Error::new(
                "Template type too long (max 50 characters)",
            ));
        }

        if !template_type
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(ggen_utils::error::Error::new(
                "Invalid template type format: only alphanumeric characters, dashes, and underscores allowed",
            ));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_valid_input() {
        assert!(validate_template_input("my-template", &Some("rust".to_string())).is_ok());
        assert!(validate_template_input("test_123", &None).is_ok());
    }

    #[test]
    fn test_validate_invalid_name() {
        assert!(validate_template_input("", &None).is_err());
        assert!(validate_template_input(&"a".repeat(101), &None).is_err());
        assert!(validate_template_input("invalid@name", &None).is_err());
    }

    #[test]
    fn test_validate_invalid_type() {
        assert!(validate_template_input("test", &Some("".to_string())).is_err());
        assert!(validate_template_input("test", &Some("a".repeat(51))).is_err());
        assert!(validate_template_input("test", &Some("invalid@type".to_string())).is_err());
    }
}
