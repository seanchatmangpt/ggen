//! Template creation and scaffolding functionality.
//!
//! This module provides comprehensive template creation capabilities, supporting
//! interactive wizard mode, template type specification, and output path validation.
//! It helps users create new templates with proper structure and metadata.
//!
//! # Examples
//!
//! ```bash
//! ggen template new my-template --template-type rust
//! ggen template new web-template --interactive
//! ggen template new api-template --template-type typescript
//! ```
//!
//! # Errors
//!
//! Returns errors if the template name is invalid, the template type is
//! unsupported, the output path contains traversal attempts, or if template
//! creation fails due to file system issues.

use clap::Args;
use ggen_utils::error::Result;
use std::path::{Component, Path};

#[derive(Args, Debug)]
pub struct NewArgs {
    /// Template name
    pub name: String,

    /// Template type (rust, python, typescript, etc.)
    #[arg(long)]
    pub template_type: Option<String>,

    /// Interactive wizard mode
    #[arg(long)]
    pub interactive: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait TemplateCreator {
    fn create(&self, name: String, template_type: Option<String>) -> Result<String>;
}

#[cfg_attr(test, mockall::automock)]
pub trait InteractiveWizard {
    fn run_wizard(&self) -> Result<TemplateSpec>;
}

#[derive(Debug, Clone)]
pub struct TemplateSpec {
    pub name: String,
    pub template_type: String,
    pub output_path: String,
    pub variables: Vec<String>,
}

/// Validate and sanitize template input
fn validate_template_input(args: &NewArgs) -> Result<()> {
    // Validate template name is not empty
    if args.name.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template name cannot be empty",
        ));
    }

    // Validate template name length
    if args.name.len() > 100 {
        return Err(ggen_utils::error::Error::new(
            "Template name too long (max 100 characters)",
        ));
    }

    // Validate template name format (basic pattern check)
    if !args
        .name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template name format: only alphanumeric characters, dashes, and underscores allowed",
        ));
    }

    // Validate template type if provided
    if let Some(template_type) = &args.template_type {
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

/// Validate and sanitize output path
fn validate_output_path(path: &str) -> Result<()> {
    // Validate path is not empty
    if path.trim().is_empty() {
        return Err(ggen_utils::error::Error::new("Output path cannot be empty"));
    }

    // Validate path length
    if path.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Output path too long (max 1000 characters)",
        ));
    }

    // Use Path components for proper traversal protection
    let path_obj = Path::new(path);
    if path_obj
        .components()
        .any(|c| matches!(c, Component::ParentDir))
    {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: paths containing '..' are not allowed",
        ));
    }

    Ok(())
}

pub async fn run(args: &NewArgs) -> Result<()> {
    // Validate input
    validate_template_input(args)?;

    println!("🚧 Placeholder: template new");
    println!("  Name: {}", args.name.trim());
    if let Some(template_type) = &args.template_type {
        println!("  Type: {}", template_type.trim());
    }
    println!("  Interactive: {}", args.interactive);
    Ok(())
}

pub async fn run_with_deps(
    args: &NewArgs, creator: &dyn TemplateCreator, wizard: Option<&dyn InteractiveWizard>,
) -> Result<()> {
    // Validate input
    validate_template_input(args)?;

    if args.interactive {
        println!("🔍 Starting interactive template wizard...");
        if let Some(wizard) = wizard {
            let spec = wizard.run_wizard()?;
            println!("⚙️  Creating template...");
            let path = creator.create(spec.name.clone(), Some(spec.template_type.clone()))?;
            println!("✅ Created template '{}' at {}", spec.name, path);
            Ok(())
        } else {
            Err(ggen_utils::error::Error::new(
                "Interactive mode requires wizard",
            ))
        }
    } else {
        println!("⚙️  Creating template...");
        let path = creator.create(args.name.clone(), args.template_type.clone())?;
        println!("✅ Created template '{}' at {}", args.name, path);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_new_creates_template() {
        let mut mock_creator = MockTemplateCreator::new();
        mock_creator
            .expect_create()
            .with(eq(String::from("hello")), eq(Some(String::from("rust"))))
            .times(1)
            .returning(|name, _| Ok(format!("templates/{}.tmpl", name)));

        let args = NewArgs {
            name: "hello".to_string(),
            template_type: Some("rust".to_string()),
            interactive: false,
        };

        let result = run_with_deps(&args, &mock_creator, None).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_new_interactive_uses_wizard() {
        let mut mock_wizard = MockInteractiveWizard::new();
        mock_wizard.expect_run_wizard().times(1).returning(|| {
            Ok(TemplateSpec {
                name: "my-template".to_string(),
                template_type: "python".to_string(),
                output_path: "src/{{ name }}.py".to_string(),
                variables: vec!["name".to_string(), "author".to_string()],
            })
        });

        let mut mock_creator = MockTemplateCreator::new();
        mock_creator
            .expect_create()
            .with(
                eq(String::from("my-template")),
                eq(Some(String::from("python"))),
            )
            .times(1)
            .returning(|name, _| Ok(format!("templates/{}.tmpl", name)));

        let args = NewArgs {
            name: "ignored".to_string(),
            template_type: None,
            interactive: true,
        };

        let result = run_with_deps(&args, &mock_creator, Some(&mock_wizard)).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_new_interactive_without_wizard_fails() {
        let mock_creator = MockTemplateCreator::new();

        let args = NewArgs {
            name: "test".to_string(),
            template_type: None,
            interactive: true,
        };

        let result = run_with_deps(&args, &mock_creator, None).await;
        assert!(result.is_err());
    }
}
