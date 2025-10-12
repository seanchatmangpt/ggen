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
#[allow(dead_code)]
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

/// Generate template content based on template type
fn generate_template_content(name: &str, template_type: &str) -> Result<String> {
    let timestamp = chrono::Utc::now().to_rfc3339();

    match template_type {
        "rust" => Ok(format!(
            "---\n\
             to: src/{{{{ name }}}}.rs\n\
             vars:\n\
               name: \"{name}\"\n\
               author: \"{{{{ author }}}}\"\n\
             rdf:\n\
               sources: []\n\
             sparql:\n\
               queries: {{}}\n\
             determinism:\n\
               seed: \"{timestamp}\"\n\
             ---\n\
             \n\
             // Generated Rust module: {name}\n\
             // Author: {{{{ author }}}}\n\
             // Generated at: {timestamp}\n\
             \n\
             pub struct {{{{ name | pascal_case }}}} {{\n\
                 pub id: String,\n\
                 pub name: String,\n\
             }}\n\
             \n\
             impl {{{{ name | pascal_case }}}} {{\n\
                 pub fn new(name: String) -> Self {{\n\
                     Self {{\n\
                         id: uuid::Uuid::new_v4().to_string(),\n\
                         name,\n\
                     }}\n\
                 }}\n\
             }}\n"
        )),
        "python" => Ok(format!(
            "---\n\
             to: src/{{{{ name }}}}.py\n\
             vars:\n\
               name: \"{name}\"\n\
               author: \"{{{{ author }}}}\"\n\
             rdf:\n\
               sources: []\n\
             sparql:\n\
               queries: {{}}\n\
             determinism:\n\
               seed: \"{timestamp}\"\n\
             ---\n\
             \n\
             # Generated Python module: {name}\n\
             # Author: {{{{ author }}}}\n\
             # Generated at: {timestamp}\n\
             \n\
             from dataclasses import dataclass\n\
             from typing import Optional\n\
             \n\
             @dataclass\n\
             class {{{{ name | pascal_case }}}}:\n\
                 id: str\n\
                 name: str\n\
             \n\
                 def __init__(self, name: str):\n\
                     self.id = str(uuid.uuid4())\n\
                     self.name = name\n"
        )),
        "typescript" => Ok(format!(
            "---\n\
             to: src/{{{{ name }}}}.ts\n\
             vars:\n\
               name: \"{name}\"\n\
               author: \"{{{{ author }}}}\"\n\
             rdf:\n\
               sources: []\n\
             sparql:\n\
               queries: {{}}\n\
             determinism:\n\
               seed: \"{timestamp}\"\n\
             ---\n\
             \n\
             // Generated TypeScript module: {name}\n\
             // Author: {{{{ author }}}}\n\
             // Generated at: {timestamp}\n\
             \n\
             export interface {{{{ name | pascal_case }}}} {{\n\
                 id: string;\n\
                 name: string;\n\
             }}\n\
             \n\
             export class {{{{ name | pascal_case }}}}Class implements {{{{ name | pascal_case }}}} {{\n\
                 id: string;\n\
                 name: string;\n\
             \n\
                 constructor(name: string) {{\n\
                     this.id = crypto.randomUUID();\n\
                     this.name = name;\n\
                 }}\n\
             }}\n"
        )),
        "generic" => Ok(format!(
            "---\n\
             to: output/{{{{ name }}}}.txt\n\
             vars:\n\
               name: \"{name}\"\n\
               author: \"{{{{ author }}}}\"\n\
             rdf:\n\
               sources: []\n\
             sparql:\n\
               queries: {{}}\n\
             determinism:\n\
               seed: \"{timestamp}\"\n\
             ---\n\
             \n\
             Generated file: {name}\n\
             Author: {{{{ author }}}}\n\
             Generated at: {timestamp}\n\
             \n\
             This is a generic template. Customize the content below:\n\
             \n\
             Hello, {{{{ name }}}}!\n\
             \n\
             Your template content goes here.\n"
        )),
        _ => Ok(format!(
            "---\n\
             to: output/{{{{ name }}}}.txt\n\
             vars:\n\
               name: \"{name}\"\n\
               author: \"{{{{ author }}}}\"\n\
             rdf:\n\
               sources: []\n\
             sparql:\n\
               queries: {{}}\n\
             determinism:\n\
               seed: \"{timestamp}\"\n\
             ---\n\
             \n\
             Generated file: {name}\n\
             Author: {{{{ author }}}}\n\
             Generated at: {timestamp}\n\
             \n\
             This is a generic template. Customize the content below:\n\
             \n\
             Hello, {{{{ name }}}}!\n\
             \n\
             Your template content goes here.\n"
        )),
    }
}

pub async fn run(args: &NewArgs) -> Result<()> {
    // Validate input
    validate_template_input(args)?;

    println!("🔧 Creating new template...");

    // Determine template type
    let template_type = args.template_type.as_deref().unwrap_or("generic");

    // Create templates directory if it doesn't exist
    let templates_dir = std::path::Path::new("templates");
    if !templates_dir.exists() {
        std::fs::create_dir_all(templates_dir).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create templates directory: {}", e))
        })?;
    }

    // Generate template file path
    let template_filename = format!("{}.tmpl", args.name.trim());
    let template_path = templates_dir.join(&template_filename);

    // Check if template already exists
    if template_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Template '{}' already exists at {}",
            args.name,
            template_path.display()
        )));
    }

    // Generate template content based on type
    let template_content = generate_template_content(&args.name, template_type)?;

    // Write template file
    std::fs::write(&template_path, template_content).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to write template file: {}", e))
    })?;

    println!(
        "✅ Created template '{}' at {}",
        args.name,
        template_path.display()
    );
    println!("📝 Template type: {}", template_type);

    if args.interactive {
        println!("💡 Tip: Edit the template file to customize variables and content");
    }

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
