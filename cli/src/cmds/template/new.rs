use clap::Args;
use ggen_utils::error::Result;

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

pub async fn run(args: &NewArgs) -> Result<()> {
    println!("🚧 Placeholder: template new");
    println!("  Name: {}", args.name);
    println!("  Type: {:?}", args.template_type);
    println!("  Interactive: {}", args.interactive);
    Ok(())
}

pub async fn run_with_deps(
    args: &NewArgs,
    creator: &dyn TemplateCreator,
    wizard: Option<&dyn InteractiveWizard>,
) -> Result<()> {
    if args.interactive {
        if let Some(wizard) = wizard {
            let spec = wizard.run_wizard()?;
            let path = creator.create(spec.name.clone(), Some(spec.template_type.clone()))?;
            println!("✅ Created template '{}' at {}", spec.name, path);
            Ok(())
        } else {
            Err(ggen_utils::error::Error::new(
                "Interactive mode requires wizard",
            ))
        }
    } else {
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
            .with(eq("hello"), eq(Some("rust")))
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
            .with(eq("my-template"), eq(Some("python")))
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
