use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct ListArgs {
    /// Filter by pattern (glob)
    #[arg(long)]
    pub pattern: Option<String>,

    /// Show only local templates
    #[arg(long)]
    pub local: bool,

    /// Show only gpack templates
    #[arg(long)]
    pub gpack: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait TemplateLister {
    fn list_templates(&self, filters: &ListFilters) -> Result<Vec<TemplateInfo>>;
}

#[derive(Debug, Clone)]
pub struct ListFilters {
    pub pattern: Option<String>,
    pub local_only: bool,
    pub gpack_only: bool,
}

#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub name: String,
    pub path: String,
    pub source: TemplateSource,
    pub description: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplateSource {
    Local,
    Gpack(String),
}

pub async fn run(args: &ListArgs) -> Result<()> {
    println!("🚧 Placeholder: template list");
    println!("  Pattern: {:?}", args.pattern);
    println!("  Local: {}", args.local);
    println!("  Gpack: {}", args.gpack);
    Ok(())
}

pub async fn run_with_deps(args: &ListArgs, lister: &dyn TemplateLister) -> Result<()> {
    let filters = ListFilters {
        pattern: args.pattern.clone(),
        local_only: args.local,
        gpack_only: args.gpack,
    };

    let templates = lister.list_templates(&filters)?;

    if templates.is_empty() {
        println!("No templates found");
        return Ok(());
    }

    for template in templates {
        match template.source {
            TemplateSource::Local => {
                println!("📄 {} (local)", template.name);
            }
            TemplateSource::Gpack(ref gpack_id) => {
                println!("📦 {} ({})", template.name, gpack_id);
            }
        }
        if let Some(desc) = template.description {
            println!("   {}", desc);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_displays_templates() {
        let mut mock_lister = MockTemplateLister::new();
        mock_lister
            .expect_list_templates()
            .times(1)
            .returning(|_| {
                Ok(vec![
                    TemplateInfo {
                        name: "hello.tmpl".to_string(),
                        path: "templates/hello.tmpl".to_string(),
                        source: TemplateSource::Local,
                        description: Some("Hello world template".to_string()),
                    },
                    TemplateInfo {
                        name: "rust-cli".to_string(),
                        path: "gpacks/io.ggen.rust.cli/template.tmpl".to_string(),
                        source: TemplateSource::Gpack("io.ggen.rust.cli".to_string()),
                        description: None,
                    },
                ])
            });

        let args = ListArgs {
            pattern: None,
            local: false,
            gpack: false,
        };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_with_pattern_filter() {
        let mut mock_lister = MockTemplateLister::new();
        mock_lister
            .expect_list_templates()
            .withf(|filters| filters.pattern == Some("rust*".to_string()))
            .times(1)
            .returning(|_| Ok(vec![]));

        let args = ListArgs {
            pattern: Some("rust*".to_string()),
            local: false,
            gpack: false,
        };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_list_empty() {
        let mut mock_lister = MockTemplateLister::new();
        mock_lister
            .expect_list_templates()
            .times(1)
            .returning(|_| Ok(vec![]));

        let args = ListArgs {
            pattern: None,
            local: false,
            gpack: false,
        };

        let result = run_with_deps(&args, &mock_lister).await;
        assert!(result.is_ok());
    }
}
