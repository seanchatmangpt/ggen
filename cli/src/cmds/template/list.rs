use clap::Args;
use ggen_utils::error::Result;
use glob::glob;
use std::fs;
use std::path::Path;

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

/// Validate and sanitize pattern input (if provided)
fn validate_pattern(pattern: &Option<String>) -> Result<()> {
    if let Some(pattern) = pattern {
        // Validate pattern is not empty
        if pattern.trim().is_empty() {
            return Err(ggen_utils::error::Error::new("Pattern cannot be empty"));
        }

        // Validate pattern length
        if pattern.len() > 200 {
            return Err(ggen_utils::error::Error::new(
                "Pattern too long (max 200 characters)",
            ));
        }

        // Basic path traversal protection
        if pattern.contains("..") {
            return Err(ggen_utils::error::Error::new(
                "Path traversal detected: pattern cannot contain '..'",
            ));
        }

        // Validate pattern format (basic pattern check)
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
            return Err(ggen_utils::error::Error::new(
                "Invalid pattern format: only alphanumeric characters, dots, wildcards, brackets, dashes, and underscores allowed",
            ));
        }
    }

    Ok(())
}

pub async fn run(args: &ListArgs) -> Result<()> {
    // Validate input
    validate_pattern(&args.pattern)?;

    println!("📄 Listing templates...");

    let filters = ListFilters {
        pattern: args.pattern.clone(),
        local_only: args.local,
        gpack_only: args.gpack,
    };

    let templates = list_local_templates(&filters)?;

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
}

/// List local templates from the templates directory
fn list_local_templates(filters: &ListFilters) -> Result<Vec<TemplateInfo>> {
    let mut templates = Vec::new();

    // Check if templates directory exists
    let templates_dir = Path::new("templates");
    if !templates_dir.exists() {
        return Ok(templates);
    }

    // Build glob pattern
    let pattern = if let Some(ref filter_pattern) = filters.pattern {
        format!("templates/{}", filter_pattern)
    } else {
        "templates/*.tmpl".to_string()
    };

    // Find template files
    for entry in glob(&pattern)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Invalid glob pattern: {}", e)))?
    {
        let path = entry.map_err(|e| {
            ggen_utils::error::Error::new(&format!("Error reading directory entry: {}", e))
        })?;

        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("tmpl") {
            let name = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unknown")
                .to_string();

            // Extract description from template content
            let description = extract_template_description(&path).ok();

            templates.push(TemplateInfo {
                name,
                path: path.to_string_lossy().to_string(),
                source: TemplateSource::Local,
                description: description.flatten(),
            });
        }
    }

    Ok(templates)
}

/// Extract description from template frontmatter
fn extract_template_description(path: &Path) -> Result<Option<String>> {
    let content = fs::read_to_string(path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))?;

    // Look for YAML frontmatter
    if content.starts_with("---\n") {
        if let Some(end_pos) = content.find("\n---\n") {
            let frontmatter = &content[4..end_pos];

            // Simple extraction of description field
            for line in frontmatter.lines() {
                if line.trim().starts_with("description:") {
                    if let Some(desc) = line.split_once(':').map(|x| x.1) {
                        return Ok(Some(desc.trim().trim_matches('"').to_string()));
                    }
                }
            }
        }
    }

    Ok(None)
}

pub async fn run_with_deps(args: &ListArgs, lister: &dyn TemplateLister) -> Result<()> {
    // Validate input
    validate_pattern(&args.pattern)?;

    // Show progress for listing operation
    println!("🔍 Listing templates...");

    let filters = ListFilters {
        pattern: args.pattern.clone(),
        local_only: args.local,
        gpack_only: args.gpack,
    };

    let templates = lister.list_templates(&filters)?;

    if templates.is_empty() {
        println!("ℹ️  No templates found");
        return Ok(());
    }

    // Show progress for large result sets
    if templates.len() > 20 {
        println!("📊 Processing {} templates...", templates.len());
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_list_displays_templates() {
        let mut mock_lister = MockTemplateLister::new();
        mock_lister.expect_list_templates().times(1).returning(|_| {
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
