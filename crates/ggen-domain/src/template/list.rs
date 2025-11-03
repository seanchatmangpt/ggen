//! Template listing domain logic

use ggen_utils::error::Result;
use glob::glob;
use std::fs;
use std::path::{Path, PathBuf};

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

/// List templates from the templates directory
pub fn list_templates(templates_dir: &Path, filters: &ListFilters) -> Result<Vec<TemplateInfo>> {
    let mut templates = Vec::new();

    if !templates_dir.exists() {
        return Ok(templates);
    }

    // Build glob pattern
    let pattern = if let Some(ref filter_pattern) = filters.pattern {
        format!("{}/{}", templates_dir.display(), filter_pattern)
    } else {
        format!("{}/*.tmpl", templates_dir.display())
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
            let description = extract_template_description(&path).ok().flatten();

            templates.push(TemplateInfo {
                name,
                path: path.to_string_lossy().to_string(),
                source: TemplateSource::Local,
                description,
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
            // Handle empty frontmatter (---\n---\n case)
            if end_pos >= 4 {
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
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_list_empty_directory() {
        let temp_dir = TempDir::new().unwrap();
        let templates_dir = temp_dir.path().join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        let filters = ListFilters {
            pattern: None,
            local_only: false,
            gpack_only: false,
        };

        let templates = list_templates(&templates_dir, &filters).unwrap();
        assert_eq!(templates.len(), 0);
    }

    #[test]
    fn test_list_with_templates() {
        let temp_dir = TempDir::new().unwrap();
        let templates_dir = temp_dir.path().join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        // Create test template
        fs::write(
            templates_dir.join("test.tmpl"),
            r#"---
description: "Test template"
---
Content"#,
        )
        .unwrap();

        let filters = ListFilters {
            pattern: None,
            local_only: false,
            gpack_only: false,
        };

        let templates = list_templates(&templates_dir, &filters).unwrap();
        assert_eq!(templates.len(), 1);
        assert_eq!(templates[0].name, "test.tmpl");
        assert_eq!(templates[0].description, Some("Test template".to_string()));
    }

    #[test]
    fn test_list_with_pattern() {
        let temp_dir = TempDir::new().unwrap();
        let templates_dir = temp_dir.path().join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        fs::write(templates_dir.join("rust.tmpl"), "---\n---\nRust").unwrap();
        fs::write(templates_dir.join("python.tmpl"), "---\n---\nPython").unwrap();

        let filters = ListFilters {
            pattern: Some("rust*".to_string()),
            local_only: false,
            gpack_only: false,
        };

        let templates = list_templates(&templates_dir, &filters).unwrap();
        assert_eq!(templates.len(), 1);
        assert_eq!(templates[0].name, "rust.tmpl");
    }
}

/// CLI Arguments for list command
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ListInput {
    /// Template directory to list from
    pub directory: PathBuf,

    /// Filter pattern (glob)
    pub pattern: Option<String>,

    /// Only show local templates
    pub local_only: bool,

    /// Only show gpack templates
    pub gpack_only: bool,
}

/// Execute list templates with input (pure domain function)
pub async fn execute_list(input: ListInput) -> Result<Vec<TemplateInfo>> {
    let filters = ListFilters {
        pattern: input.pattern,
        local_only: input.local_only,
        gpack_only: input.gpack_only,
    };

    list_templates(&input.directory, &filters)
}

impl TemplateInfo {
    fn source_display(&self) -> &str {
        match &self.source {
            TemplateSource::Local => "local",
            TemplateSource::Gpack(name) => name,
        }
    }
}
