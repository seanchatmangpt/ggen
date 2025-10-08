use std::fs;
use std::path::{Path, PathBuf};
use utils::error::{Error, Result};

use crate::frontmatter::{Frontmatter, TemplateSpec};

/// Template metadata for discovery
#[derive(Debug, Clone)]
pub struct TemplateRef {
    pub scope: String,
    pub action: String,
    pub path: PathBuf,
}

/// Template information for listing
#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub scope: String,
    pub action: String,
}

/// List all available template scopes and actions
pub fn list_templates(templates_root: &Path) -> Result<Vec<TemplateInfo>> {
    let mut templates = Vec::new();

    if !templates_root.exists() {
        return Ok(templates);
    }

    let scope_dirs = fs::read_dir(templates_root).map_err(|e| {
        Error::new(&format!(
            "Failed to read templates directory {}: {}",
            templates_root.display(),
            e
        ))
    })?;

    for scope_result in scope_dirs {
        let scope_dir = scope_result
            .map_err(|e| Error::new(&format!("Failed to read scope directory: {}", e)))?;

        if scope_dir
            .file_type()
            .map_err(|e| {
                Error::new(&format!(
                    "Failed to get file type for {}: {}",
                    scope_dir.path().display(),
                    e
                ))
            })?
            .is_dir()
        {
            let scope_name = scope_dir.file_name().to_string_lossy().to_string();

            // Read action directories within scope
            let action_dirs = fs::read_dir(scope_dir.path()).map_err(|e| {
                Error::new(&format!(
                    "Failed to read action directory {}: {}",
                    scope_dir.path().display(),
                    e
                ))
            })?;

            for action_result in action_dirs {
                let action_dir = action_result
                    .map_err(|e| Error::new(&format!("Failed to read action directory: {}", e)))?;

                if action_dir
                    .file_type()
                    .map_err(|e| {
                        Error::new(&format!(
                            "Failed to get file type for {}: {}",
                            action_dir.path().display(),
                            e
                        ))
                    })?
                    .is_dir()
                {
                    let action_name = action_dir.file_name().to_string_lossy().to_string();

                    // Check if there's a .tmpl file in this directory
                    let tmpl_file = action_dir.path().join(format!("{}.tmpl", action_name));
                    if tmpl_file.exists() && tmpl_file.is_file() {
                        templates.push(TemplateInfo {
                            scope: scope_name.clone(),
                            action: action_name,
                        });
                    }
                }
            }
        }
    }

    // Sort for deterministic output
    templates.sort_by(|a, b| match a.scope.cmp(&b.scope) {
        std::cmp::Ordering::Equal => a.action.cmp(&b.action),
        other => other,
    });

    Ok(templates)
}

/// Read and parse a template from the templates directory
/// Looks for: templates/{scope}/{action}/{action}.tmpl
pub fn read_template(scope: &str, action: &str) -> Result<TemplateSpec> {
    // Input validation
    if scope.is_empty() || action.is_empty() {
        return Err(Error::new("Scope and action cannot be empty"));
    }
    
    // Sanitize inputs to prevent path traversal
    if scope.contains("..") || action.contains("..") || scope.contains('/') || action.contains('/') {
        return Err(Error::new("Invalid characters in scope or action"));
    }
    let template_path = PathBuf::from("templates")
        .join(scope)
        .join(action)
        .join(format!("{}.tmpl", action));

    if !template_path.exists() {
        return Err(Error::new(&format!(
            "Template not found: {}",
            template_path.display()
        )));
    }

    let content = fs::read_to_string(&template_path).map_err(|e| {
        Error::new(&format!(
            "Failed to read template '{}': {}",
            template_path.display(),
            e
        ))
    })?;

    let frontmatter = Frontmatter::parse_frontmatter(&template_path)?;

    Ok(TemplateSpec {
        path: template_path,
        output_path: PathBuf::from(&frontmatter.to),
        content,
        frontmatter,
    })
}

/// Write rendered content to output file
pub fn write_output(output_path: &Path, content: &str) -> Result<()> {
    ensure_parent_dirs(output_path)?;

    fs::write(output_path, content).map_err(|e| {
        Error::new(&format!(
            "Failed to write output file {}: {}",
            output_path.display(),
            e
        ))
    })?;

    Ok(())
}

/// Ensure parent directories exist for the given path
pub fn ensure_parent_dirs(path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            Error::new(&format!(
                "Failed to create parent directories for {}: {}",
                path.display(),
                e
            ))
        })?;
    }

    Ok(())
}

/// Write content to file (alias for write_output for compatibility)
pub fn write_file(path: &Path, content: &str) -> Result<()> {
    write_output(path, content)
}

/// Discover templates in a specific scope/action directory
pub fn discover_templates(templates_dir: &Path) -> Result<Vec<TemplateSpec>> {
    let mut specs = Vec::new();

    if !templates_dir.exists() {
        log::debug!("Template directory does not exist: {}", templates_dir.display());
        return Ok(specs);
    }

    let entries = fs::read_dir(templates_dir).map_err(|e| {
        Error::new(&format!(
            "Failed to read templates directory {}: {}",
            templates_dir.display(),
            e
        ))
    })?;

    for entry_result in entries {
        let entry = entry_result
            .map_err(|e| Error::new(&format!("Failed to read template entry: {}", e)))?;

        let path = entry.path();

        if let Some(extension) = path.extension() {
            if extension == "tmpl" {
                // Parse template and add to specs
                let content = fs::read_to_string(&path).map_err(|e| {
                    Error::new(&format!(
                        "Failed to read template file {}: {}",
                        path.display(),
                        e
                    ))
                })?;

                let frontmatter = Frontmatter::parse_frontmatter(&path)?;

                specs.push(TemplateSpec {
                    path,
                    output_path: PathBuf::from(&frontmatter.to),
                    content,
                    frontmatter,
                });
            }
        }
    }

    Ok(specs)
}
