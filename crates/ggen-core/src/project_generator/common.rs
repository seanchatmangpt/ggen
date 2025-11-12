// Common utilities for project generators

use anyhow::Result;
use std::path::Path;

/// Validates project name
pub fn validate_project_name(name: &str) -> Result<()> {
    if name.is_empty() {
        anyhow::bail!("Project name cannot be empty");
    }

    if name.contains(char::is_whitespace) {
        anyhow::bail!("Project name cannot contain whitespace");
    }

    if name.starts_with('-') || name.starts_with('_') {
        anyhow::bail!("Project name cannot start with '-' or '_'");
    }

    // Check for valid characters (alphanumeric, dash, underscore)
    if !name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        anyhow::bail!(
            "Project name can only contain alphanumeric characters, dashes, and underscores"
        );
    }

    Ok(())
}

/// Checks if a directory is empty
pub fn is_directory_empty(path: &Path) -> Result<bool> {
    if !path.exists() {
        return Ok(true);
    }

    let entries =
        std::fs::read_dir(path).map_err(|e| anyhow::anyhow!("Failed to read directory: {}", e))?;

    Ok(entries.count() == 0)
}

/// Creates a basic .editorconfig file
pub fn generate_editorconfig() -> String {
    r#"root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{js,jsx,ts,tsx,vue}]
indent_style = space
indent_size = 2

[*.{rs,toml}]
indent_style = space
indent_size = 4

[*.md]
trim_trailing_whitespace = false
"#
    .to_string()
}

/// Creates a basic .prettierrc file
pub fn generate_prettierrc() -> String {
    r#"{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2
}
"#
    .to_string()
}

/// Creates a basic .eslintrc file
pub fn generate_eslintrc() -> String {
    r#"{
  "extends": ["next/core-web-vitals"],
  "rules": {
    "no-console": "warn",
    "no-unused-vars": "warn"
  }
}
"#
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_project_name() {
        assert!(validate_project_name("my-project").is_ok());
        assert!(validate_project_name("my_project").is_ok());
        assert!(validate_project_name("myproject123").is_ok());

        assert!(validate_project_name("").is_err());
        assert!(validate_project_name("my project").is_err());
        assert!(validate_project_name("-myproject").is_err());
        assert!(validate_project_name("my@project").is_err());
    }

    #[test]
    fn test_generate_editorconfig() {
        let config = generate_editorconfig();
        assert!(config.contains("root = true"));
        assert!(config.contains("charset = utf-8"));
    }
}
