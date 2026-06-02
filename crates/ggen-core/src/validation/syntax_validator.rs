//! Multi-language syntax validator
//!
//! Validates generated files for syntax errors using language-appropriate validators.
//! Supports: Rust (syn), TOML, YAML, JSON, Tera templates.
//!
//! This module is part of the validate_syntax gate in the μ₃ (validation) stage
//! of the code generation pipeline. It prevents emission of syntactically invalid artifacts.

use super::error::{Result, ValidationError};
use std::path::Path;

/// Detects language type from file extension
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanguageType {
    /// Rust source (.rs)
    Rust,
    /// TOML config (.toml)
    Toml,
    /// YAML config (.yaml, .yml)
    Yaml,
    /// JSON (.json)
    Json,
    /// Tera template (.tera)
    Tera,
    /// Markdown (.md)
    Markdown,
    /// SPARQL query (.rq)
    Sparql,
    /// Turtle RDF (.ttl)
    Turtle,
    /// JSON-LD (.jsonld)
    JsonLd,
}

impl LanguageType {
    /// Get string representation
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Rust => "Rust",
            Self::Toml => "TOML",
            Self::Yaml => "YAML",
            Self::Json => "JSON",
            Self::Tera => "Tera",
            Self::Markdown => "Markdown",
            Self::Sparql => "SPARQL",
            Self::Turtle => "Turtle",
            Self::JsonLd => "JSON-LD",
        }
    }
}

/// Detects language type from file path
pub fn detect_language(path: &Path) -> LanguageType {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| match ext.to_lowercase().as_str() {
            "rs" => LanguageType::Rust,
            "toml" => LanguageType::Toml,
            "yaml" | "yml" => LanguageType::Yaml,
            "json" => LanguageType::Json,
            "tera" => LanguageType::Tera,
            "md" => LanguageType::Markdown,
            "rq" => LanguageType::Sparql,
            "ttl" => LanguageType::Turtle,
            "jsonld" => LanguageType::JsonLd,
            _ => LanguageType::Markdown, // default to passthrough
        })
        .unwrap_or(LanguageType::Markdown)
}

/// Validates Rust syntax using syn crate
fn validate_rust_syntax(content: &str) -> Result<()> {
    match syn::parse_file(content) {
        Ok(_) => Ok(()),
        Err(e) => {
            // syn::Error doesn't expose line/column directly; extract from message if present
            // For now, use 0 as placeholder since the error message contains useful info
            let error_msg = e.to_string();
            Err(ValidationError::syntax_validation_failed(
                "<rust>", 0, // line info not directly available from syn::Error
                0, // column info not directly available
                "Rust", error_msg,
            ))
        }
    }
}

/// Validates TOML syntax
fn validate_toml_syntax(content: &str) -> Result<()> {
    match toml::from_str::<toml::Value>(content) {
        Ok(_) => Ok(()),
        Err(e) => {
            let (line, col) = if let Some(span) = e.span() {
                let start = content[..span.start].matches('\n').count() + 1;
                let col_start = content[..span.start].rfind('\n').unwrap_or(0);
                let col = span.start - col_start;
                (start, col)
            } else {
                (0, 0)
            };
            Err(ValidationError::syntax_validation_failed(
                "<toml>",
                line,
                col,
                "TOML",
                e.to_string(),
            ))
        }
    }
}

/// Validates YAML syntax
fn validate_yaml_syntax(content: &str) -> Result<()> {
    match serde_yaml::from_str::<serde_yaml::Value>(content) {
        Ok(_) => Ok(()),
        Err(e) => {
            // serde_yaml doesn't provide precise line/column in error
            Err(ValidationError::syntax_validation_failed(
                "<yaml>",
                0,
                0,
                "YAML",
                e.to_string(),
            ))
        }
    }
}

/// Validates JSON syntax
fn validate_json_syntax(content: &str) -> Result<()> {
    match serde_json::from_str::<serde_json::Value>(content) {
        Ok(_) => Ok(()),
        Err(e) => {
            let line = e.line();
            let column = e.column();
            Err(ValidationError::syntax_validation_failed(
                "<json>",
                line,
                column,
                "JSON",
                e.to_string(),
            ))
        }
    }
}

/// Validates Tera template syntax
///
/// Separates parse validation from render validation:
/// - Parse: Check Tera syntax (no context required)
/// - Render: Validate with sample context (catches missing variables separately)
fn validate_tera_syntax(content: &str) -> Result<()> {
    use gray_matter::{engine::YAML, Matter, ParsedEntity};

    // Parse frontmatter (YAML)
    let matter = Matter::<YAML>::new();
    let _parsed = matter.parse::<serde_yaml::Value>(content).map_err(|e| {
        ValidationError::syntax_validation_failed(
            "<tera>",
            0,
            0,
            "Tera",
            format!("Frontmatter parse error: {}", e),
        )
    })?;

    // Extract body (everything after frontmatter)
    let body = if let Some(end_pos) = content.find("\n---\n") {
        if end_pos + 5 < content.len() {
            &content[end_pos + 5..]
        } else {
            ""
        }
    } else {
        // No frontmatter, entire content is body
        content
    };

    // Validate Tera syntax by attempting to parse as a template
    // This checks for syntactic errors without requiring context
    let mut tera = tera::Tera::default();
    tera.add_raw_template("_validate", body).map_err(|e| {
        ValidationError::syntax_validation_failed(
            "<tera>",
            0,
            0,
            "Tera",
            format!("Tera syntax error: {}", e),
        )
    })?;

    Ok(())
}

/// Passthrough validator for descriptive formats (markdown, RDF, SPARQL)
fn validate_passthrough(_content: &str) -> Result<()> {
    // These formats are validated elsewhere or are documentation-only.
    // Passthrough to allow generation without strict syntax checking.
    Ok(())
}

/// Validates generated file syntax based on language type
///
/// # Arguments
///
/// * `path` - File path (for extension-based language detection)
/// * `content` - File content
///
/// # Returns
///
/// `Ok(())` if syntax is valid, or `Err(ValidationError::SyntaxValidationFailed)` with
/// line/column/language/error details if invalid.
pub fn validate_syntax(path: &Path, content: &str) -> Result<()> {
    // Empty files are invalid for code but valid for descriptive formats
    if content.is_empty() {
        let lang = detect_language(path);
        return match lang {
            LanguageType::Rust | LanguageType::Toml | LanguageType::Json => {
                Err(ValidationError::syntax_validation_failed(
                    path.to_string_lossy(),
                    1,
                    0,
                    lang.as_str(),
                    "empty file".to_string(),
                ))
            }
            _ => Ok(()), // Markdown, YAML, TTL, etc. can be empty
        };
    }

    let lang = detect_language(path);
    match lang {
        LanguageType::Rust => validate_rust_syntax(content),
        LanguageType::Toml => validate_toml_syntax(content),
        LanguageType::Yaml => validate_yaml_syntax(content),
        LanguageType::Json => validate_json_syntax(content),
        LanguageType::Tera => validate_tera_syntax(content),
        _ => validate_passthrough(content),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_language_rust() {
        assert_eq!(detect_language(Path::new("main.rs")), LanguageType::Rust);
    }

    #[test]
    fn test_detect_language_toml() {
        assert_eq!(detect_language(Path::new("Cargo.toml")), LanguageType::Toml);
    }

    #[test]
    fn test_detect_language_yaml() {
        assert_eq!(
            detect_language(Path::new("config.yaml")),
            LanguageType::Yaml
        );
        assert_eq!(detect_language(Path::new("config.yml")), LanguageType::Yaml);
    }

    #[test]
    fn test_detect_language_json() {
        assert_eq!(detect_language(Path::new("data.json")), LanguageType::Json);
    }

    #[test]
    fn test_validate_rust_valid() {
        let code = "fn main() { let x = 1; }";
        assert!(validate_rust_syntax(code).is_ok());
    }

    #[test]
    fn test_validate_rust_invalid() {
        let code = "fn main() { let x = 1 }"; // missing semicolon
        assert!(validate_rust_syntax(code).is_err());
    }

    #[test]
    fn test_validate_toml_valid() {
        let content = "[package]\nname = \"test\"\n";
        assert!(validate_toml_syntax(content).is_ok());
    }

    #[test]
    fn test_validate_toml_invalid() {
        let content = "[package\nname = \"test\"\n"; // missing ]
        assert!(validate_toml_syntax(content).is_err());
    }

    #[test]
    fn test_validate_json_valid() {
        let content = r#"{"key": "value"}"#;
        assert!(validate_json_syntax(content).is_ok());
    }

    #[test]
    fn test_validate_json_invalid() {
        let content = r#"{"key": "value",}"#; // trailing comma
        assert!(validate_json_syntax(content).is_err());
    }

    #[test]
    fn test_validate_yaml_valid() {
        let content = "key: value\n";
        assert!(validate_yaml_syntax(content).is_ok());
    }

    #[test]
    fn test_validate_yaml_invalid() {
        let content = "key: value\n  bad indent: no\n"; // bad indentation
        assert!(validate_yaml_syntax(content).is_err());
    }
}
