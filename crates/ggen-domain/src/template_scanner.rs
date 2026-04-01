//! Template Default Scanner
//!
//! Scans Tera templates for undeclared default values.
//! Required for CISO `ForbidTemplateDefaults` policy enforcement.

use std::path::Path;

use ggen_utils::error::{Error, Result};

/// A detected template default violation
#[derive(Debug, Clone)]
pub struct DefaultViolation {
    /// Template file path
    pub file: String,
    /// Line number (1-based)
    pub line: usize,
    /// The variable name with default
    pub variable: String,
    /// The default value found
    pub default_value: String,
    /// Full line context
    pub context: String,
}

/// Scans Tera templates for undeclared default values
pub struct TemplateScanner;

impl TemplateScanner {
    /// Scan a single template file for default value patterns
    ///
    /// Tera default filter syntax: `{{ value | default(value) }}`
    /// Also detects: `{% set var = "default" %}` without ontology binding
    pub fn scan_file(path: &Path) -> Result<Vec<DefaultViolation>> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Cannot read {}: {}", path.display(), e)))?;

        let mut violations = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed: &str = line.trim();

            // Skip comments
            if trimmed.starts_with("{#") {
                continue;
            }

            // Pattern 1: {{ var | default(value) }}
            if let Some(caps) = Self::match_default_filter(trimmed) {
                violations.push(DefaultViolation {
                    file: path.display().to_string(),
                    line: line_num + 1,
                    variable: caps.variable,
                    default_value: caps.default_value,
                    context: trimmed.to_string(),
                });
            }

            // Pattern 2: {% set var = "literal" %}
            if let Some(caps) = Self::match_set_literal(trimmed) {
                violations.push(DefaultViolation {
                    file: path.display().to_string(),
                    line: line_num + 1,
                    variable: caps.variable,
                    default_value: caps.default_value,
                    context: trimmed.to_string(),
                });
            }
        }

        Ok(violations)
    }

    /// Scan all template files in a directory
    pub fn scan_directory(dir: &Path) -> Result<Vec<DefaultViolation>> {
        let mut all_violations = Vec::new();

        if !dir.exists() {
            return Ok(all_violations);
        }

        let entries = std::fs::read_dir(dir)
            .map_err(|e| Error::new(&format!("Cannot read {}: {}", dir.display(), e)))?;

        for entry in entries {
            let entry =
                entry.map_err(|e| Error::new(&format!("Cannot read directory entry: {}", e)))?;
            let path = entry.path();

            if path.is_dir() {
                all_violations.extend(Self::scan_directory(&path)?);
            } else if path
                .extension()
                .map(|e| e == "tera" || e == "html" || e == "j2" || e == "jinja2")
                .unwrap_or(false)
            {
                all_violations.extend(Self::scan_file(&path)?);
            }
        }

        Ok(all_violations)
    }

    /// Check if any violations exist
    pub fn has_defaults(dir: &Path) -> Result<bool> {
        Ok(!Self::scan_directory(dir)?.is_empty())
    }

    fn match_default_filter(line: &str) -> Option<DefaultMatch> {
        // Match patterns like: {{ var | default("value") }} or {{ var|default(value) }}
        let line_lower = line.to_lowercase();

        let default_idx = line_lower.find("| default(")?;
        let var_part = line[..default_idx].trim();

        // Extract variable name from {{ var }}
        let var_start = var_part.find("{{")?;
        let var_end = var_part.find("}}")?;
        let variable = var_part[var_start + 2..var_end].trim().to_string();

        // Extract default value
        let remaining = &line[default_idx + "| default(".len()..];
        let value_end = remaining.find(')')?;
        let default_value = remaining[..value_end].trim().to_string();

        if !variable.is_empty() && !default_value.is_empty() {
            Some(DefaultMatch {
                variable,
                default_value,
            })
        } else {
            None
        }
    }

    fn match_set_literal(line: &str) -> Option<DefaultMatch> {
        // Match patterns like: {% set var = "literal" %}
        let line_lower = line.to_lowercase();
        let set_idx = line_lower.find("{% set ")?;

        let remaining = &line[set_idx + "{% set ".len()..];
        let eq_idx = remaining.find('=')?;
        let variable = remaining[..eq_idx].trim().to_string();

        let value_part = remaining[eq_idx + 1..].trim();
        let end_idx = value_part.find("%}")?;
        let literal = value_part[..end_idx]
            .trim()
            .trim_matches('"')
            .trim_matches('\'')
            .to_string();

        if !variable.is_empty() && !literal.is_empty() {
            Some(DefaultMatch {
                variable,
                default_value: literal,
            })
        } else {
            None
        }
    }
}

struct DefaultMatch {
    variable: String,
    default_value: String,
}
