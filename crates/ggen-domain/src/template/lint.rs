//! Template linting domain logic


use ggen_utils::error::Result;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct LintOptions {
    pub check_sparql: bool,
    pub check_schema: bool,
}

#[derive(Debug, Clone)]
pub struct LintReport {
    pub errors: Vec<LintError>,
    pub warnings: Vec<LintWarning>,
}

#[derive(Debug, Clone)]
pub struct LintError {
    pub line: Option<usize>,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct LintWarning {
    pub line: Option<usize>,
    pub message: String,
}

impl LintReport {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }
}

/// Lint a template file for common issues
pub fn lint_template(template_ref: &str, options: &LintOptions) -> Result<LintReport> {
    let mut report = LintReport {
        errors: Vec::new(),
        warnings: Vec::new(),
    };

    // Determine template path
    let template_path = if template_ref.starts_with("gpack:") {
        return Err(ggen_utils::error::Error::new(
            "gpack templates not yet supported",
        ));
    } else if template_ref.contains('/') {
        template_ref.to_string()
    } else {
        format!("templates/{}", template_ref)
    };

    // Check if template exists
    let path = Path::new(&template_path);
    if !path.exists() {
        report.errors.push(LintError {
            line: None,
            message: format!("Template file not found: {}", template_path),
        });
        return Ok(report);
    }

    // Read template content
    let content = fs::read_to_string(path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to read template: {}", e)))?;

    // Check for YAML frontmatter
    if !content.starts_with("---\n") {
        report.warnings.push(LintWarning {
            line: Some(1),
            message: "Template should start with YAML frontmatter (---)".to_string(),
        });
    } else {
        // Validate YAML frontmatter
        if let Some(end_pos) = content.find("\n---\n") {
            let frontmatter = &content[4..end_pos];
            validate_frontmatter(frontmatter, &mut report);
        }
    }

    // Check for template variables
    validate_template_variables(&content, &mut report);

    // Check SPARQL queries if requested
    if options.check_sparql {
        validate_sparql_queries(&content, &mut report);
    }

    // Check schema if requested
    if options.check_schema {
        validate_schema(&content, &mut report);
    }

    Ok(report)
}

/// Validate YAML frontmatter
fn validate_frontmatter(frontmatter: &str, report: &mut LintReport) {
    // Check for required fields
    let has_to = frontmatter.contains("to:");
    let has_vars = frontmatter.contains("vars:");

    if !has_to {
        report.warnings.push(LintWarning {
            line: None,
            message: "Frontmatter should include 'to:' field for output path".to_string(),
        });
    }

    if !has_vars {
        report.warnings.push(LintWarning {
            line: None,
            message: "Frontmatter should include 'vars:' field for template variables".to_string(),
        });
    }
}

/// Validate template variables
fn validate_template_variables(content: &str, report: &mut LintReport) {
    let lines: Vec<&str> = content.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        // Check for unclosed template variables
        if line.contains("{{") && !line.contains("}}") {
            report.errors.push(LintError {
                line: Some(line_num + 1),
                message: "Unclosed template variable".to_string(),
            });
        }

        if line.contains("}}") && !line.contains("{{") {
            report.errors.push(LintError {
                line: Some(line_num + 1),
                message: "Closing template variable without opening".to_string(),
            });
        }

        // Check for empty template variables
        if line.contains("{{ }}") || line.contains("{{}}") {
            report.warnings.push(LintWarning {
                line: Some(line_num + 1),
                message: "Empty template variable".to_string(),
            });
        }
    }
}

/// Validate SPARQL queries
fn validate_sparql_queries(content: &str, report: &mut LintReport) {
    let lines: Vec<&str> = content.lines().collect();

    for (line_num, line) in lines.iter().enumerate() {
        if line.trim().starts_with("sparql:")
            || line.contains("SELECT")
            || line.contains("CONSTRUCT")
        {
            // Basic SPARQL validation
            if line.contains("SELECT") && !line.contains("WHERE") {
                report.warnings.push(LintWarning {
                    line: Some(line_num + 1),
                    message: "SPARQL SELECT query should include WHERE clause".to_string(),
                });
            }
        }
    }
}

/// Validate schema compliance
fn validate_schema(content: &str, report: &mut LintReport) {
    // Basic schema validation - check for common patterns
    if content.contains("rdf:") || content.contains("@prefix") {
        // Check for proper RDF structure
        if content.contains("@prefix") && !content.contains(".") {
            report.warnings.push(LintWarning {
                line: None,
                message: "RDF prefixes should end with '.'".to_string(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_report_has_errors() {
        let report = LintReport {
            errors: vec![LintError {
                line: Some(1),
                message: "Test error".to_string(),
            }],
            warnings: vec![],
        };
        assert!(report.has_errors());
        assert!(!report.has_warnings());
    }

    #[test]
    fn test_lint_report_has_warnings() {
        let report = LintReport {
            errors: vec![],
            warnings: vec![LintWarning {
                line: Some(1),
                message: "Test warning".to_string(),
            }],
        };
        assert!(!report.has_errors());
        assert!(report.has_warnings());
    }

    #[test]
    fn test_validate_template_variables() {
        let mut report = LintReport {
            errors: vec![],
            warnings: vec![],
        };

        let content = "{{ name }}\n{{ unclosed\n{{}}";
        validate_template_variables(content, &mut report);

        assert_eq!(report.errors.len(), 1); // unclosed variable
        assert_eq!(report.warnings.len(), 1); // empty variable
    }

    #[test]
    fn test_validate_frontmatter() {
        let mut report = LintReport {
            errors: vec![],
            warnings: vec![],
        };

        let frontmatter = "description: test\nversion: 1.0";
        validate_frontmatter(frontmatter, &mut report);

        assert_eq!(report.warnings.len(), 2); // missing 'to:' and 'vars:'
    }
}

/// CLI Arguments for lint command
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LintInput {
    /// Template file or reference
    pub template: String,

    /// Check SPARQL queries
    pub check_sparql: bool,

    /// Check schema validity
    pub check_schema: bool,
}

/// CLI run function - bridges sync CLI to async domain logic
        let options = LintOptions {
            check_sparql: args.check_sparql,
            check_schema: args.check_schema,
        };

        let report = lint_template(&args.template, &options)?;

        if report.has_errors() {
            println!("❌ Lint errors found:");
            for error in &report.errors {
                if let Some(line) = error.line {
                    println!("  Line {}: {}", line, error.message);
                } else {
                    println!("  {}", error.message);
                }
            }
        }

        if report.has_warnings() {
            println!("⚠️  Lint warnings:");
            for warning in &report.warnings {
                if let Some(line) = warning.line {
                    println!("  Line {}: {}", line, warning.message);
                } else {
                    println!("  {}", warning.message);
                }
            }
        }

        if !report.has_errors() && !report.has_warnings() {
            println!("✅ Template passed all linting checks");
        }

        Ok(())
    })
}
