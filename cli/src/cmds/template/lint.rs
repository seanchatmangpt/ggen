use clap::Args;
use ggen_utils::error::Result;
use std::fs;
use std::path::Path;

#[derive(Args, Debug)]
pub struct LintArgs {
    /// Template reference (local path or gpack:template)
    pub template_ref: String,

    /// Check SPARQL query syntax
    #[arg(long)]
    pub sparql: bool,

    /// Validate against RDF schema
    #[arg(long)]
    pub schema: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait TemplateLinter {
    fn lint(&self, template_ref: &str, options: &LintOptions) -> Result<LintReport>;
}

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

/// Validate and sanitize template reference input
fn validate_template_ref(template_ref: &str) -> Result<()> {
    // Validate template reference is not empty
    if template_ref.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Template reference cannot be empty",
        ));
    }

    // Validate template reference length
    if template_ref.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Template reference too long (max 500 characters)",
        ));
    }

    // Basic path traversal protection
    if template_ref.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: template reference cannot contain '..'",
        ));
    }

    // Validate template reference format (basic pattern check)
    if !template_ref
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == ':' || c == '-' || c == '_')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid template reference format: only alphanumeric characters, dots, slashes, colons, dashes, and underscores allowed",
        ));
    }

    Ok(())
}

pub async fn run(args: &LintArgs) -> Result<()> {
    // Validate input
    validate_template_ref(&args.template_ref)?;

    println!("🔍 Linting template...");

    let options = LintOptions {
        check_sparql: args.sparql,
        check_schema: args.schema,
    };

    let report = lint_template(&args.template_ref, &options)?;

    if !report.errors.is_empty() {
        println!("❌ Errors:");
        for error in &report.errors {
            if let Some(line) = error.line {
                println!("  Line {}: {}", line, error.message);
            } else {
                println!("  {}", error.message);
            }
        }
    }

    if !report.warnings.is_empty() {
        println!("⚠️  Warnings:");
        for warning in &report.warnings {
            if let Some(line) = warning.line {
                println!("  Line {}: {}", line, warning.message);
            } else {
                println!("  {}", warning.message);
            }
        }
    }

    if !report.has_errors() && !report.has_warnings() {
        println!("✅ No issues found");
    }

    if report.has_errors() {
        Err(ggen_utils::error::Error::new("Template has errors"))
    } else {
        Ok(())
    }
}

/// Lint a template file for common issues
fn lint_template(template_ref: &str, options: &LintOptions) -> Result<LintReport> {
    let mut report = LintReport {
        errors: Vec::new(),
        warnings: Vec::new(),
    };

    // Determine template path
    let template_path = if template_ref.starts_with("gpack:") {
        return Err(ggen_utils::error::Error::new("gpack templates not yet supported"));
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
    let content = fs::read_to_string(path).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to read template: {}", e))
    })?;

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
        if line.trim().starts_with("sparql:") || line.contains("SELECT") || line.contains("CONSTRUCT") {
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

pub async fn run_with_deps(args: &LintArgs, linter: &dyn TemplateLinter) -> Result<()> {
    // Validate input
    validate_template_ref(&args.template_ref)?;

    // Show progress for linting operation
    println!("🔍 Linting template...");

    let options = LintOptions {
        check_sparql: args.sparql,
        check_schema: args.schema,
    };

    let report = linter.lint(&args.template_ref, &options)?;

    if !report.errors.is_empty() {
        println!("❌ Errors:");
        for error in &report.errors {
            if let Some(line) = error.line {
                println!("  Line {}: {}", line, error.message);
            } else {
                println!("  {}", error.message);
            }
        }
    }

    if !report.warnings.is_empty() {
        println!("⚠️  Warnings:");
        for warning in &report.warnings {
            if let Some(line) = warning.line {
                println!("  Line {}: {}", line, warning.message);
            } else {
                println!("  {}", warning.message);
            }
        }
    }

    if !report.has_errors() && !report.has_warnings() {
        println!("✅ No issues found");
    }

    if report.has_errors() {
        Err(ggen_utils::error::Error::new("Template has errors"))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_lint_no_issues() {
        let mut mock_linter = MockTemplateLinter::new();
        mock_linter
            .expect_lint()
            .with(eq(String::from("hello.tmpl")), always())
            .times(1)
            .returning(|_, _| {
                Ok(LintReport {
                    errors: vec![],
                    warnings: vec![],
                })
            });

        let args = LintArgs {
            template_ref: "hello.tmpl".to_string(),
            sparql: false,
            schema: false,
        };

        let result = run_with_deps(&args, &mock_linter).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_lint_with_errors() {
        let mut mock_linter = MockTemplateLinter::new();
        mock_linter.expect_lint().times(1).returning(|_, _| {
            Ok(LintReport {
                errors: vec![LintError {
                    line: Some(10),
                    message: "Invalid SPARQL syntax".to_string(),
                }],
                warnings: vec![],
            })
        });

        let args = LintArgs {
            template_ref: "bad.tmpl".to_string(),
            sparql: true,
            schema: false,
        };

        let result = run_with_deps(&args, &mock_linter).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_lint_with_warnings() {
        let mut mock_linter = MockTemplateLinter::new();
        mock_linter.expect_lint().times(1).returning(|_, _| {
            Ok(LintReport {
                errors: vec![],
                warnings: vec![LintWarning {
                    line: None,
                    message: "Variable 'name' is unused".to_string(),
                }],
            })
        });

        let args = LintArgs {
            template_ref: "warning.tmpl".to_string(),
            sparql: false,
            schema: false,
        };

        let result = run_with_deps(&args, &mock_linter).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_lint_options_passed_correctly() {
        let mut mock_linter = MockTemplateLinter::new();
        mock_linter
            .expect_lint()
            .withf(|_, options| options.check_sparql && options.check_schema)
            .times(1)
            .returning(|_, _| {
                Ok(LintReport {
                    errors: vec![],
                    warnings: vec![],
                })
            });

        let args = LintArgs {
            template_ref: "test.tmpl".to_string(),
            sparql: true,
            schema: true,
        };

        let result = run_with_deps(&args, &mock_linter).await;
        assert!(result.is_ok());
    }
}
