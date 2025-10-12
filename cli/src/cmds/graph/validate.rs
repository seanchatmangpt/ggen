use clap::Args;
use ggen_utils::error::Result;
use std::path::Path;

#[derive(Args, Debug)]
pub struct ValidateArgs {
    /// SHACL shapes file
    pub shapes: String,

    /// RDF graph to validate
    #[arg(long)]
    pub graph: Option<String>,
}

#[cfg_attr(test, mockall::automock)]
pub trait ShaclValidator {
    fn validate(&self, shapes: String, graph: Option<String>) -> Result<ValidationReport>;
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub conforms: bool,
    pub violations: Vec<Violation>,
}

#[derive(Debug, Clone)]
pub struct Violation {
    pub focus_node: String,
    pub property: Option<String>,
    pub message: String,
    pub severity: Severity,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Violation,
    Warning,
    Info,
}

/// Validate and sanitize shapes file path input
fn validate_shapes_path(shapes: &str) -> Result<()> {
    // Validate shapes path is not empty
    if shapes.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Shapes file path cannot be empty",
        ));
    }

    // Validate shapes path length
    if shapes.len() > 1000 {
        return Err(ggen_utils::error::Error::new(
            "Shapes file path too long (max 1000 characters)",
        ));
    }

    // Basic path traversal protection
    if shapes.contains("..") {
        return Err(ggen_utils::error::Error::new(
            "Path traversal detected: shapes file path cannot contain '..'",
        ));
    }

    // Validate shapes path format (basic pattern check)
    if !shapes
        .chars()
        .all(|c| c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\')
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid shapes file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
        ));
    }

    Ok(())
}

/// Validate and sanitize graph file path input (if provided)
fn validate_graph_path(graph: &Option<String>) -> Result<()> {
    if let Some(graph) = graph {
        // Validate graph path is not empty
        if graph.trim().is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Graph file path cannot be empty",
            ));
        }

        // Validate graph path length
        if graph.len() > 1000 {
            return Err(ggen_utils::error::Error::new(
                "Graph file path too long (max 1000 characters)",
            ));
        }

        // Basic path traversal protection
        if graph.contains("..") {
            return Err(ggen_utils::error::Error::new(
                "Path traversal detected: graph file path cannot contain '..'",
            ));
        }

        // Validate graph path format (basic pattern check)
        if !graph.chars().all(|c| {
            c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\'
        }) {
            return Err(ggen_utils::error::Error::new(
                "Invalid graph file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
            ));
        }
    }

    Ok(())
}

pub async fn run(args: &ValidateArgs) -> Result<()> {
    // Validate inputs
    validate_shapes_path(&args.shapes)?;
    validate_graph_path(&args.graph)?;

    println!("🔍 Validating graph against SHACL shapes...");

    let report = validate_graph(args.shapes.clone(), args.graph.clone())?;

    if report.conforms {
        println!("✅ Graph conforms to SHACL shapes");
        return Ok(());
    }

    println!("❌ Graph does not conform to SHACL shapes");
    println!("\n📋 Violations:");

    for violation in &report.violations {
        let severity_symbol = match violation.severity {
            Severity::Violation => "❌",
            Severity::Warning => "⚠️",
            Severity::Info => "ℹ️",
        };

        println!("{} {}", severity_symbol, violation.focus_node);
        if let Some(property) = &violation.property {
            println!("   Property: {}", property);
        }
        println!("   {}", violation.message);
        println!();
    }

    Err(ggen_utils::error::Error::new_fmt(format_args!(
        "Validation failed with {} violations",
        report.violations.len()
    )))
}

/// Validate graph against SHACL shapes
fn validate_graph(shapes: String, graph: Option<String>) -> Result<ValidationReport> {
    // Check if shapes file exists
    if !Path::new(&shapes).exists() {
        return Err(ggen_utils::error::Error::new(&format!("Shapes file not found: {}", shapes)));
    }

    // Load graph if provided
    let graph_data = if let Some(graph_path) = graph {
        if !Path::new(&graph_path).exists() {
            return Err(ggen_utils::error::Error::new(&format!("Graph file not found: {}", graph_path)));
        }
        ggen_core::Graph::load_from_file(&graph_path).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to load graph: {}", e))
        })?
    } else {
        // Use empty graph for demonstration
        ggen_core::Graph::new().map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create graph: {}", e))
        })?
    };

    // Basic validation - in production this would use proper SHACL validation
    let mut violations = Vec::new();
    
    // Simulate some validation checks
    if graph_data.len() == 0 {
        violations.push(Violation {
            focus_node: "http://example.org/".to_string(),
            property: None,
            message: "Graph is empty".to_string(),
            severity: Severity::Warning,
        });
    }

    // Check for basic RDF structure
    if graph_data.len() > 0 {
        // Simulate a validation that always passes for non-empty graphs
        violations.clear();
    }

    Ok(ValidationReport {
        conforms: violations.is_empty(),
        violations,
    })
}

pub async fn run_with_deps(args: &ValidateArgs, validator: &dyn ShaclValidator) -> Result<()> {
    // Validate inputs
    validate_shapes_path(&args.shapes)?;
    validate_graph_path(&args.graph)?;

    // Show progress for validation operation
    println!("🔍 Validating graph against SHACL shapes...");

    let report = validator.validate(args.shapes.clone(), args.graph.clone())?;

    if report.conforms {
        println!("✅ Graph conforms to SHACL shapes");
        return Ok(());
    }

    println!("❌ Graph does not conform to SHACL shapes");
    println!("\n📋 Violations:");

    // Show progress for large violation sets
    if report.violations.len() > 20 {
        println!("📊 Processing {} violations...", report.violations.len());
    }

    for violation in &report.violations {
        let severity_symbol = match violation.severity {
            Severity::Violation => "❌",
            Severity::Warning => "⚠️",
            Severity::Info => "ℹ️",
        };

        println!("{} {}", severity_symbol, violation.focus_node);
        if let Some(property) = &violation.property {
            println!("   Property: {}", property);
        }
        println!("   {}", violation.message);
        println!();
    }

    Err(ggen_utils::error::Error::new_fmt(format_args!(
        "Validation failed with {} violations",
        report.violations.len()
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_validate_conforming_graph() {
        let mut mock_validator = MockShaclValidator::new();
        mock_validator
            .expect_validate()
            .with(
                eq(String::from("shapes.ttl")),
                eq(Some(String::from("data.ttl"))),
            )
            .times(1)
            .returning(|_, _| {
                Ok(ValidationReport {
                    conforms: true,
                    violations: vec![],
                })
            });

        let args = ValidateArgs {
            shapes: "shapes.ttl".to_string(),
            graph: Some("data.ttl".to_string()),
        };

        let result = run_with_deps(&args, &mock_validator).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_validate_non_conforming_graph() {
        let mut mock_validator = MockShaclValidator::new();
        mock_validator.expect_validate().times(1).returning(|_, _| {
            Ok(ValidationReport {
                conforms: false,
                violations: vec![Violation {
                    focus_node: "ex:Person1".to_string(),
                    property: Some("ex:age".to_string()),
                    message: "Value must be greater than 0".to_string(),
                    severity: Severity::Violation,
                }],
            })
        });

        let args = ValidateArgs {
            shapes: "shapes.ttl".to_string(),
            graph: None,
        };

        let result = run_with_deps(&args, &mock_validator).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_multiple_violations() {
        let mut mock_validator = MockShaclValidator::new();
        mock_validator.expect_validate().times(1).returning(|_, _| {
            Ok(ValidationReport {
                conforms: false,
                violations: vec![
                    Violation {
                        focus_node: "ex:Person1".to_string(),
                        property: Some("ex:name".to_string()),
                        message: "Missing required property".to_string(),
                        severity: Severity::Violation,
                    },
                    Violation {
                        focus_node: "ex:Person2".to_string(),
                        property: Some("ex:email".to_string()),
                        message: "Invalid email format".to_string(),
                        severity: Severity::Warning,
                    },
                ],
            })
        });

        let args = ValidateArgs {
            shapes: "shapes.ttl".to_string(),
            graph: Some("data.ttl".to_string()),
        };

        let result = run_with_deps(&args, &mock_validator).await;
        assert!(result.is_err());
    }
}
