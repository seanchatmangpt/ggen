use clap::Args;
use ggen_utils::error::Result;

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

pub async fn run(args: &ValidateArgs) -> Result<()> {
    println!("🚧 Placeholder: graph validate");
    println!("  Shapes: {}", args.shapes);
    println!("  Graph: {:?}", args.graph);
    Ok(())
}

pub async fn run_with_deps(args: &ValidateArgs, validator: &dyn ShaclValidator) -> Result<()> {
    let report = validator.validate(args.shapes.clone(), args.graph.clone())?;

    if report.conforms {
        println!("✅ Graph conforms to SHACL shapes");
        return Ok(());
    }

    println!("❌ Graph does not conform to SHACL shapes");
    println!("\nViolations:");

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
            .with(eq("shapes.ttl"), eq(Some("data.ttl")))
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
        mock_validator
            .expect_validate()
            .times(1)
            .returning(|_, _| {
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
        mock_validator
            .expect_validate()
            .times(1)
            .returning(|_, _| {
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
